#' Long-form heatmap data reshaped into a column(process-or-app) x user
#' matrix, zero-filled for combinations with no cases (a `0`, not a
#' missing cell, since "this user never touched this column" is a real
#' value, not an unknown one) -- what `plotly::plot_ly(type = "heatmap")`
#' wants.
#' @param long a long-form heatmap data.frame (e.g. [pv_portfolio_heatmap()]
#'   or [pv_app_heatmap()]'s result), with a `col` column already added
#'   (see [pv_heatmap_columns()]) naming each x-axis category.
#' @noRd
pv_heatmap_matrix <- function(long) {
  cols <- unique(long$col)
  dims <- unique(long$user_id)
  if (length(cols) == 0 || length(dims) == 0) {
    return(list(z = matrix(numeric(0), 0, 0), x = character(0), y = character(0)))
  }
  z <- matrix(0, nrow = length(dims), ncol = length(cols), dimnames = list(dims, cols))
  for (i in seq_len(nrow(long))) {
    z[long$user_id[i], long$col[i]] <- long$n_cases[i]
  }
  list(z = z, x = cols, y = dims)
}

#' Label each row of a heatmap data.frame with its x-axis category --
#' the business process's own label at process granularity, or
#' `"<process> / <app>"` at app granularity (so two different processes'
#' identically-named apps, if that ever happens, still read as distinct
#' columns).
#' @noRd
pv_heatmap_columns <- function(long, by_app) {
  long$col <- if (by_app) paste(long$process, long$app, sep = " / ") else long$process
  long
}

#' Users tab: a heatmap of activity across the whole process portfolio --
#' the portfolio-wide, side-by-side view the business/app detail tabs'
#' own filters can't give. Narrows to one business process when the
#' sidebar's shared process picker (see [mod_filters_server()]) has one
#' selected, rather than keeping its own separate multi-process filter --
#' "(All processes)" (the default) is how to get the side-by-side,
#' every-process view back. Filter to a role via the same sidebar -- the
#' real event log itself (see [pv_pull_subprocess_log()]) carries no role
#' column, so this tab has no role breakdown of its own.
#'
#' @param id module id.
#' @export
mod_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      fill = FALSE,
      bslib::card_body(
        style = "padding-bottom: 4px;",
        shiny::checkboxInput(ns("show_apps"), "Break down by app", value = FALSE)
      )
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Cases touched, by process"),
      shiny::uiOutput(ns("no_data_note")),
      plotly::plotlyOutput(ns("heatmap_plot"), height = "480px")
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Detail"),
      DT::DTOutput(ns("heatmap_table"), fill = FALSE)
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Engagement score correlations"),
      shiny::helpText(
        style = "padding: 0 0.95rem;",
        "Score vs. time: is this user's (or this app's) weekly engagement trending up or down? ",
        "Score vs. compliance: do a user's higher-scoring cases also tend to follow the declared step diagram more closely?"
      ),
      DT::DTOutput(ns("app_summary_table"), fill = FALSE),
      shiny::helpText(
        style = "padding: 0 0.95rem;",
        "The per-user breakdown below follows the app picked in the sidebar filters."
      ),
      shiny::uiOutput(ns("no_app_note")),
      shiny::div(
        class = "pv-kpi-row", style = "padding: 0 0.95rem;",
        pv_kpi_box("Score vs. time (app-wide)", shiny::textOutput(ns("kpi_time_r"), inline = TRUE), shiny::icon("arrow-trend-up")),
        pv_kpi_box("Score vs. compliance (app-wide)", shiny::textOutput(ns("kpi_compliance_r"), inline = TRUE), shiny::icon("link"))
      ),
      bslib::layout_column_wrap(
        width = 1 / 2, fill = FALSE,
        bslib::card(fill = FALSE, bslib::card_header("Score vs. time, by user"), DT::DTOutput(ns("time_corr_table"), fill = FALSE)),
        bslib::card(fill = FALSE, bslib::card_header("Score vs. compliance, by user"), DT::DTOutput(ns("compliance_corr_table"), fill = FALSE))
      )
    )
  )
}

#' Server logic for the users heatmap tab
#'
#' @param id module id.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @param refresh a reactive (any value) bumped whenever logs should be
#'   re-read.
#' @param date_range,users,process_id,app reactives giving the
#'   dashboard's global sidebar filters (see [mod_filters_server()]);
#'   `users()` already resolves any role selection. `process_id()` is a
#'   sentinel value (never a real `process_id`) for "(All processes)"
#'   (every instrumented process, side by side); `app()` is
#'   `list(process_id, subprocess)`, or `NULL`.
#' @export
mod_heatmap_server <- function(id, registry, refresh = shiny::reactive(NULL),
                                date_range = shiny::reactive(NULL), users = shiny::reactive(NULL),
                                process_id = shiny::reactive(NULL), app = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    corr_app_name <- shiny::reactive({
      shiny::req(registry(), app())
      subs <- registry()$subprocesses
      subs$app[subs$process_id == app()$process_id & subs$subprocess == app()$subprocess][1]
    })

    output$no_app_note <- shiny::renderUI({
      if (is.null(app())) {
        bslib::card(
          bslib::card_body("Pick an app in the sidebar filters to see its engagement correlations.", class = "text-muted")
        )
      }
    })

    app_log <- shiny::reactive({
      refresh()
      shiny::req(registry(), corr_app_name())
      rng <- date_range()
      pv_app_log(registry(), corr_app_name(), start_date = rng[1], end_date = rng[2], users = users())
    })

    output$kpi_time_r <- shiny::renderText({
      shiny::req(corr_app_name())
      r <- pv_score_time_trend(app_log(), by = "app")$r
      if (is.na(r)) "--" else sprintf("%.2f", r)
    })
    output$kpi_compliance_r <- shiny::renderText({
      shiny::req(registry(), corr_app_name())
      rng <- date_range()
      r <- pv_app_score_compliance_correlation(registry(), corr_app_name(), by = "app", start_date = rng[1], end_date = rng[2], users = users())$r
      if (is.na(r)) "--" else sprintf("%.2f", r)
    })

    output$app_summary_table <- DT::renderDT({
      refresh()
      shiny::req(registry())
      rng <- date_range()
      summary <- pv_app_engagement_summary(registry(), start_date = rng[1], end_date = rng[2], users = users())
      shiny::validate(shiny::need(nrow(summary) > 0, "No app is monitored yet."))
      display <- data.frame(
        App = summary$app, Users = summary$n_users,
        `Score vs. time` = ifelse(is.na(summary$time_r), "--", sprintf("%.2f", summary$time_r)),
        `Weeks active` = summary$n_weeks,
        `Score vs. compliance` = ifelse(is.na(summary$compliance_r), "--", sprintf("%.2f", summary$compliance_r)),
        Cases = summary$n_cases,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(display, rownames = FALSE, options = list(dom = "t", pageLength = 20), class = "compact stripe")
    })

    output$time_corr_table <- DT::renderDT({
      shiny::req(corr_app_name())
      tab <- pv_score_time_trend(app_log(), by = "user")
      shiny::validate(shiny::need(nrow(tab) > 0, "No events logged yet for this app."))
      display <- tab
      names(display) <- c("User", "r", "Weeks active")
      DT::datatable(display, rownames = FALSE, options = list(pageLength = 8, dom = "tp"), class = "compact stripe")
    })

    output$compliance_corr_table <- DT::renderDT({
      shiny::req(registry(), corr_app_name())
      rng <- date_range()
      tab <- pv_app_score_compliance_correlation(registry(), corr_app_name(), by = "user", start_date = rng[1], end_date = rng[2], users = users())
      shiny::validate(shiny::need(nrow(tab) > 0, "No events logged yet for this app."))
      display <- tab
      names(display) <- c("User", "r", "Cases")
      DT::datatable(display, rownames = FALSE, options = list(pageLength = 8, dom = "tp"), class = "compact stripe")
    })

    heatmap_long <- shiny::reactive({
      refresh()
      shiny::req(registry())
      selected <- if (!is.null(process_id()) && !identical(process_id(), .pv_all_processes_sentinel)) process_id() else NULL
      rng <- date_range()

      long <- if (isTRUE(input$show_apps)) {
        pv_app_heatmap(registry(), process_ids = selected, start_date = rng[1], end_date = rng[2], users = users())
      } else {
        full <- pv_portfolio_heatmap(registry(), start_date = rng[1], end_date = rng[2], users = users())
        if (is.null(selected)) full else full[full$process_id %in% selected, ]
      }
      pv_heatmap_columns(long, by_app = isTRUE(input$show_apps))
    })

    output$no_data_note <- shiny::renderUI({
      if (nrow(heatmap_long()) == 0) {
        bslib::card(bslib::card_body("No events logged yet across the selected processes.", class = "text-muted"))
      }
    })

    output$heatmap_plot <- plotly::renderPlotly({
      long <- heatmap_long()
      shiny::validate(shiny::need(nrow(long) > 0, "No data."))
      mat <- pv_heatmap_matrix(long)
      scale <- grDevices::colorRampPalette(c("#ffffff", .pv_accent))(10)
      plotly::plot_ly(
        z = mat$z, x = mat$x, y = mat$y, type = "heatmap",
        colors = scale, hovertemplate = "%{y} x %{x}: %{z} cases<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = if (isTRUE(input$show_apps)) "Business process / App" else "Business process"),
          yaxis = list(title = "User")
        )
    })

    output$heatmap_table <- DT::renderDT({
      long <- heatmap_long()
      shiny::validate(shiny::need(nrow(long) > 0, "No data."))
      long <- long[order(-long$n_cases), ]

      if (isTRUE(input$show_apps)) {
        display <- long[, c("process", "app", "user_id", "n_cases")]
        names(display) <- c("Process", "App", "User", "Cases")
      } else {
        display <- long[, c("process", "user_id", "n_cases")]
        names(display) <- c("Process", "User", "Cases")
      }
      DT::datatable(display, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })
  })
}
