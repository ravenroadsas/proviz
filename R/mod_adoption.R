#' The per-app IGA table's display data.frame: percentages as cell bars,
#' `--` for any component that couldn't be computed.
#' @noRd
pv_iga_display_table <- function(iga_tab) {
  pct_label <- function(x) ifelse(is.na(x), NA_character_, paste0(x, "%"))
  data.frame(
    App = iga_tab$app,
    `Identified users` = iga_tab$n_users,
    Trained = vapply(seq_len(nrow(iga_tab)), function(i) pv_cell_bar(pct_label(iga_tab$pct_trained[i]), iga_tab$pct_trained[i]), character(1)),
    Satisfaction = vapply(seq_len(nrow(iga_tab)), function(i) pv_cell_bar(pct_label(iga_tab$satisfaction[i]), iga_tab$satisfaction[i]), character(1)),
    `Active users` = vapply(seq_len(nrow(iga_tab)), function(i) pv_cell_bar(pct_label(iga_tab$pct_active[i]), iga_tab$pct_active[i]), character(1)),
    IGA = vapply(seq_len(nrow(iga_tab)), function(i) pv_cell_bar(pct_label(iga_tab$iga[i]), iga_tab$iga[i]), character(1)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' Adoption tab: the official IGA adoption index per app, portfolio KPIs,
#' and whether better-adopted apps also see better process conformance
#'
#' IGA = the equal-weighted average of % trained, satisfaction, and %
#' of identified users actively using the app (see [pv_app_iga()]) --
#' training and satisfaction are yearly snapshots from the admin's own
#' workbook ([pv_load_adoption()]); the active-users share is the one
#' component with real week-to-week granularity, so it's what the
#' trend chart tracks over time.
#'
#' @param id module id.
#' @export
mod_adoption_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("no_data_note")),
    shiny::div(
      class = "pv-kpi-row",
      pv_kpi_box("Apps measured", shiny::textOutput(ns("kpi_measured"), inline = TRUE), shiny::icon("clipboard-check")),
      pv_kpi_box("Apps with IGA", shiny::textOutput(ns("kpi_with_iga"), inline = TRUE), shiny::icon("chart-pie")),
      pv_kpi_box("Average IGA", shiny::textOutput(ns("kpi_avg_iga"), inline = TRUE), shiny::icon("gauge-high")),
      pv_kpi_box("Identified users", shiny::textOutput(ns("kpi_users"), inline = TRUE), shiny::icon("users"))
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Weekly active-users rate (portfolio average)"),
      shiny::helpText(
        style = "padding: 0 0.95rem;",
        "Training and satisfaction are yearly snapshots -- this is the one IGA component with a real ",
        "week-to-week trend: the share of each app's identified users averaging more than its declared ",
        "usage threshold that week, averaged across every app with one declared."
      ),
      plotly::plotlyOutput(ns("trend_plot"), height = "260px")
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Adoption index (IGA) by app"),
      DT::DTOutput(ns("iga_table"), fill = FALSE)
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Adoption vs. process conformance"),
      shiny::helpText(
        style = "padding: 0 0.95rem;",
        "One point per app: its IGA against its mean case conformance to its own ideal process. ",
        shiny::textOutput(ns("iga_conformance_r"), inline = TRUE)
      ),
      plotly::plotlyOutput(ns("iga_conformance_plot"), height = "320px")
    )
  )
}

#' Server logic for the adoption tab
#'
#' @param id module id.
#' @param pv_con a reactive returning the current [pv_connect_registry()] result.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @param adoption a reactive returning the current [pv_load_adoption()] result,
#'   or `NULL` if no `adoption_source` is configured or it failed to load.
#' @export
mod_adoption_server <- function(id, pv_con, registry, adoption) {
  shiny::moduleServer(id, function(input, output, session) {
    iga_tab <- shiny::reactive({
      shiny::req(pv_con(), registry())
      ad <- adoption()
      if (is.null(ad) || length(ad) == 0) {
        return(pv_portfolio_iga(pv_con(), registry(), list()))
      }
      pv_portfolio_iga(pv_con(), registry(), ad)
    })

    output$no_data_note <- shiny::renderUI({
      if (is.null(adoption())) {
        bslib::card(
          bslib::card_body(
            "No adoption workbook configured or loadable yet -- declare an 'adoption_source' path in the registry ",
            "(see pv_load_registry()'s docs). Showing apps with zero identified users below.",
            class = "text-muted"
          )
        )
      }
    })

    output$kpi_measured <- shiny::renderText({
      tab <- iga_tab()
      sprintf("%d / %d", sum(tab$n_users > 0), nrow(tab))
    })
    output$kpi_with_iga <- shiny::renderText({
      tab <- iga_tab()
      sprintf("%d / %d", sum(!is.na(tab$iga)), nrow(tab))
    })
    output$kpi_avg_iga <- shiny::renderText({
      tab <- iga_tab()
      avg <- mean(tab$iga, na.rm = TRUE)
      if (is.nan(avg)) "--" else sprintf("%.0f%%", avg)
    })
    output$kpi_users <- shiny::renderText(as.character(sum(iga_tab()$n_users)))

    output$trend_plot <- plotly::renderPlotly({
      shiny::req(pv_con(), registry())
      ad <- adoption() %||% list()
      trend <- pv_portfolio_active_rate_trend(pv_con(), registry(), ad)
      shiny::validate(shiny::need(nrow(trend) > 0, "No usage-threshold data yet (declare 'adoption_threshold' per app and load an adoption workbook)."))
      plotly::plot_ly(trend,
        x = ~week, y = ~pct_active, type = "scatter", mode = "lines+markers",
        line = list(color = .pv_accent), marker = list(color = .pv_accent),
        hovertemplate = "%{x}: %{y}%% active (avg over %{text} app(s))<extra></extra>", text = ~n_apps
      ) |>
        plotly::layout(xaxis = list(title = "Week"), yaxis = list(title = "% of identified users active", range = c(0, 100)))
    })

    output$iga_table <- DT::renderDT({
      tab <- iga_tab()
      shiny::validate(shiny::need(nrow(tab) > 0, "No apps scoped yet."))
      DT::datatable(
        pv_iga_display_table(tab), rownames = FALSE, escape = FALSE,
        options = list(dom = "t", pageLength = 20), class = "compact stripe"
      )
    })

    iga_conformance <- shiny::reactive({
      shiny::req(pv_con(), registry())
      pv_iga_compliance_correlation(pv_con(), registry(), adoption() %||% list())
    })

    output$iga_conformance_r <- shiny::renderText({
      r <- iga_conformance()$r
      if (is.na(r)) "Not enough apps with both figures yet to estimate a correlation." else sprintf("Correlation: r = %.2f.", r)
    })

    output$iga_conformance_plot <- plotly::renderPlotly({
      data <- iga_conformance()$data
      shiny::validate(shiny::need(nrow(data) > 0, "No app has both an IGA and a conformance figure yet."))
      plotly::plot_ly(data,
        x = ~iga, y = ~ (100 * mean_conformance), type = "scatter", mode = "markers+text",
        text = ~app, textposition = "top center", marker = list(color = .pv_accent, size = 12)
      ) |>
        plotly::layout(
          xaxis = list(title = "IGA (%)", range = c(0, 100)),
          yaxis = list(title = "Mean conformance (%)", range = c(0, 100))
        )
    })
  })
}
