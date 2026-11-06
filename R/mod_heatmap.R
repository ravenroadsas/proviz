#' Long-form heatmap data reshaped into a column(process-or-app) x
#' user/role matrix, zero-filled for combinations with no cases (a `0`,
#' not a missing cell, since "this user never touched this column" is a
#' real value, not an unknown one) -- what `plotly::plot_ly(type =
#' "heatmap")` wants.
#' @param long a long-form heatmap data.frame (e.g. [pv_portfolio_heatmap()]
#'   or [pv_app_heatmap()]'s result), with a `col` column already added
#'   (see [pv_heatmap_columns()]) naming each x-axis category.
#' @param dim_col the y-axis column name (`"user_id"` or `"role"`).
#' @noRd
pv_heatmap_matrix <- function(long, dim_col) {
  cols <- unique(long$col)
  dims <- unique(long[[dim_col]])
  if (length(cols) == 0 || length(dims) == 0) {
    return(list(z = matrix(numeric(0), 0, 0), x = character(0), y = character(0)))
  }
  z <- matrix(0, nrow = length(dims), ncol = length(cols), dimnames = list(dims, cols))
  for (i in seq_len(nrow(long))) {
    z[long[[dim_col]][i], long$col[i]] <- long$n_cases[i]
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

#' Users & roles tab: a heatmap of activity across the whole process
#' portfolio -- the hybrid approach to by-user/by-role analysis (filters
#' on the business/app detail tabs cover the per-process case; this tab
#' is the portfolio-wide, side-by-side view those filters can't give).
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
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::radioButtons(ns("dimension"), "Dimension", c("Users" = "user", "Roles" = "role"), inline = TRUE)
          ),
          shiny::column(
            7,
            shiny::selectizeInput(ns("process_filter"), "Business processes", choices = NULL, multiple = TRUE, width = "100%")
          ),
          shiny::column(
            2,
            shiny::checkboxInput(ns("show_apps"), "Break down by app", value = FALSE)
          )
        )
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
    )
  )
}

#' Server logic for the users & roles heatmap tab
#'
#' @param id module id.
#' @param pv_con a reactive returning the current [pv_connect_registry()] result.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @export
mod_heatmap_server <- function(id, pv_con, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    process_choices <- shiny::reactive({
      shiny::req(registry())
      procs <- registry()$processes[registry()$processes$status == "instrumented", ]
      stats::setNames(procs$process_id, procs$process)
    })

    shiny::observe({
      choices <- process_choices()
      shiny::updateSelectizeInput(session, "process_filter", choices = choices, selected = unname(choices))
    })

    dim_col <- shiny::reactive(if (input$dimension == "user") "user_id" else "role")

    heatmap_long <- shiny::reactive({
      shiny::req(pv_con(), registry(), input$dimension)
      selected <- if (!is.null(input$process_filter) && length(input$process_filter) > 0) input$process_filter else NULL

      long <- if (isTRUE(input$show_apps)) {
        pv_app_heatmap(pv_con(), registry(), by = input$dimension, process_ids = selected)
      } else {
        full <- pv_portfolio_heatmap(pv_con(), registry(), by = input$dimension)
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
      mat <- pv_heatmap_matrix(long, dim_col())
      scale <- grDevices::colorRampPalette(c("#ffffff", .pv_accent))(10)
      plotly::plot_ly(
        z = mat$z, x = mat$x, y = mat$y, type = "heatmap",
        colors = scale, hovertemplate = "%{y} x %{x}: %{z} cases<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = if (isTRUE(input$show_apps)) "Business process / App" else "Business process"),
          yaxis = list(title = if (input$dimension == "user") "User" else "Role")
        )
    })

    output$heatmap_table <- DT::renderDT({
      long <- heatmap_long()
      shiny::validate(shiny::need(nrow(long) > 0, "No data."))
      long <- long[order(-long$n_cases), ]
      dim_label <- if (input$dimension == "user") "User" else "Role"

      if (isTRUE(input$show_apps)) {
        display <- long[, c("process", "app", dim_col(), "n_cases")]
        names(display) <- c("Process", "App", dim_label, "Cases")
      } else {
        display <- long[, c("process", dim_col(), "n_cases")]
        names(display) <- c("Process", dim_label, "Cases")
      }
      DT::datatable(display, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })
  })
}
