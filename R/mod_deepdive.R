#' Deep-dive tab: one process, filtered, in depth
#'
#' @param id module id.
#' @export
mod_deepdive_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      fill = FALSE,
      bslib::card_body(
        style = "padding-bottom: 4px;",
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("process_picker"), "Process", choices = NULL, width = "100%")),
          shiny::column(4, shiny::dateRangeInput(ns("date_range"), "Case start date range", width = "100%")),
          shiny::column(4, shiny::selectizeInput(ns("role_filter"), "Roles", choices = NULL, multiple = TRUE, width = "100%"))
        )
      )
    ),
    shiny::uiOutput(ns("no_data_note")),
    shiny::div(
      class = "pv-kpi-row",
      pv_kpi_box("Runs", shiny::textOutput(ns("kpi_cases"), inline = TRUE), shiny::icon("diagram-project")),
      pv_kpi_box("Median duration", shiny::textOutput(ns("kpi_duration"), inline = TRUE), shiny::icon("clock")),
      pv_kpi_box("Users", shiny::textOutput(ns("kpi_users"), inline = TRUE), shiny::icon("users")),
      pv_kpi_box("Conformance", shiny::textOutput(ns("kpi_conformance"), inline = TRUE), shiny::icon("check-double"))
    ),
    shiny::helpText(
      "Durations are elapsed time to the next logged action, not a measured task ",
      "duration (this is an event log, not a start+complete activity log) -- see the process mining playbook."
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header(
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          "Process map",
          shiny::radioButtons(ns("map_type"), NULL, c("Frequency" = "freq", "Performance" = "perf"),
            inline = TRUE, width = "auto"
          )
        )
      ),
      shiny::uiOutput(ns("process_map"))
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Variants"),
      DT::DTOutput(ns("variant_table"), fill = FALSE)
    ),
    bslib::layout_column_wrap(
      width = 1 / 2, fill = FALSE,
      bslib::card(fill = FALSE, bslib::card_header("Case duration distribution"), plotly::plotlyOutput(ns("duration_plot"), height = "280px", fill = FALSE)),
      bslib::card(fill = FALSE, bslib::card_header("Usage per week"), plotly::plotlyOutput(ns("weekly_plot"), height = "280px", fill = FALSE))
    ),
    bslib::layout_column_wrap(
      width = 1 / 2, fill = FALSE,
      bslib::card(fill = FALSE, bslib::card_header("Workload by user"), plotly::plotlyOutput(ns("resource_plot"), height = "280px", fill = FALSE)),
      bslib::card(fill = FALSE, bslib::card_header("Conformance distribution"), plotly::plotlyOutput(ns("conformance_plot"), height = "280px", fill = FALSE))
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Lowest-conformance cases"),
      DT::DTOutput(ns("worst_cases_table"), fill = FALSE)
    )
  )
}

#' Server logic for the deep-dive tab
#'
#' @param id module id.
#' @param pv_con a reactive returning the current [pv_connect_registry()] result.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @param selected_process a reactive giving a `process_id` to jump to (e.g.
#'   from [mod_overview_server()]'s "Open deep-dive"), or `NULL`.
#' @export
mod_deepdive_server <- function(id, pv_con, registry, selected_process) {
  shiny::moduleServer(id, function(input, output, session) {
    process_choices <- shiny::reactive({
      shiny::req(registry())
      procs <- registry()$processes[registry()$processes$status == "instrumented", ]
      stats::setNames(procs$process_id, procs$process)
    })

    shiny::observe({
      choices <- process_choices()
      shiny::req(length(choices) > 0)
      current <- if (!is.null(input$process_picker) && input$process_picker %in% choices) input$process_picker else choices[1]
      shiny::updateSelectInput(session, "process_picker", choices = choices, selected = current)
    })

    shiny::observeEvent(selected_process(), {
      shiny::req(selected_process())
      shiny::updateSelectInput(session, "process_picker", selected = selected_process())
    })

    raw_events <- shiny::reactive({
      shiny::req(input$process_picker, pv_con(), registry())
      pv_pull_process_events(pv_con(), registry(), input$process_picker)
    })

    ideal_process <- shiny::reactive({
      shiny::req(input$process_picker, registry())
      registry()$ideal[[input$process_picker]]
    })

    shiny::observeEvent(raw_events(), {
      events <- raw_events()
      if (nrow(events) == 0) {
        return()
      }
      rng <- range(as.Date(events$timestamp))
      shiny::updateDateRangeInput(session, "date_range", start = rng[1], end = rng[2], min = rng[1], max = rng[2])
      shiny::updateSelectizeInput(session, "role_filter",
        choices = sort(unique(stats::na.omit(events$role))), selected = character(0)
      )
    })

    filtered_events <- shiny::reactive({
      events <- raw_events()
      shiny::req(input$date_range)
      pv_filter_events(events,
        start_date = input$date_range[1], end_date = input$date_range[2],
        roles = input$role_filter
      )
    })

    kpis <- shiny::reactive(pv_process_kpis(filtered_events(), ideal_process()))

    output$no_data_note <- shiny::renderUI({
      shiny::req(input$process_picker)
      if (nrow(raw_events()) == 0) {
        bslib::card(
          bslib::card_body("No events logged yet for this process.", class = "text-muted")
        )
      }
    })

    output$kpi_cases <- shiny::renderText(as.character(kpis()$n_cases))
    output$kpi_duration <- shiny::renderText(pv_format_duration(kpis()$median_duration_mins) %||% "--")
    output$kpi_users <- shiny::renderText(as.character(kpis()$n_users))
    output$kpi_conformance <- shiny::renderText({
      cm <- kpis()$conformance_median
      if (is.na(cm)) "--" else sprintf("%.0f%%", 100 * cm)
    })

    output$process_map <- shiny::renderUI({
      events <- filtered_events()
      shiny::validate(shiny::need(nrow(events) > 0, "No events in range."))
      el <- pv_build_eventlog(events)
      pm <- if (input$map_type == "perf") {
        processmapR::process_map(el,
          type_nodes = processmapR::performance(FUN = stats::median, units = "mins"),
          type_edges = processmapR::performance(FUN = stats::median, units = "mins")
        )
      } else {
        processmapR::process_map(el,
          type_nodes = processmapR::frequency("absolute"),
          type_edges = processmapR::frequency("absolute")
        )
      }
      pm
    })

    output$variant_table <- DT::renderDT({
      variants <- pv_variant_table(filtered_events(), ideal_process())
      DT::datatable(
        variants,
        colnames = c("Trace", "Cases", "%", "Avg. conformance"),
        rownames = FALSE, options = list(pageLength = 5, dom = "tp"),
        class = "compact stripe"
      ) |> DT::formatPercentage("mean_conformance", digits = 0)
    })

    output$duration_plot <- plotly::renderPlotly({
      durations <- pv_case_durations(filtered_events())
      shiny::validate(shiny::need(nrow(durations) > 0, "No data."))
      med <- stats::median(durations$duration_mins)
      plotly::plot_ly(durations, x = ~duration_mins, type = "histogram", marker = list(color = .pv_accent)) |>
        plotly::layout(
          xaxis = list(title = "Duration (minutes)"), yaxis = list(title = "Cases"),
          shapes = list(list(
            type = "line", x0 = med, x1 = med, y0 = 0, y1 = 1, yref = "paper",
            line = list(color = .pv_ink_secondary, dash = "dash")
          ))
        )
    })

    output$weekly_plot <- plotly::renderPlotly({
      weekly <- pv_weekly_usage(filtered_events())
      shiny::validate(shiny::need(nrow(weekly) > 0, "No data."))
      plotly::plot_ly(weekly,
        x = ~week_start, y = ~n_cases, type = "scatter", mode = "lines+markers",
        line = list(color = .pv_accent), marker = list(color = .pv_accent)
      ) |>
        plotly::layout(xaxis = list(title = "Week"), yaxis = list(title = "Cases started"))
    })

    output$resource_plot <- plotly::renderPlotly({
      breakdown <- pv_resource_breakdown(filtered_events())
      shiny::validate(shiny::need(nrow(breakdown) > 0, "No data."))
      breakdown <- breakdown[order(breakdown$n_cases), ]
      plotly::plot_ly(breakdown,
        x = ~n_cases, y = ~ factor(user_id, levels = user_id), type = "bar",
        orientation = "h", color = ~role, colors = .pv_categorical
      ) |>
        plotly::layout(xaxis = list(title = "Cases involved in"), yaxis = list(title = ""))
    })

    output$conformance_plot <- plotly::renderPlotly({
      shiny::req(ideal_process())
      conf <- pv_process_conformance(filtered_events(), ideal_process())
      shiny::validate(shiny::need(nrow(conf) > 0, "No data."))
      plotly::plot_ly(conf, x = ~score, type = "histogram", marker = list(color = .pv_accent)) |>
        plotly::layout(xaxis = list(title = "Conformance score", range = c(0, 1)), yaxis = list(title = "Cases"))
    })

    output$worst_cases_table <- DT::renderDT({
      shiny::req(ideal_process())
      conf <- pv_process_conformance(filtered_events(), ideal_process())
      conf <- conf[order(conf$score), ]
      DT::datatable(
        utils::head(conf, 10),
        colnames = c("Case", "Conformance", "Activities"),
        rownames = FALSE, options = list(dom = "t"), class = "compact stripe"
      ) |> DT::formatPercentage("score", digits = 0)
    })
  })
}
