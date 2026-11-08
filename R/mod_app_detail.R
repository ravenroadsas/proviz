#' App detail tab: one app's own process, filtered, in depth
#'
#' The deepest level: one specific app's own step diagram, mined from
#' its own event log.
#'
#' @param id module id.
#' @export
mod_app_detail_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("no_data_note")),
    shiny::div(
      class = "pv-kpi-row",
      pv_kpi_box("Runs", shiny::textOutput(ns("kpi_cases"), inline = TRUE), shiny::icon("diagram-project")),
      pv_kpi_box("Median duration", shiny::textOutput(ns("kpi_duration"), inline = TRUE), shiny::icon("clock")),
      pv_kpi_box("Users", shiny::textOutput(ns("kpi_users"), inline = TRUE), shiny::icon("users")),
      pv_kpi_box("Conformance", shiny::textOutput(ns("kpi_conformance"), inline = TRUE), shiny::icon("check-double"))
    ),
    shiny::helpText(
      "Durations are elapsed time to the next logged step, not a measured task ",
      "duration (this is an event log, not a start+complete activity log)."
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Outcomes"),
      shiny::helpText(
        style = "padding: 0 0.95rem;",
        "How this app actually ended -- a deliverable, one or more named decisions, or (for a case with ",
        "none yet) incomplete. Which decision fired is a business result, not a conformance defect: see the ",
        "conformance chart below for whether the right steps were followed to get there."
      ),
      DT::DTOutput(ns("outcomes_table"), fill = FALSE)
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

#' Server logic for the app detail tab
#'
#' @param id module id.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @param refresh a reactive (any value) bumped whenever logs should be
#'   re-read.
#' @param date_range,users,app reactives giving the dashboard's global
#'   sidebar filters (see [mod_filters_server()]). `app()` is
#'   `list(process_id, subprocess)`, or `NULL` if nothing is monitored
#'   yet -- this tab shows its usual "no data yet" empty state then.
#' @export
mod_app_detail_server <- function(id, registry, refresh = shiny::reactive(NULL),
                                   date_range = shiny::reactive(NULL), users = shiny::reactive(NULL),
                                   app = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    raw_log <- shiny::reactive({
      refresh()
      shiny::req(registry(), app())
      rng <- date_range()
      pv_pull_subprocess_log(registry(), app()$process_id, app()$subprocess, start_date = rng[1], end_date = rng[2], users = users())
    })

    diagram <- shiny::reactive({
      shiny::req(registry(), app())
      registry()$subprocess_diagrams[[paste(app()$process_id, app()$subprocess, sep = "::")]]
    })

    filtered_log <- shiny::reactive(raw_log())

    kpis <- shiny::reactive(pv_process_kpis(filtered_log(), diagram()))

    output$no_data_note <- shiny::renderUI({
      if (is.null(app())) {
        return(bslib::card(
          bslib::card_body("Pick an app in the sidebar filters to see its detail.", class = "text-muted")
        ))
      }
      if (nrow(raw_log()) == 0) {
        bslib::card(
          bslib::card_body("No events logged yet for this app.", class = "text-muted")
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

    output$outcomes_table <- DT::renderDT({
      shiny::req(diagram())
      dist <- pv_outcome_distribution(pv_session_step_sequences(filtered_log()), diagram()$outcomes, "step_id")
      shiny::validate(shiny::need(nrow(dist) > 0, "No outcomes declared for this app yet."))
      DT::datatable(
        pv_outcomes_display_table(dist),
        rownames = FALSE, escape = FALSE, options = list(dom = "t"), class = "compact stripe"
      )
    })

    output$process_map <- shiny::renderUI({
      log <- filtered_log()
      shiny::validate(shiny::need(nrow(log) > 0, "No events in range."))
      el <- pv_build_eventlog(log)
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
      shiny::req(diagram())
      variants <- pv_variant_table(filtered_log(), diagram())
      conformance_pct <- round(100 * variants$mean_conformance)
      variants$pct <- vapply(seq_len(nrow(variants)), function(i) {
        pv_cell_bar(paste0(variants$pct[i], "%"), variants$pct[i])
      }, character(1))
      variants$mean_conformance <- vapply(seq_len(nrow(variants)), function(i) {
        label <- if (is.na(conformance_pct[i])) NA_character_ else paste0(conformance_pct[i], "%")
        pv_cell_bar(label, conformance_pct[i])
      }, character(1))
      DT::datatable(
        variants,
        colnames = c("Trace", "Cases", "Share", "Conformance"),
        rownames = FALSE, escape = FALSE, options = list(pageLength = 5, dom = "tp"),
        class = "compact stripe"
      )
    })

    output$duration_plot <- plotly::renderPlotly({
      durations <- pv_case_durations(filtered_log())
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
      weekly <- pv_weekly_usage(filtered_log())
      shiny::validate(shiny::need(nrow(weekly) > 0, "No data."))
      plotly::plot_ly(weekly,
        x = ~week_start, y = ~n_cases, type = "scatter", mode = "lines+markers",
        line = list(color = .pv_accent), marker = list(color = .pv_accent)
      ) |>
        plotly::layout(xaxis = list(title = "Week"), yaxis = list(title = "Cases started"))
    })

    output$resource_plot <- plotly::renderPlotly({
      breakdown <- pv_resource_breakdown(filtered_log())
      shiny::validate(shiny::need(nrow(breakdown) > 0, "No data."))
      breakdown <- breakdown[order(breakdown$n_cases), ]
      plotly::plot_ly(breakdown,
        x = ~n_cases, y = ~ factor(user_id, levels = user_id), type = "bar",
        orientation = "h", marker = list(color = .pv_accent)
      ) |>
        plotly::layout(xaxis = list(title = "Cases involved in"), yaxis = list(title = ""))
    })

    output$conformance_plot <- plotly::renderPlotly({
      shiny::req(diagram())
      conf <- pv_diagram_conformance(pv_session_step_sequences(filtered_log()), diagram()$edges, "from_step", "to_step")
      shiny::validate(shiny::need(nrow(conf) > 0, "No data."))
      plotly::plot_ly(conf, x = ~score, type = "histogram", marker = list(color = .pv_accent)) |>
        plotly::layout(xaxis = list(title = "Conformance score", range = c(0, 1)), yaxis = list(title = "Cases"))
    })

    output$worst_cases_table <- DT::renderDT({
      shiny::req(diagram())
      conf <- pv_diagram_conformance(pv_session_step_sequences(filtered_log()), diagram()$edges, "from_step", "to_step")
      conf <- utils::head(conf[order(conf$score), ], 10)
      score_pct <- round(100 * conf$score)
      conf$score <- vapply(seq_len(nrow(conf)), function(i) {
        pv_cell_bar(paste0(score_pct[i], "%"), score_pct[i])
      }, character(1))
      DT::datatable(
        conf[, c("case_id", "score", "n_nodes")],
        colnames = c("Case", "Conformance", "Steps"),
        rownames = FALSE, escape = FALSE, options = list(dom = "t"), class = "compact stripe"
      )
    })
  })
}
