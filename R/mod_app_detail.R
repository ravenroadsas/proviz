#' Business processes that have at least one monitored app -- the app
#' detail tab's own process picker only offers these, since there's
#' nothing to deep-dive into otherwise.
#' @noRd
pv_app_detail_process_choices <- function(registry) {
  subs <- registry$subprocesses
  monitored_ids <- unique(subs$process_id[!is.na(subs$duckdb_path)])
  procs <- registry$processes[registry$processes$process_id %in% monitored_ids, ]
  stats::setNames(procs$process_id, procs$process)
}

#' A process's steps that have at least one monitored app.
#' @noRd
pv_app_detail_step_choices <- function(registry, process_id) {
  steps <- registry$steps[registry$steps$process_id == process_id, ]
  subs <- registry$subprocesses[registry$subprocesses$process_id == process_id, ]
  monitored_steps <- unique(subs$step[!is.na(subs$duckdb_path)])
  steps <- steps[steps$step %in% monitored_steps, ]
  steps <- steps[order(steps$step_order), ]
  stats::setNames(steps$step, steps$step_label)
}

#' A step's monitored app(s), within one process.
#' @noRd
pv_app_detail_app_choices <- function(registry, process_id, step) {
  subs <- registry$subprocesses[
    registry$subprocesses$process_id == process_id &
      registry$subprocesses$step == step &
      !is.na(registry$subprocesses$duckdb_path),
  ]
  subs <- subs[order(subs$app_order), ]
  stats::setNames(subs$subprocess, subs$app)
}

#' App detail tab: one app's own process, filtered, in depth
#'
#' The deepest of the three levels: one specific app within one step of
#' one business process, mined at that app's own activity granularity
#' (its own `ideal_process` file) -- as opposed to the business detail
#' tab's step-granularity view across every app in the process.
#'
#' @param id module id.
#' @export
mod_app_detail_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      fill = FALSE,
      bslib::card_body(
        style = "padding-bottom: 4px;",
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("process_picker"), "Business process", choices = NULL, width = "100%")),
          shiny::column(4, shiny::selectInput(ns("step_picker"), "Step", choices = NULL, width = "100%")),
          shiny::column(4, shiny::selectInput(ns("app_picker"), "App", choices = NULL, width = "100%"))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::dateRangeInput(ns("date_range"), "Case start date range", width = "100%")),
          shiny::column(4, shiny::selectizeInput(ns("role_filter"), "Roles", choices = NULL, multiple = TRUE, width = "100%")),
          shiny::column(4, shiny::selectizeInput(ns("user_filter"), "Users", choices = NULL, multiple = TRUE, width = "100%"))
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
#' @param pv_con a reactive returning the current [pv_connect_registry()] result.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @param selected_app a reactive giving `list(process_id, subprocess)` to
#'   jump to (e.g. from [mod_business_detail_server()]'s "Open app
#'   detail"), or `NULL`.
#' @export
mod_app_detail_server <- function(id, pv_con, registry, selected_app) {
  shiny::moduleServer(id, function(input, output, session) {
    pending_jump <- shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::req(registry())
      choices <- pv_app_detail_process_choices(registry())
      shiny::req(length(choices) > 0)
      current <- if (!is.null(input$process_picker) && input$process_picker %in% choices) input$process_picker else choices[1]
      shiny::updateSelectInput(session, "process_picker", choices = choices, selected = current)
    })

    shiny::observeEvent(selected_app(), {
      sel <- selected_app()
      shiny::req(sel, registry())
      pending_jump(sel)
      shiny::updateSelectInput(session, "process_picker",
        choices = pv_app_detail_process_choices(registry()), selected = sel$process_id
      )
    })

    shiny::observeEvent(input$process_picker, {
      shiny::req(registry(), input$process_picker)
      choices <- pv_app_detail_step_choices(registry(), input$process_picker)
      shiny::req(length(choices) > 0)

      pj <- pending_jump()
      target_step <- NULL
      if (!is.null(pj) && pj$process_id == input$process_picker) {
        row <- registry()$subprocesses[
          registry()$subprocesses$process_id == pj$process_id & registry()$subprocesses$subprocess == pj$subprocess,
        ]
        if (nrow(row) > 0 && row$step[1] %in% choices) target_step <- row$step[1]
      }
      current <- if (!is.null(target_step)) {
        target_step
      } else if (!is.null(input$step_picker) && input$step_picker %in% choices) {
        input$step_picker
      } else {
        choices[1]
      }
      shiny::updateSelectInput(session, "step_picker", choices = choices, selected = current)
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$step_picker, {
      shiny::req(registry(), input$process_picker, input$step_picker)
      choices <- pv_app_detail_app_choices(registry(), input$process_picker, input$step_picker)
      shiny::req(length(choices) > 0)

      pj <- pending_jump()
      target_app <- NULL
      if (!is.null(pj) && pj$process_id == input$process_picker && pj$subprocess %in% choices) {
        target_app <- pj$subprocess
      }
      current <- if (!is.null(target_app)) {
        target_app
      } else if (!is.null(input$app_picker) && input$app_picker %in% choices) {
        input$app_picker
      } else {
        choices[1]
      }
      shiny::updateSelectInput(session, "app_picker", choices = choices, selected = current)
      pending_jump(NULL)
    }, ignoreNULL = TRUE)

    raw_events <- shiny::reactive({
      shiny::req(input$process_picker, input$app_picker, pv_con(), registry())
      pv_pull_app_events(pv_con(), registry(), input$process_picker, input$app_picker)
    })

    ideal_process <- shiny::reactive({
      shiny::req(input$process_picker, input$app_picker, registry())
      registry()$ideal[[paste(input$process_picker, input$app_picker, sep = "::")]]
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
      shiny::updateSelectizeInput(session, "user_filter",
        choices = sort(unique(stats::na.omit(events$user_id))), selected = character(0)
      )
    })

    filtered_events <- shiny::reactive({
      events <- raw_events()
      shiny::req(input$date_range)
      pv_filter_events(events,
        start_date = input$date_range[1], end_date = input$date_range[2],
        roles = input$role_filter, users = input$user_filter
      )
    })

    kpis <- shiny::reactive(pv_process_kpis(filtered_events(), ideal_process()))

    output$no_data_note <- shiny::renderUI({
      shiny::req(input$process_picker, input$app_picker)
      if (nrow(raw_events()) == 0) {
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
      shiny::req(ideal_process())
      dist <- pv_outcome_distribution(filtered_events(), ideal_process())
      shiny::validate(shiny::need(nrow(dist) > 0, "No outcomes declared for this app yet."))
      DT::datatable(
        pv_outcomes_display_table(dist),
        rownames = FALSE, escape = FALSE, options = list(dom = "t"), class = "compact stripe"
      )
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
      conf <- utils::head(conf[order(conf$score), ], 10)
      score_pct <- round(100 * conf$score)
      conf$score <- vapply(seq_len(nrow(conf)), function(i) {
        pv_cell_bar(paste0(score_pct[i], "%"), score_pct[i])
      }, character(1))
      DT::datatable(
        conf,
        colnames = c("Case", "Conformance", "Activities"),
        rownames = FALSE, escape = FALSE, options = list(dom = "t"), class = "compact stripe"
      )
    })
  })
}
