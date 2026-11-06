#' One step's flow card: its label/order plus each of its app(s),
#' monitored or not -- the "which step is monitored" view the business
#' detail tab leads with.
#' @noRd
pv_step_flow_card <- function(step_row, apps) {
  app_rows <- lapply(seq_len(nrow(apps)), function(i) {
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center; gap: 0.5rem; padding: 0.15rem 0;",
      shiny::span(apps$app[i], style = "font-size: 0.82rem;"),
      shiny::HTML(pv_monitored_badge(!is.na(apps$duckdb_path[i])))
    )
  })
  shiny::div(
    style = "flex: 1 1 200px; background: #fff; border: 1px solid #e1e0d9; border-radius: 0.5rem; padding: 0.7rem 0.9rem;",
    shiny::div(
      style = "font-weight: 600; font-size: 0.85rem; margin-bottom: 0.4rem;",
      sprintf("%d. %s", step_row$step_order, step_row$step_label)
    ),
    app_rows
  )
}

#' Business process detail tab: the business process's own step flow,
#' which step is monitored, per-app metrics, and business-level process
#' mining (process mining applied at step granularity -- see
#' [pv_pull_business_events()]/[pv_step_sequence()]).
#'
#' @param id module id.
#' @export
mod_business_detail_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      fill = FALSE,
      bslib::card_body(
        style = "padding-bottom: 4px;",
        shiny::fluidRow(
          shiny::column(3, shiny::selectInput(ns("process_picker"), "Business process", choices = NULL, width = "100%")),
          shiny::column(3, shiny::dateRangeInput(ns("date_range"), "Case start date range", width = "100%")),
          shiny::column(3, shiny::selectizeInput(ns("role_filter"), "Roles", choices = NULL, multiple = TRUE, width = "100%")),
          shiny::column(3, shiny::selectizeInput(ns("user_filter"), "Users", choices = NULL, multiple = TRUE, width = "100%"))
        )
      )
    ),
    shiny::uiOutput(ns("no_data_note")),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Step flow"),
      shiny::div(style = "display: flex; flex-wrap: wrap; gap: 0.6rem;", shiny::uiOutput(ns("step_flow")))
    ),
    shiny::div(
      class = "pv-kpi-row",
      pv_kpi_box("Cases", shiny::textOutput(ns("kpi_cases"), inline = TRUE), shiny::icon("diagram-project")),
      pv_kpi_box("Median duration", shiny::textOutput(ns("kpi_duration"), inline = TRUE), shiny::icon("clock")),
      pv_kpi_box("Users", shiny::textOutput(ns("kpi_users"), inline = TRUE), shiny::icon("users")),
      pv_kpi_box("Business conformance", shiny::textOutput(ns("kpi_conformance"), inline = TRUE), shiny::icon("check-double")),
      pv_kpi_box("Completed", shiny::textOutput(ns("kpi_completion"), inline = TRUE), shiny::icon("circle-check"))
    ),
    shiny::helpText(
      "Conformance and the process map below score each case's trace of ",
      "*steps* against the business process's declared step order -- not any one app's own activities (see the app detail tab for that)."
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Outcomes"),
      shiny::helpText(
        style = "padding: 0 0.95rem;",
        "How this business process ended, as decided by the app(s) in its final step."
      ),
      DT::DTOutput(ns("outcomes_table"), fill = FALSE)
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Metrics by app"),
      DT::DTOutput(ns("app_metrics_table"), fill = FALSE),
      shiny::div(
        style = "margin-top: 8px;",
        shiny::actionButton(ns("open_app_detail"), "Open app detail ->", class = "btn-primary btn-sm"),
        shiny::span(
          "Select a row above first.",
          style = "margin-left: 8px; color: #868e96; font-size: 0.85em;"
        )
      )
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header(
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          "Business process map (step granularity)",
          shiny::radioButtons(ns("map_type"), NULL, c("Frequency" = "freq", "Performance" = "perf"),
            inline = TRUE, width = "auto"
          )
        )
      ),
      shiny::uiOutput(ns("process_map"))
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Step-sequence variants"),
      DT::DTOutput(ns("variant_table"), fill = FALSE)
    ),
    bslib::layout_column_wrap(
      width = 1 / 2, fill = FALSE,
      bslib::card(fill = FALSE, bslib::card_header("Case duration distribution"), plotly::plotlyOutput(ns("duration_plot"), height = "280px", fill = FALSE)),
      bslib::card(fill = FALSE, bslib::card_header("Usage per week"), plotly::plotlyOutput(ns("weekly_plot"), height = "280px", fill = FALSE))
    )
  )
}

#' Server logic for the business process detail tab
#'
#' @param id module id.
#' @param pv_con a reactive returning the current [pv_connect_registry()] result.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @param selected_process a reactive giving a `process_id` to jump to (e.g.
#'   from [mod_overview_server()]'s "Open business process"), or `NULL`.
#' @return a list: `selected_app`, a reactive giving `list(process_id, subprocess)`
#'   for the app detail tab to jump to (`NULL` until a row's "Open app detail"
#'   is clicked).
#' @export
mod_business_detail_server <- function(id, pv_con, registry, selected_process) {
  shiny::moduleServer(id, function(input, output, session) {
    process_choices <- shiny::reactive({
      shiny::req(registry())
      procs <- registry()$processes[registry()$processes$status != "deprecated", ]
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

    steps <- shiny::reactive({
      shiny::req(input$process_picker, registry())
      s <- registry()$steps[registry()$steps$process_id == input$process_picker, ]
      s[order(s$step_order), ]
    })

    raw_business_events <- shiny::reactive({
      shiny::req(input$process_picker, pv_con(), registry())
      pv_pull_business_events(pv_con(), registry(), input$process_picker)
    })

    business_ideal <- shiny::reactive({
      shiny::req(input$process_picker, registry())
      pv_step_sequence(registry(), input$process_picker)
    })

    shiny::observeEvent(raw_business_events(), {
      events <- raw_business_events()
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

    filtered_business_events <- shiny::reactive({
      events <- raw_business_events()
      shiny::req(input$date_range)
      pv_filter_events(events,
        start_date = input$date_range[1], end_date = input$date_range[2],
        roles = input$role_filter, users = input$user_filter
      )
    })

    final_step_events <- shiny::reactive({
      shiny::req(input$process_picker, pv_con(), registry(), input$date_range)
      raw <- pv_pull_final_step_events(pv_con(), registry(), input$process_picker)
      pv_filter_events(raw,
        start_date = input$date_range[1], end_date = input$date_range[2],
        roles = input$role_filter, users = input$user_filter
      )
    })

    kpis <- shiny::reactive(pv_process_kpis(filtered_business_events(), business_ideal()))
    completion <- shiny::reactive({
      shiny::req(input$process_picker, pv_con(), registry())
      pv_business_outcome_distribution(pv_con(), registry(), input$process_picker, events = final_step_events())
    })

    output$no_data_note <- shiny::renderUI({
      shiny::req(input$process_picker)
      if (nrow(raw_business_events()) == 0) {
        bslib::card(
          bslib::card_body("No events logged yet for this business process.", class = "text-muted")
        )
      }
    })

    output$step_flow <- shiny::renderUI({
      s <- steps()
      shiny::validate(shiny::need(nrow(s) > 0, "No steps declared for this business process yet."))
      subs <- registry()$subprocesses[registry()$subprocesses$process_id == input$process_picker, ]
      lapply(seq_len(nrow(s)), function(i) {
        apps <- subs[subs$step == s$step[i], ]
        apps <- apps[order(apps$app_order), ]
        pv_step_flow_card(s[i, ], apps)
      })
    })

    output$kpi_cases <- shiny::renderText(as.character(kpis()$n_cases))
    output$kpi_duration <- shiny::renderText(pv_format_duration(kpis()$median_duration_mins) %||% "--")
    output$kpi_users <- shiny::renderText(as.character(kpis()$n_users))
    output$kpi_conformance <- shiny::renderText({
      cm <- kpis()$conformance_median
      if (is.na(cm)) "--" else sprintf("%.0f%%", 100 * cm)
    })
    output$kpi_completion <- shiny::renderText({
      dist <- completion()
      if (nrow(dist) == 0) {
        return("--")
      }
      n_total <- sum(dist$n_cases)
      n_incomplete <- sum(dist$n_cases[dist$outcome_id == "incomplete"])
      if (n_total == 0) "--" else sprintf("%.0f%%", 100 * (n_total - n_incomplete) / n_total)
    })

    output$outcomes_table <- DT::renderDT({
      dist <- completion()
      shiny::validate(shiny::need(nrow(dist) > 0, "No outcomes declared for this business process's final step yet."))
      DT::datatable(
        pv_outcomes_display_table(dist),
        rownames = FALSE, escape = FALSE, options = list(dom = "t"), class = "compact stripe"
      )
    })

    app_metrics_subs <- shiny::reactive({
      shiny::req(input$process_picker, registry())
      subs <- registry()$subprocesses[registry()$subprocesses$process_id == input$process_picker, ]
      subs[order(subs$step_order, subs$app_order), ]
    })

    output$app_metrics_table <- DT::renderDT({
      pid <- input$process_picker
      subs <- app_metrics_subs()
      shiny::validate(shiny::need(nrow(subs) > 0, "No apps scoped for this business process yet."))

      rows <- lapply(seq_len(nrow(subs)), function(i) {
        monitored <- !is.na(subs$duckdb_path[i])
        app_kpis <- list(n_cases = NA_integer_, n_users = NA_integer_, median_duration_mins = NA_real_, conformance_median = NA_real_)
        if (monitored) {
          events <- pv_pull_app_events(pv_con(), registry(), pid, subs$subprocess[i])
          events <- pv_filter_events(events,
            start_date = input$date_range[1], end_date = input$date_range[2],
            roles = input$role_filter, users = input$user_filter
          )
          app_ideal <- registry()$ideal[[paste(pid, subs$subprocess[i], sep = "::")]]
          app_kpis <- pv_process_kpis(events, app_ideal)
        }
        data.frame(
          Step = subs$step_label[i], App = subs$app[i],
          Monitored = pv_monitored_badge(monitored),
          Cases = ifelse(is.na(app_kpis$n_cases), "--", as.character(app_kpis$n_cases)),
          Users = ifelse(is.na(app_kpis$n_users), "--", as.character(app_kpis$n_users)),
          `Median duration` = pv_format_duration(app_kpis$median_duration_mins) %||% "--",
          Conformance = if (is.na(app_kpis$conformance_median)) "--" else sprintf("%.0f%%", 100 * app_kpis$conformance_median),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      })
      DT::datatable(
        do.call(rbind, rows), rownames = FALSE, escape = FALSE, selection = "single",
        options = list(dom = "t", pageLength = 20), class = "compact stripe"
      )
    })

    selected_app <- shiny::eventReactive(input$open_app_detail, {
      row <- input$app_metrics_table_rows_selected
      shiny::req(row)
      subs <- app_metrics_subs()
      list(process_id = input$process_picker, subprocess = subs$subprocess[row])
    })

    output$process_map <- shiny::renderUI({
      events <- filtered_business_events()
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
      variants <- pv_variant_table(filtered_business_events(), business_ideal())
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
        colnames = c("Step sequence", "Cases", "Share", "Conformance"),
        rownames = FALSE, escape = FALSE, options = list(pageLength = 5, dom = "tp"),
        class = "compact stripe"
      )
    })

    output$duration_plot <- plotly::renderPlotly({
      durations <- pv_case_durations(filtered_business_events())
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
      weekly <- pv_weekly_usage(filtered_business_events())
      shiny::validate(shiny::need(nrow(weekly) > 0, "No data."))
      plotly::plot_ly(weekly,
        x = ~week_start, y = ~n_cases, type = "scatter", mode = "lines+markers",
        line = list(color = .pv_accent), marker = list(color = .pv_accent)
      ) |>
        plotly::layout(xaxis = list(title = "Week"), yaxis = list(title = "Cases started"))
    })

    list(selected_app = selected_app)
  })
}
