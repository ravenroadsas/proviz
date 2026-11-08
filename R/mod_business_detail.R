#' One app's flow card within a business process's app list.
#' @noRd
pv_app_flow_card <- function(sub_row) {
  shiny::div(
    style = "flex: 1 1 200px; background: #fff; border: 1px solid #e1e0d9; border-radius: 0.5rem; padding: 0.7rem 0.9rem;",
    shiny::div(style = "font-weight: 600; font-size: 0.85rem; margin-bottom: 0.3rem;", sub_row$app),
    shiny::HTML(pv_monitored_badge(!is.na(sub_row$log_path)))
  )
}

#' Business process detail tab: which apps make up this process, their
#' own metrics, and the business-level outcome (decided by whichever
#' app(s) sit at the end of the process's declared flow -- see
#' [pv_final_subprocesses()]).
#'
#' There is no single process-map/variant-sequence view at this level
#' any more: each app has its own step vocabulary (`"validate"`,
#' `"setting params"`, ...), not shared across apps the way a business
#' process's `step` grouping used to be, so that detail lives on the app
#' detail tab instead -- this tab is the roll-up across apps.
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
        shiny::selectInput(ns("process_picker"), "Business process", choices = NULL, width = "100%")
      )
    ),
    shiny::uiOutput(ns("no_data_note")),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Apps in this process"),
      shiny::div(style = "display: flex; flex-wrap: wrap; gap: 0.6rem;", shiny::uiOutput(ns("app_flow")))
    ),
    shiny::div(
      class = "pv-kpi-row",
      pv_kpi_box("Cases", shiny::textOutput(ns("kpi_cases"), inline = TRUE), shiny::icon("diagram-project")),
      pv_kpi_box("Median duration", shiny::textOutput(ns("kpi_duration"), inline = TRUE), shiny::icon("clock")),
      pv_kpi_box("Users", shiny::textOutput(ns("kpi_users"), inline = TRUE), shiny::icon("users")),
      pv_kpi_box("Mean app conformance", shiny::textOutput(ns("kpi_conformance"), inline = TRUE), shiny::icon("check-double")),
      pv_kpi_box("Completed", shiny::textOutput(ns("kpi_completion"), inline = TRUE), shiny::icon("circle-check"))
    ),
    shiny::helpText(
      "\"Mean app conformance\" averages each app's own step-level conformance (see the app detail tab) -- ",
      "there is no single cross-app case identity in the event log to score a whole business-process walk against."
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Outcomes"),
      shiny::helpText(
        style = "padding: 0 0.95rem;",
        "How this business process ended, as decided by the app(s) at the end of its declared flow."
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
    )
  )
}

#' Server logic for the business process detail tab
#'
#' @param id module id.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @param selected_process a reactive giving a `process_id` to jump to (e.g.
#'   from [mod_overview_server()]'s "Open business process"), or `NULL`.
#' @param refresh a reactive (any value) bumped whenever logs should be
#'   re-read.
#' @param date_range,users reactives giving the dashboard's global
#'   sidebar filters (see [mod_filters_server()]).
#' @return a list: `selected_app`, a reactive giving `list(process_id, subprocess)`
#'   for the app detail tab to jump to (`NULL` until a row's "Open app detail"
#'   is clicked).
#' @export
mod_business_detail_server <- function(id, registry, selected_process, refresh = shiny::reactive(NULL),
                                        date_range = shiny::reactive(NULL), users = shiny::reactive(NULL)) {
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

    process_subs <- shiny::reactive({
      shiny::req(input$process_picker, registry())
      registry()$subprocesses[registry()$subprocesses$process_id == input$process_picker, ]
    })

    raw_process_log <- shiny::reactive({
      refresh()
      shiny::req(input$process_picker, registry())
      rng <- date_range()
      pv_pull_process_log(registry(), input$process_picker, start_date = rng[1], end_date = rng[2], users = users())
    })

    kpis <- shiny::reactive({
      log <- raw_process_log()
      subs <- process_subs()
      conf_medians <- vapply(subs$subprocess, function(sp) {
        conf <- pv_subprocess_conformance(registry(), input$process_picker, sp, log = log[log$subprocess == sp, ])
        if (nrow(conf) == 0) NA_real_ else stats::median(conf$score, na.rm = TRUE)
      }, numeric(1))
      list(
        n_cases = if (nrow(log) == 0) 0L else length(unique(log$session_id)),
        n_users = if (nrow(log) == 0) 0L else length(unique(log$user_id)),
        median_duration_mins = if (nrow(log) == 0) NA_real_ else stats::median(pv_case_durations(log)$duration_mins, na.rm = TRUE),
        conformance_median = if (length(conf_medians) == 0 || all(is.na(conf_medians))) NA_real_ else mean(conf_medians, na.rm = TRUE)
      )
    })

    completion <- shiny::reactive({
      shiny::req(input$process_picker, registry())
      rng <- date_range()
      pv_process_outcome_distribution(registry(), input$process_picker, start_date = rng[1], end_date = rng[2], users = users())
    })

    output$no_data_note <- shiny::renderUI({
      shiny::req(input$process_picker)
      if (nrow(raw_process_log()) == 0) {
        bslib::card(
          bslib::card_body("No events logged yet for this business process.", class = "text-muted")
        )
      }
    })

    output$app_flow <- shiny::renderUI({
      subs <- process_subs()
      shiny::validate(shiny::need(nrow(subs) > 0, "No apps scoped for this business process yet."))
      lapply(seq_len(nrow(subs)), function(i) pv_app_flow_card(subs[i, ]))
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
      shiny::validate(shiny::need(nrow(dist) > 0, "No outcomes declared for this business process's final app(s) yet."))
      DT::datatable(
        pv_outcomes_display_table(dist),
        rownames = FALSE, escape = FALSE, options = list(dom = "t"), class = "compact stripe"
      )
    })

    output$app_metrics_table <- DT::renderDT({
      pid <- input$process_picker
      subs <- process_subs()
      shiny::validate(shiny::need(nrow(subs) > 0, "No apps scoped for this business process yet."))

      rows <- lapply(seq_len(nrow(subs)), function(i) {
        monitored <- !is.na(subs$log_path[i])
        app_kpis <- list(n_cases = NA_integer_, n_users = NA_integer_, median_duration_mins = NA_real_, conformance_median = NA_real_)
        if (monitored) {
          rng <- date_range()
          log <- pv_pull_subprocess_log(registry(), pid, subs$subprocess[i], start_date = rng[1], end_date = rng[2], users = users())
          diagram <- registry()$subprocess_diagrams[[paste(pid, subs$subprocess[i], sep = "::")]]
          app_kpis <- pv_process_kpis(log, diagram)
        }
        data.frame(
          App = subs$app[i],
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
      subs <- process_subs()
      list(process_id = input$process_picker, subprocess = subs$subprocess[row])
    })

    list(selected_app = selected_app)
  })
}
