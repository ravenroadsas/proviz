#' @noRd
pv_loop_label <- function(loop_length) {
  c(short = "Short loop", medium = "Medium loop", long = "Long loop")[loop_length]
}

#' Comma-joined app names per process, from the registry's own
#' `subprocesses` list -- populated as soon as app(s) are scoped for a
#' process (registration time), not only once instrumented.
#' @noRd
pv_apps_by_process <- function(subprocesses, process_ids) {
  apps <- vapply(process_ids, function(pid) {
    row_apps <- unique(subprocesses$app[subprocesses$process_id == pid])
    if (length(row_apps) == 0) NA_character_ else paste(row_apps, collapse = ", ")
  }, character(1))
  unname(apps)
}

#' @noRd
pv_format_duration <- function(mins) {
  vapply(mins, function(m) {
    if (is.na(m)) {
      return(NA_character_)
    }
    if (m < 60) {
      return(sprintf("%.0f min", m))
    }
    sprintf("%.1f hr", m / 60)
  }, character(1))
}

#' Portfolio overview tab: portfolio-wide KPIs and the business-process list
#'
#' The dashboard's landing tab -- a portfolio-of-processes view, not a
#' single process's own detail (see [mod_business_detail_ui()] for that):
#' how many business processes are registered, how much of their declared
#' step map is actually monitored, and usage/completion totals across the
#' whole portfolio.
#'
#' @param id module id.
#' @export
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "pv-kpi-row",
      pv_kpi_box("Business processes", shiny::textOutput(ns("kpi_processes"), inline = TRUE), shiny::icon("sitemap")),
      pv_kpi_box("Instrumented", shiny::textOutput(ns("kpi_instrumented"), inline = TRUE), shiny::icon("plug")),
      pv_kpi_box("Steps monitored", shiny::textOutput(ns("kpi_steps"), inline = TRUE), shiny::icon("list-check")),
      pv_kpi_box("Active cases", shiny::textOutput(ns("kpi_cases"), inline = TRUE), shiny::icon("diagram-project")),
      pv_kpi_box("Distinct users", shiny::textOutput(ns("kpi_users"), inline = TRUE), shiny::icon("users")),
      pv_kpi_box("Weekly active users", shiny::textOutput(ns("kpi_weekly_users"), inline = TRUE), shiny::icon("calendar-week")),
      pv_kpi_box("Completed", shiny::textOutput(ns("kpi_completion"), inline = TRUE), shiny::icon("circle-check"))
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Business process portfolio"),
      DT::DTOutput(ns("registry_table"), fill = FALSE),
      shiny::div(
        style = "margin-top: 8px;",
        shiny::actionButton(ns("open_detail"), "Open business process ->", class = "btn-primary btn-sm"),
        shiny::span(
          "Select a row above first.",
          style = "margin-left: 8px; color: #868e96; font-size: 0.85em;"
        )
      )
    )
  )
}

#' Server logic for the portfolio overview tab
#'
#' @param id module id.
#' @param pv_con a reactive returning the current [pv_connect_registry()] result.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @return a list: `selected_process`, a reactive giving the `process_id`
#'   the user asked to open ("Open business process" clicked). `NULL`
#'   until clicked.
#' @export
mod_overview_server <- function(id, pv_con, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    kpi_grid <- shiny::reactive({
      shiny::req(pv_con(), registry())
      pv_registry_kpis(pv_con(), registry())
    })

    portfolio <- shiny::reactive({
      shiny::req(pv_con(), registry())
      pv_portfolio_kpis(pv_con(), registry())
    })

    output$kpi_processes <- shiny::renderText(as.character(portfolio()$n_processes))
    output$kpi_instrumented <- shiny::renderText(as.character(portfolio()$n_instrumented))
    output$kpi_steps <- shiny::renderText({
      p <- portfolio()
      sprintf("%d / %d", p$n_steps_monitored, p$n_steps)
    })
    output$kpi_cases <- shiny::renderText(as.character(portfolio()$n_cases))
    output$kpi_users <- shiny::renderText(as.character(portfolio()$n_users))
    output$kpi_weekly_users <- shiny::renderText(as.character(portfolio()$n_weekly_users))
    output$kpi_completion <- shiny::renderText({
      pct <- portfolio()$completion_pct
      if (is.na(pct)) "--" else sprintf("%.0f%%", pct)
    })

    display_table <- shiny::reactive({
      grid <- kpi_grid()

      # Runs has no natural 0-100 scale (a raw case count) -- bar length is
      # scaled relative to the busiest instrumented process in this table,
      # so the row-to-row comparison ("who's actually used the most") is
      # the thing the bar shows, not an absolute unit.
      max_runs <- suppressWarnings(max(grid$n_cases, na.rm = TRUE))
      runs_pct <- if (is.finite(max_runs) && max_runs > 0) {
        100 * grid$n_cases / max_runs
      } else {
        rep(NA_real_, length(grid$n_cases))
      }
      conformance_pct <- round(100 * grid$conformance_median)
      conformance_label <- ifelse(is.na(conformance_pct), NA_character_, paste0(conformance_pct, "%"))
      completion_label <- ifelse(is.na(grid$completion_pct), NA_character_, paste0(round(grid$completion_pct), "%"))
      steps_label <- ifelse(is.na(grid$n_steps), NA_character_, sprintf("%d / %d", grid$n_steps_monitored, grid$n_steps))

      data.frame(
        process_id = grid$process_id,
        Process = grid$process,
        Macroprocess = grid$macroprocess,
        Applications = pv_apps_by_process(registry()$subprocesses, grid$process_id),
        Owner = grid$owner,
        Status = vapply(grid$status, pv_status_badge, character(1)),
        Loop = vapply(pv_loop_label(grid$loop_length), pv_type_badge, character(1), USE.NAMES = FALSE),
        Steps = vapply(steps_label, pv_type_badge, character(1), USE.NAMES = FALSE),
        Runs = vapply(seq_len(nrow(grid)), function(i) pv_cell_bar(as.character(grid$n_cases[i]), runs_pct[i]), character(1)),
        `Median duration` = pv_format_duration(grid$median_duration_mins),
        Users = grid$n_users,
        Conformance = vapply(seq_len(nrow(grid)), function(i) pv_cell_bar(conformance_label[i], conformance_pct[i]), character(1)),
        Completion = vapply(seq_len(nrow(grid)), function(i) pv_cell_bar(completion_label[i], grid$completion_pct[i]), character(1)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })

    output$registry_table <- DT::renderDT({
      DT::datatable(
        display_table()[, -1], # hide process_id, kept for row-selection lookup
        selection = "single", rownames = FALSE, escape = FALSE,
        options = list(dom = "tp", pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
      )
    })

    selected_process <- shiny::eventReactive(input$open_detail, {
      row <- input$registry_table_rows_selected
      shiny::req(row)
      display_table()$process_id[row]
    })

    list(selected_process = selected_process)
  })
}
