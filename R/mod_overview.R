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

#' Overview tab: registry-wide KPIs and the process list
#'
#' @param id module id.
#' @export
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "pv-kpi-row",
      pv_kpi_box("Coverage", shiny::textOutput(ns("kpi_coverage"), inline = TRUE), shiny::icon("chart-pie")),
      pv_kpi_box("Processes registered", shiny::textOutput(ns("kpi_registered"), inline = TRUE), shiny::icon("list-check")),
      pv_kpi_box("Instrumented", shiny::textOutput(ns("kpi_instrumented"), inline = TRUE), shiny::icon("plug")),
      pv_kpi_box("Active cases", shiny::textOutput(ns("kpi_cases"), inline = TRUE), shiny::icon("diagram-project")),
      pv_kpi_box("Distinct users", shiny::textOutput(ns("kpi_users"), inline = TRUE), shiny::icon("users"))
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Process registry"),
      DT::DTOutput(ns("registry_table"), fill = FALSE),
      shiny::div(
        style = "margin-top: 8px;",
        shiny::actionButton(ns("open_deepdive"), "Open deep-dive ->", class = "btn-primary btn-sm"),
        shiny::span(
          "Select a row above first.",
          style = "margin-left: 8px; color: #868e96; font-size: 0.85em;"
        )
      )
    )
  )
}

#' Server logic for the overview tab
#'
#' @param id module id.
#' @param pv_con a reactive returning the current [pv_connect_registry()] result.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @return a list: `selected_process`, a reactive giving the `process_id`
#'   the user asked to deep-dive into (`NULL` until "Open deep-dive" is
#'   clicked).
#' @export
mod_overview_server <- function(id, pv_con, registry) {
  shiny::moduleServer(id, function(input, output, session) {
    kpi_grid <- shiny::reactive({
      shiny::req(pv_con(), registry())
      pv_registry_kpis(pv_con(), registry())
    })

    coverage <- shiny::reactive({
      shiny::req(registry())
      pv_coverage(registry())
    })

    output$kpi_coverage <- shiny::renderText({
      cov <- coverage()
      sprintf("%.0f%%", 100 * (cov$pct %||% 0))
    })
    output$kpi_registered <- shiny::renderText(as.character(coverage()$total))
    output$kpi_instrumented <- shiny::renderText(as.character(coverage()$instrumented))
    output$kpi_cases <- shiny::renderText(as.character(sum(kpi_grid()$n_cases, na.rm = TRUE)))
    output$kpi_users <- shiny::renderText(as.character(sum(kpi_grid()$n_users, na.rm = TRUE)))

    display_table <- shiny::reactive({
      grid <- kpi_grid()
      data.frame(
        process_id = grid$process_id,
        Process = grid$process,
        Macroprocess = grid$macroprocess,
        Owner = grid$owner,
        Status = vapply(grid$status, pv_status_badge, character(1)),
        Runs = grid$n_cases,
        `Median duration` = pv_format_duration(grid$median_duration_mins),
        Users = grid$n_users,
        Conformance = ifelse(is.na(grid$conformance_median), NA_character_,
          sprintf("%.0f%%", 100 * grid$conformance_median)
        ),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })

    output$registry_table <- DT::renderDT({
      DT::datatable(
        display_table()[, -1], # hide process_id, kept for row-selection lookup
        selection = "single", rownames = FALSE, escape = FALSE,
        options = list(dom = "tp", pageLength = 10, autoWidth = TRUE)
      )
    })

    selected_process <- shiny::eventReactive(input$open_deepdive, {
      row <- input$registry_table_rows_selected
      shiny::req(row)
      display_table()$process_id[row]
    })

    list(selected_process = selected_process)
  })
}
