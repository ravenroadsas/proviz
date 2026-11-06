#' Launch the process intelligence dashboard
#'
#' Loads the process registry (a real one, or synthetic demo data), attaches
#' every instrumented app's DuckDB store (see [pv_connect_registry()]), and
#' runs the four-tab Shiny dashboard: a portfolio overview of every
#' registered business process, a business-process detail tab (its step
#' flow and business-level process mining), an app detail tab (one app's
#' own activity-level mining), and a users/roles heatmap across the whole
#' portfolio.
#'
#' @param registry_path path to a registry YAML (see [pv_load_registry()]).
#'   Ignored if `demo = TRUE`. Defaults to the registry shipped with the
#'   package.
#' @param demo if `TRUE`, ignore `registry_path` and run against synthetic
#'   data from [pv_demo_registry()] instead -- for trying the dashboard out
#'   without a real shared mount or real application data.
#' @param ... passed on to `shiny::runApp()` (e.g. `port`, `launch.browser`).
#' @return does not return; runs the Shiny app.
#' @export
run_pi_dashboard <- function(registry_path = NULL, demo = FALSE, ...) {
  registry <- if (demo) {
    pv_demo_registry()
  } else {
    pv_load_registry(registry_path %||% system.file("registry/processes.yml", package = "proviz"))
  }

  app <- shiny::shinyApp(ui = app_ui(), server = app_server(registry))
  shiny::runApp(app, ...)
}
