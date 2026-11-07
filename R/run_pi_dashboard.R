#' Launch the process intelligence dashboard
#'
#' Loads the process registry (a real one, or synthetic demo data) and
#' runs the five-tab Shiny dashboard, reading each app's own event log
#' straight from its `log_path` (see [pv_pull_subprocess_log()]): a
#' portfolio overview of every registered business process, a
#' business-process detail tab (the apps that make it up, rolled up), an
#' app detail tab (one app's own step-level mining), a users heatmap
#' (plus per-user/app engagement-score correlations) across the whole
#' portfolio, and an adoption tab (the IGA index per app, from the
#' registry's `adoption_source` workbook -- see [pv_load_adoption()]).
#'
#' @param registry_path path to a registry DuckDB file (see
#'   [pv_load_registry()]). Ignored if `demo = TRUE`. Defaults to the
#'   registry shipped with the package.
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
    pv_load_registry(registry_path %||% system.file("registry/registry.duckdb", package = "proviz"))
  }

  app <- shiny::shinyApp(ui = app_ui(), server = app_server(registry))
  shiny::runApp(app, ...)
}
