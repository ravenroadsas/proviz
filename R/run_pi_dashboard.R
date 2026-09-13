#' Launch the process intelligence dashboard
#'
#' Loads the process registry (a real one, or synthetic demo data), attaches
#' every instrumented process's DuckDB store (see [pv_connect_registry()]),
#' and runs the two-tab Shiny dashboard: an overview of every registered
#' process with coverage/KPI cards, and a deep-dive tab for exploring one
#' process's map, variants, timing, usage, and conformance in detail.
#'
#' @param registry_path path to a registry YAML (see [pv_load_registry()]).
#'   Ignored if `demo = TRUE`. Defaults to the registry shipped with the
#'   package.
#' @param demo if `TRUE`, ignore `registry_path` and run against synthetic
#'   data from [pv_demo_registry()] instead -- for trying the dashboard out
#'   without a real shared mount or real application data.
#' @param bootswatch `"flatly"` (default) or `"cosmos"` -- both light,
#'   compact, professional bootswatch themes.
#' @param ... passed on to `shiny::runApp()` (e.g. `port`, `launch.browser`).
#' @return does not return; runs the Shiny app.
#' @export
run_pi_dashboard <- function(registry_path = NULL, demo = FALSE, bootswatch = "flatly", ...) {
  registry <- if (demo) {
    pv_demo_registry()
  } else {
    pv_load_registry(registry_path %||% system.file("registry/processes.yml", package = "proviz"))
  }

  app <- shiny::shinyApp(ui = app_ui(bootswatch), server = app_server(registry))
  shiny::runApp(app, ...)
}
