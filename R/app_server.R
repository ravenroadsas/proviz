#' @noRd
app_server <- function(initial_registry) {
  function(input, output, session) {
    registry <- shiny::reactiveVal(initial_registry)
    data_as_of <- shiny::reactiveVal(Sys.time())
    pv_con <- shiny::reactiveVal(pv_connect_registry(initial_registry))

    session$onSessionEnded(function() {
      tryCatch(pv_disconnect(shiny::isolate(pv_con())), error = function(e) NULL)
    })

    shiny::observeEvent(input$refresh_data, {
      tryCatch(pv_disconnect(pv_con()), error = function(e) NULL)
      pv_con(pv_connect_registry(registry()))
      data_as_of(Sys.time())
    })

    output$data_as_of <- shiny::renderText({
      paste("Data as of", format(data_as_of(), "%Y-%m-%d %H:%M"))
    })

    overview <- mod_overview_server("overview", pv_con = pv_con, registry = registry)
    mod_deepdive_server("deepdive", pv_con = pv_con, registry = registry, selected_process = overview$selected_process)

    shiny::observeEvent(overview$selected_process(), {
      shiny::req(overview$selected_process())
      bslib::nav_select("main_nav", "deepdive", session = session)
    })
  }
}
