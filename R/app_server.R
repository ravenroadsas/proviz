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
    business_detail <- mod_business_detail_server("business_detail",
      pv_con = pv_con, registry = registry, selected_process = overview$selected_process
    )
    mod_app_detail_server("app_detail",
      pv_con = pv_con, registry = registry, selected_app = business_detail$selected_app
    )
    mod_heatmap_server("heatmap", pv_con = pv_con, registry = registry)

    shiny::observeEvent(overview$selected_process(), {
      shiny::req(overview$selected_process())
      bslib::nav_select("main_nav", "business_detail", session = session)
    })

    shiny::observeEvent(business_detail$selected_app(), {
      shiny::req(business_detail$selected_app())
      bslib::nav_select("main_nav", "app_detail", session = session)
    })
  }
}
