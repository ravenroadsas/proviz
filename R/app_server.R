#' Safely load the adoption workbook, if the registry declares one --
#' `NULL` (not an error) if there's no `adoption_source`, the file is
#' missing, or it fails to parse, so a stale/broken workbook never takes
#' down the rest of the dashboard.
#' @noRd
pv_load_adoption_safe <- function(registry) {
  src <- registry$adoption_source
  if (is.null(src) || !file.exists(src)) {
    return(NULL)
  }
  tryCatch(
    pv_load_adoption(src, registry),
    error = function(e) {
      warning("proviz: failed to load adoption workbook '", src, "': ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

#' @noRd
app_server <- function(initial_registry) {
  function(input, output, session) {
    registry <- shiny::reactiveVal(initial_registry)
    data_as_of <- shiny::reactiveVal(Sys.time())
    pv_con <- shiny::reactiveVal(pv_connect_registry(initial_registry))
    adoption <- shiny::reactiveVal(pv_load_adoption_safe(initial_registry))

    session$onSessionEnded(function() {
      tryCatch(pv_disconnect(shiny::isolate(pv_con())), error = function(e) NULL)
    })

    shiny::observeEvent(input$refresh_data, {
      tryCatch(pv_disconnect(pv_con()), error = function(e) NULL)
      pv_con(pv_connect_registry(registry()))
      adoption(pv_load_adoption_safe(registry()))
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
    mod_adoption_server("adoption", pv_con = pv_con, registry = registry, adoption = adoption)

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
