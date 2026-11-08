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
    adoption <- shiny::reactiveVal(pv_load_adoption_safe(initial_registry))

    shiny::observeEvent(input$refresh_data, {
      adoption(pv_load_adoption_safe(registry()))
      data_as_of(Sys.time())
    })

    output$data_as_of <- shiny::renderText({
      paste("Data as of", format(data_as_of(), "%Y-%m-%d %H:%M"))
    })

    filters <- mod_filters_server("filters", registry = registry, refresh = data_as_of)

    overview <- mod_overview_server("overview",
      registry = registry, refresh = data_as_of, date_range = filters$date_range, users = filters$users
    )
    business_detail <- mod_business_detail_server("business_detail",
      registry = registry, refresh = data_as_of,
      date_range = filters$date_range, users = filters$users, process_id = filters$process_id
    )
    mod_app_detail_server("app_detail",
      registry = registry, refresh = data_as_of,
      date_range = filters$date_range, users = filters$users, app = filters$app
    )
    mod_heatmap_server("heatmap",
      registry = registry, refresh = data_as_of, date_range = filters$date_range, users = filters$users,
      process_id = filters$process_id, app = filters$app
    )
    mod_adoption_server("adoption", registry = registry, adoption = adoption, refresh = data_as_of)

    shiny::observeEvent(overview$selected_process(), {
      shiny::req(overview$selected_process())
      filters$set_process(overview$selected_process())
      bslib::nav_select("main_nav", "business_process", session = session)
      bslib::nav_select("business_process_nav", "process_detail", session = session)
    })

    shiny::observeEvent(business_detail$selected_app(), {
      shiny::req(business_detail$selected_app())
      filters$set_app(business_detail$selected_app())
      bslib::nav_select("main_nav", "business_process", session = session)
      bslib::nav_select("business_process_nav", "app_detail", session = session)
    })
  }
}
