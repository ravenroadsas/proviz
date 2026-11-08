#' @noRd
app_ui <- function() {
  function(request) {
    bslib::page_navbar(
      title = "Process Intelligence",
      id = "main_nav",
      theme = pv_theme(),
      fillable = FALSE, # a long, many-card scrolling page, not a fixed-height single-screen layout
      sidebar = bslib::sidebar(title = "Filters", width = 300, mod_filters_ui("filters")),
      bslib::nav_panel(
        "Business process", value = "business_process",
        bslib::navset_tab(
          id = "business_process_nav",
          bslib::nav_panel("Overview", value = "overview", mod_overview_ui("overview")),
          bslib::nav_panel("Process detail", value = "process_detail", mod_business_detail_ui("business_detail")),
          bslib::nav_panel("App detail", value = "app_detail", mod_app_detail_ui("app_detail"))
        )
      ),
      bslib::nav_panel("Users & roles", value = "heatmap", mod_heatmap_ui("heatmap")),
      bslib::nav_panel("Adoption", value = "adoption", mod_adoption_ui("adoption")),
      bslib::nav_spacer(),
      bslib::nav_item(shiny::span(shiny::textOutput("data_as_of", inline = TRUE), class = "navbar-text me-2 small")),
      bslib::nav_item(shiny::actionButton("refresh_data", "Refresh data",
        icon = shiny::icon("rotate"), class = "btn btn-sm btn-outline-secondary"
      ))
    )
  }
}
