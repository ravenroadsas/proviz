#' @noRd
app_ui <- function() {
  function(request) {
    bslib::page_navbar(
      title = "Process Intelligence",
      id = "main_nav",
      theme = pv_theme(),
      fillable = FALSE, # a long, many-card scrolling page, not a fixed-height single-screen layout
      bslib::nav_panel("Overview", value = "overview", mod_overview_ui("overview")),
      bslib::nav_panel("Deep dive", value = "deepdive", mod_deepdive_ui("deepdive")),
      bslib::nav_spacer(),
      bslib::nav_item(shiny::span(shiny::textOutput("data_as_of", inline = TRUE), class = "navbar-text me-2 small")),
      bslib::nav_item(shiny::actionButton("refresh_data", "Refresh data",
        icon = shiny::icon("rotate"), class = "btn btn-sm btn-outline-secondary"
      ))
    )
  }
}
