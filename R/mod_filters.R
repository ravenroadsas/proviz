#' Global sidebar filters: date range, role, and users
#'
#' The one filter bar every tab reads from, instead of each tab keeping
#' its own separate date-range/user picker -- a reader filtering to "last
#' month, Reservoir Engineers only" sees that same scope no matter which
#' tab is open. Role isn't a column in the real event log itself (see
#' [pv_load_registry()]'s `$user_roles` docs) -- the role multi-select is
#' resolved against `$user_roles` into a set of `user_id`s, intersected
#' with any explicitly selected users, before being handed to every tab
#' as one `users` filter value (see [pv_pull_subprocess_log()]'s
#' `start_date`/`end_date`/`users` passthrough params).
#'
#' @param id module id.
#' @export
mod_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::dateRangeInput(ns("date_range"), "Case start date range", width = "100%"),
    shiny::selectizeInput(ns("roles"), "Roles", choices = NULL, multiple = TRUE, width = "100%"),
    shiny::selectizeInput(ns("users"), "Users", choices = NULL, multiple = TRUE, width = "100%")
  )
}

#' Server logic for the global sidebar filters
#'
#' @param id module id.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @param refresh a reactive (any value) bumped whenever logs should be
#'   re-read -- e.g. the "Refresh data" button's timestamp.
#' @return a list: `date_range` (a reactive giving the current
#'   `c(start, end)` `Date`s), `users` (a reactive giving a character
#'   vector of `user_id`s to keep, or `NULL` when neither a role nor a
#'   user is selected -- no filter).
#' @export
mod_filters_server <- function(id, registry, refresh = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    portfolio_log <- shiny::reactive({
      refresh()
      shiny::req(registry())
      pv_pull_portfolio_log(registry())
    })

    all_users <- shiny::reactive({
      shiny::req(registry())
      sort(union(unique(portfolio_log()$user_id), registry()$user_roles$user_id))
    })

    shiny::observeEvent(portfolio_log(), {
      log <- portfolio_log()
      if (nrow(log) == 0) {
        return()
      }
      rng <- range(as.Date(log$started_at))
      shiny::updateDateRangeInput(session, "date_range", start = rng[1], end = rng[2], min = rng[1], max = rng[2])
    })

    shiny::observe({
      shiny::updateSelectizeInput(session, "users", choices = all_users(), selected = input$users)
    })

    shiny::observe({
      shiny::req(registry())
      shiny::updateSelectizeInput(session, "roles", choices = sort(unique(registry()$user_roles$role)), selected = input$roles)
    })

    effective_users <- shiny::reactive({
      has_role_filter <- length(input$roles) > 0
      has_user_filter <- length(input$users) > 0
      if (!has_role_filter && !has_user_filter) {
        return(NULL)
      }
      candidates <- if (has_user_filter) input$users else all_users()
      if (has_role_filter) {
        role_users <- registry()$user_roles$user_id[registry()$user_roles$role %in% input$roles]
        candidates <- intersect(candidates, role_users)
      }
      # A role/user combination that matches nobody must still filter
      # everything out, not fall through to pv_filter_log()'s "no users
      # given" no-op -- a sentinel that can never equal a real user_id.
      if (length(candidates) == 0) "<proviz:no-match>" else candidates
    })

    list(date_range = shiny::reactive(input$date_range), users = effective_users)
  })
}
