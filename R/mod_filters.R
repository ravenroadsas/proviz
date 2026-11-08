#' Non-empty sentinels for "(All processes)"/"(All Apps)" -- `selectInput`'s
#' selectize widget renders an empty-string value's label as blank
#' instead of showing its text, so these can't just be `""`. Both are
#' the leading (and default) choice in their picker, for consistency --
#' a reader switching between the process and app pickers sees the same
#' "nothing narrowed yet" shape in both, instead of one picker always
#' landing on a concrete value and the other on a placeholder.
#' @noRd
.pv_all_processes_sentinel <- "__all__"
#' @noRd
.pv_all_apps_sentinel <- "__all_apps__"

#' Global sidebar filters: business process, app, date range, role, and users
#'
#' The one filter bar every tab reads from, instead of each tab keeping
#' its own separate process/app/date-range/user picker -- picking "Daily
#' Production Surveillance" here drives Process detail, App detail, and
#' Users & roles alike, with no "wait, which app am I even looking at"
#' mismatch between tabs. Role isn't a column in the real event log
#' itself (see [pv_load_registry()]'s `$user_roles` docs) -- the role
#' multi-select is resolved against `$user_roles` into a set of
#' `user_id`s, intersected with any explicitly selected users, before
#' being handed to every tab as one `users` filter value (see
#' [pv_pull_subprocess_log()]'s `start_date`/`end_date`/`users`
#' passthrough params).
#'
#' Both pickers default to their own "(All ...)" choice -- "(All
#' processes)"/"(All Apps)" -- rather than silently landing on an
#' arbitrary first real process/app, which previously made the two
#' pickers behave inconsistently (one always concrete, the other not)
#' and made it unclear whether a given tab's numbers were scoped by the
#' sidebar or by something else (e.g. the Adoption tab's own IGA-table
#' row selection, a separate mechanism). A tab that can meaningfully
#' show a portfolio-wide view in that state (Users & roles' heatmap)
#' does; a tab that can't (Process detail, App detail) just degrades to
#' its own existing "nothing picked yet" empty state, the same way it
#' already degrades for a process/app with no logged events.
#'
#' @param id module id.
#' @export
mod_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("process_picker"), "Business process", choices = NULL, width = "100%"),
    shiny::selectInput(ns("app_picker"), "App", choices = NULL, width = "100%"),
    shiny::dateRangeInput(ns("date_range"), "Case start date range", width = "100%"),
    shiny::selectizeInput(ns("roles"), "Roles", choices = NULL, multiple = TRUE, width = "100%"),
    shiny::selectizeInput(ns("users"), "Users", choices = NULL, multiple = TRUE, width = "100%")
  )
}

#' Every monitored app, as `app_picker` choices
#'
#' Values are `"<process_id>::<subprocess>"` (unique even if the same
#' app name were ever reused under two processes) -- `"(process)"`
#' is included in the label here since there's no process_picker
#' selection narrowing the list yet.
#' @noRd
pv_filters_all_app_choices <- function(registry) {
  subs <- registry$subprocesses[!is.na(registry$subprocesses$log_path), ]
  if (nrow(subs) == 0) {
    return(character(0))
  }
  proc_labels <- stats::setNames(registry$processes$process, registry$processes$process_id)
  stats::setNames(
    paste(subs$process_id, subs$subprocess, sep = "::"),
    paste0(subs$app, " (", unname(proc_labels[subs$process_id]), ")")
  )
}

#' One process's monitored app(s), as `app_picker` choices (see
#' [pv_filters_all_app_choices()] for the value shape).
#' @noRd
pv_filters_process_app_choices <- function(registry, process_id) {
  subs <- registry$subprocesses[
    registry$subprocesses$process_id == process_id & !is.na(registry$subprocesses$log_path),
  ]
  if (nrow(subs) == 0) {
    return(character(0))
  }
  stats::setNames(paste(subs$process_id, subs$subprocess, sep = "::"), subs$app)
}

#' Server logic for the global sidebar filters
#'
#' @param id module id.
#' @param registry a reactive returning the current [pv_load_registry()] result.
#' @param refresh a reactive (any value) bumped whenever logs should be
#'   re-read -- e.g. the "Refresh data" button's timestamp.
#' @return a list:
#'   \describe{
#'     \item{`date_range`}{a reactive giving the current `c(start, end)` `Date`s.}
#'     \item{`users`}{a reactive giving a character vector of `user_id`s
#'       to keep, or `NULL` when neither a role nor a user is selected --
#'       no filter.}
#'     \item{`process_id`}{a reactive giving the selected `process_id`, or
#'       a sentinel value (never a real `process_id`) for "(All processes)".}
#'     \item{`app`}{a reactive giving `list(process_id, subprocess)` for
#'       the selected app, or `NULL` for "(All Apps)" (the default) or
#'       when nothing is monitored yet -- a tab that needs one concrete
#'       app degrades to its own "nothing picked" empty state either way.}
#'     \item{`set_process`}{a plain function, `set_process(process_id)` --
#'       jumps the shared picker to a process (e.g. from
#'       [mod_overview_server()]'s "Open business process").}
#'     \item{`set_app`}{a plain function, `set_app(list(process_id, subprocess))` --
#'       jumps the shared picker to a specific app (e.g. from
#'       [mod_business_detail_server()]'s "Open app detail").}
#'   }
#' @export
mod_filters_server <- function(id, registry, refresh = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    pending_app_jump <- shiny::reactiveVal(NULL) # list(process_id, subprocess), consumed once by the app_picker cascade below

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

    process_choices <- shiny::reactive({
      shiny::req(registry())
      procs <- registry()$processes[registry()$processes$status != "deprecated", ]
      c("(All processes)" = .pv_all_processes_sentinel, stats::setNames(procs$process_id, procs$process))
    })

    shiny::observe({
      choices <- process_choices()
      shiny::req(length(choices) > 0)
      current <- if (!is.null(input$process_picker) && input$process_picker %in% choices) input$process_picker else choices[1]
      shiny::updateSelectInput(session, "process_picker", choices = choices, selected = current)
    })

    app_choices <- shiny::reactive({
      shiny::req(registry())
      real_choices <- if (identical(input$process_picker, .pv_all_processes_sentinel) || is.null(input$process_picker)) {
        pv_filters_all_app_choices(registry())
      } else {
        pv_filters_process_app_choices(registry(), input$process_picker)
      }
      c("(All Apps)" = .pv_all_apps_sentinel, real_choices)
    })

    shiny::observeEvent(input$process_picker, {
      choices <- app_choices()
      pj <- pending_app_jump()
      target <- NULL
      if (!is.null(pj)) {
        candidate <- paste(pj$process_id, pj$subprocess, sep = "::")
        if (candidate %in% choices) target <- candidate
      }
      current <- if (!is.null(target)) {
        target
      } else if (!is.null(input$app_picker) && input$app_picker %in% choices) {
        input$app_picker
      } else if (length(choices) > 0) {
        choices[1]
      } else {
        character(0)
      }
      shiny::updateSelectInput(session, "app_picker", choices = choices, selected = current)
      pending_app_jump(NULL)
    }, ignoreNULL = TRUE)

    app <- shiny::reactive({
      shiny::req(input$app_picker)
      if (identical(input$app_picker, .pv_all_apps_sentinel)) {
        return(NULL)
      }
      parts <- strsplit(input$app_picker, "::", fixed = TRUE)[[1]]
      list(process_id = parts[1], subprocess = parts[2])
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

    list(
      date_range = shiny::reactive(input$date_range),
      users = effective_users,
      process_id = shiny::reactive(input$process_picker),
      app = app,
      set_process = function(process_id) {
        shiny::updateSelectInput(session, "process_picker", selected = process_id)
      },
      set_app = function(sel) {
        if (identical(input$process_picker, sel$process_id)) {
          # The process isn't changing, so the process_picker
          # observeEvent below (which is what normally consumes a
          # pending jump) won't re-fire -- update app_picker directly.
          choices <- app_choices()
          candidate <- paste(sel$process_id, sel$subprocess, sep = "::")
          shiny::updateSelectInput(session, "app_picker", selected = if (candidate %in% choices) candidate else choices[1])
        } else {
          pending_app_jump(sel)
          shiny::updateSelectInput(session, "process_picker", selected = sel$process_id)
        }
      }
    )
  })
}
