#' Pull one process's raw events from its attached DuckDB stores
#'
#' Queries every subprocess/app attached for `process_id` (see
#' [pv_connect_registry()]), unions the results, and resolves the
#' provisional-id/entity-id case-identity link (see [pv_link_cases()]) --
#' the single most important mechanic in the playbook's case-identity
#' scheme, and something every consumer of this data needs done
#' consistently, not re-implemented per KPI.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry the same [pv_load_registry()] result `pv_con` was built
#'   from (needed for the `macroprocess`/`process` values to filter on).
#' @param process_id a `process_id` from `registry$processes`.
#' @return a `data.frame`, one row per logged event, case-linked: `case_id`
#'   (already remapped -- see [pv_link_cases()]), `entity_id`,
#'   `activity_id`, `timestamp`, `user_id`, `role`, `session_id`, `app`,
#'   `subprocess`. Zero rows (not an error) if no subprocess for this
#'   process attached successfully.
#' @export
pv_pull_process_events <- function(pv_con, registry, process_id) {
  stopifnot(inherits(pv_con, "pv_connection"), inherits(registry, "pv_registry"))

  proc_row <- registry$processes[registry$processes$process_id == process_id, ]
  if (nrow(proc_row) == 0) {
    stop("proviz: process_id '", process_id, "' not found in registry", call. = FALSE)
  }

  subs <- pv_con$subprocesses[pv_con$subprocesses$process_id == process_id & pv_con$subprocesses$attached, ]
  if (nrow(subs) == 0) {
    return(pv_empty_events())
  }

  selects <- vapply(seq_len(nrow(subs)), function(i) {
    sprintf(
      "SELECT case_id, entity_id, activity_id, occurred_at AS timestamp,
              user_id, role, session_id, %s AS app, %s AS subprocess
       FROM %s.protrace_event
       WHERE macroprocess = %s AND process = %s AND subprocess = %s",
      DBI::dbQuoteString(pv_con$con, subs$app[i]), DBI::dbQuoteString(pv_con$con, subs$subprocess[i]),
      DBI::dbQuoteIdentifier(pv_con$con, subs$app[i]),
      DBI::dbQuoteString(pv_con$con, proc_row$macroprocess), DBI::dbQuoteString(pv_con$con, proc_row$process),
      DBI::dbQuoteString(pv_con$con, subs$subprocess[i])
    )
  }, character(1))

  events <- DBI::dbGetQuery(pv_con$con, paste(selects, collapse = "\nUNION ALL\n"))
  pv_link_cases(events)
}

#' Pull one app's raw events from its attached DuckDB store
#'
#' Like [pv_pull_process_events()], but scoped to exactly one app/step
#' within a business process rather than unioned across every app the
#' process spans -- what the app-detail tab needs (one app's own
#' activity-level trace), as opposed to the business-process-level step
#' trace [pv_events_at_step_level()] builds from the same union
#' [pv_pull_process_events()] already returns.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry the same [pv_load_registry()] result `pv_con` was built
#'   from (needed for the `macroprocess`/`process` values to filter on).
#' @param process_id a `process_id` from `registry$processes`.
#' @param subprocess the app's `subprocess` value, from `registry$subprocesses`.
#' @return a `data.frame`, the same shape as [pv_pull_process_events()]'s,
#'   case-linked, scoped to just this one app. Zero rows if this app isn't
#'   attached.
#' @export
pv_pull_app_events <- function(pv_con, registry, process_id, subprocess) {
  stopifnot(inherits(pv_con, "pv_connection"), inherits(registry, "pv_registry"))

  proc_row <- registry$processes[registry$processes$process_id == process_id, ]
  if (nrow(proc_row) == 0) {
    stop("proviz: process_id '", process_id, "' not found in registry", call. = FALSE)
  }

  subs <- pv_con$subprocesses[
    pv_con$subprocesses$process_id == process_id &
      pv_con$subprocesses$subprocess == subprocess &
      pv_con$subprocesses$attached,
  ]
  if (nrow(subs) == 0) {
    return(pv_empty_events())
  }

  sql <- sprintf(
    "SELECT case_id, entity_id, activity_id, occurred_at AS timestamp,
            user_id, role, session_id, %s AS app, %s AS subprocess
     FROM %s.protrace_event
     WHERE macroprocess = %s AND process = %s AND subprocess = %s",
    DBI::dbQuoteString(pv_con$con, subs$app[1]), DBI::dbQuoteString(pv_con$con, subs$subprocess[1]),
    DBI::dbQuoteIdentifier(pv_con$con, subs$app[1]),
    DBI::dbQuoteString(pv_con$con, proc_row$macroprocess), DBI::dbQuoteString(pv_con$con, proc_row$process),
    DBI::dbQuoteString(pv_con$con, subs$subprocess[1])
  )

  events <- DBI::dbGetQuery(pv_con$con, sql)
  pv_link_cases(events)
}

#' @noRd
pv_empty_events <- function() {
  data.frame(
    case_id = character(0), entity_id = character(0), activity_id = character(0),
    timestamp = as.POSIXct(character(0)), user_id = character(0), role = character(0),
    session_id = character(0), app = character(0), subprocess = character(0),
    stringsAsFactors = FALSE
  )
}

#' Resolve provisional-case-id / entity-id links into one case identity
#'
#' Implements the playbook's case-identity scheme: a case may start before
#' a permanent identifier exists, tagged with a provisional id (`"prov_"`
#' prefixed, from `protrace::pt_new_case_id()`) and `entity_id = NA`. The
#' event that creates the permanent record carries **both** -- `case_id`
#' still the provisional one, `entity_id` now the real one. That row is the
#' link: every row sharing that provisional `case_id` (before *and*
#' including the link row) gets remapped to the entity id, so it reads as
#' one continuous case. A provisional id with no such link row was never
#' saved -- left alone, it stays its own short case rather than being
#' silently dropped, since that's a real "abandoned" outcome worth seeing
#' in the data, not an error.
#'
#' @param events a `data.frame` with at least `case_id` and `entity_id`
#'   columns (as returned by a `protrace_event` query).
#' @return `events`, with `case_id` remapped for every row belonging to a
#'   linked provisional case.
#' @export
pv_link_cases <- function(events) {
  stopifnot(is.data.frame(events), all(c("case_id", "entity_id") %in% names(events)))
  if (nrow(events) == 0) {
    return(events)
  }

  is_provisional <- grepl("^prov_", events$case_id)
  link_rows <- events[is_provisional & !is.na(events$entity_id), c("case_id", "entity_id")]
  link_rows <- link_rows[!duplicated(link_rows$case_id), ]

  map <- stats::setNames(link_rows$entity_id, link_rows$case_id)
  linked <- events$case_id %in% names(map)
  events$case_id[linked] <- unname(map[events$case_id[linked]])
  events
}

#' Wrap a process's raw event rows as a bupaR eventlog
#'
#' Every row is a single instant, already-settled action
#' (`lifecycle_id = "complete"` for all of them) -- a real protrace log is
#' an *eventlog*, not an *activitylog* with start+complete pairs (see the
#' playbook's caveat on what this means for duration analysis: elapsed
#' time to the next action, not a measured task duration).
#'
#' @param events a case-linked events `data.frame`, as returned by
#'   [pv_pull_process_events()] (or hand-built with the same columns for
#'   testing): `case_id`, `activity_id`, `timestamp`, `user_id`.
#' @return a `bupaR::eventlog`.
#' @export
pv_build_eventlog <- function(events) {
  stopifnot(is.data.frame(events), all(c("case_id", "activity_id", "timestamp", "user_id") %in% names(events)))
  events$activity_instance_id <- seq_len(nrow(events))
  events$lifecycle_id <- "complete"

  bupaR::eventlog(
    events,
    case_id = "case_id", activity_id = "activity_id", activity_instance_id = "activity_instance_id",
    lifecycle_id = "lifecycle_id", timestamp = "timestamp", resource_id = "user_id"
  )
}
