#' Pull one subprocess's raw event log
#'
#' Reads the fourth artifact -- the actual recorded activity -- straight
#' from the app's own `log_path` (a CSV today, a parquet file or glob
#' tomorrow; both read the same way via DuckDB). This shape is fixed by
#' the real recorder, not proviz's to redesign: one row per `(session,
#' step)` bucket actually touched, with the click count and weight total
#' already summed by the recorder, not raw per-click rows --
#' `app_id`, `app_name`, `session_id`, `username`, `process` (the step
#' id), `starttime` (first time this step fired in this session),
#' `nclicks`, `total_weight`.
#'
#' @param registry a [pv_load_registry()] result.
#' @param process_id,subprocess identify the app via its registry row.
#' @return a `data.frame`: `session_id`, `user_id`, `step_id`,
#'   `started_at` (`POSIXct`), `nclicks`, `total_weight`, `app`. Zero rows
#'   if this subprocess has no declared `log_path`, or the file doesn't
#'   exist (an app not yet instrumented, or a mount that isn't live --
#'   consistent with how the dashboard treats an unreachable store
#'   elsewhere: skip it, don't fail the whole page).
#' @export
pv_pull_subprocess_log <- function(registry, process_id, subprocess) {
  stopifnot(inherits(registry, "pv_registry"))
  row <- registry$subprocesses[
    registry$subprocesses$process_id == process_id & registry$subprocesses$subprocess == subprocess,
  ]
  if (nrow(row) == 0) {
    stop("proviz: subprocess '", subprocess, "' not found under process_id '", process_id, "'", call. = FALSE)
  }

  log_path <- row$log_path[1]
  if (is.na(log_path) || !file.exists(log_path)) {
    return(pv_empty_log())
  }

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  reader <- if (grepl("\\.parquet$", log_path, ignore.case = TRUE)) "read_parquet" else "read_csv_auto"
  raw <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s(%s)", reader, DBI::dbQuoteString(con, log_path)))
  if (nrow(raw) == 0) {
    return(pv_empty_log())
  }

  data.frame(
    session_id = as.character(raw$session_id), user_id = as.character(raw$username),
    step_id = as.character(raw$process), started_at = lubridate::ymd_hms(as.character(raw$starttime), tz = "UTC", quiet = TRUE),
    nclicks = as.integer(raw$nclicks), total_weight = as.numeric(raw$total_weight),
    app = as.character(raw$app_id), stringsAsFactors = FALSE
  )
}

#' @noRd
pv_empty_log <- function() {
  data.frame(
    session_id = character(0), user_id = character(0), step_id = character(0),
    started_at = as.POSIXct(character(0), tz = "UTC"), nclicks = integer(0), total_weight = numeric(0),
    app = character(0), stringsAsFactors = FALSE
  )
}

#' Pull every subprocess of a process's log, unioned
#'
#' The process-level counterpart to [pv_pull_subprocess_log()] -- every
#' app in `process_id` contributes its own rows, `subprocess`-tagged so a
#' caller can tell which app each row came from. There's no cross-app
#' case identity in this log shape (no shared entity/case key across
#' apps, only a per-app `session_id`), so this is a union for
#' portfolio-wide tallies (distinct users, total score), not a basis for
#' process-level conformance -- see [pv_load_registry()]'s
#' `$process_diagrams` docs for that gap.
#'
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a `data.frame`, [pv_pull_subprocess_log()]'s shape plus
#'   `subprocess`. Zero rows if no subprocess under this process has a
#'   reachable log yet.
#' @export
pv_pull_process_log <- function(registry, process_id) {
  subs <- registry$subprocesses[registry$subprocesses$process_id == process_id, ]
  rows <- lapply(seq_len(nrow(subs)), function(i) {
    log <- pv_pull_subprocess_log(registry, process_id, subs$subprocess[i])
    if (nrow(log) == 0) {
      return(NULL)
    }
    log$subprocess <- subs$subprocess[i]
    log
  })
  out <- do.call(rbind, c(rows, list(NULL)))
  if (is.null(out)) {
    out <- cbind(pv_empty_log(), subprocess = character(0))
  }
  out
}

#' Reconstruct each session's ordered step sequence
#'
#' The log gives one row per `(session, step)` bucket, with only that
#' bucket's first-seen time, not a timestamp per click -- so a session's
#' walk through the diagram is approximated by ordering its distinct
#' steps by `started_at` ascending. This is the best ordering available
#' from this log shape, and the basis every conformance/outcome check
#' needs.
#'
#' @param log a [pv_pull_subprocess_log()] result (or any `data.frame`
#'   with `session_id`, `step_id`, `started_at`).
#' @return a named list of character vectors, keyed by `session_id`: each
#'   session's steps, in `started_at` order.
#' @export
pv_session_step_sequences <- function(log) {
  if (nrow(log) == 0) {
    return(list())
  }
  ordered <- log[order(log$session_id, log$started_at), ]
  split(ordered$step_id, ordered$session_id, drop = TRUE)
}

#' Graph-walk conformance for one subprocess's whole log
#'
#' Ties [pv_pull_subprocess_log()], [pv_session_step_sequences()], and
#' [pv_diagram_conformance()] together -- the one call the app-detail tab
#' needs for "how well did each session follow this app's declared step
#' diagram".
#'
#' @param registry a [pv_load_registry()] result.
#' @param process_id,subprocess identify the app.
#' @param log optionally, an already-pulled/filtered log (see
#'   [pv_filter_log()]) to reuse instead of pulling fresh.
#' @return a `data.frame`, [pv_diagram_conformance()]'s shape (`case_id`
#'   here is each session's id). Zero rows if this subprocess has no
#'   log, or no declared step diagram.
#' @export
pv_subprocess_conformance <- function(registry, process_id, subprocess, log = NULL) {
  empty <- data.frame(
    case_id = character(0), score = numeric(0), n_deviations = integer(0),
    n_transitions = integer(0), n_nodes = integer(0)
  )
  diagram <- registry$subprocess_diagrams[[paste(process_id, subprocess, sep = "::")]]
  if (is.null(diagram)) {
    return(empty)
  }
  if (is.null(log)) log <- pv_pull_subprocess_log(registry, process_id, subprocess)
  if (nrow(log) == 0) {
    return(empty)
  }
  sequences <- pv_session_step_sequences(log)
  pv_diagram_conformance(sequences, diagram$edges, "from_step", "to_step")
}

#' Filter a subprocess's log to a date range and/or a set of users
#'
#' Filters at the **session** level, not the row level: a session is
#' kept or dropped whole, based on when it started and who worked it --
#' truncating a session's own rows at a date boundary would corrupt
#' conformance and duration, which need the full step sequence.
#'
#' @param log a [pv_pull_subprocess_log()] result.
#' @param start_date,end_date `Date` (or coercible), or `NULL` for no bound.
#' @param users character vector of `user_id`s to keep, or `NULL` for all.
#' @return `log`, filtered.
#' @export
pv_filter_log <- function(log, start_date = NULL, end_date = NULL, users = NULL) {
  stopifnot(is.data.frame(log))
  if (nrow(log) == 0) {
    return(log)
  }

  session_start <- stats::aggregate(started_at ~ session_id, log, min)
  keep <- rep(TRUE, nrow(session_start))
  if (!is.null(start_date)) keep <- keep & as.Date(session_start$started_at) >= as.Date(start_date)
  if (!is.null(end_date)) keep <- keep & as.Date(session_start$started_at) <= as.Date(end_date)
  keep_ids <- session_start$session_id[keep]

  if (!is.null(users) && length(users) > 0) {
    user_ids <- unique(log$session_id[log$user_id %in% users])
    keep_ids <- intersect(keep_ids, user_ids)
  }

  log[log$session_id %in% keep_ids, ]
}

#' Wrap a subprocess's log as a bupaR eventlog
#'
#' Each `(session, step)` row is one instant, already-settled action
#' (`lifecycle_id = "complete"`) -- a real log is an *eventlog*, not an
#' *activitylog* with start+complete pairs, so elapsed-time metrics
#' (`edeaR::throughput_time()`) measure time-to-next-step, not a measured
#' task duration.
#'
#' @param log a [pv_pull_subprocess_log()] result (or hand-built with the
#'   same columns for testing): `session_id`, `step_id`, `started_at`,
#'   `user_id`.
#' @return a `bupaR::eventlog`.
#' @export
pv_build_eventlog <- function(log) {
  stopifnot(is.data.frame(log), all(c("session_id", "step_id", "started_at", "user_id") %in% names(log)))
  log$activity_instance_id <- seq_len(nrow(log))
  log$lifecycle_id <- "complete"

  bupaR::eventlog(
    log,
    case_id = "session_id", activity_id = "step_id", activity_instance_id = "activity_instance_id",
    lifecycle_id = "lifecycle_id", timestamp = "started_at", resource_id = "user_id"
  )
}
