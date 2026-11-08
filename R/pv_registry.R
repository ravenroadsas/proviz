#' Load and validate the process registry
#'
#' The registry lives in a local DuckDB file (see `pv_registry_db_create_schema()`'s
#' schema, built up via the registry editor app or [pv_demo_registry()]'s
#' seeding helpers) and models four things, read-only here:
#' \enumerate{
#'   \item **Registry** -- `$processes`/`$subprocesses`: the
#'     macroprocess/process/subprocess hierarchy. A subprocess is 1:1
#'     with an app, but keeps its own `app` name and `app_link` (which
#'     can differ from the subprocess's own id) plus `log_path` (where
#'     that app's event log lives -- see [pv_pull_subprocess_log()]).
#'   \item **Ideal diagrams** -- `$process_diagrams`/`$subprocess_diagrams`:
#'     a graph (nodes + directed edges, cycles allowed -- there is no
#'     enumerated list of "variants") plus named outcomes, at the process
#'     level (nodes = its subprocesses) and at the subprocess level
#'     (nodes = that one app's own steps).
#'   \item **Click map** -- `$clicks`: each subprocess's concrete
#'     click -> step bindings (selector/kind/feature/event/weight), many
#'     clicks to one step. Not consulted when reading a real log (see
#'     [pv_pull_subprocess_log()]) -- only relevant for a future
#'     telemetry-config export.
#'   \item **User roles** -- `$user_roles`: a flat `user_id` -> `role`
#'     lookup. The real event log carries no role column, so this is
#'     the only source for a role filter/breakdown.
#' }
#' The fourth artifact, the actual log records, is never part of the
#' registry -- it's read straight from each subprocess's `log_path`.
#'
#' @param path path to a registry DuckDB file. Defaults to the registry
#'   shipped with the package (`system.file("registry/registry.duckdb",
#'   package = "proviz")`) -- pass an explicit path to use a real
#'   organization-specific registry instead.
#' @return a `pv_registry` object (a list):
#'   \describe{
#'     \item{`$processes`}{one row per business process: `process_id`,
#'       `macroprocess`, `process`, `owner`, `status`, `loop_length`.}
#'     \item{`$subprocesses`}{one row per app: `process_id`, `subprocess`,
#'       `app`, `app_link` (`NA` if undeclared), `log_path` (`NA` until
#'       instrumented), `adoption_threshold` (`NA` unless declared -- the
#'       weekly score, in points, an identified user must average to
#'       count as "actively using" this app for [pv_app_iga()]'s usage
#'       component).}
#'     \item{`$process_diagrams`}{a named list, keyed by `process_id`:
#'       `$edges` (a `data.frame`: `from_subprocess`, `to_subprocess`) and
#'       `$outcomes` (a `data.frame`: `id`, `label`, `kind`
#'       (`"deliverable"`/`"decision"`), `subprocess` (which node
#'       concludes it), `tier`, `outcome_tracking_app`,
#'       `outcome_tracking_join_key`). Absent (`NULL` via `[[`) for a
#'       process with no declared edges/outcomes yet.}
#'     \item{`$subprocess_diagrams`}{a named list, keyed by
#'       `"<process_id>::<subprocess>"`: `$steps` (a `data.frame`: `id`,
#'       `label`), `$edges` (`from_step`, `to_step`), `$outcomes` (same
#'       shape as `$process_diagrams`' but `step_id` instead of
#'       `subprocess`). Absent for a subprocess with no declared steps yet.}
#'     \item{`$clicks`}{a named list, keyed by `"<process_id>::<subprocess>"`,
#'       of `data.frame`s: `id`, `step_id`, `label`, `selector`, `kind`,
#'       `feature`, `event`, `weight`. Absent for a subprocess with no
#'       declared clicks yet.}
#'     \item{`$user_roles`}{a `data.frame`: `user_id`, `role`. Zero rows
#'       if none declared.}
#'     \item{`$adoption_source`}{path to the yearly training/satisfaction
#'       Excel workbook (see [pv_load_adoption()]) -- `NULL` if the
#'       registry's `settings` table has no `adoption_source` row.}
#'   }
#' @export
pv_load_registry <- function(path = system.file("registry/registry.duckdb", package = "proviz")) {
  stopifnot(is.character(path), length(path) == 1, file.exists(path))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  processes <- DBI::dbGetQuery(con, "SELECT * FROM processes ORDER BY process_id")
  if (nrow(processes) == 0) {
    stop("proviz: registry at '", path, "' declares no processes", call. = FALSE)
  }

  subprocesses <- DBI::dbGetQuery(con, "
    SELECT process_id, subprocess, app, app_link, log_path, adoption_threshold
    FROM subprocesses ORDER BY process_id, subprocess")
  base_dir <- dirname(normalizePath(path, mustWork = TRUE))
  subprocesses$log_path <- vapply(subprocesses$log_path, pv_resolve_registry_path, character(1), base_dir = base_dir)

  instrumented_empty <- setdiff(
    processes$process_id[processes$status == "instrumented"],
    unique(subprocesses$process_id)
  )
  for (pid in instrumented_empty) {
    warning("proviz: registry entry '", pid,
      "' is status 'instrumented' but declares no subprocesses -- it will contribute no data",
      call. = FALSE
    )
  }

  process_diagrams <- pv_registry_db_read_process_diagrams(con, processes)
  subprocess_diagrams <- pv_registry_db_read_subprocess_diagrams(con, subprocesses)
  clicks <- pv_registry_db_read_clicks(con, subprocesses)

  settings <- DBI::dbGetQuery(con, "SELECT * FROM settings")
  adoption_source <- settings$value[settings$key == "adoption_source"]
  adoption_source <- if (length(adoption_source) == 0) {
    NULL
  } else {
    pv_resolve_registry_path(adoption_source[1], base_dir)
  }

  user_roles <- DBI::dbGetQuery(con, "SELECT * FROM user_roles ORDER BY user_id")

  structure(
    list(
      processes = processes, subprocesses = subprocesses,
      process_diagrams = process_diagrams, subprocess_diagrams = subprocess_diagrams,
      clicks = clicks, user_roles = user_roles, adoption_source = adoption_source
    ),
    class = "pv_registry"
  )
}

#' Read every process's ideal diagram (edges + outcomes), keyed by
#' `process_id`.
#' @noRd
pv_registry_db_read_process_diagrams <- function(con, processes) {
  edges_all <- DBI::dbGetQuery(con, "SELECT * FROM process_edges")
  outcomes_all <- DBI::dbGetQuery(con, "SELECT * FROM process_outcomes")

  diagrams <- list()
  for (pid in processes$process_id) {
    edges <- edges_all[edges_all$process_id == pid, c("from_subprocess", "to_subprocess")]
    outcomes <- outcomes_all[outcomes_all$process_id == pid,
      c("outcome_id", "label", "kind", "subprocess", "tier", "outcome_tracking_app", "outcome_tracking_join_key")]
    if (nrow(edges) == 0 && nrow(outcomes) == 0) next
    names(outcomes)[names(outcomes) == "outcome_id"] <- "id"
    rownames(edges) <- NULL
    rownames(outcomes) <- NULL
    diagrams[[pid]] <- list(edges = edges, outcomes = outcomes)
  }
  diagrams
}

#' Read every subprocess's ideal diagram (steps + edges + outcomes),
#' keyed by `"<process_id>::<subprocess>"`.
#' @noRd
pv_registry_db_read_subprocess_diagrams <- function(con, subprocesses) {
  steps_all <- DBI::dbGetQuery(con, "SELECT * FROM steps")
  edges_all <- DBI::dbGetQuery(con, "SELECT * FROM step_edges")
  outcomes_all <- DBI::dbGetQuery(con, "SELECT * FROM subprocess_outcomes")

  diagrams <- list()
  for (i in seq_len(nrow(subprocesses))) {
    pid <- subprocesses$process_id[i]
    sp <- subprocesses$subprocess[i]
    key <- paste(pid, sp, sep = "::")

    steps <- steps_all[steps_all$process_id == pid & steps_all$subprocess == sp, c("step_id", "label")]
    if (nrow(steps) == 0) next
    names(steps)[names(steps) == "step_id"] <- "id"
    rownames(steps) <- NULL

    edges <- edges_all[edges_all$process_id == pid & edges_all$subprocess == sp, c("from_step", "to_step")]
    rownames(edges) <- NULL

    outcomes <- outcomes_all[outcomes_all$process_id == pid & outcomes_all$subprocess == sp,
      c("outcome_id", "label", "kind", "step_id", "tier", "outcome_tracking_app", "outcome_tracking_join_key")]
    names(outcomes)[names(outcomes) == "outcome_id"] <- "id"
    rownames(outcomes) <- NULL

    diagrams[[key]] <- list(steps = steps, edges = edges, outcomes = outcomes)
  }
  diagrams
}

#' Read every subprocess's click map, keyed by `"<process_id>::<subprocess>"`.
#' @noRd
pv_registry_db_read_clicks <- function(con, subprocesses) {
  clicks_all <- DBI::dbGetQuery(con, "SELECT * FROM clicks")
  clicks <- list()
  for (i in seq_len(nrow(subprocesses))) {
    pid <- subprocesses$process_id[i]
    sp <- subprocesses$subprocess[i]
    key <- paste(pid, sp, sep = "::")
    rows <- clicks_all[clicks_all$process_id == pid & clicks_all$subprocess == sp,
      c("click_id", "step_id", "label", "selector", "kind", "feature", "event", "weight")]
    if (nrow(rows) == 0) next
    names(rows)[names(rows) == "click_id"] <- "id"
    rownames(rows) <- NULL
    clicks[[key]] <- rows
  }
  clicks
}

#' Resolve a registry-stored path (e.g. `log_path`, `adoption_source`)
#' that may be either absolute (a real organization's shared-mount path,
#' entered as-is via the registry editor) or relative (this package's own
#' demo/reference registry stores log/workbook paths relative to its own
#' directory, so the shipped files stay relocatable). `NA` passes through
#' unchanged.
#' @noRd
pv_resolve_registry_path <- function(x, base_dir) {
  if (is.na(x) || startsWith(x, "/")) {
    return(x)
  }
  file.path(base_dir, x)
}

#' @noRd
pv_prettify_label <- function(x) {
  words <- strsplit(gsub("[_-]+", " ", x), " ")[[1]]
  words <- words[nzchar(words)]
  paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = " ")
}
