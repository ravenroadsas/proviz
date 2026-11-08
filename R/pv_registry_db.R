#' Create the registry's DuckDB schema
#'
#' The registry models four things (see the package's process-mining
#' glossary):
#' \enumerate{
#'   \item **Registry** -- `processes`, `subprocesses` (one app each,
#'     1:1): who owns what, its status, and where to find its app
#'     (`app_link`) and its event log (`log_path`).
#'   \item **Ideal diagrams** -- a graph (nodes + directed edges, cycles
#'     allowed -- no enumerated "variants") plus named outcomes, at two
#'     levels: the *process* level, where the nodes are its subprocesses
#'     (`process_edges`/`process_outcomes`), and the *subprocess* level,
#'     where the nodes are that one app's own `steps`
#'     (`steps`/`step_edges`/`subprocess_outcomes`).
#'   \item **Click map** -- `clicks`: a concrete UI binding
#'     (selector/kind/feature/event) plus a weight, many-to-one onto a
#'     step. This is what a future telemetry-config export would be
#'     built from; it is not consulted when reading a real event log,
#'     which already arrives with its weight totals pre-computed.
#'   \item **User roles** -- `user_roles`: a flat `user_id` -> `role`
#'     lookup, independent of any process/app/log -- the one piece of
#'     resource metadata the real event log itself doesn't carry (it has
#'     no role column), needed for a role filter on the dashboard.
#' }
#' The fourth artifact, the actual log records, is never stored here --
#' see [pv_pull_subprocess_log()], which reads it straight from each
#' subprocess's `log_path`.
#'
#' Enforces `PRIMARY KEY`/`CHECK` constraints but deliberately not
#' `FOREIGN KEY` ones: DuckDB allocates a fixed-size index segment per
#' constraint regardless of row count, which inflates a small registry
#' to several MB for no benefit here. Referential integrity is this
#' package's write-helpers' job (see e.g. [pv_registry_db_insert_step_edge()]'s
#' validation), not the schema's.
#'
#' Idempotent -- `CREATE TABLE IF NOT EXISTS`, safe to call against an
#' already-populated database.
#'
#' @param con a DBI connection to a DuckDB database.
#' @return `con`, invisibly.
#' @noRd
pv_registry_db_create_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS processes (
      process_id TEXT PRIMARY KEY,
      macroprocess TEXT NOT NULL,
      process TEXT NOT NULL,
      owner TEXT NOT NULL,
      status TEXT NOT NULL CHECK (status IN ('not_started', 'designed', 'instrumented', 'deprecated')),
      loop_length TEXT NOT NULL CHECK (loop_length IN ('short', 'medium', 'long'))
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS subprocesses (
      process_id TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      app TEXT NOT NULL,
      app_link TEXT,
      log_path TEXT,
      adoption_threshold DOUBLE,
      PRIMARY KEY (process_id, subprocess)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS process_edges (
      process_id TEXT NOT NULL,
      from_subprocess TEXT NOT NULL,
      to_subprocess TEXT NOT NULL,
      PRIMARY KEY (process_id, from_subprocess, to_subprocess)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS process_outcomes (
      process_id TEXT NOT NULL,
      outcome_id TEXT NOT NULL,
      label TEXT NOT NULL,
      kind TEXT NOT NULL CHECK (kind IN ('deliverable', 'decision')),
      subprocess TEXT NOT NULL,
      tier TEXT CHECK (tier IS NULL OR tier IN ('operational', 'tactical', 'strategic')),
      outcome_tracking_app TEXT,
      outcome_tracking_join_key TEXT,
      PRIMARY KEY (process_id, outcome_id)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS steps (
      process_id TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      step_id TEXT NOT NULL,
      label TEXT NOT NULL,
      PRIMARY KEY (process_id, subprocess, step_id)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS step_edges (
      process_id TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      from_step TEXT NOT NULL,
      to_step TEXT NOT NULL,
      PRIMARY KEY (process_id, subprocess, from_step, to_step)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS subprocess_outcomes (
      process_id TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      outcome_id TEXT NOT NULL,
      label TEXT NOT NULL,
      kind TEXT NOT NULL CHECK (kind IN ('deliverable', 'decision')),
      step_id TEXT NOT NULL,
      tier TEXT CHECK (tier IS NULL OR tier IN ('operational', 'tactical', 'strategic')),
      outcome_tracking_app TEXT,
      outcome_tracking_join_key TEXT,
      PRIMARY KEY (process_id, subprocess, outcome_id)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS clicks (
      process_id TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      click_id TEXT NOT NULL,
      step_id TEXT NOT NULL,
      label TEXT,
      selector TEXT,
      kind TEXT,
      feature TEXT,
      event TEXT,
      weight DOUBLE,
      PRIMARY KEY (process_id, subprocess, click_id)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS settings (
      key TEXT PRIMARY KEY,
      value TEXT
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_roles (
      user_id TEXT PRIMARY KEY,
      role TEXT NOT NULL
    )")

  invisible(con)
}

## -------------------------------------------------------------------------
## Write helpers -- for the registry editor app (run_registry_editor()) and
## for seeding registries (pv_demo_registry(), data-raw scripts). Each opens
## its own short-lived read-write connection and closes it before returning
## -- never held open across a Shiny reactive tick -- so a long-running
## editor session and the read-only analytics dashboard (pv_load_registry())
## don't contend for the database file any longer than one edit takes.
## -------------------------------------------------------------------------

#' @noRd
pv_registry_db_connect <- function(path, read_only = FALSE) {
  DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
}

#' Add a business process to the registry
#' @noRd
pv_registry_db_insert_process <- function(path, process_id, macroprocess, process, owner, status, loop_length) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO processes VALUES (?, ?, ?, ?, ?, ?)", params = list(
    process_id, macroprocess, process, owner, status, loop_length
  ))
  invisible(process_id)
}

#' Add an app (a `subprocesses` row, 1:1 with the app) to a process
#' @noRd
pv_registry_db_insert_subprocess <- function(path, process_id, subprocess, app, app_link = NA_character_,
                                              log_path = NA_character_, adoption_threshold = NA_real_) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "INSERT INTO subprocesses VALUES (?, ?, ?, ?, ?, ?)", params = list(
    process_id, subprocess, app, app_link, log_path, as.numeric(adoption_threshold)
  ))
  invisible(subprocess)
}

#' Declare one allowed transition between two of a process's subprocesses
#' (an edge in the process-level diagram). Cycles are allowed (e.g.
#' `to_subprocess` can equal an earlier node, for a rework loop).
#' @noRd
pv_registry_db_insert_process_edge <- function(path, process_id, from_subprocess, to_subprocess) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  known <- DBI::dbGetQuery(con, "SELECT subprocess FROM subprocesses WHERE process_id = ?",
    params = list(process_id))$subprocess
  unknown <- setdiff(c(from_subprocess, to_subprocess), known)
  if (length(unknown) > 0) {
    stop("proviz: process_edge references unknown subprocess(es) for '", process_id, "': ",
      paste(unknown, collapse = ", "), call. = FALSE)
  }
  DBI::dbExecute(con, "
    INSERT INTO process_edges VALUES (?, ?, ?)
    ON CONFLICT (process_id, from_subprocess, to_subprocess) DO NOTHING",
    params = list(process_id, from_subprocess, to_subprocess))
  invisible(NULL)
}

#' Add or update a process-level outcome (concluded by one of the
#' process's subprocess-nodes). Idempotent on `(process_id, outcome_id)`.
#' @noRd
pv_registry_db_upsert_process_outcome <- function(path, process_id, outcome_id, label, kind, subprocess,
                                                   tier = NA_character_, outcome_tracking_app = NA_character_,
                                                   outcome_tracking_join_key = NA_character_) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "
    INSERT INTO process_outcomes VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT (process_id, outcome_id) DO UPDATE SET
      label = excluded.label, kind = excluded.kind, subprocess = excluded.subprocess,
      tier = excluded.tier, outcome_tracking_app = excluded.outcome_tracking_app,
      outcome_tracking_join_key = excluded.outcome_tracking_join_key",
    params = list(process_id, outcome_id, label, kind, subprocess, tier, outcome_tracking_app, outcome_tracking_join_key))
  invisible(outcome_id)
}

#' Add one step (a node in a subprocess's own diagram) -- e.g. a telemetry
#' catalog feature like `"validate"` or `"setting params"`.
#' @noRd
pv_registry_db_insert_step <- function(path, process_id, subprocess, step_id, label = NULL) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "
    INSERT INTO steps VALUES (?, ?, ?, ?)
    ON CONFLICT (process_id, subprocess, step_id) DO UPDATE SET label = excluded.label",
    params = list(process_id, subprocess, step_id, label %||% pv_prettify_label(step_id)))
  invisible(step_id)
}

#' Declare one allowed transition between two of a subprocess's steps.
#' Cycles are allowed. Errors if either step isn't already declared.
#' @noRd
pv_registry_db_insert_step_edge <- function(path, process_id, subprocess, from_step, to_step) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  known <- DBI::dbGetQuery(con, "SELECT step_id FROM steps WHERE process_id = ? AND subprocess = ?",
    params = list(process_id, subprocess))$step_id
  unknown <- setdiff(c(from_step, to_step), known)
  if (length(unknown) > 0) {
    stop("proviz: step_edge references unknown step(s) for '", process_id, "::", subprocess, "': ",
      paste(unknown, collapse = ", "), call. = FALSE)
  }
  DBI::dbExecute(con, "
    INSERT INTO step_edges VALUES (?, ?, ?, ?)
    ON CONFLICT (process_id, subprocess, from_step, to_step) DO NOTHING",
    params = list(process_id, subprocess, from_step, to_step))
  invisible(NULL)
}

#' Add or update a subprocess-level outcome (concluded by one of its own
#' steps). Idempotent on `(process_id, subprocess, outcome_id)`.
#' @noRd
pv_registry_db_upsert_subprocess_outcome <- function(path, process_id, subprocess, outcome_id, label, kind, step_id,
                                                      tier = NA_character_, outcome_tracking_app = NA_character_,
                                                      outcome_tracking_join_key = NA_character_) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "
    INSERT INTO subprocess_outcomes VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT (process_id, subprocess, outcome_id) DO UPDATE SET
      label = excluded.label, kind = excluded.kind, step_id = excluded.step_id,
      tier = excluded.tier, outcome_tracking_app = excluded.outcome_tracking_app,
      outcome_tracking_join_key = excluded.outcome_tracking_join_key",
    params = list(
      process_id, subprocess, outcome_id, label, kind, step_id,
      tier, outcome_tracking_app, outcome_tracking_join_key
    ))
  invisible(outcome_id)
}

#' Add or update one click -> step binding (many clicks can map to one
#' step; `weight` is what a real recorder sums into that log's
#' `total_weight` -- this table isn't consulted when reading a real log,
#' only when exporting a telemetry config for the recorder to use).
#' Idempotent on `(process_id, subprocess, click_id)`.
#' @noRd
pv_registry_db_upsert_click <- function(path, process_id, subprocess, click_id, step_id, label = NA_character_,
                                         selector = NA_character_, kind = NA_character_, feature = NA_character_,
                                         event = NA_character_, weight = NA_real_) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  known <- DBI::dbGetQuery(con, "SELECT step_id FROM steps WHERE process_id = ? AND subprocess = ?",
    params = list(process_id, subprocess))$step_id
  if (!step_id %in% known) {
    stop("proviz: click references unknown step '", step_id, "' for '", process_id, "::", subprocess, "'", call. = FALSE)
  }
  DBI::dbExecute(con, "
    INSERT INTO clicks VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT (process_id, subprocess, click_id) DO UPDATE SET
      step_id = excluded.step_id, label = excluded.label, selector = excluded.selector,
      kind = excluded.kind, feature = excluded.feature, event = excluded.event, weight = excluded.weight",
    params = list(
      process_id, subprocess, click_id, step_id, label, selector, kind, feature, event, as.numeric(weight)
    ))
  invisible(click_id)
}

#' Add or update a user's role. There is no dependency on `subprocesses`
#' or any log -- a role can be declared for a user who hasn't logged any
#' activity yet (e.g. a newly identified user from the adoption
#' workbook). Idempotent on `user_id`.
#' @noRd
pv_registry_db_upsert_user_role <- function(path, user_id, role) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "
    INSERT INTO user_roles VALUES (?, ?)
    ON CONFLICT (user_id) DO UPDATE SET role = excluded.role",
    params = list(user_id, role))
  invisible(user_id)
}

#' Set (or replace) the registry-wide `adoption_source` setting -- a path
#' relative to the registry database's own directory, resolved at read
#' time by [pv_load_registry()].
#' @noRd
pv_registry_db_set_adoption_source <- function(path, relative_path) {
  con <- pv_registry_db_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "
    INSERT INTO settings VALUES ('adoption_source', ?)
    ON CONFLICT (key) DO UPDATE SET value = excluded.value",
    params = list(relative_path))
  invisible(relative_path)
}
