#' Create the registry's DuckDB schema
#'
#' The registry used to be a hand-authored YAML file; it is now a local
#' DuckDB database so a companion editor app (see the forthcoming
#' `run_registry_editor()`) can read and write it a row at a time instead
#' of rewriting the whole file. The schema normalizes what the YAML held
#' into relational tables, keyed to mirror the YAML's own nesting:
#' `subprocesses` under a process, `activities`/`variant_steps`/`outcomes`
#' under an app's ideal process (`ideal_processes`, one row per
#' `(process_id, subprocess)`), `step_variants` under a process, and a
#' flat `settings` table for registry-wide values (currently just
#' `adoption_source`).
#'
#' `activities` carries five columns (`selector`, `kind`, `feature`,
#' `event`, `weight`) that the current hand-modeled ideal processes leave
#' `NULL` -- they exist for the scan-and-curate workflow the editor app
#' will drive, where each activity is mapped to a concrete UI hook
#' instead of only declared by id/label/role/score.
#'
#' Idempotent -- `CREATE TABLE IF NOT EXISTS`, safe to call against an
#' already-populated database.
#'
#' Enforces `PRIMARY KEY` and `CHECK` constraints, but deliberately not
#' `FOREIGN KEY` ones (e.g. `activities.process_id, subprocess` ->
#' `ideal_processes`): DuckDB allocates a fixed-size index segment per
#' constraint regardless of row count, which inflated this few-KB registry
#' to multiple MB for no benefit here. Referential integrity across these
#' tables is pv_registry_migrate_yaml()'s and the editor app's job, not
#' the schema's.
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
      app TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      step TEXT NOT NULL,
      step_label TEXT NOT NULL,
      step_order INTEGER NOT NULL,
      app_order INTEGER NOT NULL,
      duckdb_path TEXT,
      adoption_threshold DOUBLE,
      PRIMARY KEY (process_id, subprocess)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ideal_processes (
      process_id TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      ideal_process_id TEXT,
      outcome_exclusivity TEXT NOT NULL DEFAULT 'single' CHECK (outcome_exclusivity IN ('single', 'multi')),
      PRIMARY KEY (process_id, subprocess)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS activities (
      process_id TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      activity_id TEXT NOT NULL,
      label TEXT NOT NULL,
      role TEXT NOT NULL,
      score DOUBLE NOT NULL DEFAULT 0,
      selector TEXT,
      kind TEXT,
      feature TEXT,
      event TEXT,
      weight DOUBLE,
      PRIMARY KEY (process_id, subprocess, activity_id)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS variant_steps (
      process_id TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      variant_no INTEGER NOT NULL,
      step_no INTEGER NOT NULL,
      activity_id TEXT NOT NULL,
      PRIMARY KEY (process_id, subprocess, variant_no, step_no)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS outcomes (
      process_id TEXT NOT NULL,
      subprocess TEXT NOT NULL,
      outcome_id TEXT NOT NULL,
      label TEXT NOT NULL,
      kind TEXT NOT NULL CHECK (kind IN ('deliverable', 'decision')),
      activity_id TEXT NOT NULL,
      tier TEXT CHECK (tier IS NULL OR tier IN ('operational', 'tactical', 'strategic')),
      outcome_tracking_app TEXT,
      outcome_tracking_join_key TEXT,
      PRIMARY KEY (process_id, subprocess, outcome_id)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS step_variants (
      process_id TEXT NOT NULL,
      variant_no INTEGER NOT NULL,
      step_no INTEGER NOT NULL,
      step TEXT NOT NULL,
      PRIMARY KEY (process_id, variant_no, step_no)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS settings (
      key TEXT PRIMARY KEY,
      value TEXT
    )")

  invisible(con)
}

#' Migrate a hand-authored YAML registry into the DuckDB registry format
#'
#' One-off conversion from the original `processes.yml` (plus each app's
#' `ideal_process` YAML file, read via [pv_load_ideal_process()]) into the
#' relational schema `pv_registry_db_create_schema()` creates. Field
#' validation mirrors what the old YAML-based `pv_load_registry()` did --
#' this is now where that validation happens, since hand-editing moves
#' from the YAML file to this one conversion (ongoing edits are expected
#' to go through the registry editor app instead, which writes directly
#' into an existing database).
#'
#' Does not delete `db_path` first: re-running this against a file that
#' already has the same `process_id`/`subprocess` rows fails loudly on a
#' PRIMARY KEY violation rather than silently merging two versions.
#' Delete the file yourself to re-migrate from scratch.
#'
#' @param yaml_path path to the YAML registry file (the format the
#'   package used before the registry moved to DuckDB).
#' @param db_path path to the DuckDB file to create.
#' @return `db_path`, invisibly.
#' @export
pv_registry_migrate_yaml <- function(yaml_path, db_path) {
  stopifnot(is.character(yaml_path), length(yaml_path) == 1, file.exists(yaml_path))
  stopifnot(is.character(db_path), length(db_path) == 1)

  entries <- yaml::read_yaml(yaml_path)
  if (!is.list(entries) || length(entries) == 0) {
    stop("proviz: registry at '", yaml_path, "' is empty or not a list of process entries", call. = FALSE)
  }
  base_dir <- dirname(normalizePath(yaml_path, mustWork = TRUE))

  required_fields <- c("process_id", "macroprocess", "process", "owner", "status", "loop_length")
  valid_status <- c("not_started", "designed", "instrumented", "deprecated")
  valid_loop_length <- c("short", "medium", "long")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  pv_registry_db_create_schema(con)

  # One transaction for the whole migration, not one autocommit per
  # INSERT -- besides the usual atomicity benefit, DuckDB checkpoints
  # storage per committed transaction, so hundreds of one-row autocommits
  # across eight tables was inflating this few-hundred-row registry to
  # several MB of fragmented segments.
  DBI::dbExecute(con, "BEGIN TRANSACTION")
  adoption_source_written <- FALSE

  for (entry in entries) {
    missing <- setdiff(required_fields, names(entry))
    if (length(missing) > 0) {
      stop("proviz: registry entry missing required field(s): ", paste(missing, collapse = ", "),
        " (entry: ", deparse1_safe(entry), ")",
        call. = FALSE
      )
    }
    if (!entry$status %in% valid_status) {
      stop("proviz: registry entry '", entry$process_id, "' has invalid status '", entry$status,
        "' -- must be one of: ", paste(valid_status, collapse = ", "),
        call. = FALSE
      )
    }
    if (!entry$loop_length %in% valid_loop_length) {
      stop("proviz: registry entry '", entry$process_id, "' has invalid loop_length '", entry$loop_length,
        "' -- must be one of: ", paste(valid_loop_length, collapse = ", "),
        call. = FALSE
      )
    }
    if (!is.null(entry$ideal_process)) {
      warning("proviz: registry entry '", entry$process_id, "' has a process-level 'ideal_process' field, ",
        "which is no longer used -- each app declares its own 'ideal_process' on its subprocess entry. Ignoring it.",
        call. = FALSE
      )
    }

    DBI::dbExecute(con, "INSERT INTO processes VALUES (?, ?, ?, ?, ?, ?)", params = list(
      entry$process_id, entry$macroprocess, entry$process, entry$owner, entry$status, entry$loop_length
    ))

    if (!adoption_source_written && !is.null(entry$adoption_source)) {
      # Stored as given (relative to the YAML's own directory at migration
      # time), not resolved to an absolute path here -- pv_load_registry()
      # resolves it relative to the *database's* directory at load time,
      # so the shipped registry.duckdb stays relocatable (an absolute path
      # baked in at migration/build time would only be valid on this
      # machine).
      DBI::dbExecute(con, "INSERT INTO settings VALUES ('adoption_source', ?)",
        params = list(entry$adoption_source)
      )
      adoption_source_written <- TRUE
    }

    subs <- entry$subprocesses %||% list()
    if (entry$status == "instrumented" && length(subs) == 0) {
      warning("proviz: registry entry '", entry$process_id,
        "' is status 'instrumented' but declares no subprocesses -- it will contribute no data",
        call. = FALSE
      )
    }

    step_counters <- list()
    for (s in subs) {
      required_sub_fields <- c("app", "subprocess", "step", "step_order")
      if (entry$status == "instrumented") required_sub_fields <- c(required_sub_fields, "duckdb_path")
      sub_missing <- setdiff(required_sub_fields, names(s))
      if (length(sub_missing) > 0) {
        stop("proviz: a subprocess entry under '", entry$process_id,
          "' is missing required field(s): ", paste(sub_missing, collapse = ", "),
          call. = FALSE
        )
      }

      step_counters[[s$step]] <- (step_counters[[s$step]] %||% 0L) + 1L
      step_label <- s$step_label %||% pv_prettify_label(s$step)
      app_order <- as.integer(s$app_order %||% step_counters[[s$step]])

      DBI::dbExecute(con, "INSERT INTO subprocesses VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)", params = list(
        entry$process_id, s$app, s$subprocess, s$step, step_label,
        as.integer(s$step_order), app_order,
        s$duckdb_path %||% NA_character_, as.numeric(s$adoption_threshold %||% NA_real_)
      ))

      if (!is.null(s$ideal_process)) {
        ideal_path <- file.path(base_dir, s$ideal_process)
        if (!file.exists(ideal_path)) {
          warning("proviz: ideal_process file for '", entry$process_id, "::", s$subprocess,
            "' not found at '", ideal_path, "'",
            call. = FALSE
          )
        } else {
          ip <- pv_load_ideal_process(ideal_path)

          DBI::dbExecute(con, "INSERT INTO ideal_processes VALUES (?, ?, ?, ?)", params = list(
            entry$process_id, s$subprocess, ip$process_id, ip$outcome_exclusivity
          ))

          for (r in seq_len(nrow(ip$activities))) {
            a <- ip$activities[r, ]
            DBI::dbExecute(con, "INSERT INTO activities VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", params = list(
              entry$process_id, s$subprocess, a$id, a$label, a$role, a$score,
              NA_character_, NA_character_, NA_character_, NA_character_, NA_real_
            ))
          }

          for (vno in seq_along(ip$variants)) {
            v <- ip$variants[[vno]]
            for (sno in seq_along(v)) {
              DBI::dbExecute(con, "INSERT INTO variant_steps VALUES (?, ?, ?, ?, ?)", params = list(
                entry$process_id, s$subprocess, vno - 1L, sno - 1L, v[sno]
              ))
            }
          }

          for (r in seq_len(nrow(ip$outcomes))) {
            o <- ip$outcomes[r, ]
            DBI::dbExecute(con, "INSERT INTO outcomes VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)", params = list(
              entry$process_id, s$subprocess, o$id, o$label, o$kind, o$activity_id,
              o$tier, o$outcome_tracking_app, o$outcome_tracking_join_key
            ))
          }
        }
      }
    }

    # An empty `step_variants: []` is treated the same as omitting the
    # field entirely (no rows written -- pv_load_registry() then leaves
    # $step_variants[[process_id]] absent, and pv_step_sequence() falls
    # back to the step_order-derived sequence) -- not as "zero acceptable
    # paths", which would silently make conformance unscorable (NA) for
    # every case.
    if (!is.null(entry$step_variants) && length(entry$step_variants) > 0) {
      declared <- lapply(entry$step_variants, function(v) as.character(unlist(v)))
      known_steps <- vapply(subs, function(s) s$step, character(1))
      unknown <- unique(unlist(lapply(declared, setdiff, y = known_steps)))
      if (length(unknown) > 0) {
        stop("proviz: registry entry '", entry$process_id,
          "' has step_variants referencing unknown step(s): ", paste(unknown, collapse = ", "),
          call. = FALSE
        )
      }
      for (vno in seq_along(declared)) {
        v <- declared[[vno]]
        for (sno in seq_along(v)) {
          DBI::dbExecute(con, "INSERT INTO step_variants VALUES (?, ?, ?, ?)", params = list(
            entry$process_id, vno - 1L, sno - 1L, v[sno]
          ))
        }
      }
    }
  }

  DBI::dbExecute(con, "COMMIT")
  invisible(db_path)
}
