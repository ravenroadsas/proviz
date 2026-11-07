#' Load and validate the process registry
#'
#' The registry is the source of truth for the dashboard's portfolio
#' overview and its coverage KPI: one row per target business process,
#' whether or not it is instrumented yet. It lives in a local DuckDB file
#' (see [pv_registry_migrate_yaml()] for how the original hand-authored
#' `processes.yml` is converted, and `data-raw/build_registry_db.R` for
#' how the registry shipped with this package was produced), so a
#' companion editor app can write into it a row at a time -- this
#' function only reads it, read-only.
#'
#' A business process is composed of one or more ordered **steps**, and
#' each step is carried out by one or more ordered **apps** (an app's own
#' internal activities/variants/outcomes -- its own ideal process -- are
#' declared per app, not at the business-process level: see
#' [pv_load_ideal_process()]).
#'
#' By default the business-level "ideal flow" is just the step order
#' itself, a single straight-through sequence (see [pv_step_sequence()]).
#' A process whose real flow branches or loops back to an earlier step
#' can instead declare `step_variants`: a list of acceptable step-slug
#' sequences, mirroring how an app's own ideal process declares multiple
#' `variants` -- a branch is just two variants that share a prefix then
#' diverge, and a loop is a variant that repeats a step slug. When
#' present, this overrides the single `step_order`-derived sequence for
#' conformance purposes; `step_order` still orders the step-flow display
#' either way.
#'
#' @param path path to a registry DuckDB file. Defaults to the registry
#'   shipped with the package (`system.file("registry/registry.duckdb",
#'   package = "proviz")`) -- pass an explicit path to use a real
#'   organization-specific registry instead.
#' @return a `pv_registry` object (a list):
#'   \describe{
#'     \item{`$processes`}{one row per business process: `process_id`,
#'       `macroprocess`, `process`, `owner`, `status`, `loop_length` --
#'       `"short"`/`"medium"`/`"long"`, how immediate the process's outcome
#'       and the data behind it are, e.g. daily production surveillance is
#'       short-loop, reserves estimation is long-loop.}
#'     \item{`$subprocesses`}{one row per app/step a process spans:
#'       `process_id`, `app`, `subprocess`, `step` (a slug grouping one or
#'       more apps into the same ordered step), `step_label`, `step_order`
#'       (position of the step within the process), `app_order` (position
#'       of this app within its step), `duckdb_path` (`NA` until the app is
#'       instrumented), `adoption_threshold` (`NA` unless declared -- the
#'       weekly score, in points, an identified user must average to count
#'       as "actively using" this app for [pv_app_iga()]'s usage
#'       component). Zero rows for a process with no app scoped yet.}
#'     \item{`$steps`}{one row per distinct (process, step), derived from
#'       `$subprocesses`: `process_id`, `step`, `step_label`, `step_order`
#'       -- ordered within each process. A convenience view; not separately
#'       authored.}
#'     \item{`$ideal`}{a named list, keyed by `"<process_id>::<subprocess>"`
#'       (one entry per app, not per business process), of
#'       [pv_load_ideal_process()]-shaped results -- `NULL` for an app with
#'       no ideal process declared.}
#'     \item{`$step_variants`}{a named list, keyed by `process_id`, of
#'       declared acceptable step-label sequences (each a character
#'       vector) -- absent (`NULL` via `[[`) for a process that didn't
#'       declare `step_variants`, or declared it as an empty list
#'       (`step_variants: []` is treated the same as omitting the field,
#'       not as "zero acceptable paths" -- the latter would silently make
#'       conformance unscorable), in which case [pv_step_sequence()] falls
#'       back to the single `step_order`-derived sequence.}
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
    SELECT process_id, app, subprocess, step, step_label, step_order, app_order,
           duckdb_path, adoption_threshold
    FROM subprocesses ORDER BY process_id, step_order, app_order")

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

  ideal <- pv_registry_db_read_ideal(con, subprocesses)
  step_variants <- pv_registry_db_read_step_variants(con, subprocesses)

  settings <- DBI::dbGetQuery(con, "SELECT * FROM settings")
  adoption_source <- settings$value[settings$key == "adoption_source"]
  adoption_source <- if (length(adoption_source) == 0) {
    NULL
  } else {
    # Resolved relative to the registry database's own directory, the same
    # way the old YAML-based registry resolved it relative to the YAML
    # file's directory -- keeps the value relocatable rather than baking
    # in an absolute path at migration time.
    file.path(dirname(normalizePath(path, mustWork = TRUE)), adoption_source[1])
  }

  structure(
    list(
      processes = processes, subprocesses = subprocesses, steps = pv_derive_steps(subprocesses),
      ideal = ideal, step_variants = step_variants, adoption_source = adoption_source
    ),
    class = "pv_registry"
  )
}

#' Read every app's ideal process (`activities`/`variants`/`outcomes`) out
#' of the registry database, keyed the same way [pv_load_registry()]'s
#' `$ideal` always has been: `"<process_id>::<subprocess>"`.
#' @noRd
pv_registry_db_read_ideal <- function(con, subprocesses) {
  ideal_meta <- DBI::dbGetQuery(con, "SELECT * FROM ideal_processes")
  activities_all <- DBI::dbGetQuery(con, "SELECT * FROM activities")
  variant_steps_all <- DBI::dbGetQuery(con, "SELECT * FROM variant_steps ORDER BY variant_no, step_no")
  outcomes_all <- DBI::dbGetQuery(con, "SELECT * FROM outcomes")

  ideal <- list()
  for (i in seq_len(nrow(subprocesses))) {
    pid <- subprocesses$process_id[i]
    sp <- subprocesses$subprocess[i]
    ideal_key <- paste(pid, sp, sep = "::")

    meta_row <- ideal_meta[ideal_meta$process_id == pid & ideal_meta$subprocess == sp, ]
    if (nrow(meta_row) == 0) {
      ideal[[ideal_key]] <- NULL
      next
    }

    act <- activities_all[activities_all$process_id == pid & activities_all$subprocess == sp,
      c("activity_id", "label", "role", "score")]
    names(act)[names(act) == "activity_id"] <- "id"
    rownames(act) <- NULL

    vs <- variant_steps_all[variant_steps_all$process_id == pid & variant_steps_all$subprocess == sp, ]
    variants <- pv_registry_db_ordered_groups(vs, "activity_id")

    oc <- outcomes_all[outcomes_all$process_id == pid & outcomes_all$subprocess == sp,
      c("outcome_id", "label", "kind", "activity_id", "tier", "outcome_tracking_app", "outcome_tracking_join_key")]
    names(oc)[names(oc) == "outcome_id"] <- "id"
    rownames(oc) <- NULL

    ideal[[ideal_key]] <- structure(
      list(
        process_id = meta_row$ideal_process_id[1] %||% pid,
        activities = act, variants = variants, outcomes = oc,
        outcome_exclusivity = meta_row$outcome_exclusivity[1]
      ),
      class = "pv_ideal_process"
    )
  }
  ideal
}

#' Translate the registry database's `step_variants` rows (declared as
#' step slugs, one row per `(process_id, variant_no, step_no)`) into the
#' same named-list-of-step-label-sequences shape
#' [pv_load_registry()]'s `$step_variants` has always had.
#' @noRd
pv_registry_db_read_step_variants <- function(con, subprocesses) {
  rows <- DBI::dbGetQuery(con, "SELECT * FROM step_variants ORDER BY process_id, variant_no, step_no")
  step_variants <- list()
  for (pid in unique(rows$process_id)) {
    subs_p <- subprocesses[subprocesses$process_id == pid, ]
    step_label_by_slug <- stats::setNames(subs_p$step_label, subs_p$step)
    step_label_by_slug <- step_label_by_slug[!duplicated(names(step_label_by_slug))]

    declared <- pv_registry_db_ordered_groups(rows[rows$process_id == pid, ], "step")
    step_variants[[pid]] <- lapply(declared, function(v) unname(step_label_by_slug[v]))
  }
  step_variants
}

#' Group a `variant_no`/`step_no`-ordered data.frame by `variant_no` into
#' a list of `value_col` vectors, ordered by `variant_no` (not by its
#' character sort order, which `split()` would otherwise give for 10+
#' variants) and then by `step_no` within each.
#' @noRd
pv_registry_db_ordered_groups <- function(df, value_col) {
  if (nrow(df) == 0) return(list())
  groups <- split(df, df$variant_no)
  groups <- groups[order(as.integer(names(groups)))]
  result <- lapply(groups, function(g) g[[value_col]][order(g$step_no)])
  unname(result)
}

#' @noRd
pv_prettify_label <- function(x) {
  words <- strsplit(gsub("[_-]+", " ", x), " ")[[1]]
  words <- words[nzchar(words)]
  paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = " ")
}

#' One row per distinct (process, step), ordered -- derived from
#' `subprocesses`, not separately authored, so it can't drift out of sync
#' with the apps that actually declare each step.
#' @noRd
pv_derive_steps <- function(subprocesses) {
  empty <- data.frame(
    process_id = character(0), step = character(0), step_label = character(0),
    step_order = integer(0), stringsAsFactors = FALSE
  )
  if (nrow(subprocesses) == 0) {
    return(empty)
  }

  key <- paste(subprocesses$process_id, subprocesses$step, sep = "::")
  groups <- split(subprocesses[c("step_order", "step_label")], key)
  inconsistent <- names(groups)[vapply(groups, function(g) {
    length(unique(g$step_order)) > 1 || length(unique(g$step_label)) > 1
  }, logical(1))]
  if (length(inconsistent) > 0) {
    stop("proviz: apps sharing the same step disagree on step_order/step_label: ",
      paste(inconsistent, collapse = ", "),
      call. = FALSE
    )
  }

  steps <- subprocesses[!duplicated(key), c("process_id", "step", "step_label", "step_order")]
  steps <- steps[order(steps$process_id, steps$step_order), ]
  rownames(steps) <- NULL
  steps
}

#' Load and validate an ideal-process definition
#'
#' @param path path to an ideal-process YAML file (`activities` + `variants`
#'   -- see `inst/registry/ideal_processes/*.yml` for real examples, and
#'   `data-raw/build_ideal_processes.R` for how they were derived).
#' @return a `pv_ideal_process` object (a list): `$process_id`,
#'   `$activities` (a `data.frame`: `id`, `label`, `role`, `score` -- the
#'   engagement-score weight assigned to this activity, e.g. per click;
#'   `0` if not declared, so an app that hasn't adopted scoring yet just
#'   contributes nothing to [pv_score_events()] rather than erroring),
#'   `$variants` (a list of character vectors -- each an acceptable
#'   activity-id sequence,
#'   used by [pv_case_conformance()] as the reference set to score against),
#'   `$outcomes` (a `data.frame`, one row per named way this process can
#'   end -- see [pv_case_outcome()]: `id`, `label`, `kind` (`"deliverable"`
#'   or `"decision"`), `activity_id` (which activity in `$activities` logs
#'   it), `tier` (`"operational"`/`"tactical"`/`"strategic"`, `NA` for a
#'   deliverable), `outcome_tracking_app`/`outcome_tracking_join_key` (where
#'   this decision's real-world result is tracked, if anywhere -- `NA` if
#'   not tracked, or not yet linked). Zero rows if the process declares no
#'   `outcomes` yet. `$outcome_exclusivity` (`"single"` or `"multi"` -- can
#'   more than one outcome fire on the same case; defaults to `"single"`).
#' @export
pv_load_ideal_process <- function(path) {
  stopifnot(is.character(path), length(path) == 1, file.exists(path))
  spec <- yaml::read_yaml(path)

  required <- c("process_id", "activities", "variants")
  missing <- setdiff(required, names(spec))
  if (length(missing) > 0) {
    stop("proviz: ideal-process file '", path, "' missing required field(s): ",
      paste(missing, collapse = ", "), call. = FALSE)
  }
  if (length(spec$activities) == 0) {
    stop("proviz: ideal-process '", spec$process_id, "' declares no activities", call. = FALSE)
  }
  if (length(spec$variants) == 0) {
    stop("proviz: ideal-process '", spec$process_id, "' declares no variants", call. = FALSE)
  }

  activities <- do.call(rbind, lapply(spec$activities, function(a) {
    a_missing <- setdiff(c("id", "label", "role"), names(a))
    if (length(a_missing) > 0) {
      stop("proviz: an activity in '", spec$process_id, "' is missing field(s): ",
        paste(a_missing, collapse = ", "), call. = FALSE)
    }
    data.frame(
      id = a$id, label = a$label, role = a$role, score = as.numeric(a$score %||% 0),
      stringsAsFactors = FALSE
    )
  }))
  rownames(activities) <- NULL

  if (anyDuplicated(activities$id) != 0) {
    dupes <- unique(activities$id[duplicated(activities$id)])
    stop("proviz: ideal-process '", spec$process_id, "' has duplicate activity id(s): ",
      paste(dupes, collapse = ", "), call. = FALSE)
  }

  variants <- lapply(spec$variants, function(v) as.character(unlist(v)))
  unknown <- unique(unlist(lapply(variants, setdiff, y = activities$id)))
  if (length(unknown) > 0) {
    stop("proviz: ideal-process '", spec$process_id,
      "' has variant(s) referencing unknown activity id(s): ", paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  outcomes <- pv_parse_outcomes(spec$outcomes, activities, spec$process_id)
  outcome_exclusivity <- spec$outcome_exclusivity %||% "single"
  if (!outcome_exclusivity %in% c("single", "multi")) {
    stop("proviz: ideal-process '", spec$process_id, "' has invalid outcome_exclusivity '",
      outcome_exclusivity, "' -- must be 'single' or 'multi'",
      call. = FALSE
    )
  }

  structure(
    list(
      process_id = spec$process_id, activities = activities, variants = variants,
      outcomes = outcomes, outcome_exclusivity = outcome_exclusivity
    ),
    class = "pv_ideal_process"
  )
}

#' @noRd
pv_parse_outcomes <- function(spec_outcomes, activities, process_id) {
  empty <- data.frame(
    id = character(0), label = character(0), kind = character(0), activity_id = character(0),
    tier = character(0), outcome_tracking_app = character(0), outcome_tracking_join_key = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(spec_outcomes) || length(spec_outcomes) == 0) {
    return(empty)
  }

  valid_kind <- c("deliverable", "decision")
  valid_tier <- c("operational", "tactical", "strategic")

  rows <- lapply(spec_outcomes, function(o) {
    o_missing <- setdiff(c("id", "label", "kind", "activity_id"), names(o))
    if (length(o_missing) > 0) {
      stop("proviz: an outcome in '", process_id, "' is missing field(s): ",
        paste(o_missing, collapse = ", "), call. = FALSE)
    }
    if (!o$kind %in% valid_kind) {
      stop("proviz: outcome '", o$id, "' in '", process_id, "' has invalid kind '", o$kind,
        "' -- must be one of: ", paste(valid_kind, collapse = ", "), call. = FALSE)
    }
    if (!o$activity_id %in% activities$id) {
      stop("proviz: outcome '", o$id, "' in '", process_id,
        "' references unknown activity_id '", o$activity_id, "'", call. = FALSE)
    }
    tier <- o$tier %||% NA_character_
    if (o$kind == "decision" && !is.na(tier) && !tier %in% valid_tier) {
      stop("proviz: outcome '", o$id, "' in '", process_id, "' has invalid tier '", tier,
        "' -- must be one of: ", paste(valid_tier, collapse = ", "), call. = FALSE)
    }
    tracking <- o$outcome_tracking %||% list()
    data.frame(
      id = o$id, label = o$label, kind = o$kind, activity_id = o$activity_id, tier = tier,
      outcome_tracking_app = tracking$app %||% NA_character_,
      outcome_tracking_join_key = tracking$join_key %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  outcomes <- do.call(rbind, rows)
  rownames(outcomes) <- NULL
  if (anyDuplicated(outcomes$id) != 0) {
    dupes <- unique(outcomes$id[duplicated(outcomes$id)])
    stop("proviz: ideal-process '", process_id, "' has duplicate outcome id(s): ",
      paste(dupes, collapse = ", "), call. = FALSE)
  }
  outcomes
}
