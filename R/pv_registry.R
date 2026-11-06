#' Load and validate the process registry
#'
#' The registry is the source of truth for the dashboard's portfolio
#' overview and its coverage KPI: one row per target business process,
#' whether or not it is instrumented yet (see `inst/registry/processes.yml`
#' for the format and field meanings, and the process mining playbook's
#' Step 1-3 for how a process gets added here).
#'
#' A business process is composed of one or more ordered **steps**, and
#' each step is carried out by one or more ordered **apps** (an app's own
#' internal activities/variants/outcomes -- its own ideal process -- are
#' declared per app, via that app's own `ideal_process:` entry, not at the
#' business-process level: see [pv_load_ideal_process()]).
#'
#' By default the business-level "ideal flow" is just the step order
#' itself, a single straight-through sequence (see [pv_step_sequence()]).
#' A process whose real flow branches or loops back to an earlier step
#' can instead declare `step_variants`: a list of acceptable step-slug
#' sequences, mirroring how an app's own `ideal_process` declares
#' multiple `variants` -- a branch is just two variants that share a
#' prefix then diverge, and a loop is a variant that repeats a step slug.
#' When present, this overrides the single `step_order`-derived sequence
#' for conformance purposes; `step_order` still orders the step-flow
#' display either way.
#'
#' @param path path to a registry YAML file. Defaults to the registry
#'   shipped with the package (`system.file("registry/processes.yml",
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
#'       instrumented). Zero rows for a process with no app scoped yet.}
#'     \item{`$steps`}{one row per distinct (process, step), derived from
#'       `$subprocesses`: `process_id`, `step`, `step_label`, `step_order`
#'       -- ordered within each process. A convenience view; not separately
#'       authored.}
#'     \item{`$ideal`}{a named list, keyed by `"<process_id>::<subprocess>"`
#'       (one entry per app, not per business process), of
#'       [pv_load_ideal_process()] results -- `NULL` for an app with no
#'       `ideal_process` entry or a missing file.}
#'     \item{`$step_variants`}{a named list, keyed by `process_id`, of
#'       declared acceptable step-label sequences (each a character
#'       vector) -- absent (`NULL` via `[[`) for a process that didn't
#'       declare `step_variants`, in which case [pv_step_sequence()] falls
#'       back to the single `step_order`-derived sequence.}
#'   }
#' @export
pv_load_registry <- function(path = system.file("registry/processes.yml", package = "proviz")) {
  stopifnot(is.character(path), length(path) == 1, file.exists(path))
  entries <- yaml::read_yaml(path)
  if (!is.list(entries) || length(entries) == 0) {
    stop("proviz: registry at '", path, "' is empty or not a list of process entries", call. = FALSE)
  }

  required_fields <- c("process_id", "macroprocess", "process", "owner", "status", "loop_length")
  valid_status <- c("not_started", "designed", "instrumented", "deprecated")
  valid_loop_length <- c("short", "medium", "long")
  base_dir <- dirname(normalizePath(path, mustWork = TRUE))

  processes <- list()
  subprocesses <- list()
  ideal <- list()
  step_variants <- list()

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
        "which is no longer used -- each app now declares its own 'ideal_process' on its subprocess entry. Ignoring it.",
        call. = FALSE
      )
    }

    processes[[length(processes) + 1]] <- data.frame(
      process_id = entry$process_id, macroprocess = entry$macroprocess,
      process = entry$process, owner = entry$owner, status = entry$status,
      loop_length = entry$loop_length,
      stringsAsFactors = FALSE
    )

    subs <- entry$subprocesses %||% list()
    if (entry$status == "instrumented" && length(subs) == 0) {
      warning("proviz: registry entry '", entry$process_id,
        "' is status 'instrumented' but declares no subprocesses -- it will contribute no data",
        call. = FALSE
      )
    }

    step_counters <- list()
    for (s in subs) {
      # duckdb_path is only meaningful once there's a real store to attach --
      # a not-yet-instrumented process can still declare which app(s) it
      # will live in, and which step each belongs to, as soon as that's
      # scoped (playbook step 2-3), before any instrumentation (step 4)
      # exists.
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

      subprocesses[[length(subprocesses) + 1]] <- data.frame(
        process_id = entry$process_id, app = s$app, subprocess = s$subprocess,
        step = s$step, step_label = s$step_label %||% pv_prettify_label(s$step),
        step_order = as.integer(s$step_order), app_order = as.integer(s$app_order %||% step_counters[[s$step]]),
        duckdb_path = s$duckdb_path %||% NA_character_,
        stringsAsFactors = FALSE
      )

      ideal_key <- paste(entry$process_id, s$subprocess, sep = "::")
      if (!is.null(s$ideal_process)) {
        ideal_path <- file.path(base_dir, s$ideal_process)
        if (file.exists(ideal_path)) {
          ideal[[ideal_key]] <- pv_load_ideal_process(ideal_path)
        } else {
          warning("proviz: ideal_process file for '", ideal_key, "' not found at '", ideal_path, "'",
            call. = FALSE
          )
          ideal[[ideal_key]] <- NULL
        }
      } else {
        ideal[[ideal_key]] <- NULL
      }
    }

    if (!is.null(entry$step_variants)) {
      step_label_by_slug <- stats::setNames(
        vapply(subs, function(s) s$step_label %||% pv_prettify_label(s$step), character(1)),
        vapply(subs, function(s) s$step, character(1))
      )
      step_label_by_slug <- step_label_by_slug[!duplicated(names(step_label_by_slug))]

      declared <- lapply(entry$step_variants, function(v) as.character(unlist(v)))
      unknown <- unique(unlist(lapply(declared, setdiff, y = names(step_label_by_slug))))
      if (length(unknown) > 0) {
        stop("proviz: registry entry '", entry$process_id,
          "' has step_variants referencing unknown step(s): ", paste(unknown, collapse = ", "),
          call. = FALSE
        )
      }
      step_variants[[entry$process_id]] <- lapply(declared, function(v) unname(step_label_by_slug[v]))
    }
  }

  processes <- do.call(rbind, processes)
  if (anyDuplicated(processes$process_id) != 0) {
    dupes <- unique(processes$process_id[duplicated(processes$process_id)])
    stop("proviz: registry has duplicate process_id(s): ", paste(dupes, collapse = ", "), call. = FALSE)
  }
  rownames(processes) <- NULL

  subprocesses <- if (length(subprocesses) > 0) {
    do.call(rbind, subprocesses)
  } else {
    data.frame(
      process_id = character(0), app = character(0), subprocess = character(0),
      step = character(0), step_label = character(0), step_order = integer(0), app_order = integer(0),
      duckdb_path = character(0), stringsAsFactors = FALSE
    )
  }
  rownames(subprocesses) <- NULL

  structure(
    list(
      processes = processes, subprocesses = subprocesses, steps = pv_derive_steps(subprocesses),
      ideal = ideal, step_variants = step_variants
    ),
    class = "pv_registry"
  )
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
#'   `$activities` (a `data.frame`: `id`, `label`, `role`), `$variants` (a
#'   list of character vectors -- each an acceptable activity-id sequence,
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
    data.frame(id = a$id, label = a$label, role = a$role, stringsAsFactors = FALSE)
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
