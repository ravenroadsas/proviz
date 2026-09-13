#' Load and validate the process registry
#'
#' The registry is the source of truth for the dashboard's overview tab and
#' its coverage KPI: one row per target business process, whether or not it
#' is instrumented yet (see `inst/registry/processes.yml` for the format and
#' field meanings, and the process mining playbook's Step 1-3 for how a
#' process gets added here).
#'
#' @param path path to a registry YAML file. Defaults to the registry
#'   shipped with the package (`system.file("registry/processes.yml",
#'   package = "proviz")`) -- pass an explicit path to use a real
#'   organization-specific registry instead.
#' @return a `pv_registry` object (a list): `$processes` (one row per
#'   process: `process_id`, `macroprocess`, `process`, `owner`, `status`),
#'   `$subprocesses` (one row per app/module a process spans: `process_id`,
#'   `app`, `subprocess`, `duckdb_path` -- zero rows for a process with none
#'   yet), and `$ideal` (a named list, keyed by `process_id`, of
#'   [pv_load_ideal_process()] results -- `NULL` for a process with no
#'   `ideal_process` entry or a missing file).
#' @export
pv_load_registry <- function(path = system.file("registry/processes.yml", package = "proviz")) {
  stopifnot(is.character(path), length(path) == 1, file.exists(path))
  entries <- yaml::read_yaml(path)
  if (!is.list(entries) || length(entries) == 0) {
    stop("proviz: registry at '", path, "' is empty or not a list of process entries", call. = FALSE)
  }

  required_fields <- c("process_id", "macroprocess", "process", "owner", "status")
  valid_status <- c("not_started", "designed", "instrumented", "deprecated")
  base_dir <- dirname(normalizePath(path, mustWork = TRUE))

  processes <- list()
  subprocesses <- list()
  ideal <- list()

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

    processes[[length(processes) + 1]] <- data.frame(
      process_id = entry$process_id, macroprocess = entry$macroprocess,
      process = entry$process, owner = entry$owner, status = entry$status,
      stringsAsFactors = FALSE
    )

    subs <- entry$subprocesses %||% list()
    if (entry$status == "instrumented" && length(subs) == 0) {
      warning("proviz: registry entry '", entry$process_id,
        "' is status 'instrumented' but declares no subprocesses -- it will contribute no data",
        call. = FALSE
      )
    }
    for (s in subs) {
      sub_missing <- setdiff(c("app", "subprocess", "duckdb_path"), names(s))
      if (length(sub_missing) > 0) {
        stop("proviz: a subprocess entry under '", entry$process_id,
          "' is missing required field(s): ", paste(sub_missing, collapse = ", "),
          call. = FALSE
        )
      }
      subprocesses[[length(subprocesses) + 1]] <- data.frame(
        process_id = entry$process_id, app = s$app,
        subprocess = s$subprocess, duckdb_path = s$duckdb_path,
        stringsAsFactors = FALSE
      )
    }

    if (!is.null(entry$ideal_process)) {
      ideal_path <- file.path(base_dir, entry$ideal_process)
      if (file.exists(ideal_path)) {
        ideal[[entry$process_id]] <- pv_load_ideal_process(ideal_path)
      } else {
        warning("proviz: ideal_process file for '", entry$process_id, "' not found at '", ideal_path, "'",
          call. = FALSE
        )
        ideal[[entry$process_id]] <- NULL
      }
    } else {
      ideal[[entry$process_id]] <- NULL
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
    data.frame(process_id = character(0), app = character(0), subprocess = character(0),
      duckdb_path = character(0), stringsAsFactors = FALSE)
  }
  rownames(subprocesses) <- NULL

  structure(
    list(processes = processes, subprocesses = subprocesses, ideal = ideal),
    class = "pv_registry"
  )
}

#' Load and validate an ideal-process definition
#'
#' @param path path to an ideal-process YAML file (`activities` + `variants`
#'   -- see `inst/registry/ideal_processes/*.yml` for real examples, and
#'   `data-raw/build_ideal_processes.R` for how they were derived).
#' @return a `pv_ideal_process` object (a list): `$process_id`,
#'   `$activities` (a `data.frame`: `id`, `label`, `role`), `$variants` (a
#'   list of character vectors -- each an acceptable activity-id sequence,
#'   used by [pv_case_conformance()] as the reference set to score against).
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

  structure(
    list(process_id = spec$process_id, activities = activities, variants = variants),
    class = "pv_ideal_process"
  )
}
