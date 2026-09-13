#' Synthetic demo registry, for local development and screenshots
#'
#' Writes real DuckDB event stores -- via `protrace::pt_connect()`/
#' `pt_log_event()`, the exact API a real instrumented app uses, not a
#' shortcut -- seeded with synthetic cases for the two ideal processes
#' migrated from the original PoC, plus two placeholder registry rows in
#' other statuses. Exists so `run_pi_dashboard(demo = TRUE)` (and this
#' package's own screenshots/manual testing) can exercise every panel and
#' every registry status without a real shared mount or real application
#' data. Not a real organization's process list.
#'
#' @param dir directory to write the synthetic DuckDB files into. Defaults
#'   to a fresh temp directory.
#' @param seed RNG seed, for reproducible demo data.
#' @return a `pv_registry` object, structurally identical to
#'   [pv_load_registry()]'s.
#' @export
pv_demo_registry <- function(dir = tempfile("proviz_demo_"), seed = 42) {
  if (!requireNamespace("protrace", quietly = TRUE)) {
    stop("proviz: pv_demo_registry() needs the 'protrace' package (Suggests) to write synthetic event stores.", call. = FALSE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  set.seed(seed)

  ideal_tc <- pv_load_ideal_process(system.file("registry/ideal_processes/type_curve_construction.yml", package = "proviz"))
  ideal_ds <- pv_load_ideal_process(system.file("registry/ideal_processes/daily_production_surveillance.yml", package = "proviz"))

  tc_path <- file.path(dir, "typecurves.duckdb")
  ds_path <- file.path(dir, "surveillance.duckdb")

  pv_demo_seed_store(
    path = tc_path, macroprocess = "reservoir_engineering", process = "Type Curve Construction",
    subprocess = "type_curve_module", ideal = ideal_tc, n_cases = 140, entity_prefix = "CURVE",
    users = c("l.fontaine", "m.adeyemi", "n.petrov", "o.haddad", "q.alvarez"),
    role_of_user = c(
      l.fontaine = "Reservoir Engineer", m.adeyemi = "Reservoir Engineer", n.petrov = "Reservoir Engineer",
      o.haddad = "Data Analyst", q.alvarez = "Super User"
    ),
    start_date = Sys.Date() - 120, noise_prob = 0.25, provisional_prob = 0.5
  )
  pv_demo_seed_store(
    path = ds_path, macroprocess = "production_operations", process = "Daily Production Surveillance",
    subprocess = "surveillance_app", ideal = ideal_ds, n_cases = 220, entity_prefix = "SURV",
    users = c("a.reyes", "b.chen", "c.novak", "d.singh", "h.muller"),
    role_of_user = c(
      a.reyes = "Surveillance Analyst", b.chen = "Surveillance Analyst", c.novak = "Production Engineer",
      d.singh = "Production Engineer", h.muller = "Shift Supervisor"
    ),
    start_date = Sys.Date() - 90, noise_prob = 0.10, provisional_prob = 0
  )

  processes <- data.frame(
    process_id = c(
      "type_curve_construction", "daily_production_surveillance",
      "well_completion_reporting", "legacy_ticketing"
    ),
    macroprocess = c("reservoir_engineering", "production_operations", "drilling_completions", "field_operations"),
    process = c(
      "Type Curve Construction", "Daily Production Surveillance",
      "Well Completion Reporting", "Legacy Ticketing (retired)"
    ),
    owner = c("Reservoir Engineering Lead", "Production Engineering Lead", "Completions Lead", "Ops Lead"),
    status = c("instrumented", "instrumented", "not_started", "deprecated"),
    stringsAsFactors = FALSE
  )
  subprocesses <- data.frame(
    process_id = c("type_curve_construction", "daily_production_surveillance"),
    app = c("typecurves", "surveillance"),
    subprocess = c("type_curve_module", "surveillance_app"),
    duckdb_path = c(tc_path, ds_path),
    stringsAsFactors = FALSE
  )
  ideal <- list(
    type_curve_construction = ideal_tc, daily_production_surveillance = ideal_ds,
    well_completion_reporting = NULL, legacy_ticketing = NULL
  )

  structure(list(processes = processes, subprocesses = subprocesses, ideal = ideal), class = "pv_registry")
}

#' Generate one noisy variant of an ideal-process trace
#' @noRd
pv_demo_noisy_trace <- function(variant, activity_ids, noise_prob) {
  if (stats::runif(1) >= noise_prob) {
    return(variant)
  }
  mutation <- sample(c("drop", "duplicate", "swap"), 1)
  trace <- variant
  if (mutation == "drop" && length(trace) > 2) {
    trace <- trace[-sample(2:(length(trace) - 1), 1)]
  } else if (mutation == "duplicate") {
    i <- sample(seq_along(trace), 1)
    trace <- append(trace, trace[i], after = i)
  } else if (mutation == "swap" && length(trace) > 2) {
    i <- sample(2:(length(trace) - 1), 1)
    trace[c(i, i + 1)] <- trace[c(i + 1, i)]
  }
  trace
}

#' Write synthetic protrace events for one process into a fresh DuckDB store
#' @noRd
pv_demo_seed_store <- function(path, macroprocess, process, subprocess, ideal, n_cases,
                                entity_prefix, users, role_of_user, start_date, noise_prob,
                                provisional_prob) {
  pt_con <- protrace::pt_connect(path, macroprocess, process, subprocess)
  on.exit(protrace::pt_disconnect(pt_con))

  activity_ids <- ideal$activities$id
  for (i in seq_len(n_cases)) {
    variant <- ideal$variants[[sample(length(ideal$variants), 1)]]
    trace <- pv_demo_noisy_trace(variant, activity_ids, noise_prob)

    entity_id <- sprintf("%s-%04d", entity_prefix, i)
    provisional <- stats::runif(1) < provisional_prob
    case_id <- if (provisional) protrace::pt_new_case_id() else entity_id
    linked_yet <- !provisional

    cur_time <- as.POSIXct(paste(start_date + sample(0:84, 1), "08:00:00"), tz = "UTC") +
      stats::runif(1, 0, 6) * 3600
    user <- sample(users, 1)

    for (aid in trace) {
      cur_time <- cur_time + stats::runif(1, 5, 90) * 60
      this_entity <- NA_character_
      if (provisional && !linked_yet && stats::runif(1) < 0.5) {
        this_entity <- entity_id
        linked_yet <- TRUE
      } else if (linked_yet) {
        case_id <- entity_id
      }
      protrace::pt_log_event(pt_con,
        case_id = case_id, activity_id = aid, session_id = paste0("s_", i),
        user_id = user, entity_id = this_entity,
        role = unname(role_of_user[user]), occurred_at = cur_time
      )
    }
  }
  invisible(path)
}
