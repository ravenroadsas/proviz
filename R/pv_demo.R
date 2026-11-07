#' Synthetic demo registry, for local development and screenshots
#'
#' Builds a fresh registry DuckDB file (see `pv_registry_db_create_schema()`)
#' and writes synthetic event-log CSVs in the exact shape a real recorder
#' produces (see [pv_pull_subprocess_log()]) for two business processes: a
#' **two-app** process (type curve construction: a drafting app and a
#' separate review/publish app, with a declared process-level rework
#' loop back to drafting on rejection) and a single-app one (daily
#' production surveillance), plus two placeholder registry rows in other
#' statuses. Also writes a synthetic yearly training/satisfaction
#' adoption workbook (see [pv_load_adoption()]) covering all three real
#' apps, each with a declared `adoption_threshold` and a couple of
#' identified-but-never-logged-in users, so the Adoption tab's IGA index
#' has something non-trivial to show. Exists so `run_pi_dashboard(demo =
#' TRUE)` (and this package's own screenshots/manual testing) can
#' exercise every panel and every registry status without a real shared
#' mount or real application data. Not a real organization's process list.
#'
#' @param dir directory to write the registry and synthetic log/workbook
#'   files into. Defaults to a fresh temp directory.
#' @param seed RNG seed, for reproducible demo data.
#' @return a `pv_registry` object, from [pv_load_registry()] on the
#'   freshly-built database.
#' @export
pv_demo_registry <- function(dir = tempfile("proviz_demo_"), seed = 42) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  set.seed(seed)

  db_path <- file.path(dir, "registry.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  pv_registry_db_create_schema(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  draft_log <- file.path(dir, "typecurves_draft_log.csv")
  review_log <- file.path(dir, "typecurves_review_log.csv")
  ds_log <- file.path(dir, "surveillance_log.csv")

  pv_demo_seed_type_curve(db_path, draft_log, review_log)
  pv_demo_seed_surveillance(db_path, ds_log)

  pv_registry_db_insert_process(db_path, "well_completion_reporting", "drilling_completions",
    "Well Completion Reporting", "Completions Lead", "not_started", "medium"
  )
  pv_registry_db_insert_process(db_path, "legacy_ticketing", "field_operations",
    "Legacy Ticketing (retired)", "Ops Lead", "deprecated", "short"
  )

  adoption_path <- pv_demo_seed_adoption_workbook(
    dir = dir,
    draft_users = c("l.fontaine", "m.adeyemi", "n.petrov", "o.haddad"), draft_untrained = "o.haddad",
    review_users = c("q.alvarez", "r.kim"), review_untrained = character(0),
    surveillance_users = c("a.reyes", "b.chen", "c.novak", "d.singh", "h.muller"), surveillance_untrained = "d.singh",
    # Identified but never logged-in users -- real organizations always have
    # a few, and they're what keeps the usage component below 100%.
    extra_untrained_inactive = list(typecurves = "p.silva", typecurve_review = "s.khan", surveillance = "e.ortiz"),
    satisfaction = c(typecurves = 88, typecurve_review = 92, surveillance = 71)
  )
  pv_registry_db_set_adoption_source(db_path, basename(adoption_path))

  pv_load_registry(db_path)
}

#' Generate `n_sessions` synthetic sessions from a set of candidate step
#' paths, with light noise (an occasional dropped or duplicated step),
#' and reshape into the exact log-row shape [pv_pull_subprocess_log()]
#' reads: one row per `(session, step)` bucket actually touched.
#' @noRd
pv_demo_generate_sessions <- function(step_paths, step_weights, n_sessions, users, start_date, noise_prob = 0.2) {
  rows <- vector("list", n_sessions)
  for (i in seq_len(n_sessions)) {
    path <- step_paths[[sample(length(step_paths), 1)]]
    if (stats::runif(1) < noise_prob && length(path) > 2) {
      if (sample(c("drop", "duplicate"), 1) == "drop") {
        path <- path[-sample(2:(length(path) - 1), 1)]
      } else {
        j <- sample(seq_along(path), 1)
        path <- append(path, path[j], after = j)
      }
    }

    session_id <- sprintf("s%06d-%04x", i, sample.int(65535, 1))
    user <- sample(users, 1)
    cur_time <- as.POSIXct(paste(start_date + sample(0:60, 1), "08:00:00"), tz = "UTC") +
      stats::runif(1, 0, 8) * 3600

    distinct_steps <- unique(path)
    session_rows <- lapply(distinct_steps, function(s) {
      nclicks <- sum(path == s)
      row <- data.frame(
        session_id = session_id, username = user, step = s, starttime = cur_time,
        nclicks = nclicks, total_weight = nclicks * (step_weights[[s]] %||% 1),
        stringsAsFactors = FALSE
      )
      cur_time <<- cur_time + stats::runif(1, 5, 60) * 60
      row
    })
    rows[[i]] <- do.call(rbind, session_rows)
  }
  do.call(rbind, rows)
}

#' Write a set of generated sessions out as a log CSV, in the exact shape
#' a real recorder produces.
#' @noRd
pv_demo_write_log_csv <- function(path, app_id, app_name, sessions) {
  out <- data.frame(
    app_id = app_id, app_name = app_name, session_id = sessions$session_id, username = sessions$username,
    process = sessions$step, starttime = paste0(format(sessions$starttime, "%Y-%m-%dT%H:%M:%OS3", tz = "UTC"), "Z"),
    nclicks = sessions$nclicks, total_weight = sessions$total_weight, stringsAsFactors = FALSE
  )
  utils::write.csv(out, path, row.names = FALSE)
  invisible(path)
}

#' Seed the two-app type curve construction process: registry rows,
#' declared step diagrams (steps/edges/clicks/outcomes) for each app, a
#' process-level diagram with a rework loop back to drafting on
#' rejection, and synthetic session logs for both apps.
#' @noRd
pv_demo_seed_type_curve <- function(db_path, draft_log, review_log) {
  pv_registry_db_insert_process(db_path, "type_curve_construction", "reservoir_engineering",
    "Type Curve Construction", "Reservoir Engineering Lead", "instrumented", "medium"
  )
  pv_registry_db_insert_subprocess(db_path, "type_curve_construction", "type_curve_module",
    app = "typecurves", app_link = "https://connect.example.org/typecurves", log_path = basename(draft_log), adoption_threshold = 10
  )
  pv_registry_db_insert_subprocess(db_path, "type_curve_construction", "type_curve_review_module",
    app = "typecurve_review", app_link = "https://connect.example.org/typecurve-review", log_path = basename(review_log), adoption_threshold = 8
  )
  pv_registry_db_insert_process_edge(db_path, "type_curve_construction", "type_curve_module", "type_curve_review_module")
  pv_registry_db_insert_process_edge(db_path, "type_curve_construction", "type_curve_review_module", "type_curve_module")
  pv_registry_db_upsert_process_outcome(
    db_path, "type_curve_construction", "published", "Type curve published", "deliverable", "type_curve_review_module"
  )

  draft_weights <- c(
    Define_Population_Criteria = 2, Query_Analog_Wells = 1, Review_Analog_Statistics = 2,
    Refine_Analog_Selection = 3, Normalize_Production_Data = 1, Generate_Aggregate_Curve = 3,
    Fit_Decline_Curve = 4, Adjust_Fitted_Curve = 3, Compare_Existing_Curves = 2, Save_Draft = 5
  )
  draft_paths <- list(
    c("Define_Population_Criteria", "Query_Analog_Wells", "Review_Analog_Statistics", "Normalize_Production_Data",
      "Generate_Aggregate_Curve", "Fit_Decline_Curve", "Compare_Existing_Curves", "Save_Draft"),
    # a rework loop within the draft app itself: analog selection gets refined and re-reviewed
    c("Define_Population_Criteria", "Query_Analog_Wells", "Review_Analog_Statistics", "Refine_Analog_Selection",
      "Review_Analog_Statistics", "Normalize_Production_Data", "Generate_Aggregate_Curve", "Fit_Decline_Curve",
      "Compare_Existing_Curves", "Save_Draft"),
    c("Define_Population_Criteria", "Query_Analog_Wells", "Review_Analog_Statistics", "Normalize_Production_Data",
      "Generate_Aggregate_Curve", "Fit_Decline_Curve", "Adjust_Fitted_Curve", "Compare_Existing_Curves", "Save_Draft")
  )
  pv_demo_seed_subprocess_diagram(
    db_path, "type_curve_construction", "type_curve_module", draft_weights, draft_paths,
    outcomes = list(list(id = "draft_saved", label = "Draft saved, ready for review", kind = "deliverable", step_id = "Save_Draft"))
  )

  review_weights <- c(Postulate_For_Review = 2, Super_User_Review = 4, Reject_Draft = 3, Publish_Official = 8)
  review_paths <- list(
    c("Postulate_For_Review", "Super_User_Review", "Publish_Official"),
    c("Postulate_For_Review", "Super_User_Review", "Reject_Draft")
  )
  pv_demo_seed_subprocess_diagram(
    db_path, "type_curve_construction", "type_curve_review_module", review_weights, review_paths,
    outcomes = list(list(id = "publish_official", label = "Type curve published (Official)", kind = "deliverable", step_id = "Publish_Official"))
  )

  draft_users <- c("l.fontaine", "m.adeyemi", "n.petrov", "o.haddad")
  review_users <- c("q.alvarez", "r.kim")
  draft_sessions <- pv_demo_generate_sessions(draft_paths, draft_weights, 140, draft_users, Sys.Date() - 120)
  review_sessions <- pv_demo_generate_sessions(review_paths, review_weights, 120, review_users, Sys.Date() - 118)
  pv_demo_write_log_csv(draft_log, "typecurves", "Type Curves", draft_sessions)
  pv_demo_write_log_csv(review_log, "typecurve_review", "Type Curve Review", review_sessions)
  invisible(NULL)
}

#' Seed the single-app daily production surveillance process.
#' @noRd
pv_demo_seed_surveillance <- function(db_path, log_path) {
  pv_registry_db_insert_process(db_path, "daily_production_surveillance", "production_operations",
    "Daily Production Surveillance", "Production Engineering Lead", "instrumented", "short"
  )
  pv_registry_db_insert_subprocess(db_path, "daily_production_surveillance", "surveillance_app",
    app = "surveillance", app_link = "https://connect.example.org/surveillance", log_path = basename(log_path), adoption_threshold = 6
  )

  weights <- c(
    Data_Ingestion = 1, Data_Quality_Check = 2, Production_Allocation = 2, Variance_Analysis = 2,
    Well_Anomaly_Screening = 3, Exception_Prioritization = 3, Root_Cause_Diagnostics = 4,
    Decide_No_Action = 5, Decide_Change_ESP_Frequency = 6, Surveillance_Reporting = 2,
    Issue_Escalation = 6, Report_Signoff = 3
  )
  base <- c("Data_Ingestion", "Data_Quality_Check", "Production_Allocation", "Variance_Analysis", "Well_Anomaly_Screening")
  paths <- list(
    c(base, "Exception_Prioritization", "Decide_No_Action", "Surveillance_Reporting", "Report_Signoff"),
    c(base, "Exception_Prioritization", "Root_Cause_Diagnostics", "Decide_Change_ESP_Frequency", "Surveillance_Reporting", "Report_Signoff"),
    c(base, "Exception_Prioritization", "Root_Cause_Diagnostics", "Decide_Change_ESP_Frequency", "Surveillance_Reporting", "Issue_Escalation", "Report_Signoff")
  )
  pv_demo_seed_subprocess_diagram(
    db_path, "daily_production_surveillance", "surveillance_app", weights, paths,
    outcomes = list(
      list(id = "no_action", label = "Decision: No Action Required", kind = "decision", step_id = "Decide_No_Action"),
      list(id = "change_esp_frequency", label = "Decision: Change ESP Frequency", kind = "decision", step_id = "Decide_Change_ESP_Frequency"),
      list(id = "escalate_to_engineering", label = "Decision: Escalate to Engineering", kind = "decision", step_id = "Issue_Escalation")
    )
  )

  users <- c("a.reyes", "b.chen", "c.novak", "d.singh", "h.muller")
  sessions <- pv_demo_generate_sessions(paths, weights, 220, users, Sys.Date() - 90, noise_prob = 0.1)
  pv_demo_write_log_csv(log_path, "surveillance", "Surveillance", sessions)
  invisible(NULL)
}

#' Write one subprocess's step diagram (steps, edges derived from a set
#' of candidate paths, one click per step, and named outcomes) into the
#' registry database.
#' @noRd
pv_demo_seed_subprocess_diagram <- function(db_path, process_id, subprocess, weights, paths, outcomes) {
  for (step_id in names(weights)) {
    pv_registry_db_insert_step(db_path, process_id, subprocess, step_id)
  }
  edges <- unique(do.call(rbind, lapply(paths, function(p) {
    if (length(p) < 2) {
      return(NULL)
    }
    data.frame(from = p[-length(p)], to = p[-1], stringsAsFactors = FALSE)
  })))
  for (i in seq_len(nrow(edges))) {
    pv_registry_db_insert_step_edge(db_path, process_id, subprocess, edges$from[i], edges$to[i])
  }
  for (step_id in names(weights)) {
    pv_registry_db_upsert_click(
      db_path, process_id, subprocess, click_id = paste0("c_", step_id), step_id = step_id,
      label = pv_prettify_label(step_id), selector = paste0("#", step_id), kind = "click",
      feature = step_id, event = paste0(step_id, "_click"), weight = unname(weights[step_id])
    )
  }
  for (o in outcomes) {
    pv_registry_db_upsert_subprocess_outcome(db_path, process_id, subprocess, o$id, o$label, o$kind, o$step_id)
  }
  invisible(NULL)
}

#' Write a synthetic yearly training/satisfaction adoption workbook, one
#' sheet per demo app (see [pv_load_adoption()])
#' @noRd
pv_demo_seed_adoption_workbook <- function(dir, draft_users, draft_untrained, review_users, review_untrained,
                                            surveillance_users, surveillance_untrained,
                                            extra_untrained_inactive, satisfaction) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("proviz: pv_demo_registry() needs the 'openxlsx' package (Suggests) to write the synthetic adoption workbook.", call. = FALSE)
  }

  sheet_for <- function(users, untrained, extra_inactive_user) {
    all_users <- c(users, extra_inactive_user)
    data.frame(
      user_id = all_users, trained = !all_users %in% c(untrained, extra_inactive_user),
      stringsAsFactors = FALSE
    )
  }

  path <- file.path(dir, "adoption.xlsx")
  openxlsx::write.xlsx(
    list(
      typecurves = cbind(sheet_for(draft_users, draft_untrained, extra_untrained_inactive$typecurves), satisfaction = satisfaction[["typecurves"]]),
      typecurve_review = cbind(sheet_for(review_users, review_untrained, extra_untrained_inactive$typecurve_review), satisfaction = satisfaction[["typecurve_review"]]),
      surveillance = cbind(sheet_for(surveillance_users, surveillance_untrained, extra_untrained_inactive$surveillance), satisfaction = satisfaction[["surveillance"]])
    ),
    file = path
  )
  path
}
