#' Synthetic demo registry, for local development and screenshots
#'
#' Writes real DuckDB event stores -- via `protrace::pt_connect()`/
#' `pt_log_event()`, the exact API a real instrumented app uses, not a
#' shortcut -- seeded with synthetic cases for two business processes: a
#' **two-app** business process (type curve construction: a drafting app
#' and a separate review/publish app, cross-app case-linked, illustrating
#' the step/app registry model end to end) and a single-app one (daily
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

  ideal_draft <- pv_load_ideal_process(system.file("registry/ideal_processes/type_curve_draft.yml", package = "proviz"))
  ideal_review <- pv_load_ideal_process(system.file("registry/ideal_processes/type_curve_review.yml", package = "proviz"))
  ideal_ds <- pv_load_ideal_process(system.file("registry/ideal_processes/daily_production_surveillance.yml", package = "proviz"))

  draft_path <- file.path(dir, "typecurves_draft.duckdb")
  review_path <- file.path(dir, "typecurves_review.duckdb")
  ds_path <- file.path(dir, "surveillance.duckdb")

  pv_demo_seed_type_curve(
    draft_path = draft_path, review_path = review_path,
    ideal_draft = ideal_draft, ideal_review = ideal_review,
    n_cases = 140, entity_prefix = "CURVE",
    draft_users = c("l.fontaine", "m.adeyemi", "n.petrov", "o.haddad"),
    draft_role_of_user = c(
      l.fontaine = "Reservoir Engineer", m.adeyemi = "Reservoir Engineer",
      n.petrov = "Reservoir Engineer", o.haddad = "Data Analyst"
    ),
    review_users = c("q.alvarez", "r.kim"),
    review_role_of_user = c(q.alvarez = "Super User", r.kim = "Super User"),
    start_date = Sys.Date() - 120, noise_prob = 0.25, provisional_prob = 0.5,
    reject_prob = 0.12, rework_prob = 0.6
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
    loop_length = c("medium", "short", "medium", "short"),
    stringsAsFactors = FALSE
  )
  subprocesses <- data.frame(
    process_id = c("type_curve_construction", "type_curve_construction", "daily_production_surveillance"),
    app = c("typecurves", "typecurve_review", "surveillance"),
    subprocess = c("type_curve_module", "type_curve_review_module", "surveillance_app"),
    step = c("draft", "review", "surveillance"),
    step_label = c("Draft", "Review & Publish", "Surveillance"),
    step_order = c(1L, 2L, 1L),
    app_order = c(1L, 1L, 1L),
    duckdb_path = c(draft_path, review_path, ds_path),
    adoption_threshold = c(10, 8, 6),
    stringsAsFactors = FALSE
  )
  ideal <- list(
    "type_curve_construction::type_curve_module" = ideal_draft,
    "type_curve_construction::type_curve_review_module" = ideal_review,
    "daily_production_surveillance::surveillance_app" = ideal_ds
  )
  step_variants <- list(
    type_curve_construction = list(
      c("Draft", "Review & Publish"), # straight-through: approved on the first pass
      c("Draft", "Review & Publish", "Draft", "Review & Publish") # one rework loop
    )
  )

  adoption_source <- pv_demo_seed_adoption_workbook(
    dir = dir,
    draft_users = c("l.fontaine", "m.adeyemi", "n.petrov", "o.haddad"), draft_untrained = "o.haddad",
    review_users = c("q.alvarez", "r.kim"), review_untrained = character(0),
    surveillance_users = c("a.reyes", "b.chen", "c.novak", "d.singh", "h.muller"), surveillance_untrained = "d.singh",
    # Identified but never logged-in users -- real organizations always have
    # a few, and they're what keeps the usage component below 100%.
    extra_untrained_inactive = list(typecurves = "p.silva", typecurve_review = "s.khan", surveillance = "e.ortiz"),
    satisfaction = c(typecurves = 88, typecurve_review = 92, surveillance = 71)
  )

  structure(
    list(
      processes = processes, subprocesses = subprocesses, steps = pv_derive_steps(subprocesses),
      ideal = ideal, step_variants = step_variants, adoption_source = adoption_source
    ),
    class = "pv_registry"
  )
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

#' Write synthetic protrace events for a two-app business process (a
#' drafting app feeding a review/publish app) into two fresh DuckDB
#' stores, with the draft case's provisional/permanent id link (see
#' [pv_link_cases()]) always resolved by the time its draft trace ends --
#' so the review app can log every one of its own events directly under
#' the permanent `entity_id`, exactly as a real second app would once the
#' upstream record already exists.
#'
#' A rejected case has a `rework_prob` chance of looping back to the
#' draft app for one more (shorter) pass before a second, successful
#' review -- a real business-level step loop (`Draft -> Review -> Draft
#' -> Review`), demonstrating the registry's `step_variants` (see
#' [pv_load_registry()]) rather than only ever producing the
#' straight-through `Draft -> Review` trace. A rejected case that
#' *doesn't* get reworked stays abandoned (no further events) -- still a
#' legitimate "incomplete" outcome, same as the single-app demo store's
#' unlinked provisional cases.
#' @noRd
pv_demo_seed_type_curve <- function(draft_path, review_path, ideal_draft, ideal_review, n_cases,
                                     entity_prefix, draft_users, draft_role_of_user,
                                     review_users, review_role_of_user, start_date,
                                     noise_prob, provisional_prob, reject_prob, rework_prob = 0) {
  pt_draft <- protrace::pt_connect(draft_path, "reservoir_engineering", "Type Curve Construction", "type_curve_module")
  pt_review <- protrace::pt_connect(review_path, "reservoir_engineering", "Type Curve Construction", "type_curve_review_module")
  on.exit({
    protrace::pt_disconnect(pt_draft)
    protrace::pt_disconnect(pt_review)
  })

  draft_activity_ids <- ideal_draft$activities$id
  straight_through_review <- ideal_review$variants[[1]]

  for (i in seq_len(n_cases)) {
    entity_id <- sprintf("%s-%04d", entity_prefix, i)
    provisional <- stats::runif(1) < provisional_prob
    case_id <- if (provisional) protrace::pt_new_case_id() else entity_id
    linked_yet <- !provisional

    draft_variant <- ideal_draft$variants[[sample(length(ideal_draft$variants), 1)]]
    draft_trace <- pv_demo_noisy_trace(draft_variant, draft_activity_ids, noise_prob)

    cur_time <- as.POSIXct(paste(start_date + sample(0:84, 1), "08:00:00"), tz = "UTC") +
      stats::runif(1, 0, 6) * 3600
    draft_user <- sample(draft_users, 1)

    for (j in seq_along(draft_trace)) {
      aid <- draft_trace[j]
      cur_time <- cur_time + stats::runif(1, 5, 90) * 60
      this_entity <- NA_character_
      is_last <- j == length(draft_trace)
      # Force the provisional/permanent link by the trace's last event if
      # it hasn't happened yet -- the review app below needs a resolved
      # entity_id from the start, unlike the single-app demo store where
      # an unresolved "abandoned" provisional case is a legitimate outcome.
      if (provisional && !linked_yet && (is_last || stats::runif(1) < 0.5)) {
        this_entity <- entity_id
        linked_yet <- TRUE
      } else if (linked_yet) {
        case_id <- entity_id
      }
      protrace::pt_log_event(pt_draft,
        case_id = case_id, activity_id = aid, session_id = paste0("s_draft_", i),
        user_id = draft_user, entity_id = this_entity,
        role = unname(draft_role_of_user[draft_user]), occurred_at = cur_time
      )
    }

    rejected <- stats::runif(1) < reject_prob
    review_trace <- if (rejected) {
      c("Postulate_For_Review", "Super_User_Review", "Reject_Draft")
    } else {
      pv_demo_noisy_trace(straight_through_review, ideal_review$activities$id, noise_prob)
    }

    cur_time <- cur_time + stats::runif(1, 1, 3) * 86400
    review_user <- sample(review_users, 1)

    for (aid in review_trace) {
      cur_time <- cur_time + stats::runif(1, 10, 120) * 60
      protrace::pt_log_event(pt_review,
        case_id = entity_id, activity_id = aid, session_id = paste0("s_review_", i),
        user_id = review_user, entity_id = NA_character_,
        role = unname(review_role_of_user[review_user]), occurred_at = cur_time
      )
    }

    if (rejected && stats::runif(1) < rework_prob) {
      cur_time <- cur_time + stats::runif(1, 1, 2) * 86400
      rework_user <- sample(draft_users, 1)
      for (aid in c("Adjust_Fitted_Curve", "Save_Draft")) {
        cur_time <- cur_time + stats::runif(1, 10, 60) * 60
        protrace::pt_log_event(pt_draft,
          case_id = entity_id, activity_id = aid, session_id = paste0("s_draft2_", i),
          user_id = rework_user, entity_id = NA_character_,
          role = unname(draft_role_of_user[rework_user]), occurred_at = cur_time
        )
      }

      cur_time <- cur_time + stats::runif(1, 1, 2) * 86400
      review_user2 <- sample(review_users, 1)
      for (aid in straight_through_review) {
        cur_time <- cur_time + stats::runif(1, 10, 120) * 60
        protrace::pt_log_event(pt_review,
          case_id = entity_id, activity_id = aid, session_id = paste0("s_review2_", i),
          user_id = review_user2, entity_id = NA_character_,
          role = unname(review_role_of_user[review_user2]), occurred_at = cur_time
        )
      }
    }
  }
  invisible(list(draft_path = draft_path, review_path = review_path))
}
