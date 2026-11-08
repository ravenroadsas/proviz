#' A minimal, real log CSV (the exact shape pv_pull_subprocess_log() reads)
#' plus the registry rows that point at it, for tests that exercise the
#' real read path rather than hand-building a log data.frame in memory.
local_fixture_registry <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  log_path <- file.path(dir, "typecurves_log.csv")
  writeLines(c(
    "app_id,app_name,session_id,username,process,starttime,nclicks,total_weight",
    "typecurves,Type Curves,s1,alice,a,2026-01-01T09:00:00.000Z,1,1",
    "typecurves,Type Curves,s1,alice,b,2026-01-01T09:10:00.000Z,1,1"
  ), log_path)

  subprocesses <- data.frame(
    process_id = "type_curve_construction", subprocess = "type_curve_module", app = "typecurves",
    app_link = NA_character_, log_path = log_path, adoption_threshold = NA_real_,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      processes = data.frame(
        process_id = c("type_curve_construction", "not_yet"),
        macroprocess = c("reservoir_engineering", "ops"),
        process = c("Type Curve Construction", "Not Yet Started"),
        owner = c("x", "y"), status = c("instrumented", "not_started"),
        loop_length = c("medium", "medium"),
        stringsAsFactors = FALSE
      ),
      subprocesses = subprocesses,
      process_diagrams = list(),
      subprocess_diagrams = list(
        "type_curve_construction::type_curve_module" = list(
          steps = data.frame(id = c("a", "b"), label = c("A", "B"), stringsAsFactors = FALSE),
          edges = data.frame(from_step = "a", to_step = "b", stringsAsFactors = FALSE),
          outcomes = data.frame(
            id = "b_done", label = "B done", kind = "deliverable", step_id = "b",
            tier = NA_character_, outcome_tracking_app = NA_character_, outcome_tracking_join_key = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      ),
      clicks = list(),
      user_roles = data.frame(user_id = "alice", role = "Analyst", stringsAsFactors = FALSE),
      adoption_source = NULL
    ),
    class = "pv_registry"
  )
}
