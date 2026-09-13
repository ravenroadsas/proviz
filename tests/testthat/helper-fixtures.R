local_fixture_registry <- function(env = parent.frame()) {
  skip_if_not_installed("protrace")
  dir <- withr::local_tempdir(.local_envir = env)
  db_path <- file.path(dir, "typecurves.duckdb")

  pt_con <- protrace::pt_connect(db_path, "reservoir_engineering", "Type Curve Construction", "type_curve_module")
  protrace::pt_log_event(pt_con,
    case_id = "CURVE-1", activity_id = "a", session_id = "s1", user_id = "alice",
    occurred_at = as.POSIXct("2026-01-01 09:00:00", tz = "UTC")
  )
  protrace::pt_log_event(pt_con,
    case_id = "CURVE-1", activity_id = "b", session_id = "s1", user_id = "alice",
    occurred_at = as.POSIXct("2026-01-01 09:10:00", tz = "UTC")
  )
  protrace::pt_disconnect(pt_con)

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
      subprocesses = data.frame(
        process_id = "type_curve_construction", app = "typecurves",
        subprocess = "type_curve_module", duckdb_path = db_path, stringsAsFactors = FALSE
      ),
      ideal = list(
        type_curve_construction = structure(
          list(
            variants = list(c("a", "b")),
            outcomes = data.frame(
              id = "b_done", label = "B done", kind = "deliverable", activity_id = "b",
              tier = NA_character_, outcome_tracking_app = NA_character_, outcome_tracking_join_key = NA_character_,
              stringsAsFactors = FALSE
            ),
            outcome_exclusivity = "single"
          ),
          class = "pv_ideal_process"
        ),
        not_yet = NULL
      )
    ),
    class = "pv_registry"
  )
}
