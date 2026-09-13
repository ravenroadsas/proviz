test_that("pv_link_cases remaps every row of a linked provisional case", {
  events <- data.frame(
    case_id = c("prov_abc", "prov_abc", "prov_abc", "REC-42"),
    entity_id = c(NA, NA, "REC-42", "REC-42"),
    activity_id = c("draft_created", "analog_selected", "save_curve", "generate_report"),
    stringsAsFactors = FALSE
  )
  linked <- pv_link_cases(events)
  expect_equal(linked$case_id, c("REC-42", "REC-42", "REC-42", "REC-42"))
})

test_that("pv_link_cases leaves an unlinked (never-saved) provisional case alone", {
  events <- data.frame(
    case_id = c("prov_abandoned", "prov_abandoned", "REC-1", "REC-1"),
    entity_id = c(NA, NA, "REC-1", "REC-1"),
    activity_id = c("draft_created", "analog_selected", "save_curve", "generate_report"),
    stringsAsFactors = FALSE
  )
  linked <- pv_link_cases(events)
  expect_equal(linked$case_id, c("prov_abandoned", "prov_abandoned", "REC-1", "REC-1"))
})

test_that("pv_link_cases handles a process with no provisional ids at all", {
  events <- data.frame(
    case_id = c("REC-1", "REC-1"), entity_id = c("REC-1", "REC-1"),
    activity_id = c("a", "b"), stringsAsFactors = FALSE
  )
  expect_equal(pv_link_cases(events)$case_id, c("REC-1", "REC-1"))
})

test_that("pv_link_cases is a no-op on zero rows", {
  events <- pv_empty_events()
  expect_equal(nrow(pv_link_cases(events)), 0)
})

test_that("pv_build_eventlog wraps events as a single-lifecycle bupaR eventlog", {
  events <- data.frame(
    case_id = c("1", "1", "2"),
    activity_id = c("a", "b", "a"),
    timestamp = as.POSIXct(c("2026-01-01 01:00", "2026-01-01 02:00", "2026-01-01 01:00")),
    user_id = c("u1", "u1", "u2"),
    stringsAsFactors = FALSE
  )
  el <- pv_build_eventlog(events)
  expect_s3_class(el, "eventlog")
  expect_equal(bupaR::n_cases(el), 2)
  expect_equal(bupaR::n_activities(el), 2)
})

test_that("pv_connect_registry attaches real protrace stores and pv_pull_process_events reads through them", {
  skip_if_not_installed("protrace")
  dir <- withr::local_tempdir()
  db_path <- file.path(dir, "typecurves.duckdb")

  pt_con <- protrace::pt_connect(db_path, "reservoir_engineering", "Type Curve Construction", "type_curve_module")
  withr::defer(protrace::pt_disconnect(pt_con))

  protrace::pt_log_event(pt_con,
    case_id = "prov_x1", activity_id = "draft_created",
    session_id = "s1", user_id = "alice", occurred_at = as.POSIXct("2026-01-01 09:00:00", tz = "UTC")
  )
  protrace::pt_log_event(pt_con,
    case_id = "prov_x1", activity_id = "save_curve", entity_id = "CURVE-1",
    session_id = "s1", user_id = "alice", occurred_at = as.POSIXct("2026-01-01 09:05:00", tz = "UTC")
  )
  protrace::pt_log_event(pt_con,
    case_id = "CURVE-1", activity_id = "generate_report",
    session_id = "s2", user_id = "bob", occurred_at = as.POSIXct("2026-01-01 10:00:00", tz = "UTC")
  )

  registry <- structure(
    list(
      processes = data.frame(
        process_id = "type_curve_construction", macroprocess = "reservoir_engineering",
        process = "Type Curve Construction", owner = "x", status = "instrumented",
        stringsAsFactors = FALSE
      ),
      subprocesses = data.frame(
        process_id = "type_curve_construction", app = "typecurves",
        subprocess = "type_curve_module", duckdb_path = db_path, stringsAsFactors = FALSE
      ),
      ideal = list()
    ),
    class = "pv_registry"
  )

  pv_con <- pv_connect_registry(registry)
  withr::defer(pv_disconnect(pv_con))

  expect_true(all(pv_con$subprocesses$attached))

  events <- pv_pull_process_events(pv_con, registry, "type_curve_construction")
  expect_equal(nrow(events), 3)
  expect_true(all(events$case_id == "CURVE-1")) # provisional id resolved everywhere
})

test_that("pv_connect_registry warns and skips a missing duckdb_path instead of failing", {
  registry <- structure(
    list(
      processes = data.frame(process_id = "p", macroprocess = "m", process = "p", owner = "o", status = "instrumented", stringsAsFactors = FALSE),
      subprocesses = data.frame(process_id = "p", app = "ghost_app", subprocess = "s", duckdb_path = "/no/such/file.duckdb", stringsAsFactors = FALSE),
      ideal = list()
    ),
    class = "pv_registry"
  )
  expect_warning(pv_con <- pv_connect_registry(registry), "not found")
  expect_false(pv_con$subprocesses$attached)
  pv_disconnect(pv_con)
})
