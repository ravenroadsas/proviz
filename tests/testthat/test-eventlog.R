test_that("pv_pull_subprocess_log reads the exact given CSV shape", {
  registry <- local_fixture_registry()
  log <- pv_pull_subprocess_log(registry, "type_curve_construction", "type_curve_module")
  expect_equal(nrow(log), 2)
  expect_equal(log$session_id, c("s1", "s1"))
  expect_equal(log$user_id, c("alice", "alice"))
  expect_equal(log$step_id, c("a", "b"))
  expect_equal(log$total_weight, c(1, 1))
  expect_s3_class(log$started_at, "POSIXct")
})

test_that("pv_pull_subprocess_log returns the empty schema when log_path is NA or missing", {
  registry <- local_fixture_registry()
  registry$subprocesses$log_path <- NA_character_
  log <- pv_pull_subprocess_log(registry, "type_curve_construction", "type_curve_module")
  expect_equal(nrow(log), 0)
})

test_that("pv_pull_subprocess_log returns the empty schema for an unrecognized process/subprocess combination", {
  # Tolerated, not an error: Shiny's updateSelectInput() round-trip can leave
  # two linked dropdowns transiently mismatched for one reactive tick.
  registry <- local_fixture_registry()
  log <- pv_pull_subprocess_log(registry, "type_curve_construction", "nope")
  expect_equal(nrow(log), 0)
})

test_that("pv_session_step_sequences orders each session's steps by started_at", {
  log <- data.frame(
    session_id = c("s1", "s1", "s2"), step_id = c("b", "a", "x"),
    started_at = as.POSIXct(c("2026-01-01 10:00", "2026-01-01 09:00", "2026-01-01 09:00")),
    stringsAsFactors = FALSE
  )
  seqs <- pv_session_step_sequences(log)
  expect_equal(seqs$s1, c("a", "b")) # re-ordered despite input row order
  expect_equal(seqs$s2, "x")
})

test_that("pv_session_step_sequences returns an empty list for zero rows", {
  expect_equal(pv_session_step_sequences(pv_empty_log()), list())
})

test_that("pv_subprocess_conformance ties the log, sequences, and diagram together", {
  registry <- local_fixture_registry()
  conf <- pv_subprocess_conformance(registry, "type_curve_construction", "type_curve_module")
  expect_equal(conf$case_id, "s1")
  expect_equal(conf$score, 1) # a -> b is declared
})

test_that("pv_subprocess_conformance returns the empty schema with no declared diagram", {
  registry <- local_fixture_registry()
  registry$subprocess_diagrams[["type_curve_construction::type_curve_module"]] <- NULL
  conf <- pv_subprocess_conformance(registry, "type_curve_construction", "type_curve_module")
  expect_equal(nrow(conf), 0)
})

test_that("pv_filter_log keeps a session whole when filtering by user", {
  log <- data.frame(
    session_id = c("1", "1", "2"), step_id = c("a", "b", "a"),
    started_at = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 10:00", "2026-01-02 09:00")),
    user_id = c("u1", "u1", "u2"), stringsAsFactors = FALSE
  )
  filtered <- pv_filter_log(log, users = "u1")
  expect_setequal(unique(filtered$session_id), "1")
  expect_equal(nrow(filtered), 2)
})

test_that("pv_build_eventlog wraps a log as a bupaR eventlog with session_id as the case", {
  registry <- local_fixture_registry()
  log <- pv_pull_subprocess_log(registry, "type_curve_construction", "type_curve_module")
  el <- pv_build_eventlog(log)
  expect_s3_class(el, "eventlog")
  expect_equal(nrow(el), 2)
})

test_that("pv_pull_process_log unions every subprocess under a process, tagging each row's subprocess", {
  registry <- local_fixture_registry()
  log <- pv_pull_process_log(registry, "type_curve_construction")
  expect_equal(nrow(log), 2)
  expect_equal(unique(log$subprocess), "type_curve_module")
})
