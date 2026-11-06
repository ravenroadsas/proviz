fixture_registry_2steps <- function() {
  subprocesses <- data.frame(
    process_id = c("proc1", "proc1"), app = c("app1", "app2"), subprocess = c("sub1", "sub2"),
    step = c("draft", "review"), step_label = c("Draft", "Review"),
    step_order = c(1L, 2L), app_order = c(1L, 1L),
    duckdb_path = c("x", "y"), stringsAsFactors = FALSE
  )
  structure(
    list(
      processes = data.frame(
        process_id = "proc1", macroprocess = "mp1", process = "Proc One",
        owner = "owner", status = "instrumented", loop_length = "short",
        stringsAsFactors = FALSE
      ),
      subprocesses = subprocesses,
      steps = pv_derive_steps(subprocesses),
      ideal = list()
    ),
    class = "pv_registry"
  )
}

test_that("pv_step_sequence wraps the step order as a single-variant list", {
  reg <- fixture_registry_2steps()
  seq <- pv_step_sequence(reg, "proc1")
  expect_equal(seq$variants, list(c("Draft", "Review")))
})

test_that("pv_step_sequence returns a zero-length variant list for a process with no steps", {
  reg <- fixture_registry_2steps()
  reg$steps <- reg$steps[0, ]
  seq <- pv_step_sequence(reg, "proc1")
  expect_equal(seq$variants, list())
})

test_that("pv_step_sequence prefers declared step_variants over the step_order-derived sequence", {
  reg <- fixture_registry_2steps()
  reg$step_variants <- list(proc1 = list(c("Draft", "Review"), c("Draft", "Review", "Draft", "Review")))
  seq <- pv_step_sequence(reg, "proc1")
  expect_equal(seq$variants, list(c("Draft", "Review"), c("Draft", "Review", "Draft", "Review")))
})

test_that("pv_step_sequence falls back to step_order when no step_variants are declared for this process", {
  reg <- fixture_registry_2steps()
  reg$step_variants <- list(some_other_process = list(c("A", "B")))
  seq <- pv_step_sequence(reg, "proc1")
  expect_equal(seq$variants, list(c("Draft", "Review")))
})

test_that("pv_events_at_step_level relabels activity_id to step_label and collapses consecutive repeats", {
  subs <- data.frame(subprocess = c("sub1", "sub2"), step_label = c("Draft", "Review"), stringsAsFactors = FALSE)
  events <- data.frame(
    case_id = c("c1", "c1", "c1", "c2"),
    activity_id = c("a1", "a2", "a3", "a1"),
    timestamp = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 09:05", "2026-01-01 10:00", "2026-01-02 09:00")),
    subprocess = c("sub1", "sub1", "sub2", "sub1"),
    stringsAsFactors = FALSE
  )
  out <- pv_events_at_step_level(events, subs)
  expect_equal(out$activity_id, c("Draft", "Review", "Draft"))
  expect_equal(out$case_id, c("c1", "c1", "c2"))
})

test_that("pv_events_at_step_level prefixes an unmapped subprocess rather than dropping it", {
  subs <- data.frame(subprocess = "sub1", step_label = "Draft", stringsAsFactors = FALSE)
  events <- data.frame(
    case_id = "c1", activity_id = "mystery_activity",
    timestamp = as.POSIXct("2026-01-01 09:00"), subprocess = "unknown_sub",
    stringsAsFactors = FALSE
  )
  out <- pv_events_at_step_level(events, subs)
  expect_equal(out$activity_id, "[unmapped] mystery_activity")
})

test_that("pv_final_step_apps returns the highest-step_order app(s)", {
  reg <- fixture_registry_2steps()
  final <- pv_final_step_apps(reg, "proc1")
  expect_equal(final$subprocess, "sub2")
})

test_that("pv_final_step_apps returns zero rows for a process with no steps", {
  reg <- fixture_registry_2steps()
  final <- pv_final_step_apps(reg, "no_such_process")
  expect_equal(nrow(final), 0)
})

test_that("pv_pull_business_events composes the process union with step relabeling", {
  reg <- fixture_registry_2steps()
  fake_events <- data.frame(
    case_id = c("c1", "c1"), activity_id = c("a1", "a2"),
    timestamp = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 09:05")),
    subprocess = c("sub1", "sub2"), stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(pv_pull_process_events = function(...) fake_events, .package = "proviz")
  out <- pv_pull_business_events(NULL, reg, "proc1")
  expect_equal(out$activity_id, c("Draft", "Review"))
})

test_that("pv_business_process_conformance scores step-relabeled traces against the step order", {
  reg <- fixture_registry_2steps()
  fake_events <- data.frame(
    case_id = c("c1", "c1", "c2"), activity_id = c("a1", "a2", "a1"),
    timestamp = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 09:05", "2026-01-02 09:00")),
    subprocess = c("sub1", "sub2", "sub1"), stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(pv_pull_process_events = function(...) fake_events, .package = "proviz")
  conf <- pv_business_process_conformance(NULL, reg, "proc1")
  conf <- conf[order(conf$case_id), ]
  expect_equal(conf$score, c(1, 0.5)) # c1 exact Draft->Review; c2 only Draft (missing Review)
})
