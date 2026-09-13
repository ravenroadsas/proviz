test_that("pv_demo_registry produces a connectable registry with linked cases and varied conformance", {
  skip_if_not_installed("protrace")

  dir <- withr::local_tempdir()
  reg <- pv_demo_registry(dir = dir, seed = 1)
  expect_equal(nrow(reg$processes), 4)
  expect_setequal(reg$processes$status, c("instrumented", "instrumented", "not_started", "deprecated"))

  pv_con <- pv_connect_registry(reg)
  withr::defer(pv_disconnect(pv_con))
  expect_true(all(pv_con$subprocesses$attached))

  events <- pv_pull_process_events(pv_con, reg, "type_curve_construction")
  expect_gt(nrow(events), 0)
  # Most cases should resolve to their permanent CURVE-#### id; an unlinked
  # provisional case (never saved) staying "prov_"-prefixed is expected,
  # not a bug -- see pv_link_cases()'s docs -- so only most, not all.
  expect_gt(mean(grepl("^CURVE-", events$case_id)), 0.5)

  conf <- pv_process_conformance(events, reg$ideal$type_curve_construction)
  expect_true(any(conf$score < 1)) # noise_prob > 0 should produce at least one imperfect trace
  expect_true(any(conf$score == 1))
})
