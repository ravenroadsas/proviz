test_that("pv_demo_registry produces a loadable registry with real logs and both instrumented processes", {
  dir <- withr::local_tempdir()
  reg <- pv_demo_registry(dir = dir, seed = 1)

  expect_equal(nrow(reg$processes), 4)
  expect_setequal(reg$processes$status, c("instrumented", "instrumented", "not_started", "deprecated"))
  expect_equal(nrow(reg$subprocesses[reg$subprocesses$process_id == "type_curve_construction", ]), 2)

  log <- pv_pull_subprocess_log(reg, "type_curve_construction", "type_curve_module")
  expect_gt(nrow(log), 0)
  expect_equal(length(unique(log$session_id)), 140)

  conf <- pv_subprocess_conformance(reg, "type_curve_construction", "type_curve_module")
  expect_true(any(conf$score < 1)) # noise_prob > 0 should produce at least one imperfect session
  expect_true(any(conf$score == 1))

  # The process-level diagram declares a rework loop back to drafting on
  # rejection (review_module -> type_curve_module) -- a real cycle, not
  # just tolerated as noise.
  diagram <- reg$process_diagrams[["type_curve_construction"]]
  expect_true(any(diagram$edges$from_subprocess == "type_curve_review_module" & diagram$edges$to_subprocess == "type_curve_module"))

  dist <- pv_process_outcome_distribution(reg, "type_curve_construction")
  expect_true(all(c("publish_official", "incomplete") %in% dist$outcome_id)) # some rejected, some published
})
