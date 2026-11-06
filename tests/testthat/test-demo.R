test_that("pv_demo_registry produces a connectable, step-aware registry with linked cross-app cases", {
  skip_if_not_installed("protrace")

  dir <- withr::local_tempdir()
  reg <- pv_demo_registry(dir = dir, seed = 1)
  expect_equal(nrow(reg$processes), 4)
  expect_setequal(reg$processes$status, c("instrumented", "instrumented", "not_started", "deprecated"))
  expect_equal(nrow(reg$steps[reg$steps$process_id == "type_curve_construction", ]), 2)

  pv_con <- pv_connect_registry(reg)
  withr::defer(pv_disconnect(pv_con))
  expect_true(all(pv_con$subprocesses$attached))

  events <- pv_pull_process_events(pv_con, reg, "type_curve_construction")
  expect_gt(nrow(events), 0)
  expect_setequal(events$app, c("typecurves", "typecurve_review"))
  # The draft app always resolves its provisional/permanent id link by the
  # end of its own trace (see pv_demo_seed_type_curve()'s docs) -- the
  # review app can then log straight under the permanent id -- so every
  # case in the unioned, linked trace should carry the permanent CURVE-#### id.
  expect_true(all(grepl("^CURVE-", events$case_id)))

  draft_events <- pv_pull_app_events(pv_con, reg, "type_curve_construction", "type_curve_module")
  draft_conf <- pv_process_conformance(draft_events, reg$ideal[["type_curve_construction::type_curve_module"]])
  expect_true(any(draft_conf$score < 1)) # noise_prob > 0 should produce at least one imperfect draft trace
  expect_true(any(draft_conf$score == 1))

  biz_conf <- pv_business_process_conformance(pv_con, reg, "type_curve_construction")
  expect_true(all(biz_conf$score == 1)) # step order (draft -> review) is never violated
})
