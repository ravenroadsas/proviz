demo_log <- function() {
  data.frame(
    session_id = c("1", "1", "1", "2", "2", "3", "3", "3"),
    step_id = c("a", "b", "c", "a", "b", "a", "b", "c"),
    started_at = as.POSIXct(c(
      "2026-01-05 09:00", "2026-01-05 10:00", "2026-01-05 12:00", # session 1: week of Jan 4
      "2026-01-06 09:00", "2026-01-06 09:30", # session 2: week of Jan 4, short
      "2026-01-15 09:00", "2026-01-15 11:00", "2026-01-15 13:00" # session 3: week of Jan 11
    )),
    user_id = c("u1", "u1", "u1", "u2", "u2", "u1", "u1", "u1"),
    nclicks = 1, total_weight = 1,
    stringsAsFactors = FALSE
  )
}

demo_diagram <- function() {
  list(edges = data.frame(from_step = c("a", "b"), to_step = c("b", "c"), stringsAsFactors = FALSE))
}

test_that("pv_case_durations returns per-session elapsed minutes", {
  log <- demo_log()
  durations <- pv_case_durations(log)
  expect_equal(durations$duration_mins[durations$case_id == "1"], 180)
  expect_equal(durations$duration_mins[durations$case_id == "2"], 30)
})

test_that("pv_process_kpis reports n_cases, n_users, and median-over-mean duration", {
  log <- demo_log()
  kpis <- pv_process_kpis(log)
  expect_equal(kpis$n_cases, 3)
  expect_equal(kpis$n_users, 2)
  expect_equal(kpis$median_duration_mins, stats::median(c(180, 30, 240)))
  expect_true(is.na(kpis$conformance_median)) # no diagram supplied
})

test_that("pv_process_kpis computes conformance when given a diagram", {
  kpis <- pv_process_kpis(demo_log(), demo_diagram())
  expect_equal(kpis$conformance_median, 1) # sessions 1 and 3 exact a->b->c; session 2 a->b, also fully declared
})

test_that("pv_process_kpis handles zero rows", {
  kpis <- pv_process_kpis(demo_log()[0, ])
  expect_equal(kpis$n_cases, 0L)
  expect_true(is.na(kpis$median_duration_mins))
})

test_that("pv_weekly_usage buckets sessions by the week they started", {
  weekly <- pv_weekly_usage(demo_log())
  expect_equal(nrow(weekly), 2)
  expect_equal(sum(weekly$n_cases), 3)
})

test_that("pv_resource_breakdown reports each user's involvement, by user only (no role column in the real log)", {
  breakdown <- pv_resource_breakdown(demo_log())
  expect_setequal(breakdown$user_id, c("u1", "u2"))
  u1 <- breakdown[breakdown$user_id == "u1", ]
  expect_equal(u1$n_cases, 2) # involved in sessions 1 and 3
})

test_that("pv_variant_table groups identical traces and averages their conformance", {
  variants <- pv_variant_table(demo_log(), demo_diagram())
  expect_equal(nrow(variants), 2) # "a -> b -> c" (sessions 1,3) and "a -> b" (session 2)
  top <- variants[variants$n_cases == 2, ]
  expect_equal(top$trace, "a -> b -> c")
  expect_equal(top$mean_conformance, 1)
})

test_that("pv_coverage excludes deprecated processes from the denominator", {
  registry <- structure(
    list(processes = data.frame(
      process_id = c("a", "b", "c", "d"),
      status = c("instrumented", "designed", "not_started", "deprecated"),
      stringsAsFactors = FALSE
    )),
    class = "pv_registry"
  )
  cov <- pv_coverage(registry)
  expect_equal(cov$instrumented, 1)
  expect_equal(cov$total, 3)
  expect_equal(cov$pct, 1 / 3)
})
