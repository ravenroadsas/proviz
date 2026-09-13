demo_events <- function() {
  data.frame(
    case_id = c("1", "1", "1", "2", "2", "3", "3", "3"),
    activity_id = c("a", "b", "c", "a", "b", "a", "b", "c"),
    timestamp = as.POSIXct(c(
      "2026-01-05 09:00", "2026-01-05 10:00", "2026-01-05 12:00", # case 1: week of Jan 4
      "2026-01-06 09:00", "2026-01-06 09:30", # case 2: week of Jan 4, short
      "2026-01-15 09:00", "2026-01-15 11:00", "2026-01-15 13:00" # case 3: week of Jan 11
    )),
    user_id = c("u1", "u1", "u1", "u2", "u2", "u1", "u3", "u1"),
    role = c("eng", "eng", "eng", "analyst", "analyst", "eng", "reviewer", "eng"),
    stringsAsFactors = FALSE
  )
}

test_that("pv_filter_events keeps a case whole based on when it started", {
  events <- demo_events()
  filtered <- pv_filter_events(events, start_date = "2026-01-10")
  expect_setequal(unique(filtered$case_id), "3")
  expect_equal(nrow(filtered), 3) # all of case 3's rows kept, none truncated
})

test_that("pv_filter_events filters by role at the case level (keeps the whole matching case)", {
  events <- demo_events()
  filtered <- pv_filter_events(events, roles = "reviewer")
  expect_setequal(unique(filtered$case_id), "3")
  expect_equal(nrow(filtered), 3)
})

test_that("pv_case_durations returns per-case elapsed minutes", {
  events <- demo_events()
  durations <- pv_case_durations(events)
  expect_equal(durations$duration_mins[durations$case_id == "1"], 180)
  expect_equal(durations$duration_mins[durations$case_id == "2"], 30)
})

test_that("pv_process_kpis reports n_cases, n_users, and median-over-mean duration", {
  events <- demo_events()
  kpis <- pv_process_kpis(events)
  expect_equal(kpis$n_cases, 3)
  expect_equal(kpis$n_users, 3)
  expect_equal(kpis$median_duration_mins, stats::median(c(180, 30, 240)))
  expect_true(is.na(kpis$conformance_median)) # no ideal_process supplied
})

test_that("pv_process_kpis computes conformance when given an ideal process", {
  ideal <- structure(list(variants = list(c("a", "b", "c"))), class = "pv_ideal_process")
  kpis <- pv_process_kpis(demo_events(), ideal)
  expect_equal(kpis$conformance_median, 1) # cases 1 and 3 exact; case 2 partial
})

test_that("pv_process_kpis handles zero events", {
  kpis <- pv_process_kpis(demo_events()[0, ])
  expect_equal(kpis$n_cases, 0L)
  expect_true(is.na(kpis$median_duration_mins))
})

test_that("pv_weekly_usage buckets cases by the week they started", {
  weekly <- pv_weekly_usage(demo_events())
  expect_equal(nrow(weekly), 2)
  expect_equal(sum(weekly$n_cases), 3)
})

test_that("pv_resource_breakdown reports each user's dominant role and involvement", {
  breakdown <- pv_resource_breakdown(demo_events())
  expect_setequal(breakdown$user_id, c("u1", "u2", "u3"))
  u1 <- breakdown[breakdown$user_id == "u1", ]
  expect_equal(u1$role, "eng")
  expect_equal(u1$n_cases, 2) # involved in cases 1 and 3
})

test_that("pv_variant_table groups identical traces and averages their conformance", {
  ideal <- structure(list(variants = list(c("a", "b", "c"))), class = "pv_ideal_process")
  variants <- pv_variant_table(demo_events(), ideal)
  expect_equal(nrow(variants), 2) # "a->b->c" (cases 1,3) and "a->b" (case 2)
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
