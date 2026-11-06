test_that("pv_safe_cor is NA for too few points or zero variance, else a rounded Pearson r", {
  expect_true(is.na(pv_safe_cor(1:2, c(1, 2))))
  expect_true(is.na(pv_safe_cor(c(1, 1, 1), c(1, 2, 3))))
  expect_equal(pv_safe_cor(1:5, 1:5), 1)
  expect_equal(pv_safe_cor(1:5, 5:1), -1)
})

test_that("pv_score_time_trend detects an upward trend per user and app-wide", {
  dates <- seq(as.Date("2026-01-01"), by = "week", length.out = 5)
  events <- data.frame(
    timestamp = as.POSIXct(c(dates, dates)),
    user_id = rep(c("up", "flat"), each = 5),
    score = c(1, 2, 3, 4, 5, 3, 3, 3, 3, 3), stringsAsFactors = FALSE
  )
  by_user <- pv_score_time_trend(events, by = "user")
  expect_equal(by_user$r[by_user$user_id == "up"], 1)
  expect_true(is.na(by_user$r[by_user$user_id == "flat"])) # zero variance

  app <- pv_score_time_trend(events, by = "app")
  expect_equal(app$r, 1) # pooled weekly totals still trend upward
})

test_that("pv_score_time_trend returns the empty schema for zero events", {
  empty <- data.frame(timestamp = as.POSIXct(character(0)), user_id = character(0), score = numeric(0))
  expect_equal(nrow(pv_score_time_trend(empty, by = "user")), 0)
  expect_true(is.na(pv_score_time_trend(empty, by = "app")$r))
})

test_that("pv_case_score_conformance_table attributes each case to its most-frequent user", {
  events <- data.frame(
    case_id = c("c1", "c1", "c1"), activity_id = c("a", "b", "c"),
    timestamp = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 09:05", "2026-01-01 09:10")),
    user_id = c("u1", "u1", "u2"), stringsAsFactors = FALSE
  )
  scored <- cbind(events, score = c(5, 5, 5))
  ideal <- structure(list(variants = list(c("a", "b", "c"))), class = "pv_ideal_process")

  tab <- pv_case_score_conformance_table(events, scored, ideal)
  expect_equal(tab$user_id, "u1") # 2 of 3 events are u1's
  expect_equal(tab$total_score, 15)
  expect_equal(tab$conformance, 1)
})

test_that("pv_score_compliance_correlation correlates case score against case conformance, by user or app", {
  events <- data.frame(
    case_id = c("c1", "c1", "c2", "c3", "c3", "c3"),
    activity_id = c("a", "b", "a", "a", "b", "c"),
    timestamp = as.POSIXct("2026-01-01 09:00") + c(0, 60, 0, 0, 60, 120),
    user_id = "u1", stringsAsFactors = FALSE
  )
  scored <- cbind(events, score = c(1, 1, 1, 10, 10, 10)) # c3 (full trace) scores highest and conforms best
  ideal <- structure(list(variants = list(c("a", "b", "c"))), class = "pv_ideal_process")

  by_user <- pv_score_compliance_correlation(events, scored, ideal, by = "user")
  expect_equal(by_user$n_cases, 3)

  by_app <- pv_score_compliance_correlation(events, scored, ideal, by = "app")
  expect_equal(by_app$n_cases, 3)
  expect_gt(by_app$r, 0) # higher score goes with higher conformance here
})

test_that("pv_app_scored_events unions every monitored (process, subprocess) slot for an app", {
  reg <- structure(
    list(subprocesses = data.frame(
      process_id = c("p1", "p2"), app = c("shared_app", "shared_app"),
      subprocess = c("sub1", "sub2"), duckdb_path = c("x", "y"), stringsAsFactors = FALSE
    )),
    class = "pv_registry"
  )
  ev1 <- data.frame(case_id = "c1", activity_id = "a", timestamp = as.POSIXct("2026-01-01"), user_id = "u1", stringsAsFactors = FALSE)
  ev2 <- data.frame(case_id = "c2", activity_id = "b", timestamp = as.POSIXct("2026-01-02"), user_id = "u2", stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    pv_pull_app_events = function(pv_con, registry, pid, sp) if (sp == "sub1") ev1 else ev2,
    .package = "proviz"
  )

  out <- pv_app_scored_events(NULL, reg, "shared_app")
  expect_setequal(out$case_id, c("c1", "c2"))
  expect_equal(out$score, c(0, 0)) # no ideal_process declared anywhere -- defaults to 0, not an error
})

test_that("pv_iga_compliance_correlation pairs each app's IGA with its mean conformance", {
  reg <- structure(
    list(
      processes = data.frame(process_id = "p1", process = "P1", status = "instrumented", stringsAsFactors = FALSE),
      subprocesses = data.frame(
        process_id = "p1", app = "app1", subprocess = "sub1", duckdb_path = "x",
        adoption_threshold = 1, stringsAsFactors = FALSE
      ),
      ideal = list("p1::sub1" = structure(list(variants = list(c("a", "b"))), class = "pv_ideal_process"))
    ),
    class = "pv_registry"
  )
  events <- data.frame(
    case_id = c("c1", "c1"), activity_id = c("a", "b"),
    timestamp = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 09:05")),
    user_id = "u1", stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(pv_pull_app_events = function(...) events, .package = "proviz")
  adoption <- list(app1 = list(users = data.frame(user_id = "u1", trained = TRUE, stringsAsFactors = FALSE), satisfaction = 90))

  res <- pv_iga_compliance_correlation(NULL, reg, adoption)
  expect_equal(res$data$app, "app1")
  expect_equal(res$data$mean_conformance, 1)
  expect_true(is.na(res$r)) # only one app -- not enough points
})
