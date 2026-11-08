test_that("pv_safe_cor is NA for too few points or zero variance, else a rounded Pearson r", {
  expect_true(is.na(pv_safe_cor(1:2, c(1, 2))))
  expect_true(is.na(pv_safe_cor(c(1, 1, 1), c(1, 2, 3))))
  expect_equal(pv_safe_cor(1:5, 1:5), 1)
  expect_equal(pv_safe_cor(1:5, 5:1), -1)
})

test_that("pv_score_time_trend detects an upward trend per user and app-wide", {
  dates <- seq(as.Date("2026-01-01"), by = "week", length.out = 5)
  log <- data.frame(
    started_at = as.POSIXct(c(dates, dates)),
    user_id = rep(c("up", "flat"), each = 5),
    total_weight = c(1, 2, 3, 4, 5, 3, 3, 3, 3, 3), stringsAsFactors = FALSE
  )
  by_user <- pv_score_time_trend(log, by = "user")
  expect_equal(by_user$r[by_user$user_id == "up"], 1)
  expect_true(is.na(by_user$r[by_user$user_id == "flat"])) # zero variance

  app <- pv_score_time_trend(log, by = "app")
  expect_equal(app$r, 1) # pooled weekly totals still trend upward
})

test_that("pv_score_time_trend returns the empty schema for zero rows", {
  empty <- data.frame(started_at = as.POSIXct(character(0)), user_id = character(0), total_weight = numeric(0))
  expect_equal(nrow(pv_score_time_trend(empty, by = "user")), 0)
  expect_true(is.na(pv_score_time_trend(empty, by = "app")$r))
})

test_that("pv_case_score_conformance_table attributes each session to its most-frequent user", {
  log <- data.frame(
    session_id = c("c1", "c1", "c1"), step_id = c("a", "b", "c"),
    started_at = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 09:05", "2026-01-01 09:10")),
    user_id = c("u1", "u1", "u2"), total_weight = c(5, 5, 5), stringsAsFactors = FALSE
  )
  diagram <- list(edges = data.frame(from_step = c("a", "b"), to_step = c("b", "c"), stringsAsFactors = FALSE))

  tab <- pv_case_score_conformance_table(log, diagram)
  expect_equal(tab$user_id, "u1") # 2 of 3 rows are u1's
  expect_equal(tab$total_score, 15)
  expect_equal(tab$conformance, 1)
})

test_that("pv_score_compliance_correlation correlates session score against session conformance, by user or app", {
  log <- data.frame(
    session_id = c("c1", "c1", "c2", "c2", "c3", "c3", "c3"),
    step_id = c("a", "b", "a", "x", "a", "b", "c"),
    started_at = as.POSIXct("2026-01-01 09:00") + c(0, 60, 0, 60, 0, 60, 120),
    user_id = "u1", total_weight = c(2, 2, 1, 1, 10, 10, 10), stringsAsFactors = FALSE
  ) # c2 takes an undeclared transition (a->x) and scores lowest; c1 and c3 both conform, c3 scores highest
  diagram <- list(edges = data.frame(from_step = c("a", "b"), to_step = c("b", "c"), stringsAsFactors = FALSE))

  by_user <- pv_score_compliance_correlation(log, diagram, by = "user")
  expect_equal(by_user$n_cases, 3)

  by_app <- pv_score_compliance_correlation(log, diagram, by = "app")
  expect_equal(by_app$n_cases, 3)
  expect_gt(by_app$r, 0) # higher score goes with higher conformance here
})

test_that("pv_app_log unions every monitored (process, subprocess) slot for an app", {
  reg <- structure(
    list(subprocesses = data.frame(
      process_id = c("p1", "p2"), app = c("shared_app", "shared_app"),
      subprocess = c("sub1", "sub2"), log_path = c("x", "y"), stringsAsFactors = FALSE
    )),
    class = "pv_registry"
  )
  log1 <- data.frame(session_id = "c1", step_id = "a", started_at = as.POSIXct("2026-01-01"), user_id = "u1", total_weight = 1, stringsAsFactors = FALSE)
  log2 <- data.frame(session_id = "c2", step_id = "b", started_at = as.POSIXct("2026-01-02"), user_id = "u2", total_weight = 1, stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    pv_pull_subprocess_log = function(registry, pid, sp, ...) if (sp == "sub1") log1 else log2,
    .package = "proviz"
  )

  out <- pv_app_log(reg, "shared_app")
  expect_setequal(out$session_id, c("c1", "c2"))
})

test_that("pv_iga_compliance_correlation pairs each app's IGA with its mean conformance", {
  reg <- structure(
    list(
      processes = data.frame(process_id = "p1", process = "P1", status = "instrumented", stringsAsFactors = FALSE),
      subprocesses = data.frame(
        process_id = "p1", app = "app1", subprocess = "sub1", log_path = "x",
        adoption_threshold = 1, stringsAsFactors = FALSE
      )
    ),
    class = "pv_registry"
  )
  conf <- data.frame(case_id = "c1", score = 1, n_deviations = 0L, n_transitions = 1L, n_nodes = 2L)
  testthat::local_mocked_bindings(pv_subprocess_conformance = function(...) conf, .package = "proviz")
  adoption <- list(app1 = list(users = data.frame(user_id = "u1", trained = TRUE, stringsAsFactors = FALSE), satisfaction = 90))

  res <- pv_iga_compliance_correlation(reg, adoption)
  expect_equal(res$data$app, "app1")
  expect_equal(res$data$mean_conformance, 1)
  expect_true(is.na(res$r)) # only one app -- not enough points
})

test_that("pv_app_engagement_summary returns one row per monitored app", {
  reg <- local_fixture_registry()
  summary <- pv_app_engagement_summary(reg)
  expect_equal(summary$app, "typecurves")
  expect_equal(summary$n_users, 1)
  expect_equal(summary$n_cases, 1) # one session in the fixture log -- too few to estimate an r
  expect_true(is.na(summary$compliance_r))
})

test_that("pv_app_engagement_summary returns the empty schema when no app is monitored", {
  reg <- local_fixture_registry()
  reg$subprocesses$log_path <- NA_character_
  expect_equal(nrow(pv_app_engagement_summary(reg)), 0)
})
