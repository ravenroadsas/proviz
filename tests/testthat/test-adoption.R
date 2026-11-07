fixture_adoption_registry <- function() {
  subprocesses <- data.frame(
    process_id = "p1", app = "app1", subprocess = "sub1", app_link = NA_character_,
    log_path = "x", adoption_threshold = 5, stringsAsFactors = FALSE
  )
  structure(
    list(
      processes = data.frame(
        process_id = "p1", macroprocess = "mp1", process = "P1", owner = "o",
        status = "instrumented", loop_length = "short", stringsAsFactors = FALSE
      ),
      subprocesses = subprocesses
    ),
    class = "pv_registry"
  )
}

fixture_log_for_app1 <- function() {
  data.frame(
    session_id = c("s1", "s1", "s2"), step_id = c("a", "b", "a"),
    started_at = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 09:05", "2026-01-08 09:00")),
    user_id = c("u1", "u1", "u2"), nclicks = 1, total_weight = c(10, 2, 10),
    app = "app1", stringsAsFactors = FALSE
  )
}

test_that("pv_load_adoption parses one sheet per app and an app-wide satisfaction value", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  reg <- fixture_adoption_registry()
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(
    app1 = data.frame(user_id = c("u1", "u2", "u3"), trained = c(TRUE, TRUE, FALSE), satisfaction = c(85, 85, 85))
  ), file = path)

  adoption <- pv_load_adoption(path, reg)
  expect_equal(adoption$app1$users$user_id, c("u1", "u2", "u3"))
  expect_equal(adoption$app1$satisfaction, 85)
})

test_that("pv_load_adoption warns (not errors) on an unmatched app or sheet, and a missing column", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  reg <- fixture_adoption_registry()
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(
    "Typo Sheet" = data.frame(user_id = "x", trained = TRUE),
    app1 = data.frame(not_user_id = "u1")
  ), file = path)

  # "app1" has a matching sheet (just malformed -- missing user_id/trained),
  # so no "no sheet for app" warning fires; only the unmatched-sheet and
  # malformed-sheet warnings do.
  expect_warning(expect_warning(
    adoption <- pv_load_adoption(path, reg),
    "matching no known app"
  ), "missing a 'user_id'")
  expect_null(adoption[["app1"]])
})

test_that("pv_load_adoption uses the first satisfaction value and warns if they disagree", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  reg <- fixture_adoption_registry()
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(
    app1 = data.frame(user_id = c("u1", "u2"), trained = c(TRUE, FALSE), satisfaction = c(70, 90))
  ), file = path)

  expect_warning(adoption <- pv_load_adoption(path, reg), "more than one distinct satisfaction value")
  expect_equal(adoption$app1$satisfaction, 70)
})

test_that("pv_score_by_user/step/session aggregate total_weight directly -- no per-click lookup needed", {
  log <- fixture_log_for_app1()
  expect_equal(pv_score_by_user(log)$total_score, c(12, 10)) # u1=12, u2=10, highest first
  expect_equal(sort(pv_score_by_step(log)$total_score), c(2, 20)) # b=2, a=10+10=20
  expect_equal(nrow(pv_score_by_session(log)), 2)
})

test_that("pv_app_weekly_user_scores averages only over weeks a user was actually active", {
  reg <- fixture_adoption_registry()
  testthat::local_mocked_bindings(pv_pull_subprocess_log = function(...) fixture_log_for_app1(), .package = "proviz")

  weekly <- pv_app_weekly_user_scores(reg, "app1")
  u1 <- weekly[weekly$user_id == "u1", ]
  expect_equal(u1$avg_weekly_score, 12) # one active week, total 12
})

test_that("pv_app_iga computes trained/satisfaction/active components and NA-guards an incomplete IGA", {
  reg <- fixture_adoption_registry()
  testthat::local_mocked_bindings(pv_pull_subprocess_log = function(...) fixture_log_for_app1(), .package = "proviz")

  adoption <- list(app1 = list(
    users = data.frame(user_id = c("u1", "u2", "u3"), trained = c(TRUE, TRUE, FALSE), stringsAsFactors = FALSE),
    satisfaction = 90
  ))
  iga <- pv_app_iga(reg, "app1", adoption)
  expect_equal(iga$pct_trained, 66.7)
  expect_equal(iga$satisfaction, 90)
  expect_equal(iga$pct_active, 66.7) # u1 (avg 12) and u2 (avg 10) both exceed threshold 5; u3 never appears (0)
  expect_false(is.na(iga$iga))

  # Missing adoption data entirely -> everything NA.
  missing <- pv_app_iga(reg, "no_such_app", adoption)
  expect_true(is.na(missing$iga))
  expect_equal(missing$n_users, 0L)
})

test_that("pv_portfolio_iga returns one row per app", {
  reg <- fixture_adoption_registry()
  testthat::local_mocked_bindings(pv_pull_subprocess_log = function(...) fixture_log_for_app1(), .package = "proviz")
  adoption <- list(app1 = list(users = data.frame(user_id = "u1", trained = TRUE, stringsAsFactors = FALSE), satisfaction = 90))

  tab <- pv_portfolio_iga(reg, adoption)
  expect_equal(tab$app, "app1")
  expect_equal(tab$n_users, 1)
})
