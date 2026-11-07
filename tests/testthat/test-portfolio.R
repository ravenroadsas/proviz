fixture_portfolio_registry <- function() {
  subprocesses <- data.frame(
    process_id = c("proc1", "proc1"), app = c("app1", "app2"), subprocess = c("sub1", "sub2"),
    app_link = NA_character_, log_path = c("x", NA_character_), adoption_threshold = NA_real_,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      processes = data.frame(
        process_id = c("proc1", "proc2"), macroprocess = c("mp1", "mp1"),
        process = c("Proc One", "Proc Two"), owner = c("o1", "o2"),
        status = c("instrumented", "not_started"), loop_length = c("short", "short"),
        stringsAsFactors = FALSE
      ),
      subprocesses = subprocesses,
      process_diagrams = list(
        proc1 = list(
          edges = data.frame(from_subprocess = character(0), to_subprocess = character(0)),
          outcomes = data.frame(
            id = "approved", label = "Approved", kind = "decision", subprocess = "sub2",
            tier = "primary", outcome_tracking_app = NA_character_, outcome_tracking_join_key = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      ),
      subprocess_diagrams = list(
        "proc1::sub2" = list(
          outcomes = data.frame(
            id = "approved", label = "Approved", kind = "decision", step_id = "a_approve",
            tier = "primary", outcome_tracking_app = NA_character_, outcome_tracking_join_key = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      ),
      clicks = list()
    ),
    class = "pv_registry"
  )
}

test_that("pv_filter_log keeps a session whole when filtering by user", {
  log <- data.frame(
    session_id = c("1", "1", "2"), step_id = c("a", "b", "a"),
    started_at = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 10:00", "2026-01-02 09:00")),
    user_id = c("u1", "u2", "u3"), stringsAsFactors = FALSE
  )
  filtered <- pv_filter_log(log, users = "u2")
  expect_setequal(unique(filtered$session_id), "1")
  expect_equal(nrow(filtered), 2) # all of session 1's rows kept
})

test_that("pv_app_coverage counts each app individually", {
  reg <- fixture_portfolio_registry()
  cov <- pv_app_coverage(reg)
  by_proc <- cov$by_process[cov$by_process$process_id == "proc1", ]
  expect_equal(by_proc$n_apps, 2)
  expect_equal(by_proc$n_apps_monitored, 1) # only sub1 has a log_path
  expect_equal(cov$portfolio$n_apps, 2) # proc2 has no apps declared
  expect_equal(cov$portfolio$n_apps_monitored, 1)
})

test_that("pv_weekly_active_users anchors to the most recent session, not Sys.time()", {
  log <- data.frame(
    user_id = c("u1", "u2", "u3"),
    started_at = as.POSIXct(c("2020-01-01", "2020-01-15", "2020-01-20"))
  )
  expect_equal(pv_weekly_active_users(log), 2) # u2, u3 within 7 days of the latest (u3's) timestamp
})

test_that("pv_weekly_active_users returns 0 for zero rows", {
  expect_equal(pv_weekly_active_users(data.frame(user_id = character(0), started_at = as.POSIXct(character(0)))), 0L)
})

test_that("pv_final_subprocesses prefers subprocesses named by declared process_outcomes", {
  reg <- fixture_portfolio_registry()
  finals <- pv_final_subprocesses(reg, "proc1")
  expect_equal(finals$subprocess, "sub2")
})

test_that("pv_final_subprocesses falls back to sink nodes when no outcomes are declared, and to every node when no edges either", {
  reg <- fixture_portfolio_registry()
  reg$process_diagrams$proc1$outcomes <- reg$process_diagrams$proc1$outcomes[0, ]
  reg$process_diagrams$proc1$edges <- data.frame(from_subprocess = "sub1", to_subprocess = "sub2")
  finals <- pv_final_subprocesses(reg, "proc1")
  expect_equal(finals$subprocess, "sub2") # sub1 has an outgoing edge, sub2 doesn't

  reg$process_diagrams[["proc1"]] <- NULL
  finals2 <- pv_final_subprocesses(reg, "proc1")
  expect_setequal(finals2$subprocess, c("sub1", "sub2")) # no declared flow at all
})

test_that("pv_process_completion_rate reshapes the outcome distribution into a completion fraction", {
  reg <- fixture_portfolio_registry()
  testthat::local_mocked_bindings(
    pv_process_outcome_distribution = function(...) {
      data.frame(outcome_id = c("approved", "incomplete"), n_cases = c(1, 1))
    },
    .package = "proviz"
  )
  rate <- pv_process_completion_rate(reg, "proc1")
  expect_equal(rate, list(n_cases = 2, n_completed = 1, pct = 50))
})

test_that("pv_portfolio_completion sums cases/completions across instrumented processes before dividing", {
  reg <- fixture_portfolio_registry()
  testthat::local_mocked_bindings(
    pv_process_completion_rate = function(registry, process_id) list(n_cases = 10, n_completed = 5, pct = 50),
    .package = "proviz"
  )
  completion <- pv_portfolio_completion(reg)
  # Only "proc1" is instrumented -- "proc2" is not_started and excluded.
  expect_equal(completion, list(n_cases = 10, n_completed = 5, pct = 50))
})

test_that("pv_portfolio_kpis unions logs across instrumented processes without double counting", {
  reg <- fixture_portfolio_registry()
  fake_log <- data.frame(
    session_id = c("c1", "c2"), user_id = c("u1", "u2"),
    started_at = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 09:05"))
  )
  testthat::local_mocked_bindings(
    pv_pull_process_log = function(registry, pid) fake_log,
    pv_portfolio_completion = function(...) list(n_cases = 2, n_completed = 1, pct = 50),
    .package = "proviz"
  )
  pk <- pv_portfolio_kpis(reg)
  expect_equal(pk$n_processes, 2)
  expect_equal(pk$n_instrumented, 1)
  expect_equal(pk$n_apps, 2)
  expect_equal(pk$n_apps_monitored, 1)
  expect_equal(pk$n_cases, 2)
  expect_equal(pk$n_users, 2)
  expect_equal(pk$completion_pct, 50)
})

test_that("pv_portfolio_heatmap counts distinct sessions per user per process", {
  reg <- fixture_portfolio_registry()
  fake_log <- data.frame(session_id = c("c1", "c1", "c2"), user_id = c("u1", "u1", "u2"), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(pv_pull_process_log = function(...) fake_log, .package = "proviz")
  hm <- pv_portfolio_heatmap(reg)
  expect_equal(sum(hm$n_cases[hm$user_id == "u1"]), 1) # u1 touched only 1 distinct session, not 2 rows
  expect_equal(sum(hm$n_cases[hm$user_id == "u2"]), 1)
})

test_that("pv_portfolio_heatmap returns the empty schema when no process is instrumented", {
  reg <- fixture_portfolio_registry()
  reg$processes$status <- "not_started"
  hm <- pv_portfolio_heatmap(reg)
  expect_equal(nrow(hm), 0)
})

test_that("pv_app_heatmap counts distinct sessions per user per monitored app, scoped to process_ids", {
  reg <- fixture_portfolio_registry()
  fake_log <- data.frame(session_id = c("c1", "c1", "c2"), user_id = c("u1", "u1", "u2"), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(pv_pull_subprocess_log = function(...) fake_log, .package = "proviz")

  hm <- pv_app_heatmap(reg)
  # Only sub1 is monitored (has a log_path) -- sub2 is skipped entirely.
  expect_equal(unique(hm$subprocess), "sub1")
  expect_equal(sum(hm$n_cases[hm$user_id == "u1"]), 1)

  scoped <- pv_app_heatmap(reg, process_ids = "proc2")
  expect_equal(nrow(scoped), 0) # proc2 has no apps at all
})

test_that("pv_heatmap_columns labels columns by process alone, or 'process / app' when breaking down", {
  long <- data.frame(process = c("Proc One", "Proc One"), app = c("App A", "App B"), stringsAsFactors = FALSE)
  by_process <- pv_heatmap_columns(long, by_app = FALSE)
  expect_equal(by_process$col, c("Proc One", "Proc One"))

  by_app <- pv_heatmap_columns(long, by_app = TRUE)
  expect_equal(by_app$col, c("Proc One / App A", "Proc One / App B"))
})

test_that("pv_heatmap_matrix zero-fills missing column/user combinations", {
  long <- data.frame(col = c("P1", "P1", "P2"), user_id = c("u1", "u2", "u1"), n_cases = c(3, 1, 2), stringsAsFactors = FALSE)
  mat <- pv_heatmap_matrix(long)
  expect_equal(mat$z["u1", "P1"], 3)
  expect_equal(mat$z["u2", "P2"], 0) # u2 never touched P2 -- zero, not missing
})
