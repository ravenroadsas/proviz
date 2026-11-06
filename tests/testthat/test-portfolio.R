fixture_portfolio_registry <- function() {
  subprocesses <- data.frame(
    process_id = c("proc1", "proc1"), app = c("app1", "app2"), subprocess = c("sub1", "sub2"),
    step = c("draft", "review"), step_label = c("Draft", "Review"),
    step_order = c(1L, 2L), app_order = c(1L, 1L),
    duckdb_path = c("x", NA_character_), stringsAsFactors = FALSE
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
      steps = pv_derive_steps(subprocesses),
      ideal = list(
        "proc1::sub2" = structure(
          list(outcomes = data.frame(
            id = "approved", label = "Approved", kind = "decision", activity_id = "a_approve",
            tier = "primary", outcome_tracking_app = NA_character_, outcome_tracking_join_key = NA_character_,
            stringsAsFactors = FALSE
          )),
          class = "pv_ideal_process"
        )
      )
    ),
    class = "pv_registry"
  )
}

fixture_final_step_events <- function() {
  data.frame(
    case_id = c("c1", "c1", "c2"), entity_id = NA, activity_id = c("a_start", "a_approve", "a_start"),
    timestamp = as.POSIXct(c("2026-01-01 10:00", "2026-01-01 11:00", "2026-01-02 10:00")),
    user_id = c("u1", "u1", "u2"), role = c("r1", "r1", "r2"), session_id = "s1",
    app = "app2", subprocess = "sub2", stringsAsFactors = FALSE
  )
}

test_that("pv_filter_events keeps a case whole when filtering by user", {
  events <- data.frame(
    case_id = c("1", "1", "2"), activity_id = c("a", "b", "a"),
    timestamp = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 10:00", "2026-01-02 09:00")),
    user_id = c("u1", "u2", "u3"), role = c("eng", "eng", "eng"), stringsAsFactors = FALSE
  )
  filtered <- pv_filter_events(events, users = "u2")
  expect_setequal(unique(filtered$case_id), "1")
  expect_equal(nrow(filtered), 2) # all of case 1's rows kept
})

test_that("pv_app_coverage counts each app individually, unlike pv_step_coverage", {
  reg <- fixture_portfolio_registry()
  cov <- pv_app_coverage(reg)
  by_proc <- cov$by_process[cov$by_process$process_id == "proc1", ]
  expect_equal(by_proc$n_apps, 2)
  expect_equal(by_proc$n_apps_monitored, 1) # only sub1 has a duckdb_path
  expect_equal(cov$portfolio$n_apps, 2) # proc2 has no apps declared
  expect_equal(cov$portfolio$n_apps_monitored, 1)
})

test_that("pv_step_coverage counts a step monitored if at least one of its apps has a duckdb_path", {
  reg <- fixture_portfolio_registry()
  cov <- pv_step_coverage(reg)
  by_proc <- cov$by_process[cov$by_process$process_id == "proc1", ]
  expect_equal(by_proc$n_steps, 2)
  expect_equal(by_proc$n_steps_monitored, 1) # only "draft" has a monitored app
  expect_equal(cov$portfolio$n_steps, 2) # proc2 has no steps declared
  expect_equal(cov$portfolio$n_steps_monitored, 1)
})

test_that("pv_weekly_active_users anchors to the most recent event, not Sys.time()", {
  events <- data.frame(
    user_id = c("u1", "u2", "u3"),
    timestamp = as.POSIXct(c("2020-01-01", "2020-01-15", "2020-01-20"))
  )
  expect_equal(pv_weekly_active_users(events), 2) # u2, u3 within 7 days of the latest (u3's) timestamp
})

test_that("pv_weekly_active_users returns 0 for zero events", {
  expect_equal(pv_weekly_active_users(data.frame(user_id = character(0), timestamp = as.POSIXct(character(0)))), 0L)
})

test_that("pv_business_outcome_distribution scores the final step's own declared outcomes", {
  reg <- fixture_portfolio_registry()
  dist <- pv_business_outcome_distribution(NULL, reg, "proc1", events = fixture_final_step_events())
  expect_equal(dist$outcome_id[dist$n_cases > 0], c("approved", "incomplete"))
  expect_equal(sum(dist$n_cases), 2)
})

test_that("pv_business_outcome_distribution returns the empty schema when the process has no steps", {
  reg <- fixture_portfolio_registry()
  dist <- pv_business_outcome_distribution(NULL, reg, "proc2")
  expect_equal(nrow(dist), 0)
})

test_that("pv_process_completion_rate reshapes the outcome distribution into a completion fraction", {
  reg <- fixture_portfolio_registry()
  testthat::local_mocked_bindings(
    pv_business_outcome_distribution = function(...) {
      data.frame(outcome_id = c("approved", "incomplete"), n_cases = c(1, 1))
    },
    .package = "proviz"
  )
  rate <- pv_process_completion_rate(NULL, reg, "proc1")
  expect_equal(rate, list(n_cases = 2, n_completed = 1, pct = 50))
})

test_that("pv_portfolio_completion sums cases/completions across instrumented processes before dividing", {
  reg <- fixture_portfolio_registry()
  testthat::local_mocked_bindings(
    pv_process_completion_rate = function(pv_con, registry, process_id) {
      list(n_cases = 10, n_completed = 5, pct = 50)
    },
    .package = "proviz"
  )
  completion <- pv_portfolio_completion(NULL, reg)
  # Only "proc1" is instrumented -- "proc2" is not_started and excluded.
  expect_equal(completion, list(n_cases = 10, n_completed = 5, pct = 50))
})

test_that("pv_portfolio_kpis unions events across instrumented processes without double counting", {
  reg <- fixture_portfolio_registry()
  fake_events <- data.frame(
    case_id = c("c1", "c2"), user_id = c("u1", "u2"),
    timestamp = as.POSIXct(c("2026-01-01 09:00", "2026-01-01 09:05"))
  )
  testthat::local_mocked_bindings(
    pv_pull_process_events = function(pv_con, registry, pid) fake_events,
    pv_portfolio_completion = function(...) list(n_cases = 2, n_completed = 1, pct = 50),
    .package = "proviz"
  )
  pk <- pv_portfolio_kpis(NULL, reg)
  expect_equal(pk$n_processes, 2)
  expect_equal(pk$n_instrumented, 1)
  expect_equal(pk$n_apps, 2)
  expect_equal(pk$n_apps_monitored, 1)
  expect_equal(pk$n_cases, 2)
  expect_equal(pk$n_users, 2)
  expect_equal(pk$completion_pct, 50)
})

test_that("pv_portfolio_heatmap counts distinct cases per user per process", {
  reg <- fixture_portfolio_registry()
  fake_events <- data.frame(
    case_id = c("c1", "c1", "c2"), user_id = c("u1", "u1", "u2"), stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(pv_pull_process_events = function(...) fake_events, .package = "proviz")
  hm <- pv_portfolio_heatmap(NULL, reg, by = "user")
  expect_equal(sum(hm$n_cases[hm$user_id == "u1"]), 1) # u1 touched only 1 distinct case, not 2 events
  expect_equal(sum(hm$n_cases[hm$user_id == "u2"]), 1)
})

test_that("pv_portfolio_heatmap returns the empty schema when no process is instrumented", {
  reg <- fixture_portfolio_registry()
  reg$processes$status <- "not_started"
  hm <- pv_portfolio_heatmap(NULL, reg, by = "role")
  expect_equal(nrow(hm), 0)
})

test_that("pv_app_heatmap counts distinct cases per user per monitored app, scoped to process_ids", {
  reg <- fixture_portfolio_registry()
  fake_events <- data.frame(
    case_id = c("c1", "c1", "c2"), user_id = c("u1", "u1", "u2"), stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(pv_pull_app_events = function(...) fake_events, .package = "proviz")

  hm <- pv_app_heatmap(NULL, reg, by = "user")
  # Only sub1 is monitored (has a duckdb_path) -- sub2 is skipped entirely.
  expect_equal(unique(hm$subprocess), "sub1")
  expect_equal(sum(hm$n_cases[hm$user_id == "u1"]), 1)

  scoped <- pv_app_heatmap(NULL, reg, by = "user", process_ids = "proc2")
  expect_equal(nrow(scoped), 0) # proc2 has no apps at all
})

test_that("pv_heatmap_columns labels columns by process alone, or 'process / app' when breaking down", {
  long <- data.frame(process = c("Proc One", "Proc One"), app = c("App A", "App B"), stringsAsFactors = FALSE)
  by_process <- pv_heatmap_columns(long, by_app = FALSE)
  expect_equal(by_process$col, c("Proc One", "Proc One"))

  by_app <- pv_heatmap_columns(long, by_app = TRUE)
  expect_equal(by_app$col, c("Proc One / App A", "Proc One / App B"))
})

test_that("pv_heatmap_matrix zero-fills missing column/dimension combinations", {
  long <- data.frame(col = c("P1", "P1", "P2"), user_id = c("u1", "u2", "u1"), n_cases = c(3, 1, 2), stringsAsFactors = FALSE)
  mat <- pv_heatmap_matrix(long, "user_id")
  expect_equal(mat$z["u1", "P1"], 3)
  expect_equal(mat$z["u2", "P2"], 0) # u2 never touched P2 -- zero, not missing
})
