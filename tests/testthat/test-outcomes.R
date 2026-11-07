demo_outcomes <- function() {
  data.frame(
    id = c("no_action", "change_esp_frequency", "propose_workover", "escalate_to_engineering"),
    label = c("No action", "Change ESP frequency", "Propose workover", "Escalate"),
    kind = c("decision", "decision", "decision", "decision"),
    step_id = c("Decide_No_Action", "Decide_Change_ESP_Frequency", "Decide_Propose_Workover", "Issue_Escalation"),
    tier = c("operational", "operational", "tactical", "tactical"),
    outcome_tracking_app = NA_character_, outcome_tracking_join_key = NA_character_,
    stringsAsFactors = FALSE
  )
}

test_that("pv_case_outcome finds a single fired outcome", {
  trace <- c("Data_Ingestion", "Decide_Change_ESP_Frequency", "Report_Signoff")
  res <- pv_case_outcome(trace, demo_outcomes())
  expect_equal(res$outcome_ids, "change_esp_frequency")
  expect_equal(res$status, "completed")
})

test_that("pv_case_outcome finds multiple fired outcomes in trace order", {
  trace <- c("Data_Ingestion", "Decide_Propose_Workover", "Surveillance_Reporting", "Issue_Escalation", "Report_Signoff")
  res <- pv_case_outcome(trace, demo_outcomes())
  expect_equal(res$outcome_ids, c("propose_workover", "escalate_to_engineering"))
  expect_equal(res$status, "completed")
})

test_that("pv_case_outcome reports incomplete when no outcome node is present", {
  trace <- c("Data_Ingestion", "Data_Quality_Check")
  res <- pv_case_outcome(trace, demo_outcomes())
  expect_equal(res$outcome_ids, character(0))
  expect_equal(res$status, "incomplete")
})

test_that("pv_case_outcome handles empty trace and zero-row outcomes", {
  expect_equal(pv_case_outcome(character(0), demo_outcomes())$status, "incomplete")
  expect_equal(pv_case_outcome(c("a", "b"), demo_outcomes()[0, ])$status, "incomplete")
})

test_that("pv_case_outcome_table returns one row per case with comma-joined outcomes", {
  traces <- list(
    "1" = c("Decide_No_Action", "Report_Signoff"),
    "2" = c("Decide_Propose_Workover", "Issue_Escalation"),
    "3" = c("Data_Ingestion")
  )
  tab <- pv_case_outcome_table(traces, demo_outcomes())
  tab <- tab[order(tab$case_id), ]

  expect_equal(tab$status, c("completed", "completed", "incomplete"))
  expect_equal(tab$outcome_ids[tab$case_id == "2"], "propose_workover, escalate_to_engineering")
  expect_equal(tab$outcome_ids[tab$case_id == "3"], "")
})

test_that("pv_outcome_distribution counts each outcome's share and an incomplete bucket", {
  traces <- list("1" = "Decide_No_Action", "2" = "Decide_No_Action", "3" = "Decide_Propose_Workover", "4" = "Data_Ingestion")
  dist <- pv_outcome_distribution(traces, demo_outcomes())

  expect_equal(dist$n_cases[dist$outcome_id == "no_action"], 2)
  expect_equal(dist$pct[dist$outcome_id == "no_action"], 50)
  expect_equal(dist$n_cases[dist$outcome_id == "incomplete"], 1)
  expect_equal(dist$n_cases[dist$outcome_id == "change_esp_frequency"], 0) # declared, never reached
})

test_that("pv_outcome_distribution lets percentages exceed 100 in total for a multi-outcome process", {
  traces <- list("1" = c("Decide_Propose_Workover", "Issue_Escalation"))
  dist <- pv_outcome_distribution(traces, demo_outcomes())
  expect_equal(sum(dist$pct), 200) # one case, two outcomes, 100% each
})

test_that("pv_outcome_distribution returns the empty schema for zero traces", {
  dist <- pv_outcome_distribution(list(), demo_outcomes())
  expect_equal(nrow(dist), 0)
})

test_that("pv_join_external_outcomes left-joins on the given key", {
  case_outcomes <- data.frame(case_id = c("1", "2", "3"), outcome_ids = c("propose_workover", "no_action", "no_action"), stringsAsFactors = FALSE)
  external <- data.frame(case_id = c("1", "2"), uplift_pct = c(12.5, 1.0), stringsAsFactors = FALSE)

  joined <- pv_join_external_outcomes(case_outcomes, external)
  joined <- joined[order(joined$case_id), ]
  expect_equal(joined$uplift_pct, c(12.5, 1.0, NA))
})

test_that("pv_join_external_outcomes errors clearly on a missing join column", {
  case_outcomes <- data.frame(case_id = "1", stringsAsFactors = FALSE)
  external <- data.frame(well_id = "1", metric = 1, stringsAsFactors = FALSE)
  expect_error(pv_join_external_outcomes(case_outcomes, external), "join column")
  expect_error(pv_join_external_outcomes(case_outcomes, external, by = "well_id"), "join column")
})
