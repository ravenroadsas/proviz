test_that("pv_outcomes_display_table doesn't error when every row's kind/tier is NA", {
  # Regression test: vapply()'s default USE.NAMES behavior takes an
  # unnamed, all-NA character input as the *names* of its result, which
  # data.frame() then tries to use as row names -- erroring when every
  # row shares an NA kind/tier (e.g. a single declared deliverable, whose
  # tier is always NA, or an "incomplete" row with no real outcome).
  dist <- data.frame(
    outcome_id = c("publish_official", "incomplete"),
    label = c("Published", "Incomplete (no outcome reached yet)"),
    kind = c("deliverable", NA_character_), tier = c(NA_character_, NA_character_),
    n_cases = c(118, 22), pct = c(84.3, 15.7), stringsAsFactors = FALSE
  )
  display <- pv_outcomes_display_table(dist)
  expect_equal(nrow(display), 2)
  expect_equal(display$Outcome, c("Published", "Incomplete (no outcome reached yet)"))
  expect_match(display$Kind[1], "Deliverable")
})

test_that("pv_outcomes_display_table still labels a populated kind/tier correctly", {
  dist <- data.frame(
    outcome_id = "propose_workover", label = "Propose workover",
    kind = "decision", tier = "tactical", n_cases = 5, pct = 100,
    stringsAsFactors = FALSE
  )
  display <- pv_outcomes_display_table(dist)
  expect_match(display$Kind, "Decision")
  expect_match(display$Tier, "Tactical")
})

test_that("pv_cell_bar renders an optional sub_label caption below the bar", {
  with_sub <- pv_cell_bar("60%", 60, "Feb 2026")
  expect_match(with_sub, '<span class="pv-cellbar-sub">Feb 2026</span>', fixed = TRUE)

  without_sub <- pv_cell_bar("60%", 60)
  expect_false(grepl("pv-cellbar-sub", without_sub, fixed = TRUE))
})

test_that("pv_cell_bar ignores sub_label when the value itself is NA", {
  empty <- pv_cell_bar(NA_character_, NA_real_, "Feb 2026")
  expect_equal(empty, '<span class="pv-cellbar-empty">--</span>')
})
