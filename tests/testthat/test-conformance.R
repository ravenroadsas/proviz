test_that("pv_seq_edit_distance matches hand-computed distances", {
  expect_equal(pv_seq_edit_distance(c("a", "b", "c"), c("a", "b", "c")), 0)
  expect_equal(pv_seq_edit_distance(c("a", "b", "c"), c("a", "b")), 1) # one deletion
  expect_equal(pv_seq_edit_distance(c("a", "b"), c("a", "b", "c")), 1) # one insertion
  expect_equal(pv_seq_edit_distance(c("a", "b", "c"), c("a", "x", "c")), 1) # one substitution
  expect_equal(pv_seq_edit_distance(character(0), c("a", "b")), 2)
  expect_equal(pv_seq_edit_distance(c("a", "b"), character(0)), 2)
  expect_equal(pv_seq_edit_distance(character(0), character(0)), 0)
})

test_that("pv_seq_edit_distance treats a whole token as one edit unit", {
  # "ab" vs "a","b" as two tokens: not the same as string-level distance --
  # this is exactly the property that rules out reusing a char-based
  # string-distance package without remapping activity ids first.
  expect_equal(pv_seq_edit_distance("ab", c("a", "b")), 2)
})

test_that("pv_case_conformance picks the nearest variant and normalizes correctly", {
  variants <- list(
    c("a", "b", "c"),
    c("a", "b", "c", "d", "e")
  )
  exact <- pv_case_conformance(c("a", "b", "c"), variants)
  expect_equal(exact$score, 1)
  expect_equal(exact$best_variant, 1)

  near_long <- pv_case_conformance(c("a", "b", "c", "d"), variants)
  expect_equal(near_long$best_variant, 2)
  expect_equal(near_long$distance, 1)
  expect_equal(near_long$score, 1 - 1 / 5)

  off_path <- pv_case_conformance(c("x", "y", "z"), variants)
  expect_lt(off_path$score, 0.5)
})

test_that("pv_case_conformance returns NA for empty trace or no variants", {
  expect_true(is.na(pv_case_conformance(character(0), list(c("a", "b")))$score))
  expect_true(is.na(pv_case_conformance(c("a", "b"), list())$score))
})

test_that("pv_process_conformance scores each case using its time-ordered trace", {
  ideal <- list(variants = list(c("a", "b", "c")))
  class(ideal) <- "pv_ideal_process"

  cases <- data.frame(
    case_id = c("1", "1", "1", "2", "2"),
    activity_id = c("c", "a", "b", "a", "b"), # case 1 rows deliberately out of time order
    timestamp = as.POSIXct(c("2026-01-01 03:00", "2026-01-01 01:00", "2026-01-01 02:00",
                              "2026-01-01 01:00", "2026-01-01 02:00")),
    stringsAsFactors = FALSE
  )

  result <- pv_process_conformance(cases, ideal)
  result <- result[order(result$case_id), ]

  expect_equal(result$score[result$case_id == "1"], 1) # reordered by timestamp -> exact match
  expect_equal(result$n_activities, c(3L, 2L))
  expect_equal(result$score[result$case_id == "2"], 1 - 1 / 3) # missing "c"
})

test_that("pv_process_conformance returns the empty schema for no cases", {
  ideal <- list(variants = list(c("a", "b")))
  class(ideal) <- "pv_ideal_process"
  cases <- data.frame(case_id = character(0), activity_id = character(0), timestamp = as.POSIXct(character(0)))
  result <- pv_process_conformance(cases, ideal)
  expect_equal(nrow(result), 0)
})
