#' Which named outcome(s) a case's trace reached
#'
#' A case can reach zero outcomes (nothing terminal logged yet --
#' `"incomplete"`), exactly one, or (for an `outcome_exclusivity = "multi"`
#' process) more than one -- e.g. a workover proposal logged alongside an
#' escalation. Matching is by activity id present anywhere in the trace,
#' not position, since a wrap-up step (a report signoff) can legitimately
#' come after the decision it documents.
#'
#' @param trace character vector: one case's activity ids, in the order
#'   they occurred.
#' @param outcomes an `outcomes` data.frame from a [pv_load_ideal_process()]
#'   result (zero rows is fine -- every case is then `"incomplete"`).
#' @return a list: `outcome_ids` (character vector, in the order they
#'   occurred in the trace; length 0 if none fired), `status`
#'   (`"completed"` if at least one fired, else `"incomplete"`).
#' @export
pv_case_outcome <- function(trace, outcomes) {
  trace <- as.character(trace)
  if (length(trace) == 0 || is.null(outcomes) || nrow(outcomes) == 0) {
    return(list(outcome_ids = character(0), status = "incomplete"))
  }

  hit_activities <- intersect(outcomes$activity_id, trace)
  if (length(hit_activities) == 0) {
    return(list(outcome_ids = character(0), status = "incomplete"))
  }

  ordered_activities <- hit_activities[order(match(hit_activities, trace))]
  outcome_ids <- outcomes$id[match(ordered_activities, outcomes$activity_id)]
  list(outcome_ids = outcome_ids, status = "completed")
}

#' Per-case outcome table
#'
#' The per-case view behind [pv_outcome_distribution()]'s aggregate, and
#' the natural left-hand side for [pv_join_external_outcomes()] -- one row
#' per case, ready to attach a real-world result to.
#'
#' @param events a case-linked events `data.frame` (`case_id`,
#'   `activity_id`, `timestamp`).
#' @param ideal_process a [pv_load_ideal_process()] result.
#' @return a `data.frame`, one row per case: `case_id`, `outcome_ids`
#'   (comma-joined; `""` if none), `status` (`"completed"`/`"incomplete"`).
#' @export
pv_case_outcome_table <- function(events, ideal_process) {
  stopifnot(is.data.frame(events), all(c("case_id", "activity_id", "timestamp") %in% names(events)))
  if (nrow(events) == 0) {
    return(data.frame(case_id = character(0), outcome_ids = character(0), status = character(0)))
  }

  ordered <- events[order(events$case_id, events$timestamp), ]
  traces <- split(ordered$activity_id, ordered$case_id)

  rows <- lapply(names(traces), function(cid) {
    res <- pv_case_outcome(traces[[cid]], ideal_process$outcomes)
    data.frame(
      case_id = cid, outcome_ids = paste(res$outcome_ids, collapse = ", "),
      status = res$status, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Outcome distribution across a process's cases
#'
#' Counts how often each named outcome (see [pv_load_ideal_process()]'s
#' `$outcomes`) was reached, plus how many cases reached none yet
#' (`"incomplete"`). For an `outcome_exclusivity = "multi"` process a case
#' can count toward more than one outcome, so percentages need not sum to
#' 100 -- each is simply the share of cases that reached that outcome.
#'
#' This is deliberately a separate metric from conformance: which decision
#' a case landed on is a business result, not a measure of whether the
#' right steps were followed to get there (see [pv_case_conformance()]).
#'
#' @param events a case-linked events `data.frame`.
#' @param ideal_process a [pv_load_ideal_process()] result.
#' @return a `data.frame`, most common first: `outcome_id`, `label`,
#'   `kind`, `tier`, `n_cases`, `pct`. Includes an `"incomplete"` row
#'   (`kind`/`tier` `NA`) whenever any case reached no outcome.
#' @export
pv_outcome_distribution <- function(events, ideal_process) {
  outcomes <- ideal_process$outcomes
  empty <- data.frame(
    outcome_id = character(0), label = character(0), kind = character(0),
    tier = character(0), n_cases = integer(0), pct = numeric(0)
  )
  if (nrow(events) == 0) {
    return(empty)
  }

  ordered <- events[order(events$case_id, events$timestamp), ]
  traces <- split(ordered$activity_id, ordered$case_id)
  n_total <- length(traces)

  results <- lapply(traces, pv_case_outcome, outcomes = outcomes)
  fired <- unlist(lapply(results, `[[`, "outcome_ids"))
  n_incomplete <- sum(vapply(results, function(r) r$status == "incomplete", logical(1)))

  if (nrow(outcomes) == 0) {
    counts <- data.frame(outcome_id = character(0), n_cases = integer(0))
  } else {
    tab <- table(factor(fired, levels = outcomes$id))
    counts <- data.frame(outcome_id = names(tab), n_cases = as.integer(tab), stringsAsFactors = FALSE)
  }

  out <- merge(
    data.frame(
      outcome_id = outcomes$id, label = outcomes$label, kind = outcomes$kind, tier = outcomes$tier,
      stringsAsFactors = FALSE
    ),
    counts,
    by = "outcome_id", all.x = TRUE
  )
  if (nrow(out) > 0) out$n_cases[is.na(out$n_cases)] <- 0L

  if (n_incomplete > 0) {
    out <- rbind(out, data.frame(
      outcome_id = "incomplete", label = "Incomplete (no outcome reached yet)",
      kind = NA_character_, tier = NA_character_, n_cases = n_incomplete, stringsAsFactors = FALSE
    ))
  }

  if (nrow(out) == 0) {
    return(empty)
  }
  out$pct <- round(100 * out$n_cases / n_total, 1)
  out <- out[order(-out$n_cases), ]
  rownames(out) <- NULL
  out
}

#' Join externally-tracked real-world results onto mined per-case outcomes
#'
#' proviz doesn't own or connect to whatever system tracks a decision's
#' eventual real-world result (did the workover help, did the well come in
#' on target) -- each outcome's `outcome_tracking` pointer in the ideal
#' process definition just documents where that lives. This is the join
#' point: export a plain table from that system, keyed the same way, and
#' get back the mined case data with those results attached -- ready for a
#' by-variant or by-resource comparison (e.g. "cases handled by engineer X,
#' via a different path than most, show a better linked result").
#'
#' @param case_outcomes a `data.frame` with a join column (default
#'   `"case_id"`) -- e.g. [pv_case_outcome_table()]'s result, optionally
#'   merged with resource/variant info first.
#' @param external_results a `data.frame` from whatever system tracks the
#'   real-world result, with a matching join column plus at least one
#'   metric column.
#' @param by join column name, present in both data frames. Defaults to
#'   `"case_id"` -- pass e.g. `"entity_id"` if that's what the external
#'   system's export is keyed on instead.
#' @return `case_outcomes` left-joined with `external_results` on `by`.
#' @export
pv_join_external_outcomes <- function(case_outcomes, external_results, by = "case_id") {
  stopifnot(is.data.frame(case_outcomes), is.data.frame(external_results))
  if (!by %in% names(case_outcomes)) {
    stop("proviz: join column '", by, "' not found in case_outcomes", call. = FALSE)
  }
  if (!by %in% names(external_results)) {
    stop("proviz: join column '", by, "' not found in external_results", call. = FALSE)
  }
  merge(case_outcomes, external_results, by = by, all.x = TRUE)
}
