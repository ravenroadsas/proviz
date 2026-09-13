#' Generalized edit (Levenshtein) distance between two token sequences
#'
#' Standard Levenshtein dynamic program, but operating on whole elements of
#' `a`/`b` (activity ids) rather than characters of a string -- so a
#' multi-word activity id like `"Postulate_For_Review"` is one edit unit,
#' not several. This is the only sequence-comparison primitive proviz needs
#' for conformance scoring, so it's implemented directly here rather than
#' pulling in a string-distance package (`stringdist`/`TraMineR`) whose
#' character-based distance would first need every activity id remapped to
#' a single character.
#'
#' @param a,b character vectors (activity-id sequences).
#' @return a single integer: the minimum number of insertions, deletions,
#'   or substitutions to turn `a` into `b`.
#' @export
pv_seq_edit_distance <- function(a, b) {
  a <- as.character(a)
  b <- as.character(b)
  n <- length(a)
  m <- length(b)
  if (n == 0) {
    return(m)
  }
  if (m == 0) {
    return(n)
  }

  prev <- 0:m
  for (i in seq_len(n)) {
    curr <- integer(m + 1)
    curr[1] <- i
    for (j in seq_len(m)) {
      cost <- if (identical(a[i], b[j])) 0L else 1L
      curr[j + 1] <- min(
        prev[j + 1] + 1L, # deletion from a
        curr[j] + 1L, # insertion into a
        prev[j] + cost # match or substitution
      )
    }
    prev <- curr
  }
  prev[m + 1]
}

#' Conformance of a single case's trace against a set of ideal variants
#'
#' Scores `trace` against every declared ideal variant and keeps the best
#' match -- a case is judged against whichever acceptable path it most
#' resembles, not an arbitrary single "happy path". Each distance is
#' normalized by the longer of the two sequences before comparing across
#' variants: variants can differ a lot in length (a straight-through path
#' vs. one with rework loops), so the same raw distance means a worse match
#' against a short variant than a long one -- picking "best" by raw
#' distance would pick the wrong variant whenever two are tied on raw edits
#' but not on length. The final score is `1 - normalized distance`, so
#' higher is better (`1` = exact match, `0` = maximally different).
#'
#' @param trace character vector: one case's activity ids, in the order
#'   they occurred.
#' @param variants a list of character vectors -- an ideal process's
#'   declared acceptable paths (see [pv_load_ideal_process()]).
#' @return a list: `score` (0-1, `NA` if `trace` or `variants` is empty),
#'   `distance` (the raw edit distance to the best-matching variant), and
#'   `best_variant` (its index into `variants`).
#' @export
pv_case_conformance <- function(trace, variants) {
  trace <- as.character(trace)
  if (length(trace) == 0 || length(variants) == 0) {
    return(list(score = NA_real_, distance = NA_integer_, best_variant = NA_integer_))
  }

  distances <- vapply(variants, function(v) pv_seq_edit_distance(trace, v), integer(1))
  denoms <- vapply(variants, function(v) max(length(trace), length(v)), integer(1))
  scores <- ifelse(denoms == 0, 1, 1 - (distances / denoms))
  best <- which.max(scores)

  list(score = unname(scores[best]), distance = unname(distances[best]), best_variant = unname(best))
}

#' Conformance of every case in a process against its ideal variants
#'
#' @param cases a `data.frame` of raw (event-level) rows for one process:
#'   `case_id`, `activity_id`, `timestamp`. One row per logged activity
#'   instance -- exactly the shape [pv_build_eventlog()] produces before
#'   it's wrapped as a `bupaR::eventlog`.
#' @param ideal_process a [pv_load_ideal_process()] result.
#' @return a `data.frame`, one row per case: `case_id`, `score` (see
#'   [pv_case_conformance()]), `n_activities`.
#' @export
pv_process_conformance <- function(cases, ideal_process) {
  stopifnot(is.data.frame(cases), all(c("case_id", "activity_id", "timestamp") %in% names(cases)))
  if (nrow(cases) == 0) {
    return(data.frame(case_id = character(0), score = numeric(0), n_activities = integer(0)))
  }

  cases <- cases[order(cases$case_id, cases$timestamp), ]
  traces <- split(cases$activity_id, cases$case_id)

  rows <- lapply(names(traces), function(cid) {
    res <- pv_case_conformance(traces[[cid]], ideal_process$variants)
    data.frame(case_id = cid, score = res$score, n_activities = length(traces[[cid]]), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
