#' Conformance of one observed node sequence against a declared diagram
#'
#' An ideal diagram (see [pv_load_registry()]'s `$process_diagrams`/
#' `$subprocess_diagrams`) is a directed graph, not an enumerated list of
#' acceptable sequences -- cycles (rework loops) are just back-edges, no
#' combinatorial blow-up needed to cover every branch. Conformance is
#' therefore a graph-walk check: for each consecutive pair in `trace`, is
#' that transition one of the declared edges? Deviations are counted, not
#' just detected, so this stays a graduated score rather than a flat
#' pass/fail -- a trace with one bad transition out of twenty reads very
#' differently from one with ten out of twenty.
#'
#' A trace of 0 or 1 nodes has no transitions to judge, so it's treated as
#' vacuously conformant (`score = 1`) rather than `NA` -- there is no
#' transition in it that *isn't* declared.
#'
#' @param trace character vector: the observed node ids, in the order
#'   they occurred (a session's steps ordered by first-seen time, or a
#'   case's subprocess visits).
#' @param edges a `data.frame` of declared edges (e.g. a diagram's
#'   `$edges`), or `NULL`/zero rows if none are declared.
#' @param from_col,to_col column names in `edges` naming the edge's
#'   source/target node id. Defaults match `$subprocess_diagrams`'
#'   `$edges` shape (`from_step`/`to_step`); pass `"from_subprocess"`/
#'   `"to_subprocess"` for a `$process_diagrams` edge table.
#' @return a list: `score` (0-1, higher is better), `n_deviations` (count
#'   of transitions not found among `edges`), `n_transitions`
#'   (`length(trace) - 1`, floored at 0).
#' @export
pv_graph_conformance <- function(trace, edges, from_col = "from_step", to_col = "to_step") {
  trace <- as.character(trace)
  n <- length(trace)
  if (n < 2) {
    return(list(score = 1, n_deviations = 0L, n_transitions = 0L))
  }

  n_transitions <- n - 1L
  used <- paste(trace[-n], trace[-1], sep = "\u2192")

  if (is.null(edges) || nrow(edges) == 0) {
    return(list(score = 0, n_deviations = n_transitions, n_transitions = n_transitions))
  }

  declared <- paste(edges[[from_col]], edges[[to_col]], sep = "\u2192")
  n_deviations <- sum(!used %in% declared)
  list(score = 1 - n_deviations / n_transitions, n_deviations = n_deviations, n_transitions = n_transitions)
}

#' Conformance of every case in a named list of traces against one
#' declared diagram
#'
#' @param traces a named list of character vectors (the node sequence for
#'   each case/session, keyed by case/session id) -- e.g.
#'   [pv_session_step_sequences()]'s result.
#' @param edges,from_col,to_col see [pv_graph_conformance()].
#' @return a `data.frame`, one row per entry in `traces`: `case_id`,
#'   `score`, `n_deviations`, `n_transitions`, `n_nodes` (`length(trace)`).
#' @export
pv_diagram_conformance <- function(traces, edges, from_col = "from_step", to_col = "to_step") {
  empty <- data.frame(
    case_id = character(0), score = numeric(0), n_deviations = integer(0),
    n_transitions = integer(0), n_nodes = integer(0)
  )
  if (length(traces) == 0) {
    return(empty)
  }

  rows <- lapply(names(traces), function(cid) {
    res <- pv_graph_conformance(traces[[cid]], edges, from_col, to_col)
    data.frame(
      case_id = cid, score = res$score, n_deviations = res$n_deviations,
      n_transitions = res$n_transitions, n_nodes = length(traces[[cid]]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
