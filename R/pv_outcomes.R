#' Which named outcome(s) a trace reached
#'
#' A case can reach zero outcomes (nothing terminal logged yet --
#' `"incomplete"`), exactly one, or (for an `outcome_exclusivity`-free
#' model, more than one is always allowed here) more than one. Matching
#' is by node id present anywhere in the trace, not position, since a
#' wrap-up step can legitimately come after the decision it documents.
#'
#' @param trace character vector: one case's node ids (steps, or
#'   subprocesses at process level), in the order they occurred.
#' @param outcomes an outcomes `data.frame` from a [pv_load_registry()]
#'   diagram (`$subprocess_diagrams[[key]]$outcomes` or
#'   `$process_diagrams[[pid]]$outcomes`) -- zero rows is fine (every
#'   case is then `"incomplete"`).
#' @param node_col the column in `outcomes` naming which node concludes
#'   it -- `"step_id"` (subprocess-level outcomes, the default) or
#'   `"subprocess"` (process-level outcomes).
#' @return a list: `outcome_ids` (character vector, in the order they
#'   occurred in the trace; length 0 if none fired), `status`
#'   (`"completed"` if at least one fired, else `"incomplete"`).
#' @export
pv_case_outcome <- function(trace, outcomes, node_col = "step_id") {
  trace <- as.character(trace)
  if (length(trace) == 0 || is.null(outcomes) || nrow(outcomes) == 0) {
    return(list(outcome_ids = character(0), status = "incomplete"))
  }

  hit_nodes <- intersect(outcomes[[node_col]], trace)
  if (length(hit_nodes) == 0) {
    return(list(outcome_ids = character(0), status = "incomplete"))
  }

  ordered_nodes <- hit_nodes[order(match(hit_nodes, trace))]
  outcome_ids <- outcomes$id[match(ordered_nodes, outcomes[[node_col]])]
  list(outcome_ids = outcome_ids, status = "completed")
}

#' Per-case outcome table
#'
#' @param traces a named list of character vectors (e.g.
#'   [pv_session_step_sequences()]'s result), keyed by case/session id.
#' @param outcomes,node_col see [pv_case_outcome()].
#' @return a `data.frame`, one row per entry in `traces`: `case_id`,
#'   `outcome_ids` (comma-joined; `""` if none), `status`
#'   (`"completed"`/`"incomplete"`).
#' @export
pv_case_outcome_table <- function(traces, outcomes, node_col = "step_id") {
  if (length(traces) == 0) {
    return(data.frame(case_id = character(0), outcome_ids = character(0), status = character(0)))
  }
  rows <- lapply(names(traces), function(cid) {
    res <- pv_case_outcome(traces[[cid]], outcomes, node_col)
    data.frame(
      case_id = cid, outcome_ids = paste(res$outcome_ids, collapse = ", "),
      status = res$status, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Outcome distribution across a set of traces
#'
#' Counts how often each named outcome was reached, plus how many cases
#' reached none yet (`"incomplete"`). A case can count toward more than
#' one outcome, so percentages need not sum to 100.
#'
#' This is deliberately a separate metric from conformance (see
#' [pv_graph_conformance()]): which outcome a case landed on is a
#' business result, not a measure of whether the right steps were
#' followed to get there.
#'
#' @param traces a named list of character vectors, keyed by case id.
#' @param outcomes,node_col see [pv_case_outcome()].
#' @return a `data.frame`, most common first: `outcome_id`, `label`,
#'   `kind`, `tier`, `n_cases`, `pct`. Includes an `"incomplete"` row
#'   (`kind`/`tier` `NA`) whenever any case reached no outcome.
#' @export
pv_outcome_distribution <- function(traces, outcomes, node_col = "step_id") {
  empty <- data.frame(
    outcome_id = character(0), label = character(0), kind = character(0),
    tier = character(0), n_cases = integer(0), pct = numeric(0)
  )
  if (length(traces) == 0 || is.null(outcomes) || nrow(outcomes) == 0) {
    return(empty)
  }

  n_total <- length(traces)
  results <- lapply(traces, pv_case_outcome, outcomes = outcomes, node_col = node_col)
  fired <- unlist(lapply(results, `[[`, "outcome_ids"))
  n_incomplete <- sum(vapply(results, function(r) r$status == "incomplete", logical(1)))

  tab <- table(factor(fired, levels = outcomes$id))
  counts <- data.frame(outcome_id = names(tab), n_cases = as.integer(tab), stringsAsFactors = FALSE)

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

#' This subprocess's own outcome distribution
#' @param registry a [pv_load_registry()] result.
#' @param process_id,subprocess identify the app.
#' @param log optionally, an already-pulled/filtered log to reuse (see
#'   [pv_filter_log()]) instead of pulling fresh.
#' @return a `data.frame`, [pv_outcome_distribution()]'s shape.
#' @export
pv_subprocess_outcome_distribution <- function(registry, process_id, subprocess, log = NULL) {
  empty <- data.frame(
    outcome_id = character(0), label = character(0), kind = character(0),
    tier = character(0), n_cases = integer(0), pct = numeric(0)
  )
  diagram <- registry$subprocess_diagrams[[paste(process_id, subprocess, sep = "::")]]
  if (is.null(diagram)) {
    return(empty)
  }
  if (is.null(log)) log <- pv_pull_subprocess_log(registry, process_id, subprocess)
  traces <- pv_session_step_sequences(log)
  pv_outcome_distribution(traces, diagram$outcomes, "step_id")
}

#' Which of a process's subprocesses conclude it
#'
#' Prefers the process's own declared `$process_diagrams[[pid]]$outcomes`
#' (each names the one subprocess that concludes it directly -- ground
#' truth, and the only option that works when the diagram has a cycle,
#' where a pure graph-theoretic "sink" -- no outgoing edge -- may not
#' exist at all: a rework loop back to an earlier subprocess means every
#' node has an outgoing edge). Falls back to sinks in the process-level
#' diagram (a subprocess with no outgoing edge) only when no outcomes are
#' declared yet; with no edges declared either, every subprocess counts
#' (there's no declared flow to say otherwise).
#'
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a `subprocesses`-shaped `data.frame`, filtered to the final
#'   node(s). Zero rows if the process has no subprocesses.
#' @export
pv_final_subprocesses <- function(registry, process_id) {
  subs <- registry$subprocesses[registry$subprocesses$process_id == process_id, ]
  if (nrow(subs) == 0) {
    return(subs)
  }
  diagram <- registry$process_diagrams[[process_id]]
  if (!is.null(diagram) && nrow(diagram$outcomes) > 0) {
    return(subs[subs$subprocess %in% unique(diagram$outcomes$subprocess), ])
  }
  if (is.null(diagram) || nrow(diagram$edges) == 0) {
    return(subs)
  }
  sources <- unique(diagram$edges$from_subprocess)
  subs[!subs$subprocess %in% sources, ]
}

#' Business-level outcome distribution for one process
#'
#' A business process's overall outcome is the one reached by whichever
#' subprocess(es) sit at the end of its declared flow (see
#' [pv_final_subprocesses()]) -- pools each final subprocess's own
#' sessions (tagged by subprocess so same-named session ids from
#' different apps can't collide) and their own declared outcomes, then
#' reuses [pv_outcome_distribution()] unchanged.
#'
#' There is deliberately no attempt here to link a case across more than
#' one subprocess/app into a single cross-app case: this log shape
#' carries only a per-app `session_id`, no shared entity/case key, so
#' each final app's own sessions are counted as independent cases. If a
#' real cross-app case identity becomes available, this is the function
#' to extend.
#'
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a `data.frame`, same shape as [pv_outcome_distribution()]'s.
#'   Zero rows if the process has no final subprocess with both a log and
#'   a declared outcome.
#' @export
pv_process_outcome_distribution <- function(registry, process_id) {
  empty <- data.frame(
    outcome_id = character(0), label = character(0), kind = character(0),
    tier = character(0), n_cases = integer(0), pct = numeric(0)
  )
  finals <- pv_final_subprocesses(registry, process_id)
  if (nrow(finals) == 0) {
    return(empty)
  }

  all_traces <- list()
  all_outcomes <- list()
  for (i in seq_len(nrow(finals))) {
    key <- paste(process_id, finals$subprocess[i], sep = "::")
    diagram <- registry$subprocess_diagrams[[key]]
    if (is.null(diagram) || nrow(diagram$outcomes) == 0) next
    log <- pv_pull_subprocess_log(registry, process_id, finals$subprocess[i])
    seqs <- pv_session_step_sequences(log)
    if (length(seqs) == 0) next
    names(seqs) <- paste(finals$subprocess[i], names(seqs), sep = "::")
    all_traces <- c(all_traces, seqs)
    all_outcomes[[length(all_outcomes) + 1]] <- diagram$outcomes
  }

  outcomes <- do.call(rbind, c(all_outcomes, list(NULL)))
  if (is.null(outcomes) || nrow(outcomes) == 0 || length(all_traces) == 0) {
    return(empty)
  }
  pv_outcome_distribution(all_traces, outcomes, "step_id")
}

#' Business process completion rate
#'
#' A thin reshaping of [pv_process_outcome_distribution()] into the
#' single number the portfolio overview wants: what share of this
#' process's cases (as seen at its final subprocess(es)) have reached
#' *any* named outcome yet, regardless of which one.
#'
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a list: `n_cases` (total distinct sessions seen at the final
#'   subprocess(es)), `n_completed` (reached at least one outcome), `pct`
#'   (`NA` if `n_cases` is 0).
#' @export
pv_process_completion_rate <- function(registry, process_id) {
  dist <- pv_process_outcome_distribution(registry, process_id)
  if (nrow(dist) == 0) {
    return(list(n_cases = 0L, n_completed = 0L, pct = NA_real_))
  }
  n_total <- sum(dist$n_cases)
  n_incomplete <- sum(dist$n_cases[dist$outcome_id == "incomplete"])
  n_completed <- n_total - n_incomplete
  list(
    n_cases = n_total, n_completed = n_completed,
    pct = if (n_total == 0) NA_real_ else round(100 * n_completed / n_total, 1)
  )
}

#' Portfolio-wide completion rate
#'
#' Cases-weighted across every instrumented process -- summing each
#' process's own case/completion counts before dividing, rather than
#' averaging per-process percentages, so a process with many cases isn't
#' drowned out by one with few.
#'
#' @param registry a [pv_load_registry()] result.
#' @return a list: `n_cases`, `n_completed`, `pct` (`NA` if no
#'   instrumented process has any cases yet).
#' @export
pv_portfolio_completion <- function(registry) {
  instrumented_ids <- registry$processes$process_id[registry$processes$status == "instrumented"]
  rates <- lapply(instrumented_ids, pv_process_completion_rate, registry = registry)

  n_cases <- sum(vapply(rates, `[[`, numeric(1), "n_cases"))
  n_completed <- sum(vapply(rates, `[[`, numeric(1), "n_completed"))
  list(
    n_cases = n_cases, n_completed = n_completed,
    pct = if (n_cases == 0) NA_real_ else round(100 * n_completed / n_cases, 1)
  )
}

#' Join externally-tracked real-world results onto mined per-case outcomes
#'
#' proviz doesn't own or connect to whatever system tracks a decision's
#' eventual real-world result -- each outcome's `outcome_tracking`
#' pointer in the registry just documents where that lives. This is the
#' join point: export a plain table from that system, keyed the same
#' way, and get back the mined case data with those results attached.
#'
#' @param case_outcomes a `data.frame` with a join column (default
#'   `"case_id"`) -- e.g. [pv_case_outcome_table()]'s result.
#' @param external_results a `data.frame` from whatever system tracks the
#'   real-world result, with a matching join column plus at least one
#'   metric column.
#' @param by join column name, present in both data frames.
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
