#' Filter a process's events to a date range and/or a set of roles
#'
#' Filters at the **case** level, not the event level: a case is kept or
#' dropped whole, based on when it started and whether any of its events
#' were performed by one of `roles`. Truncating a case's own events at a
#' date boundary would corrupt every case-level metric that depends on the
#' full trace (conformance, throughput time, the process map) -- so a case
#' that started in range keeps all of its events, even ones that spill
#' past `end_date`.
#'
#' @param events a case-linked events `data.frame` (see
#'   [pv_pull_process_events()]).
#' @param start_date,end_date `Date` (or coercible), or `NULL` for no
#'   bound. Compared against each case's first event.
#' @param roles character vector of roles to keep, or `NULL` for all.
#' @return `events`, filtered.
#' @export
pv_filter_events <- function(events, start_date = NULL, end_date = NULL, roles = NULL) {
  stopifnot(is.data.frame(events))
  if (nrow(events) == 0) {
    return(events)
  }

  case_start <- stats::aggregate(timestamp ~ case_id, events, min)
  keep <- rep(TRUE, nrow(case_start))
  if (!is.null(start_date)) keep <- keep & as.Date(case_start$timestamp) >= as.Date(start_date)
  if (!is.null(end_date)) keep <- keep & as.Date(case_start$timestamp) <= as.Date(end_date)
  keep_ids <- case_start$case_id[keep]

  if (!is.null(roles) && length(roles) > 0) {
    role_ids <- unique(events$case_id[events$role %in% roles])
    keep_ids <- intersect(keep_ids, role_ids)
  }

  events[events$case_id %in% keep_ids, ]
}

#' Per-case throughput time (elapsed time to the next-to-last logged
#' action), in minutes
#'
#' A thin wrapper around `edeaR::throughput_time()` that returns plain
#' numeric minutes -- see the playbook's caveat: this is elapsed time
#' across an *eventlog* (single-instant actions), not a measured task
#' duration, and its **mean** is fragile for a process with a low-frequency
#' long-tail case; prefer the median as the headline KPI.
#' @param events a case-linked events `data.frame`.
#' @return a `data.frame`: `case_id`, `duration_mins`.
#' @export
pv_case_durations <- function(events) {
  if (nrow(events) == 0) {
    return(data.frame(case_id = character(0), duration_mins = numeric(0)))
  }
  tt <- edeaR::throughput_time(pv_build_eventlog(events), level = "case", units = "mins")
  data.frame(case_id = tt$case_id, duration_mins = as.numeric(tt$throughput_time), stringsAsFactors = FALSE)
}

#' Core KPI set for one process (one filtered slice of its events)
#'
#' @param events a case-linked, already-filtered events `data.frame` (see
#'   [pv_filter_events()]).
#' @param ideal_process a [pv_load_ideal_process()] result, or `NULL` to
#'   skip the conformance KPI (e.g. a process with no ideal-process
#'   definition on file yet).
#' @return a list: `n_cases`, `n_users`, `median_duration_mins`,
#'   `mean_duration_mins`, `conformance_median` (`NA` if `ideal_process`
#'   is `NULL` or there are no cases).
#' @export
pv_process_kpis <- function(events, ideal_process = NULL) {
  if (nrow(events) == 0) {
    return(list(
      n_cases = 0L, n_users = 0L, median_duration_mins = NA_real_,
      mean_duration_mins = NA_real_, conformance_median = NA_real_
    ))
  }

  durations <- pv_case_durations(events)$duration_mins
  out <- list(
    n_cases = length(unique(events$case_id)),
    n_users = length(unique(events$user_id)),
    median_duration_mins = stats::median(durations, na.rm = TRUE),
    mean_duration_mins = mean(durations, na.rm = TRUE),
    conformance_median = NA_real_
  )
  if (!is.null(ideal_process)) {
    conf <- pv_process_conformance(events, ideal_process)
    out$conformance_median <- stats::median(conf$score, na.rm = TRUE)
  }
  out
}

#' Cases started per calendar week
#' @param events a case-linked events `data.frame`.
#' @return a `data.frame`, ordered by week: `week_start` (`Date`), `n_cases`.
#' @export
pv_weekly_usage <- function(events) {
  if (nrow(events) == 0) {
    return(data.frame(week_start = as.Date(character(0)), n_cases = integer(0)))
  }
  case_start <- stats::aggregate(timestamp ~ case_id, events, min)
  case_start$week_start <- lubridate::floor_date(as.Date(case_start$timestamp), unit = "week")
  agg <- stats::aggregate(case_id ~ week_start, case_start, length)
  names(agg)[names(agg) == "case_id"] <- "n_cases"
  agg[order(agg$week_start), ]
}

#' Per-user workload and role, for the resource-breakdown panel
#' @param events a case-linked events `data.frame`.
#' @return a `data.frame`, most-involved first: `user_id`, `role` (the
#'   user's most frequent role in this slice), `n_cases` (from
#'   `edeaR::resource_involvement()`), `pct`.
#' @export
pv_resource_breakdown <- function(events) {
  if (nrow(events) == 0) {
    return(data.frame(user_id = character(0), role = character(0), n_cases = integer(0), pct = numeric(0)))
  }
  ri <- as.data.frame(edeaR::resource_involvement(pv_build_eventlog(events), level = "resource"))
  role_lookup <- stats::aggregate(role ~ user_id, events, function(x) names(sort(table(x), decreasing = TRUE))[1])

  out <- merge(ri, role_lookup, by = "user_id", all.x = TRUE)
  out <- out[order(-out$absolute), ]
  data.frame(
    user_id = out$user_id, role = out$role, n_cases = out$absolute,
    pct = round(100 * out$relative, 1), stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Variant (trace) frequency table, with conformance per variant
#'
#' Groups cases by their exact activity sequence (a "variant"/trace) and
#' reports how common each is and how well it conforms to the ideal
#' process -- the view that shows *which* deviations are common enough to
#' matter, complementing the single conformance-score summary.
#' @param events a case-linked events `data.frame`.
#' @param ideal_process a [pv_load_ideal_process()] result.
#' @return a `data.frame`, most frequent first: `trace` (activity ids
#'   joined by `" -> "`), `n_cases`, `pct`, `mean_conformance`.
#' @export
pv_variant_table <- function(events, ideal_process) {
  if (nrow(events) == 0) {
    return(data.frame(trace = character(0), n_cases = integer(0), pct = numeric(0), mean_conformance = numeric(0)))
  }

  ordered <- events[order(events$case_id, events$timestamp), ]
  traces <- split(ordered$activity_id, ordered$case_id)
  trace_strs <- vapply(traces, paste, collapse = " -> ", FUN.VALUE = character(1))

  conf <- pv_process_conformance(events, ideal_process)
  conf_by_case <- stats::setNames(conf$score, conf$case_id)

  case_df <- data.frame(case_id = names(traces), trace = trace_strs, stringsAsFactors = FALSE)
  case_df$conformance <- conf_by_case[case_df$case_id]

  n_by_trace <- table(case_df$trace)
  mean_conf_by_trace <- tapply(case_df$conformance, case_df$trace, mean, na.rm = TRUE)

  out <- data.frame(
    trace = names(n_by_trace), n_cases = as.integer(n_by_trace),
    mean_conformance = as.numeric(mean_conf_by_trace[names(n_by_trace)]),
    stringsAsFactors = FALSE
  )
  out$pct <- round(100 * out$n_cases / sum(out$n_cases), 1)
  out <- out[order(-out$n_cases), c("trace", "n_cases", "pct", "mean_conformance")]
  rownames(out) <- NULL
  out
}

#' Coverage: how many registered processes are actually instrumented
#'
#' Excludes `"deprecated"` processes from the denominator -- a retired
#' process was never going to be instrumented and shouldn't be held
#' against coverage.
#' @param registry a [pv_load_registry()] result.
#' @return a list: `instrumented`, `total`, `pct`.
#' @export
pv_coverage <- function(registry) {
  active <- registry$processes[registry$processes$status != "deprecated", ]
  instrumented <- sum(active$status == "instrumented")
  total <- nrow(active)
  list(instrumented = instrumented, total = total, pct = if (total == 0) NA_real_ else instrumented / total)
}

#' KPI grid for every instrumented process in the registry
#'
#' The overview tab's main table: one row per registered process, joined
#' with live KPIs for the ones that are actually instrumented (`NA` KPI
#' columns for `designed`/`not_started`/`deprecated` rows -- there is
#' nothing to compute yet).
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry the same [pv_load_registry()] result `pv_con` was built
#'   from.
#' @return `registry$processes` with added columns: `n_cases`, `n_users`,
#'   `median_duration_mins`, `conformance_median`.
#' @export
pv_registry_kpis <- function(pv_con, registry) {
  instrumented <- registry$processes[registry$processes$status == "instrumented", ]

  kpi_rows <- lapply(instrumented$process_id, function(pid) {
    events <- pv_pull_process_events(pv_con, registry, pid)
    kpis <- pv_process_kpis(events, registry$ideal[[pid]])
    data.frame(
      process_id = pid, n_cases = kpis$n_cases, n_users = kpis$n_users,
      median_duration_mins = kpis$median_duration_mins,
      conformance_median = kpis$conformance_median, stringsAsFactors = FALSE
    )
  })

  kpis <- if (length(kpi_rows) > 0) {
    do.call(rbind, kpi_rows)
  } else {
    data.frame(
      process_id = character(0), n_cases = integer(0), n_users = integer(0),
      median_duration_mins = numeric(0), conformance_median = numeric(0)
    )
  }

  merge(registry$processes, kpis, by = "process_id", all.x = TRUE)
}
