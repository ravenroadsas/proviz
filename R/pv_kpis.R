#' Filter a process's events to a date range and/or a set of roles/users
#'
#' Filters at the **case** level, not the event level: a case is kept or
#' dropped whole, based on when it started and whether any of its events
#' were performed by one of `roles`/`users`. Truncating a case's own events
#' at a date boundary would corrupt every case-level metric that depends on
#' the full trace (conformance, throughput time, the process map) -- so a
#' case that started in range keeps all of its events, even ones that spill
#' past `end_date`.
#'
#' @param events a case-linked events `data.frame` (see
#'   [pv_pull_process_events()]).
#' @param start_date,end_date `Date` (or coercible), or `NULL` for no
#'   bound. Compared against each case's first event.
#' @param roles character vector of roles to keep, or `NULL` for all.
#' @param users character vector of `user_id`s to keep, or `NULL` for all
#'   -- the by-user half of the by-user/by-role analysis the three
#'   detail tabs (and the users-and-roles heatmap) all share.
#' @return `events`, filtered.
#' @export
pv_filter_events <- function(events, start_date = NULL, end_date = NULL, roles = NULL, users = NULL) {
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
  if (!is.null(users) && length(users) > 0) {
    user_ids <- unique(events$case_id[events$user_id %in% users])
    keep_ids <- intersect(keep_ids, user_ids)
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

#' Steps monitored, per process and portfolio-wide
#'
#' A step counts as monitored if at least one of its apps has a
#' `duckdb_path` declared (not necessarily live-attached right now --
#' consistent with how [pv_coverage()] already treats `status` as a
#' declared signal, not a live-checked one). A step with more than one app
#' and only some of them instrumented still counts: partial visibility into
#' a step is better than none, and is itself worth seeing (the per-app
#' breakdown on the business-process detail tab shows which app(s) still
#' aren't).
#'
#' @param registry a [pv_load_registry()] result.
#' @return a list: `by_process` (a `data.frame`: `process_id`, `n_steps`,
#'   `n_steps_monitored`, `pct`), `portfolio` (a list: `n_steps`,
#'   `n_steps_monitored`, `pct`, totalled across every non-`deprecated`
#'   process).
#' @export
pv_step_coverage <- function(registry) {
  steps <- registry$steps
  subs <- registry$subprocesses
  empty_by_process <- data.frame(
    process_id = character(0), n_steps = integer(0), n_steps_monitored = integer(0), pct = numeric(0)
  )

  if (nrow(steps) == 0) {
    return(list(by_process = empty_by_process, portfolio = list(n_steps = 0L, n_steps_monitored = 0L, pct = NA_real_)))
  }

  monitored_app <- !is.na(subs$duckdb_path)
  monitored_step_keys <- unique(paste(subs$process_id[monitored_app], subs$step[monitored_app], sep = "::"))
  step_key <- paste(steps$process_id, steps$step, sep = "::")
  steps$monitored <- step_key %in% monitored_step_keys

  by_process <- do.call(rbind, lapply(split(steps, steps$process_id), function(g) {
    data.frame(
      process_id = g$process_id[1], n_steps = nrow(g), n_steps_monitored = sum(g$monitored),
      stringsAsFactors = FALSE
    )
  }))
  rownames(by_process) <- NULL
  by_process$pct <- round(100 * by_process$n_steps_monitored / by_process$n_steps, 1)

  active_ids <- registry$processes$process_id[registry$processes$status != "deprecated"]
  active_steps <- steps[steps$process_id %in% active_ids, ]
  portfolio <- list(
    n_steps = nrow(active_steps),
    n_steps_monitored = sum(active_steps$monitored),
    pct = if (nrow(active_steps) == 0) NA_real_ else round(100 * sum(active_steps$monitored) / nrow(active_steps), 1)
  )

  list(by_process = by_process, portfolio = portfolio)
}

#' Distinct users active in the trailing week
#'
#' "Weekly active users" anchored to the most recent event in `events`
#' (not `Sys.time()`), so this stays meaningful against historical/demo
#' data too, not only a live system queried in real time.
#' @param events a case-linked events `data.frame`.
#' @return a single integer.
#' @export
pv_weekly_active_users <- function(events) {
  if (nrow(events) == 0) {
    return(0L)
  }
  cutoff <- max(events$timestamp) - as.difftime(7, units = "days")
  length(unique(events$user_id[events$timestamp >= cutoff]))
}

#' Portfolio-wide KPIs, across every instrumented business process
#'
#' The headline numbers for the portfolio overview tab: how many processes
#' are registered/instrumented, how much of their declared step map is
#' actually monitored, and usage/completion totals computed over the
#' *union* of every instrumented process's events -- not a sum of each
#' process's own per-process counts, which would double-count a user or
#' case touching more than one process.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry the same [pv_load_registry()] result `pv_con` was built
#'   from.
#' @return a list: `n_processes`, `n_instrumented`, `n_steps`,
#'   `n_steps_monitored`, `steps_pct`, `n_cases`, `n_users`,
#'   `n_weekly_users`, `completion_pct` (see [pv_portfolio_completion()]).
#' @export
pv_portfolio_kpis <- function(pv_con, registry) {
  instrumented_ids <- registry$processes$process_id[registry$processes$status == "instrumented"]
  all_events <- do.call(rbind, c(
    lapply(instrumented_ids, function(pid) pv_pull_process_events(pv_con, registry, pid)),
    list(pv_empty_events())
  ))

  coverage <- pv_coverage(registry)
  step_cov <- pv_step_coverage(registry)$portfolio
  completion <- pv_portfolio_completion(pv_con, registry)

  list(
    n_processes = coverage$total, n_instrumented = coverage$instrumented,
    n_steps = step_cov$n_steps, n_steps_monitored = step_cov$n_steps_monitored, steps_pct = step_cov$pct,
    n_cases = length(unique(all_events$case_id)), n_users = length(unique(all_events$user_id)),
    n_weekly_users = pv_weekly_active_users(all_events),
    completion_pct = completion$pct
  )
}

#' User/role activity heatmap across the whole process portfolio
#'
#' The data behind the hybrid users-and-roles tab: for every instrumented
#' process, how many distinct cases did each user (or role) touch -- a
#' process x user/role matrix, long-form, ready to hand to a heatmap. Cases
#' are counted distinctly per user/role per process (not raw event counts),
#' so an app that happens to log many small events per case doesn't make
#' its users look busier than they are.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry the same [pv_load_registry()] result `pv_con` was built
#'   from.
#' @param by `"user"` (default) or `"role"` -- which dimension to
#'   cross-tabulate against process.
#' @return a `data.frame`, long-form: `process_id`, `process` (label),
#'   `user_id` or `role` (named for `by`), `n_cases`. Zero rows if no
#'   instrumented process has any events yet.
#' @export
pv_portfolio_heatmap <- function(pv_con, registry, by = c("user", "role")) {
  by <- match.arg(by)
  dim_col <- if (by == "user") "user_id" else "role"
  empty <- data.frame(process_id = character(0), process = character(0), n_cases = integer(0))
  empty[[dim_col]] <- character(0)

  instrumented_ids <- registry$processes$process_id[registry$processes$status == "instrumented"]
  if (length(instrumented_ids) == 0) {
    return(empty[, c("process_id", "process", dim_col, "n_cases")])
  }

  rows <- lapply(instrumented_ids, function(pid) {
    events <- pv_pull_process_events(pv_con, registry, pid)
    if (nrow(events) == 0) {
      return(NULL)
    }
    distinct <- unique(events[, c("case_id", dim_col)])
    tab <- table(distinct[[dim_col]])
    if (length(tab) == 0) {
      return(NULL)
    }
    out <- data.frame(process_id = pid, n_cases = as.integer(tab), stringsAsFactors = FALSE)
    out[[dim_col]] <- names(tab)
    out
  })

  out <- do.call(rbind, c(rows, list(NULL)))
  if (is.null(out) || nrow(out) == 0) {
    return(empty[, c("process_id", "process", dim_col, "n_cases")])
  }

  proc_labels <- stats::setNames(registry$processes$process, registry$processes$process_id)
  out$process <- unname(proc_labels[out$process_id])
  rownames(out) <- NULL
  out[, c("process_id", "process", dim_col, "n_cases")]
}

#' KPI grid for every instrumented business process in the registry
#'
#' The portfolio overview tab's main table: one row per registered
#' business process, joined with live business-level KPIs for the ones
#' that are actually instrumented (`NA` KPI columns for
#' `designed`/`not_started`/`deprecated` rows -- there is nothing to
#' compute yet). `conformance_median` is business-level (step-order)
#' conformance, from [pv_business_process_conformance()] -- not any one
#' app's own activity-level conformance, which belongs on the app-detail
#' tab instead.
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry the same [pv_load_registry()] result `pv_con` was built
#'   from.
#' @return `registry$processes` with added columns: `n_steps`,
#'   `n_steps_monitored`, `steps_pct`, `n_cases`, `n_users`,
#'   `median_duration_mins`, `conformance_median`, `completion_pct`.
#' @export
pv_registry_kpis <- function(pv_con, registry) {
  instrumented <- registry$processes[registry$processes$status == "instrumented", ]
  step_cov <- pv_step_coverage(registry)$by_process

  kpi_rows <- lapply(instrumented$process_id, function(pid) {
    events <- pv_pull_business_events(pv_con, registry, pid)
    kpis <- pv_process_kpis(events, pv_step_sequence(registry, pid))
    completion <- pv_process_completion_rate(pv_con, registry, pid)
    data.frame(
      process_id = pid, n_cases = kpis$n_cases, n_users = kpis$n_users,
      median_duration_mins = kpis$median_duration_mins,
      conformance_median = kpis$conformance_median,
      completion_pct = completion$pct, stringsAsFactors = FALSE
    )
  })

  kpis <- if (length(kpi_rows) > 0) {
    do.call(rbind, kpi_rows)
  } else {
    data.frame(
      process_id = character(0), n_cases = integer(0), n_users = integer(0),
      median_duration_mins = numeric(0), conformance_median = numeric(0), completion_pct = numeric(0)
    )
  }

  out <- merge(registry$processes, kpis, by = "process_id", all.x = TRUE)
  out <- merge(out, step_cov[, c("process_id", "n_steps", "n_steps_monitored", "pct")], by = "process_id", all.x = TRUE)
  names(out)[names(out) == "pct"] <- "steps_pct"
  out
}
