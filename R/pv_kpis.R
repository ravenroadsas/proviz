#' Per-session throughput time (elapsed time to the next-to-last logged
#' step), in minutes
#'
#' A thin wrapper around `edeaR::throughput_time()` that returns plain
#' numeric minutes -- this is elapsed time across an *eventlog*
#' (single-instant actions), not a measured task duration, and its
#' **mean** is fragile for a long-tail session; prefer the median as the
#' headline KPI.
#' @param log a [pv_pull_subprocess_log()] result.
#' @return a `data.frame`: `case_id` (the `session_id`), `duration_mins`.
#' @export
pv_case_durations <- function(log) {
  if (nrow(log) == 0) {
    return(data.frame(case_id = character(0), duration_mins = numeric(0)))
  }
  tt <- edeaR::throughput_time(pv_build_eventlog(log), level = "case", units = "mins")
  data.frame(case_id = tt$session_id, duration_mins = as.numeric(tt$throughput_time), stringsAsFactors = FALSE)
}

#' Core KPI set for one subprocess's (already filtered) log
#'
#' @param log a [pv_pull_subprocess_log()] result (optionally filtered,
#'   see [pv_filter_log()]).
#' @param diagram a `$subprocess_diagrams[[key]]` entry (or `NULL` to
#'   skip the conformance KPI, e.g. a subprocess with no declared steps
#'   yet).
#' @return a list: `n_cases`, `n_users`, `median_duration_mins`,
#'   `mean_duration_mins`, `conformance_median` (`NA` if `diagram` is
#'   `NULL` or there are no cases).
#' @export
pv_process_kpis <- function(log, diagram = NULL) {
  if (nrow(log) == 0) {
    return(list(
      n_cases = 0L, n_users = 0L, median_duration_mins = NA_real_,
      mean_duration_mins = NA_real_, conformance_median = NA_real_
    ))
  }

  durations <- pv_case_durations(log)$duration_mins
  out <- list(
    n_cases = length(unique(log$session_id)),
    n_users = length(unique(log$user_id)),
    median_duration_mins = stats::median(durations, na.rm = TRUE),
    mean_duration_mins = mean(durations, na.rm = TRUE),
    conformance_median = NA_real_
  )
  if (!is.null(diagram)) {
    sequences <- pv_session_step_sequences(log)
    conf <- pv_diagram_conformance(sequences, diagram$edges, "from_step", "to_step")
    out$conformance_median <- stats::median(conf$score, na.rm = TRUE)
  }
  out
}

#' Sessions started per calendar week
#' @param log a [pv_pull_subprocess_log()] result.
#' @return a `data.frame`, ordered by week: `week_start` (`Date`), `n_cases`.
#' @export
pv_weekly_usage <- function(log) {
  if (nrow(log) == 0) {
    return(data.frame(week_start = as.Date(character(0)), n_cases = integer(0)))
  }
  session_start <- stats::aggregate(started_at ~ session_id, log, min)
  session_start$week_start <- lubridate::floor_date(as.Date(session_start$started_at), unit = "week")
  agg <- stats::aggregate(session_id ~ week_start, session_start, length)
  names(agg)[names(agg) == "session_id"] <- "n_cases"
  agg[order(agg$week_start), ]
}

#' Per-user workload, for the resource-breakdown panel
#'
#' There is no role column in the real event log (see
#' [pv_pull_subprocess_log()]'s fixed shape), so this is by user only --
#' a role-based breakdown isn't computable from this log shape.
#' @param log a [pv_pull_subprocess_log()] result.
#' @return a `data.frame`, most-involved first: `user_id`, `n_cases`
#'   (from `edeaR::resource_involvement()`), `pct`.
#' @export
pv_resource_breakdown <- function(log) {
  if (nrow(log) == 0) {
    return(data.frame(user_id = character(0), n_cases = integer(0), pct = numeric(0)))
  }
  ri <- as.data.frame(edeaR::resource_involvement(pv_build_eventlog(log), level = "resource"))
  ri <- ri[order(-ri$absolute), ]
  data.frame(
    user_id = ri$user_id, n_cases = ri$absolute,
    pct = round(100 * ri$relative, 1), stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Variant (step-sequence) frequency table, with conformance per variant
#'
#' Groups sessions by their exact step sequence and reports how common
#' each is and how well it conforms to the declared diagram -- the view
#' that shows *which* deviations are common enough to matter,
#' complementing the single conformance-score summary.
#' @param log a [pv_pull_subprocess_log()] result.
#' @param diagram a `$subprocess_diagrams[[key]]` entry.
#' @return a `data.frame`, most frequent first: `trace` (step ids joined
#'   by `" -> "`), `n_cases`, `pct`, `mean_conformance`.
#' @export
pv_variant_table <- function(log, diagram) {
  if (nrow(log) == 0) {
    return(data.frame(trace = character(0), n_cases = integer(0), pct = numeric(0), mean_conformance = numeric(0)))
  }

  sequences <- pv_session_step_sequences(log)
  trace_strs <- vapply(sequences, paste, collapse = " -> ", FUN.VALUE = character(1))
  conf <- pv_diagram_conformance(sequences, diagram$edges, "from_step", "to_step")
  conf_by_case <- stats::setNames(conf$score, conf$case_id)

  case_df <- data.frame(case_id = names(sequences), trace = trace_strs, stringsAsFactors = FALSE)
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

#' Apps monitored, per process and portfolio-wide
#'
#' An app counts as monitored if it has a `log_path` declared (not
#' necessarily reachable right now -- consistent with how [pv_coverage()]
#' already treats `status` as a declared signal, not a live-checked one).
#'
#' @param registry a [pv_load_registry()] result.
#' @return a list: `by_process` (a `data.frame`: `process_id`, `n_apps`,
#'   `n_apps_monitored`, `pct`), `portfolio` (a list: `n_apps`,
#'   `n_apps_monitored`, `pct`, totalled across every non-`deprecated`
#'   process).
#' @export
pv_app_coverage <- function(registry) {
  subs <- registry$subprocesses
  empty_by_process <- data.frame(
    process_id = character(0), n_apps = integer(0), n_apps_monitored = integer(0), pct = numeric(0)
  )

  if (nrow(subs) == 0) {
    return(list(by_process = empty_by_process, portfolio = list(n_apps = 0L, n_apps_monitored = 0L, pct = NA_real_)))
  }

  subs$monitored <- !is.na(subs$log_path)
  by_process <- do.call(rbind, lapply(split(subs, subs$process_id), function(g) {
    data.frame(
      process_id = g$process_id[1], n_apps = nrow(g), n_apps_monitored = sum(g$monitored),
      stringsAsFactors = FALSE
    )
  }))
  rownames(by_process) <- NULL
  by_process$pct <- round(100 * by_process$n_apps_monitored / by_process$n_apps, 1)

  active_ids <- registry$processes$process_id[registry$processes$status != "deprecated"]
  active_subs <- subs[subs$process_id %in% active_ids, ]
  portfolio <- list(
    n_apps = nrow(active_subs),
    n_apps_monitored = sum(active_subs$monitored),
    pct = if (nrow(active_subs) == 0) NA_real_ else round(100 * sum(active_subs$monitored) / nrow(active_subs), 1)
  )

  list(by_process = by_process, portfolio = portfolio)
}

#' Distinct users active in the trailing week
#'
#' Anchored to the most recent session in `log` (not `Sys.time()`), so
#' this stays meaningful against historical/demo data too.
#' @param log a [pv_pull_subprocess_log()] result.
#' @return a single integer.
#' @export
pv_weekly_active_users <- function(log) {
  if (nrow(log) == 0) {
    return(0L)
  }
  cutoff <- max(log$started_at) - as.difftime(7, units = "days")
  length(unique(log$user_id[log$started_at >= cutoff]))
}

#' Portfolio-wide KPIs, across every instrumented business process
#'
#' @param registry a [pv_load_registry()] result.
#' @param start_date,end_date,users optional global-filter passthrough,
#'   see [pv_pull_subprocess_log()].
#' @return a list: `n_processes`, `n_instrumented`, `n_apps`,
#'   `n_apps_monitored`, `apps_pct`, `n_cases`, `n_users`,
#'   `n_weekly_users`, `completion_pct` (see [pv_portfolio_completion()]).
#' @export
pv_portfolio_kpis <- function(registry, start_date = NULL, end_date = NULL, users = NULL) {
  all_log <- pv_pull_portfolio_log(registry, start_date = start_date, end_date = end_date, users = users)

  coverage <- pv_coverage(registry)
  app_cov <- pv_app_coverage(registry)$portfolio
  completion <- pv_portfolio_completion(registry, start_date = start_date, end_date = end_date, users = users)

  list(
    n_processes = coverage$total, n_instrumented = coverage$instrumented,
    n_apps = app_cov$n_apps, n_apps_monitored = app_cov$n_apps_monitored, apps_pct = app_cov$pct,
    n_cases = length(unique(all_log$session_id)), n_users = length(unique(all_log$user_id)),
    n_weekly_users = pv_weekly_active_users(all_log),
    completion_pct = completion$pct
  )
}

#' User activity heatmap across the whole process portfolio
#'
#' The data behind the users tab: for every instrumented process, how
#' many distinct sessions did each user touch -- a process x user
#' matrix, long-form, ready to hand to a heatmap.
#'
#' @param registry a [pv_load_registry()] result.
#' @param start_date,end_date,users optional global-filter passthrough,
#'   see [pv_pull_subprocess_log()].
#' @return a `data.frame`, long-form: `process_id`, `process` (label),
#'   `user_id`, `n_cases`. Zero rows if no instrumented process has any
#'   log data yet.
#' @export
pv_portfolio_heatmap <- function(registry, start_date = NULL, end_date = NULL, users = NULL) {
  empty <- data.frame(process_id = character(0), process = character(0), user_id = character(0), n_cases = integer(0))

  instrumented_ids <- registry$processes$process_id[registry$processes$status == "instrumented"]
  if (length(instrumented_ids) == 0) {
    return(empty)
  }

  rows <- lapply(instrumented_ids, function(pid) {
    log <- pv_pull_process_log(registry, pid, start_date = start_date, end_date = end_date, users = users)
    if (nrow(log) == 0) {
      return(NULL)
    }
    distinct <- unique(log[, c("session_id", "user_id")])
    tab <- table(distinct$user_id)
    if (length(tab) == 0) {
      return(NULL)
    }
    data.frame(process_id = pid, user_id = names(tab), n_cases = as.integer(tab), stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, c(rows, list(NULL)))
  if (is.null(out) || nrow(out) == 0) {
    return(empty)
  }

  proc_labels <- stats::setNames(registry$processes$process, registry$processes$process_id)
  out$process <- unname(proc_labels[out$process_id])
  rownames(out) <- NULL
  out[, c("process_id", "process", "user_id", "n_cases")]
}

#' User activity heatmap at app granularity, within a chosen set of
#' business processes
#'
#' The "break down by app" mode behind the users heatmap --
#' [pv_portfolio_heatmap()]'s one-column-per-process view, one level
#' finer: a column per monitored app instead.
#'
#' @param registry a [pv_load_registry()] result.
#' @param process_ids character vector of `process_id`s to scope to, or
#'   `NULL` (default) for every monitored app across the whole portfolio.
#' @param start_date,end_date,users optional global-filter passthrough,
#'   see [pv_pull_subprocess_log()].
#' @return a `data.frame`, long-form: `process_id`, `process` (label),
#'   `app`, `subprocess`, `user_id`, `n_cases`. Zero rows if none of the
#'   scoped apps has a `log_path` or any log data yet.
#' @export
pv_app_heatmap <- function(registry, process_ids = NULL, start_date = NULL, end_date = NULL, users = NULL) {
  empty <- data.frame(
    process_id = character(0), process = character(0), app = character(0),
    subprocess = character(0), user_id = character(0), n_cases = integer(0)
  )

  subs <- registry$subprocesses[!is.na(registry$subprocesses$log_path), ]
  if (!is.null(process_ids)) {
    subs <- subs[subs$process_id %in% process_ids, ]
  }
  if (nrow(subs) == 0) {
    return(empty)
  }

  rows <- lapply(seq_len(nrow(subs)), function(i) {
    log <- pv_pull_subprocess_log(registry, subs$process_id[i], subs$subprocess[i], start_date = start_date, end_date = end_date, users = users)
    if (nrow(log) == 0) {
      return(NULL)
    }
    distinct <- unique(log[, c("session_id", "user_id")])
    tab <- table(distinct$user_id)
    if (length(tab) == 0) {
      return(NULL)
    }
    data.frame(
      process_id = subs$process_id[i], app = subs$app[i], subprocess = subs$subprocess[i],
      user_id = names(tab), n_cases = as.integer(tab), stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, c(rows, list(NULL)))
  if (is.null(out) || nrow(out) == 0) {
    return(empty)
  }

  proc_labels <- stats::setNames(registry$processes$process, registry$processes$process_id)
  out$process <- unname(proc_labels[out$process_id])
  rownames(out) <- NULL
  out[, c("process_id", "process", "app", "subprocess", "user_id", "n_cases")]
}

#' KPI grid for every instrumented business process in the registry
#'
#' The portfolio overview tab's main table: one row per registered
#' business process, joined with live KPIs for the ones that are
#' actually instrumented. `conformance_median` is each process's own
#' subprocesses' step-level conformance medians, averaged -- not a
#' cross-app business-flow check (this log shape carries no cross-app
#' case identity to check that against; see
#' [pv_process_outcome_distribution()]'s docs for the same limitation).
#'
#' @param registry a [pv_load_registry()] result.
#' @param start_date,end_date,users optional global-filter passthrough,
#'   see [pv_pull_subprocess_log()].
#' @return `registry$processes` with added columns: `n_apps`,
#'   `n_apps_monitored`, `apps_pct`, `n_cases`, `n_users`,
#'   `median_duration_mins`, `conformance_median`, `completion_pct`.
#' @export
pv_registry_kpis <- function(registry, start_date = NULL, end_date = NULL, users = NULL) {
  instrumented <- registry$processes[registry$processes$status == "instrumented", ]
  app_cov <- pv_app_coverage(registry)$by_process

  kpi_rows <- lapply(instrumented$process_id, function(pid) {
    log <- pv_pull_process_log(registry, pid, start_date = start_date, end_date = end_date, users = users)
    subs <- registry$subprocesses[registry$subprocesses$process_id == pid, ]
    conf_medians <- vapply(subs$subprocess, function(sp) {
      conf <- pv_subprocess_conformance(registry, pid, sp, start_date = start_date, end_date = end_date, users = users)
      if (nrow(conf) == 0) NA_real_ else stats::median(conf$score, na.rm = TRUE)
    }, numeric(1))
    completion <- pv_process_completion_rate(registry, pid, start_date = start_date, end_date = end_date, users = users)
    data.frame(
      process_id = pid,
      n_cases = if (nrow(log) == 0) 0L else length(unique(log$session_id)),
      n_users = if (nrow(log) == 0) 0L else length(unique(log$user_id)),
      median_duration_mins = if (nrow(log) == 0) NA_real_ else stats::median(pv_case_durations(log)$duration_mins, na.rm = TRUE),
      conformance_median = if (length(conf_medians) == 0 || all(is.na(conf_medians))) NA_real_ else mean(conf_medians, na.rm = TRUE),
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
  out <- merge(out, app_cov[, c("process_id", "n_apps", "n_apps_monitored", "pct")], by = "process_id", all.x = TRUE)
  names(out)[names(out) == "pct"] <- "apps_pct"
  out
}
