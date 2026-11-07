#' A defensively-computed Pearson correlation
#'
#' `NA` whenever the correlation would be undefined or not meaningfully
#' estimable -- fewer than `min_n` complete pairs, or either side
#' constant (zero variance, which makes `stats::cor()` return `NA` with a
#' warning anyway; this just skips the warning).
#' @noRd
pv_safe_cor <- function(x, y, min_n = 3) {
  ok <- stats::complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < min_n || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }
  round(stats::cor(x, y), 2)
}

#' Correlation between engagement score and time, per user or app-wide
#'
#' Buckets `total_weight` into weeks and correlates the week's ordinal
#' position against that week's total score -- a positive `r` means
#' engagement is trending up over the observed period, negative means
#' it's tailing off.
#'
#' @param log a [pv_pull_subprocess_log()] result.
#' @param by `"user"` (default, one row per user) or `"app"` (a single
#'   pooled value across every user).
#' @return for `by = "user"`: a `data.frame`, most-observed first:
#'   `user_id`, `r` (`NA` if fewer than 3 distinct weeks), `n_weeks`.
#'   For `by = "app"`: a list `list(r, n_weeks)`.
#' @export
pv_score_time_trend <- function(log, by = c("user", "app")) {
  by <- match.arg(by)
  if (nrow(log) == 0) {
    return(if (by == "app") list(r = NA_real_, n_weeks = 0L) else data.frame(user_id = character(0), r = numeric(0), n_weeks = integer(0)))
  }

  weeks <- lubridate::floor_date(as.Date(log$started_at), unit = "week")
  week_index <- as.numeric(weeks - min(weeks)) / 7 + 1

  if (by == "app") {
    weekly <- stats::aggregate(log$total_weight, by = list(week_index), sum)
    return(list(r = pv_safe_cor(weekly$Group.1, weekly$x), n_weeks = nrow(weekly)))
  }

  weekly <- stats::aggregate(
    total_weight ~ user_id + week_index,
    data.frame(user_id = log$user_id, week_index = week_index, total_weight = log$total_weight),
    sum
  )
  rows <- lapply(split(weekly, weekly$user_id), function(g) {
    data.frame(user_id = g$user_id[1], r = pv_safe_cor(g$week_index, g$total_weight), n_weeks = nrow(g), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(-out$n_weeks), ]
}

#' Per-session engagement score, conformance, and primary user -- the
#' shared basis for [pv_score_compliance_correlation()]
#' @noRd
pv_case_score_conformance_table <- function(log, diagram) {
  empty <- data.frame(case_id = character(0), user_id = character(0), total_score = numeric(0), conformance = numeric(0))
  if (nrow(log) == 0) {
    return(empty)
  }

  # A session's "primary user" is whoever logged the most of its rows --
  # a session should in practice have exactly one user, but this stays
  # defensive rather than assuming it.
  primary_user <- stats::aggregate(
    user_id ~ session_id, log,
    function(u) names(sort(table(u), decreasing = TRUE))[1]
  )

  case_score <- stats::aggregate(total_weight ~ session_id, log, sum)
  names(case_score) <- c("session_id", "total_score")

  sequences <- pv_session_step_sequences(log)
  conf <- pv_diagram_conformance(sequences, diagram$edges, "from_step", "to_step")

  out <- merge(primary_user, case_score, by = "session_id")
  out <- merge(out, conf[, c("case_id", "score")], by.x = "session_id", by.y = "case_id")
  names(out)[names(out) == "session_id"] <- "case_id"
  names(out)[names(out) == "score"] <- "conformance"
  out
}

#' Correlation between a session's engagement score and its conformance,
#' per user or app-wide
#'
#' Whether more-engaged (higher-scoring) sessions also tend to follow the
#' declared step diagram more closely -- a business question distinct
#' from either metric alone: a user can click a lot without doing the
#' right things, or conform perfectly with minimal engagement.
#'
#' @param log a [pv_pull_subprocess_log()] result.
#' @param diagram a `$subprocess_diagrams[[key]]` entry.
#' @param by `"user"` (default, one row per user) or `"app"` (a single
#'   pooled value across every session).
#' @return for `by = "user"`: a `data.frame`, most-cases first: `user_id`,
#'   `r` (`NA` if fewer than 3 of that user's cases), `n_cases`. For
#'   `by = "app"`: a list `list(r, n_cases)`.
#' @export
pv_score_compliance_correlation <- function(log, diagram, by = c("user", "app")) {
  by <- match.arg(by)
  tab <- pv_case_score_conformance_table(log, diagram)
  pv_correlate_case_table(tab, by)
}

#' Correlate a (user_id, total_score, conformance) case table, by user or
#' pooled app-wide -- the shared tail end of
#' [pv_score_compliance_correlation()] and [pv_app_score_compliance_correlation()].
#' @noRd
pv_correlate_case_table <- function(tab, by = c("user", "app")) {
  by <- match.arg(by)
  if (by == "app") {
    return(list(r = pv_safe_cor(tab$total_score, tab$conformance), n_cases = nrow(tab)))
  }
  if (nrow(tab) == 0) {
    return(data.frame(user_id = character(0), r = numeric(0), n_cases = integer(0)))
  }
  rows <- lapply(split(tab, tab$user_id), function(g) {
    data.frame(user_id = g$user_id[1], r = pv_safe_cor(g$total_score, g$conformance), n_cases = nrow(g), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(-out$n_cases), ]
}

#' This app's log, unioned across every (process, subprocess) slot it
#' occupies (an app can in principle occupy more than one slot).
#'
#' @param registry a [pv_load_registry()] result.
#' @param app an `app` value from `registry$subprocesses`.
#' @return a [pv_pull_subprocess_log()]-shaped `data.frame` plus
#'   `process_id`/`subprocess`, unioned across every slot. Zero rows if
#'   none has any log data yet.
#' @export
pv_app_log <- function(registry, app) {
  subs <- registry$subprocesses[registry$subprocesses$app == app & !is.na(registry$subprocesses$log_path), ]
  rows <- lapply(seq_len(nrow(subs)), function(i) {
    log <- pv_pull_subprocess_log(registry, subs$process_id[i], subs$subprocess[i])
    if (nrow(log) == 0) {
      return(NULL)
    }
    log$process_id <- subs$process_id[i]
    log$subprocess <- subs$subprocess[i]
    log
  })
  out <- do.call(rbind, c(rows, list(NULL)))
  if (is.null(out)) {
    out <- cbind(pv_empty_log(), process_id = character(0), subprocess = character(0))
  }
  out
}

#' Per-session score and conformance for one app, pooled across every
#' (process, subprocess) slot it occupies
#'
#' @param registry a [pv_load_registry()] result.
#' @param app an `app` value from `registry$subprocesses`.
#' @return a `data.frame`: `case_id`, `user_id` (primary user), `total_score`,
#'   `conformance`. Zero rows if this app has no log data or no declared
#'   step diagram anywhere it's scoped.
#' @export
pv_app_case_score_conformance <- function(registry, app) {
  subs <- registry$subprocesses[registry$subprocesses$app == app & !is.na(registry$subprocesses$log_path), ]
  tabs <- lapply(seq_len(nrow(subs)), function(i) {
    log <- pv_pull_subprocess_log(registry, subs$process_id[i], subs$subprocess[i])
    diagram <- registry$subprocess_diagrams[[paste(subs$process_id[i], subs$subprocess[i], sep = "::")]]
    if (nrow(log) == 0 || is.null(diagram)) {
      return(NULL)
    }
    pv_case_score_conformance_table(log, diagram)
  })
  out <- do.call(rbind, c(tabs, list(NULL)))
  if (is.null(out)) {
    out <- data.frame(case_id = character(0), user_id = character(0), total_score = numeric(0), conformance = numeric(0))
  }
  out
}

#' Correlation between a session's engagement score and its conformance,
#' for one app, pooled across every (process, subprocess) slot it occupies
#'
#' The app-centric counterpart to [pv_score_compliance_correlation()] --
#' built for the People tab's app picker, where the caller only has an
#' `app` name, not a single `log`/`diagram` pair to hand in.
#'
#' @param registry a [pv_load_registry()] result.
#' @param app an `app` value from `registry$subprocesses`.
#' @param by `"user"` (default) or `"app"` (see [pv_score_compliance_correlation()]).
#' @return same shape as [pv_score_compliance_correlation()]'s.
#' @export
pv_app_score_compliance_correlation <- function(registry, app, by = c("user", "app")) {
  by <- match.arg(by)
  pv_correlate_case_table(pv_app_case_score_conformance(registry, app), by)
}

#' Correlation between adoption (IGA) and overall step conformance,
#' across apps
#'
#' One point per app: its IGA against its mean session conformance
#' (against that app's own declared step diagram) -- do better-adopted
#' apps also see their steps followed more faithfully? Needs at least 3
#' apps with both an IGA and a conformance figure to return a meaningful
#' `r`.
#'
#' @param registry a [pv_load_registry()] result.
#' @param adoption a [pv_load_adoption()] result.
#' @return a list: `data` (a `data.frame`: `app`, `iga`, `mean_conformance`
#'   -- the scatter's points, apps with either value missing excluded),
#'   `r` (`NA` if fewer than 3 points).
#' @export
pv_iga_compliance_correlation <- function(registry, adoption) {
  iga_tab <- pv_portfolio_iga(registry, adoption)
  subs <- registry$subprocesses[!is.na(registry$subprocesses$log_path), ]

  rows <- lapply(unique(subs$app), function(app) {
    app_subs <- subs[subs$app == app, ]
    confs <- do.call(rbind, c(
      lapply(seq_len(nrow(app_subs)), function(i) {
        pv_subprocess_conformance(registry, app_subs$process_id[i], app_subs$subprocess[i])
      }),
      list(NULL)
    ))
    if (is.null(confs) || nrow(confs) == 0) {
      return(NULL)
    }
    data.frame(app = app, mean_conformance = mean(confs$score, na.rm = TRUE), stringsAsFactors = FALSE)
  })
  conf_tab <- do.call(rbind, c(rows, list(NULL)))

  if (is.null(conf_tab) || nrow(conf_tab) == 0) {
    return(list(data = data.frame(app = character(0), iga = numeric(0), mean_conformance = numeric(0)), r = NA_real_))
  }

  merged <- merge(iga_tab[, c("app", "iga")], conf_tab, by = "app")
  merged <- merged[stats::complete.cases(merged), ]
  list(data = merged, r = pv_safe_cor(merged$iga, merged$mean_conformance))
}
