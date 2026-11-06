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
#' Buckets scores into weeks and correlates the week's ordinal position
#' against that week's total score -- a positive `r` means engagement is
#' trending up over the observed period, negative means it's tailing off.
#'
#' @param scored_events a [pv_score_events()] result (or any events
#'   `data.frame` with `timestamp`, `user_id`, `score` columns).
#' @param by `"user"` (default, one row per user) or `"app"` (a single
#'   pooled value across every user).
#' @return for `by = "user"`: a `data.frame`, most-observed first:
#'   `user_id`, `r` (`NA` if fewer than 3 distinct weeks), `n_weeks`.
#'   For `by = "app"`: a list `list(r, n_weeks)`.
#' @export
pv_score_time_trend <- function(scored_events, by = c("user", "app")) {
  by <- match.arg(by)
  if (nrow(scored_events) == 0) {
    return(if (by == "app") list(r = NA_real_, n_weeks = 0L) else data.frame(user_id = character(0), r = numeric(0), n_weeks = integer(0)))
  }

  weeks <- lubridate::floor_date(as.Date(scored_events$timestamp), unit = "week")
  week_index <- as.numeric(weeks - min(weeks)) / 7 + 1

  if (by == "app") {
    weekly <- stats::aggregate(scored_events$score, by = list(week_index), sum)
    return(list(r = pv_safe_cor(weekly$Group.1, weekly$x), n_weeks = nrow(weekly)))
  }

  weekly <- stats::aggregate(
    score ~ user_id + week_index,
    data.frame(user_id = scored_events$user_id, week_index = week_index, score = scored_events$score),
    sum
  )
  rows <- lapply(split(weekly, weekly$user_id), function(g) {
    data.frame(user_id = g$user_id[1], r = pv_safe_cor(g$week_index, g$score), n_weeks = nrow(g), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(-out$n_weeks), ]
}

#' Per-case engagement score, conformance, and primary user -- the shared
#' basis for [pv_score_compliance_correlation()]
#' @noRd
pv_case_score_conformance_table <- function(events, scored_events, ideal_process) {
  empty <- data.frame(case_id = character(0), user_id = character(0), total_score = numeric(0), conformance = numeric(0))
  if (nrow(events) == 0) {
    return(empty)
  }

  # A case's "primary user" is whoever logged the most of its events --
  # a case can span more than one user, but the per-user correlation
  # needs exactly one owner per case to group by.
  primary_user <- stats::aggregate(
    user_id ~ case_id, events,
    function(u) names(sort(table(u), decreasing = TRUE))[1]
  )

  case_score <- stats::aggregate(score ~ case_id, scored_events, sum)
  names(case_score) <- c("case_id", "total_score")

  conf <- pv_process_conformance(events, ideal_process)[, c("case_id", "score")]
  names(conf) <- c("case_id", "conformance")

  out <- merge(primary_user, case_score, by = "case_id")
  out <- merge(out, conf, by = "case_id")
  out
}

#' Correlation between a case's engagement score and its conformance,
#' per user or app-wide
#'
#' Whether more-engaged (higher-scoring) cases also tend to follow the
#' ideal process more closely -- a business question distinct from
#' either metric alone: a user can click a lot without doing the right
#' things, or conform perfectly with minimal engagement.
#'
#' @param events a case-linked events `data.frame` (e.g.
#'   [pv_pull_process_events()]/[pv_pull_app_events()]'s result).
#' @param scored_events the same events, scored (see [pv_score_events()]).
#' @param ideal_process a [pv_load_ideal_process()] result (or
#'   [pv_step_sequence()]'s, for a business-level correlation).
#' @param by `"user"` (default, one row per user) or `"app"` (a single
#'   pooled value across every case).
#' @return for `by = "user"`: a `data.frame`, most-cases first: `user_id`,
#'   `r` (`NA` if fewer than 3 of that user's cases), `n_cases`. For
#'   `by = "app"`: a list `list(r, n_cases)`.
#' @export
pv_score_compliance_correlation <- function(events, scored_events, ideal_process, by = c("user", "app")) {
  by <- match.arg(by)
  tab <- pv_case_score_conformance_table(events, scored_events, ideal_process)
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

#' This app's events, scored against each matching subprocess's own
#' `ideal_process` -- the raw basis for the People tab's per-app score
#' correlations (an app can in principle occupy more than one
#' process/step slot, each with its own activities/score table).
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param app an `app` value from `registry$subprocesses`.
#' @return a case-linked, scored events `data.frame` (adds `score` to
#'   [pv_pull_app_events()]'s shape), unioned across every monitored
#'   (process, subprocess) slot this app occupies. Zero rows if none has
#'   any events yet.
#' @export
pv_app_scored_events <- function(pv_con, registry, app) {
  subs <- registry$subprocesses[registry$subprocesses$app == app & !is.na(registry$subprocesses$duckdb_path), ]
  rows <- lapply(seq_len(nrow(subs)), function(i) {
    events <- pv_pull_app_events(pv_con, registry, subs$process_id[i], subs$subprocess[i])
    if (nrow(events) == 0) {
      return(NULL)
    }
    ideal <- registry$ideal[[paste(subs$process_id[i], subs$subprocess[i], sep = "::")]]
    score_by_activity <- if (is.null(ideal)) NULL else stats::setNames(ideal$activities$score, ideal$activities$id)
    events$score <- if (is.null(score_by_activity)) 0 else unname(score_by_activity[events$activity_id])
    events$score[is.na(events$score)] <- 0
    events
  })
  out <- do.call(rbind, c(rows, list(NULL)))
  if (is.null(out)) {
    out <- data.frame(
      case_id = character(0), entity_id = character(0), activity_id = character(0),
      timestamp = as.POSIXct(character(0)), user_id = character(0), role = character(0),
      session_id = character(0), app = character(0), subprocess = character(0), score = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  out
}

#' Per-case score and conformance for one app, pooled across every
#' (process, subprocess) slot it occupies
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param app an `app` value from `registry$subprocesses`.
#' @return a `data.frame`: `case_id`, `user_id` (primary user), `total_score`,
#'   `conformance`. Zero rows if this app has no events or no declared
#'   `ideal_process` anywhere it's scoped.
#' @export
pv_app_case_score_conformance <- function(pv_con, registry, app) {
  subs <- registry$subprocesses[registry$subprocesses$app == app & !is.na(registry$subprocesses$duckdb_path), ]
  tabs <- lapply(seq_len(nrow(subs)), function(i) {
    events <- pv_pull_app_events(pv_con, registry, subs$process_id[i], subs$subprocess[i])
    ideal <- registry$ideal[[paste(subs$process_id[i], subs$subprocess[i], sep = "::")]]
    if (nrow(events) == 0 || is.null(ideal)) {
      return(NULL)
    }
    score_by_activity <- stats::setNames(ideal$activities$score, ideal$activities$id)
    scored <- events
    scored$score <- unname(score_by_activity[scored$activity_id])
    scored$score[is.na(scored$score)] <- 0
    pv_case_score_conformance_table(events, scored, ideal)
  })
  out <- do.call(rbind, c(tabs, list(NULL)))
  if (is.null(out)) {
    out <- data.frame(case_id = character(0), user_id = character(0), total_score = numeric(0), conformance = numeric(0))
  }
  out
}

#' Correlation between a case's engagement score and its conformance,
#' for one app, pooled across every (process, subprocess) slot it occupies
#'
#' The app-centric counterpart to [pv_score_compliance_correlation()] --
#' built for the People tab's app picker, where the caller only has an
#' `app` name, not a single `events`/`ideal_process` pair to hand in.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param app an `app` value from `registry$subprocesses`.
#' @param by `"user"` (default) or `"app"` (see [pv_score_compliance_correlation()]).
#' @return same shape as [pv_score_compliance_correlation()]'s.
#' @export
pv_app_score_compliance_correlation <- function(pv_con, registry, app, by = c("user", "app")) {
  by <- match.arg(by)
  pv_correlate_case_table(pv_app_case_score_conformance(pv_con, registry, app), by)
}

#' Correlation between adoption (IGA) and overall process conformance,
#' across apps
#'
#' One point per app: its IGA against its mean case conformance (against
#' that app's own `ideal_process`) -- do better-adopted apps also see
#' their process followed more faithfully? Needs at least 3 apps with
#' both an IGA and a conformance figure to return a meaningful `r`.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param adoption a [pv_load_adoption()] result.
#' @return a list: `data` (a `data.frame`: `app`, `iga`, `mean_conformance`
#'   -- the scatter's points, apps with either value missing excluded),
#'   `r` (`NA` if fewer than 3 points).
#' @export
pv_iga_compliance_correlation <- function(pv_con, registry, adoption) {
  iga_tab <- pv_portfolio_iga(pv_con, registry, adoption)
  subs <- registry$subprocesses[!is.na(registry$subprocesses$duckdb_path), ]

  rows <- lapply(unique(subs$app), function(app) {
    app_subs <- subs[subs$app == app, ]
    confs <- do.call(rbind, c(
      lapply(seq_len(nrow(app_subs)), function(i) {
        events <- pv_pull_app_events(pv_con, registry, app_subs$process_id[i], app_subs$subprocess[i])
        ideal <- registry$ideal[[paste(app_subs$process_id[i], app_subs$subprocess[i], sep = "::")]]
        if (nrow(events) == 0 || is.null(ideal)) {
          return(NULL)
        }
        pv_process_conformance(events, ideal)
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
