#' Load yearly training/satisfaction adoption data from the admin's workbook
#'
#' One sheet per app (see [pv_load_registry()]'s `adoption_source` field,
#' typically passed here as `path`): a row per identified user with their
#' `user_id` and whether they were `trained` (`TRUE`/`FALSE`), plus a
#' `satisfaction` column holding one app-wide value -- the same number can
#' be repeated down every row or present just once; either way the first
#' non-NA value found is used (more than one *distinct* value is a
#' warning, not an error, since this is an externally-authored
#' spreadsheet, not request input).
#'
#' Sheet names are matched to apps case-insensitively. An app with no
#' matching sheet, or a sheet missing `user_id`/`trained`, is skipped with
#' a warning rather than failing the whole load -- one bad sheet shouldn't
#' take down adoption reporting for every other app.
#'
#' @param path path to the `.xlsx` workbook. Typically `registry$adoption_source`.
#' @param registry a [pv_load_registry()] result -- used only to know which
#'   app names to look for among the workbook's sheets.
#' @return a named list, keyed by `app`: each entry a list with `users`
#'   (a `data.frame`: `user_id`, `trained`) and `satisfaction` (a single
#'   numeric value, `NA` if the sheet has no usable `satisfaction`
#'   column). An app with no matching sheet is simply absent (`NULL` via
#'   `[[`).
#' @export
pv_load_adoption <- function(path, registry) {
  stopifnot(is.character(path), length(path) == 1, file.exists(path))
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("proviz: pv_load_adoption() needs the 'readxl' package (Suggests) to read the adoption workbook.", call. = FALSE)
  }

  known_apps <- unique(registry$subprocesses$app)
  sheets <- readxl::excel_sheets(path)
  sheet_by_app <- stats::setNames(rep(NA_character_, length(known_apps)), known_apps)
  for (app in known_apps) {
    hit <- which(tolower(sheets) == tolower(app))
    if (length(hit) > 0) sheet_by_app[app] <- sheets[hit[1]]
  }

  unmatched_apps <- names(sheet_by_app)[is.na(sheet_by_app)]
  if (length(unmatched_apps) > 0) {
    warning("proviz: adoption workbook '", path, "' has no sheet for app(s): ",
      paste(unmatched_apps, collapse = ", "), call. = FALSE
    )
  }
  unmatched_sheets <- setdiff(sheets, stats::na.omit(sheet_by_app))
  if (length(unmatched_sheets) > 0) {
    warning("proviz: adoption workbook '", path, "' has sheet(s) matching no known app (check for typos): ",
      paste(unmatched_sheets, collapse = ", "), call. = FALSE
    )
  }

  adoption <- list()
  for (app in names(sheet_by_app)) {
    sheet <- sheet_by_app[[app]]
    if (is.na(sheet)) next

    df <- as.data.frame(readxl::read_excel(path, sheet = sheet))
    names(df) <- tolower(trimws(names(df)))

    if (!all(c("user_id", "trained") %in% names(df))) {
      warning("proviz: adoption sheet '", sheet, "' (app '", app,
        "') is missing a 'user_id' and/or 'trained' column -- skipping",
        call. = FALSE
      )
      next
    }

    users <- data.frame(
      user_id = as.character(df$user_id), trained = as.logical(df$trained),
      stringsAsFactors = FALSE
    )
    users <- users[!is.na(users$user_id) & nzchar(users$user_id), ]

    satisfaction <- NA_real_
    if ("satisfaction" %in% names(df)) {
      vals <- unique(stats::na.omit(as.numeric(df$satisfaction)))
      if (length(vals) > 1) {
        warning("proviz: adoption sheet '", sheet, "' (app '", app,
          "') has more than one distinct satisfaction value -- using the first (", vals[1], ")",
          call. = FALSE
        )
      }
      if (length(vals) > 0) satisfaction <- vals[1]
    }

    adoption[[app]] <- list(users = users, satisfaction = satisfaction)
  }
  adoption
}

#' Attach each event's activity score and business step to a process's events
#'
#' The event-level basis for [pv_score_by_user()]/[pv_score_by_step()]/
#' [pv_score_by_session()] -- each app has its own score-per-activity
#' table (declared on its own `ideal_process` activities, see
#' [pv_load_ideal_process()]), so scoring a cross-app business process's
#' events needs each row looked up against its *own* app's table, not one
#' shared vocabulary.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a case-linked events `data.frame`, same shape as
#'   [pv_pull_process_events()]'s, with two added columns: `score` (the
#'   activity's declared score, `0` if that app declares none, or the
#'   activity isn't found in its table) and `step_label`.
#' @export
pv_score_events <- function(pv_con, registry, process_id) {
  events <- pv_pull_process_events(pv_con, registry, process_id)
  if (nrow(events) == 0) {
    events$score <- numeric(0)
    events$step_label <- character(0)
    return(events)
  }

  subs <- registry$subprocesses[registry$subprocesses$process_id == process_id, ]
  step_label_by_subprocess <- stats::setNames(subs$step_label, subs$subprocess)
  events$step_label <- unname(step_label_by_subprocess[events$subprocess])

  score_lookup <- do.call(rbind, c(
    lapply(unique(events$subprocess), function(sp) {
      ideal <- registry$ideal[[paste(process_id, sp, sep = "::")]]
      if (is.null(ideal)) {
        return(NULL)
      }
      data.frame(subprocess = sp, activity_id = ideal$activities$id, score = ideal$activities$score, stringsAsFactors = FALSE)
    }),
    list(NULL)
  ))

  if (is.null(score_lookup) || nrow(score_lookup) == 0) {
    events$score <- 0
    return(events)
  }

  score_key <- paste(events$subprocess, events$activity_id, sep = "::")
  lookup_key <- paste(score_lookup$subprocess, score_lookup$activity_id, sep = "::")
  events$score <- score_lookup$score[match(score_key, lookup_key)]
  events$score[is.na(events$score)] <- 0
  events
}

#' @noRd
pv_score_by <- function(scored_events, by) {
  empty <- data.frame(total_score = numeric(0), n_events = integer(0))
  empty[[by]] <- character(0)
  if (nrow(scored_events) == 0) {
    return(empty[, c(by, "total_score", "n_events")])
  }

  total <- stats::aggregate(scored_events$score, by = list(scored_events[[by]]), sum)
  n <- stats::aggregate(scored_events$score, by = list(scored_events[[by]]), length)
  names(total) <- c(by, "total_score")
  total$n_events <- n$x
  total <- total[order(-total$total_score), ]
  rownames(total) <- NULL
  total
}

#' Total engagement score per user
#' @param scored_events a [pv_score_events()] result.
#' @return a `data.frame`, highest first: `user_id`, `total_score`, `n_events`.
#' @export
pv_score_by_user <- function(scored_events) pv_score_by(scored_events, "user_id")

#' Total engagement score per business step
#' @param scored_events a [pv_score_events()] result.
#' @return a `data.frame`, highest first: `step_label`, `total_score`, `n_events`.
#' @export
pv_score_by_step <- function(scored_events) pv_score_by(scored_events, "step_label")

#' Total engagement score per logged session
#' @param scored_events a [pv_score_events()] result.
#' @return a `data.frame`, highest first: `session_id`, `total_score`, `n_events`.
#' @export
pv_score_by_session <- function(scored_events) pv_score_by(scored_events, "session_id")

#' Each identified user's average weekly engagement score for one app
#'
#' Pulls every (process, step) slot this app occupies (an app name can in
#' principle appear in more than one business process) and unions their
#' scored events -- "average weekly score" is computed only over weeks
#' the user was actually active (total score that week), not diluted by
#' entirely inactive weeks, so it reads as "how much do they do in a
#' typical week they show up", the basis [pv_app_iga()]'s usage component
#' needs.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param app an `app` value from `registry$subprocesses`.
#' @return a `data.frame`: `user_id`, `avg_weekly_score`. Zero rows if this
#'   app has no events yet.
#' @export
pv_app_weekly_user_scores <- function(pv_con, registry, app) {
  weekly <- pv_app_weekly_scores(pv_con, registry, app)
  if (nrow(weekly) == 0) {
    return(data.frame(user_id = character(0), avg_weekly_score = numeric(0)))
  }
  avg <- stats::aggregate(score ~ user_id, weekly, mean)
  names(avg) <- c("user_id", "avg_weekly_score")
  avg
}

#' This app's events, scored and bucketed into (user, week) totals -- the
#' shared basis for [pv_app_weekly_user_scores()] (averaged across weeks)
#' and [pv_app_weekly_active_rate()] (thresholded per week).
#' @noRd
pv_app_weekly_scores <- function(pv_con, registry, app) {
  subs <- registry$subprocesses[registry$subprocesses$app == app, ]
  empty <- data.frame(user_id = character(0), week = as.Date(character(0)), score = numeric(0))
  if (nrow(subs) == 0) {
    return(empty)
  }

  events <- do.call(rbind, c(
    lapply(seq_len(nrow(subs)), function(i) {
      raw <- pv_pull_app_events(pv_con, registry, subs$process_id[i], subs$subprocess[i])
      if (nrow(raw) == 0) {
        return(NULL)
      }
      ideal <- registry$ideal[[paste(subs$process_id[i], subs$subprocess[i], sep = "::")]]
      score_by_activity <- if (is.null(ideal)) NULL else stats::setNames(ideal$activities$score, ideal$activities$id)
      raw$score <- if (is.null(score_by_activity)) 0 else unname(score_by_activity[raw$activity_id])
      raw$score[is.na(raw$score)] <- 0
      raw
    }),
    list(NULL)
  ))
  if (is.null(events) || nrow(events) == 0) {
    return(empty)
  }

  events$week <- lubridate::floor_date(as.Date(events$timestamp), unit = "week")
  stats::aggregate(score ~ user_id + week, events, sum)
}

#' Week-by-week share of identified users exceeding the usage threshold
#'
#' The one piece of IGA with real week-to-week granularity (training and
#' satisfaction are yearly snapshots) -- lets the Adoption tab show an
#' actual trend over time rather than a single static number.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param app an `app` value from `registry$subprocesses`.
#' @param adoption a [pv_load_adoption()] result.
#' @return a `data.frame`, ordered by week: `week` (`Date`), `pct_active`.
#'   Zero rows if this app has no adoption data, no declared
#'   `adoption_threshold`, or no events yet.
#' @export
pv_app_weekly_active_rate <- function(pv_con, registry, app, adoption) {
  empty <- data.frame(week = as.Date(character(0)), pct_active = numeric(0))
  entry <- adoption[[app]]
  if (is.null(entry) || nrow(entry$users) == 0) {
    return(empty)
  }
  threshold_vals <- unique(stats::na.omit(registry$subprocesses$adoption_threshold[registry$subprocesses$app == app]))
  if (length(threshold_vals) == 0) {
    return(empty)
  }
  threshold <- threshold_vals[1]

  weekly <- pv_app_weekly_scores(pv_con, registry, app)
  if (nrow(weekly) == 0) {
    return(empty)
  }

  n_users <- nrow(entry$users)
  rows <- lapply(split(weekly, weekly$week), function(g) {
    active_scores <- g$score[match(entry$users$user_id, g$user_id)]
    data.frame(week = g$week[1], pct_active = round(100 * sum(active_scores > threshold, na.rm = TRUE) / n_users, 1))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$week), ]
}

#' Adoption index (IGA) for one app
#'
#' IGA is the equally-weighted average of three components, each a
#' percentage of that app's *identified* users (from [pv_load_adoption()],
#' the admin-maintained denominator -- not just whoever happens to have
#' logged an event):
#' \itemize{
#'   \item `pct_trained` -- the share marked `trained` in the adoption workbook.
#'   \item `satisfaction` -- the workbook's single app-wide satisfaction
#'     value, assumed already on the same 0-100 scale as the other two
#'     components (rescale upstream in the workbook if yours isn't).
#'   \item `pct_active` -- the share whose average weekly engagement score
#'     (see [pv_app_weekly_user_scores()]) exceeds this app's declared
#'     `adoption_threshold` (`registry$subprocesses`); a user who never
#'     touched the app counts as `0`, correctly failing the threshold.
#' }
#' `iga` is `NA` whenever any component can't be computed (no adoption
#' data for this app, or no declared `adoption_threshold`) -- a partial
#' average would misrepresent an incomplete picture as a complete one;
#' the components that *are* available are still returned so the gap is
#' visible.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param app an `app` value from `registry$subprocesses`.
#' @param adoption a [pv_load_adoption()] result.
#' @return a list: `pct_trained`, `satisfaction`, `pct_active`, `iga`
#'   (all `NA_real_` if unavailable), `n_users` (identified user count).
#' @export
pv_app_iga <- function(pv_con, registry, app, adoption) {
  entry <- adoption[[app]]
  if (is.null(entry) || nrow(entry$users) == 0) {
    return(list(pct_trained = NA_real_, satisfaction = NA_real_, pct_active = NA_real_, iga = NA_real_, n_users = 0L))
  }

  n_users <- nrow(entry$users)
  pct_trained <- round(100 * sum(entry$users$trained, na.rm = TRUE) / n_users, 1)
  satisfaction <- entry$satisfaction

  threshold_vals <- unique(stats::na.omit(registry$subprocesses$adoption_threshold[registry$subprocesses$app == app]))
  threshold <- if (length(threshold_vals) > 0) threshold_vals[1] else NA_real_

  pct_active <- NA_real_
  if (!is.na(threshold)) {
    weekly <- pv_app_weekly_user_scores(pv_con, registry, app)
    score_by_user <- stats::setNames(weekly$avg_weekly_score, weekly$user_id)
    user_scores <- unname(score_by_user[entry$users$user_id])
    user_scores[is.na(user_scores)] <- 0
    pct_active <- round(100 * sum(user_scores > threshold) / n_users, 1)
  }

  components <- c(pct_trained, satisfaction, pct_active)
  iga <- if (anyNA(components)) NA_real_ else round(mean(components), 1)

  list(pct_trained = pct_trained, satisfaction = satisfaction, pct_active = pct_active, iga = iga, n_users = n_users)
}

#' Portfolio-wide weekly active-rate trend
#'
#' [pv_app_weekly_active_rate()] averaged across every app that has one,
#' week by week -- the Adoption tab's headline trend chart.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param adoption a [pv_load_adoption()] result.
#' @return a `data.frame`, ordered by week: `week` (`Date`), `pct_active`
#'   (mean across apps with data that week), `n_apps` (how many
#'   contributed). Zero rows if no app has a computable weekly rate.
#' @export
pv_portfolio_active_rate_trend <- function(pv_con, registry, adoption) {
  apps <- unique(registry$subprocesses$app)
  per_app <- do.call(rbind, c(
    lapply(apps, function(app) pv_app_weekly_active_rate(pv_con, registry, app, adoption)),
    list(NULL)
  ))
  if (is.null(per_app) || nrow(per_app) == 0) {
    return(data.frame(week = as.Date(character(0)), pct_active = numeric(0), n_apps = integer(0)))
  }

  mean_tab <- stats::aggregate(pct_active ~ week, per_app, mean)
  n_tab <- stats::aggregate(pct_active ~ week, per_app, length)
  names(n_tab)[2] <- "n_apps"
  out <- merge(mean_tab, n_tab, by = "week")
  out$pct_active <- round(out$pct_active, 1)
  out[order(out$week), ]
}

#' IGA for every app in the registry
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param adoption a [pv_load_adoption()] result.
#' @return a `data.frame`, one row per distinct `app`: `app`, `pct_trained`,
#'   `satisfaction`, `pct_active`, `iga`, `n_users`.
#' @export
pv_portfolio_iga <- function(pv_con, registry, adoption) {
  apps <- unique(registry$subprocesses$app)
  rows <- lapply(apps, function(app) {
    r <- pv_app_iga(pv_con, registry, app, adoption)
    data.frame(
      app = app, pct_trained = r$pct_trained, satisfaction = r$satisfaction,
      pct_active = r$pct_active, iga = r$iga, n_users = r$n_users,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
