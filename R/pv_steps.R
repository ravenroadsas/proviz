#' Business-process-level step sequence (the "ideal" business flow)
#'
#' A business process's intended flow isn't a separately-authored ideal
#' process file -- by default it's simply the step order already declared
#' in the registry (see [pv_load_registry()]'s `$steps`), wrapped as a
#' single-variant list. A process whose real flow branches or loops back
#' to an earlier step can instead declare `step_variants` in the registry
#' (see [pv_load_registry()]'s docs) -- when present, those declared
#' step-label sequences are used instead, so business-level conformance
#' can score a branch or a rework loop as a legitimate path rather than a
#' deviation. Either way this is shaped the way
#' [pv_case_conformance()]/[pv_process_conformance()] expect, so
#' business-level conformance reuses the exact same scoring mechanism as
#' app-level conformance -- just fed steps instead of one app's own
#' activities.
#'
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a list: `variants` (a list of step-label sequences -- the
#'   declared `step_variants` if any, else a length-1 list containing the
#'   `step_order`-derived sequence). Zero-length if the process declares
#'   no steps yet and no `step_variants` either.
#' @export
pv_step_sequence <- function(registry, process_id) {
  declared <- registry$step_variants[[process_id]]
  if (!is.null(declared)) {
    return(list(variants = declared))
  }

  steps <- registry$steps[registry$steps$process_id == process_id, ]
  steps <- steps[order(steps$step_order), ]
  if (nrow(steps) == 0) {
    return(list(variants = list()))
  }
  list(variants = list(steps$step_label))
}

#' Re-label a process's events at step granularity
#'
#' For mining *at the business-process level*: an app's own `activity_id`
#' values aren't comparable across apps (two different apps' internal
#' steps mean different things), but every event's `subprocess` maps to
#' exactly one `step` (see [pv_load_registry()]) -- a coarse dimension
#' that *is* comparable across apps, since every app in a business process
#' shares the same step vocabulary the admin declared for that process.
#' This replaces `activity_id` with the matching `step_label`, so the
#' existing [pv_build_eventlog()]/conformance/outcome machinery can be
#' reused unchanged, just fed a coarser "activity".
#'
#' Consecutive events within the same case that relabel to the same step
#' are collapsed to one occurrence (keeping the first's timestamp): an app
#' logging several of its own fine-grained activities inside one step
#' would otherwise inflate the business-level trace with noisy self-loops
#' that don't reflect a real step-to-step transition.
#'
#' @param events a case-linked events `data.frame` (as from
#'   [pv_pull_process_events()]) -- must include `subprocess`.
#' @param subprocesses a subprocesses `data.frame` (`registry$subprocesses`,
#'   already filtered to the one process these events belong to, to avoid
#'   any cross-process `subprocess` name collision) with `subprocess` and
#'   `step_label` columns.
#' @return `events`, ordered by `case_id`/`timestamp`, with `activity_id`
#'   replaced by the matching `step_label` and consecutive same-step
#'   repeats within a case collapsed (a row whose `subprocess` isn't found
#'   in `subprocesses` keeps its original `activity_id`, prefixed
#'   `"[unmapped] "`, rather than being silently dropped or mislabeled as
#'   a real step).
#' @export
pv_events_at_step_level <- function(events, subprocesses) {
  stopifnot(is.data.frame(events), all(c("activity_id", "subprocess") %in% names(events)))
  if (nrow(events) == 0) {
    return(events)
  }

  events <- events[order(events$case_id, events$timestamp), ]
  lookup <- stats::setNames(subprocesses$step_label, subprocesses$subprocess)
  step_label <- unname(lookup[events$subprocess])
  events$activity_id <- ifelse(is.na(step_label), paste("[unmapped]", events$activity_id), step_label)

  n <- nrow(events)
  is_consecutive_repeat <- c(FALSE, events$case_id[-1] == events$case_id[-n] & events$activity_id[-1] == events$activity_id[-n])
  events[!is_consecutive_repeat, ]
}

#' Which app(s) sit in a business process's final step
#'
#' The final step (highest `step_order`) is where a case's overall
#' business outcome is decided (see [pv_business_outcome_distribution()]) --
#' shared logic any caller that needs "the app(s) that finish this
#' process" (the outcomes table, a filtered re-pull for that same table)
#' can reuse, rather than re-deriving the final step inline.
#'
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a `subprocesses`-shaped `data.frame`, filtered to the final
#'   step's app(s). Zero rows if the process declares no steps.
#' @export
pv_final_step_apps <- function(registry, process_id) {
  steps <- registry$steps[registry$steps$process_id == process_id, ]
  if (nrow(steps) == 0) {
    return(registry$subprocesses[0, ])
  }
  final_step <- steps$step[which.max(steps$step_order)]
  registry$subprocesses[registry$subprocesses$process_id == process_id & registry$subprocesses$step == final_step, ]
}

#' Pull a business process's final-step events (app-level activity ids)
#'
#' The raw events behind [pv_business_outcome_distribution()] -- app-level
#' activity ids, *not* step-relabeled (an outcome's `activity_id` is one
#' app's own, so detecting it needs the app-level trace, unlike
#' [pv_pull_business_events()]'s step-relabeled one). Exposed separately
#' so a caller that wants to filter the outcomes table by date/role/user
#' (e.g. [mod_business_detail_server()]) can pull once, filter with
#' [pv_filter_events()], and pass the result into
#' [pv_business_outcome_distribution()]'s `events` argument.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a case-linked events `data.frame`, same shape as
#'   [pv_pull_process_events()]'s. Zero rows if the process has no steps
#'   or its final step has no attached apps.
#' @export
pv_pull_final_step_events <- function(pv_con, registry, process_id) {
  final_apps <- pv_final_step_apps(registry, process_id)
  if (nrow(final_apps) == 0) {
    return(pv_empty_events())
  }
  do.call(rbind, c(
    lapply(seq_len(nrow(final_apps)), function(i) {
      pv_pull_app_events(pv_con, registry, process_id, final_apps$subprocess[i])
    }),
    list(pv_empty_events())
  ))
}

#' Pull one process's events at business (step) granularity
#'
#' Combines [pv_pull_process_events()] (the cross-app union) with
#' [pv_events_at_step_level()] (the activity_id -> step_label relabeling)
#' -- the one preprocessing a business-level metric needs, done once
#' rather than repeated by every caller that wants it (conformance, the
#' process map, duration).
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a case-linked events `data.frame`, same shape as
#'   [pv_pull_process_events()]'s but with `activity_id` relabeled to each
#'   event's step (see [pv_events_at_step_level()]).
#' @export
pv_pull_business_events <- function(pv_con, registry, process_id) {
  events <- pv_pull_process_events(pv_con, registry, process_id)
  subs <- registry$subprocesses[registry$subprocesses$process_id == process_id, ]
  pv_events_at_step_level(events, subs)
}

#' Business-level conformance for one process
#'
#' Scores each case's step-level trace against the business process's
#' step order (see [pv_step_sequence()]) -- process mining applied at
#' business-process granularity, reusing the same scoring machinery
#' [pv_case_conformance()]/[pv_process_conformance()] already provide at
#' app level, just fed step-relabeled events and the step-order-derived
#' ideal flow instead of one app's own activities/ideal process.
#'
#' @param pv_con a [pv_connect_registry()] result.
#' @param registry a [pv_load_registry()] result.
#' @param process_id a `process_id` from `registry$processes`.
#' @return a `data.frame`, same shape as [pv_process_conformance()]'s.
#' @export
pv_business_process_conformance <- function(pv_con, registry, process_id) {
  events <- pv_pull_business_events(pv_con, registry, process_id)
  ideal <- pv_step_sequence(registry, process_id)
  pv_process_conformance(events, ideal)
}
