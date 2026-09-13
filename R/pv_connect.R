#' Attach every instrumented process's DuckDB store
#'
#' Opens one DuckDB connection and `ATTACH`es every distinct `duckdb_path`
#' named in `registry$subprocesses`, aliased by `app` -- the "federated
#' storage, query at read time" architecture the playbook documents:
#' proviz never copies or centralizes another application's data, it
#' queries the files where they already live (a shared mount all
#' instrumented apps and this dashboard can see). Every attach is
#' `READ_ONLY`: the dashboard must never be able to write into a
#' production application's event store.
#'
#' A path that doesn't exist (an app not yet deployed, a mount that isn't
#' live) is skipped with a warning rather than failing the whole
#' connection -- one unreachable app should not take down the dashboard
#' for every other instrumented process. Check `$subprocesses$attached` to
#' see what actually succeeded.
#'
#' @param registry a [pv_load_registry()] result.
#' @return a `pv_connection` object: `$con` (the DBI connection -- attached
#'   databases are queryable as `<alias>.protrace_event`), `$subprocesses`
#'   (`registry$subprocesses` with an added logical `attached` column).
#' @export
pv_connect_registry <- function(registry) {
  stopifnot(inherits(registry, "pv_registry"))
  con <- DBI::dbConnect(duckdb::duckdb())

  subs <- registry$subprocesses
  subs$attached <- FALSE

  seen_alias <- character(0)
  for (i in seq_len(nrow(subs))) {
    alias <- subs$app[i]
    path <- subs$duckdb_path[i]

    if (alias %in% seen_alias) {
      subs$attached[i] <- TRUE # already attached by an earlier row for the same app
      next
    }
    if (!file.exists(path)) {
      warning("proviz: duckdb_path for app '", alias, "' not found at '", path, "' -- skipping", call. = FALSE)
      next
    }

    ok <- tryCatch(
      {
        DBI::dbExecute(con, sprintf(
          "ATTACH %s AS %s (READ_ONLY)",
          DBI::dbQuoteString(con, path), DBI::dbQuoteIdentifier(con, alias)
        ))
        TRUE
      },
      error = function(e) {
        warning("proviz: failed to attach '", path, "' as '", alias, "': ", conditionMessage(e), call. = FALSE)
        FALSE
      }
    )
    subs$attached[i] <- ok
    if (ok) seen_alias <- c(seen_alias, alias)
  }

  structure(list(con = con, subprocesses = subs), class = "pv_connection")
}

#' Close a proviz federated connection
#' @param pv_con a [pv_connect_registry()] result.
#' @export
pv_disconnect <- function(pv_con) {
  stopifnot(inherits(pv_con, "pv_connection"))
  DBI::dbDisconnect(pv_con$con, shutdown = TRUE)
  invisible(NULL)
}
