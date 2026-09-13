#' Scan for custom-JS-driven Shiny inputs
#'
#' `Shiny.setInputValue("x", ...)` calls (typically in `www/*.js`) create
#' inputs invisible to [pv_scan_app()]'s R-source analysis entirely -- one
#' of that function's stated blind spots. This is a much lighter,
#' regex-based pass over `.js`/`.html` files, surfacing them as a
#' to-check list rather than a resolved inventory: no attempt is made to
#' understand the surrounding JS (e.g. whether the call is conditional,
#' inside a loop building a dynamic id, ...).
#'
#' @param path directory to scan, recursively (typically an app's `www/`
#'   folder, or the whole app directory).
#' @return a `data.frame`: `file`, `line`, `id` (the string literal
#'   passed as `setInputValue()`'s first argument, when there is one),
#'   `id_is_dynamic` (`TRUE` when it isn't a plain string literal --
#'   e.g. a JS variable).
#' @export
pv_scan_custom_js <- function(path) {
  stopifnot(is.character(path), length(path) == 1, dir.exists(path))
  files <- list.files(path, pattern = "\\.(js|html)$", recursive = TRUE, full.names = TRUE)

  rows <- lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    hit <- grepl("setInputValue\\s*\\(", lines)
    if (!any(hit)) {
      return(NULL)
    }
    idx <- which(hit)
    ids <- vapply(lines[idx], function(l) {
      m <- regmatches(l, regexpr("setInputValue\\s*\\(\\s*[\"']([^\"']*)[\"']", l, perl = TRUE))
      if (length(m) == 0 || !nzchar(m)) {
        return(NA_character_)
      }
      sub("setInputValue\\s*\\(\\s*[\"']([^\"']*)[\"'].*", "\\1", m, perl = TRUE)
    }, character(1), USE.NAMES = FALSE)

    data.frame(
      file = pv_relative_path(f, path), line = idx, id = ids,
      id_is_dynamic = is.na(ids), stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)

  if (is.null(rows)) {
    rows <- data.frame(
      file = character(0), line = integer(0), id = character(0),
      id_is_dynamic = logical(0), stringsAsFactors = FALSE
    )
  }
  rows
}
