#' Scan a Shiny app's R source for candidate process-mining hook points
#'
#' Static-analysis inventory, **not** a classifier: finds every
#' `observeEvent()` on an input, every raw `input$x` read (including ones
#' never wrapped in `observeEvent`), every UI widget declaration (with its
#' label), every `downloadHandler`, every navigation tab title, and every
#' `moduleServer()` boundary, across every `.R` file under `path`. It never
#' decides what any of this *means* for a business process -- that's the
#' human mapping step (see [run_registry_editor()]'s "Scan & map steps"
#' tab). The output is meant to be reviewed and annotated by hand,
#' turning "read this codebase cold" into "fill in two blank columns."
#'
#' Deliberately does not try to be a complete R static analyzer. Known,
#' accepted blind spots (see the `note` column on affected rows and
#' [pv_scan_custom_js()]):
#' - **Dynamically-computed ids** (e.g. `sliderInput(paste0("qi_", i), ...)`
#'   inside a `renderUI()`/`lapply()`) are flagged (`id_is_dynamic = TRUE`)
#'   rather than resolved -- there is, in general, no static value to
#'   report.
#' - **Custom JavaScript** (`Shiny.setInputValue()` in `www/*.js`) is
#'   invisible to R source analysis entirely; use [pv_scan_custom_js()]
#'   alongside this function.
#' - Only literal calls to the functions this scans for are found; a
#'   custom wrapper function that itself calls e.g. `shiny::actionButton()`
#'   is invisible unless the wrapper's own definition is also in scope
#'   (which it is, if it's in one of the scanned files -- but a call
#'   *through* such a wrapper is not resolved back to the widget it
#'   ultimately creates).
#'
#' @param path directory to scan, recursively.
#' @param file_pattern regex selecting which files to include (default
#'   every `.R`/`.r` file).
#' @return a `data.frame` (subclassed `pv_hook_inventory`), one row per
#'   finding, columns:
#'   \describe{
#'     \item{file, line}{location, relative to `path`}
#'     \item{kind}{one of `"ui_widget"`, `"observe_event_input"`,
#'       `"input_read"`, `"download_handler"`, `"nav_tab"`,
#'       `"module_boundary"`, `"db_write_call"` (see Details in the
#'       package overview for what each captures)}
#'     \item{id}{the widget/input id, function-call id, module id, or
#'       tab value -- whichever applies to `kind`}
#'     \item{label}{UI label text, for `kind == "ui_widget"`/`"nav_tab"`}
#'     \item{module}{the nearest enclosing named top-level function
#'       definition (e.g. `mod_create_basic_server`) -- used as the
#'       module/subprocess proxy rather than `moduleServer()`'s own id
#'       argument, which in idiomatic Shiny code is itself just a
#'       passthrough of the enclosing function's own `id` parameter, not
#'       a literal}
#'     \item{n_occurrences}{for `kind == "input_read"`: how many times
#'       this input is read outside any `observeEvent` in this file}
#'     \item{id_is_dynamic}{`TRUE` if the id/label was not a literal
#'       string (computed at runtime) -- see Details}
#'     \item{suggested_debounce}{for `kind == "ui_widget"`: a heuristic
#'       guess (by widget type) at whether settling this input should be
#'       debounced before the recorder logs it (a continuous input like a
#'       slider firing on every drag tick vs. a discrete click)}
#'     \item{nearby_comment}{the comment block immediately preceding this
#'       line, if any -- often already states business intent}
#'     \item{activity_id, notes}{blank; for the human mapping step}
#'   }
#' @export
pv_scan_app <- function(path, file_pattern = "\\.[Rr]$") {
  stopifnot(is.character(path), length(path) == 1, dir.exists(path))

  files <- list.files(path, pattern = file_pattern, recursive = TRUE, full.names = TRUE)
  rows <- lapply(files, function(f) tryCatch(pv_scan_file(f, path), error = function(e) {
    warning("proviz: skipped ", f, " (parse error): ", conditionMessage(e), call. = FALSE)
    NULL
  }))
  rows <- do.call(rbind, rows)

  if (is.null(rows) || nrow(rows) == 0) {
    rows <- pv_empty_inventory()
  } else {
    rows$activity_id <- NA_character_
    rows$notes <- NA_character_
    rows <- rows[order(rows$file, rows$line), ]
    rownames(rows) <- NULL
  }

  class(rows) <- c("pv_hook_inventory", class(rows))
  rows
}

#' @noRd
pv_empty_inventory <- function() {
  data.frame(
    file = character(0), line = integer(0), kind = character(0), id = character(0),
    label = character(0), module = character(0), n_occurrences = integer(0),
    id_is_dynamic = logical(0), suggested_debounce = logical(0), nearby_comment = character(0),
    activity_id = character(0), notes = character(0), stringsAsFactors = FALSE
  )
}

## Widget-type -> "suggest debouncing this before logging it" heuristic.
## A guess, by widget type only -- see the roxygen Details above.
.pv_continuous_widgets <- c(
  "sliderInput", "textInput", "textAreaInput", "numericInput",
  "dateRangeInput", "selectizeInput", "passwordInput"
)
.pv_ui_widget_fns <- c(
  "actionButton", "actionLink", "submitButton", .pv_continuous_widgets,
  "selectInput", "checkboxInput", "checkboxGroupInput", "radioButtons",
  "fileInput", "dateInput", "downloadButton"
)
.pv_nav_fns <- c("nav_panel", "tabPanel", "navbarMenu", "menuItem")

#' @noRd
pv_scan_file <- function(f, base_path) {
  parsed <- parse(f, keep.source = TRUE)
  pd <- utils::getParseData(parsed)
  src_lines <- readLines(f, warn = FALSE)
  rel_file <- pv_relative_path(f, base_path)

  target_fns <- c(.pv_ui_widget_fns, .pv_nav_fns, "observeEvent", "downloadHandler", "moduleServer")
  findings <- list()
  for (expr in as.list(parsed)) {
    findings <- pv_walk_calls(expr, target_fns, enclosing = list(), out = findings)
  }

  # Occurrence-order line lookup: the Nth AST match of a given function
  # name is matched to the Nth SYMBOL_FUNCTION_CALL token of that name in
  # source order -- both traversals visit calls left-to-right, so this
  # reliably recovers a precise line number even for a call buried inside
  # other calls, without needing to reconcile two different tree
  # representations node-for-node. See pv_scan_app()'s Details for why a
  # hybrid (AST for values, token table for locations) approach is used.
  line_cursor <- new.env()

  next_line <- function(fn_name) {
    rows <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == fn_name, , drop = FALSE]
    rows <- rows[order(rows$line1, rows$col1), ]
    i <- (line_cursor[[fn_name]] %||% 0L) + 1L
    line_cursor[[fn_name]] <- i
    if (i <= nrow(rows)) rows$line1[i] else NA_integer_
  }

  out_rows <- list()
  for (f_ in findings) {
    if (identical(f_$fn_name, "$input")) next # handled separately, below
    ln <- next_line(f_$fn_name)
    module <- pv_enclosing_module(f_$enclosing)

    if (f_$fn_name %in% .pv_ui_widget_fns) {
      arg1 <- pv_arg(f_$call, 1, "inputId")
      lbl <- pv_arg(f_$call, 2, "label")
      out_rows[[length(out_rows) + 1]] <- pv_row(
        rel_file, ln, "ui_widget", arg1$value, lbl$value, module,
        NA_integer_, arg1$dynamic, f_$fn_name %in% .pv_continuous_widgets,
        pv_nearby_comment(src_lines, ln)
      )
    } else if (f_$fn_name %in% .pv_nav_fns) {
      lbl <- pv_arg(f_$call, 1, "title")
      val <- pv_named_arg(f_$call, "value")
      out_rows[[length(out_rows) + 1]] <- pv_row(
        rel_file, ln, "nav_tab", val$value %||% lbl$value, lbl$value, module,
        NA_integer_, lbl$dynamic, NA,
        pv_nearby_comment(src_lines, ln)
      )
    } else if (identical(f_$fn_name, "observeEvent")) {
      trigger <- if (length(f_$call) >= 2) f_$call[[2]] else NULL
      input_id <- pv_input_dollar_id(trigger)
      out_rows[[length(out_rows) + 1]] <- pv_row(
        rel_file, ln, "observe_event_input", input_id %||% deparse1_safe(trigger), NA_character_,
        module, NA_integer_, is.null(input_id), NA,
        pv_nearby_comment(src_lines, ln)
      )
    } else if (identical(f_$fn_name, "downloadHandler")) {
      out_rows[[length(out_rows) + 1]] <- pv_row(
        rel_file, ln, "download_handler", pv_enclosing_output_id(f_$enclosing), NA_character_,
        module, NA_integer_, FALSE, NA,
        pv_nearby_comment(src_lines, ln)
      )
    } else if (identical(f_$fn_name, "moduleServer")) {
      arg1 <- pv_arg(f_$call, 1, "id")
      out_rows[[length(out_rows) + 1]] <- pv_row(
        rel_file, ln, "module_boundary", arg1$value, NA_character_,
        module, NA_integer_, arg1$dynamic, NA,
        pv_nearby_comment(src_lines, ln)
      )
    }
  }

  ## input$x reads outside any observeEvent() trigger expression -- catches
  ## logic read inside a plain reactive()/renderUI()/etc that never gets an
  ## observeEvent() wrapper (a real, previously-missed case, not a
  ## hypothetical one -- see the playbook). pv_walk_calls() already skips
  ## descending into an observeEvent()'s own trigger expression, so every
  ## "$input" finding collected here is, by construction, a read that
  ## isn't that trigger -- no post-hoc filtering needed.
  outside <- Filter(function(x) identical(x$fn_name, "$input"), findings)
  if (length(outside) > 0) {
    ids <- vapply(outside, function(x) x$input_id %||% NA_character_, character(1))
    modules <- vapply(outside, function(x) pv_enclosing_module(x$enclosing) %||% NA_character_, character(1))
    dyn <- vapply(outside, function(x) is.na(x$input_id), logical(1))
    agg <- stats::aggregate(
      list(n_occurrences = ids),
      by = list(id = ids, module = modules, id_is_dynamic = dyn),
      FUN = length
    )
    for (i in seq_len(nrow(agg))) {
      out_rows[[length(out_rows) + 1]] <- pv_row(
        rel_file, NA_integer_, "input_read", agg$id[i], NA_character_, agg$module[i],
        agg$n_occurrences[i], agg$id_is_dynamic[i], NA, NA_character_
      )
    }
  }

  if (length(out_rows) == 0) return(NULL)
  do.call(rbind, out_rows)
}

#' @noRd
pv_row <- function(file, line, kind, id, label, module, n_occurrences, id_is_dynamic, suggested_debounce, nearby_comment) {
  data.frame(
    file = file, line = line, kind = kind, id = id %||% NA_character_, label = label %||% NA_character_,
    module = module %||% NA_character_, n_occurrences = n_occurrences, id_is_dynamic = isTRUE(id_is_dynamic),
    suggested_debounce = suggested_debounce, nearby_comment = nearby_comment %||% NA_character_,
    stringsAsFactors = FALSE
  )
}
