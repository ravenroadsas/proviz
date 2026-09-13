## Internal helpers for pv_scan_app(). Not exported.
##
## Design note: getting both (a) precise line numbers and (b) clean
## argument values for a call buried inside other calls (very common in
## real Shiny UI code -- e.g. a sliderInput() nested inside tagList()
## inside a slider_with_hist() wrapper inside a renderUI()) is awkward
## from either representation alone. Base R attaches a srcref to each
## top-level expression (and to each statement inside a `{ }` block), but
## not reliably to arbitrary nested call arguments -- so line numbers for
## a deeply-nested call can't be read off the parsed call object directly.
## utils::getParseData() has exact line/col for every token, including
## nested ones, but reconstructing clean argument values from its flat
## token table is fragile.
##
## The approach here is a hybrid: walk the actual parsed call objects
## (via as.list()/is.call() recursion, pv_walk_calls()) to find matches
## and extract argument values -- this gives real R values for free
## (a string is just a string, no text-parsing). Separately, look up each
## match's line number by position in getParseData()'s SYMBOL_FUNCTION_CALL
## rows for that function name, matched by *occurrence order* -- both
## traversals visit calls left-to-right in source order, so the Nth AST
## match for a given function name reliably corresponds to the Nth token
## row for that name, without needing to reconcile the two tree
## representations node-for-node.

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

#' `f`'s path relative to `base_path`, for readable output (nobody wants
#' `/home/user/app/R/mod_x.R` in a report meant to be read by a person).
#' @noRd
pv_relative_path <- function(f, base_path) {
  base <- normalizePath(base_path, mustWork = FALSE)
  full <- normalizePath(f, mustWork = FALSE)
  rel <- sub(base, "", full, fixed = TRUE) # fixed = TRUE: base is a literal path, not a regex
  sub("^/", "", rel)
}

#' @noRd
deparse1_safe <- function(expr) {
  txt <- paste(deparse(expr), collapse = " ")
  txt <- gsub("\\s+", " ", txt)
  if (nchar(txt) > 80) txt <- paste0(substr(txt, 1, 77), "...")
  txt
}

#' Recursively walk a parsed expression, collecting every call matching
#' one of `fn_names` (matched on the call's own function name, so
#' `pkg::fn(...)` matches `fn`), plus every `input$x`-shaped expression
#' (tagged `fn_name = "$input"`), each with the stack of enclosing calls
#' at the point it was found (used to recover the nearest enclosing
#' `moduleServer()` id, and the `output$x <-` an enclosing
#' `downloadHandler()` is assigned to).
#' @noRd
pv_walk_calls <- function(expr, fn_names, enclosing, out) {
  if (!is.call(expr)) {
    return(out)
  }

  fn <- expr[[1]]
  fn_name <- if (is.symbol(fn)) {
    as.character(fn)
  } else if (is.call(fn) && length(fn) == 3 && identical(fn[[1]], as.symbol("::"))) {
    as.character(fn[[3]])
  } else {
    NA_character_
  }

  if (identical(fn_name, "$") && length(expr) == 3) {
    input_id <- pv_input_dollar_id(expr)
    lhs_is_input <- is.symbol(expr[[2]]) && identical(as.character(expr[[2]]), "input")
    if (lhs_is_input) {
      out[[length(out) + 1]] <- list(call = expr, fn_name = "$input", enclosing = enclosing, input_id = input_id)
    }
  } else if (!is.na(fn_name) && fn_name %in% fn_names) {
    out[[length(out) + 1]] <- list(call = expr, fn_name = fn_name, enclosing = enclosing)
  }

  # Tracked so pv_enclosing_module() can report "which top-level function
  # is this inside" -- a far more reliable module/subprocess proxy than
  # moduleServer()'s own id argument, which in idiomatic Shiny code is
  # itself just a passthrough of the enclosing function's own `id`
  # parameter (e.g. `mod_x_server <- function(id, ...) moduleServer(id,
  # ...)`), not a literal -- the literal only exists back at the call
  # site, in a different file.
  def_name <- NA_character_
  if (fn_name %in% c("<-", "=", "<<-") && length(expr) == 3) {
    rhs <- expr[[3]]
    if (is.call(rhs) && identical(rhs[[1]], as.symbol("function")) && is.symbol(expr[[2]])) {
      def_name <- as.character(expr[[2]])
    }
  }

  new_enclosing <- c(enclosing, list(list(call = expr, fn_name = fn_name, def_name = def_name)))
  arg_list <- as.list(expr)[-1]
  if (identical(fn_name, "observeEvent") && length(arg_list) >= 1) {
    # Skip the trigger expression itself: it's already captured whole as
    # this observeEvent's own finding (see pv_scan_file()) -- walking into
    # it too would record its input$x a second time as an unrelated
    # "read outside any observeEvent", which it isn't.
    arg_list <- arg_list[-1]
  }
  for (a in arg_list) {
    # A skipped/empty argument slot in the ORIGINAL call (e.g. the second
    # argument of `x[i, ]`-style indexing) comes back from as.list() as R's
    # internal "missing" marker; passing that on as a real argument
    # propagates actual missingness, not a value, and forcing it inside
    # the callee raises "argument is missing" -- a quirk of R's promise
    # mechanism, not a bug in the expression being scanned. Skip such
    # elements defensively rather than letting one indexing expression
    # anywhere in a file abort the whole scan.
    out <- tryCatch(pv_walk_calls(a, fn_names, new_enclosing, out), error = function(e) out)
  }
  out
}

#' If `expr` is exactly `input$x` (or `input$"x"`), the id as a string;
#' otherwise `NULL`.
#' @noRd
pv_input_dollar_id <- function(expr) {
  if (!(is.call(expr) && identical(expr[[1]], as.symbol("$")) && length(expr) == 3)) {
    return(NULL)
  }
  lhs <- expr[[2]]
  if (!(is.symbol(lhs) && identical(as.character(lhs), "input"))) {
    return(NULL)
  }
  rhs <- expr[[3]]
  if (is.symbol(rhs)) {
    return(as.character(rhs))
  }
  if (is.character(rhs) && length(rhs) == 1) {
    return(rhs)
  }
  NULL
}

#' A call's argument at conceptual `position` (counting only unnamed
#' arguments) OR under any of `...` as a name -- whichever resolves.
#' Approximates R's own positional/named matching; does not implement
#' partial matching.
#' @noRd
pv_arg <- function(call, position, ...) {
  candidate_names <- c(...)
  args <- as.list(call)[-1]
  nm <- names(args)
  if (is.null(nm)) nm <- rep("", length(args))

  val <- NULL
  if (length(candidate_names) > 0) {
    hit <- which(nm %in% candidate_names)
    if (length(hit) > 0) val <- args[[hit[1]]]
  }
  if (is.null(val)) {
    unnamed_idx <- which(nm == "")
    if (position <= length(unnamed_idx)) val <- args[[unnamed_idx[position]]]
  }
  pv_literal_or_dynamic(val)
}

#' A call's argument under exactly `name`, or a missing/NA result.
#' @noRd
pv_named_arg <- function(call, name) {
  args <- as.list(call)[-1]
  nm <- names(args)
  if (is.null(nm)) {
    return(list(value = NA_character_, dynamic = FALSE))
  }
  hit <- which(nm == name)
  if (length(hit) == 0) {
    return(list(value = NA_character_, dynamic = FALSE))
  }
  pv_literal_or_dynamic(args[[hit[1]]])
}

#' A string literal is reported as-is. `ns("x")` -- the near-universal
#' Shiny module id-namespacing idiom -- is unwrapped to `"x"`, since it's
#' effectively static despite being a call. Anything else (a variable, a
#' `paste0(...)`, ...) is reported as its deparsed source text, flagged
#' `dynamic = TRUE`: there's no static value to resolve to.
#' @noRd
pv_literal_or_dynamic <- function(val) {
  if (is.null(val)) {
    return(list(value = NA_character_, dynamic = FALSE))
  }
  if (is.character(val) && length(val) == 1) {
    return(list(value = val, dynamic = FALSE))
  }
  if (is.call(val) && identical(val[[1]], as.symbol("ns")) && length(val) == 2 &&
    is.character(val[[2]]) && length(val[[2]]) == 1) {
    return(list(value = as.character(val[[2]]), dynamic = FALSE))
  }
  list(value = deparse1_safe(val), dynamic = TRUE)
}

#' The nearest enclosing named top-level function definition (e.g.
#' `mod_create_basic_server`), searching from the innermost enclosing call
#' outward -- see the note where this is tracked, in `pv_walk_calls()`,
#' for why this is used as the module/subprocess proxy instead of
#' `moduleServer()`'s own id argument.
#' @noRd
pv_enclosing_module <- function(enclosing) {
  for (e in rev(enclosing)) {
    if (!is.null(e$def_name) && !is.na(e$def_name)) {
      return(e$def_name)
    }
  }
  NA_character_
}

#' The `output$x` this call is (nearest-enclosing-ly) being assigned to,
#' e.g. for a `downloadHandler()` on the right-hand side of
#' `output$export_data <- downloadHandler(...)`.
#' @noRd
pv_enclosing_output_id <- function(enclosing) {
  for (e in rev(enclosing)) {
    if (e$fn_name %in% c("<-", "=", "<<-") && length(e$call) >= 2) {
      lhs <- e$call[[2]]
      if (is.call(lhs) && identical(lhs[[1]], as.symbol("$")) && length(lhs) == 3) {
        obj <- lhs[[2]]
        if (is.symbol(obj) && identical(as.character(obj), "output")) {
          slot <- lhs[[3]]
          return(if (is.symbol(slot)) as.character(slot) else as.character(slot))
        }
      }
    }
  }
  NA_character_
}

#' The contiguous run of `#`-comment lines immediately above `line` (no
#' blank-line gap), oldest first, collapsed to one string -- often
#' already states business intent in plain English.
#' @noRd
pv_nearby_comment <- function(src_lines, line) {
  if (is.na(line) || line <= 1) {
    return(NA_character_)
  }
  i <- line - 1
  comments <- character(0)
  while (i >= 1 && grepl("^\\s*#", src_lines[i])) {
    comments <- c(trimws(sub("^\\s*#'?\\s?", "", src_lines[i])), comments)
    i <- i - 1
  }
  if (length(comments) == 0) {
    return(NA_character_)
  }
  paste(comments, collapse = " ")
}
