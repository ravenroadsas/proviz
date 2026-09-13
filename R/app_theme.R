#' The dashboard's visual theme: flatly/cosmos, tightened for information density
#'
#' Both are bootswatch themes with a light background and a modern,
#' professional look out of the box; the rules added on top only reduce
#' padding/font-size so more of the overview grid and deep-dive panels fit
#' without scrolling -- a KPI/registry dashboard read many times a day
#' should stay compact, not spacious.
#' @param bootswatch `"flatly"` (default) or `"cosmos"`.
#' @noRd
pv_theme <- function(bootswatch = c("flatly", "cosmos")) {
  bootswatch <- match.arg(bootswatch)
  bslib::bs_theme(version = 5, bootswatch = bootswatch, base_font = bslib::font_google("Inter")) |>
    bslib::bs_add_rules("
      body { font-size: 0.92rem; }
      .card { margin-bottom: 0.75rem; }
      .card-header { padding: 0.5rem 0.9rem; font-weight: 600; }
      .card-body { padding: 0.75rem 0.9rem; }
      .bslib-value-box .value-box-title { font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.02em; }
      .bslib-value-box .value-box-value { font-size: 1.5rem; }
      table.dataTable { font-size: 0.85rem; }
      .navbar { min-height: 48px; }
    ")
}
