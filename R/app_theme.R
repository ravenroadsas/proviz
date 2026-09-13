## Single-tone theme: one accent (blue, matching the sequential/primary hue
## from the palette this app's charts also draw from -- see mod_deepdive.R)
## carries every interactive/highlight role -- buttons, active nav, KPI
## side-bars, chart fills. Status (registry state) is the one reserved
## exception: a small fixed palette, shipped as icon+label chips, never
## reused as decoration elsewhere -- see pv_status_badge().
.pv_accent <- "#2a78d6"
.pv_accent_dark <- "#184f95"
.pv_accent_soft <- "#eaf1fb"
.pv_ink <- "#0b0b0b"
.pv_ink_secondary <- "#52514e"
.pv_ink_muted <- "#898781"
.pv_border <- "#e1e0d9"
.pv_page_bg <- "#f7f7f5"
.pv_surface <- "#ffffff"
.pv_track_bg <- "#ece9e3"

## Categorical order for the one chart that genuinely needs to distinguish
## several identities (workload by role) -- fixed order, never cycled, per
## the dataviz playbook; kept short since the resource-breakdown chart only
## ever shows a handful of roles at once.
.pv_categorical <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4")

#' The dashboard's visual theme
#'
#' A single accent color (a professional blue) carries every interactive or
#' highlighted element -- buttons, the active nav tab, KPI card side-bars,
#' chart fills -- rather than the mixed semantic palette (green/blue/grey/
#' teal) a stock bootswatch KPI row defaults to. Registry status is the one
#' reserved exception (a small fixed palette shipped as icon+label chips,
#' never reused as decoration) -- see [pv_status_badge()].
#' @noRd
pv_theme <- function() {
  bslib::bs_theme(
    version = 5, base_font = bslib::font_google("Inter"),
    bg = .pv_page_bg, fg = .pv_ink, primary = .pv_accent, secondary = .pv_ink_muted,
    "body-color" = .pv_ink, "border-radius" = "0.6rem", "border-color" = .pv_border
  ) |>
    bslib::bs_add_rules(sprintf("
      body { font-size: 0.92rem; }

      .navbar { min-height: 52px; background: %s !important; border-bottom: 1px solid %s; box-shadow: none; }
      .navbar .navbar-brand { font-weight: 700; color: %s; }
      .navbar .nav-link { color: %s; font-weight: 500; padding-bottom: 0.9rem; border-bottom: 2px solid transparent; }
      .navbar .nav-link.active { color: %s; border-bottom-color: %s; }
      .navbar-text, .navbar .nav-link { font-size: 0.85rem; }

      .card { margin-bottom: 0.75rem; background: %s; border: 1px solid %s; box-shadow: 0 1px 2px rgba(11,11,11,0.04); }
      .card-header { padding: 0.55rem 0.95rem; font-weight: 600; font-size: 0.82rem; background: transparent; border-bottom: 1px solid %s; color: %s; }
      .card-body { padding: 0.75rem 0.95rem; }

      .btn-primary { background: %s; border-color: %s; }
      .btn-primary:hover { background: %s; border-color: %s; }
      .btn-outline-secondary { color: %s; border-color: %s; }

      table.dataTable { font-size: 0.85rem; }
      table.dataTable thead th { color: %s; font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.03em; border-bottom: 1px solid %s !important; }
      table.dataTable tbody tr:hover { background-color: %s !important; }

      .pv-kpi-row { display: flex; flex-wrap: wrap; gap: 0.75rem; margin-bottom: 0.9rem; }
      .pv-kpi { flex: 1 1 180px; display: flex; align-items: center; gap: 0.8rem;
        background: %s; border: 1px solid %s; border-left: 4px solid %s;
        border-radius: 0.5rem; padding: 0.85rem 1.05rem; box-shadow: 0 1px 2px rgba(11,11,11,0.04); }
      .pv-kpi-icon { width: 2.2rem; height: 2.2rem; border-radius: 0.5rem; flex-shrink: 0;
        background: %s; color: %s; display: flex; align-items: center; justify-content: center; font-size: 1rem; }
      .pv-kpi-text { min-width: 0; }
      .pv-kpi-title { font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.04em; color: %s; font-weight: 600; margin-bottom: 0.1rem; }
      .pv-kpi-value { font-size: 1.4rem; font-weight: 700; color: %s; line-height: 1.15; }

      .pv-badge { display: inline-flex; align-items: center; gap: 0.35rem; padding: 0.15rem 0.55rem;
        border-radius: 999px; font-size: 0.78rem; font-weight: 500; }
      .pv-badge .pv-dot { width: 0.5rem; height: 0.5rem; border-radius: 50%%; flex-shrink: 0; }

      .pv-cellbar { display: flex; flex-direction: column; gap: 0.25rem; min-width: 64px; padding: 0.15rem 0; }
      .pv-cellbar-value { font-size: 0.85rem; font-weight: 600; color: %s; font-variant-numeric: tabular-nums; }
      .pv-cellbar-track { height: 5px; background: %s; border-radius: 3px; overflow: hidden; }
      .pv-cellbar-fill { height: 100%%; background: %s; border-radius: 3px; }
      .pv-cellbar-empty { color: %s; }
    ", .pv_surface, .pv_border, .pv_ink, .pv_ink_secondary, .pv_accent, .pv_accent,
      .pv_surface, .pv_border, .pv_border, .pv_ink,
      .pv_accent, .pv_accent, .pv_accent_dark, .pv_accent_dark, .pv_ink_secondary, .pv_border,
      .pv_ink_muted, .pv_border, .pv_accent_soft,
      .pv_surface, .pv_border, .pv_accent,
      .pv_accent_soft, .pv_accent,
      .pv_ink_muted, .pv_ink,
      .pv_ink, .pv_track_bg, .pv_accent, .pv_ink_muted
    ))
}

#' One KPI tile: white surface, grey border, a narrow accent side-bar
#'
#' Deliberately not `bslib::value_box()` -- a full-bleed colored background
#' per KPI (the stock bootswatch look) reads as a rainbow of unrelated
#' semantics; a shared white surface with one accent stripe reads as one
#' coherent set of numbers about the same system.
#' @param title short label, rendered uppercase/muted.
#' @param value_output a `shiny::textOutput()` (or any tag) for the figure.
#' @param icon optional `shiny::icon()`.
#' @noRd
pv_kpi_box <- function(title, value_output, icon = NULL) {
  shiny::div(
    class = "pv-kpi",
    if (!is.null(icon)) shiny::div(class = "pv-kpi-icon", icon),
    shiny::div(
      class = "pv-kpi-text",
      shiny::div(class = "pv-kpi-title", title),
      shiny::div(class = "pv-kpi-value", value_output)
    )
  )
}

## Status palette: fixed, reserved for registry state alone (see
## dataviz playbook -- a status color is never reused as decoration
## elsewhere in the UI, and always ships as an icon/dot + label, never
## color alone).
.pv_status_colors <- c(
  instrumented = "#0ca30c", designed = "#fab219",
  not_started = "#898781", deprecated = "#898781"
)
.pv_status_labels <- c(
  instrumented = "Instrumented", designed = "Designed",
  not_started = "Not started", deprecated = "Deprecated"
)

#' A status value as an icon(dot)+label chip -- HTML, for use in a DT column
#' with `escape = FALSE`.
#' @noRd
pv_status_badge <- function(status) {
  color <- unname(.pv_status_colors[status])
  label <- unname(.pv_status_labels[status])
  sprintf(
    '<span class="pv-badge" style="background:%s1a;color:%s;"><span class="pv-dot" style="background:%s;"></span>%s</span>',
    color, .pv_ink_secondary, color, label
  )
}

#' A table cell showing a value alongside a thin proportional bar -- lets a
#' column like conformance or usage be scanned and compared at a glance
#' (bar length), not just read number by number. HTML, for use in a DT
#' column with `escape = FALSE`.
#' @param value_label the text to show (already formatted, e.g. `"84%"` or
#'   `"220"`); `NA` renders as a plain dash, no bar.
#' @param pct the bar's fill, 0-100 (clamped); `NA` renders as a plain dash.
#'   For a naturally 0-100 value (a percentage) pass it directly; for a
#'   raw count, scale it relative to the column's max first (see callers).
#' @noRd
pv_cell_bar <- function(value_label, pct) {
  if (is.na(value_label) || is.na(pct)) {
    return('<span class="pv-cellbar-empty">--</span>')
  }
  pct <- max(0, min(100, pct))
  sprintf(
    '<div class="pv-cellbar"><span class="pv-cellbar-value">%s</span><div class="pv-cellbar-track"><div class="pv-cellbar-fill" style="width:%s%%;"></div></div></div>',
    value_label, pct
  )
}
