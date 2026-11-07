#' Launch the registry editor app
#'
#' A separate, write-capable companion to the (strictly read-only)
#' process intelligence dashboard ([run_pi_dashboard()]) -- the
#' read/write boundary is why this is its own Shiny app rather than a
#' dashboard tab. Drives the registry's "scan + human curation" workflow:
#' run [pv_scan_app()] against a target app's source, review the
#' candidate hook inventory it finds, map each hook a human wants to
#' track onto a business activity (an id, label, role, and the
#' selector/kind/feature/event/weight a future telemetry export will
#' need), and declare that app's acceptable activity sequences
#' (variants) and named outcomes -- all written directly into the
#' registry DuckDB file [pv_load_registry()] reads.
#'
#' v1 scope is deliberately append-only: add a process, add an app to an
#' existing process, add/update activities (including by curating a
#' fresh scan), add a variant, add an outcome, add a business-level step
#' variant. There is no delete or rename here yet -- editing a mistake
#' means fixing it with a direct `DBI`/DuckDB statement against the file,
#' same as any other data-entry error would need until a later version
#' adds it.
#'
#' Every write opens its own short-lived connection and closes it before
#' the next reactive tick (see the `pv_registry_db_*` write helpers) --
#' it never holds the registry file open, so [run_pi_dashboard()] (or
#' another editor session) can read or write it between edits. Running
#' this and the dashboard against the very same file at the exact same
#' instant can still collide (DuckDB allows one writer at a time); this
#' is meant for the occasional, human-paced editing session, not
#' concurrent multi-user use.
#'
#' @param registry_path path to the registry DuckDB file to edit.
#' @param ... passed on to `shiny::runApp()` (e.g. `port`, `launch.browser`).
#' @return does not return; runs the Shiny app.
#' @export
run_registry_editor <- function(registry_path, ...) {
  stopifnot(is.character(registry_path), length(registry_path) == 1, file.exists(registry_path))

  ui <- bslib::page_navbar(
    title = "Registry Editor",
    theme = pv_theme(),
    bslib::nav_panel("Processes & apps", pv_registry_editor_processes_ui("processes")),
    bslib::nav_panel("Scan & map activities", pv_registry_editor_scan_ui("scan")),
    bslib::nav_panel("Variants & outcomes", pv_registry_editor_ideal_ui("ideal")),
    bslib::nav_panel("Step flow", pv_registry_editor_stepflow_ui("stepflow"))
  )

  server <- function(input, output, session) {
    registry_version <- shiny::reactiveVal(0L) # bumped after every write, to invalidate the read reactives below
    touch <- function() registry_version(registry_version() + 1L)

    registry <- shiny::reactive({
      registry_version()
      pv_load_registry(registry_path)
    })

    pv_registry_editor_processes_server("processes", registry_path, registry, touch)
    pv_registry_editor_scan_server("scan", registry_path, registry, touch)
    pv_registry_editor_ideal_server("ideal", registry_path, registry, touch)
    pv_registry_editor_stepflow_server("stepflow", registry_path, registry, touch)
  }

  shiny::runApp(shiny::shinyApp(ui, server), ...)
}

#' `x %||% default` only falls back on `NULL`, not `NA` -- these DT-edited
#' columns come back as (possibly-`NA`) vector elements, never `NULL`, so
#' a plain `%||%` would leave e.g. `activities.role`'s `NOT NULL`
#' constraint violated by a blank cell the human never filled in. `x` is
#' always length 1 here (one row's one cell).
#' @noRd
pv_na_default <- function(x, default) if (is.na(x)) default else x

#' @noRd
pv_registry_editor_notify_error <- function(expr) {
  tryCatch(expr, error = function(e) {
    shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
    NULL
  })
}

## -------------------------------------------------------------------------
## Tab 1: Processes & apps
## -------------------------------------------------------------------------

#' @noRd
pv_registry_editor_processes_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      7,
      bslib::card(
        bslib::card_header("Processes"),
        DT::DTOutput(ns("processes_table"))
      ),
      bslib::card(
        bslib::card_header("Apps / steps"),
        DT::DTOutput(ns("subprocesses_table"))
      )
    ),
    shiny::column(
      5,
      bslib::card(
        bslib::card_header("Add a process"),
        shiny::textInput(ns("process_id"), "process_id"),
        shiny::textInput(ns("macroprocess"), "macroprocess"),
        shiny::textInput(ns("process"), "process (display name)"),
        shiny::textInput(ns("owner"), "owner"),
        shiny::selectInput(ns("status"), "status", c("not_started", "designed", "instrumented", "deprecated")),
        shiny::selectInput(ns("loop_length"), "loop_length", c("short", "medium", "long")),
        shiny::actionButton(ns("add_process"), "Add process", class = "btn-primary")
      ),
      bslib::card(
        bslib::card_header("Add an app to an existing process"),
        shiny::selectInput(ns("sub_process_id"), "process_id", choices = NULL),
        shiny::textInput(ns("app"), "app"),
        shiny::textInput(ns("subprocess"), "subprocess"),
        shiny::textInput(ns("step"), "step (slug)"),
        shiny::textInput(ns("step_label"), "step_label (optional)"),
        shiny::numericInput(ns("step_order"), "step_order", value = 1, min = 1, step = 1),
        shiny::textInput(ns("duckdb_path"), "duckdb_path (optional, once instrumented)"),
        shiny::numericInput(ns("adoption_threshold"), "adoption_threshold (optional)", value = NA),
        shiny::actionButton(ns("add_subprocess"), "Add app", class = "btn-primary")
      )
    )
  )
}

#' @noRd
pv_registry_editor_processes_server <- function(id, registry_path, registry, touch) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      ids <- stats::setNames(registry()$processes$process_id, registry()$processes$process)
      shiny::updateSelectInput(session, "sub_process_id", choices = ids)
    })

    output$processes_table <- DT::renderDT({
      DT::datatable(registry()$processes, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })
    output$subprocesses_table <- DT::renderDT({
      DT::datatable(registry()$subprocesses, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })

    shiny::observeEvent(input$add_process, {
      pv_registry_editor_notify_error({
        shiny::req(input$process_id, input$macroprocess, input$process, input$owner)
        pv_registry_db_insert_process(
          registry_path, input$process_id, input$macroprocess, input$process,
          input$owner, input$status, input$loop_length
        )
        shiny::showNotification(paste0("Added process '", input$process_id, "'"), type = "message")
        touch()
      })
    })

    shiny::observeEvent(input$add_subprocess, {
      pv_registry_editor_notify_error({
        shiny::req(input$sub_process_id, input$app, input$subprocess, input$step, input$step_order)
        pv_registry_db_insert_subprocess(
          registry_path, input$sub_process_id, input$app, input$subprocess, input$step,
          step_label = if (nzchar(input$step_label %||% "")) input$step_label else NULL,
          step_order = input$step_order,
          duckdb_path = if (nzchar(input$duckdb_path %||% "")) input$duckdb_path else NA_character_,
          adoption_threshold = input$adoption_threshold
        )
        shiny::showNotification(paste0("Added app '", input$app, "'"), type = "message")
        touch()
      })
    })
  })
}

## -------------------------------------------------------------------------
## Tab 2: Scan & map activities
## -------------------------------------------------------------------------

#' @noRd
pv_registry_editor_scan_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      4,
      bslib::card(
        bslib::card_header("Scan"),
        shiny::selectInput(ns("target_subprocess"), "App (subprocess) to map hooks onto", choices = NULL),
        shiny::textInput(ns("scan_dir"), "Directory to scan", placeholder = "/path/to/shiny/app"),
        shiny::actionButton(ns("run_scan"), "Run scan", class = "btn-primary"),
        shiny::helpText(
          "Static-analysis inventory of UI widgets, observeEvent() triggers, etc. ",
          "(see pv_scan_app()). Fill in 'activity_id' for every row you want to keep; ",
          "blank rows are skipped on save. 'selector' defaults to '#' + the widget id, ",
          "which is right for most Shiny inputs but not for nav tabs -- check it."
        )
      )
    ),
    shiny::column(
      8,
      bslib::card(
        bslib::card_header("Candidate hooks -- edit the mapping columns, then save"),
        DT::DTOutput(ns("scan_table")),
        shiny::br(),
        shiny::actionButton(ns("save_activities"), "Save mapped activities", class = "btn-primary")
      )
    )
  )
}

#' @noRd
.pv_scan_editable_cols <- c("activity_id", "label", "role", "score", "selector", "kind", "feature", "event", "weight")

#' @noRd
pv_registry_editor_scan_server <- function(id, registry_path, registry, touch) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      subs <- registry()$subprocesses
      choices <- stats::setNames(subs$subprocess, paste0(subs$process_id, " :: ", subs$app))
      shiny::updateSelectInput(session, "target_subprocess", choices = choices)
    })

    scan_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_scan, {
      pv_registry_editor_notify_error({
        shiny::req(input$scan_dir)
        if (!dir.exists(input$scan_dir)) stop("proviz: directory not found: ", input$scan_dir, call. = FALSE)
        hooks <- pv_scan_app(input$scan_dir)
        # pv_scan_app()'s own `kind` is the finding type (ui_widget,
        # nav_tab, ...) -- useful context, kept as `hook_type` -- not the
        # DOM-event kind (click/change/tab) a telemetry mapping needs, so
        # the editable `kind` column starts blank rather than inheriting
        # a value in the wrong vocabulary.
        hooks$hook_type <- hooks$kind
        hooks$label <- hooks$label %||% NA_character_
        hooks$role <- NA_character_
        hooks$score <- 0
        # a reasonable starting guess, not a claim -- Shiny renders most
        # input widgets with id="<inputId>" in the DOM, but nav tabs and
        # anything custom need the human to correct this.
        hooks$selector <- ifelse(!hooks$id_is_dynamic & !is.na(hooks$id), paste0("#", hooks$id), NA_character_)
        hooks$kind <- NA_character_
        hooks$feature <- NA_character_
        hooks$event <- NA_character_
        hooks$weight <- 1
        scan_result(hooks)
      })
    })

    output$scan_table <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(scan_result()), "Run a scan to see candidate hooks."))
      DT::datatable(
        scan_result(), rownames = FALSE,
        editable = list(target = "column", columns = which(names(scan_result()) %in% .pv_scan_editable_cols) - 1),
        options = list(pageLength = 15, scrollX = TRUE), class = "compact stripe"
      )
    }, server = TRUE)

    shiny::observeEvent(input$scan_table_cell_edit, {
      # In "column"-target editable mode, DT's cell_edit payload is one
      # row per table row (not one cell) -- $row/$col/$value are parallel
      # vectors, $col constant. Working a column at a time via `[[` (never
      # `df[i, j]` with a vector j) keeps this off the subclassed
      # pv_hook_inventory data.frame's `[`/`[<-` edge cases entirely.
      info <- input$scan_table_cell_edit
      df <- scan_result()
      col_idx <- info$col[1] + 1L
      col_vec <- df[[col_idx]]
      for (i in seq_along(info$row)) {
        col_vec[info$row[i]] <- DT::coerceValue(info$value[i], col_vec[info$row[i]])
      }
      df[[col_idx]] <- col_vec
      scan_result(df)
    })

    shiny::observeEvent(input$save_activities, {
      pv_registry_editor_notify_error({
        shiny::req(input$target_subprocess, scan_result())
        process_id <- registry()$subprocesses$process_id[registry()$subprocesses$subprocess == input$target_subprocess][1]
        df <- scan_result()
        to_save <- df[!is.na(df$activity_id) & nzchar(df$activity_id), ]
        shiny::req(nrow(to_save) > 0)
        for (i in seq_len(nrow(to_save))) {
          r <- to_save[i, ]
          pv_registry_db_upsert_activity(
            registry_path, process_id, input$target_subprocess, r$activity_id,
            label = pv_na_default(r$label, r$activity_id), role = pv_na_default(r$role, "Unassigned"),
            score = pv_na_default(r$score, 0), selector = r$selector, kind = r$kind,
            feature = pv_na_default(r$feature, r$activity_id), event = r$event, weight = r$weight
          )
        }
        shiny::showNotification(paste0("Saved ", nrow(to_save), " activity(ies)"), type = "message")
        touch()
      })
    })
  })
}

## -------------------------------------------------------------------------
## Tab 3: Variants & outcomes (per app)
## -------------------------------------------------------------------------

#' @noRd
pv_registry_editor_ideal_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      4,
      bslib::card(
        bslib::card_header("App"),
        shiny::selectInput(ns("subprocess"), "App (subprocess)", choices = NULL)
      ),
      bslib::card(
        bslib::card_header("Add a variant"),
        shiny::helpText("Comma-separated activity ids, in order, e.g. a, b, c"),
        shiny::textInput(ns("variant_ids"), "Activity id sequence"),
        shiny::actionButton(ns("add_variant"), "Add variant", class = "btn-primary")
      ),
      bslib::card(
        bslib::card_header("Add an outcome"),
        shiny::textInput(ns("outcome_id"), "outcome_id"),
        shiny::textInput(ns("outcome_label"), "label"),
        shiny::selectInput(ns("outcome_kind"), "kind", c("deliverable", "decision")),
        shiny::selectInput(ns("outcome_activity"), "activity_id", choices = NULL),
        shiny::selectInput(ns("outcome_tier"), "tier (decisions only)", c("", "operational", "tactical", "strategic")),
        shiny::textInput(ns("tracking_app"), "outcome_tracking_app (optional)"),
        shiny::textInput(ns("tracking_key"), "outcome_tracking_join_key (optional)"),
        shiny::actionButton(ns("add_outcome"), "Add outcome", class = "btn-primary")
      )
    ),
    shiny::column(
      8,
      bslib::card(bslib::card_header("Activities"), DT::DTOutput(ns("activities_table"))),
      bslib::card(bslib::card_header("Variants"), shiny::uiOutput(ns("variants_list"))),
      bslib::card(bslib::card_header("Outcomes"), DT::DTOutput(ns("outcomes_table")))
    )
  )
}

#' @noRd
pv_registry_editor_ideal_server <- function(id, registry_path, registry, touch) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      subs <- registry()$subprocesses
      choices <- stats::setNames(subs$subprocess, paste0(subs$process_id, " :: ", subs$app))
      shiny::updateSelectInput(session, "subprocess", choices = choices)
    })

    ideal <- shiny::reactive({
      shiny::req(input$subprocess)
      process_id <- registry()$subprocesses$process_id[registry()$subprocesses$subprocess == input$subprocess][1]
      list(process_id = process_id, ip = registry()$ideal[[paste(process_id, input$subprocess, sep = "::")]])
    })

    shiny::observe({
      acts <- ideal()$ip$activities
      choices <- if (is.null(acts)) character(0) else acts$id
      shiny::updateSelectInput(session, "outcome_activity", choices = choices)
    })

    output$activities_table <- DT::renderDT({
      acts <- ideal()$ip$activities
      shiny::validate(shiny::need(!is.null(acts) && nrow(acts) > 0, "No activities declared for this app yet -- use Scan & map activities."))
      DT::datatable(acts, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })

    output$variants_list <- shiny::renderUI({
      v <- ideal()$ip$variants
      if (is.null(v) || length(v) == 0) return(shiny::p(class = "text-muted", "No variants declared yet."))
      shiny::tags$ol(lapply(v, function(seq) shiny::tags$li(paste(seq, collapse = " -> "))))
    })

    output$outcomes_table <- DT::renderDT({
      oc <- ideal()$ip$outcomes
      shiny::validate(shiny::need(!is.null(oc) && nrow(oc) > 0, "No outcomes declared yet."))
      DT::datatable(oc, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })

    shiny::observeEvent(input$add_variant, {
      pv_registry_editor_notify_error({
        shiny::req(input$subprocess, input$variant_ids)
        ids <- trimws(strsplit(input$variant_ids, ",")[[1]])
        ids <- ids[nzchar(ids)]
        shiny::req(length(ids) > 0)
        pv_registry_db_insert_variant(registry_path, ideal()$process_id, input$subprocess, ids)
        shiny::showNotification("Added variant", type = "message")
        touch()
      })
    })

    shiny::observeEvent(input$add_outcome, {
      pv_registry_editor_notify_error({
        shiny::req(input$subprocess, input$outcome_id, input$outcome_label, input$outcome_activity)
        pv_registry_db_upsert_outcome(
          registry_path, ideal()$process_id, input$subprocess, input$outcome_id, input$outcome_label,
          input$outcome_kind, input$outcome_activity,
          tier = if (nzchar(input$outcome_tier)) input$outcome_tier else NA_character_,
          outcome_tracking_app = if (nzchar(input$tracking_app)) input$tracking_app else NA_character_,
          outcome_tracking_join_key = if (nzchar(input$tracking_key)) input$tracking_key else NA_character_
        )
        shiny::showNotification(paste0("Added outcome '", input$outcome_id, "'"), type = "message")
        touch()
      })
    })
  })
}

## -------------------------------------------------------------------------
## Tab 4: Business-level step flow
## -------------------------------------------------------------------------

#' @noRd
pv_registry_editor_stepflow_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      4,
      bslib::card(
        bslib::card_header("Process"),
        shiny::selectInput(ns("process_id"), "process_id", choices = NULL)
      ),
      bslib::card(
        bslib::card_header("Add a step_variant"),
        shiny::helpText("Comma-separated step slugs, in order, e.g. draft, review"),
        shiny::textInput(ns("step_seq"), "Step sequence"),
        shiny::actionButton(ns("add_step_variant"), "Add step_variant", class = "btn-primary")
      )
    ),
    shiny::column(
      8,
      bslib::card(bslib::card_header("Declared steps, in order"), DT::DTOutput(ns("steps_table"))),
      bslib::card(bslib::card_header("Declared step_variants"), shiny::uiOutput(ns("step_variants_list")))
    )
  )
}

#' @noRd
pv_registry_editor_stepflow_server <- function(id, registry_path, registry, touch) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      ids <- stats::setNames(registry()$processes$process_id, registry()$processes$process)
      shiny::updateSelectInput(session, "process_id", choices = ids)
    })

    output$steps_table <- DT::renderDT({
      shiny::req(input$process_id)
      steps <- registry()$steps[registry()$steps$process_id == input$process_id, ]
      DT::datatable(steps, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })

    output$step_variants_list <- shiny::renderUI({
      shiny::req(input$process_id)
      v <- registry()$step_variants[[input$process_id]]
      if (is.null(v) || length(v) == 0) {
        return(shiny::p(class = "text-muted", "None declared -- falls back to the step_order sequence."))
      }
      shiny::tags$ol(lapply(v, function(seq) shiny::tags$li(paste(seq, collapse = " -> "))))
    })

    shiny::observeEvent(input$add_step_variant, {
      pv_registry_editor_notify_error({
        shiny::req(input$process_id, input$step_seq)
        steps <- trimws(strsplit(input$step_seq, ",")[[1]])
        steps <- steps[nzchar(steps)]
        shiny::req(length(steps) > 0)
        pv_registry_db_insert_step_variant(registry_path, input$process_id, steps)
        shiny::showNotification("Added step_variant", type = "message")
        touch()
      })
    })
  })
}
