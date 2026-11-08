#' Launch the registry editor app
#'
#' A separate, write-capable companion to the (strictly read-only)
#' process intelligence dashboard ([run_pi_dashboard()]) -- the
#' read/write boundary is why this is its own Shiny app rather than a
#' dashboard tab. Drives the registry's "scan + human curation"
#' workflow end to end, against the four-artifact model (registry,
#' ideal diagrams, click map, event log -- see [pv_load_registry()]'s
#' docs):
#' \itemize{
#'   \item **Processes & apps**: add a business process, add an app
#'     (a `subprocess`, 1:1 with it) to an existing process.
#'   \item **Process flow**: declare allowed transitions between a
#'     process's own apps (cycles allowed -- a rework loop is just a
#'     back-edge), and named process-level outcomes.
#'   \item **Scan & map steps**: run [pv_scan_app()] against a target
#'     app's source, review the candidate hook inventory, map each hook
#'     a human wants to track onto a step (creating it if new) with its
#'     concrete click binding (selector/kind/feature/event/weight) --
#'     many clicks can map to one step.
#'   \item **Steps & outcomes**: declare an app's allowed step
#'     transitions (its own diagram) and named outcomes.
#'   \item **Users & roles**: declare (or update) a user's role -- the
#'     one piece of resource metadata not in the real event log itself,
#'     needed for the dashboard's role filter.
#' }
#'
#' v1 is deliberately append-only (no delete/rename yet). Every write
#' goes through a small set of plain, unit-tested `pv_registry_db_*`
#' helpers that open a connection, write, and close before the next
#' reactive tick, so the editor never holds the registry file open
#' against the dashboard or another edit.
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
    bslib::nav_panel("Process flow", pv_registry_editor_processflow_ui("processflow")),
    bslib::nav_panel("Scan & map steps", pv_registry_editor_scan_ui("scan")),
    bslib::nav_panel("Steps & outcomes", pv_registry_editor_steps_ui("steps")),
    bslib::nav_panel("Users & roles", pv_registry_editor_roles_ui("roles"))
  )

  server <- function(input, output, session) {
    registry_version <- shiny::reactiveVal(0L) # bumped after every write, to invalidate the read reactives below
    touch <- function() registry_version(registry_version() + 1L)

    registry <- shiny::reactive({
      registry_version()
      pv_load_registry(registry_path)
    })

    pv_registry_editor_processes_server("processes", registry_path, registry, touch)
    pv_registry_editor_processflow_server("processflow", registry_path, registry, touch)
    pv_registry_editor_scan_server("scan", registry_path, registry, touch)
    pv_registry_editor_steps_server("steps", registry_path, registry, touch)
    pv_registry_editor_roles_server("roles", registry_path, registry, touch)
  }

  shiny::runApp(shiny::shinyApp(ui, server), ...)
}

#' @noRd
pv_registry_editor_notify_error <- function(expr) {
  tryCatch(expr, error = function(e) {
    shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
    NULL
  })
}

#' `x %||% default` only falls back on `NULL`, not `NA` -- these DT-edited
#' columns come back as (possibly-`NA`) vector elements, never `NULL`, so
#' a plain `%||%` would leave e.g. a `NOT NULL` text column violated by a
#' blank cell the human never filled in. `x` is always length 1 here.
#' @noRd
pv_na_default <- function(x, default) if (is.na(x)) default else x

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
        bslib::card_header("Apps"),
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
        shiny::textInput(ns("subprocess"), "subprocess (id)"),
        shiny::textInput(ns("app"), "app (display name -- can differ from subprocess)"),
        shiny::textInput(ns("app_link"), "app_link (optional, a URL)"),
        shiny::textInput(ns("log_path"), "log_path (optional, once instrumented)"),
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
      DT::datatable(registry()$subprocesses, rownames = FALSE, options = list(pageLength = 10, dom = "tp", scrollX = TRUE), class = "compact stripe")
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
        shiny::req(input$sub_process_id, input$subprocess, input$app)
        pv_registry_db_insert_subprocess(
          registry_path, input$sub_process_id, input$subprocess, input$app,
          app_link = if (nzchar(input$app_link %||% "")) input$app_link else NA_character_,
          log_path = if (nzchar(input$log_path %||% "")) input$log_path else NA_character_,
          adoption_threshold = input$adoption_threshold
        )
        shiny::showNotification(paste0("Added app '", input$app, "'"), type = "message")
        touch()
      })
    })
  })
}

## -------------------------------------------------------------------------
## Tab 2: Process flow (edges between a process's own apps, + outcomes)
## -------------------------------------------------------------------------

#' @noRd
pv_registry_editor_processflow_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      4,
      bslib::card(
        bslib::card_header("Process"),
        shiny::selectInput(ns("process_id"), "process_id", choices = NULL)
      ),
      bslib::card(
        bslib::card_header("Add an allowed transition"),
        shiny::helpText("Cycles are allowed -- e.g. a rework loop back to an earlier app."),
        shiny::selectInput(ns("from_subprocess"), "From app", choices = NULL),
        shiny::selectInput(ns("to_subprocess"), "To app", choices = NULL),
        shiny::actionButton(ns("add_edge"), "Add transition", class = "btn-primary")
      ),
      bslib::card(
        bslib::card_header("Add a process-level outcome"),
        shiny::textInput(ns("outcome_id"), "outcome_id"),
        shiny::textInput(ns("outcome_label"), "label"),
        shiny::selectInput(ns("outcome_kind"), "kind", c("deliverable", "decision")),
        shiny::selectInput(ns("outcome_subprocess"), "concluded by app", choices = NULL),
        shiny::selectInput(ns("outcome_tier"), "tier (decisions only)", c("", "operational", "tactical", "strategic")),
        shiny::actionButton(ns("add_outcome"), "Add outcome", class = "btn-primary")
      )
    ),
    shiny::column(
      8,
      bslib::card(bslib::card_header("Apps"), DT::DTOutput(ns("apps_table"))),
      bslib::card(bslib::card_header("Declared transitions"), DT::DTOutput(ns("edges_table"))),
      bslib::card(bslib::card_header("Declared process-level outcomes"), DT::DTOutput(ns("outcomes_table")))
    )
  )
}

#' @noRd
pv_registry_editor_processflow_server <- function(id, registry_path, registry, touch) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      ids <- stats::setNames(registry()$processes$process_id, registry()$processes$process)
      shiny::updateSelectInput(session, "process_id", choices = ids)
    })

    process_subs <- shiny::reactive({
      shiny::req(input$process_id)
      registry()$subprocesses[registry()$subprocesses$process_id == input$process_id, ]
    })

    shiny::observe({
      choices <- stats::setNames(process_subs()$subprocess, process_subs()$app)
      shiny::updateSelectInput(session, "from_subprocess", choices = choices)
      shiny::updateSelectInput(session, "to_subprocess", choices = choices)
      shiny::updateSelectInput(session, "outcome_subprocess", choices = choices)
    })

    diagram <- shiny::reactive({
      shiny::req(input$process_id)
      registry()$process_diagrams[[input$process_id]]
    })

    output$apps_table <- DT::renderDT({
      DT::datatable(process_subs(), rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })
    output$edges_table <- DT::renderDT({
      edges <- diagram()$edges
      shiny::validate(shiny::need(!is.null(edges) && nrow(edges) > 0, "No transitions declared yet."))
      DT::datatable(edges, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })
    output$outcomes_table <- DT::renderDT({
      outcomes <- diagram()$outcomes
      shiny::validate(shiny::need(!is.null(outcomes) && nrow(outcomes) > 0, "No outcomes declared yet."))
      DT::datatable(outcomes, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })

    shiny::observeEvent(input$add_edge, {
      pv_registry_editor_notify_error({
        shiny::req(input$process_id, input$from_subprocess, input$to_subprocess)
        pv_registry_db_insert_process_edge(registry_path, input$process_id, input$from_subprocess, input$to_subprocess)
        shiny::showNotification("Added transition", type = "message")
        touch()
      })
    })

    shiny::observeEvent(input$add_outcome, {
      pv_registry_editor_notify_error({
        shiny::req(input$process_id, input$outcome_id, input$outcome_label, input$outcome_subprocess)
        pv_registry_db_upsert_process_outcome(
          registry_path, input$process_id, input$outcome_id, input$outcome_label,
          input$outcome_kind, input$outcome_subprocess,
          tier = if (nzchar(input$outcome_tier)) input$outcome_tier else NA_character_
        )
        shiny::showNotification(paste0("Added outcome '", input$outcome_id, "'"), type = "message")
        touch()
      })
    })
  })
}

## -------------------------------------------------------------------------
## Tab 3: Scan & map steps
## -------------------------------------------------------------------------

#' @noRd
pv_registry_editor_scan_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      4,
      bslib::card(
        bslib::card_header("Scan"),
        shiny::selectInput(ns("target_subprocess"), "App to map hooks onto", choices = NULL),
        shiny::textInput(ns("scan_dir"), "Directory to scan", placeholder = "/path/to/shiny/app"),
        shiny::actionButton(ns("run_scan"), "Run scan", class = "btn-primary"),
        shiny::helpText(
          "Static-analysis inventory of UI widgets, observeEvent() triggers, etc. ",
          "(see pv_scan_app()). Fill in 'step_id' for every row you want to keep (creating a ",
          "new step if it doesn't exist yet, or reusing one so several clicks count toward the ",
          "same step); blank rows are skipped on save. 'selector' defaults to '#' + the widget ",
          "id, which is right for most Shiny inputs but not for nav tabs -- check it."
        )
      )
    ),
    shiny::column(
      8,
      bslib::card(
        bslib::card_header("Candidate hooks -- edit the mapping columns, then save"),
        DT::DTOutput(ns("scan_table")),
        shiny::br(),
        shiny::actionButton(ns("save_clicks"), "Save mapped steps", class = "btn-primary")
      )
    )
  )
}

#' @noRd
.pv_scan_editable_cols <- c("step_id", "label", "selector", "kind", "feature", "event", "weight")

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
        # DOM-event kind (click/change/tab) a click mapping needs, so the
        # editable `kind` column starts blank rather than inheriting a
        # value in the wrong vocabulary.
        hooks$hook_type <- hooks$kind
        hooks$step_id <- hooks$activity_id
        hooks$label <- hooks$label %||% NA_character_
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

    shiny::observeEvent(input$save_clicks, {
      pv_registry_editor_notify_error({
        shiny::req(input$target_subprocess, scan_result())
        process_id <- registry()$subprocesses$process_id[registry()$subprocesses$subprocess == input$target_subprocess][1]
        df <- scan_result()
        to_save <- df[!is.na(df$step_id) & nzchar(df$step_id), ]
        shiny::req(nrow(to_save) > 0)
        for (step_id in unique(to_save$step_id)) {
          pv_registry_db_insert_step(registry_path, process_id, input$target_subprocess, step_id)
        }
        for (i in seq_len(nrow(to_save))) {
          r <- to_save[i, ]
          click_id <- pv_na_default(r$id, paste0(r$step_id, "_", i))
          pv_registry_db_upsert_click(
            registry_path, process_id, input$target_subprocess, click_id, r$step_id,
            label = pv_na_default(r$label, r$step_id), selector = r$selector, kind = r$kind,
            feature = pv_na_default(r$feature, r$step_id), event = r$event, weight = r$weight
          )
        }
        shiny::showNotification(paste0("Saved ", nrow(to_save), " click(s)"), type = "message")
        touch()
      })
    })
  })
}

## -------------------------------------------------------------------------
## Tab 4: Steps & outcomes (per app)
## -------------------------------------------------------------------------

#' @noRd
pv_registry_editor_steps_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      4,
      bslib::card(
        bslib::card_header("App"),
        shiny::selectInput(ns("subprocess"), "App", choices = NULL)
      ),
      bslib::card(
        bslib::card_header("Add an allowed transition"),
        shiny::helpText("Cycles are allowed -- e.g. a step that loops back for rework."),
        shiny::selectInput(ns("from_step"), "From step", choices = NULL),
        shiny::selectInput(ns("to_step"), "To step", choices = NULL),
        shiny::actionButton(ns("add_edge"), "Add transition", class = "btn-primary")
      ),
      bslib::card(
        bslib::card_header("Add an outcome"),
        shiny::textInput(ns("outcome_id"), "outcome_id"),
        shiny::textInput(ns("outcome_label"), "label"),
        shiny::selectInput(ns("outcome_kind"), "kind", c("deliverable", "decision")),
        shiny::selectInput(ns("outcome_step"), "concluded by step", choices = NULL),
        shiny::selectInput(ns("outcome_tier"), "tier (decisions only)", c("", "operational", "tactical", "strategic")),
        shiny::textInput(ns("tracking_app"), "outcome_tracking_app (optional)"),
        shiny::textInput(ns("tracking_key"), "outcome_tracking_join_key (optional)"),
        shiny::actionButton(ns("add_outcome"), "Add outcome", class = "btn-primary")
      )
    ),
    shiny::column(
      8,
      bslib::card(bslib::card_header("Steps"), DT::DTOutput(ns("steps_table"))),
      bslib::card(bslib::card_header("Declared transitions"), DT::DTOutput(ns("edges_table"))),
      bslib::card(bslib::card_header("Outcomes"), DT::DTOutput(ns("outcomes_table")))
    )
  )
}

#' @noRd
pv_registry_editor_steps_server <- function(id, registry_path, registry, touch) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      subs <- registry()$subprocesses
      choices <- stats::setNames(subs$subprocess, paste0(subs$process_id, " :: ", subs$app))
      shiny::updateSelectInput(session, "subprocess", choices = choices)
    })

    pids <- shiny::reactive({
      shiny::req(input$subprocess)
      registry()$subprocesses$process_id[registry()$subprocesses$subprocess == input$subprocess][1]
    })

    diagram <- shiny::reactive({
      shiny::req(input$subprocess)
      registry()$subprocess_diagrams[[paste(pids(), input$subprocess, sep = "::")]]
    })

    shiny::observe({
      steps <- diagram()$steps
      choices <- if (is.null(steps)) character(0) else stats::setNames(steps$id, steps$label)
      shiny::updateSelectInput(session, "from_step", choices = choices)
      shiny::updateSelectInput(session, "to_step", choices = choices)
      shiny::updateSelectInput(session, "outcome_step", choices = choices)
    })

    output$steps_table <- DT::renderDT({
      steps <- diagram()$steps
      shiny::validate(shiny::need(!is.null(steps) && nrow(steps) > 0, "No steps declared for this app yet -- use Scan & map steps."))
      DT::datatable(steps, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })

    output$edges_table <- DT::renderDT({
      edges <- diagram()$edges
      shiny::validate(shiny::need(!is.null(edges) && nrow(edges) > 0, "No transitions declared yet."))
      DT::datatable(edges, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })

    output$outcomes_table <- DT::renderDT({
      oc <- diagram()$outcomes
      shiny::validate(shiny::need(!is.null(oc) && nrow(oc) > 0, "No outcomes declared yet."))
      DT::datatable(oc, rownames = FALSE, options = list(pageLength = 10, dom = "tp"), class = "compact stripe")
    })

    shiny::observeEvent(input$add_edge, {
      pv_registry_editor_notify_error({
        shiny::req(input$subprocess, input$from_step, input$to_step)
        pv_registry_db_insert_step_edge(registry_path, pids(), input$subprocess, input$from_step, input$to_step)
        shiny::showNotification("Added transition", type = "message")
        touch()
      })
    })

    shiny::observeEvent(input$add_outcome, {
      pv_registry_editor_notify_error({
        shiny::req(input$subprocess, input$outcome_id, input$outcome_label, input$outcome_step)
        pv_registry_db_upsert_subprocess_outcome(
          registry_path, pids(), input$subprocess, input$outcome_id, input$outcome_label,
          input$outcome_kind, input$outcome_step,
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
## Tab 5: Users & roles
## -------------------------------------------------------------------------

#' @noRd
pv_registry_editor_roles_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      4,
      bslib::card(
        bslib::card_header("Set a user's role"),
        shiny::helpText(
          "Not tied to any process/app -- a role can be declared for a user who hasn't ",
          "logged any activity yet. Setting an existing user_id's role again updates it."
        ),
        shiny::textInput(ns("user_id"), "user_id"),
        shiny::textInput(ns("role"), "role"),
        shiny::actionButton(ns("set_role"), "Set role", class = "btn-primary")
      )
    ),
    shiny::column(
      8,
      bslib::card(bslib::card_header("Declared roles"), DT::DTOutput(ns("roles_table")))
    )
  )
}

#' @noRd
pv_registry_editor_roles_server <- function(id, registry_path, registry, touch) {
  shiny::moduleServer(id, function(input, output, session) {
    output$roles_table <- DT::renderDT({
      roles <- registry()$user_roles
      shiny::validate(shiny::need(nrow(roles) > 0, "No roles declared yet."))
      DT::datatable(roles, rownames = FALSE, options = list(pageLength = 15, dom = "tp"), class = "compact stripe")
    })

    shiny::observeEvent(input$set_role, {
      pv_registry_editor_notify_error({
        shiny::req(input$user_id, input$role)
        pv_registry_db_upsert_user_role(registry_path, input$user_id, input$role)
        shiny::showNotification(paste0("Set role for '", input$user_id, "'"), type = "message")
        touch()
      })
    })
  })
}
