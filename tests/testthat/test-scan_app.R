local_fixture_app <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)

  writeLines(c(
    "app_ui <- function() {",
    "  tagList(",
    "    # Ask the user to pick an analog well",
    "    selectInput('analog_id', 'Analog well', choices = NULL),",
    "    sliderInput('qi', 'Initial rate', min = 0, max = 100, value = 50),",
    "    actionButton('save_curve', 'Save curve'),",
    "    tabPanel('Overview', value = 'overview_tab')",
    "  )",
    "}"
  ), file.path(dir, "app_ui.R"))

  writeLines(c(
    "mod_x_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    # Persist the curve when the user clicks save",
    "    observeEvent(input$save_curve, {",
    "      curve_name <- input$curve_name",
    "    })",
    "",
    "    output$export_data <- downloadHandler(",
    "      filename = function() 'out.csv',",
    "      content = function(file) {}",
    "    )",
    "",
    "    dynamic_id <- paste0('qi_', 1)",
    "    sliderInput(dynamic_id, 'Dynamic', min = 0, max = 1)",
    "  })",
    "}"
  ), file.path(dir, "mod_x_server.R"))

  writeLines(c(
    "broken <- function( {"
  ), file.path(dir, "broken.R"))

  dir
}

test_that("pv_scan_app finds ui widgets with labels and dynamic ids", {
  dir <- local_fixture_app()
  result <- suppressWarnings(pv_scan_app(dir))

  widgets <- result[result$kind == "ui_widget", ]
  expect_true(all(c("analog_id", "qi", "save_curve") %in% widgets$id))

  analog <- widgets[widgets$id == "analog_id", ]
  expect_equal(analog$label, "Analog well")
  expect_false(analog$id_is_dynamic)

  dyn <- widgets[widgets$id_is_dynamic, ]
  expect_equal(nrow(dyn), 1)
  expect_equal(dyn$id, "dynamic_id")
})

test_that("pv_scan_app flags continuous widgets for debounce and not action buttons", {
  dir <- local_fixture_app()
  result <- suppressWarnings(pv_scan_app(dir))
  widgets <- result[result$kind == "ui_widget", ]

  expect_true(widgets$suggested_debounce[widgets$id == "qi"])
  expect_false(widgets$suggested_debounce[widgets$id == "save_curve"])
})

test_that("pv_scan_app captures nav tabs", {
  dir <- local_fixture_app()
  result <- suppressWarnings(pv_scan_app(dir))
  tabs <- result[result$kind == "nav_tab", ]
  expect_equal(tabs$id, "overview_tab")
  expect_equal(tabs$label, "Overview")
})

test_that("pv_scan_app resolves observeEvent triggers and skips them from input_read", {
  dir <- local_fixture_app()
  result <- suppressWarnings(pv_scan_app(dir))

  obs <- result[result$kind == "observe_event_input", ]
  expect_equal(obs$id, "save_curve")

  reads <- result[result$kind == "input_read", ]
  expect_true("curve_name" %in% reads$id)
  expect_false("save_curve" %in% reads$id)
})

test_that("pv_scan_app resolves module and output ids", {
  dir <- local_fixture_app()
  result <- suppressWarnings(pv_scan_app(dir))

  mod <- result[result$kind == "module_boundary", ]
  expect_equal(mod$module, "mod_x_server")

  dl <- result[result$kind == "download_handler", ]
  expect_equal(dl$id, "export_data")
  expect_equal(dl$module, "mod_x_server")
})

test_that("pv_scan_app captures the nearby comment", {
  dir <- local_fixture_app()
  result <- suppressWarnings(pv_scan_app(dir))
  obs <- result[result$kind == "observe_event_input", ]
  expect_equal(obs$nearby_comment, "Persist the curve when the user clicks save")
})

test_that("pv_scan_app warns and skips files with parse errors, but keeps other findings", {
  dir <- local_fixture_app()
  expect_warning(result <- pv_scan_app(dir), "parse error")
  expect_gt(nrow(result), 0)
  expect_false(any(result$file == "broken.R"))
})

test_that("pv_scan_app returns the empty schema for a directory with no matches", {
  dir <- withr::local_tempdir()
  writeLines("x <- 1", file.path(dir, "plain.R"))
  result <- pv_scan_app(dir)
  expect_s3_class(result, "pv_hook_inventory")
  expect_equal(nrow(result), 0)
})
