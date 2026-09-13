test_that("pv_relative_path strips the base path", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "R"))
  f <- file.path(dir, "R", "mod_x.R")
  writeLines("1", f)
  expect_equal(pv_relative_path(f, dir), "R/mod_x.R")
})

test_that("pv_literal_or_dynamic unwraps ns() and flags non-literals as dynamic", {
  expect_equal(pv_literal_or_dynamic("plain")$value, "plain")
  expect_false(pv_literal_or_dynamic("plain")$dynamic)

  ns_call <- quote(ns("x"))
  resolved <- pv_literal_or_dynamic(ns_call)
  expect_equal(resolved$value, "x")
  expect_false(resolved$dynamic)

  dyn <- pv_literal_or_dynamic(quote(paste0("qi_", i)))
  expect_true(dyn$dynamic)
  expect_match(dyn$value, "paste0")
})

test_that("pv_input_dollar_id recognizes input$x but not other objects", {
  expect_equal(pv_input_dollar_id(quote(input$go)), "go")
  expect_null(pv_input_dollar_id(quote(output$go)))
  expect_null(pv_input_dollar_id(quote(input[["go"]])))
})

test_that("pv_walk_calls survives a missing-argument slot without erroring", {
  expr <- quote(observeEvent(input$go, {
    x[i, ]
  }))
  out <- pv_walk_calls(expr, c("observeEvent"), enclosing = list(), out = list())
  expect_equal(length(out), 1)
  expect_equal(out[[1]]$fn_name, "observeEvent")
})

test_that("pv_walk_calls does not double-count an observeEvent trigger as an input_read", {
  expr <- quote(observeEvent(input$go, {
    y <- 1
  }))
  out <- pv_walk_calls(expr, c("observeEvent"), enclosing = list(), out = list())
  input_hits <- Filter(function(x) identical(x$fn_name, "$input"), out)
  expect_equal(length(input_hits), 0)
})

test_that("pv_enclosing_module tracks the nearest named top-level function, not moduleServer's id arg", {
  expr <- quote(mod_x_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      observeEvent(input$go, {
        NULL
      })
    })
  })
  out <- pv_walk_calls(expr, c("moduleServer", "observeEvent"), enclosing = list(), out = list())
  modules <- vapply(out, function(x) pv_enclosing_module(x$enclosing), character(1))
  expect_true(all(modules == "mod_x_server"))
})

test_that("pv_enclosing_output_id finds the output$x this call is assigned to", {
  expr <- quote(output$export_data <- downloadHandler(
    filename = function() "f.csv",
    content = function(file) NULL
  ))
  out <- pv_walk_calls(expr, c("downloadHandler"), enclosing = list(), out = list())
  expect_equal(pv_enclosing_output_id(out[[1]]$enclosing), "export_data")
})
