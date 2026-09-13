test_that("pv_scan_custom_js finds static and dynamic setInputValue ids", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "www"))
  writeLines(c(
    "Shiny.setInputValue('js_input', value);",
    "Shiny.setInputValue(dynamicVar, value2);"
  ), file.path(dir, "www", "custom.js"))

  result <- pv_scan_custom_js(dir)
  expect_equal(nrow(result), 2)

  static_row <- result[!result$id_is_dynamic, ]
  expect_equal(static_row$id, "js_input")

  dynamic_row <- result[result$id_is_dynamic, ]
  expect_true(is.na(dynamic_row$id))
})

test_that("pv_scan_custom_js returns the empty schema when nothing matches", {
  dir <- withr::local_tempdir()
  writeLines("console.log('hi');", file.path(dir, "plain.js"))
  result <- pv_scan_custom_js(dir)
  expect_equal(nrow(result), 0)
  expect_named(result, c("file", "line", "id", "id_is_dynamic"))
})
