fixture_registry_with_clicks <- function() {
  structure(
    list(
      processes = data.frame(
        process_id = "p1", macroprocess = "mp1", process = "P1", owner = "o",
        status = "instrumented", loop_length = "short", stringsAsFactors = FALSE
      ),
      subprocesses = data.frame(
        process_id = "p1", subprocess = "sub1", app = "app1", app_link = "https://a.example",
        log_path = NA_character_, adoption_threshold = NA_real_, stringsAsFactors = FALSE
      ),
      process_diagrams = list(),
      subprocess_diagrams = list(),
      clicks = list(
        "p1::sub1" = data.frame(
          id = c("c1", "c2"), step_id = c("a", "a"), label = c("A1", "A2"),
          selector = c("#a1", "#a2"), kind = c("click", "click"),
          feature = c("validate", "validate"), event = c("a1_click", "a2_click"),
          weight = c(1, 2), stringsAsFactors = FALSE
        )
      ),
      user_roles = data.frame(user_id = character(0), role = character(0), stringsAsFactors = FALSE),
      adoption_source = NULL
    ),
    class = "pv_registry"
  )
}

test_that("pv_telemetry_config() returns one entry per subprocess with declared clicks", {
  reg <- fixture_registry_with_clicks()
  cfg <- pv_telemetry_config(reg)

  expect_length(cfg, 1)
  expect_equal(cfg[[1]]$process_id, "p1")
  expect_equal(cfg[[1]]$subprocess, "sub1")
  expect_equal(cfg[[1]]$app, "app1")
  expect_equal(cfg[[1]]$app_link, "https://a.example")
  expect_equal(nrow(cfg[[1]]$clicks), 2)
  expect_equal(
    names(cfg[[1]]$clicks),
    c("step_id", "label", "selector", "kind", "feature", "event", "weight")
  )
  expect_false("id" %in% names(cfg[[1]]$clicks)) # registry bookkeeping, not a recorder's concern
})

test_that("pv_telemetry_config() returns an empty list when no subprocess has a declared click", {
  reg <- fixture_registry_with_clicks()
  reg$clicks <- list()
  expect_equal(pv_telemetry_config(reg), list())
})

test_that("pv_telemetry_config() is valid JSON via jsonlite, one array entry per app", {
  skip_if_not_installed("jsonlite")
  reg <- fixture_registry_with_clicks()
  json <- jsonlite::toJSON(pv_telemetry_config(reg), auto_unbox = TRUE)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_length(parsed, 1)
  expect_equal(parsed[[1]]$app, "app1")
  expect_length(parsed[[1]]$clicks, 2)
})

test_that("pv_export_telemetry_config() pins the config as JSON", {
  skip_if_not_installed("pins")
  reg <- fixture_registry_with_clicks()
  board <- pins::board_folder(withr::local_tempdir())

  name <- pv_export_telemetry_config(reg, board, name = "telemetry_config")
  expect_true("telemetry_config" %in% pins::pin_list(board))

  roundtrip <- pins::pin_read(board, "telemetry_config")
  expect_length(roundtrip, 1)
  expect_equal(roundtrip[[1]]$app, "app1")
})
