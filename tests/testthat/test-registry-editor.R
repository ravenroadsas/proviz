local_fixture_db <- function(env = parent.frame()) {
  db_path <- withr::local_tempfile(fileext = ".duckdb", .local_envir = env)
  con <- pv_registry_db_connect(db_path)
  pv_registry_db_create_schema(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  db_path
}

test_that("pv_registry_db_insert_subprocess defaults optional fields to NA", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "not_started", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "subA", "appA")

  reg <- pv_load_registry(db_path)
  subs <- reg$subprocesses
  expect_true(is.na(subs$app_link))
  expect_true(is.na(subs$log_path))
  expect_true(is.na(subs$adoption_threshold))
})

test_that("pv_registry_db_insert_process errors on a duplicate process_id", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "not_started", "short")
  expect_error(
    pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One Again", "owner", "not_started", "short")
  )
})

test_that("pv_registry_db_insert_process_edge is idempotent on the same edge", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "instrumented", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "subA", "appA")
  pv_registry_db_insert_subprocess(db_path, "p1", "subB", "appB")

  pv_registry_db_insert_process_edge(db_path, "p1", "subA", "subB")
  pv_registry_db_insert_process_edge(db_path, "p1", "subA", "subB") # no error, no duplicate row

  reg <- pv_load_registry(db_path)
  expect_equal(nrow(reg$process_diagrams[["p1"]]$edges), 1)
})

test_that("pv_registry_db_upsert_process_outcome updates in place rather than erroring on re-save", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "instrumented", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "subA", "appA")

  pv_registry_db_upsert_process_outcome(db_path, "p1", "o1", "Done", "deliverable", "subA")
  pv_registry_db_upsert_process_outcome(db_path, "p1", "o1", "Done v2", "deliverable", "subA")

  reg <- pv_load_registry(db_path)
  outcomes <- reg$process_diagrams[["p1"]]$outcomes
  expect_equal(nrow(outcomes), 1)
  expect_equal(outcomes$label, "Done v2")
})

test_that("pv_registry_db_insert_step is idempotent and lets a human override the default label", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "instrumented", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "subA", "appA")

  pv_registry_db_insert_step(db_path, "p1", "subA", "my_step")
  pv_registry_db_insert_step(db_path, "p1", "subA", "my_step", label = "Custom Label")

  reg <- pv_load_registry(db_path)
  steps <- reg$subprocess_diagrams[["p1::subA"]]$steps
  expect_equal(nrow(steps), 1)
  expect_equal(steps$label, "Custom Label")
})

test_that("pv_registry_db_upsert_click updates in place and keeps many-to-one with a step", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "instrumented", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "subA", "appA")
  pv_registry_db_insert_step(db_path, "p1", "subA", "a")

  pv_registry_db_upsert_click(db_path, "p1", "subA", "c1", "a", weight = 1)
  pv_registry_db_upsert_click(db_path, "p1", "subA", "c1", "a", weight = 5) # re-save updates in place
  pv_registry_db_upsert_click(db_path, "p1", "subA", "c2", "a", weight = 2) # a second click, same step

  reg <- pv_load_registry(db_path)
  clicks <- reg$clicks[["p1::subA"]]
  expect_equal(nrow(clicks), 2)
  expect_equal(clicks$weight[clicks$id == "c1"], 5)
  expect_true(all(clicks$step_id == "a"))
})

test_that("pv_registry_db_set_adoption_source upserts the settings row", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "not_started", "short")

  pv_registry_db_set_adoption_source(db_path, "adoption.xlsx")
  expect_equal(pv_load_registry(db_path)$adoption_source, file.path(dirname(db_path), "adoption.xlsx"))

  pv_registry_db_set_adoption_source(db_path, "adoption_v2.xlsx")
  expect_equal(pv_load_registry(db_path)$adoption_source, file.path(dirname(db_path), "adoption_v2.xlsx"))
})

test_that("pv_registry_db_connect() pins a widely-readable on-disk storage version", {
  # Regression test for a real failure: a registry checkpointed by this
  # package's own (newer) DuckDB build could fail to open at all in an
  # older install ("Trying to read a database file with version number
  # 999, but we can only read versions between 64 and 68"). DuckDB's
  # file header is 8 checksum bytes, then the 4-byte "DUCK" magic, then
  # an 8-byte little-endian storage version number -- 64 is what
  # storage_compatibility_version = "v0.10.2" (see pv_registry_db.R)
  # is documented to write.
  db_path <- local_fixture_db()
  header <- readBin(db_path, "raw", n = 20)
  expect_equal(readBin(header[13:20], "integer", n = 1, size = 8), 64)
})
