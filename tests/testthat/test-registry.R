local_fixture_db <- function(env = parent.frame()) {
  db_path <- withr::local_tempfile(fileext = ".duckdb", .local_envir = env)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  pv_registry_db_create_schema(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  db_path
}

test_that("pv_load_registry reads processes and subprocesses", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "designed", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "subA", "appA", app_link = "https://a.example", adoption_threshold = 10)
  pv_registry_db_insert_subprocess(db_path, "p1", "subB", "appB")

  reg <- pv_load_registry(db_path)
  expect_s3_class(reg, "pv_registry")
  expect_equal(reg$processes$process_id, "p1")
  expect_equal(reg$subprocesses$subprocess, c("subA", "subB"))
  expect_equal(reg$subprocesses$app_link[reg$subprocesses$subprocess == "subA"], "https://a.example")
  expect_true(is.na(reg$subprocesses$app_link[reg$subprocesses$subprocess == "subB"]))
})

test_that("pv_load_registry errors clearly when the registry has no processes", {
  db_path <- local_fixture_db()
  expect_error(pv_load_registry(db_path), "declares no processes")
})

test_that("pv_load_registry warns when an instrumented process declares no subprocesses", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "instrumented", "short")
  expect_warning(pv_load_registry(db_path), "declares no subprocesses")
})

test_that("pv_load_registry reads a process-level diagram (edges + outcomes)", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "instrumented", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "subA", "appA")
  pv_registry_db_insert_subprocess(db_path, "p1", "subB", "appB")
  pv_registry_db_insert_process_edge(db_path, "p1", "subA", "subB")
  pv_registry_db_insert_process_edge(db_path, "p1", "subB", "subA") # a cycle is allowed
  pv_registry_db_upsert_process_outcome(db_path, "p1", "done", "Done", "deliverable", "subB")

  reg <- pv_load_registry(db_path)
  diagram <- reg$process_diagrams[["p1"]]
  expect_equal(nrow(diagram$edges), 2)
  expect_equal(diagram$outcomes$id, "done")
  expect_equal(diagram$outcomes$subprocess, "subB")

  expect_error(
    pv_registry_db_insert_process_edge(db_path, "p1", "subA", "nope"),
    "unknown subprocess"
  )
})

test_that("pv_load_registry reads a subprocess-level diagram (steps + edges + outcomes)", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "instrumented", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "subA", "appA")
  pv_registry_db_insert_step(db_path, "p1", "subA", "a")
  pv_registry_db_insert_step(db_path, "p1", "subA", "b")
  pv_registry_db_insert_step_edge(db_path, "p1", "subA", "a", "b")
  pv_registry_db_insert_step_edge(db_path, "p1", "subA", "b", "a") # a cycle is allowed
  pv_registry_db_upsert_subprocess_outcome(db_path, "p1", "subA", "b_done", "B done", "deliverable", "b")

  reg <- pv_load_registry(db_path)
  diagram <- reg$subprocess_diagrams[["p1::subA"]]
  expect_equal(diagram$steps$id, c("a", "b"))
  expect_equal(diagram$steps$label, c("A", "B")) # defaulted from the step slug
  expect_equal(nrow(diagram$edges), 2)
  expect_equal(diagram$outcomes$id, "b_done")

  expect_error(
    pv_registry_db_insert_step_edge(db_path, "p1", "subA", "a", "nope"),
    "unknown step"
  )
})

test_that("pv_load_registry reads a subprocess's click map", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "instrumented", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "subA", "appA")
  pv_registry_db_insert_step(db_path, "p1", "subA", "a")
  pv_registry_db_upsert_click(db_path, "p1", "subA", "click1", "a", selector = "#foo", kind = "click", weight = 2)
  pv_registry_db_upsert_click(db_path, "p1", "subA", "click2", "a", selector = "#bar", kind = "click", weight = 1)

  reg <- pv_load_registry(db_path)
  clicks <- reg$clicks[["p1::subA"]]
  expect_equal(nrow(clicks), 2) # many clicks -> one step
  expect_setequal(clicks$selector, c("#foo", "#bar"))

  expect_error(
    pv_registry_db_upsert_click(db_path, "p1", "subA", "click3", "nonexistent_step"),
    "unknown step"
  )
})

test_that("pv_load_registry resolves adoption_source relative to the registry db's own directory", {
  dir <- withr::local_tempdir()
  db_path <- file.path(dir, "sub", "registry.duckdb")
  dir.create(dirname(db_path))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  pv_registry_db_create_schema(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "not_started", "short")
  pv_registry_db_set_adoption_source(db_path, "adoption.xlsx")

  reg <- pv_load_registry(db_path)
  expect_equal(reg$adoption_source, file.path(dirname(db_path), "adoption.xlsx"))
})

test_that("pv_load_registry returns NULL adoption_source when none is declared", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "not_started", "short")
  expect_null(pv_load_registry(db_path)$adoption_source)
})

test_that("pv_load_registry reads declared user roles", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "not_started", "short")
  pv_registry_db_upsert_user_role(db_path, "alice", "Analyst")
  pv_registry_db_upsert_user_role(db_path, "bob", "Manager")
  pv_registry_db_upsert_user_role(db_path, "alice", "Senior Analyst") # idempotent update

  reg <- pv_load_registry(db_path)
  expect_equal(reg$user_roles$user_id, c("alice", "bob"))
  expect_equal(reg$user_roles$role, c("Senior Analyst", "Manager"))
})

test_that("pv_load_registry returns zero-row user_roles when none is declared", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "not_started", "short")
  expect_equal(nrow(pv_load_registry(db_path)$user_roles), 0)
})
