local_fixture_db <- function(env = parent.frame()) {
  db_path <- withr::local_tempfile(fileext = ".duckdb", .local_envir = env)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  pv_registry_db_create_schema(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  db_path
}

test_that("pv_registry_db_insert_process + pv_registry_db_insert_subprocess round-trip through pv_load_registry", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "designed", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "appA", "subA", "draft", step_order = 1)
  pv_registry_db_insert_subprocess(db_path, "p1", "appB", "subB", "draft", step_order = 1)

  reg <- pv_load_registry(db_path)
  expect_equal(reg$processes$process_id, "p1")
  expect_equal(reg$subprocesses$app, c("appA", "appB"))
  expect_equal(reg$subprocesses$app_order, c(1L, 2L)) # auto-incremented within the shared step
})

test_that("pv_registry_db_insert_subprocess defaults step_label from the step slug", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "designed", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "appA", "subA", "draft_review", step_order = 1)

  reg <- pv_load_registry(db_path)
  expect_equal(reg$subprocesses$step_label, "Draft Review")
})

test_that("pv_registry_db_upsert_activity creates the ideal_processes meta row and is idempotent", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "designed", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "appA", "subA", "draft", step_order = 1)

  pv_registry_db_upsert_activity(db_path, "p1", "subA", "a1", "Do thing", "Analyst",
    score = 2, selector = "#foo", kind = "click", feature = "f1", event = "e1", weight = 1
  )
  reg <- pv_load_registry(db_path)
  ideal <- reg$ideal[["p1::subA"]]
  expect_s3_class(ideal, "pv_ideal_process")
  expect_equal(ideal$activities$label, "Do thing")
  expect_equal(ideal$outcome_exclusivity, "single")

  # re-saving the same activity_id updates in place rather than erroring
  pv_registry_db_upsert_activity(db_path, "p1", "subA", "a1", "Do thing v2", "Analyst")
  reg2 <- pv_load_registry(db_path)
  expect_equal(reg2$ideal[["p1::subA"]]$activities$label, "Do thing v2")
  expect_equal(nrow(reg2$ideal[["p1::subA"]]$activities), 1)
})

test_that("pv_registry_db_insert_variant numbers successive variants and validates activity ids", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "designed", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "appA", "subA", "draft", step_order = 1)
  pv_registry_db_upsert_activity(db_path, "p1", "subA", "a1", "A", "Analyst")
  pv_registry_db_upsert_activity(db_path, "p1", "subA", "a2", "B", "Analyst")

  pv_registry_db_insert_variant(db_path, "p1", "subA", c("a1", "a2"))
  pv_registry_db_insert_variant(db_path, "p1", "subA", c("a1"))

  reg <- pv_load_registry(db_path)
  expect_equal(reg$ideal[["p1::subA"]]$variants, list(c("a1", "a2"), "a1"))

  expect_error(
    pv_registry_db_insert_variant(db_path, "p1", "subA", c("a1", "nope")),
    "unknown activity id"
  )
})

test_that("pv_registry_db_upsert_outcome is idempotent on (process_id, subprocess, outcome_id)", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "designed", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "appA", "subA", "draft", step_order = 1)
  pv_registry_db_upsert_activity(db_path, "p1", "subA", "a1", "A", "Analyst")

  pv_registry_db_upsert_outcome(db_path, "p1", "subA", "o1", "Done", "deliverable", "a1")
  pv_registry_db_upsert_outcome(db_path, "p1", "subA", "o1", "Done v2", "deliverable", "a1")

  reg <- pv_load_registry(db_path)
  oc <- reg$ideal[["p1::subA"]]$outcomes
  expect_equal(nrow(oc), 1)
  expect_equal(oc$label, "Done v2")
})

test_that("pv_registry_db_insert_step_variant numbers successive variants and validates steps", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "designed", "short")
  pv_registry_db_insert_subprocess(db_path, "p1", "appA", "subA", "draft", step_order = 1)
  pv_registry_db_insert_subprocess(db_path, "p1", "appB", "subB", "review", step_order = 2)

  pv_registry_db_insert_step_variant(db_path, "p1", c("draft", "review"))
  pv_registry_db_insert_step_variant(db_path, "p1", c("draft", "review", "draft", "review"))

  reg <- pv_load_registry(db_path)
  expect_equal(
    reg$step_variants$p1,
    list(c("Draft", "Review"), c("Draft", "Review", "Draft", "Review"))
  )

  expect_error(
    pv_registry_db_insert_step_variant(db_path, "p1", c("draft", "nonexistent")),
    "unknown step"
  )
})

test_that("pv_registry_db_set_adoption_source upserts the settings row", {
  db_path <- local_fixture_db()
  pv_registry_db_insert_process(db_path, "p1", "mp1", "Proc One", "owner", "not_started", "short")

  pv_registry_db_set_adoption_source(db_path, "adoption.xlsx")
  expect_equal(pv_load_registry(db_path)$adoption_source, file.path(dirname(db_path), "adoption.xlsx"))

  pv_registry_db_set_adoption_source(db_path, "adoption_v2.xlsx")
  expect_equal(pv_load_registry(db_path)$adoption_source, file.path(dirname(db_path), "adoption_v2.xlsx"))
})
