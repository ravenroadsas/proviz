load_registry_fixture <- function(yaml_text) {
  yaml_path <- withr::local_tempfile(fileext = ".yml")
  writeLines(yaml_text, yaml_path)
  db_path <- withr::local_tempfile(fileext = ".duckdb")
  pv_registry_migrate_yaml(yaml_path, db_path)
  pv_load_registry(db_path)
}

test_that("pv_load_registry parses step_variants and translates step slugs to step labels", {
  reg <- load_registry_fixture('
- process_id: proc1
  macroprocess: mp1
  process: "Proc One"
  owner: "owner"
  status: instrumented
  loop_length: short
  step_variants:
    - [draft, review]
    - [draft, review, draft, review]
  subprocesses:
    - app: app1
      subprocess: sub1
      step: draft
      step_label: "Draft"
      step_order: 1
      duckdb_path: /tmp/x1.duckdb
    - app: app2
      subprocess: sub2
      step: review
      step_label: "Review"
      step_order: 2
      duckdb_path: /tmp/x2.duckdb
')
  expect_equal(
    reg$step_variants$proc1,
    list(c("Draft", "Review"), c("Draft", "Review", "Draft", "Review"))
  )
})

test_that("pv_load_registry leaves step_variants absent for a process that doesn't declare it", {
  reg <- load_registry_fixture('
- process_id: proc1
  macroprocess: mp1
  process: "Proc One"
  owner: "owner"
  status: instrumented
  loop_length: short
  subprocesses:
    - app: app1
      subprocess: sub1
      step: draft
      step_order: 1
      duckdb_path: /tmp/x1.duckdb
')
  expect_null(reg$step_variants[["proc1"]])
})

test_that("pv_load_registry treats an explicit empty step_variants: [] the same as omitting it", {
  # Regression test: an empty list must NOT be stored as "zero declared
  # variants" -- pv_step_sequence() would then return list(variants =
  # list()) instead of falling back to the step_order-derived sequence,
  # silently making every case's conformance unscorable (NA).
  reg <- load_registry_fixture('
- process_id: proc1
  macroprocess: mp1
  process: "Proc One"
  owner: "owner"
  status: instrumented
  loop_length: short
  step_variants: []
  subprocesses:
    - app: app1
      subprocess: sub1
      step: draft
      step_order: 1
      duckdb_path: /tmp/x1.duckdb
')
  expect_null(reg$step_variants[["proc1"]])

  seq <- pv_step_sequence(reg, "proc1")
  expect_equal(seq$variants, list("Draft")) # fell back to the step_order-derived sequence
})

test_that("pv_registry_migrate_yaml errors clearly when step_variants references an unknown step", {
  yaml_path <- withr::local_tempfile(fileext = ".yml")
  writeLines('
- process_id: proc1
  macroprocess: mp1
  process: "Proc One"
  owner: "owner"
  status: instrumented
  loop_length: short
  step_variants:
    - [draft, nonexistent_step]
  subprocesses:
    - app: app1
      subprocess: sub1
      step: draft
      step_order: 1
      duckdb_path: /tmp/x1.duckdb
', yaml_path)
  db_path <- withr::local_tempfile(fileext = ".duckdb")
  expect_error(pv_registry_migrate_yaml(yaml_path, db_path), "unknown step")
})

test_that("pv_registry_migrate_yaml + pv_load_registry round-trip an app's full ideal process", {
  dir <- withr::local_tempdir()
  yaml_path <- file.path(dir, "processes.yml")
  ideal_path <- file.path(dir, "ideal.yml")
  writeLines('
process_id: proc1
activities:
  - id: a
    label: "A"
    role: "Analyst"
    score: 2
  - id: b
    label: "B"
    role: "Analyst"
variants:
  - [a, b]
outcomes:
  - id: b_done
    label: "B done"
    kind: deliverable
    activity_id: b
outcome_exclusivity: single
', ideal_path)
  writeLines('
- process_id: proc1
  macroprocess: mp1
  process: "Proc One"
  owner: "owner"
  status: instrumented
  loop_length: short
  subprocesses:
    - app: app1
      subprocess: sub1
      step: draft
      step_order: 1
      duckdb_path: /tmp/x1.duckdb
      ideal_process: ideal.yml
', yaml_path)

  db_path <- file.path(dir, "registry.duckdb")
  pv_registry_migrate_yaml(yaml_path, db_path)
  reg <- pv_load_registry(db_path)

  ideal <- reg$ideal[["proc1::sub1"]]
  expect_s3_class(ideal, "pv_ideal_process")
  expect_equal(ideal$activities$id, c("a", "b"))
  expect_equal(ideal$activities$score, c(2, 0)) # score defaults to 0 when not declared
  expect_equal(ideal$variants, list(c("a", "b")))
  expect_equal(ideal$outcomes$id, "b_done")
  expect_equal(ideal$outcome_exclusivity, "single")
})

test_that("pv_registry_migrate_yaml resolves adoption_source relative to the registry db's own directory", {
  dir <- withr::local_tempdir()
  yaml_path <- file.path(dir, "processes.yml")
  writeLines('
- process_id: proc1
  macroprocess: mp1
  process: "Proc One"
  owner: "owner"
  status: not_started
  loop_length: short
  adoption_source: adoption.xlsx
', yaml_path)

  db_path <- file.path(dir, "sub", "registry.duckdb") # different directory than the yaml
  dir.create(dirname(db_path))
  pv_registry_migrate_yaml(yaml_path, db_path)
  reg <- pv_load_registry(db_path)

  expect_equal(reg$adoption_source, file.path(dirname(db_path), "adoption.xlsx"))
})
