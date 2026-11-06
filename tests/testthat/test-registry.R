write_registry_fixture <- function(yaml_text) {
  path <- tempfile(fileext = ".yml")
  writeLines(yaml_text, path)
  path
}

test_that("pv_load_registry parses step_variants and translates step slugs to step labels", {
  path <- write_registry_fixture('
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
  reg <- pv_load_registry(path)
  expect_equal(
    reg$step_variants$proc1,
    list(c("Draft", "Review"), c("Draft", "Review", "Draft", "Review"))
  )
})

test_that("pv_load_registry leaves step_variants absent for a process that doesn't declare it", {
  path <- write_registry_fixture('
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
  reg <- pv_load_registry(path)
  expect_null(reg$step_variants[["proc1"]])
})

test_that("pv_load_registry errors clearly when step_variants references an unknown step", {
  path <- write_registry_fixture('
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
')
  expect_error(pv_load_registry(path), "unknown step")
})
