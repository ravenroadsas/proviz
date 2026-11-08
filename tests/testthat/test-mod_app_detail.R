test_that("mod_app_detail_server computes KPIs and variants once an app is picked", {
  registry <- local_fixture_registry()

  shiny::testServer(mod_app_detail_server, args = list(
    registry = shiny::reactiveVal(registry),
    date_range = shiny::reactiveVal(as.Date(c("2026-01-01", "2026-01-02"))),
    app = shiny::reactiveVal(list(process_id = "type_curve_construction", subprocess = "type_curve_module"))
  ), {
    session$flushReact()
    log <- raw_log()
    expect_equal(nrow(log), 2)

    expect_equal(output$kpi_cases, "1")
    expect_equal(output$kpi_conformance, "100%")

    variants <- pv_variant_table(filtered_log(), diagram())
    expect_equal(variants$trace, "a -> b")

    dist <- pv_outcome_distribution(pv_session_step_sequences(filtered_log()), diagram()$outcomes, "step_id")
    expect_equal(dist$label, "B done")
    expect_equal(dist$n_cases, 1)
    expect_true(nzchar(output$outcomes_table)) # renders without error
  })
})

test_that("mod_app_detail_server shows its no-data note when no app is monitored yet", {
  registry <- local_fixture_registry()

  shiny::testServer(mod_app_detail_server, args = list(
    registry = shiny::reactiveVal(registry), app = shiny::reactiveVal(NULL)
  ), {
    session$flushReact()
    expect_true(any(grepl("No app is monitored yet", as.character(output$no_data_note), fixed = TRUE)))
  })
})
