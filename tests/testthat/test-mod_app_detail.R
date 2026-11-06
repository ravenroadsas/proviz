test_that("mod_app_detail_server computes KPIs and variants once an app is picked", {
  # updateSelectInput() has no real client to echo the choice back into
  # input$process_picker/step_picker/app_picker under testServer() (confirmed:
  # this is a general testServer limitation, not specific to this module) --
  # that round trip is exactly what the Playwright-driven browser run already
  # verified end-to-end. Here, set the inputs directly, as the browser would
  # after receiving the update, and test the reactive chain downstream of it.
  registry <- local_fixture_registry()
  pv_con_obj <- pv_connect_registry(registry)
  withr::defer(pv_disconnect(pv_con_obj))

  shiny::testServer(mod_app_detail_server, args = list(
    pv_con = shiny::reactiveVal(pv_con_obj), registry = shiny::reactiveVal(registry),
    selected_app = shiny::reactiveVal(NULL)
  ), {
    session$setInputs(
      process_picker = "type_curve_construction", step_picker = "draft", app_picker = "type_curve_module"
    )
    session$setInputs(date_range = c(as.Date("2026-01-01"), as.Date("2026-01-02")))

    events <- raw_events()
    expect_equal(nrow(events), 2)

    expect_equal(output$kpi_cases, "1")
    expect_equal(output$kpi_conformance, "100%")

    variants <- pv_variant_table(filtered_events(), ideal_process())
    expect_equal(variants$trace, "a -> b")

    dist <- pv_outcome_distribution(filtered_events(), ideal_process())
    expect_equal(dist$label, "B done")
    expect_equal(dist$n_cases, 1)
    expect_true(nzchar(output$outcomes_table)) # renders without error
  })
})
