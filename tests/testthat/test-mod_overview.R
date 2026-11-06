test_that("mod_overview_server computes portfolio KPIs and lets a row selection drive selected_process", {
  registry <- local_fixture_registry()
  pv_con_obj <- pv_connect_registry(registry)
  withr::defer(pv_disconnect(pv_con_obj))

  shiny::testServer(mod_overview_server, args = list(
    pv_con = shiny::reactiveVal(pv_con_obj), registry = shiny::reactiveVal(registry)
  ), {
    session$flushReact()
    expect_equal(output$kpi_processes, "2")
    expect_equal(output$kpi_instrumented, "1")
    expect_equal(output$kpi_steps, "1 / 1")
    expect_equal(output$kpi_cases, "1")
    expect_equal(output$kpi_users, "1")

    table <- display_table()
    expect_true("type_curve_construction" %in% table$process_id)

    row <- which(table$process_id == "type_curve_construction")
    session$setInputs(registry_table_rows_selected = row)
    session$setInputs(open_detail = 1)
    expect_equal(session$returned$selected_process(), "type_curve_construction")
  })
})
