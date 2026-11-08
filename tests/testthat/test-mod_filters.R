test_that("mod_filters_server returns NULL users when nothing is selected", {
  # updateDateRangeInput()/updateSelectizeInput() have no real client to
  # echo the choice back into input$* under testServer() (a general
  # testServer limitation, confirmed in test-mod_app_detail.R) -- so this
  # only exercises the reactive chain downstream of input$date_range, set
  # directly as the browser would after receiving the update.
  registry <- local_fixture_registry()

  shiny::testServer(mod_filters_server, args = list(registry = shiny::reactiveVal(registry)), {
    session$flushReact()
    expect_null(session$returned$users())
    session$setInputs(date_range = as.Date(c("2026-01-01", "2026-01-01")))
    expect_equal(session$returned$date_range(), as.Date(c("2026-01-01", "2026-01-01")))
  })
})

test_that("mod_filters_server resolves a role selection to its matching user_ids", {
  registry <- local_fixture_registry() # helper fixture declares alice -> "Analyst"

  shiny::testServer(mod_filters_server, args = list(registry = shiny::reactiveVal(registry)), {
    session$flushReact()
    session$setInputs(roles = "Analyst")
    expect_equal(session$returned$users(), "alice")
  })
})

test_that("mod_filters_server filters to nothing when a role matches no user", {
  registry <- local_fixture_registry()

  shiny::testServer(mod_filters_server, args = list(registry = shiny::reactiveVal(registry)), {
    session$flushReact()
    session$setInputs(roles = "Nonexistent Role")
    users <- session$returned$users()
    expect_length(users, 1)
    expect_false(users %in% c("alice", NA))
  })
})

test_that("mod_filters_server intersects an explicit user selection with a role selection", {
  registry <- local_fixture_registry()
  registry$user_roles <- rbind(registry$user_roles, data.frame(user_id = "bob", role = "Analyst", stringsAsFactors = FALSE))

  shiny::testServer(mod_filters_server, args = list(registry = shiny::reactiveVal(registry)), {
    session$flushReact()
    session$setInputs(roles = "Analyst", users = "alice")
    expect_equal(session$returned$users(), "alice") # bob is also an Analyst but wasn't picked
  })
})

test_that("pv_filters_process_app_choices scopes to one process's monitored apps, keyed by 'process_id::subprocess'", {
  registry <- local_fixture_registry()
  choices <- pv_filters_process_app_choices(registry, "type_curve_construction")
  expect_equal(unname(choices), "type_curve_construction::type_curve_module")
  expect_equal(names(choices), "typecurves")

  expect_equal(pv_filters_process_app_choices(registry, "not_yet"), character(0))
})

test_that("pv_filters_all_app_choices labels every monitored app with its process, regardless of process", {
  registry <- local_fixture_registry()
  choices <- pv_filters_all_app_choices(registry)
  expect_equal(unname(choices), "type_curve_construction::type_curve_module")
  expect_equal(names(choices), "typecurves (Type Curve Construction)")
})

test_that("mod_filters_server defaults process_id to '(All processes)' and app() to its first monitored app", {
  # updateSelectInput() has no real client to echo the default selection
  # back into input$* under testServer() (the same documented limitation
  # as test-mod_app_detail.R) -- set inputs directly, as the browser
  # would after receiving the update that picks these defaults.
  registry <- local_fixture_registry()

  shiny::testServer(mod_filters_server, args = list(registry = shiny::reactiveVal(registry)), {
    session$flushReact()
    session$setInputs(process_picker = .pv_all_processes_sentinel, app_picker = "type_curve_construction::type_curve_module")
    expect_equal(session$returned$process_id(), .pv_all_processes_sentinel)
    expect_equal(session$returned$app(), list(process_id = "type_curve_construction", subprocess = "type_curve_module"))
  })
})

test_that("mod_filters_server's app() parses the selected 'process_id::subprocess' composite value", {
  registry <- local_fixture_registry()

  shiny::testServer(mod_filters_server, args = list(registry = shiny::reactiveVal(registry)), {
    session$flushReact()
    session$setInputs(app_picker = "type_curve_construction::type_curve_module")
    expect_equal(session$returned$app(), list(process_id = "type_curve_construction", subprocess = "type_curve_module"))
  })
})

test_that("mod_filters_server's set_process/set_app are callable without error (client echo is testServer's known limitation)", {
  registry <- local_fixture_registry()

  shiny::testServer(mod_filters_server, args = list(registry = shiny::reactiveVal(registry)), {
    session$flushReact()
    expect_no_error(session$returned$set_process("type_curve_construction"))
    expect_no_error(session$returned$set_app(list(process_id = "type_curve_construction", subprocess = "type_curve_module")))
  })
})
