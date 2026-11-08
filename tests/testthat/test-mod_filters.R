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
