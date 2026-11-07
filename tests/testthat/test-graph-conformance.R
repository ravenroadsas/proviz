test_that("pv_graph_conformance scores a perfectly conforming trace 1", {
  edges <- data.frame(from_step = c("a", "b"), to_step = c("b", "c"), stringsAsFactors = FALSE)
  res <- pv_graph_conformance(c("a", "b", "c"), edges)
  expect_equal(res$score, 1)
  expect_equal(res$n_deviations, 0L)
  expect_equal(res$n_transitions, 2L)
})

test_that("pv_graph_conformance counts each undeclared transition as a deviation", {
  edges <- data.frame(from_step = c("a", "b"), to_step = c("b", "c"), stringsAsFactors = FALSE)
  res <- pv_graph_conformance(c("a", "x", "c"), edges)
  expect_equal(res$n_transitions, 2L)
  expect_equal(res$n_deviations, 2L) # a->x and x->c are both undeclared
  expect_equal(res$score, 0)
})

test_that("pv_graph_conformance allows cycles (a back-edge is just a declared edge)", {
  edges <- data.frame(from_step = c("a", "b", "b"), to_step = c("b", "c", "a"), stringsAsFactors = FALSE)
  res <- pv_graph_conformance(c("a", "b", "a", "b", "c"), edges)
  expect_equal(res$score, 1) # a->b, b->a, a->b, b->c all declared
})

test_that("pv_graph_conformance treats a 0- or 1-node trace as vacuously conformant", {
  edges <- data.frame(from_step = "a", to_step = "b", stringsAsFactors = FALSE)
  expect_equal(pv_graph_conformance(character(0), edges)$score, 1)
  expect_equal(pv_graph_conformance("a", edges)$score, 1)
})

test_that("pv_graph_conformance scores every transition as a deviation when no edges are declared", {
  res <- pv_graph_conformance(c("a", "b"), NULL)
  expect_equal(res$score, 0)
  expect_equal(res$n_deviations, 1L)
})

test_that("pv_graph_conformance respects custom from/to column names", {
  edges <- data.frame(from_subprocess = "a", to_subprocess = "b", stringsAsFactors = FALSE)
  res <- pv_graph_conformance(c("a", "b"), edges, "from_subprocess", "to_subprocess")
  expect_equal(res$score, 1)
})

test_that("pv_diagram_conformance scores every case in a named list of traces", {
  edges <- data.frame(from_step = c("a", "b"), to_step = c("b", "c"), stringsAsFactors = FALSE)
  traces <- list(s1 = c("a", "b", "c"), s2 = c("a", "x"))
  out <- pv_diagram_conformance(traces, edges)
  out <- out[order(out$case_id), ]
  expect_equal(out$case_id, c("s1", "s2"))
  expect_equal(out$score, c(1, 0))
  expect_equal(out$n_nodes, c(3L, 2L))
})

test_that("pv_diagram_conformance returns the empty schema for zero traces", {
  out <- pv_diagram_conformance(list(), NULL)
  expect_equal(nrow(out), 0)
})
