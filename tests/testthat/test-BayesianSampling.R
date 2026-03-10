# Tests for Bayesian Sampling

test_that("Bayesian sampling - planning table is generated", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$showPlansplan <- TRUE
  options$priorPlotplan <- FALSE
  options$showThreeBFplan <- FALSE
  options$inferPosteriorinfer <- FALSE

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  plan_table <- results[["results"]][["planContainer"]][["collection"]][["planContainer_planTable"]][["data"]]

  expect_true(length(plan_table) >= 3)

  expect_equal(plan_table[[1]]$c, 0)
  expect_equal(plan_table[[1]]$n, 19)
  expect_equal(round(plan_table[[1]]$prior_p, 4), 0.5)
  expect_equal(round(plan_table[[1]]$post_p, 4), 0.9708)
  expect_equal(round(plan_table[[1]]$bf_odds, 4), 33.2465)

  expect_equal(plan_table[[2]]$c, 1)
  expect_equal(plan_table[[2]]$n, 30)
  expect_equal(round(plan_table[[2]]$prior_p, 4), 0.5)
  expect_equal(round(plan_table[[2]]$post_p, 4), 0.9690)
  expect_equal(round(plan_table[[2]]$bf_odds, 4), 31.2231)

  expect_equal(plan_table[[3]]$c, 2)
  expect_equal(plan_table[[3]]$n, 40)
  expect_equal(round(plan_table[[3]]$prior_p, 4), 0.5)
  expect_equal(round(plan_table[[3]]$post_p, 4), 0.9678)
  expect_equal(round(plan_table[[3]]$bf_odds, 4), 30.0741)
})


test_that("Bayesian sampling - inference decision table is generated", {
  options <- jaspTools::analysisOptions("BayesianSampling")

  options$showPlansplan <- FALSE
  options$priorPlotplan <- FALSE
  options$showThreeBFplan <- FALSE

  options$inferPosteriorinfer <- TRUE
  options$showInferenceTableinfer <- TRUE
  options$posteriorPlotinfer <- FALSE
  options$ppdPlotinfer <- FALSE

  results <- jaspTools::runAnalysis("BayesianSampling", "test.csv", options)

  inf_table <- results[["results"]][["infContainer"]][["collection"]][["infContainer_inferenceDecisionTable"]][["data"]]

  expect_equal(length(inf_table), 8)
  jaspTools::expect_equal_tables(inf_table[[1]], list("Sample Size (n)", "30"))
  jaspTools::expect_equal_tables(inf_table[[2]], list("Defects (d)", "0"))
  expect_equal(inf_table[[3]]$col_1, "AQL")
  expect_equal(as.numeric(inf_table[[3]]$col_2), 0.05)
  expect_equal(inf_table[[4]]$col_1, "RQL")
  expect_equal(as.numeric(inf_table[[4]]$col_2), 0.15)
  expect_equal(inf_table[[5]]$col_1, "Bayes Factor")
  expect_equal(as.numeric(inf_table[[5]]$col_2), 188.832, tolerance = 1e-6)
  expect_equal(as.numeric(inf_table[[6]]$col_2), 0.889, tolerance = 1e-6)
  expect_equal(as.numeric(inf_table[[7]]$col_2), 0.110, tolerance = 1e-6)
  expect_equal(as.numeric(inf_table[[8]]$col_2), 0.001, tolerance = 1e-6)
})
