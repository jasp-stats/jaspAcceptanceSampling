.ssm_expected_fit_dependencies <- function() {
  c("ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
    "ssm_predictor",
    "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
    "ssm_priorSigmaSD", "ssm_priorBetaScale",
    "ssm_advancedMcmcBurnin", "ssm_advancedMcmcChains",
    "ssm_advancedMcmcSamples", "ssm_advancedMcmcThin",
    "ssm_advancedMcmcSeed", "ssm_advancedMcmcAdaptDelta",
    "ssm_advancedMcmcMaxTreeDepth")
}

.ssm_capture_dependencies <- function(function_name) {
  # Capture the real builder's declaration before creating Rcpp outputs or fitting.
  capture <- function(keys) {
    stop(structure(list(message = "Dependencies captured", call = NULL, keys = keys),
                   class = c("ssm_dependencies_captured", "condition")))
  }
  factory <- function(...) list(dependOn = capture)
  fun <- getFromNamespace(function_name, "jaspAcceptanceSampling")
  environment(fun) <- list2env(list(
    createJaspState = factory, createJaspTable = factory, createJaspPlot = factory,
    createJaspHtml = function(..., dependencies) capture(dependencies)
  ), parent = environment(fun))
  options <- list(ssm_predictor = "x", ssm_showMcmcSummary = TRUE,
                  ssm_printDiagnostics = TRUE,
                  ssm_statePlot = TRUE, ssm_predictionPlot = TRUE,
                  ssm_posteriorDistPlot = TRUE, ssm_plotBeta = TRUE)
  results <- if (function_name == ".asSSM_reportDiagnostics")
    list(fit = list(object = list())) else list()
  arguments <- list(jaspResults = results, options = options)
  if ("dataset" %in% names(formals(fun))) arguments["dataset"] <- list(NULL)
  captured <- tryCatch(do.call(fun, arguments), ssm_dependencies_captured = base::identity)
  if (!inherits(captured, "ssm_dependencies_captured"))
    stop("The builder did not declare its dependencies: ", function_name)
  captured$keys
}

test_that("the fit depends on every data, prior and sampler setting", {
  dependencies <- getFromNamespace(".asSSM_fitDependencies", "jaspAcceptanceSampling")()
  expected <- .ssm_expected_fit_dependencies()
  expect_setequal(dependencies, expected)
  expect_length(dependencies, length(expected))
  expect_setequal(.ssm_capture_dependencies(".asSSM_fitModel"), expected)
})

test_that("every fitted output includes all model dependencies and its display controls", {
  extras <- list(
    .asSSM_createSummaryTable = c("ssm_controlLimitsLower", "ssm_controlLimitsUpper",
      "ssm_decisionAcceptThreshold", "ssm_decisionRejectThreshold"),
    .asSSM_createPredictorSummaryTable = character(),
    .asSSM_plotState = "ssm_statePlot",
    .asSSM_plotPrediction = c("ssm_predictionPlot", "ssm_controlLimitsLower", "ssm_controlLimitsUpper"),
    .asSSM_plotPosteriorDist = "ssm_posteriorDistPlot",
    .asSSM_plotBetaCoefficient = "ssm_plotBeta",
    .asSSM_createMcmcSummaryTable = c("ssm_showMcmcSummary", "ssm_showTheta",
      "ssm_showSigma", "ssm_showYpred", "ssm_showBeta"),
    .asSSM_reportDiagnostics = "ssm_printDiagnostics"
  )
  for (name in names(extras)) {
    declared <- .ssm_capture_dependencies(name)
    expected <- c(.ssm_expected_fit_dependencies(), extras[[name]])
    expect_equal(sort(declared), sort(expected), info = name)
  }
})

test_that("changing decisions or output visibility does not invalidate the fit", {
  display_controls <- c("ssm_controlLimitsLower", "ssm_controlLimitsUpper",
    "ssm_decisionAcceptThreshold", "ssm_decisionRejectThreshold",
    "ssm_statePlot", "ssm_predictionPlot", "ssm_posteriorDistPlot", "ssm_plotBeta",
    "ssm_showMcmcSummary", "ssm_showTheta", "ssm_showSigma", "ssm_showYpred", "ssm_showBeta",
    "ssm_printDiagnostics")
  declared <- .ssm_capture_dependencies(".asSSM_fitModel")
  expect_length(intersect(declared, display_controls), 0L)
})
