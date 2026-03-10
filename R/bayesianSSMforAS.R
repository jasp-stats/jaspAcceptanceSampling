
# Copyright (C) 2013-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

BayesianSSMforAS <- function(jaspResults, dataset, options) {
  bayesianSSMforASInternal(jaspResults, dataset, options)
}

# Backward-compatible alias.
bayesianSSMforAS <- BayesianSSMforAS

bayesianSSMforASInternal <- function(jaspResults, dataset, options) {

  dataset <- .asSSM_readData(dataset, options)

  ready <- .asSSMReady(options, dataset)

  jaspResults[["placeholder"]] <- NULL

  if (ready)
    .asSSM_fitModel(jaspResults, dataset, options)

  .asSSM_createSummaryTable(jaspResults, dataset, options)
  .asSSM_createMcmcSummaryTable(jaspResults, dataset, options)

  if (ready && options[["ssm_statePlot"]])
    .asSSM_plotState(jaspResults, dataset, options)

  if (ready && options[["ssm_posteriorDistPlot"]])
    .asSSM_plotPosteriorDist(jaspResults, dataset, options)

  if (ready && options[["ssm_predictionPlot"]])
    .asSSM_plotPrediction(jaspResults, dataset, options)

  if (ready && options[["ssm_plotBeta"]])
    .asSSM_plotBetaCoefficient(jaspResults, dataset, options)
}

.asSSM_runtimeFitCache <- new.env(parent = emptyenv())

.asSSM_colSignature <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L)
    return("empty")
  paste(
    length(x),
    signif(sum(x), 10),
    signif(sum(x * x), 10),
    signif(min(x), 10),
    signif(max(x), 10),
    sep = ":"
  )
}

.asSSM_fitCacheKey <- function(dataset, options) {
  parts <- c(
    paste0("count=", options[["ssm_count"]]),
    paste0("sampleSize=", options[["ssm_sampleSize"]]),
    paste0("total=", options[["ssm_total"]]),
    paste0("time=", options[["ssm_time"]]),
    paste0("model=", options[["ssm_scaleModel"]]),
    paste0("predictor=", options[["ssm_predictor"]]),
    paste0("priorTheta1a=", options[["ssm_priorTheta1Shape1"]]),
    paste0("priorTheta1b=", options[["ssm_priorTheta1Shape2"]]),
    paste0("priorSigmaSD=", options[["ssm_priorSigmaSD"]]),
    paste0("priorBetaScale=", options[["ssm_priorBetaScale"]]),
    paste0("burnin=", options[["ssm_advancedMcmcBurnin"]]),
    paste0("samples=", options[["ssm_advancedMcmcSamples"]]),
    paste0("chains=", options[["ssm_advancedMcmcChains"]]),
    paste0("thin=", options[["ssm_advancedMcmcThin"]]),
    paste0("seed=", options[["ssm_advancedMcmcSeed"]]),
    paste0("adaptDelta=", options[["ssm_advancedMcmcAdaptDelta"]]),
    paste0("treeDepth=", options[["ssm_advancedMcmcMaxTreeDepth"]]),
    paste0("nRows=", nrow(dataset))
  )

  cols <- c(options[["ssm_count"]], options[["ssm_sampleSize"]], options[["ssm_total"]], options[["ssm_time"]])
  if (options[["ssm_scaleModel"]] == "WithPredictor" && options[["ssm_predictor"]] != "")
    cols <- c(cols, options[["ssm_predictor"]])
  cols <- unique(cols[nzchar(cols)])

  for (col in cols) {
    if (col %in% names(dataset))
      parts <- c(parts, paste0("sig_", col, "=", .asSSM_colSignature(dataset[[col]])))
  }

  paste(parts, collapse = "|")
}

.asSSMReady <- function(options, dataset) {
  if (options[["ssm_count"]] == "" || options[["ssm_total"]] == "" ||
      options[["ssm_sampleSize"]] == "" || options[["ssm_time"]] == "") return(FALSE)
  if (options[["ssm_scaleModel"]] == "WithPredictor" && options[["ssm_predictor"]] == "") return(FALSE)
  return(TRUE)
}

.asSSM_readData <- function(dataset, options) {

  if (options[["ssm_count"]] == "" ||
      options[["ssm_total"]] == "" ||
      options[["ssm_sampleSize"]] == "" ||
      options[["ssm_time"]] == "") {
    return(dataset)
  }

  colsNumeric <- c()
  if (options[["ssm_count"]]      != "") colsNumeric <- c(colsNumeric, options[["ssm_count"]])
  if (options[["ssm_total"]]      != "") colsNumeric <- c(colsNumeric, options[["ssm_total"]])
  if (options[["ssm_sampleSize"]] != "") colsNumeric <- c(colsNumeric, options[["ssm_sampleSize"]])
  if (options[["ssm_time"]]       != "") colsNumeric <- c(colsNumeric, options[["ssm_time"]])
  if (options[["ssm_scaleModel"]] == "WithPredictor" && options[["ssm_predictor"]] != "")
    colsNumeric <- c(colsNumeric, options[["ssm_predictor"]])

  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = colsNumeric)

    if (options[["ssm_time"]] != "" && options[["ssm_time"]] %in% colnames(dataset)) {
      dataset <- dataset[order(dataset[[options[["ssm_time"]]]]), ]
    }
  }

  return(dataset)
}

.asSSM_fitModel <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["fit"]])) {
    fitContainer <- createJaspState()
    fitContainer$dependOn(c(
      "ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
      "ssm_scaleModel", "ssm_predictor",
      "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
      "ssm_priorSigmaSD", "ssm_priorBetaScale",
      "ssm_advancedMcmcBurnin",
      "ssm_advancedMcmcChains", "ssm_advancedMcmcSamples",
      "ssm_advancedMcmcThin", "ssm_advancedMcmcSeed",
      "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
    ))
    jaspResults[["fit"]] <- fitContainer
  }

  if (!is.null(jaspResults[["fit"]]$object)) return()

  cacheKey <- .asSSM_fitCacheKey(dataset, options)
  if (exists(cacheKey, envir = .asSSM_runtimeFitCache, inherits = FALSE)) {
    jaspResults[["fit"]]$object <- get(cacheKey, envir = .asSSM_runtimeFitCache, inherits = FALSE)
    return()
  }

  stanData <- list(
    T = nrow(dataset),
    n = as.integer(dataset[[options[["ssm_sampleSize"]]]]),  # tested items
    s = as.integer(dataset[[options[["ssm_total"]]]]),       # lot size
    y = as.integer(dataset[[options[["ssm_count"]]]]),       # defects in sample
    prior_theta1_a = options[["ssm_priorTheta1Shape1"]],
    prior_theta1_b = options[["ssm_priorTheta1Shape2"]],
    prior_sigma_sd = options[["ssm_priorSigmaSD"]]
  )

  usePredictor <- options[["ssm_scaleModel"]] == "WithPredictor" && options[["ssm_predictor"]] != ""

  if (usePredictor) {
    stanData$temperature      <- as.numeric(scale(dataset[[options[["ssm_predictor"]]]]))
    stanData$prior_beta_scale <- options[["ssm_priorBetaScale"]]
    model_obj <- jaspAcceptanceSampling:::.asSSM_getStanModel("complex")
  } else {
    model_obj <- jaspAcceptanceSampling:::.asSSM_getStanModel("simple")
  }

  thisSeed  <- options[["ssm_advancedMcmcSeed"]]
  thisDelta <- options[["ssm_advancedMcmcAdaptDelta"]]
  thisDepth <- options[["ssm_advancedMcmcMaxTreeDepth"]]
  thisChains <- as.integer(options[["ssm_advancedMcmcChains"]])

  availableCores <- suppressWarnings(parallel::detectCores())
  if (!is.finite(availableCores) || availableCores < 1)
    availableCores <- 1L
  thisCores <- min(thisChains, as.integer(availableCores))

  controlList <- list()
  if (is.numeric(thisDelta) && length(thisDelta) == 1L &&
      is.finite(thisDelta) && thisDelta > 0 && thisDelta < 1)
    controlList$adapt_delta <- thisDelta

  if (is.numeric(thisDepth) && length(thisDepth) == 1L &&
      is.finite(thisDepth) && thisDepth > 0)
    controlList$max_treedepth <- as.integer(thisDepth)

  jaspBase::startProgressbar(1, gettext("Sampling..."))

  tryCatch({
    stanFit <- rstan::sampling(
      object     = model_obj,
      data       = stanData,
      chains     = thisChains,
      iter       = options[["ssm_advancedMcmcBurnin"]] + options[["ssm_advancedMcmcSamples"]],
      warmup     = options[["ssm_advancedMcmcBurnin"]],
      thin       = options[["ssm_advancedMcmcThin"]],
      cores      = thisCores,
      refresh    = 0,
      seed       = thisSeed,
      control    = controlList
    )

    samples <- rstan::extract(stanFit)
    summary <- rstan::summary(stanFit)$summary

    jaspResults[["fit"]]$object <- list(
      samples = samples,
      summary = summary
    )
    assign(cacheKey, jaspResults[["fit"]]$object, envir = .asSSM_runtimeFitCache)

  }, error = function(e) {
    msg <- gettextf("Estimation failed: %s", e$message)
    fitObj <- jaspResults[["fit"]]
    if (!is.null(fitObj) && is.function(fitObj$setError)) {
      fitObj$setError(msg)
      return()
    }
    stop(msg, call. = FALSE)
  })

  jaspBase::progressbarTick()
}


.asSSM_createSummaryTable <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["summaryTable"]])) return()

  table <- createJaspTable(title = gettext("Posterior Summary (Current Lot)"))
  table$dependOn(c(
    "ssm_postHocCorrection", "ssm_postHocCorrectionWeight",
    "ssm_controlLimitsLower", "ssm_controlLimitsUpper",
    "ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
    "ssm_scaleModel", "ssm_predictor",
    "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
    "ssm_priorSigmaSD", "ssm_priorBetaScale",
    "ssm_advancedMcmcBurnin", "ssm_advancedMcmcSamples",
    "ssm_advancedMcmcChains", "ssm_advancedMcmcThin",
    "ssm_advancedMcmcSeed",
    "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth",
    "ssm_decisionAcceptThreshold", "ssm_decisionRejectThreshold"
  ))
  table$position <- 0

  table$addColumnInfo(name = "t",       title = gettext("Lot Number"),                          type = "integer")
  table$addColumnInfo(name = "y",       title = gettext("Observed Defects"),                     type = "integer")
  table$addColumnInfo(name = "pred",    title = gettext("Median Predicted Defects"),             type = "number")
  table$addColumnInfo(name = "massAQL", title = gettext("P(Defects < AQL Limit)"),               type = "number", format = "dp:3")
  table$addColumnInfo(name = "massRQL", title = gettext("P(Defects > RQL Limit)"),               type = "number", format = "dp:3")
  table$addColumnInfo(name = "decision", title = gettext("Decision"),                            type = "string")

  jaspResults[["summaryTable"]] <- table

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples

  T_idx <- nrow(dataset)
  sT <- dataset[[options[["ssm_total"]]]][T_idx]        # total lot size
  nT <- dataset[[options[["ssm_sampleSize"]]]][T_idx]   # sample size
  yT <- dataset[[options[["ssm_count"]]]][T_idx]

  if (is.na(sT) || is.na(yT)) {
    table$setError("The Lot Size or Defect Count is missing (NA) for the last lot.")
    return()
  }

  y_pred <- samples$y_pred

  if (options[["ssm_postHocCorrection"]]) {
    y_pred <- .asSSM_applyAdHocCorrection(y_pred, sT, options[["ssm_postHocCorrectionWeight"]])
  }

  limit_AQL <- options[["ssm_controlLimitsLower"]] * sT
  limit_RQL <- options[["ssm_controlLimitsUpper"]] * sT

  mass_below_AQL <- mean(y_pred < limit_AQL, na.rm = TRUE)
  mass_above_RQL <- mean(y_pred > limit_RQL, na.rm = TRUE)

  acceptThreshold <- options[["ssm_decisionAcceptThreshold"]]
  rejectThreshold <- options[["ssm_decisionRejectThreshold"]]

  decision <- "Continue"
  if (!is.na(mass_above_RQL) && mass_above_RQL < acceptThreshold) {
    decision <- "Accept"
  } else if (!is.na(mass_above_RQL) && !is.na(mass_below_AQL) &&
             mass_above_RQL > acceptThreshold && mass_below_AQL < rejectThreshold) {
    decision <- "Reject"
  }

  table$addRows(list(
    t = T_idx,
    y = yT,
    pred = median(y_pred, na.rm = TRUE),
    massAQL = mass_below_AQL,
    massRQL = mass_above_RQL,
    decision = decision
  ))
}

.asSSM_plotState <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["ssm_statePlot"]])) return()

  plot <- createJaspPlot(title = gettext("Defect Rate Over Time"), width = 600, height = 400)
  plot$dependOn(c(
    "ssm_statePlot",
    "ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
    "ssm_scaleModel", "ssm_predictor",
    "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
    "ssm_priorSigmaSD", "ssm_priorBetaScale",
    "ssm_advancedMcmcSamples", "ssm_advancedMcmcSeed",
    "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
  ))
  plot$position <- 1
  jaspResults[["ssm_statePlot"]] <- plot

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  theta <- samples$theta * 100

  df <- data.frame(
    time  = 1:ncol(theta),
    mean  = colMeans(theta),
    lower = apply(theta, 2, quantile, 0.025),
    upper = apply(theta, 2, quantile, 0.975)
  )

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$time)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(df$lower, df$upper))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         fill = "#c2c2c2", alpha = 0.6) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      x = gettext("Lot"),
      y = gettext("Defect Rate (%)")
    ) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}

.asSSM_plotPrediction <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["predPlot"]])) return()

  depVars <- c(
    "ssm_predictionPlot",
    "ssm_postHocCorrection", "ssm_postHocCorrectionWeight",
    "ssm_count", "ssm_total", "ssm_sampleSize",
    "ssm_controlLimitsLower", "ssm_controlLimitsUpper",
    "ssm_scaleModel", "ssm_predictor",
    "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
    "ssm_priorSigmaSD", "ssm_priorBetaScale",
    "ssm_advancedMcmcSamples", "ssm_advancedMcmcSeed",
    "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
  )

  plot <- createJaspPlot(title = gettext("Posterior Predictive"), width = 500, height = 400)
  plot$dependOn(depVars)
  plot$position <- 2
  jaspResults[["predPlot"]] <- plot

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  y_pred_raw <- samples$y_pred
  y_pred     <- y_pred_raw

  sT <- dataset[[options[["ssm_total"]]]][nrow(dataset)]

  if (options[["ssm_postHocCorrection"]])
    y_pred <- .asSSM_applyAdHocCorrection(y_pred, sT, options[["ssm_postHocCorrectionWeight"]])

  limit_AQL <- options[["ssm_controlLimitsLower"]] * sT
  limit_RQL <- options[["ssm_controlLimitsUpper"]] * sT

  # Posterior probabilities for AQL / RQL regions
  mass_below_AQL <- mean(y_pred < limit_AQL, na.rm = TRUE)
  mass_above_RQL <- mean(y_pred > limit_RQL, na.rm = TRUE)

  # Plot range is based on the uncorrected predictive draws so the distribution
  # remains visible even when correction adds diffuse mass across the full lot size.
  raw_q <- stats::quantile(y_pred_raw, probs = c(0.001, 0.999), na.rm = TRUE, names = FALSE)
  x_min <- floor(min(raw_q[1], limit_AQL, limit_RQL))
  x_max <- ceiling(max(raw_q[2], limit_AQL, limit_RQL))
  x_min <- max(0, x_min)
  if (!is.finite(x_max) || x_max <= x_min)
    x_max <- x_min + 1

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(x_min, x_max, limit_AQL, limit_RQL))
  if (length(xBreaks) < 2L)
    xBreaks <- c(x_min, x_max)
  xLower <- min(xBreaks)
  xUpper <- max(xBreaks)

  # Use the same plotting window as the axis ticks so bars can reach the edge ticks.
  y_plot <- y_pred[y_pred >= xLower & y_pred <= xUpper]
  if (length(y_plot) == 0L)
    y_plot <- y_pred

  breaks <- seq(xLower, xUpper, length.out = 31)
  h <- hist(y_plot, breaks = breaks, plot = FALSE, include.lowest = TRUE, right = TRUE)
  binWidth <- if (length(h$breaks) > 1L) (h$breaks[2] - h$breaks[1]) else 1
  df_hist <- data.frame(
    x = h$mids,
    prob = if (length(y_pred) > 0) h$counts / length(y_pred) else h$counts
  )

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(df_hist$prob)))

  p <- ggplot2::ggplot(df_hist, ggplot2::aes(x = .data$x, y = .data$prob)) +
    ggplot2::geom_col(width = binWidth, fill = "#595959", colour = "white", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = limit_AQL, linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = limit_RQL, linetype = "dashed", linewidth = 0.6) +
    ggplot2::annotate("text", x = limit_AQL, y = max(df_hist$prob),
                      label = "AQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::annotate("text", x = limit_RQL, y = max(df_hist$prob),
                      label = "RQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::labs(
      x = gettext("Defects in Current Lot"),
      y = gettext("Posterior Predictive Probability")
    ) +
    ggplot2::scale_x_continuous(
      breaks = xBreaks,
      limits = range(xBreaks)
    ) +
    ggplot2::scale_y_continuous(
      breaks = yBreaks,
      limits = range(yBreaks)
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p

  if (isTRUE(options[["ssm_postHocCorrection"]])) {
    jaspResults[["predPlotNote"]] <- createJaspHtml(
      text = gettext("Note. Tail correction adds a uniform sudden-jump component over the full lot-size range (0 to N), so this figure is zoomed to the uncorrected predictive range to keep the posterior shape readable."),
      position = 2.1,
      dependencies = depVars
    )
  } else {
    jaspResults[["predPlotNote"]] <- NULL
  }
}


.asSSM_plotPosteriorDist <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["postDist"]])) return()
  plot <- createJaspPlot(title = gettext("Posterior Defect Rate"), width = 500, height = 400)
  plot$dependOn(c(
    "ssm_posteriorDistPlot",
    "ssm_count", "ssm_total", "ssm_sampleSize",
    "ssm_scaleModel", "ssm_predictor",
    "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
    "ssm_priorSigmaSD", "ssm_priorBetaScale",
    "ssm_advancedMcmcSamples", "ssm_advancedMcmcSeed",
    "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
  ))
  plot$position <- 3
  jaspResults[["postDist"]] <- plot

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  theta   <- samples$theta
  df    <- data.frame(x = theta[, ncol(theta)] * 100)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)

  densVals <- stats::density(df$x)
  yBreaks  <- jaspGraphs::getPrettyAxisBreaks(c(0, max(densVals$y)))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_density(fill = "#b3b3b3") +
    ggplot2::labs(
      x = gettext("Defect Rate (%)"),
      y = gettext("Density")
    ) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}

.asSSM_plotBetaCoefficient <- function(jaspResults, dataset, options) {
  if (options[["ssm_scaleModel"]] != "WithPredictor") return()
  if (!is.null(jaspResults[["betaPlot"]])) return()

  plot <- createJaspPlot(title = gettext("Beta Coefficient"), width = 500, height = 400)
  plot$dependOn(c(
    "ssm_plotBeta",
    "ssm_predictor",
    "ssm_count", "ssm_total", "ssm_sampleSize",
    "ssm_scaleModel",
    "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
    "ssm_priorSigmaSD", "ssm_priorBetaScale",
    "ssm_advancedMcmcSamples", "ssm_advancedMcmcSeed",
    "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
  ))

  plot$position <- 4
  jaspResults[["betaPlot"]] <- plot

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  beta    <- samples$beta_temperature

  df      <- data.frame(x = beta)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(beta, 0))

  densVals <- stats::density(beta)
  yBreaks  <- jaspGraphs::getPrettyAxisBreaks(c(0, max(densVals$y)))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_density(fill = "#b3b3b3") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
    ggplot2::labs(
      x = gettext("Predictor Coefficient \u03B2"),
      y = gettext("Density")
    ) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}
.asSSM_applyAdHocCorrection <- function(y_pred_draws, sT, weightPercent) {
  if (length(y_pred_draws) == 0L)
    return(y_pred_draws)

  sMax <- floor(sT)
  if (!is.finite(sMax) || sMax < 0)
    return(y_pred_draws)

  x <- weightPercent / 100
  if (!is.finite(x))
    x <- 0
  x <- max(0, min(1, x))

  # Eq. (10): (1-x) * p_pred + x * Uniform{0,...,s_l}
  # Draw-wise mixture sampling is equivalent to sampling from p_corrected.
  corrected <- as.integer(round(y_pred_draws))
  corrected <- pmin(pmax(corrected, 0L), as.integer(sMax))

  mixIdx <- stats::runif(length(corrected)) < x
  if (any(mixIdx))
    corrected[mixIdx] <- sample.int(as.integer(sMax) + 1L, size = sum(mixIdx), replace = TRUE) - 1L

  corrected
}

.asSSM_createMcmcSummaryTable <- function(jaspResults, dataset, options) {

  if (!isTRUE(options[["ssm_showMcmcSummary"]])) return()

  if (!is.null(jaspResults[["mcmcSummary"]])) return()

  table <- createJaspTable(title = gettext("MCMC Diagnostics"))
  table$dependOn(c(
    "ssm_showMcmcSummary",
    "ssm_showTheta", "ssm_showSigma", "ssm_showYpred", "ssm_showBeta",
    "ssm_scaleModel", "ssm_predictor",
    "ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
    "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
    "ssm_priorSigmaSD", "ssm_priorBetaScale",
    "ssm_advancedMcmcBurnin", "ssm_advancedMcmcSamples",
    "ssm_advancedMcmcChains", "ssm_advancedMcmcThin", "ssm_advancedMcmcSeed",
    "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
  ))
  table$position <- 5   # after the other tables/plots
  table$addColumnInfo(name = "parameter", title = gettext("Parameter"), type = "string")
  table$addColumnInfo(name = "mean",      title = gettext("Mean"),      type = "number")
  table$addColumnInfo(name = "sd",        title = gettext("SD"),        type = "number")
  table$addColumnInfo(name = "n_eff",     title = gettext("ESS"),       type = "number")
  table$addColumnInfo(name = "Rhat",      title = gettext("R-hat"),     type = "number")

  jaspResults[["mcmcSummary"]] <- table

  if (is.null(jaspResults[["fit"]]$object)) return()
  fit <- jaspResults[["fit"]]$object

  sm <- jaspResults[["fit"]]$object$summary
  allNames <- rownames(sm)
  pars <- character(0)

  if (isTRUE(options[["ssm_showTheta"]])) {
    pars <- c(pars, grep("^theta\\[", allNames, value = TRUE))
  }
  if (isTRUE(options[["ssm_showSigma"]])) {
    if ("sigma_eta" %in% allNames)
      pars <- c(pars, "sigma_eta")
  }
  if (isTRUE(options[["ssm_showYpred"]])) {
    if ("y_pred" %in% allNames)
      pars <- c(pars, "y_pred")
  }
  if (options[["ssm_scaleModel"]] == "WithPredictor" && isTRUE(options[["ssm_showBeta"]])) {
    if ("beta_temperature" %in% allNames)
      pars <- c(pars, "beta_temperature")
  }

  if (length(pars) == 0L) {
    table$setError(gettext("No parameters selected for the MCMC diagnostics table."))
    return()
  }

  smSel <- sm[pars, , drop = FALSE]

  rows <- lapply(seq_len(nrow(smSel)), function(i) list(
    parameter = rownames(smSel)[i],
    mean      = smSel[i, "mean"],
    sd        = smSel[i, "sd"],
    n_eff     = smSel[i, "n_eff"],
    Rhat      = smSel[i, "Rhat"]
  ))

  table$addRows(rows)
}
