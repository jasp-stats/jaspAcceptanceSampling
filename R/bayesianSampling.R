# Copyright (C) 2018-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

#' @importFrom jaspBase jaspDeps %setOrRetrieve%
#' @importFrom rlang .data
#' @export


BayesianSampling <- function(jaspResults, dataset = NULL, options, ...) {

  # 0. Introductory Procedure Text (Always shown up front)
  .bsProcedure(jaspResults, options, position = 0)

  # 1. Planning
  .bsPlanningAnalysis(jaspResults, options, position = 1)

  # 2. Inference
  .bsInferenceAnalysis(jaspResults, options, position = 10)
}

# Dependency helpers
.bsPlanningDeps <- function() {
  c(
    "max_nplan", "min_bfplan", "aqlplan", "rqlplan", "priorplan",
    "alphaplan", "betaplan",
    "impartialCustomModeplan", "impartialModeplan",
    "impartialThreeConstraintplan",   # NEW
    "showPlansplan", "priorPlotplan",
    "showThreeBFplan"
  )
}

.bsInferenceDeps <- function() {
  c(
    "inferPosteriorinfer",

    "aqlplan", "rqlplan", "priorplan", "alphaplan", "betaplan",
    "impartialCustomModeplan", "impartialModeplan",
    "impartialThreeConstraintplan",

    "data_ninfer", "data_dinfer", "lot_sizeinfer",
    "showInferenceTableinfer", "posteriorPlotinfer", "ppdPlotinfer"
  )
}


.bsClipProb <- function(p, eps = 1e-12) {
  pmax(pmin(p, 1 - eps), eps)
}

.bsNormalizeProbTriplet <- function(p_good, p_middle, p_bad) {
  p_good   <- pmax(p_good, 0)
  p_middle <- pmax(p_middle, 0)
  p_bad    <- pmax(p_bad, 0)

  total <- p_good + p_middle + p_bad
  if (!is.finite(total) || total <= 0)
    return(c(good = NA_real_, middle = NA_real_, bad = NA_real_))

  c(
    good = p_good / total,
    middle = p_middle / total,
    bad = p_bad / total
  )
}

.bsSafeLogProb <- function(p, eps = 1e-12) {
  log(pmax(p, eps))
}

# Beta-Binomial PMF: P(X = x | n_trials, alpha, beta)
.bsBetaBinomPMF <- function(x, n_trials, alpha, beta) {
  exp(lchoose(n_trials, x) +
        lbeta(alpha + x, beta + n_trials - x) -
        lbeta(alpha, beta))
}

# PPD region probabilities based on posterior predictive distribution
# D_rem | k,n ~ BetaBinomial(N-n, a+k, b+n-k)
# D_tot = k + D_rem
# Good:   D_tot <= floor(AQL * N)
# Middle: floor(AQL * N) < D_tot <= floor(RQL * N)
# Bad:    D_tot > floor(RQL * N)
.bsPPDRegionProbs <- function(n, d, N, aql, rql, alpha, beta) {
  alpha_post <- alpha + d
  beta_post  <- beta + n - d
  n_rem      <- N - n

  aql_count <- floor(aql * N)
  rql_count <- floor(rql * N)

  # D_rem ranges from 0 to n_rem; D_tot = d + D_rem
  d_rem_vals <- 0:n_rem
  pmf <- .bsBetaBinomPMF(d_rem_vals, n_rem, alpha_post, beta_post)

  d_tot <- d + d_rem_vals

  p_good   <- sum(pmf[d_tot <= aql_count])
  p_middle <- sum(pmf[d_tot > aql_count & d_tot <= rql_count])
  p_bad    <- sum(pmf[d_tot > rql_count])

  list(good = p_good, middle = p_middle, bad = p_bad)
}

# Full PPD PMF for D_tot
.bsPPDFull <- function(n, d, N, alpha_post, beta_post) {
  n_rem <- N - n
  d_rem_vals <- 0:n_rem
  pmf <- .bsBetaBinomPMF(d_rem_vals, n_rem, alpha_post, beta_post)
  d_tot <- d + d_rem_vals
  data.frame(d_tot = d_tot, pmf = pmf)
}


.bsCalculateBF <- function(rql, n, c, alpha, beta, prior_odds) {
  posterior_in_favor <- pbeta(rql, alpha + c, beta + n - c)
  posterior_odds <- posterior_in_favor / (1 - posterior_in_favor)
  bf <- posterior_odds / prior_odds
  return(bf)
}

# Utility functions
.bsCalculatePriorParams <- function(prior, aql, rql,
                                    alpha = NULL, beta = NULL,
                                    impartialCustomMode = FALSE,
                                    impartialMode = NULL,
                                    impartialThreeConstraint = "no_U") {

  if (prior == "impartial") {
    mode <- if (isTRUE(impartialCustomMode) && !is.null(impartialMode)) impartialMode else aql
    if (mode <= 0 || mode >= rql)
      stop(gettextf("The prior mode (%1$.4f) must be between 0 and the RQL (%2$.4f).", mode, rql))
    params <- .bsSolveImpartial(mode, rql)
    alpha  <- params$alpha
    beta   <- params$beta

  } else if (prior == "impartial_three") {
    params <- .bsSolveImpartialThree(aql, rql, constraint = impartialThreeConstraint)
    alpha <- params$alpha
    beta  <- params$beta

  } else if (prior == "uniform") {
    alpha <- beta <- 1

  } else if (prior == "custom") {
  }

  list(alpha = alpha, beta = beta)
}

.bsSafePbeta <- function(x, a, b) {
  out <- suppressWarnings(
    tryCatch(pbeta(x, a, b), error = function(e) NA_real_)
  )
  if (!is.finite(out)) NA_real_ else out
}

.bsSolveThreeRegionBeta <- function(c1, c2,
                                    pi = c(1/3, 1/3, 1/3),
                                    constraint = c("none", "no_U", "both_ge_1"),
                                    reltol = 1e-12,
                                    penalty_scale = 1e-8,
                                    log_lower = -10,
                                    log_upper =  10,
                                    ge1_eps = 1e-8) {

  constraint <- match.arg(constraint)
  stopifnot(length(pi) == 3, all(pi > 0), abs(sum(pi) - 1) < 1e-12)
  stopifnot(0 < c1, c1 < c2, c2 < 1)

  p1 <- pi[1]
  p2 <- pi[1] + pi[2]

  objective <- function(par_log) {
    a <- exp(par_log[1]); b <- exp(par_log[2])

    F1 <- .bsSafePbeta(c1, a, b)
    F2 <- .bsSafePbeta(c2, a, b)
    if (is.na(F1) || is.na(F2)) return(1e100)

    err1 <- F1 - p1
    err2 <- F2 - p2

    penalty <- penalty_scale * (a^2 + b^2)
    err1^2 + err2^2 + penalty
  }

  start0 <- c(log(1), log((1 - c2) / c2))
  start0 <- pmin(pmax(start0, log_lower), log_upper)

  lo_base <- c(log_lower, log_lower)
  up_base <- c(log_upper, log_upper)
  lo_ge1  <- log(1 + ge1_eps)

  boxes <- switch(constraint,
                  "none" = list(list(name = "box", lower = lo_base, upper = up_base)),
                  "both_ge_1" = list(list(name = "alpha>=1 & beta>=1",
                                          lower = pmax(lo_base, c(lo_ge1, lo_ge1)),
                                          upper = up_base)),
                  "no_U" = list(
                    list(name = "alpha>=1", lower = pmax(lo_base, c(lo_ge1, log_lower)), upper = up_base),
                    list(name = "beta>=1",  lower = pmax(lo_base, c(log_lower, lo_ge1)), upper = up_base)
                  )
  )

  run_box <- function(par0, lower, upper) {
    par0p <- pmin(pmax(par0, lower), upper)
    suppressWarnings(stats::optim(
      par = par0p,
      fn  = function(p) objective(p),
      method = "L-BFGS-B",
      lower = lower,
      upper = upper,
      control = list(reltol = reltol)
    ))
  }

  fits <- lapply(boxes, function(bx) {
    fit <- run_box(start0, bx$lower, bx$upper)
    fit$region <- bx$name
    fit
  })
  vals <- vapply(fits, function(f) if (is.finite(f$value)) f$value else Inf, numeric(1))
  fit  <- fits[[which.min(vals)]]

  a <- exp(fit$par[1])
  b <- exp(fit$par[2])

  if (!is.null(fit$convergence) && fit$convergence != 0)
    warning("Three-region Beta solver did not converge.")

  list(alpha = a, beta = b, convergence = fit$convergence, value = fit$value, region = fit$region)
}

.bsSolveImpartial <- function(mode, rql) {
  # Solve for Beta(alpha, beta) with given mode and P(theta <= rql) = 0.5.
  # The mode constraint gives alpha = (1 - 2*mode + mode*beta) / (1 - mode),
  # reducing the system to a single equation in beta solved via uniroot.
  # beta must be > (2*mode - 1)/mode so that alpha > 0; we also need alpha >= 1
  # (beta >= 1) for a proper unimodal density.
  alpha_from_beta <- function(b) (1 - 2 * mode + mode * b) / (1 - mode)
  f <- function(b) pbeta(rql, alpha_from_beta(b), b) - 0.5
  # Lower bound: beta where alpha = 1, i.e. b = (1 - mode) / mode + 1 = 1/mode.
  # But alpha(1/mode) = 1 gives Beta(1, 1/mode) which may still be valid.
  # We search from just above where alpha > 0: b > (2*mode - 1)/mode.
  b_min <- max(1e-8, (2 * mode - 1) / mode + 1e-8)
  f_lower <- suppressWarnings(tryCatch(f(b_min), error = function(e) NA_real_))
  f_upper <- suppressWarnings(tryCatch(f(1e4), error = function(e) NA_real_))

  if (!is.finite(f_lower) || !is.finite(f_upper) || f_lower * f_upper > 0) {
    stop(
      gettextf(
        "Could not construct the impartial prior for mode %1$.4f and RQL %2$.4f. Please choose a lower mode, a lower RQL, or a different prior.",
        mode, rql
      ),
      call. = FALSE
    )
  }

  result <- tryCatch(
    stats::uniroot(f, lower = b_min, upper = 1e4, tol = .Machine$double.eps),
    error = function(e) {
      stop(
        gettextf(
          "Could not construct the impartial prior for mode %1$.4f and RQL %2$.4f. Please choose a lower mode, a lower RQL, or a different prior.",
          mode, rql
        ),
        call. = FALSE
      )
    }
  )
  beta  <- result$root
  alpha <- alpha_from_beta(beta)
  list(alpha = alpha, beta = beta)
}

.bsSolveImpartialThree <- function(aql, rql,
                                   constraint = c("none", "no_U", "both_ge_1")) {
  constraint <- match.arg(constraint)
  fit <- .bsSolveThreeRegionBeta(aql, rql,
                                 pi = c(1/3, 1/3, 1/3),
                                 constraint = constraint)
  list(alpha = fit$alpha, beta = fit$beta)
}



.bsThreeRegionProbs <- function(aql, rql, alpha, beta, eps = 1e-12) {
  # Regions:
  # Good:   theta <= AQL
  # Middle: AQL < theta <= RQL
  # Bad:    theta > RQL

  pA <- pbeta(aql, alpha, beta)
  pR <- pbeta(rql, alpha, beta)

  p_good   <- pA
  p_middle <- pR - pA
  p_bad    <- 1 - pR

  .bsNormalizeProbTriplet(p_good, p_middle, p_bad)
}

.bsLogBFThreeHyp <- function(aql, rql, n, d, alpha, beta, eps = 1e-12) {
  # prior probs under Beta(alpha,beta)
  prior <- .bsThreeRegionProbs(aql, rql, alpha, beta, eps)

  # posterior probs under Beta(alpha+d, beta+n-d)
  post  <- .bsThreeRegionProbs(aql, rql, alpha + d, beta + n - d, eps)

  # log BF_ij = log( (post_i/post_j) / (prior_i/prior_j) )
  logBF_GB <- (.bsSafeLogProb(post["good"], eps)   - .bsSafeLogProb(post["bad"], eps))    -
              (.bsSafeLogProb(prior["good"], eps)  - .bsSafeLogProb(prior["bad"], eps))
  logBF_GM <- (.bsSafeLogProb(post["good"], eps)   - .bsSafeLogProb(post["middle"], eps)) -
              (.bsSafeLogProb(prior["good"], eps)  - .bsSafeLogProb(prior["middle"], eps))
  logBF_MB <- (.bsSafeLogProb(post["middle"], eps) - .bsSafeLogProb(post["bad"], eps))    -
              (.bsSafeLogProb(prior["middle"], eps)- .bsSafeLogProb(prior["bad"], eps))

  c(
    logBF_GB = as.numeric(logBF_GB),
    logBF_GM = as.numeric(logBF_GM),
    logBF_MB = as.numeric(logBF_MB)
  )
}

.bsGeneratePlans <- function(aql, rql, max_n, min_bf, alpha, beta,
                             add_three_hyp = FALSE, eps = 1e-12) {

  prior_cdf_rql <- pbeta(rql, alpha, beta)
  prior_odds <- prior_cdf_rql / (1 - prior_cdf_rql)

  plans <- data.frame(n = integer(), c = integer(), bf = numeric())

  for (n in seq_len(max_n)) {
    c <- n
    while (c >= 0) {
      bf <- .bsCalculateBF(rql, n, c, alpha, beta, prior_odds)
      if (bf >= min_bf) {
        plans <- rbind(plans, data.frame(n = n, c = c, bf = bf))
        break
      }
      if (c == 0) break
      c <- c - 1
    }
  }

  if (nrow(plans) == 0)
    return(plans)

  plans$log_bf <- log(plans$bf)

  # add three-hypothesis log-BFs computed at the "worst acceptable" outcome d=c
  if (isTRUE(add_three_hyp)) {
    lbf <- t(mapply(
      FUN = function(n, c) .bsLogBFThreeHyp(aql, rql, n, c, alpha, beta, eps),
      n = plans$n,
      c = plans$c
    ))

    lbf <- as.data.frame(lbf)
    plans$logBF_GB <- lbf$logBF_GB
    plans$logBF_GM <- lbf$logBF_GM
    plans$logBF_MB <- lbf$logBF_MB

    plans$BF_GB <- exp(plans$logBF_GB)
    plans$BF_GM <- exp(plans$logBF_GM)
    plans$BF_MB <- exp(plans$logBF_MB)
  }

  return(na.omit(plans))
}


# Section 1: Planning
.bsPlanningAnalysis <- function(jaspResults, options, position) {

  if (!isTRUE(options$showPlansplan) &&
      !isTRUE(options$priorPlotplan) &&
      !isTRUE(options$showThreeBFplan)) {
    return()
  }

  container <- jaspResults[["planContainer"]] %setOrRetrieve% (
    createJaspContainer(title = gettext("Planning")) |>
      createJaspState(jaspDeps(.bsPlanningDeps()))
  )

  if (is.null(container) || container$getError())
    return()

  on.exit({
    jaspResults[["planContainer"]] <- container
  }, add = TRUE)

  aql    <- options$aqlplan
  rql    <- options$rqlplan
  max_n  <- options$max_nplan
  min_bf <- options$min_bfplan

  if (!is.finite(aql) || !is.finite(rql) || aql <= 0 || rql <= 0 || aql >= rql || rql >= 1) {
    container$setError(gettext("Please ensure 0 < AQL < RQL < 1."))
    return()
  }

  if (!is.finite(max_n) || max_n <= 0) {
    container$setError(gettext("Please provide a valid maximum sample size (> 0)."))
    return()
  }

  if (!is.finite(min_bf) || min_bf <= 0) {
    container$setError(gettext("Please provide a valid minimum Bayes factor (> 0)."))
    return()
  }

  priorParams <- tryCatch(
    .bsCalculatePriorParams(
      options$priorplan, aql, rql,
      options$alphaplan, options$betaplan,
      impartialCustomMode = options$impartialCustomModeplan,
      impartialMode = options$impartialModeplan,
      impartialThreeConstraint = options$impartialThreeConstraintplan
    ),
    error = function(e) {
      container$setError(conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(priorParams))
    return()

  if (!is.finite(priorParams$alpha) || !is.finite(priorParams$beta) ||
      priorParams$alpha <= 0 || priorParams$beta <= 0) {
    container$setError(gettext("The selected prior results in invalid Beta parameters. Please adjust AQL/RQL or choose a different prior."))
    return()
  }

  plans <- jaspResults[["planData"]] %setOrRetrieve% (
    .bsGeneratePlans(aql, rql, max_n, min_bf,
                     priorParams$alpha, priorParams$beta,
                     add_three_hyp = isTRUE(options$showThreeBFplan)) |>
      createJaspState(jaspDeps(.bsPlanningDeps()))
  )

  if (is.null(plans) || nrow(plans) == 0) {
    container$setError(gettext("No valid plans found satisfying the specified constraints."))
    return()
  }

  if (isTRUE(options$showPlansplan)) {
    .bsPlanTable(container, plans, position = 1, base = "showPlansplan",
                 rql = rql, alpha = priorParams$alpha, beta = priorParams$beta)
  }

  if (isTRUE(options$priorPlotplan)) {
    .bsDistributionPlot(container, aql, rql, priorParams$alpha, priorParams$beta,
                        "Prior", position = 2, base = "priorPlotplan")
  }

  if (isTRUE(options$showThreeBFplan)) {
    .bsThreeBFTable(container, plans, position = 3, base = "showThreeBFplan",
                    aql = aql, rql = rql, alpha = priorParams$alpha, beta = priorParams$beta)
  }
}

# Section 2: Inference
.bsInferenceAnalysis <- function(jaspResults, options, position) {
  if (!isTRUE(options$inferPosteriorinfer))
    return()

  # Create/retrieve container + state
  container <- jaspResults[["infContainer"]] %setOrRetrieve% (
    createJaspContainer(title = gettext("Inference")) |>
      createJaspState(jaspDeps(.bsInferenceDeps()))
  )

  if (is.null(container) || container$getError())
    return()

  # Always write container back, even if we return early later
  on.exit({
    jaspResults[["infContainer"]] <- container
  }, add = TRUE)

  # Always use prior from planning stage
  aql <- options$aqlplan
  rql <- options$rqlplan
  priorParams <- tryCatch(
    .bsCalculatePriorParams(
      options$priorplan, aql, rql,
      options$alphaplan, options$betaplan,
      impartialCustomMode = options$impartialCustomModeplan,
      impartialMode = options$impartialModeplan,
      impartialThreeConstraint = options$impartialThreeConstraintplan
    ),
    error = function(e) {
      container$setError(conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(priorParams))
    return()

  if (!is.finite(aql) || !is.finite(rql) || aql <= 0 || rql <= 0 || aql >= rql || rql >= 1) {
    container$setError(gettext("Please ensure 0 < AQL < RQL < 1."))
    return()
  }

  if (!is.finite(priorParams$alpha) || !is.finite(priorParams$beta) ||
      priorParams$alpha <= 0 || priorParams$beta <= 0) {
    container$setError(gettext("The selected prior results in invalid Beta parameters. Please adjust AQL/RQL or choose a different prior."))
    return()
  }

  # Data
  n <- options$data_ninfer
  d <- options$data_dinfer

  if (!is.finite(n) || !is.finite(d) || n < 0 || d < 0 || d > n) {
    container$setError(gettext("Please ensure the data are valid: n >= 0 and 0 <= d <= n."))
    return()
  }

  # Posterior
  alpha_post <- priorParams$alpha + d
  beta_post  <- priorParams$beta + n - d

  # Bayes factor (with clipping to avoid division by 0)
  post_p <- .bsClipProb(pbeta(rql, alpha_post, beta_post))
  prior_p <- .bsClipProb(pbeta(rql, priorParams$alpha, priorParams$beta))

  posterior_odds <- post_p / (1 - post_p)
  prior_odds     <- prior_p / (1 - prior_p)
  bf <- posterior_odds / prior_odds

  # Outputs
  # Lot size for PPD
  N <- options$lot_sizeinfer
  if (!is.finite(N) || N < n) {
    container$setError(gettext("Lot size (N) must be at least as large as the sample size (n)."))
    return()
  }

  # Compute PPD region probabilities (Beta-Binomial)
  ppd <- .bsPPDRegionProbs(n, d, N, aql, rql, priorParams$alpha, priorParams$beta)

  if (isTRUE(options$showInferenceTableinfer)) {
    .bsInferenceDecisionTable(container, n, d, N, aql, rql,
                              bf, ppd,
                              position = 0, base = "showInferenceTableinfer")

    # Decision summary text below the table
    bfInterpretation <- if (bf >= 10) {
      gettext("Strong evidence for accepting the lot.")
    } else if (bf >= 3) {
      gettext("Moderate evidence for accepting the lot.")
    } else if (bf >= 1/3) {
      gettext("Inconclusive evidence.")
    } else if (bf >= 1/10) {
      gettext("Moderate evidence for rejecting the lot.")
    } else {
      gettext("Strong evidence for rejecting the lot.")
    }

    rql_count <- floor(rql * N)
    ppdGoodMiddle <- ppd$good + ppd$middle

    summaryText <- createJaspHtml(
      text = gettextf(
        "The <b>Bayes factor</b> in favor of the lot defect rate being below the RQL of <b>%1$.2f</b> is <b>%2$.2f</b>. %3$s<br>Based on the observed sample, there is a <b>%4$.1f%%</b> predicted probability that the current lot of %5$d items contains at most %6$d total defects.",
        rql, bf, bfInterpretation,
        ppdGoodMiddle * 100, as.integer(N), rql_count
      ),
      position = 0.5,
      dependencies = jaspDeps(c("showInferenceTableinfer", "data_ninfer", "data_dinfer", "lot_sizeinfer"))
    )
    container[["decisionSummaryText"]] <- summaryText
  } else {
    container[["inferenceDecisionTable"]] <- NULL
    container[["decisionSummaryText"]] <- NULL
  }

  if (isTRUE(options$posteriorPlotinfer)) {
    .bsDistributionPlot(container, aql, rql, alpha_post, beta_post,
                        "Posterior", position = 1, base = "posteriorPlotinfer",
                        alpha_prior = priorParams$alpha, beta_prior = priorParams$beta)

    bfText <- createJaspHtml(
      text = gettextf(
        "<u>Bayes factor</u> in favor of proportion of defects < <u>%1$.2f</u> is <b>%2$.2f</b>.",
        rql, bf
      ),
      position = 2,
      # Add posteriorPlotinfer to dependencies so it refreshes correctly
      dependencies = jaspDeps(c("data_ninfer", "data_dinfer", "posteriorPlotinfer"))
    )
    container[["bfText"]] <- bfText
  } else {
    container[["distPlotPosterior"]] <- NULL
    container[["bfText"]] <- NULL
  }

  if (isTRUE(options$ppdPlotinfer)) {
    .bsPPDPlot(container, n, d, N, aql, rql,
               alpha_post, beta_post,
               position = 3, base = "ppdPlotinfer")
  } else {
    container[["ppdPlot"]] <- NULL
  }

}


# Tables
.bsPlanTable <- function(container, plans, position, base = "showPlansplan",
                         rql, alpha, beta) {


  # Keep only the lowest n for each acceptance number c
  plans2 <- plans[order(plans$c, plans$n), , drop = FALSE]
  plans2 <- plans2[!duplicated(plans2$c), , drop = FALSE]

  # Prior P(theta < RQL)
  prior_p <- .bsClipProb(pbeta(rql, alpha, beta))
  prior_odds <- prior_p / (1 - prior_p)

  # Posterior P(theta < RQL) at d = c (row-wise)
  post_p <- .bsClipProb(pbeta(rql, alpha + plans2$c, beta + plans2$n - plans2$c))
  post_odds <- post_p / (1 - post_p)

  # Bayes factor (odds ratio)
  bf <- post_odds / prior_odds

  plans2$prior_p <- prior_p
  plans2$post_p  <- post_p
  plans2$bf_odds <- bf

  table <- createJaspTable(
    title = gettext("Sampling Plans"),
    position = position,
    dependencies = jaspDeps(base)
  )

  table$addColumnInfo(name = "c",       title = gettext("Acceptance number (c)"),  type = "integer")
  table$addColumnInfo(name = "n",       title = gettext("Lowest sample size (n)"), type = "integer")
  table$addColumnInfo(name = "prior_p", title = gettext("Prior P(θ < RQL)"),       type = "number")
  table$addColumnInfo(name = "post_p",  title = gettext("Posterior P(θ < RQL)"),   type = "number")
  table$addColumnInfo(name = "bf_odds", title = gettext("Bayes factor"),           type = "number")

  table$setData(plans2[c("c", "n", "prior_p", "post_p", "bf_odds")])

  container[["planTable"]] <- table
}


.bsThreeBFTable <- function(container, plans, position, base = "showThreeBFplan",
                            aql, rql, alpha, beta) {

  needed <- c("n", "c", "BF_GB", "BF_GM", "BF_MB")
  if (!all(needed %in% names(plans))) return()

  # Keep only the lowest n for each acceptance number c
  plans2 <- plans[order(plans$c, plans$n), , drop = FALSE]
  plans2 <- plans2[!duplicated(plans2$c), , drop = FALSE]

  # Prior region probs (constant)
  prior <- .bsThreeRegionProbs(aql, rql, alpha, beta)
  plans2$prior_good   <- unname(prior["good"])
  plans2$prior_middle <- unname(prior["middle"])
  plans2$prior_bad    <- unname(prior["bad"])

  # Posterior region probs at d = c (row-wise)
  post_matrix <- t(mapply(
    FUN = function(a_post, b_post) .bsThreeRegionProbs(aql, rql, a_post, b_post),
    a_post = alpha + plans2$c,
    b_post = beta + plans2$n - plans2$c
  ))

  plans2$post_good   <- unname(post_matrix[, "good"])
  plans2$post_middle <- unname(post_matrix[, "middle"])
  plans2$post_bad    <- unname(post_matrix[, "bad"])

  table <- createJaspTable(
    title = gettext("Three-Hypothesis Bayes Factors (at d = c)"),
    position = position,
    dependencies = jaspDeps(base)
  )

  table$addColumnInfo(name = "c", title = gettext("Acceptance number (c)"),  type = "integer")
  table$addColumnInfo(name = "n", title = gettext("Lowest sample size (n)"), type = "integer")

  table$addColumnInfo(name = "prior_good",   title = gettext("Prior P(Good)"),   type = "number")
  table$addColumnInfo(name = "post_good",    title = gettext("Post P(Good)"),    type = "number")
  table$addColumnInfo(name = "prior_middle", title = gettext("Prior P(Middle)"), type = "number")
  table$addColumnInfo(name = "post_middle",  title = gettext("Post P(Middle)"),  type = "number")
  table$addColumnInfo(name = "prior_bad",    title = gettext("Prior P(Bad)"),    type = "number")
  table$addColumnInfo(name = "post_bad",     title = gettext("Post P(Bad)"),     type = "number")

  table$addColumnInfo(name = "BF_GB", title = gettext("BF (Good vs Bad)"),    type = "number")
  table$addColumnInfo(name = "BF_GM", title = gettext("BF (Good vs Middle)"), type = "number")
  table$addColumnInfo(name = "BF_MB", title = gettext("BF (Middle vs Bad)"),  type = "number")

  table$setData(plans2[c(
    "c", "n",
    "prior_good", "post_good",
    "prior_middle", "post_middle",
    "prior_bad", "post_bad",
    "BF_GB", "BF_GM", "BF_MB"
  )])

  container[["threeBFTable"]] <- table
}


# Plots
.bsDistributionPlot <- function(container, aql, rql, alpha, beta, type,
                                position, base, alpha_prior = NULL, beta_prior = NULL) {
  plotName <- paste0("distPlot", type)

  plot <- createJaspPlot(
    title = if (type == "Posterior") gettext("Posterior Distribution") else gettext("Prior Distribution"),
    width = 570,
    height = 320,
    position = position,
    dependencies = jaspDeps(base)
  )

  eps <- 1e-6
  xValue <- seq(eps, 1 - eps, 0.001)

  dens_post <- dbeta(xValue, alpha, beta)
  dens_post[!is.finite(dens_post)] <- NA_real_

  df_post <- data.frame(
    x = xValue,
    dens = dens_post,
    dist = type
  )

  df <- df_post
  hasPriorOverlay <- identical(type, "Posterior") && !is.null(alpha_prior) && !is.null(beta_prior)
  if (hasPriorOverlay) {
    dens_prior <- dbeta(xValue, alpha_prior, beta_prior)
    dens_prior[!is.finite(dens_prior)] <- NA_real_

    df_prior <- data.frame(
      x = xValue,
      dens = dens_prior,
      dist = "Prior"
    )
    df <- rbind(df_prior, df_post)
    df$dist <- factor(df$dist, levels = c("Posterior", "Prior"))
  } else {
    df$dist <- factor(df$dist)
  }

  yMax <- max(df$dens, na.rm = TRUE)

  if (!is.finite(yMax) || yMax <= 0) yMax <- 1

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, yMax))

  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = aql, linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = rql, linetype = "dashed", linewidth = 0.6) +
    ggplot2::annotate("text", x = aql, y = yMax, label = "AQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::annotate("text", x = rql, y = yMax, label = "RQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(x = .data$x, y = .data$dens, linetype = .data$dist),
      color = "black",
      linewidth = 0.9
    ) +
    ggplot2::scale_linetype_manual(values = if (hasPriorOverlay) c(Posterior = "solid", Prior = "dashed") else setNames("dashed", type)) +
    ggplot2::labs(
      x = gettext("Lot Proportion Defective"),
      y = gettext("Density"),
      linetype = NULL
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (hasPriorOverlay) "right" else "none") +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))

  plot$plotObject <- p
  container[[plotName]] <- plot
}

# Posterior Predictive Distribution Plot
.bsPPDPlot <- function(container, n, d, N, aql, rql,
                       alpha_post, beta_post,
                       position, base) {

  plot <- createJaspPlot(
    title = gettext("Posterior Predictive Distribution"),
    width = 570,
    height = 320,
    position = position,
    dependencies = jaspDeps(base)
  )

  ppd_df <- .bsPPDFull(n, d, N, alpha_post, beta_post)

  aql_count <- floor(aql * N)
  rql_count <- floor(rql * N)

  ppd_df$region <- ifelse(ppd_df$d_tot <= aql_count, "Good",
                   ifelse(ppd_df$d_tot <= rql_count, "Middle", "Bad"))
  ppd_df$region <- factor(ppd_df$region, levels = c("Good", "Middle", "Bad"))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(ppd_df$d_tot)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(ppd_df$pmf)))

  p <- ggplot2::ggplot(ppd_df, ggplot2::aes(x = .data$d_tot, y = .data$pmf, fill = .data$region)) +
    ggplot2::geom_col(width = 0.8, color = "black", linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = aql_count + 0.5, linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = rql_count + 0.5, linetype = "dashed", linewidth = 0.6) +
    ggplot2::annotate("text", x = aql_count + 0.5, y = max(ppd_df$pmf),
                      label = "AQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::annotate("text", x = rql_count + 0.5, y = max(ppd_df$pmf),
                      label = "RQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::scale_fill_manual(
      values = c(Good = "grey75", Middle = "grey55", Bad = "grey35"),
      guide = "none"
    ) +
    ggplot2::labs(
      x = gettext("Total Defects in Lot"),
      y = gettext("Probability")
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))

  plot$plotObject <- p
  container[["ppdPlot"]] <- plot
}

.bsInferenceDecisionTable <- function(container, n, d, N, aql, rql,
                                       bf, ppd,
                                       position, base) {
  
  table <- createJaspTable(
    title = gettext("Inference Decision Table"),
    position = position,
    dependencies = jaspDeps(base)
  )
  
  table$addColumnInfo(name = "col_1", title = "", type = "string")
  table$addColumnInfo(name = "col_2", title = gettext("Value"), type = "string")
  
  # PPD region labels with count thresholds
  aql_count <- floor(aql * N)
  rql_count <- floor(rql * N)
  
  ppd_good_label   <- gettextf("PPD Good (\u2264 %1$d defects in Lot)", aql_count)
  ppd_middle_label  <- gettextf("PPD Middle (> %1$d & \u2264 %2$d defects)", aql_count, rql_count)
  ppd_bad_label     <- gettextf("PPD Bad (> %1$d defects in Lot)", rql_count)
  
  table[["col_1"]] <- c(
    gettext("Sample Size (n)"),
    gettext("Defects (d)"),
    gettext("AQL"),
    gettext("RQL"),
    gettext("Bayes Factor"),
    ppd_good_label,
    ppd_middle_label,
    ppd_bad_label
  )
  
  table[["col_2"]] <- c(
    as.character(n),
    as.character(d),
    sprintf("%.3f", aql),
    sprintf("%.3f", rql),
    sprintf("%.3f", bf),
    sprintf("%.3f", ppd$good),
    sprintf("%.3f", ppd$middle),
    sprintf("%.3f", ppd$bad)
  )
  
  table$addFootnote(gettext("Bayes factor interpretation: BF \u2265 10 (strong evidence for H\u2212), 3 \u2264 BF < 10 (moderate evidence for H\u2212), 0.33 \u2264 BF < 3 (inconclusive), 0.10 \u2264 BF < 0.33 (moderate evidence for H+), BF < 0.10 (strong evidence for H+)."))
  
  container[["inferenceDecisionTable"]] <- table
}

.bsProcedure <- function(jaspResults, options, position) {

  # Retrieve values for dynamic text
  aql <- options$aqlplan * 100
  rql <- options$rqlplan * 100

  container <- jaspResults[["procedureContainer"]] %setOrRetrieve%
    createJaspContainer(title = gettext("Procedure"))

  introText <- gettextf(
    "<h3></h3>
    <p>The <b>main objective</b> of this Bayesian sampling procedure is to evaluate the quality of a lot by determining if the proportion of defects is lower than the <b>Rejectable Quality Level (RQL)</b>, which is currently set at <b>%1$.2f%%</b>.</p>

    <p>Two primary statistical hypotheses are formulated about the lot proportion defective (&theta;):
    <ul>
      <li>The (null) hypothesis of intolerable quality H<sub>+</sub>: &theta; &ge; %1$.2f%% (user-defined)</li>
      <li>The (alternative) hypothesis of tolerable quality H<sub>-</sub>: &theta; < %1$.2f%% (user-defined)</li>
    </ul></p>

    <p>Additionally, users have the option to perform a <b>Three-Hypothesis Test</b>, which partitions the quality levels into 'Good' (&le; AQL), 'Middle' (AQL < &theta; &le; RQL), and 'Bad' (> RQL) regions.</p>

    <p>The <b>Acceptable Quality Level (AQL)</b> is currently set at <b>%2$.2f%%</b>. This value represents the
    quality level aimed for acceptance with high probability. These quality constraints can be adjusted in the Planning phase to reflect specific requirements.</p>

    <h3>Stages</h3>
    <p><b>1. Planning:</b> Determine a minimum sample size such that the sample provides sufficient evidence (a target Bayes factor) to achieve the specified sampling objectives.</p>

    <p><b>2. Inference:</b> Evaluate observed data (sample size and defects) to update the prior belief into
    a posterior distribution.</p>",
    rql, aql
  )

  container[["intro"]] <- createJaspHtml(text = introText, position = 1)

  jaspResults[["procedureContainer"]] <- container
}
