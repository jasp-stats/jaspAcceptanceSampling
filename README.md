# The Acceptance Sampling Module

## Overview

The JASP Acceptance Sampling module is an add-on module for JASP that provides comprehensive tools for lot sampling and quality control decisions. The module supports both classical and Bayesian approaches to acceptance sampling, enabling practitioners to create, analyze, and apply sampling plans for attribute and variable data. It offers single and multiple stage attribute sampling plans, variable sampling plans with accept/reject decision support, Bayesian planning and inference using Beta-Binomial models, and a Bayesian state-space model for tracking defect rates over time. The module integrates operating characteristic (OC) curves, average outgoing quality (AOQ) curves, average total inspection (ATI) curves, and posterior predictive visualizations to help quality engineers design and evaluate sampling strategies.

## R Packages

The acceptance sampling functionality is served by several R packages:

- **AcceptanceSampling** — The primary package for classical sampling plan creation and analysis ([AcceptanceSampling on CRAN](https://cran.r-project.org/package=AcceptanceSampling))
- **abtest** — Used for Bayesian hypothesis testing in sampling decisions ([abtest on CRAN](https://cran.r-project.org/package=abtest))
- **rstan** — Stan interface for Bayesian state-space model estimation ([rstan on CRAN](https://cran.r-project.org/package=rstan))

## Analyses

The organization of the analyses within the Acceptance Sampling module in JASP is as follows:

```
--- Acceptance Sampling
    -- Attribute Sampling
       - Create Attribute Plan
       - Analyze Attribute Plan
    -- Variable Sampling
       - Create Variable Plan
       - Analyze Variable Plan
       - Accept/Reject Lots
    -- Bayesian Sampling
       - Bayesian Sampling
       - Bayesian State-Space Sampling
```

## Key Features

**Attribute Sampling Plans:** Create single stage sampling plans that satisfy producer and consumer risk constraints (AQL/RQL). Analyze single and multiple stage plans with support for binomial, hypergeometric, and Poisson distributions.

**Variable Sampling Plans:** Design sampling plans based on continuous measurements with normal distribution assumptions. Evaluate plans against quality levels and make accept/reject decisions for individual lots.

**Bayesian Planning and Inference:** Plan acceptance sampling studies using Beta-Binomial models with customizable priors (uniform, custom, impartial). Perform Bayesian inference on observed lot data with posterior updating, Bayes factor evidence assessment, and posterior predictive distributions.

**Bayesian State-Space Model:** Track defect rates over time using a dynamic state-space model estimated via Stan. Supports covariates, posterior distribution visualization, and predictive inference for ongoing quality monitoring.

**Visualization:** OC curves, AOQ curves, ATI curves, ASN curves for classical plans. Prior/posterior density plots, posterior predictive distributions, and state trajectory plots for Bayesian analyses.

## Maintainers

- Henrik Godmann (Bayesian analyses)
- JASP Team (Classical analyses)
