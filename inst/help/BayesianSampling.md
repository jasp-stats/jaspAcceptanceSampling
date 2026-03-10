Bayesian Sampling
==========================
*Bayesian Sampling* supports Bayesian planning and inference for acceptance sampling with a Beta prior on the lot defect proportion.

## Input
-------
### 1. Planning
- **Quality constraints**
 - *Acceptable Quality Level (AQL)*: lower defect-rate threshold for acceptable quality.
 - *Rejectable Quality Level (RQL)*: defect-rate threshold for unacceptable quality.
- **Prior distribution (Beta)**
 - *Impartial (Around RQL)*: prior calibrated around RQL.
 - *Uniform*: Beta(1, 1).
 - *Custom*: user-defined `alpha` and `beta`.
 - *Impartial (Three-region)*: prior calibrated for Good/Middle/Bad regions.
- **Planning controls**
 - *(Max) sample size (n)*: search limit for candidate plans.
 - *Bayes factor (min)*: minimum target Bayes factor.
- **Output options**
 - *Plans table*
 - *Prior plot*
 - *Three-hypothesis BF table*

### 2. Inference
- **Use data to create posterior distribution**
 - *Sample size (n)*
 - *Observed defects (d)*
 - *Lot size (N)*
- **Output options**
 - *Inference decision table*
 - *Posterior plot*
 - *Posterior predictive distribution plot*

## Output
-------
- **Procedure text**: describes hypotheses and workflow.
- **Planning outputs**: candidate plans and optional prior/three-hypothesis summaries.
- **Inference outputs**: posterior-based Bayes factor evidence and posterior predictive lot-quality probabilities.

## Notes
-------
- The posterior uses Beta-Binomial updating from the selected prior and observed `(n, d)`.
- Posterior predictive results are computed for lot-level defects conditional on observed sample data.
