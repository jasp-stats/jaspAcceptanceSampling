Bayesian Sampling
==========================
*Bayesian Sampling* supports Bayesian planning and inference for acceptance sampling with a Beta prior on the lot defect proportion.

## Input
-------
### 1. Planning
- **Quality constraints**
 - *Acceptable Quality Level (AQL)*: used for the Good/Middle/Bad partition in the three-hypothesis workflow and as the default mode anchor for the impartial two-hypothesis prior.
 - *Rejectable Quality Level (RQL)*: defect-rate threshold for unacceptable quality and the main threshold used in the two-hypothesis planning and inference workflow.
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
- In the standard two-hypothesis workflow, planning targets evidence about whether the lot defect proportion is below the RQL.
- AQL directly affects the three-hypothesis summaries and can also affect the two-hypothesis analysis indirectly through the default impartial prior mode.
