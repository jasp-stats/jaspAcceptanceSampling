Bayesian State-Space Sampling
=============================
*Bayesian State-Space Sampling* performs acceptance sampling with a Bayesian state-space model for defect rates over lots.

## Input
-------
### Data assignment
- **Defect Count**: observed defects in each sampled lot.
- **Sample Size**: number of tested items per lot.
- **Lot Size**: total items per lot.
- **Time**: sequence/order index for lots.
- **Predictor** (optional): external covariate when model type includes predictor.

### Settings
- **Acceptance limits**: lower (AQL) and upper (RQL) defect-rate limits.
- **Sudden jump tail correction**: optional post hoc correction and weight.
- **Decision rule**: thresholds for accept/reject decisions based on posterior probabilities.

### Model
- Prior for initial defect rate `theta_1` (Beta shape parameters).
- Prior for state-noise `sigma`.
- Prior scale for predictor coefficient `beta` (predictor model only).

### MCMC
- Burnin, posterior samples, chains, thinning, seed.
- HMC tuning: adapt-delta and max tree depth.
- Optional MCMC diagnostics table with selectable parameters.

## Output
-------
- **Posterior Summary (Current Lot)**: observed defects, posterior predictive summary, and acceptance decision.
- **MCMC Diagnostics** (optional): mean, SD, ESS, and R-hat for selected parameters.
- **Plots** (as selected):
 - Defect-rate state trajectory over time.
 - Posterior predictive distribution for current lot.
 - Posterior distribution of latest defect rate.
 - Posterior distribution of predictor coefficient `beta`.
