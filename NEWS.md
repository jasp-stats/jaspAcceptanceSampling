# jaspAcceptanceSampling Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspAcceptanceSampling (development version)

## Changed
* Bayesian State-Space Sampling accepts multiple numeric covariates with additive effects, separate standardization and independent coefficient priors using one adjustable Cauchy scale. Coefficient outputs identify each covariate, and redundant covariates are rejected.
* Clarified state-space prior and decision-threshold labels and expanded the help with model equations, predictor interpretation, strict decision rules, smoothing/refitting behavior and predictive plot limits. Statistical settings and calculations are unchanged.
* Bayesian State-Space Sampling now uses a two-component innovation mixture instead of the Gaussian state model and post-hoc predictive correction supplied in the earlier beta. Recalculating saved beta analyses fits the new model, so results may differ. Applicable data assignments, priors, MCMC settings and decision thresholds are preserved; the removed correction settings are no longer used.

## Fixed
* State-space coefficient density curves extend beyond the sampled range instead of stopping at the central 99% interval, preventing visibly cut-off tails when plot axes are expanded. Posterior draws and coefficient summaries are unchanged.
* Saved state-space fits without the current model-version marker are discarded before recalculation, preventing reuse of an older model's posterior samples.



---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
