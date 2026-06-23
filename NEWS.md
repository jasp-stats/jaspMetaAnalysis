# jaspMetaAnalysis Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
>
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md).
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspMetaAnalysis (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Changed`, `## Fixed`, etc.).
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspMetaAnalysis/issues/19))`).
> * **Format Categories:**
>   * **Added:** New analyses, features, options, or output.
>   * **Changed:** Updates to existing analyses, defaults, dependencies, or output.
>   * **Fixed:** Bug fixes in analyses, plots, tables, help, or QML layouts.
>   * **Deprecated / Removed:** Outdated analyses, options, or legacy code.

---
# jaspMetaAnalysis 0.96.6
## Fixed
* Diagnostics handling for location-scale models

# jaspMetaAnalysis 0.95.5

## Added
* Added Classical Generalized (GLMM) Meta-Analysis.
* Added Effect Size Aggregation for clustered/dependent effect sizes.
* Added Forest Plot analysis.
* Added Risk of Bias Plot analysis for RoB 2, ROBINS-I, and QUADAS-2.
* Added dataset exports for model-derived fit information in classical analyses, including diagnostics, residuals, predicted values, true-effect estimates, random effects, and weights.

## Changed
* Improved forest plots with transformed axis labels, manual ticks, axis limits, reference lines, weights, metadata panels, and subgroup/aggregation displays.
* Improved forest plot handling of estimates and intervals outside the plotting range.
* Updated dependencies and test coverage for new analyses and plots.

## Fixed
* Fixed footnote handling in classical meta-analysis moderator-test tables.
* Improved/fixed transformation notes for nonlinear estimates and marginal means.
* Fixed small help/output typos.
