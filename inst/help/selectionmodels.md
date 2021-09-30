Selection Models
===

Selection models are meta-analytic methods on the fixed and random effects meta-analytic models that adjust for publication bias operating on p-values (Iyengar & Greenhouse, 1988; Vevea & Hedges, 1995). The analysis allow users to specify the selection process using either one or two-sided p-value cutoffs, which are internally transformed to one-sided p-value cutoffs prior to the fitting process.

### Input
---
#### Input type
- Effect sizes & SE: Specifying input using any other types of effect sizes and standard errors. One-sided p-values can be supplied as an optional input, if not provided, the p-values are computed using z-aproximation.
- Correlations & N: Specifying input using correlations and the sample size. Note that the effect size is internally transformed to Cohen's d scale for model estimation. The summarized output is transformed back to correlations (apart from the heterogeneity parameter tau that is always summarized on the transformed scale). One-sided p-values can be supplied as an optional input, if not provided, the p-values are computed using from the given correlation coefficients and sample sizes.

#### Data
- Effect Size: Effect sizes of the studies.
- Effect Size Standard Error: Standard errors of the effect sizes per study. Must always be positive.
- N: Overall sample sizes of the studies. Only possible if the supplied effect sizes are correlations.
- P-value (one-sided): One-sided p-values of the studies (optional, if not provided, the p-values are computed based on the rest of input.)


### Models
---
- P-value cutoffs: A vector of p-value cutoffs that separate intervals with different publication probabilities (estimated by the weights).
- P-value frequency: Creates a table with frequencies of p-values in each p-value interval, specified by the p-value cutoffs.
- Expected effect size direction: The direction of the expected effect size (the publication bias adjusted models are not symmetrical around zero due to the estimation of weights).
- Two-sided selection: Whether the specified p-values cutoffs correspond to one-sided or two-sided values. Note that the two-sided p-values cutoffs are internally transformed into one-sided p-values cutoffs with different weights at each side of the p-value distribution.
- Automatically join p-value intervals: Automatically joins p-value intervals with fewer than 4 p-values to reduce estimation issues.
- Transform correlations: Type of transformation to be applied prior to estimating models when correlation coefficients are supplied as input. The estimates and figures display mean estimates transformed back to the correlation scale (apart from the heterogeneity parameter tau that is always summarized on the transformed scale).


### Inference
---
#### Fixed Effects
- Mean estimates: Summarizes mean effect size estimates of the non-adjusted and adjusted fixed effects models.
- Estimated weights: Summarizes estimated publication bias weights of the adjusted fixed effects model.

#### Random Effects
- Mean estimates: Summarizes mean effect size estimates of the non-adjusted and adjusted fixed effects models.
- Estimated heterogeneity: Summarizes heterogeneity estimates of the non-adjusted and adjusted random effects models.
- Estimated weights: Summarizes estimated publication bias weights of the adjusted random effects model.


### Plots
---
#### Weight function
Visualizes the estimated publication bias weights as a weight function.
  - Fixed effects: Visualizes the weight function of the adjusted fixed effect model.
  - Random effects: Visualizes the weight function of the adjusted random effect model.
  - Rescale x-axis: Make the differences between the individual ticks on x-axis equal for easier interpretability.

#### Mean model estimates
Visualizes mean effect size estimates from all fitted models.


### References
---
- Iyengar, S., & Greenhouse, J. B. (1988). Selection models and the file drawer problem. Statistical Science, 109-117.
- Vevea, J. L., & Hedges, L. V. (1995). A general linear model for estimating effect size in the presence of publication bias. Psychometrika, 60(3), 419-435.


### R-packages
---
- weightr
