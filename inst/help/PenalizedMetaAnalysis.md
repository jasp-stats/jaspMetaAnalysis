Penalized Meta-Analysis
===

Penalized meta-analysis allows the user to estimate Bayesian horseshoe and lasso penalized (multilevel) random-effect meta-regression models. See Van Lissa and colleagues (2023) for more details.

### Input
---
#### Assignment Box
- Effect Size: Effect size of the studies. 
- Effect Size Standard Error: Standard errors of the effect sizes per study. Must always be positive. A 95% CI for the effect sizes can be provided instead. 
- Method: Specify the regularization method for meta-regression.
  - Horseshoe: Regularization using the horseshoe prior.
  - Lasso: Regularization using the lasso prior.
- Covariates: Continuous predictor variable(s). If ordinal variables are entered it is assumed that their levels are equidistant. Hence, ordinal variables are treated as continuous predictor variables.
- Factors: Categorical predictors variable(s). Ordinal variables here are treated as categorical predictor variables, thus, the ordinal information is ignored.
- Clustering: Variable indicating the presence of higher level cluster. If selected, a three-level meta-regression is estimated.

### Model
---

#### Components and model terms: 
  - Components: All the independent variables that can be included in the model. 
  - Model terms: The independent variables in the model. By default, all the main effects of the specified independent variables are included in the model. To include interactions, click multiple variables (e.g., by holding the ctrl/cmd button on your keyboard while clicking) and drag those into the `Model Terms` box. 

#### Include intercept:
- Include the intercept in the regression model.

#### Scale predictors
- Scale the continuous predictors.

### Priors
---

#### Horshoe
Available only if horseshoe `Method` is selected.
- Df: Degrees of freedom.
- Scale: Scale.

#### Lasso
Available only if lasso `Method` is selected.
- Df: Degrees of freedom.
- Df (global): Global degrees of freedom.
- Df (slab): Degrees of freedom for the slab.
- Scale (global): Global scale.
- Scale (slab): Scale for the slab.


### Inference
---
#### Estimates table
- Displays table summarizing the posterior distribution of the model terms.

#### Heterogeneity table
- Displays table summarizing the posterior distribution of the between-study heterogeneity estimate τ (and its square, τ<sup>01</sup>).
  - Displays table summarizing the posterior distribution of the relative heterogeneity I<sup>01</sup>.


### MCMC Diagnostics
---
#### Model terms
- Model terms whose MCMC chains can be diagnosed.

#### Plotted term
- Model term which MCMC chains will be diagnosed.

#### Plot type
- Different types of MCMC diagnostics plots.
  - Traceplot: Traceplot of the individual chains.
  - Scatterplot: Scatterplot of two model terms.
  - Histogram: Histogram of the posterior samples.
  - Density: Overlying densities of samples from each chain.
  - Autocorrelations: Average autocorrelations across all chains.


### Advanced
---
#### Estimation settings (MCMC)
- Burnin: Number of iterations reserved for burnin.
- Iterations: Number of iterations reserved for sampling.
- Chains: Number of chains.
- Adapt delta: Average target proposal acceptance of each step. Increasing `Adapt delta` results in better-behaved chains, but also longer fitting times.
- Maximum treedepth: The cap for number of trees evaluated during each iteration. Prevents excessively long execution times.

#### Repeatability
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. Note, however, that the seed may not reproduce the same results across operating systems.


### References
---
- Van Lissa, C. J., van Erp, S., & Clapper, E.-B. (2023). Selecting relevant moderators with Bayesian regularized meta-regression. Research Synthesis Methods, 14(2), 301–322. https://doi.org/10.1002/jrsm.1628

### R-packages
---
- pema
