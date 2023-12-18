Robust Bayesian Meta-Analysis
===

Robust Bayesian meta-analysis allows the user to specify a wide range of meta-analytic models, combine their estimates using model averaging, and quantify evidence for different hypotheses using Bayes factors. The analysis allows the user to specify various prior distributions for effect sizes and heterogeneity and incorporate models correcting for publication bias with selection models and PET-PEESE. 

Please note that we updated the model specification of RoBMA models with the version of JASP 0.15. The default model specification corresponds to RoBMA-PSMA described in Bartoš and colleagues (2023).

### Input
---
The input supplied as standardized effect sizes are internally transformed to Fisher's z statistics (as a variance stabilizing transformation). The results are shown the same scale that is used to specify the prior distributions (Cohen's d by default, can be changed only for the "Custom" `model type`.)

#### Input type
- Cohen's d: Specifying input using either Cohen's d effect sizes and the sample size or standard errors.
- Correlations: Specifying input using correlations and the sample size or standard errors. 
- Log odds ratios: Specifying input using log odds ratios and the standard errors.
- General effect sizes & SE: Specifying input using any other, unstandardized, types of effect sizes and the corresponding standard errors. 
- Fitted model: Specify a path to already fitted RoBMA model using R. The model must be saved as an RDS file.

#### Data
- Effect Size: Effect sizes of the studies.
- Effect Size Standard Error: Standard errors of the effect sizes per study. Must always be positive. A 95% CI for the effect sizes can be provided instead. In case that the effect sizes are measured by Cohen's d the sample sizes can be provided instead.
- 95% CI Lower and Upper Bound: 95% CI lower and upper bounds of the effect sizes per study. Must be separated into two columns. The standard errors of the effect sizes can be provided instead.
- N: Overall sample sizes of the studies. Only possible if the studies effect sizes are measured by Cohen's d or correlations. Effect Size Standard Errors can be provided instead.
- Study Labels: Optional argument that will add study labels to the output.

#### Expected direction of effect sizes
The direction of the expected effect size (the publication bias adjusted models with one-sided weight functions and PET-PEESE publication bias adjustments are not symmetrical around zero).

#### Model type
Either one of the three pre-specified model types corresponding to the models introduced in Bartoš et al. (2023) and Maier, Bartoš & Wagenmakers (2023), or a custom ensemble.
- RoBMA-PSMA corresponds to the 36 model ensemble that combines selection models and PET-PEESE adjustment for publication bias adjustment component (from Bartoš et al., 2023)
- RoBMA-PP corresponds to the 12 model ensemble that uses PET-PEESE adjustment for publication bias adjustment component (from Bartoš et al., 2023)
- RoBMA-original corresponds to the 12 model ensemble that uses two two-sided weight functions for publication bias adjustment component (from Maier, Bartoš & Wagenmakers, 2023)
- Custom allows specifying a custom model ensemble under the `Models` section

#### Prior scale
Scale that will be used for specifying the prior distributions. Defaults to Cohen's d and can be changed only with a "Custom" `Model type` .

#### Plot prior distributions
Displays the specified prior density function(s).


### Models
---
The individual models that form up the robust Bayesian meta-analysis are defined by creating combinations of all specified priors for the effect size/heterogeneity/publication bias. The individual models' prior odds are obtained by multiplying the prior odds of prior distributions for each of the parameter that forms the model.

#### Effect / Heterogeneity
Sets a prior distribution(s) for the effect size or heterogeneity.
- Distribution: Name and parametrization of the distribution.
  - Normal(μ,σ): Normal distribution parametrized by mean (μ) and standard deviation (σ).
  - Student's t(μ,σ,v): Generalized Student's t distribution parametrized by location (μ), scale (σ), and degrees of freedom (v).
  - Cauchy(x₀,θ): Cauchy distribution parametrized by location (μ) and scale (σ).
  - Gamma(α,β): Gamma distribution parametrized by shape (α) and rate (β).
  - Gamma(k,θ): Gamma distribution parametrized by shape (k) and scale (θ).
  - Inverse-Gamma(α,β): Inverse-Gamma distribution parametrized by shape (α) and scale (β).
  - Log-Normal(μ,σ): Lognormal distribution parametrized by mean (μ) and standard deviation (σ) on the log scale.
  - Beta(α,β): Beta distribution parametrized by alpha (α) and beta (β)
  - Spike(x₀): Point density parametrized by location (x₀).
  - Uniform(a,b): Uniform distribution parametrized by lower bound (a) and upper bound (b).
  - None: Absence of the parameter, defaults to a Spike(0).
- Parameters: Values for parameters of the selected distribution.
- Truncation: Lower and upper truncation of the distribution.
- Prior Weights: Prior weight of the distribution.

#### Publication bias: selection models
Sets a prior distribution(s) for the parameters of the weight function specifying the publication bias adjustment component.
- Distribution: Name and parametrization of the distribution.
  - Two-sided: Prior distribution for a two-sided weight function characterized by a vector of cut points on p-values (p-values) and vector alpha (α). The vector alpha (α) determines the alpha parameter of Dirichlet distribution which cumulative sum is used for the weights omega. The first element of α corresponds to the weight for the highest p-value interval.
  - One-sided: Prior distribution for a one-sided weight function characterized by a vector of cut points on p-values (p-values) and vector alpha (α). The vector alpha (α) determines the alpha parameter of Dirichlet distribution which cumulative sum is used for the weights omega. The first element of α corresponds to the weight for the highest p-value interval.
  - Two-sided-fixed: Prior distribution for a two-sided weight function characterized by a vector of cut points on p-values (p-values) and vector of weights omega (ω).
  - One-sided-fixed: Prior distribution for a one-sided weight function characterized by a vector of cut points on p-values (p-values) and vector of weights omega (ω).
  - None: Prior distribution assuming lack of publication bias.
- Parameters: Values for parameters of the selected distribution.
- Prior Weights: Prior weight of the distribution.

#### Publication bias: PET / PEESE
Sets a prior distribution(s) for the parameters of the PET / PEESE regression coefficient specifying the publication bias adjustment component.
- Distribution: Name and parametrization of the distribution.
  - Normal(μ,σ): Normal distribution parametrized by mean (μ) and standard deviation (σ).
  - Student's t(μ,σ,v): Generalized Student's t distribution parametrized by location (μ), scale (σ), and degrees of freedom (v).
  - Cauchy(x₀,θ): Cauchy distribution parametrized by location (μ) and scale (σ).
  - Gamma(α,β): Gamma distribution parametrized by shape (α) and rate (β).
  - Gamma(k,θ): Gamma distribution parametrized by shape (k) and scale (θ).
  - Inverse-Gamma(α,β): Inverse-Gamma distribution parametrized by shape (α) and scale (β).
  - Log-Normal(μ,σ): Lognormal distribution parametrized by mean (μ) and standard deviation (σ) on the log scale.
  - Beta(α,β): Beta distribution parametrized by alpha (α) and beta (β)
  - Spike(x₀): Point density parametrized by location (x₀).
  - Uniform(a,b): Uniform distribution parametrized by lower bound (a) and upper bound (b).
  - None: Prior distribution assuming lack of publication bias.
- Parameters: Values for parameters of the selected distribution.
- Truncation: Lower and upper truncation of the distribution.
- Prior Weights: Prior weight of the distribution.

#### Set null priors
Allows specifying prior distributions for the null models.


### Inference
---
#### Conditional estimates
Displays estimates assuming that the alternative models are true.

#### Models overview
Displays overview of the specified models.
- BF: Show different types of Bayes factors
  - Inclusion: Change from prior to posterior odds for each individual model.
  - vs. Best: Bayes factor comparing to the best fitting model.
  - vs. Previous: Bayes factor comparing to a better fitting model.
- Order: Order the overview of displayed models.
  - Model number: Based on the order of each model.
  - Bayes factor: Based on the inclusion Bayes factor of each model.
  - Posterior prob.: Based on the posterior probability of each model.

#### Individual models
Displays a detailed overview of each specified model.
- Single model: Display the overview for only one of the specified models.

#### Bayes Factor
- BF<sub>10</sub>: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- BF<sub>01</sub>: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- Log(BF<sub>01</sub>) : Natural logarithm of BF<sub>01</sub>.

#### CI width
Width of the credible intervals.

#### Output scale
Effect size scale for summarizing the numerical estimates. Only available for standardized effect sizes.

#### Shorten prior names
Abbriviates names of the prior distributions.


### Plots
---

#### Forest plot
Displays a forest plot with the observed effect sizes and the estimated overall effect size(s).
- Order
  - Ascending: Displays the effect sizes in the forest plot in ascending order.
  - Descending: Displays the effect sizes in the forest plot in descending order.
  - Row order: Displays the effect sizes in the forest plot in the same order as in the provided data.

#### Type
  - Model-averaged: Shows the model-averaged effect size estimate based on all specified models.
  - Conditional: Shows the conditional effect size estimate based on models assuming the presence of the effect.

---
#### Pooled estimates
- Effect: Displays a plot with the estimated pooled effect size.
- Heterogeneity: Displays a plot with the estimated heterogeneity parameter tau.
- Weights function: Displays the estimated weight function (the grey area corresponds to a 95% credible interval).
  - Rescale x-axis: Makes the differences between the individual ticks on x-axis equal for easier interpretability. 
- PET-PEESE: Displays the estimated PET-PEESE regression line.

#### Type
  - Model-averaged: Shows the model-averaged effect size/heterogeneity/weights function/PET-PEESE estimate based on all specified models.
  - Conditional: Shows the conditional effect size/heterogeneity/weights function/PET-PEESE estimate based on models assuming presence of the effect/heterogeneity/weights functions/PET-PEESE.

#### Show priors
Displays prior distribution density on top of the pooled estimates figures.

---
#### Individual models
Displays estimates from each individual model included in the ensemble.
- Effect: Display the estimated pooled effect size for each individual model.
- Heterogeneity: Display the estimated heterogeneity parameter tau for each individual model.

#### Type
  - Model-averaged: Shows estimates from all models and the model-averaged effect size/heterogeneity estimate based on all specified models.
  - Conditional: Shows estimates from models assuming presence of the effect/heterogeneity and the conditional effect size/heterogeneity estimate based on models assuming presence of the effect/heterogeneity/weights functions/PET-PEESE.

#### Order
Order the displayed individual model estimates the following order:
- Ascending
- Descending

#### Order by
Orders the displayed individual model estimates according to:
- Model number: Based on the order of each model.
- Estimate: Based on the estimated parameter of each model
- Bayes factor: Based on the inclusion Bayes factor of each model.
- Posterior prob.: Based on the posterior probability of each model.


### MCMC Diagnostics
---
#### Overview
Displays overview of the individual model diagnostics. The table summarizes the maximum MCMC error, the maximum of MCMC error divided by SD, the minimal estimated sample size, and maximum R-hat per model. More detailed, per-parameter, fit diagnostics can be accessed by displaying individual model summary in the `Inference` tab.

#### Plot
Displays chains summaries according to the selected type for each of the models for the selected parameters:
- Effect: The pooled effect size estimate.
- Heterogeneity: The estimated heterogeneity parameter tau.
- Weights: The estimated weights corresponding to the p-values cut-offs.
- PET: The estimated PET regression coefficients.
- PEESE: The estimated PEESE regression coefficients.

#### Type
Type of the chains summaries to be displayed.
- Trace: Displays the overlaying traces of each chain for the selected parameters. Different chains are visualized with a different colors.
- Autocorrelation: Displays the average autocorrelations of the chains for the selected parameters.
- Posterior sample densities: Displays the overlaying densities of samples from each chain for the selected parameters. Different chains are visualized with a different colors.

#### Single model
Display chains summaries for only a specific model.


### Advanced
---
#### Estimation scale
Effect size scale to be used when estimating the models. We advise using Fisher's z transformation since it acts as a variance stabilizing transformation (BF for the presence of publication bias might be biased under the PET-PEESE adjustment for publication bias otherwise). Only available for the standardized effect sizes.

#### Estimation settings (MCMC)
- Adaptation: Sets the number of iterations to be used for adapting the MCMC chains.
- Burnin: Sets the number of iterations to be used for burnin of the MCMC chains.
- Iterations: Sets the number of iterations to be used for sampling from the MCMC chains.
- Chains: Sets the number of chains for the MCMC estimation.
- Thin: Sets the thinning of the MCMC chains.

#### Autofit
The MCMC estimation for each model continues until the maximum fitting time or MCMC diagnostics criterion was reached.
- R-hat: The target R-hat of parameter estimates for terminating the automatic estimation procedure.
- ESS: The target estimated sample size of parameter estimates for terminating the automatic estimation procedure.
- MCMC error: The target MCMC error of parameter estimates for terminating the automatic estimation procedure (note that PEESE regression coefficient has usually much larger scale than the rest of the ensemble and using this option might significantly prolong the fitting time).
- MCMC error / SD: The target MCMC error standardized by the standard deviation of the posterior distribution of parameter estimates for terminating the automatic estimation procedure.
- Maximum fitting time: The maximum fitting time per model.
- Extend samples: The number of samples to be added on each autofit iteration.

#### Remove failed models
Removes models that do not satisfy the convergence checks (specified under `Autofit`)

#### Rebalance component probability on model failure
Balances the prior model probability across models with the same combinations of compontents assuming the presence/absence of the effect/heterogeneity/publication bias in the case of non-convergence (if possible).

#### Repeatability
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.


### References
---
- Maier, M., Bartoš, F., & Wagenmakers, E. J. (2023). Robust Bayesian meta-analysis: Addressing publication bias with model-averaging. Psychological Methods 28 (1), 107-122.  https://doi.org/10.1037/met0000405
- Bartoš, F., Maier, M., Wagenmakers, E. J., Doucouliagos, H., & Stanley, T. D. (2023). Robust Bayesian meta-analysis: Model-averaging across complementary publication bias adjustment methods. Research Synthesis Methods 14 (1), 99-116. https://doi.org/10.1002/jrsm.1594
- Bartoš, F., Maier, M., Quintana, D. S., & Wagenmakers, E. J. (2022). Adjusting for publication bias in JASP & R: Selection models, PET-PEESE, and robust Bayesian meta-analysis. Advances in Methods and Practices in Psychological Science 5 (3), 1-19. https://doi.org/10.1177/25152459221109259

### R-packages
---
- RoBMA
- BayesTools
