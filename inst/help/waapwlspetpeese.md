WAAP-WLS and PET-PEESE
===

The WAAP-WLS and PET-PEESE are meta-analytic methods based on weighted least squares. Instead of the classical (random or fixed) meta-analyses that assume additive heterogeneity, WAAP-WLS and PET-PEESE assume multiplicative heterogeneity. Furthermore, WAAP, PET, and PEESE incorporate different adjustments of selection processes. See Carter et al. (2019) for an overview of the methods.
- WLS is the default meta-analytic model that assumes multiplicative heterogeneity (Stanley & Doucouliagos, 2017).
- WAAP adjusts for selection processes by using only high-powered studies -- a WLS model fitted only with studies that would have at least 80% power to detect the WLS meta-analytic effect size estimate based on all studies (Ioannidis, Stanley, & Doucouliagos, 2017; Stanley et al., 2017).
- PET adjusts for selection processes by adjusting for the relationship between effect sizes and standard errors (Stanley & Doucouliagos, 2014; Stanley, 2017).
- PEESE adjusts for selection processes by adjusting for the relationship between effect sizes and standard errors squared (Stanley & Doucouliagos, 2014; Stanley, 2017).

WAAP-WLS and PET-PEESE are usually used as conditional estimators. In WAAP-WLS, WAAP is used if there are enough (at least 3) highly powered studies, otherwise WLS is used (Ioannidis, Stanley, & Doucouliagos, 2017; Stanley et al., 2017). In PET-PEESE, PET is used to test for the presence of the effect size with alpha = 0.10, the PEESE effect size estimate is used if the PET's test for effect size is significant, otherwise PET effect size estimate is used (Stanley & Doucouliagos, 2014; Stanley, 2017).

### Input
---
#### Input type
- Effect sizes & SE: Specifying input using any other types of effect sizes and standard errors.
- Correlations & N: Specifying input using correlations and the sample size. Note that the effect size is internally transformed to Fisher's z scale for model estimation. The summarized output is transformed back to correlations.

#### Data
- Effect Size: Effect sizes of the studies.
- Effect Size Standard Error: Standard errors of the effect sizes per study. Must always be positive.
- N: Overall sample sizes of the studies. Only possible if the supplied effect sizes are correlations.
- Transform correlations: Type of transformation to be applied prior to estimating models when correlation coefficients are supplied as input. The estimates and figures display mean estimates transformed back to the correlation scale (apart from the meta-regression estimates for the standard error /standard errors squared).


### Inference
---
#### Mean Estimates
Summarizes the mean effect size estimates.

#### PET-PEESE Regression Estimates
Summarizes the regression coefficients of the relationship between effect sizes and standard errors (PET) and effect sizes and standard errors squared (PEESE).

#### Multiplicative Heterogeneity Estimates
Summarizes the multiplicative heterogeneity estimates.


### Plots
---
#### Meta-Regression Estimate
Visualizes the estimated meta-regression of the relationship between the effect sizes and standard errors.

#### Mean model estimates
Visualizes mean effect size estimates from all fitted models.


### References
---
Carter, E. C., Schönbrodt, F. D., Gervais, W. M., & Hilgard, J. (2019). Correcting for bias in psychology: A comparison of meta-analytic methods. Advances in Methods and Practices in Psychological Science, 2(2), 115-144.

Ioannidis, J. P., Stanley, T. D., & Doucouliagos, H. (2017). The power of bias in economics research.

Stanley, T. D., Doucouliagos, H., & Ioannidis, J. P. (2017). Finding the power to reduce publication bias.Statistics in medicine, 36(10), 1580-1598.

Stanley, T. D., & Doucouliagos, H. (2014). Meta‐regression approximations to reduce publication selection bias. Research Synthesis Methods, 5(1), 60-78.

Stanley, T. D., & Doucouliagos, H. (2017). Neither fixed nor random: weighted least squares meta‐regression. Research synthesis methods, 8(1), 19-42.

Stanley, T. D. (2017). Limitations of PET-PEESE and other meta-analysis methods. Social Psychological and Personality Science, 8(5), 581-591.


### R-packages
---
The implementation is based on the supplementary materials of Carter et al., (2019).
