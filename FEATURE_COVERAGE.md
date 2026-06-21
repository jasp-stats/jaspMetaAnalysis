# Function Coverage

## metafor (5.0-1)

| Function | Analysis |
| --- | --- |
| `metafor::rma()` | Classical Meta-Analysis; Funnel Plot. Equal-, fixed-, and random-effects meta-analysis; mixed-effects meta-regression; location-scale models for heterogeneity; subgroup-specific model fitting; multiple heterogeneity estimators; z, t, and Knapp-Hartung coefficient tests; prediction intervals; conditional estimates; estimated marginal means; fitted model code display; H1 funnel parameter estimation. |
| `metafor::rma.mv()` | Classical Meta-Analysis (Multilevel/Multivariate). Multilevel and multivariate meta-analysis for dependent effect sizes; moderator models; random-effect/model components; nested, crossed, structured, autoregressive, spatial, and known-correlation structures; sparse-matrix and optimizer controls; subgroup-specific model fitting; prediction intervals; conditional estimates; estimated marginal means; fitted model code display. |
| `metafor::rma.glmm()` | Generalized Meta-Analysis. GLMM meta-analysis from event counts or person-time data; log odds ratio, log risk ratio, risk difference, and log incidence rate ratio models; unconditional fixed-study-effects, unconditional random-study-effects, conditional approximate-likelihood, and conditional exact-likelihood model types where available; equal-effects and random-effects estimation; moderator models; fitted model code display. |
| `metafor::rma.mh()` | Mantel-Haenszel / Peto. Mantel-Haenszel meta-analysis for frequency and event-rate outcomes; log odds ratio, log risk ratio, risk difference, log incidence rate ratio, and incidence rate difference measures; continuity-correction options; subgroup-specific model fitting; forest plot summaries. |
| `metafor::rma.peto()` | Mantel-Haenszel / Peto. Peto odds-ratio meta-analysis for binary outcomes; continuity-correction options; subgroup-specific model fitting; forest plot summaries. |
| `metafor::escalc()` | Effect Size Computation; Effect Size Aggregation; model and plot preparation. User-facing effect-size computation across independent-groups, association, single-group, repeated-measures, reliability, partial-correlation, model-fit, heterozygosity, and reported-effect-size designs; standardized effect sizes, ratios, correlations, binary outcomes, incidence-rate outcomes, and variance computation. |
| `metafor::aggregate.escalc()` | Effect Size Aggregation. Aggregation of multiple effect sizes within a cluster; inverse-variance weighting; optional cluster-size output; user-defined output column names. |
| `metafor::vcalc()` | Classical Meta-Analysis (Multilevel/Multivariate); Effect Size Aggregation. Variance-covariance matrix construction for dependent effect sizes; cluster-based dependence; subgrouped dependence; construct/type variables; within-study correlations; contrast groups; weights for contrast-based dependence. |
| `metafor::robust()` | Classical Meta-Analysis; Classical Meta-Analysis (Multilevel/Multivariate). Cluster-robust coefficient tests and confidence intervals when a clustering variable is supplied. |
| `metafor::permutest()` | Classical Meta-Analysis; Classical Meta-Analysis (Multilevel/Multivariate). Permutation tests for effect-size moderation and heterogeneity moderation; omnibus and term-level tests. |
| `metafor::trimfill()` | Funnel Plot. Trim-and-fill publication-bias adjustment; missing-study estimates; adjusted funnel plot; trim-and-fill parameter table. |
| `metafor::regtest()` | Funnel Plot. Funnel-plot asymmetry tests through meta-regression and weighted-regression/Egger-style tests. |
| `metafor::ranktest()` | Funnel Plot. Rank-correlation/Begg-style funnel-plot asymmetry test. |
| `metafor::fsn()` | Funnel Plot. Fail-safe N calculations, including Rosenthal, Orwin, Rosenberg, and general methods. |
| `metafor::profile.rma.mv()` | Classical Meta-Analysis (Multilevel/Multivariate). Profile-likelihood diagnostics for heterogeneity components. |
| `metafor::baujat()` | Classical Meta-Analysis diagnostics. Baujat/influence diagnostic plot for contribution to heterogeneity and fitted effect. |
| `metafor::vif()` | Classical Meta-Analysis diagnostics. Variance inflation factor diagnostics for moderator terms in effect-size and heterogeneity models. |
| `metafor::transf.*()` family | Classical Meta-Analysis outputs; Forest Plot; Funnel Plot; Effect Size Computation. User-facing effect-size scale transformations for estimates, intervals, plots, and computed effect sizes, including Fisher-z/correlation, log-odds/proportion, standardized mean difference/log-odds, reliability, R-squared, and common-language style transformations. |

Major `metafor` functions not implemented yet:

- `metafor::rcalc()` for variance-covariance matrices of overlapping correlations.
- `metafor::selmodel()` for selection models.
- `metafor::hc()` for Henmi-Copas adjustment.
- `metafor::tes()` for test of excess significance.
- `metafor::cumul()` for cumulative meta-analysis.
- `metafor::leave1out()` for leave-one-out diagnostics as a distinct analysis/table.
- `metafor::ranef()` / `metafor::blup()` for study-level random-effects predictions.
- `metafor::labbe()` for L'Abbe plots.
- `metafor::radial()` for radial/Galbraith plots.
- `metafor::gosh()` for GOSH diagnostics.
- `metafor::qqnorm()` for normal QQ diagnostics.

## RoBMA (3.6.1)

| Function | Analysis |
| --- | --- |
| `RoBMA::RoBMA()` / `RoBMA::RoBMA.reg()` | Robust Bayesian Meta-Analysis. Robust Bayesian meta-analysis and meta-regression; model averaging over effect, heterogeneity, moderation, and publication-bias components; RoBMA-PSMA, RoBMA-PP, original RoBMA, custom, or no publication-bias adjustment; subgroup and multilevel analyses; forest/bubble plots and MCMC diagnostics. |
| `RoBMA::NoBMA()` / `RoBMA::NoBMA.reg()` | Bayesian Meta-Analysis. Bayesian normal meta-analysis and meta-regression without publication-bias adjustment; model averaging over effect, heterogeneity, and moderation; subgroup and multilevel analyses; estimated marginal means, plots, and diagnostics. |
| `RoBMA::BiBMA()` / `RoBMA::BiBMA.reg()` | Bayesian Binomial Meta-Analysis. Binomial Bayesian meta-analysis/meta-regression from successes and sample sizes; log-odds-ratio model; effect, heterogeneity, moderation, and baseline priors; subgroup and multilevel analyses; plots and diagnostics. |
| `RoBMA::set_default_priors()` / `RoBMA::prior_informed()` / `RoBMA::set_default_binomial_priors()` | Bayesian Meta-Analysis; Bayesian Binomial Meta-Analysis; Robust Bayesian Meta-Analysis. Default, medicine/Cochrane, binomial baseline, null, and scaled prior distributions for effect, heterogeneity, moderators, and publication bias. |
| `RoBMA::prior()` / `RoBMA::prior_factor()` / `RoBMA::prior_none()` | Bayesian Meta-Analysis; Bayesian Binomial Meta-Analysis; Robust Bayesian Meta-Analysis. Custom continuous, factor, spike/null, baseline, and empty prior specifications exposed through custom-prior UI. |
| `RoBMA::prior_weightfunction()` / `RoBMA::prior_PET()` / `RoBMA::prior_PEESE()` | Robust Bayesian Meta-Analysis. Custom publication-bias model components for selection/weight functions, PET, and PEESE. |
| `RoBMA::set_autofit_control()` | Bayesian Meta-Analysis; Bayesian Binomial Meta-Analysis; Robust Bayesian Meta-Analysis. Autofit controls for R-hat, ESS, MCMC error, maximum fitting time, and sample extension. |
| `RoBMA::marginal_summary()` / `RoBMA::marginal_plot()` | Bayesian Meta-Analysis; Bayesian Binomial Meta-Analysis; Robust Bayesian Meta-Analysis. Estimated marginal means summaries and plots for meta-regression. |
| `RoBMA::pooled_effect()` / `RoBMA::adjusted_effect()` / `RoBMA::summary_heterogeneity()` | Bayesian Meta-Analysis; Bayesian Binomial Meta-Analysis; Robust Bayesian Meta-Analysis. Pooled/adjusted effects, heterogeneity summaries, credible intervals, and prediction intervals. |
| `RoBMA::diagnostics()` / `RoBMA::check_RoBMA()` | Bayesian Meta-Analysis; Bayesian Binomial Meta-Analysis; Robust Bayesian Meta-Analysis. MCMC diagnostic plots and model-fit warnings. |
| `RoBMA::r2d()` / `RoBMA::r2z()` / `RoBMA::d2r()` / `RoBMA::z2r()` / `RoBMA::se_*()` family | Selection Models; PET-PEESE. Correlation input transformations to Cohen's d or Fisher's z, inverse transformations, and standard-error conversions. |

Major `RoBMA` functions/features not implemented yet:

- Native `RoBMA::forest()` and `RoBMA::funnel()` plotting methods.
- `RoBMA::plot_models()` model composition/probability plots.
- `RoBMA::predict()` and `RoBMA::true_effects()` for prediction and study-level true-effect summaries.
- `RoBMA::residuals()` residual diagnostics.
- `RoBMA::extract_posterior()` for direct posterior-sample extraction/export.
- `RoBMA::as_zcurve()` and `plot()` / `summary()` methods for z-curve diagnostics.
- `RoBMA::interpret()` automatic textual model interpretation.
- `RoBMA::check_setup()` / `RoBMA::check_setup.reg()` / `RoBMA::check_setup.BiBMA()` model-ensemble setup previews.
- `RoBMA::update()` methods for refitting/updating fitted RoBMA, NoBMA, or BiBMA objects.

## pema (0.1.5)

| Function | Analysis |
| --- | --- |
| `pema::brma()` | Penalized Meta-Analysis. Bayesian regularized meta-regression with lasso/horseshoe priors; covariates, factors, interaction terms, optional clustering, predictor scaling, MCMC controls, coefficient and heterogeneity tables. |
| `pema::I2()` | Penalized Meta-Analysis. Optional I2 heterogeneity table, including clustered within/between components when available. |
| `pema::as.stan()` | Penalized Meta-Analysis. Extracts Stan fit for selected posterior plots and MCMC diagnostics: trace, scatter, histogram, density, and autocorrelation. |

Major `pema` functions/features not implemented yet:

- `pema::plot_sensitivity()` for BRMA posterior sensitivity plots.
- `pema::sample_prior()` / `pema::shiny_prior()` for prior sampling/exploration.
- `pema::maxap()` for maximum-a-posteriori summaries.
- `pema::simulate_smd()` for simulated meta-analytic datasets.
- Multiple-imputation/list-data workflows supported by `pema::brma()` are not exposed in the QML.

## metamisc (0.4.0)

| Function | Analysis |
| --- | --- |
| `metamisc::valmeta()` | Classical Prediction Model Performance; Bayesian Prediction Model Performance. Meta-analysis of prediction model performance for C statistic and O/E ratio; accepts estimates, SEs/CIs, event counts, expected events, participant counts, study labels, classical estimators/link models, and Bayesian priors/MCMC settings. |
| `metamisc::plot.valmeta()` | Classical Prediction Model Performance; Bayesian Prediction Model Performance. Forest plot via `plot()` dispatch, with optional labels and estimates. |
| `metamisc::fat()` | Classical Prediction Model Performance; Bayesian Prediction Model Performance. Funnel-plot asymmetry tests: Egger unweighted, Egger multiplicative overdispersion, Macaskill, Macaskill pooled, Peters, and Debray FIV. |
| `metamisc::dplot()` | Bayesian Prediction Model Performance. Prior/posterior distribution plot. |
| `metamisc::rmplot()` | Bayesian Prediction Model Performance. Running-means MCMC diagnostic plot. |
| `metamisc::acplot()` | Bayesian Prediction Model Performance. Autocorrelation MCMC diagnostic plot. |
| `metamisc::gelmanplot()` | Bayesian Prediction Model Performance. Gelman-Rubin diagnostic plot. |

Major `metamisc` functions/features not implemented yet:

- `metamisc::uvmeta()` for univariate meta-analysis of summary data.
- `metamisc::riley()` and related `predict()` / `vcov()` / plot / summary methods for bivariate random-effects meta-analysis.
- `metamisc::metapred()` / `metamisc::stackedglm()` prediction-model development from clustered/IPD data.
- Standalone `metamisc::ccalc()` and `metamisc::oecalc()` calculation workflows are not exposed; related calls are commented out.
- Debray FAV funnel-asymmetry backend is commented out despite a QML option.

## metaSEM (1.5.4)

| Function | Analysis |
| --- | --- |
| `metaSEM::lavaan2RAM()` | Meta-Analytic SEM; SEM-Based Meta-Analysis. Converts lavaan-style model syntax to RAM models for user-specified SEM/MASEM models. |
| `metaSEM::sem()` | SEM-Based Meta-Analysis. Fits SEM models to the active dataset; exposes model summaries, fit measures, comparisons, and path diagrams. |
| `metaSEM::Cor2DataFrame()` | Meta-Analytic SEM. Converts study correlation/covariance matrices, sample sizes, optional means, and moderators into one-stage MASEM input. |
| `metaSEM::osmasem2()` | Meta-Analytic SEM. One-stage MASEM for correlation/covariance or covariance-with-means input; diagonal, symmetric, zero, and full random-effect structures; model summaries, fit/convergence tables, comparisons, and path diagrams. |
| `metaSEM::tssem1()` | Meta-Analytic SEM. First-stage TSSEM pooling for the optional pooled correlation/covariance matrix output. |

Major `metaSEM` functions not implemented yet:

- `metaSEM::tssem2()` / `metaSEM::wls()` for second-stage TSSEM model fitting as a distinct workflow.
- `metaSEM::meta()` / `metaSEM::reml()` for package-native univariate/multivariate ML/REML meta-analysis.
- `metaSEM::meta3L()` / `metaSEM::reml3L()` / `metaSEM::meta3LFIML()` for three-level SEM-based meta-analysis.
- `metaSEM::osmasem()` legacy/advanced OSMASEM interface, plus `metaSEM::osmasemR2()` / `metaSEM::osmasemSRMR()` outputs.
- `metaSEM::bootuniR1()` / `metaSEM::bootuniR2()` bootstrap univariate-r MASEM.
- `metaSEM::pattern.na()` / `metaSEM::pattern.n()` matrix missingness and accumulated sample-size diagnostics.

## robvis (0.3.0)

| Function | Analysis |
| --- | --- |
| `robvis::rob_summary()` | Risk of Bias Plot. Summary weighted barplots for RoB 2, ROBINS-I, and QUADAS-2 judgments; optional overall judgment, study weights, and color scheme. |
| `robvis::rob_traffic_light()` | Risk of Bias Plot. Per-study traffic-light matrix for RoB 2, ROBINS-I, and QUADAS-2 judgments; color scheme and point-size controls. |

Major `robvis` functions/features not implemented yet:

- `robvis::rob_forest()` for appending risk-of-bias traffic-light plots to forest plots.
- `robvis::rob_append_weights()` for deriving weights directly from `metafor` model objects; current UI requires a weights column.
- Dynamic `robvis::rob_tools()` template listing; current UI hard-codes RoB 2, ROBINS-I, and QUADAS-2.
