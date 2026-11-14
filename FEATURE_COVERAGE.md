# Function Coverage

## metafor (5.0-1)

| Function | Analysis |
| --- | --- |
| `metafor::rma()` | Classical Meta-Analysis; PET-PEESE; WAAP-WLS; Funnel Plot. Equal-, fixed-, and random-effects meta-analysis; mixed-effects meta-regression; location-scale models for heterogeneity; subgroup-specific model fitting; multiple heterogeneity estimators; z, t, and Knapp-Hartung coefficient tests; prediction intervals; conditional estimates; estimated marginal means; fitted model code display; H1 funnel parameter estimation; PET, PEESE, and conditional PET-PEESE meta-regressions with unrestricted weighted least squares (multiplicative dispersion) or additive heterogeneity, moderators, subgroups, no-selection indicator, adjusted estimates at a zero standard error, and unadjusted fixed/random effects comparisons; WLS and WAAP meta-analyses with a power criterion evaluated at the pooled or moderator-specific predicted effects, moderators, subgroups, and no-selection indicator. |
| `metafor::rma.mv()` | Classical Meta-Analysis (Multilevel/Multivariate); PET-PEESE; WAAP-WLS. Multilevel and multivariate meta-analysis for dependent effect sizes; multilevel/multivariate PET, PEESE, and conditional PET-PEESE meta-regressions; multilevel/multivariate WLS and WAAP meta-analyses; moderator models; random-effect/model components; nested, crossed, structured, autoregressive, spatial, and known-correlation structures; sparse-matrix and optimizer controls; subgroup-specific model fitting; prediction intervals; conditional estimates; estimated marginal means; fitted model code display. |
| `metafor::rma.glmm()` | Generalized Meta-Analysis. GLMM meta-analysis from event counts or person-time data; log odds ratio, log risk ratio, risk difference, and log incidence rate ratio models; unconditional fixed-study-effects, unconditional random-study-effects, conditional approximate-likelihood, and conditional exact-likelihood model types where available; equal-effects and random-effects estimation; moderator models; fitted model code display. |
| `metafor::rma.mh()` | Mantel-Haenszel / Peto. Mantel-Haenszel meta-analysis for frequency and event-rate outcomes; log odds ratio, log risk ratio, risk difference, log incidence rate ratio, and incidence rate difference measures; continuity-correction options; subgroup-specific model fitting; forest plot summaries. |
| `metafor::rma.peto()` | Mantel-Haenszel / Peto. Peto odds-ratio meta-analysis for binary outcomes; continuity-correction options; subgroup-specific model fitting; forest plot summaries. |
| `metafor::escalc()` | Effect Size Computation; Effect Size Aggregation; model and plot preparation. User-facing effect-size computation across independent-groups, association, single-group, repeated-measures, reliability, partial-correlation, model-fit, heterozygosity, and reported-effect-size designs; standardized effect sizes, ratios, correlations, binary outcomes, incidence-rate outcomes, and variance computation. |
| `metafor::aggregate.escalc()` | Effect Size Aggregation. Aggregation of multiple effect sizes within a cluster; inverse-variance weighting; optional cluster-size output; user-defined output column names. |
| `metafor::vcalc()` | Classical Meta-Analysis (Multilevel/Multivariate); Effect Size Aggregation. Variance-covariance matrix construction for dependent effect sizes; cluster-based dependence; subgrouped dependence; construct/type variables; within-study correlations; contrast groups; weights for contrast-based dependence. |
| `metafor::robust()` | Classical Meta-Analysis; Classical Meta-Analysis (Multilevel/Multivariate); PET-PEESE; WAAP-WLS. Cluster-robust coefficient tests and confidence intervals when a clustering variable is supplied. |
| `metafor::permutest()` | Classical Meta-Analysis; Classical Meta-Analysis (Multilevel/Multivariate). Permutation tests for effect-size moderation and heterogeneity moderation; omnibus and term-level tests. |
| `metafor::anova.rma()` | Classical Network Meta-Analysis. Wald test of the slope coefficients (`btt`) of network meta-regression fits (the `rma.mv` model returned by `netmeta::netmetareg()`), called through `stats::anova()`; the moderation test of the network meta-regression. |
| `metafor::selmodel()` | Selection Models. Publication-bias selection models with 3PSM/4PSM presets and custom step, continuous, and truncation weight functions; moderator and subgroup models; one- or two-sided selection; precision dependence; ordinal weight constraints; multiple candidate models with fit statistics and AIC/AICc/BIC weights. |
| `metafor::trimfill()` | Funnel Plot. Trim-and-fill publication-bias adjustment; missing-study estimates; adjusted funnel plot; trim-and-fill parameter table. |
| `metafor::regtest()` | Funnel Plot. Funnel-plot asymmetry tests through meta-regression and weighted-regression/Egger-style tests. |
| `metafor::ranktest()` | Funnel Plot. Rank-correlation/Begg-style funnel-plot asymmetry test. |
| `metafor::fsn()` | Funnel Plot. Fail-safe N calculations, including Rosenthal, Orwin, Rosenberg, and general methods. |
| `metafor::profile.rma.mv()` | Classical Meta-Analysis (Multilevel/Multivariate); PET-PEESE; WAAP-WLS. Profile-likelihood diagnostics for heterogeneity components. |
| `metafor::baujat()` | Classical Meta-Analysis diagnostics. Baujat/influence diagnostic plot for contribution to heterogeneity and fitted effect. |
| `metafor::vif()` | Classical Meta-Analysis diagnostics. Variance inflation factor diagnostics for moderator terms in effect-size and heterogeneity models. |
| `metafor::transf.*()` family | Classical Meta-Analysis outputs; Forest Plot; Funnel Plot; Effect Size Computation. User-facing effect-size scale transformations for estimates, intervals, plots, and computed effect sizes, including Fisher-z/correlation, log-odds/proportion, standardized mean difference/log-odds, reliability, R-squared, and common-language style transformations. |

Major `metafor` functions not implemented yet:

- `metafor::rcalc()` for variance-covariance matrices of overlapping correlations.
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

## netmeta (3.7-0)

| Function | Analysis |
| --- | --- |
| `netmeta::netmeta()` | Classical Network Meta-Analysis. Frequentist graph-theoretical network meta-analysis from contrast-based data (effect size, standard error, two treatments) or arm-based data (through `meta::pairwise()`); equal-effects and random-effects models; DerSimonian-Laird, restricted ML, and ML estimates of the common between-study variance, optionally fixed to a user-specified value; z and t (df of the Q statistic) inference; confidence and prediction intervals; heterogeneity (tau, tau2, I2) and Q statistics; multi-arm studies with a consistency tolerance; reference treatment; subgroup-specific model fitting and a test for subgroup differences (Cochran's Q test of the subgroup network estimates of each comparison, computed by the module from its subgroup fits; equal to `netmeta::subgroup()` for inverse-variance fits without a fixed between-study variance); effect size transformations of the displayed estimates; treatment effects versus the reference treatment or for all pairwise comparisons; league table (network estimates below and direct estimates above the diagonal, in the `netleague()` orientation, ordered by treatment order or P-score); forest plot; fitted model code display (the code reproduces the fits of the analysis; labels with backslashes are escaped, so that the printed strings equal the labels; with subgroups, the fits of the subgroups are collected in `subgroupFits` and the fit of the full dataset is kept as `fullDatasetFit`). |
| `netmeta::netmetabin()` | Classical Network Meta-Analysis. Network meta-analysis of rare binary events from arm-level counts (log odds ratio): Mantel-Haenszel and non-central hypergeometric (common effect), and penalized logistic regression (common effect and random effects through a dispersion factor); P-score ranking, league table, forest plot, network plot, direct evidence, pairwise meta-analyses, funnel plot, study impact, and SIDDE node splitting (Mantel-Haenszel and non-central hypergeometric). The outputs that need the structures of the inverse variance model (resampled ranking metrics, rankogram, design decomposition, net heat plot, contribution matrix, and node splitting for penalized logistic regression) are disabled in the form and not computed. The direct limits of the random-effects model of penalized logistic regression are computed from the direct estimate and its standard error (normal quantile), not taken from `netmetabin()`, which stores the common-effect limits there. Penalized logistic regression keeps all studies and the requested reference treatment; if the Mantel-Haenszel or non-central hypergeometric methods exclude the reference treatment (the requested one, or the default of `netmetabin()` when none is requested), the first treatment of the fitted network is the reference. A fit that fails after the exclusion of studies and designs with zero cells (the exclusions can disconnect the network) stops with a message that names the exclusions reported by `netmetabin()` and gives its error. `netmetabin()` fails for penalized logistic regression with fewer than three treatments: the module reports an error for the network of such a scope. For the non-central hypergeometric method, the direct evidence proportion of `netmeasures()` divides the variance of the network estimate by the variance of the Mantel-Haenszel direct estimate, so comparisons with only direct evidence can have proportions below 1 (a note says so). |
| `netmeta::netconnection()` | Classical Network Meta-Analysis. Check of the connectedness of the network; subnetworks of a disconnected network are listed in the error message (disconnected networks are analyzed only by the component model of the inverse variance pooling; the error message recommends it only there). |
| `netmeta::netrank()` | Classical Network Meta-Analysis. P-scores (analytic) with beneficial or harmful small effect sizes; treatment ranking table and ordering of the league table and forest plot (a fit without a ranking, i.e., with contrasts that the component model does not identify, is ordered by the treatment order, with a note). |
| `netmeta::rankogram()` | Classical Network Meta-Analysis. Resampling-based SUCRA, probability of being best, mean and median rank; rankogram and cumulative rankogram plots; number of simulations and seed. Not available for a network of two treatments (the P-scores remain). |
| `netmeta::decomp.design()` | Classical Network Meta-Analysis. Decomposition of the Q statistic into within-design heterogeneity and between-design inconsistency, and the random-effects design-by-treatment interaction model. The within-design heterogeneity lists the designs with degrees of freedom, as the print of `netmeta`; for a network of two treatments, which has no decomposition, a note replaces the empty tables. |
| `netmeta::netsplit()` | Classical Network Meta-Analysis. Node splitting into direct, indirect, and network estimates with a test of their difference (back-calculation and SIDDE); table with all, direct, or direct and indirect comparisons; forest plot. The SIDDE method of the Mantel-Haenszel and non-central hypergeometric fits provides no direct evidence proportion (a note says so). |
| `netmeta::netheat()` | Classical Network Meta-Analysis. Net heat plot of the design-level inconsistency (the matrix is reproduced from the internal netmeta functions `nma_krahn()` and `tau.within()` and drawn by the module). |
| `netmeta::netmeasures()` | Classical Network Meta-Analysis. Proportion of direct evidence, mean path length, and minimal parallelism of each network estimate (the mean path length and the minimal parallelism are not available for the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods; the minimal parallelism is not defined for a network of two treatments, which a note states). |
| `netmeta::netcontrib()` | Classical Network Meta-Analysis. Contribution matrix of the direct comparisons to the network estimates (shortest path, random walk, and pseudoinverse methods). |
| `netmeta::netimpact()` | Classical Network Meta-Analysis. Impact of each study on each network estimate (computed on request; can take minutes). |
| `netmeta::netpairwise()` | Classical Network Meta-Analysis. Separate pairwise meta-analyses of the directly compared treatments (the intervals and tests use the normal distribution, also under t inference, which a note states). |
| `netmeta::netgraph()` | Classical Network Meta-Analysis. Node and edge coordinates of the network plot (circle, circle with minimal crossings, and stress majorization layouts); edge width by number of studies (halved, as `netgraph()` does, if all comparisons have the same number of studies), precision (the component model of a disconnected network has no direct estimates: the widths show the number of studies, which a note above the plot states), or equal; node size by number of studies (the studies with a comparison in the fit: `k.trts` of `netmetabin()` counts the pairwise comparisons of a study with more than two arms), sample size, or equal; numbers of studies on edges; highlighted multi-arm studies. The plot is drawn by the module (the component model of a disconnected network, whose `netgraph()` fixes the sequence and the iteration, is drawn on the circle layout, as a network of two treatments, for which the stress layout fails). |
| `meta::funnel()` (the `funnel.netmeta()` method of `netmeta`) | Classical Network Meta-Analysis. Comparison-adjusted funnel plot (study effects centered at the pooled direct estimate of their comparison) with pseudo confidence intervals and a reference-first or custom treatment order; drawn by the module. |
| `netmeta::netmetareg()` | Classical Network Meta-Analysis. Network meta-regression on a continuous predictor or a predictor with two levels; independent or common slopes; moderation test and residual heterogeneity; per-subgroup fits; the predictor is checked among the studies of the analysed comparisons of the fitted network (a single-arm study that `pairwise()` drops and a study whose comparisons `netmeta()` excluded, e.g., without events in both arms, do not count; `netmetareg()` reads the excluded comparisons from the data of the fit, so the module removes them). Not available for the count-based pooling methods, where the assigned predictor is not a data variable (no observations are omitted for its missing values). netmeta 3.7-0 uses the covariance of the comparisons of a multi-arm study only for arm-level data with sample sizes and events, times, or standard deviations and a formula in `calcV()` (OR, RR, RD, ASD, MD, SMD, IRR, IRD, IRSD; the arm-level IRR, IRD, IRSD data lack the sample sizes); for contrast-based, general arm-level, ROM, and incidence-rate input it treats the comparisons as independent, and both tables of the meta-regression have a note (named by subgroup when only some subgroups have multi-arm studies). |
| `netmeta::netcomb()` / `netmeta::discomb()` | Classical Network Meta-Analysis. Component model (additive component network meta-analysis), selected as the model of the analysis, for connected (`netcomb()`) and disconnected (`discomb()`) networks; components defined by a separator in the treatment names or by 0/1 indicator variables (arm-based data); inactive treatment (with indicators, the treatment with all indicators equal to zero); interactions between components (the component matrix is built by the module, equivalent to `netmeta::createC()` with interactions; an interaction applies to every subgroup with a treatment that contains all its components; combinations of the same components in two orders, e.g., 'A+B' and 'B+A', are one interaction (the sets of components are compared, and interaction columns with equal labels get unique names); indicators that are zero for all treatments of a scope (the full dataset or a subgroup) are dropped in every scope with a note; the inactive component is never estimated); contrasts that the components do not identify (treatments of different subnetworks) are not estimable (NA; the printed code of the component model lists them in a comment, because `netcomb()` and `discomb()` return values for them), and the P-score ranking is then not available (the league table and the forest plot ordered by the ranking use the treatment order, with a note); `discomb()` receives the effect size measure; the treatment effects, heterogeneity, league table, P-score ranking, forest and network plots follow the component model; component effects; tests of the additive, standard, and difference models; component forest plot. |

Major `netmeta` functions/features not implemented yet:

- `netmeta::netposet()` / `netmeta::hasse()` / `netmeta::vikor()` for ranking of treatments across several outcomes (partial order, Hasse diagrams, VIKOR).
- `netmeta::netbind()` for combining several network meta-analyses (for example, of several outcomes).
- `netmeta::netleague()` with two outcomes (league table of two network meta-analyses).
- `netmeta::netpath()` for the path-based inconsistency method.
- `netmeta::netcomparison()` / `netmeta::netcomplex()` for arbitrary complex interventions in component network meta-analysis.
- Three-dimensional network graphs (`netmeta::netgraph()` with `dim = "3d"`).
- Network meta-regression under the unrelated mean effects model (inconsistency meta-regression with `consistency = FALSE`).
- Contributions by the `cccp` solver (`netmeta::netcontrib()`, requires an additional package).
- Radial plots of netmeta objects (`radial()` method) and `netmeta::nettable()` summary tables.

## meta (8.5-0)

| Function | Analysis |
| --- | --- |
| `meta::pairwise()` | Classical Network Meta-Analysis. Transformation of arm-based data into pairwise contrasts for binary (log odds ratio, log risk ratio, risk difference, arcsine square root difference), continuous (mean difference, standardized mean difference as Cohen's d without the small-sample correction, log ratio of means), incidence rate (log incidence rate ratio, rate difference, square root rate difference), and general arm-level effect size data; continuity correction value, application rule, and treatment of studies without cases or events (the option to drop them applies to the odds ratio and the risk ratio, as `allstudies` of `meta`; the group is disabled for the count-based pooling methods, which do not use it); the log ratio of means requires positive means. |
| `meta::metagen()` | Classical Network Meta-Analysis. Meta-analysis of the comparison-adjusted effects (DerSimonian-Laird) for the funnel plot asymmetry tests; also used inside `netmeta::netpairwise()`. |
| `meta::metabias()` | Classical Network Meta-Analysis. Egger, Begg and Mazumdar, and Thompson and Sharp funnel plot asymmetry tests on the comparison-adjusted effects (a test that fails, e.g., because all comparisons have the same precision, shows its error in a note; the note on at least 10 comparisons is for tests that return no statistic). |

Major `meta` functions/features not implemented yet:

- Pairwise meta-analysis functions such as `meta::metabin()`, `meta::metacont()`, `meta::metainc()`, `meta::metamean()`, `meta::metaprop()`, `meta::metacor()`, and `meta::metareg()` (pairwise meta-analyses are available in other analyses through `metafor`).
- `meta::forest()` and `meta::funnel()` plots of pairwise meta-analyses (the network forest and funnel plots are drawn by the module).
- Other `meta::metabias()` asymmetry tests (for example, Harbord, Peters, Macaskill, Deeks, Pustejovsky-Rodgers, arcsine, and Schwarzer) and `meta::trimfill()`.
