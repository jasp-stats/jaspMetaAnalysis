#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Classical Meta-Analysis
#'
#' Classical meta-analysis allows you to conduct a meta-analysis using the classical approach. It provides options for fixed and random effects models, as well as meta-regression, location-scale models, and subgroup analysis. Additional options include the ability to specify clustering for robust variance estimation, permutation tests, and generating the metafor package R code. The results include estimates of effect sizes, heterogeneity, moderation, and various plots to visualize the results.
#' 
#' See [this tutorial](https://doi.org/10.48550/arXiv.2509.09845) for a detailed introduction to the module.
#'
#' @param addOmnibusModeratorTestEffectSizeCoefficients, Include an omnibus test for the specified effect size regression coefficients. Available when effect size model terms are included. The coefficients should be selected via their comma-separated indicies which correspond to the order presented in the 'Effect Size Meta-Regression Coefficients' Table.
#'    Defaults to \code{FALSE}.
#' @param addOmnibusModeratorTestEffectSizeCoefficientsValues, Specify the indices of the effect size meta-regression coefficients to include in the omnibus test, e.g. '(1, 2)' for the first and the second coefficient.
#' @param addOmnibusModeratorTestHeterogeneityCoefficients, Include an omnibus test for the specified heterogeneity regression coefficients. Available when heterogeneity model terms are included. The coefficients should be selected via their comma-separated indicies which correspond to the order presented in the 'Heterogeneity Meta-Regression Coefficients' Table.
#'    Defaults to \code{FALSE}.
#' @param addOmnibusModeratorTestHeterogeneityCoefficientsValues, Specify the indices of the heterogeneity meta-regression coefficients to include in the omnibus test, e.g. '(1, 2)' for the first and the second coefficient.
#' @param advancedExtendMetaforCall, Allow adding custom arguments to the metafor function call. Consult the metafor R package documentation for the available commands (for <a href='https://wviechtb.github.io/metafor/reference/rma.uni.html'>meta-analysis</a> and <a href='https://wviechtb.github.io/metafor/reference/rma.mv.html'>meta-analysis (multilevel/multivariate)</a>).
#'    Defaults to \code{FALSE}.
#' @param advancedExtendMetaforCallCode, The additional arguments to the metafor function call must be specified as a named list (the 'list()' call can be ommited). E.g., 'list(tau2 = 1)' (or 'tau2 = 1') can be used to fix the between-study heterogeneity to a given value. Multiple arguments must be comma-seprated, e.g. 'list(tau2 = 1, gamma2 = 0.5)' (or 'tau2 = 1, gamma2 = 0.5'). New lines are ignored.
#' @param bubblePlotBubbles, Show the observed estimates as bubbles. Uncheck to show only the model-based estimates.
#'    Defaults to \code{TRUE}.
#' @param bubblePlotBubblesJitter, Set the degree of x-coordinate jitter of the observed estimates. Available when the x-axis variable is nominal.
#' @param bubblePlotBubblesRelativeSize, Set the relative size of the observed estimates.
#' @param bubblePlotBubblesSize, Options for determining the size of the observed estimates.
#' @param bubblePlotBubblesTransparency, Set the transparency of the observed estimates.
#' @param bubblePlotConfidenceIntervals, Include confidence intervals of the estimated effect sizes.
#'    Defaults to \code{TRUE}.
#' @param bubblePlotLegendPosition, Set the legend position of the bubble plot.
#' @param bubblePlotPredictionIntervals, Include prediction intervals of the estimated effect sizes. Unavailable for unrestricted weighted least squares.
#'    Defaults to \code{TRUE}.
#' @param bubblePlotRelativeSizeText, Adjust the text size of the bubble plot.
#' @param bubblePlotSdFactorCovariates, Standard deviation cutoff used for binning continuous covariates.
#' @param bubblePlotSelectedVariable, Variable to be visualized on the x-axis.
#' @param bubblePlotSeparateLines, Variable(s) according to which predictions are split across different lines.
#' @param bubblePlotSeparatePlots, Variable(s) according to which predictions are split across different plots.
#' @param bubblePlotTheme, Set the theme of the bubble plot.
#' @param clustering, Variable indicating clustering of effect sizes for robust variance estimation. If the variable is specified, a cluster-robust tests and confidence intervals are provided. See 'Details' section for additional clustering options. This option is disabled when permutation tests are selected.
#' @param clusteringSmallSampleCorrection, Apply a small-sample correction to the clustered standard errors.
#'    Defaults to \code{TRUE}.
#' @param clusteringUseClubSandwich, Use the clubSandwich package for robust variance estimation in clustered data. Enabled when a clustering variable is specified.
#'    Defaults to \code{TRUE}.
#' @param computeCovarianceMatrix, Compute the covariance matrix of the parameter estimates. Available in multilevel/multivariate meta-analysis.
#'    Defaults to \code{TRUE}.
#' @param confidenceIntervals, Include confidence intervals in the tabular output.
#'    Defaults to \code{TRUE}.
#' @param diagnosticsCasewiseDiagnostics, Include casewise diagnostics to assess the influence of individual studies on the meta-analysis results. Note that diagnostics are always based on the non-clustered model.
#'    Defaults to \code{FALSE}.
#' @param diagnosticsCasewiseDiagnosticsDifferenceInCoefficients, Include the differences in model coefficients when each study is excluded (DFBETAS).
#'    Defaults to \code{FALSE}.
#' @param diagnosticsCasewiseDiagnosticsIncludePredictors, Include predictor variables in the casewise diagnostics output.
#'    Defaults to \code{FALSE}.
#' @param diagnosticsCasewiseDiagnosticsShowInfluentialOnly, Show only the influential studies in the casewise diagnostics. Unavailable when performing multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param diagnosticsPlotsBaujat, Include a Baujat plot to detect studies contributing to heterogeneity and overall effect size. Unavailable when performing multilevel/multivariate meta-analysis. Note that Baujat plot is always based on the non-clustered model.
#'    Defaults to \code{FALSE}.
#' @param diagnosticsPlotsProfileLikelihood, Include a profile likelihood plot for the heterogeneity parameter (τ²). Unavailable for equal effects, fixed effects, and unrestricted weighted least squares models.
#'    Defaults to \code{FALSE}.
#' @param diagnosticsResidualFunnel, Include a residual funnel plot. Note that residual funnel plot is always based on the non-clustered model.
#'    Defaults to \code{FALSE}.
#' @param diagnosticsVarianceInflationFactor, Include variance inflation factors to assess multicollinearity among predictors. Available when predictors are included in the model.
#'    Defaults to \code{FALSE}.
#' @param diagnosticsVarianceInflationFactorAggregate, Aggregate variance inflation factors by terms instead of individual coefficients.
#'    Defaults to \code{TRUE}.
#' @param effectSize, Variable containing the observed effect sizes.
#' @param effectSizeModelIncludeIntercept, Include an intercept in the effect size model.
#'    Defaults to \code{TRUE}.
#' @param effectSizeModelTerms, Variables assigned as model terms in the effect size model.
#' @param effectSizeStandardError, Variable containing the standard errors corresponding to the effect sizes.
#' @param estimatedMarginalMeansEffectSizeAddAdjustedEstimate, Include the adjusted effect estimate, which accounts for the moderators in the meta-regression model. This provides the effect size adjusted for the influence of moderators, as opposed to the pooled effect which combines the estimates across all studies.
#'    Defaults to \code{FALSE}.
#' @param estimatedMarginalMeansEffectSizeSdFactorCovariates, Standard deviation factor for covariates when computing estimated marginal means; applies to scale variables.
#' @param estimatedMarginalMeansEffectSizeSelectedVariables, Variables selected for computing estimated marginal means in the effect size model.
#' @param estimatedMarginalMeansEffectSizeTestAgainst, Option to test the estimated marginal means against a specific value.
#'    Defaults to \code{FALSE}.
#' @param estimatedMarginalMeansHeterogeneityAddAdjustedEstimate, Include the adjusted heterogeneity estimate, which accounts for the moderators in the heterogeneity meta-regression model. This provides the heterogeneity estimate adjusted for the influence of moderators, as opposed to the pooled heterogeneity estimate which combines the heterogeneity estimates across all studies.
#'    Defaults to \code{FALSE}.
#' @param estimatedMarginalMeansHeterogeneitySdFactorCovariates, Standard deviation factor for covariates when computing estimated marginal means; applies to scale variables.
#' @param estimatedMarginalMeansHeterogeneitySelectedVariables, Variables selected for computing estimated marginal means in the heterogeneity model. Unavailable when performing multilevel/multivariate meta-analysis.
#' @param estimatedMarginalMeansHeterogeneityTransformation, Transformation to apply to the heterogeneity estimate: tau (𝜏) or tau-squared (𝜏²).
#' @param exportColumnPrefix, Add a prefix to every column exported by this analysis.
#'    Defaults to \code{FALSE}.
#' @param exportColumnPrefixValue, Prefix added to every exported column name.
#' @param exportDiagnosticsCaseDiagnostics, Export casewise residual, Cook's distance, leverage, and weight diagnostics where available.
#'    Defaults to \code{FALSE}.
#' @param exportDiagnosticsCoefficientInfluence, Export coefficient influence diagnostics.
#'    Defaults to \code{FALSE}.
#' @param exportDiagnosticsInfluentialCases, Export an indicator for influential cases.
#'    Defaults to \code{FALSE}.
#' @param exportDiagnosticsModelImpact, Export diagnostics for how individual cases affect the model fit.
#'    Defaults to \code{FALSE}.
#' @param exportPredictedValues, Export predicted values and interval bounds.
#'    Defaults to \code{FALSE}.
#' @param exportRandomEffects, Export predicted random effects.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsConditional, Export conditional standardized residuals.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsPearson, Export Pearson residuals.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsRaw, Export raw residuals.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsStandardized, Export standardized residuals.
#'    Defaults to \code{FALSE}.
#' @param exportResidualsStudentized, Export studentized residuals.
#'    Defaults to \code{FALSE}.
#' @param exportTrueEffectEstimates, Export best linear unbiased predictions of the study-specific true effects.
#'    Defaults to \code{FALSE}.
#' @param exportWaapWlsEstimatedPower, Export the power of each estimate to detect the effect of the WLS model (two-sided z-test, α = .05), i.e., the value compared with the power criterion. Exported for all estimates, also when WLS is reported.
#'    Defaults to \code{FALSE}.
#' @param exportWaapWlsIncludedInWaap, Export an indicator of the estimates included in the WAAP model (the adequately powered and the unaffected estimates). Exported for all estimates, also when WLS is reported.
#'    Defaults to \code{FALSE}.
#' @param exportWeights, Export model fitting weights. These are not coefficient-specific contribution weights in meta-regression. For multilevel/multivariate models, diagonal weights match the default forest plot weights and row-sum weights are exported for intercept-only models.
#'    Defaults to \code{FALSE}.
#' @param fitMeasures, Include fit statistics for the model, such as AIC and BIC.
#'    Defaults to \code{FALSE}.
#' @param fixParametersTau2, Fix the value of 𝜏² in the model instead of estimating it. Unavailable in multilevel/multivariate meta-analysis or with meta-regression model for heterogeneity. A more complex heterogeneity terms in the multilevel/multivariate meta-analysis can be fixed via the 'Extend metafor call' option.
#'    Defaults to \code{FALSE}.
#' @param fixParametersWeights, Use custom weights for the effect sizes instead of inverse-variance weights. Unavailable for PET-PEESE and WAAP-WLS estimated with unrestricted weighted least squares, which is defined by the inverse-variance weights.
#'    Defaults to \code{FALSE}.
#' @param fixedEffectTest, Method for testing the model coefficients: 'z' uses standard normal approximation, 't' uses t-distribution, and 'knha' uses the Knapp and Hartung adjustment (default). Clustered models use t-distribution based tests.
#' @param forestPlotAuxiliaryAddVerticalLine, Add a solid vertical line in the forest plot.
#'    Defaults to \code{FALSE}.
#' @param forestPlotAuxiliaryAddVerticalLine2, Add a dotted vertical line in the forest plot.
#'    Defaults to \code{FALSE}.
#' @param forestPlotAuxiliaryDigits, Number of digits printed for the effect size and confidence intervals summary text.
#' @param forestPlotAuxiliaryEffectLabel, Change the x-axis label of the forest plot.
#' @param forestPlotAuxiliaryPlotColor, Change color of the plotted objects.
#' @param forestPlotAuxiliarySetXAxisLimit, Change the default x-axis limits.
#'    Defaults to \code{FALSE}.
#' @param forestPlotAuxiliarySetXAxisTicks, Specify x-axis ticks manually as comma-separated numbers or fractions.
#'    Defaults to \code{FALSE}.
#' @param forestPlotAuxiliaryXAxisTransformLabelsOnly, Apply the selected effect size transformation only to the x-axis labels.
#'    Defaults to \code{TRUE}.
#' @param forestPlotConditionalEstimates, Display the conditional effect, heterogeneity, and estimated marginal means estimates.
#'    Defaults to \code{FALSE}.
#' @param forestPlotEffectSizeFixedEffectEstimate, Include a fixed effect meta-analytic effect size estimate in the model information section. Not available if the model was already fitted with fixed effects or the model contains heterogeneity meta-regression.
#'    Defaults to \code{FALSE}.
#' @param forestPlotEffectSizeFixedEffectTest, Include the test of the fixed effect meta-analytic effect size estimate in the model information section.
#'    Defaults to \code{TRUE}.
#' @param forestPlotEffectSizeModerationTest, Include the omnibus effect size moderation test in the model information section. Available when effect size meta-regression is specified.
#'    Defaults to \code{FALSE}.
#' @param forestPlotEffectSizePooledEstimate, Include the overall meta-analytic effect size estimate in the model information section.
#'    Defaults to \code{TRUE}.
#' @param forestPlotEffectSizePooledEstimateTest, Include the test of the overall meta-analytic effect size estimate in the model information section. If a meta-regression model is fitted, the test corresponds to either the adjusted effect or the meta-regression intercept term and the corresponding estimate is also displayed. The test is available only if Bayesian model averaging for the effect is selected.
#'    Defaults to \code{TRUE}.
#' @param forestPlotEffectSizeUnadjustedFixedEffectEstimate, Include the fixed effect estimate without publication bias adjustment (same moderators and subgroups) for comparison with the adjusted estimate in the model information section.
#'    Defaults to \code{FALSE}.
#' @param forestPlotEffectSizeUnadjustedFixedEffectTest, Include the test of the unadjusted fixed effect estimate in the model information section.
#'    Defaults to \code{TRUE}.
#' @param forestPlotEffectSizeUnadjustedRandomEffectsEstimate, Include the random effects estimate (restricted maximum likelihood) without publication bias adjustment (same moderators and subgroups) for comparison with the adjusted estimate in the model information section.
#'    Defaults to \code{FALSE}.
#' @param forestPlotEffectSizeUnadjustedRandomEffectsTest, Include the test of the unadjusted random effects estimate in the model information section.
#'    Defaults to \code{TRUE}.
#' @param forestPlotEstimatedMarginalMeans, Add estimated marginal means information to the forest plot. Available when effect size meta-regression is specified.
#'    Defaults to \code{FALSE}.
#' @param forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate, Include the adjusted effect size estimate in the estimated marginal means section.
#'    Defaults to \code{FALSE}.
#' @param forestPlotEstimatedMarginalMeansCoefficientTests, Include coefficient tests of variables included in the estimated marginal means. The null hypothesis states that the estimated marginal mean for a given level equals the tested value.
#'    Defaults to \code{TRUE}.
#' @param forestPlotEstimatedMarginalMeansCoefficientTestsAgainst, Specify the test value for the coefficient tests.
#' @param forestPlotEstimatedMarginalMeansCoefficientTestsAgainst0, Include coefficient tests of variables included in the estimated marginal means against 0. The null hypothesis states that the estimated marginal mean for a given level equals the tested value.
#'    Defaults to \code{TRUE}.
#' @param forestPlotEstimatedMarginalMeansSelectedVariables, Select variables for which the estimated marginal means are visualized.
#' @param forestPlotEstimatedMarginalMeansTermTests, Include the omnibus term test of variables included in the estimated marginal means. The null hypothesis states that the effect size at all levels of the categorical variable are equal or that there is no linear association between the effect size and the continuous variable.
#'    Defaults to \code{TRUE}.
#' @param forestPlotEstimatesAndConfidenceIntervals, Include effect size estimates and confidence intervals summary text in the right panel.
#'    Defaults to \code{TRUE}.
#' @param forestPlotHeterogeneityEstimateH2, Include the meta-analytic H², an index indicating the ratio of total variability to sampling variability in the model information section. Not available for multilevel/multivariate meta-analysis or heterogeneity meta-regression.
#'    Defaults to \code{FALSE}.
#' @param forestPlotHeterogeneityEstimateI2, Include the meta-analytic I², the percentage of total variation across studies due to heterogeneity in the model information section. Not available for multilevel/multivariate meta-analysis or heterogeneity meta-regression.
#'    Defaults to \code{FALSE}.
#' @param forestPlotHeterogeneityEstimateTau, Include the meta-analytic 𝜏, the square root of the estimated between-study variance in the model information section. Not available for multilevel/multivariate meta-analysis.
#'    Defaults to \code{TRUE}.
#' @param forestPlotHeterogeneityEstimateTau2, Include the meta-analytic 𝜏², the estimated between-study variance in the model information section. Not available for multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param forestPlotHeterogeneityModerationTest, Include the omnibus heterogeneity moderation test in the model information section. Available when heterogeneity meta-regression is specified.
#'    Defaults to \code{FALSE}.
#' @param forestPlotHeterogeneityTest, Include the test of the residual heterogeneity in the model information section.
#'    Defaults to \code{TRUE}.
#' @param forestPlotHeterogeneityTestLRT, Include the likelihood ratio test of the residual heterogeneity in the model information section.
#'    Defaults to \code{FALSE}.
#' @param forestPlotHeterogeneityTestWald, Include the Wald-type test of the residual heterogeneity in the model information section.
#'    Defaults to \code{FALSE}.
#' @param forestPlotModelInformation, Add meta-analytic model information to the forest plot.
#'    Defaults to \code{FALSE}.
#' @param forestPlotPredictionIntervals, Include prediction intervals of the estimated marginal means and the model information output.
#'    Defaults to \code{TRUE}.
#' @param forestPlotPublicationBiasTest, Include the publication bias test in the model information section.
#'    Defaults to \code{FALSE}.
#' @param forestPlotStudyInformation, Add study-level information panel to the forest plot. There are three sections of the study-level information panel: a) the left section with study labels, names, and etc designed via the 'Selected Variables' option, b) the middle section visualizing the estimates and confidence intervals based on the meta-analytic input, c) the right section textually summarizing the estimates and confidence intervals based on the meta-analytic input.
#'    Defaults to \code{FALSE}.
#' @param forestPlotStudyInformationAggregateBy, Select the variable by which the study-level information panel is aggregated.
#' @param forestPlotStudyInformationAggregateMethod, Select the method for aggregating the estimates.
#' \itemize{
#'   \item \code{"boxplot"}: Aggregate the estimates by boxplot.
#'   \item \code{"bubbles"}: Aggregate the estimates by bubbles.
#' }
#' @param forestPlotStudyInformationBoxplotWeightsNormalized, Normalize displayed user-supplied weights to sum to 100.
#'    Defaults to \code{TRUE}.
#' @param forestPlotStudyInformationBoxplotWeightsPercentage, Display user-supplied weights as percentages.
#'    Defaults to \code{TRUE}.
#' @param forestPlotStudyInformationOrderAscending, Order the study-level information panel in ascending order.
#'    Defaults to \code{FALSE}.
#' @param forestPlotStudyInformationOrderBy, Select the variable by which the study-level information panel is ordered.
#' @param forestPlotStudyInformationPredictedEffects, Include predicted effect sizes in the middle section of the study-level information panel. Unavailable when the panel is aggregated.
#'    Defaults to \code{FALSE}.
#' @param forestPlotStudyInformationSecondaryConfidenceInterval, Include secondary confidence interval for effect sizes in the middle section of the study-level information panel.
#'    Defaults to \code{FALSE}.
#' @param forestPlotStudyInformationSelectedVariables, Select variables containing study-level information to be printed in the left section of the panel.
#' @param forestPlotStudyInformationSelectedVariablesSettings, Adjust the Title, Width, and Alignment of each column in the left section.
#' @param forestPlotStudyInformationStudyWeights, Include study weights in the right section of the study-level information panel. For multilevel/multivariate meta-analysis, diagonal model weights are shown, matching the default forest plot point-size weights.
#'    Defaults to \code{FALSE}.
#' @param forestPlotSubgroupFullDatasetEstimatedMarginalMeans, Include the full dataset estimated marginal means in the forest plot if subgroups are specified.
#'    Defaults to \code{TRUE}.
#' @param forestPlotSubgroupFullDatasetModelInformation, Include the full dataset model information in the forest plot if subgroups are specified.
#'    Defaults to \code{TRUE}.
#' @param forestPlotSubgroupPanelsWithinSubgroup, Group the output panels within their subgroup membership.
#'    Defaults to \code{FALSE}.
#' @param forestPlotSubgroupShowTitles, Show the subgroup heading labels in the forest plot.
#'    Defaults to \code{TRUE}.
#' @param forestPlotTestsInRightPanel, Move test results text to the right panel.
#'    Defaults to \code{FALSE}.
#' @param heterogeneityGamma, Include γ, the multiplicative heterogeneity of the unrestricted weighted least squares model, i.e., the factor by which the sampling variances are multiplied.
#'    Defaults to \code{TRUE}.
#' @param heterogeneityH2, Include H², an index indicating the ratio of total variability to sampling variability.
#'    Defaults to \code{FALSE}.
#' @param heterogeneityI2, Include I², the percentage of total variation across studies due to heterogeneity.
#'    Defaults to \code{FALSE}.
#' @param heterogeneityModelIncludeIntercept, Include an intercept in the heterogeneity model.
#'    Defaults to \code{TRUE}.
#' @param heterogeneityModelLink, Link function used in the heterogeneity model: 'log' or 'identity'.
#' @param heterogeneityModelTerms, Variables assigned as model terms in the heterogeneity model. Specifying a heterogeneity model results in a 'location-scale' meta-analytic model.
#' @param heterogeneitySqrtGamma, Include √γ, the square root of the multiplicative heterogeneity of the unrestricted weighted least squares model.
#'    Defaults to \code{TRUE}.
#' @param heterogeneityTau, Include 𝜏, the square root of the estimated between-study variance.
#'    Defaults to \code{TRUE}.
#' @param heterogeneityTau2, Include 𝜏², the estimated between-study variance.
#'    Defaults to \code{TRUE}.
#' @param includeFullDatasetInSubgroupAnalysis, Include the full dataset output in the subgroup analysis. This option is only available when the subgroup analysis is selected.
#'    Defaults to \code{FALSE}.
#' @param metaregressionCoefficientCorrelationMatrix, Include the correlation matrix of the regression coefficients.
#'    Defaults to \code{FALSE}.
#' @param metaregressionCoefficientEstimates, Include estimates of the regression coefficients in the meta-regression model.
#'    Defaults to \code{TRUE}.
#' @param metaregressionTermTests, Include tests for each term in the meta-regression model. The null hypothesis states that the effect size at all levels of the categorical variable are equal or that there is no linear association between the effect size and the continuous variable).
#'    Defaults to \code{TRUE}.
#' @param method, Method used for model estimation in the meta-analysis. The available methods depend on the inclusion of heterogeneity model terms.
#' @param optimizerConvergenceRelativeTolerance, Set the relative convergence tolerance for the optimizer. Available when heterogeneity model terms are included or using specific optimizers in multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param optimizerConvergenceTolerance, Set the convergence tolerance for the optimizer. Available when using certain methods without heterogeneity model terms or specific optimizers in multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param optimizerFinalTrustRegionRadius, Set the final trust region radius for the optimizer. Available when using specific optimization methods in multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param optimizerInitialTau2, Specify the initial value of 𝜏² for the optimization algorithm. Available only for specific optimization methods and unavailable in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.
#'    Defaults to \code{FALSE}.
#' @param optimizerInitialTrustRegionRadius, Set the initial trust region radius for the optimizer. Available when using specific optimization methods in multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param optimizerMaximumEvaluations, Set the maximum number of function evaluations for the optimizer. Available when using specific optimization methods in multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param optimizerMaximumIterations, Set the maximum number of iterations for the optimizer. Available when using certain estimation or optimization methods.
#'    Defaults to \code{FALSE}.
#' @param optimizerMaximumRestarts, Set the maximum number of restarts for the optimizer. Available when using the Nelder-Mead method ('nmk') in multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param optimizerMaximumTau2, Specify the maximum allowable value of 𝜏² during optimization. Available only for specific optimization methods and unavailable in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.
#'    Defaults to \code{FALSE}.
#' @param optimizerMethod, Select the optimization method to use for fitting the model. Available in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.
#' @param optimizerMinimumTau2, Specify the minimum allowable value of 𝜏² during optimization. Available only for specific optimization methods and unavailable in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.
#'    Defaults to \code{FALSE}.
#' @param optimizerStepAdjustment, Set the step adjustment factor for the optimizer. Available when using certain methods without heterogeneity model terms and unavailable in multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param permutationTest, Perform a permutation test for the model coefficients. Available in the meta-analysis analysisType when clustering is not specified. The resulting permuation p-values are displayed in the 'p (permutation)' column. Note that permutation can be computationally intesive.
#'    Defaults to \code{FALSE}.
#' @param permutationTestIteration, Specify the number of permutations to use in the approximate permutation test.
#' @param permutationTestType, Select the type of permutation test.
#' \itemize{
#'   \item \code{"approximate"}
#'   \item \code{"exact"}
#' }
#' @param petPeeseCoefficientEstimates, Include the estimate of the bias coefficient: the standard error (PET) or the sampling variance (PEESE) coefficient of the reported model.
#'    Defaults to \code{TRUE}.
#' @param petPeeseRegressionPlot, Visualize the fitted relationship between the effect sizes and their standard errors. The fitted line (PET) or curve (PEESE) at a standard error of zero corresponds to the adjusted estimate.
#'    Defaults to \code{FALSE}.
#' @param petPeeseThreshold, Significance level of the two-sided test of the PET adjusted effect. If the test is significant, PEESE is reported; otherwise PET is reported (Stanley & Doucouliagos, 2014). Available for the conditional PET-PEESE adjustment.
#' @param predictionIntervals, Include prediction intervals in the tabular output. Unavailable for unrestricted weighted least squares.
#'    Defaults to \code{TRUE}.
#' @param predictors, Variables to include as predictors (moderators) in the meta-regression model. See the 'Model' section for the meta-regression specification details.
#' @param randomEffectsConfidenceIntervals, Include confidence intervals for the Random Effects / Model Components.
#'    Defaults to \code{FALSE}.
#' @param randomEffectsTestInclusion, Test the inclusion of the individual Random Effects / Model Components. The test compares the complete model (i.e., including all components) with a model without one of the specified Random Effects / Model Structure components at a time.
#'    Defaults to \code{FALSE}.
#' @param showMetaforRCode, Display the underlying R code used by the metafor package to fit the model.
#'    Defaults to \code{FALSE}.
#' @param standardErrors, Include standard errors in the tabular output. Only available when no effect size transformation is applied.
#'    Defaults to \code{TRUE}.
#' @param studyLabels, Variable containing labels for the studies. Used for labeling outputs and plots.
#' @param subgroup, Variable indicating subgroup stratification. For each subgroup, an independent model is fitted to the corresponding data set subset.
#' @param transformEffectSize, Select a transformation to apply to the effect size estimates in the output. This transformation applies to the 'Meta-Analytic Estimates Table', 'Estimated Marginal Means Table', 'Forest Plot', and  the 'Bubble Plot'. The 'Meta-Regression Coeffient Estimates' are not transformed.
#' @param useSparseMatrices, Use sparse matrix representations to speed up computations in models with large datasets or complex random effects structures. Available in multilevel/multivariate meta-analysis.
#'    Defaults to \code{FALSE}.
#' @param weightFunctionCoefficientEstimates, Include weight function estimates in the output.
#'    Defaults to \code{TRUE}.
#' @param weightFunctionPValueFrequencyTable, Estimate counts within the specified p-value intervals.
#'    Defaults to \code{FALSE}.
#' @param weightFunctionPlot, Visualize the relative probability of study being published across the range of p-values.
#'    Defaults to \code{FALSE}.
#' @param weightFunctionPlotRescaleXAxis, Space p-value thresholds equally along the x-axis.
#'    Defaults to \code{TRUE}.
#' @param weightedEstimation, Perform weighted estimation using inverse-variance weights. Uncheck for unweighted estimation. Unavailable for multilevel/multivariate models, which are always weighted by the inverse of the variance-covariance matrix.
#'    Defaults to \code{TRUE}.
ClassicalMetaAnalysis <- function(
          data = NULL,
          version = "1",
          addOmnibusModeratorTestEffectSizeCoefficients = FALSE,
          addOmnibusModeratorTestEffectSizeCoefficientsValues = "(1, 2)",
          addOmnibusModeratorTestHeterogeneityCoefficients = FALSE,
          addOmnibusModeratorTestHeterogeneityCoefficientsValues = "(1, 2)",
          advancedExtendMetaforCall = FALSE,
          advancedExtendMetaforCallCode = "",
          bubblePlotBubbles = TRUE,
          bubblePlotBubblesJitter = 1,
          bubblePlotBubblesRelativeSize = 1,
          bubblePlotBubblesSize = "weight",
          bubblePlotBubblesTransparency = 0.9,
          bubblePlotConfidenceIntervals = TRUE,
          bubblePlotConfidenceIntervalsTransparency = 0.3,
          bubblePlotLegendPosition = "right",
          bubblePlotPredictionIntervals = TRUE,
          bubblePlotPredictionIntervalsTransparency = 0.1,
          bubblePlotRelativeSizeText = 1.5,
          bubblePlotSdFactorCovariates = 1,
          bubblePlotSelectedVariable = list(optionKey = "variable", types = list(), value = list()),
          bubblePlotSeparateLines = list(optionKey = "variable", types = list(), value = list()),
          bubblePlotSeparatePlots = list(optionKey = "variable", types = list(), value = list()),
          bubblePlotTheme = "jasp",
          clustering = list(types = list(), value = ""),
          clusteringSmallSampleCorrection = TRUE,
          clusteringUseClubSandwich = TRUE,
          colorPalette = "colorblind",
          computeCovarianceMatrix = TRUE,
          confidenceIntervals = TRUE,
          confidenceIntervalsLevel = 0.95,
          contrastsEffectSize = FALSE,
          contrastsEffectSizePValueAdjustment = "none",
          contrastsHeterogeneity = FALSE,
          contrastsHeterogeneityPValueAdjustment = "none",
          diagnosticsCasewiseDiagnostics = FALSE,
          diagnosticsCasewiseDiagnosticsDifferenceInCoefficients = FALSE,
          diagnosticsCasewiseDiagnosticsIncludePredictors = FALSE,
          diagnosticsCasewiseDiagnosticsShowInfluentialOnly = FALSE,
          diagnosticsPlotsBaujat = FALSE,
          diagnosticsPlotsProfileLikelihood = FALSE,
          diagnosticsResidualFunnel = FALSE,
          diagnosticsVarianceInflationFactor = FALSE,
          diagnosticsVarianceInflationFactorAggregate = TRUE,
          effectSize = list(types = list(), value = ""),
          effectSizeModelIncludeIntercept = TRUE,
          effectSizeModelTerms = list(optionKey = "components", types = list(), value = list()),
          effectSizeStandardError = list(types = list(), value = ""),
          estimatedMarginalMeansEffectSize = TRUE,
          estimatedMarginalMeansEffectSizeAddAdjustedEstimate = FALSE,
          estimatedMarginalMeansEffectSizeSdFactorCovariates = 1,
          estimatedMarginalMeansEffectSizeSelectedVariables = list(optionKey = "variable", types = list(), value = list()),
          estimatedMarginalMeansEffectSizeTestAgainst = FALSE,
          estimatedMarginalMeansEffectSizeTestAgainstValue = 0,
          estimatedMarginalMeansHeterogeneity = TRUE,
          estimatedMarginalMeansHeterogeneityAddAdjustedEstimate = FALSE,
          estimatedMarginalMeansHeterogeneitySdFactorCovariates = 1,
          estimatedMarginalMeansHeterogeneitySelectedVariables = list(optionKey = "variable", types = list(), value = list()),
          estimatedMarginalMeansHeterogeneityTransformation = "tau",
          exportColumnPrefix = FALSE,
          exportColumnPrefixValue = "",
          exportDiagnosticsCaseDiagnostics = FALSE,
          exportDiagnosticsCoefficientInfluence = FALSE,
          exportDiagnosticsInfluentialCases = FALSE,
          exportDiagnosticsModelImpact = FALSE,
          exportPredictedValues = FALSE,
          exportRandomEffects = FALSE,
          exportResidualsConditional = FALSE,
          exportResidualsPearson = FALSE,
          exportResidualsRaw = FALSE,
          exportResidualsStandardized = FALSE,
          exportResidualsStudentized = FALSE,
          exportTrueEffectEstimates = FALSE,
          exportWaapWlsEstimatedPower = FALSE,
          exportWaapWlsIncludedInWaap = FALSE,
          exportWeights = FALSE,
          fitMeasures = FALSE,
          fixParametersTau2 = FALSE,
          fixParametersTau2Value = 1,
          fixParametersWeights = FALSE,
          fixParametersWeightsVariable = list(types = "unknown", value = ""),
          fixedEffectTest = "knha",
          forestPlotAlignLeftPanel = "right",
          forestPlotAuxiliaryAddVerticalLine = FALSE,
          forestPlotAuxiliaryAddVerticalLine2 = FALSE,
          forestPlotAuxiliaryAddVerticalLineValue = 0,
          forestPlotAuxiliaryAddVerticalLineValue2 = 0,
          forestPlotAuxiliaryDigits = 2,
          forestPlotAuxiliaryEffectLabel = "Effect Size",
          forestPlotAuxiliaryPlotColor = "black",
          forestPlotAuxiliarySetXAxisLimit = FALSE,
          forestPlotAuxiliarySetXAxisLimitLower = -1,
          forestPlotAuxiliarySetXAxisLimitUpper = 1,
          forestPlotAuxiliarySetXAxisTicks = FALSE,
          forestPlotAuxiliarySetXAxisTicksValues = "-1, -0.5, 0, 0.5, 1",
          forestPlotAuxiliaryTestsInformation = "statisticAndPValue",
          forestPlotAuxiliaryXAxisTransformLabelsOnly = TRUE,
          forestPlotConditionalEstimates = FALSE,
          forestPlotEffectSizeFixedEffectEstimate = FALSE,
          forestPlotEffectSizeFixedEffectTest = TRUE,
          forestPlotEffectSizeModerationTest = FALSE,
          forestPlotEffectSizePooledEstimate = TRUE,
          forestPlotEffectSizePooledEstimateTest = TRUE,
          forestPlotEffectSizeUnadjustedFixedEffectEstimate = FALSE,
          forestPlotEffectSizeUnadjustedFixedEffectTest = TRUE,
          forestPlotEffectSizeUnadjustedRandomEffectsEstimate = FALSE,
          forestPlotEffectSizeUnadjustedRandomEffectsTest = TRUE,
          forestPlotEstimatedMarginalMeans = FALSE,
          forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate = FALSE,
          forestPlotEstimatedMarginalMeansCoefficientTests = TRUE,
          forestPlotEstimatedMarginalMeansCoefficientTestsAgainst = 0,
          forestPlotEstimatedMarginalMeansCoefficientTestsAgainst0 = TRUE,
          forestPlotEstimatedMarginalMeansSelectedVariables = list(optionKey = "variable", types = list(), value = list()),
          forestPlotEstimatedMarginalMeansTermTests = TRUE,
          forestPlotEstimatesAndConfidenceIntervals = TRUE,
          forestPlotHeterogeneityEstimateH2 = FALSE,
          forestPlotHeterogeneityEstimateI2 = FALSE,
          forestPlotHeterogeneityEstimateTau = TRUE,
          forestPlotHeterogeneityEstimateTau2 = FALSE,
          forestPlotHeterogeneityModerationTest = FALSE,
          forestPlotHeterogeneityTest = TRUE,
          forestPlotHeterogeneityTestLRT = FALSE,
          forestPlotHeterogeneityTestWald = FALSE,
          forestPlotMappingColor = list(types = "unknown", value = ""),
          forestPlotMappingShape = list(types = "unknown", value = ""),
          forestPlotModelInformation = FALSE,
          forestPlotPredictionIntervals = TRUE,
          forestPlotPublicationBiasTest = FALSE,
          forestPlotSizeAxisLabels = 1,
          forestPlotSizeEstimates = 1,
          forestPlotSizeLeftPanel = 1,
          forestPlotSizePlotArea = 1,
          forestPlotSizeRightPanel = 1,
          forestPlotSizeRow = 1,
          forestPlotSizeText = 1,
          forestPlotStudyInformation = FALSE,
          forestPlotStudyInformationAggregateBy = list(types = "unknown", value = ""),
          forestPlotStudyInformationAggregateMethod = "boxplot",
          forestPlotStudyInformationAggregateMethodBubbleRelativeSize = 1,
          forestPlotStudyInformationBoxplotWeightsNormalized = TRUE,
          forestPlotStudyInformationBoxplotWeightsPercentage = TRUE,
          forestPlotStudyInformationOrderAscending = FALSE,
          forestPlotStudyInformationOrderBy = list(types = "unknown", value = ""),
          forestPlotStudyInformationPredictedEffects = FALSE,
          forestPlotStudyInformationSecondaryConfidenceInterval = FALSE,
          forestPlotStudyInformationSecondaryConfidenceIntervalLevel = 0.89,
          forestPlotStudyInformationSelectedVariables = list(types = list(), value = list()),
          forestPlotStudyInformationSelectedVariablesSettings = list(),
          forestPlotStudyInformationStudyWeights = FALSE,
          forestPlotSubgroupFullDatasetEstimatedMarginalMeans = TRUE,
          forestPlotSubgroupFullDatasetModelInformation = TRUE,
          forestPlotSubgroupPanelsWithinSubgroup = FALSE,
          forestPlotSubgroupShowTitles = TRUE,
          forestPlotTestsInRightPanel = FALSE,
          heterogeneityGamma = TRUE,
          heterogeneityH2 = FALSE,
          heterogeneityI2 = FALSE,
          heterogeneityModelIncludeIntercept = TRUE,
          heterogeneityModelLink = "log",
          heterogeneityModelTerms = list(optionKey = "components", types = list(), value = list()),
          heterogeneitySqrtGamma = TRUE,
          heterogeneityTau = TRUE,
          heterogeneityTau2 = TRUE,
          includeFullDatasetInSubgroupAnalysis = FALSE,
          metaregressionCoefficientCorrelationMatrix = FALSE,
          metaregressionCoefficientEstimates = TRUE,
          metaregressionTermTests = TRUE,
          method = "restrictedML",
          optimizerConvergenceRelativeTolerance = FALSE,
          optimizerConvergenceRelativeToleranceValue = 1e-08,
          optimizerConvergenceTolerance = FALSE,
          optimizerConvergenceToleranceValue = 1e-05,
          optimizerFinalTrustRegionRadius = FALSE,
          optimizerFinalTrustRegionRadiusValue = 1,
          optimizerInitialTau2 = FALSE,
          optimizerInitialTau2Value = 1,
          optimizerInitialTrustRegionRadius = FALSE,
          optimizerInitialTrustRegionRadiusValue = 1,
          optimizerMaximumEvaluations = FALSE,
          optimizerMaximumEvaluationsValue = 250,
          optimizerMaximumIterations = FALSE,
          optimizerMaximumIterationsValue = 150,
          optimizerMaximumRestarts = FALSE,
          optimizerMaximumRestartsValue = 3,
          optimizerMaximumTau2 = FALSE,
          optimizerMaximumTau2Value = 100,
          optimizerMethod = "nlminb",
          optimizerMinimumTau2 = FALSE,
          optimizerMinimumTau2Value = 1e-06,
          optimizerStepAdjustment = FALSE,
          optimizerStepAdjustmentValue = 1,
          permutationTest = FALSE,
          permutationTestIteration = 1000,
          permutationTestType = "approximate",
          petPeeseCoefficientEstimates = TRUE,
          petPeeseRegressionPlot = FALSE,
          petPeeseThreshold = 0.1,
          plotHeight = 320,
          plotWidth = 480,
          predictionIntervals = TRUE,
          predictors = list(types = list(), value = list()),
          randomEffectsConfidenceIntervals = FALSE,
          randomEffectsTestInclusion = FALSE,
          seed = 1,
          setSeed = FALSE,
          showMetaforRCode = FALSE,
          standardErrors = TRUE,
          studyLabels = list(types = list(), value = ""),
          subgroup = list(types = list(), value = ""),
          transformEffectSize = "none",
          useSparseMatrices = FALSE,
          weightFunctionCoefficientEstimates = TRUE,
          weightFunctionPValueFrequencyTable = FALSE,
          weightFunctionPlot = FALSE,
          weightFunctionPlotRescaleXAxis = TRUE,
          weightedEstimation = TRUE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("advancedExtendMetaforCallCode", "bubblePlotBubblesSize", "bubblePlotLegendPosition", "bubblePlotSelectedVariable", "bubblePlotSeparateLines", "bubblePlotSeparatePlots", "bubblePlotTheme", "clustering", "colorPalette", "contrastsEffectSizePValueAdjustment", "contrastsHeterogeneityPValueAdjustment", "effectSize", "effectSizeModelTerms", "effectSizeStandardError", "estimatedMarginalMeansEffectSizeSelectedVariables", "estimatedMarginalMeansHeterogeneitySelectedVariables", "estimatedMarginalMeansHeterogeneityTransformation", "fixParametersWeightsVariable", "fixedEffectTest", "forestPlotAlignLeftPanel", "forestPlotAuxiliaryPlotColor", "forestPlotAuxiliaryTestsInformation", "forestPlotEstimatedMarginalMeansSelectedVariables", "forestPlotMappingColor", "forestPlotMappingShape", "forestPlotStudyInformationAggregateBy", "forestPlotStudyInformationOrderBy", "forestPlotStudyInformationSelectedVariables", "forestPlotStudyInformationSelectedVariablesSettings", "heterogeneityModelLink", "heterogeneityModelTerms", "method", "optimizerMethod", "predictors", "studyLabels", "subgroup", "transformEffectSize")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "ClassicalMetaAnalysis", "ClassicalMetaAnalysis.qml", options, version, TRUE))
}