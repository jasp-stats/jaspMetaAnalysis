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

#' Mantel-Haenszel / Peto
#'
#' Mantel-Haenszel / Peto meta-analysis allows you to conduct a meta-analysis of frequencies / time to event outcomes.
#'
#' @param advancedContinuityCorrection, Apply continuity correction to Cochran-Mantel-Haenszel test statistic.
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
#' @param effectSizeMeasure, Select the effect size measure to be used in the meta-analysis.
#' \itemize{
#'   \item \code{"OR"} (default) : Log odds ratio
#'   \item \code{"RR"}: Log risk ratio
#'   \item \code{"RD"}: Risk difference
#' }
#' @param fitMeasures, Include fit statistics for the model, such as AIC and BIC.
#'    Defaults to \code{FALSE}.
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
#'    Defaults to \code{FALSE}.
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
#'    Defaults to \code{TRUE}.
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
#' @param method, Select the method to be used for the meta-analysis.
#' @param petPeeseCoefficientEstimates, Include the estimate of the bias coefficient: the standard error (PET) or the sampling variance (PEESE) coefficient of the reported model.
#'    Defaults to \code{TRUE}.
#' @param petPeeseRegressionPlot, Visualize the fitted relationship between the effect sizes and their standard errors. The fitted line (PET) or curve (PEESE) at a standard error of zero corresponds to the adjusted estimate.
#'    Defaults to \code{FALSE}.
#' @param predictionIntervals, Include prediction intervals in the tabular output. Unavailable for unrestricted weighted least squares.
#'    Defaults to \code{TRUE}.
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
#' @param weightFunctionCoefficientEstimates, Include weight function estimates in the output.
#'    Defaults to \code{TRUE}.
#' @param weightFunctionPValueFrequencyTable, Estimate counts within the specified p-value intervals.
#'    Defaults to \code{FALSE}.
#' @param weightFunctionPlot, Visualize the relative probability of study being published across the range of p-values.
#'    Defaults to \code{FALSE}.
#' @param weightFunctionPlotRescaleXAxis, Space p-value thresholds equally along the x-axis.
#'    Defaults to \code{TRUE}.
ClassicalMantelHaenszelPeto <- function(
          data = NULL,
          version = "1",
          advancedAdd = 0.5,
          advancedContinuityCorrection = TRUE,
          advancedDropStudiesWithNoCasesOrEvents = "no",
          advancedTo = "onlyZero",
          confidenceIntervals = TRUE,
          confidenceIntervalsLevel = 0.95,
          diagnosticsCasewiseDiagnostics = FALSE,
          diagnosticsCasewiseDiagnosticsDifferenceInCoefficients = FALSE,
          diagnosticsCasewiseDiagnosticsIncludePredictors = FALSE,
          diagnosticsCasewiseDiagnosticsShowInfluentialOnly = FALSE,
          diagnosticsPlotsBaujat = FALSE,
          diagnosticsPlotsProfileLikelihood = FALSE,
          diagnosticsResidualFunnel = FALSE,
          diagnosticsVarianceInflationFactor = FALSE,
          diagnosticsVarianceInflationFactorAggregate = TRUE,
          effectSizeMeasure = "OR",
          eventsGroup1 = list(types = list(), value = ""),
          eventsGroup2 = list(types = list(), value = ""),
          fitMeasures = FALSE,
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
          forestPlotEstimatedMarginalMeansSelectedVariables = list(types = list(), value = list()),
          forestPlotEstimatedMarginalMeansTermTests = TRUE,
          forestPlotEstimatesAndConfidenceIntervals = TRUE,
          forestPlotHeterogeneityEstimateH2 = FALSE,
          forestPlotHeterogeneityEstimateI2 = FALSE,
          forestPlotHeterogeneityEstimateTau = FALSE,
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
          heterogeneityI2 = TRUE,
          heterogeneitySqrtGamma = TRUE,
          heterogeneityTau = TRUE,
          heterogeneityTau2 = TRUE,
          includeFullDatasetInSubgroupAnalysis = FALSE,
          metaregressionCoefficientCorrelationMatrix = FALSE,
          metaregressionCoefficientEstimates = TRUE,
          metaregressionTermTests = TRUE,
          method = "mantelHaenszelFrequencies",
          personTimeGroup1 = list(types = list(), value = ""),
          personTimeGroup2 = list(types = list(), value = ""),
          petPeeseCoefficientEstimates = TRUE,
          petPeeseRegressionPlot = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          predictionIntervals = TRUE,
          randomEffectsConfidenceIntervals = FALSE,
          randomEffectsTestInclusion = FALSE,
          sampleSizeGroup1 = list(types = list(), value = ""),
          sampleSizeGroup2 = list(types = list(), value = ""),
          showMetaforRCode = FALSE,
          standardErrors = TRUE,
          studyLabels = list(types = list(), value = ""),
          subgroup = list(types = list(), value = ""),
          transformEffectSize = "none",
          weightFunctionCoefficientEstimates = TRUE,
          weightFunctionPValueFrequencyTable = FALSE,
          weightFunctionPlot = FALSE,
          weightFunctionPlotRescaleXAxis = TRUE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("advancedTo", "effectSizeMeasure", "eventsGroup1", "eventsGroup2", "forestPlotAlignLeftPanel", "forestPlotAuxiliaryPlotColor", "forestPlotAuxiliaryTestsInformation", "forestPlotEstimatedMarginalMeansSelectedVariables", "forestPlotMappingColor", "forestPlotMappingShape", "forestPlotStudyInformationAggregateBy", "forestPlotStudyInformationOrderBy", "forestPlotStudyInformationSelectedVariables", "forestPlotStudyInformationSelectedVariablesSettings", "method", "personTimeGroup1", "personTimeGroup2", "sampleSizeGroup1", "sampleSizeGroup2", "studyLabels", "subgroup", "transformEffectSize")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "ClassicalMantelHaenszelPeto", "ClassicalMantelHaenszelPeto.qml", options, version, TRUE))
}