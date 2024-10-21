#
# Copyright (C) 2013-2018 University of Amsterdam
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


ClassicalMetaAnalysis <- function(jaspResults, dataset = NULL, options, ...) {

  options[["module"]] <- "metaAnalysis"

  if (.maReady(options)) {
    dataset <- .maReadData(dataset, options)
    .maCheckErrors(dataset, options)
  }

  .ClassicalMetaAnalysisCommon(jaspResults, dataset, options)

  return()
}

.maDependencies        <- c(
  "effectSize", "effectSizeStandardError", "predictors", "predictors.types", "clustering", "method", "fixedEffectTest",
  "effectSizeModelTerms", "effectSizeModelIncludeIntercept",
  "clusteringUseClubSandwich", "clusteringSmallSampleCorrection",
  "confidenceIntervalsLevel",
  "fixParametersTau2", "fixParametersTau2Value",
  "fixParametersWeights", "fixParametersWeightsVariable",
  "weightedEstimation",
  "diagnosticsCasewiseDiagnosticsRerunWithoutInfluentialCases",
  # optimizer settings
  "optimizerMethod", "optimizerInitialTau2", "optimizerInitialTau2Value",
  "optimizerMinimumTau2", "optimizerMinimumTau2Value", "optimizerMaximumTau2", "optimizerMaximumTau2Value",
  "optimizerMaximumIterations", "optimizerMaximumIterationsValue", "optimizerConvergenceTolerance", "optimizerConvergenceToleranceValue",
  "optimizerConvergenceRelativeTolerance", "optimizerConvergenceRelativeToleranceValue", "optimizerStepAdjustment", "optimizerStepAdjustmentValue",
  # simple ma specific
  "heterogeneityModelTerms", "heterogeneityModelIncludeIntercept", "heterogeneityModelLink",
  "permutationTest", "permutationTestIteration", "permutationTestType",
  # multilevel/multivariate specific
  "randomEffects", "randomEffectsSpecification",
  "computeCovarianceMatrix", "computeCovarianceMatrix"
)
.maForestPlotDependencies <- c(
  .maDependencies, "transformEffectSize", "confidenceIntervalsLevel",
  "forestPlotStudyInformation",
  "forestPlotStudyInformationAllVariables",
  "forestPlotStudyInformationSelectedVariables",
  "forestPlotStudyInformationSelectedVariablesSettings",
  "forestPlotStudyInformationPredictedEffects",
  "forestPlotStudyInformationStudyWeights",
  "forestPlotStudyInformationOrderBy",
  "forestPlotStudyInformationOrderAscending",
  "forestPlotEstimatedMarginalMeans",
  "forestPlotEstimatedMarginalMeansModelVariables",
  "forestPlotEstimatedMarginalMeansSelectedVariables",
  "forestPlotEstimatedMarginalMeansTermTests",
  "forestPlotEstimatedMarginalMeansCoefficientTests",
  "forestPlotEstimatedMarginalMeansCoefficientTestsAgainst",
  "forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate",
  "forestPlotModelInformation",
  "forestPlotPooledEffectSizeEstimate",
  "forestPlotPooledEffectSizeTest",
  "forestPlotResidualHeterogeneityTest",
  "forestPlotResidualHeterogeneityEstimate",
  "forestPlotEffectSizeModerationTest",
  "forestPlotHeterogeneityModerationTest",
  "forestPlotPredictionIntervals",
  "forestPlotEstimatesAndConfidenceIntervals",
  "forestPlotTestsInRightPanel",
  "forestPlotMappingColor",
  "forestPlotMappingShape",
  "forestPlotRelativeSizeEstimates",
  "forestPlotRelativeSizeText",
  "forestPlotRelativeSizeAxisLabels",
  "forestPlotRelativeSizeRow",
  "forestPlotRelativeSizeLeftPanel",
  "forestPlotRelativeSizeMiddlePanel",
  "forestPlotRelativeSizeRightPanel",
  "forestPlotAuxiliaryAdjustWidthBasedOnText",
  "forestPlotAuxiliaryDigits",
  "forestPlotAuxiliaryTestsInformation",
  "forestPlotAuxiliaryPlotColor",
  "forestPlotAuxiliaryAddVerticalLine",
  "forestPlotAuxiliaryAddVerticalLineValue",
  "forestPlotAuxiliaryAddVerticalLine2",
  "forestPlotAuxiliaryAddVerticalLineValue2",
  "forestPlotAuxiliaryEffectLabel",
  "forestPlotAuxiliarySetXAxisLimit",
  "forestPlotAuxiliarySetXAxisLimitLower",
  "forestPlotAuxiliarySetXAxisLimitUpper",
  "forestPlotStudyInformationSecondaryConfidenceInterval",
  "forestPlotStudyInformationSecondaryConfidenceIntervalLevel"
)
.maBubblePlotDependencies <- c(
  .maDependencies, "transformEffectSize", "confidenceIntervalsLevel",
  "bubblePlotSelectedVariable",
  "bubblePlotSeparateLines",
  "bubblePlotSeparatePlots",
  "bubblePlotSdFactorCovariates",
  "bubblePlotBubblesSize",
  "bubblePlotBubblesRelativeSize",
  "bubblePlotBubblesTransparency",
  "bubblePlotBubblesJitter",
  "bubblePlotCondifenceIntervals",
  "bubblePlotCondifenceIntervalsTransparency",
  "bubblePlotPredictionIntervals",
  "bubblePlotPredictionIntervalsTransparency",
  "colorPalette",
  "bubblePlotTheme",
  "bubblePlotLegendPosition",
  "bubblePlotRelativeSizeText"
)
.maReady               <- function(options) {

  inputReady <- options[["effectSize"]] != "" && options[["effectSizeStandardError"]] != ""
  termsEffectSizeReady    <- length(options[["effectSizeModelTerms"]]) > 0    || options[["effectSizeModelIncludeIntercept"]]
  termsHeterogeneityReady <- length(options[["heterogeneityModelTerms"]]) > 0 || options[["heterogeneityModelIncludeIntercept"]]

  return(inputReady && termsEffectSizeReady && termsHeterogeneityReady)
}
.maReadData            <- function(dataset, options) {

#  if (!is.null(dataset))
#    return(dataset)

  # model data
  predictorsNominal <- options[["predictors"]][options[["predictors.types"]] == "nominal"]
  predictorsScale   <- options[["predictors"]][options[["predictors.types"]] == "scale"]

  # forest plotting data
  additionalVariables <- unique(c(
    if (options[["studyLabels"]] != "") options[["studyLabels"]],
    if (length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) unlist(options[["forestPlotStudyInformationSelectedVariables"]]),
    if (options[["forestPlotMappingColor"]] != "") options[["forestPlotMappingColor"]],
    if (options[["forestPlotMappingShape"]] != "") options[["forestPlotMappingShape"]],
    if (options[["forestPlotStudyInformationOrderBy"]] != "")    options[["forestPlotStudyInformationOrderBy"]]
  ))
  # remove variables already specified in the model
  additionalVariables <- setdiff(
    additionalVariables,
    c(predictorsNominal, predictorsScale, options[["effectSize"]], options[["effectSizeStandardError"]], options[["clustering"]])
  )


  # load data
  dataset <- .readDataSetToEnd(
    columns.as.factor = c(
      if (length(predictorsNominal) > 0) predictorsNominal,
      if (options[["clustering"]] != "") options[["clustering"]],
      additionalVariables
    ),
    columns.as.numeric  = c(
      options[["effectSize"]],
      options[["effectSizeStandardError"]],
      if (length(predictorsScale) > 0) predictorsScale,
      if (options[["fixParametersWeights"]]) options[["fixParametersWeightsVariable"]]
    ))

  # omit NAs
  omitOnVariables <- c(
    options[["effectSize"]],
    options[["effectSizeStandardError"]],
    if (options[["clustering"]] != "") options[["clustering"]],
    if (length(predictorsNominal) > 0) predictorsNominal,
    if (length(predictorsScale) > 0)   predictorsScale
  )
  anyNaByRows <- apply(dataset[,omitOnVariables], 1, function(x) anyNA(x))
  dataset     <- dataset[!anyNaByRows,]
  attr(dataset, "NAs") <- sum(anyNaByRows)

  return(dataset)
}
.maCheckErrors         <- function(dataset, options) {

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations", "variance"),
    all.target           = c(
      options[["effectSize"]],
      options[["effectSizeStandardError"]],
      options[["predictors"]][options[["predictors.types"]] == "scale"]
    ),
    observations.amount  = "< 2",
    exitAnalysisIfErrors = TRUE)

  .hasErrors(
    dataset              = dataset,
    type                 = c("modelInteractions"),
    modelInteractions.modelTerms = c(options[["effectSizeModelTerms"]], options[["heterogeneityModelTerms"]]),
    exitAnalysisIfErrors = TRUE)

  .hasErrors(
    dataset              = dataset,
    seCheck.target       = options[["effectSizeStandardError"]],
    custom               = .maCheckStandardErrors,
    exitAnalysisIfErrors = TRUE)
}
.maCheckStandardErrors <- list(seCheck = function(dataset, target) {
    nonPositive <- !all(dataset[,target] > 0)
    if (nonPositive) {
      return(gettext("All standard errors must be positive."))
    }
  })
