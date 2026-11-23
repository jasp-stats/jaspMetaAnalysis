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

#' Forest Plot
#'
#' Create a forest plot from pre-computed effect sizes and confidence intervals without fitting a meta-analytic model. This allows visualizing results from any source — external software, individual studies, or custom computations.
#'
#' @param confidenceInterval, Two variables specifying the lower and upper bounds of the confidence interval. If not provided, intervals are computed from the standard error at the 95%% level.
#' @param effectSize, Variable containing the effect size estimates.
#' @param effectSizeStandardError, Variable containing the standard errors. Used to compute confidence intervals when not provided directly, and to determine point sizes via inverse-variance weights when no weight variable is specified.
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
#' @param forestPlotEstimateInformationSelectedVariables, Select variables containing estimate-level information to be printed in the left section of the panel for estimate rows.
#' @param forestPlotEstimateInformationSelectedVariablesSettings, Adjust the Title, Width, and Alignment of each column in the left section for estimate rows.
#' @param forestPlotEstimatesAndConfidenceIntervals, Include effect size estimates and confidence intervals summary text in the right panel.
#'    Defaults to \code{TRUE}.
#' @param forestPlotPredictionIntervals, Include prediction intervals of the estimated marginal means and the model information output.
#'    Defaults to \code{TRUE}.
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
#' @param predictionInterval, Two variables specifying the lower and upper bounds of the prediction interval. Displayed as rectangles behind estimate diamonds.
#' @param row, Variable controlling the vertical positioning of rows. The first position appears at the top of the figure. If not provided, the dataset row order is used.
#' @param subgroup, Nominal variable defining subgroups. Each subgroup is displayed as a separate panel in the forest plot.
#' @param transformEffectSize, Apply a transformation to the effect sizes and confidence intervals for display.
#' @param visualizationType, Nominal variable whose levels distinguish study rows (displayed as points with intervals) from estimate rows (displayed as diamonds).
#' @param visualizationTypeEstimate, Specify the value in the Visualization Type variable that indicates the estimate visualization type.
#' @param visualizationTypeStudy, Specify the value in the Visualization Type variable that indicates the study visualization type.
#' @param weight, Variable containing study weight. When specified, study points are scaled by these weights and the study weights can be shown in the study information panel.
ForestPlot <- function(
          data = NULL,
          version = "1",
          confidenceInterval = list(),
          effectSize = list(types = list(), value = ""),
          effectSizeStandardError = list(types = list(), value = ""),
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
          forestPlotEstimateInformationSelectedVariables = list(types = list(), value = list()),
          forestPlotEstimateInformationSelectedVariablesSettings = list(),
          forestPlotEstimatesAndConfidenceIntervals = TRUE,
          forestPlotMappingColor = list(types = "unknown", value = ""),
          forestPlotMappingShape = list(types = "unknown", value = ""),
          forestPlotPredictionIntervals = TRUE,
          forestPlotSizeAxisLabels = 1,
          forestPlotSizeEstimates = 1,
          forestPlotSizeLeftPanel = 1,
          forestPlotSizePlotArea = 1,
          forestPlotSizeRightPanel = 1,
          forestPlotSizeRow = 1,
          forestPlotSizeText = 1,
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
          plotHeight = 320,
          plotWidth = 480,
          predictionInterval = list(),
          row = list(types = list(), value = ""),
          subgroup = list(types = list(), value = ""),
          transformEffectSize = "none",
          visualizationType = list(types = list(), value = ""),
          visualizationTypeEstimate = "",
          visualizationTypeStudy = "",
          weight = list(types = list(), value = "")) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("confidenceInterval", "effectSize", "effectSizeStandardError", "forestPlotAlignLeftPanel", "forestPlotAuxiliaryPlotColor", "forestPlotAuxiliaryTestsInformation", "forestPlotEstimateInformationSelectedVariables", "forestPlotEstimateInformationSelectedVariablesSettings", "forestPlotMappingColor", "forestPlotMappingShape", "forestPlotStudyInformationAggregateBy", "forestPlotStudyInformationOrderBy", "forestPlotStudyInformationSelectedVariables", "forestPlotStudyInformationSelectedVariablesSettings", "predictionInterval", "row", "subgroup", "transformEffectSize", "visualizationType", "visualizationTypeEstimate", "visualizationTypeStudy", "weight")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "ForestPlot", "ForestPlot.qml", options, version, TRUE))
}