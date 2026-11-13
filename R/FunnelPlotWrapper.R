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

#' Funnel Plot
#'
#' Funnel plots allow you to visualize the observed effect sizes and standard errors with the funnel outlaying the sampling distribution under a given meta-analytic model. Funnel plots are often used to assess publication bias and small-study effects. However, many studies have shown that funnel plots are not very reliable for this purpose. See e.g. Lau et al., (2006) and Terrin et al., (2005).
#' 
#' See [this tutorial](https://doi.org/10.48550/arXiv.2509.09845) for a detailed introduction to the module.
#'
#' @param effectSize, Variable containing the observed effect sizes.
#' @param effectSizeStandardError, Variable containing the standard errors corresponding to the effect sizes.
#' @param estimatesLegendPosition, Set the legend position of the funnel plot. Available when color or shape mapping is enabled.
#' @param estimatesMappingColor, Map colors of the effect size estimates in the funnel plot based on the selected variable.
#' @param estimatesMappingLabel, Show all or only a subset of the effect size estimate labels.
#' @param estimatesMappingLabelOffset, Adjust the offset of labels in the funnel plot.
#' @param estimatesMappingShape, Map shapes of the effect size estimates in the funnel plot based on the selected variable.
#' @param failSafeN, Compute the minimum number of studies averaging null results that would have to be added to a given set of studies to change the conclusion of a meta-analysis tests to detect asymmetry in the funnel plot indicating potential publication bias.
#'    Defaults to \code{FALSE}.
#' @param failSafeNAlpha, Set the significance level for the Rosenthal's, Rosenberg's, and general fail-safe N tests.
#' @param failSafeNGeneral, A general method to calculate the minimum number of studies averaging null results needed to reduce the significance level or the average effect size to a target effect size value. The default is set to 0.10 which is completely aribitrary.
#'    Defaults to \code{FALSE}.
#' @param failSafeNGeneralExact, Use the exact method to calculate the fail-safe N.
#'    Defaults to \code{FALSE}.
#' @param failSafeNOrwin, The Orwin method calculates the minimum number of studies averaging null results needed to reduce the average effect size to a target value. The default is set to 0.10 which is completely aribitrary.
#'    Defaults to \code{FALSE}.
#' @param failSafeNRosenberg, The Rosenberg method calculates the minimum number of studies averaging null results needed to reduce the significance level of the average effect size to a specified alpha level.
#'    Defaults to \code{FALSE}.
#' @param failSafeNRosenthal, The Rosenthal method calculates the minimum number of studies averaging null results needed to reduce the combined significance level to a specified alpha level.
#'    Defaults to \code{TRUE}.
#' @param failSafeNTarget, Set the target effect size for the Orwin's and general fail-safe N tests.
#' @param funnelPlotAsymmetryTests, Perform tests to detect asymmetry in the funnel plot indicating potential publication bias.
#'    Defaults to \code{FALSE}.
#' @param funnelPlotAsymmetryTestsMetaRegression, Include meta-regression tests for funnel plot asymmetry. The test is performed with the 'Method' specified under 'General Settings'.
#'    Defaults to \code{TRUE}.
#' @param funnelPlotAsymmetryTestsRankCorrelation, Include rank correlation tests (also known as Begg's test) for funnel plot asymmetry.
#'    Defaults to \code{FALSE}.
#' @param funnelPlotAsymmetryTestsWeightedRegression, Include weighted regression tests (also known as Egger's test) for funnel plot asymmetry.
#'    Defaults to \code{FALSE}.
#' @param funnelPredictionInterval, Specify the confidence levels for the funnel plot prediction intervals.
#' @param funnelUnderH0, Specify the funnel plot under the null hypothesis.
#'    Defaults to \code{FALSE}.
#' @param funnelUnderH0FillColors, Fill the funnel plot's prediction intervals under the null hypothesis with different colors.
#'    Defaults to \code{FALSE}.
#' @param funnelUnderH0LineType, Set the type of line of the funnel plot's prediction intervals under the null hypothesis.
#' @param funnelUnderH0ParametersFixedMu, Fixed value for the mean effect size under the null hypothesis.
#' @param funnelUnderH0ParametersFixedTau, Fixed value for 𝜏, representing the heterogeneity under the null hypothesis.
#' @param funnelUnderH1, Estimate or specify the funnel plot under the alternative hypothesis.
#'    Defaults to \code{TRUE}.
#' @param funnelUnderH1EstimatesTable, Summarize the effect size and heterogeneity estimates used for the funnel plot under the alternative hypothesis in a table.
#'    Defaults to \code{FALSE}.
#' @param funnelUnderH1FillColors, Fill the funnel plot's prediction intervals under the alternative hypothesis with different colors.
#'    Defaults to \code{TRUE}.
#' @param funnelUnderH1IncludeHeterogeneity, Include heterogeneity (𝜏) in the funnel plot under the alternative hypothesis. If unselected, the heterogeneity estimate is not used to adjust the prediction intervals.
#'    Defaults to \code{FALSE}.
#' @param funnelUnderH1LineType, Set the type of line of the funnel plot's prediction intervals under the alternative hypothesis.
#' @param funnelUnderH1Parameters, Choose whether to estimate or manually fix the parameters for the funnel plot under the alternative hypothesis.
#' \itemize{
#'   \item \code{"estimated"}: Estimate the mean effect size and heterogeneity under the alternative hypothesis. The paramters are estimated with the 'Method' specified under 'General Settings'.
#'   \item \code{"fixed"}
#' }
#' @param funnelUnderH1ParametersFixedMu, Fixed value for the mean effect size under the alternative hypothesis.
#' @param funnelUnderH1ParametersFixedTau, Fixed value for 𝜏, representing the heterogeneity under the alternative hypothesis.
#' @param funnelUnderH1PowerEnhancement, Visualize power levels to detect the effect size under the alternative hypothesis with alpha = 0.05.
#'    Defaults to \code{FALSE}.
#' @param funnelUnderH1PowerEnhancementBreaks, Specify the breakpoints for power enhancement. All levels must be within the 0.05 - 1 range.
#' @param invertColors, Invert the colors used in the funnel plot.
#'    Defaults to \code{FALSE}.
#' @param method, Select the heterogeneity estimation method for the funnel plot under the alternative hypothesis, meta-regression funnel plot asymmetry tests, trim and fill, and the fail-safe N general method.
#' @param split, Variable used to split the funnel plot into separate groups or categories.
#' @param studyLabel, Variable containing labels for the studies. Used for labeling outputs and plots.
#' @param trimAndFill, Perform the trim and fill to adjust the funnel plot for publication bias. The test is performed with the 'Method' specified under 'General Settings'.
#'    Defaults to \code{FALSE}.
#' @param trimAndFillEstimatesTable, Summarize the effect size, heterogeneity, and number of imputed estimatesused for the trim and fill in a table.
#'    Defaults to \code{FALSE}.
#' @param trimAndFillEstimator, Select the method for the trim-and-fill adjustment.
#' @param trimAndFillFillColors, Fill the funnel plot's prediction intervals under trim and fill with different colors.
#'    Defaults to \code{TRUE}.
#' @param trimAndFillIncludeHeterogeneity, Include heterogeneity (𝜏) in the trim and fill funnel plot. If unselected, the heterogeneity estimate is not used to adjust the prediction intervals.
#'    Defaults to \code{FALSE}.
#' @param trimAndFillLineType, Set the type of line of the funnel plot's prediction intervals under trim and fill.
FunnelPlot <- function(
          data = NULL,
          version = "1",
          colorPalette = "colorblind",
          effectSize = list(types = list(), value = ""),
          effectSizeStandardError = list(types = list(), value = ""),
          estimatesLegendPosition = "right",
          estimatesMappingColor = list(types = "unknown", value = ""),
          estimatesMappingLabel = "all",
          estimatesMappingLabelOffset = 0.1,
          estimatesMappingShape = list(types = "unknown", value = ""),
          failSafeN = FALSE,
          failSafeNAlpha = 0.05,
          failSafeNGeneral = FALSE,
          failSafeNGeneralExact = FALSE,
          failSafeNOrwin = FALSE,
          failSafeNRosenberg = FALSE,
          failSafeNRosenthal = TRUE,
          failSafeNTarget = 0.1,
          funnelPlotAsymmetryTests = FALSE,
          funnelPlotAsymmetryTestsMetaRegression = TRUE,
          funnelPlotAsymmetryTestsRankCorrelation = FALSE,
          funnelPlotAsymmetryTestsWeightedRegression = FALSE,
          funnelPredictionInterval = "(0.90, 0.95, 0.99)",
          funnelUnderH0 = FALSE,
          funnelUnderH0FillColors = FALSE,
          funnelUnderH0LineType = "solid",
          funnelUnderH0ParametersFixedMu = 0,
          funnelUnderH0ParametersFixedTau = 0,
          funnelUnderH1 = TRUE,
          funnelUnderH1EstimatesTable = FALSE,
          funnelUnderH1FillColors = TRUE,
          funnelUnderH1IncludeHeterogeneity = FALSE,
          funnelUnderH1LineType = "none",
          funnelUnderH1Parameters = "estimated",
          funnelUnderH1ParametersFixedMu = 0,
          funnelUnderH1ParametersFixedTau = 0,
          funnelUnderH1PowerEnhancement = FALSE,
          funnelUnderH1PowerEnhancementBreaks = "(0.15, 0.30, 0.50, 0.70, 0.90)",
          invertColors = FALSE,
          method = "restrictedML",
          plotHeight = 320,
          plotWidth = 480,
          split = list(types = list(), value = ""),
          studyLabel = list(types = list(), value = ""),
          trimAndFill = FALSE,
          trimAndFillEstimatesTable = FALSE,
          trimAndFillEstimator = "L0",
          trimAndFillFillColors = TRUE,
          trimAndFillIncludeHeterogeneity = FALSE,
          trimAndFillLineType = "none") {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("colorPalette", "effectSize", "effectSizeStandardError", "estimatesLegendPosition", "estimatesMappingColor", "estimatesMappingLabel", "estimatesMappingShape", "funnelUnderH0LineType", "funnelUnderH1LineType", "method", "split", "studyLabel", "trimAndFillEstimator", "trimAndFillLineType")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "FunnelPlot", "FunnelPlot.qml", options, version, TRUE))
}