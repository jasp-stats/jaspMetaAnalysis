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

#' Classical Prediction Model Performance
#'
ClassicalPredictionPerformance <- function(
          data = NULL,
          version = "1",
          effectSize = list(types = list(), value = ""),
          effectSizeCi = list(),
          effectSizeSe = list(types = list(), value = ""),
          exportComputedEffectSize = FALSE,
          exportComputedEffectSizeCStatisticColumnName = "",
          exportComputedEffectSizeCStatisticLCiColumnName = "",
          exportComputedEffectSizeCStatisticUCiColumnName = "",
          exportComputedEffectSizeOeRatioColumnName = "",
          exportComputedEffectSizeOeRatioLCiColumnName = "",
          exportComputedEffectSizeOeRatioUCiColumnName = "",
          forestPlot = FALSE,
          forestPlotEstimates = TRUE,
          forestPlotLabels = TRUE,
          funnelPlotAsymmetryTest = FALSE,
          funnelPlotAsymmetryTestDebray = FALSE,
          funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion = FALSE,
          funnelPlotAsymmetryTestEggerUnweighted = TRUE,
          funnelPlotAsymmetryTestMacaskill = FALSE,
          funnelPlotAsymmetryTestMacaskillPooled = FALSE,
          funnelPlotAsymmetryTestPeters = FALSE,
          funnelPlotAsymmetryTestPlot = FALSE,
          measure = "oeRatio",
          method = "restrictedML",
          numberOfExpectedEvents = list(types = list(), value = ""),
          numberOfObservedEvents = list(types = list(), value = ""),
          numberOfParticipants = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          priorAndPosteriorPlot = FALSE,
          studyLabel = list(types = list(), value = ""),
          withinStudyVariation = "normal/log") {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("effectSize", "effectSizeCi", "effectSizeSe", "method", "numberOfExpectedEvents", "numberOfObservedEvents", "numberOfParticipants", "studyLabel", "withinStudyVariation")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "ClassicalPredictionPerformance", "ClassicalPredictionPerformance.qml", options, version, TRUE))
}