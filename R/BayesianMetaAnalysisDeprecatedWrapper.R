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

#' Bayesian Meta-Analysis (Deprecated)
#'
BayesianMetaAnalysisDeprecated <- function(
          data = NULL,
          version = "1",
          bayesFactorComputation = "integration",
          bayesFactorType = "BF10",
          bfSequentialPlot = FALSE,
          bridgeSamplingSamples = 5000,
          cauchyLocation = 0,
          cauchyScale = 0.707,
          chains = 4,
          constrainedRandomDirection = "positive",
          cumulativeForestPlot = FALSE,
          cumulativeForestPlotPrior = FALSE,
          effectSize = list(types = list(), value = ""),
          effectSizeCi = list(),
          effectSizePerStudy = FALSE,
          effectSizeSe = list(types = list(), value = ""),
          forestPlot = TRUE,
          forestPlotEffect = "observed",
          forestPlotLabel = TRUE,
          forestPlotOrder = "yearDescending",
          forestPlotRowOrder = "ascending",
          halfTDf = 1,
          halfTScale = 0.707,
          inverseGammaScale = 0.15,
          inverseGammaShape = 1,
          model = "averaging",
          modelProbability = FALSE,
          modelProbabilitySequentialPlot = FALSE,
          normalMean = 0,
          normalSd = 0.707,
          plotHeight = 320,
          plotWidth = 480,
          priorEffectSize = "cauchy",
          priorModelProbabilityFixedAlternative = 0.25,
          priorModelProbabilityFixedNull = 0.25,
          priorModelProbabilityRandomAlternative = 0.25,
          priorModelProbabilityRandomNull = 0.25,
          priorPlot = FALSE,
          priorPosterior = FALSE,
          priorPosteriorAdditionalInfo = FALSE,
          priorPosteriorCi = FALSE,
          priorPosteriorFixedAndRandom = FALSE,
          priorStandardError = "inverseGamma",
          samples = 2000,
          seed = 1,
          setSeed = FALSE,
          studyLabel = list(types = list(), value = ""),
          tDf = 1,
          tLocation = 0,
          tScale = 0.707,
          truncationLowerBound = FALSE,
          truncationLowerBoundValue = 0,
          truncationUpperBound = FALSE,
          truncationUpperBoundValue = 0) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("effectSize", "effectSizeCi", "effectSizeSe", "forestPlotOrder", "studyLabel")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "BayesianMetaAnalysisDeprecated", "BayesianMetaAnalysisDeprecated.qml", options, version, TRUE))
}