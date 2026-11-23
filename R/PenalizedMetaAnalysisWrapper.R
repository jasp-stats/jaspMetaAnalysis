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

#' Penalized Meta-Analysis
#'
PenalizedMetaAnalysis <- function(
          data = NULL,
          version = "1",
          clustering = list(types = list(), value = ""),
          covariates = list(types = list(), value = list()),
          diagnosticsType = "trace",
          effectSize = list(types = list(), value = ""),
          effectSizeSe = list(types = list(), value = ""),
          factors = list(types = list(), value = list()),
          horseshoePriorDf = 1,
          horseshoePriorScale = 1,
          inferenceEstimatesTable = TRUE,
          inferenceHeterogeneityI2 = FALSE,
          inferenceHeterogeneityTable = FALSE,
          lassoPriorDf = 1,
          lassoPriorDfGlobal = 1,
          lassoPriorDfSlab = 4,
          lassoPriorScaleGlobal = 1,
          lassoPriorScaleSlab = 1,
          mcmcAdaptDelta = 0.8,
          mcmcBurnin = 2000,
          mcmcChains = 4,
          mcmcMaxTreedepth = 15,
          mcmcSamples = 2000,
          method = "horseshoe",
          modelIncludeIntercept = TRUE,
          modelScalePredictors = TRUE,
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          plotHeight = 320,
          plotWidth = 480,
          posteriorPlotsSelectedTerms = list(optionKey = "variable", types = list(), value = list()),
          scatterVariableX = list(optionKey = "variable", types = list(), value = list()),
          scatterVariableY = list(optionKey = "variable", types = list(), value = list()),
          seed = 1,
          setSeed = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("clustering", "covariates", "diagnosticsType", "effectSize", "effectSizeSe", "factors", "method", "modelTerms", "posteriorPlotsSelectedTerms", "scatterVariableX", "scatterVariableY")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "PenalizedMetaAnalysis", "PenalizedMetaAnalysis.qml", options, version, TRUE))
}