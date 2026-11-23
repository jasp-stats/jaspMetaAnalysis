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

#' Risk of Bias Plot
#'
#' Create risk-of-bias visualizations from systematic review quality assessments. Supports RoB 2, ROBINS-I, and QUADAS-2.
#'
#' @param colorScheme, Select the color palette for risk-of-bias judgments.
#' @param overallJudgment, Variable containing the overall risk-of-bias judgment for each study.
#' @param robTool, Select the risk-of-bias assessment tool used for the systematic review. Each tool expects a specific number of domain columns with specific judgment values.
#' @param studyLabel, Variable containing study identifiers.
#' @param studyWeights, Variable containing study weights for weighted summary plots (e.g., inverse-variance weights).
#' @param summaryPlot, Display a bar chart showing the distribution of risk-of-bias judgments across domains.
#'    Defaults to \code{TRUE}.
#' @param summaryPlotWeighted, Weight the summary bars by study precision. Requires a weights variable.
#'    Defaults to \code{FALSE}.
#' @param trafficLightPlot, Display a matrix of colored circles showing per-study, per-domain risk-of-bias judgments.
#'    Defaults to \code{TRUE}.
#' @param trafficLightPlotPointSize, Size of the traffic light circles.
RiskOfBiasPlot <- function(
          data = NULL,
          version = "1",
          colorScheme = "cochrane",
          overallJudgment = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          quadas2D1 = list(types = list(), value = ""),
          quadas2D2 = list(types = list(), value = ""),
          quadas2D3 = list(types = list(), value = ""),
          quadas2D4 = list(types = list(), value = ""),
          rob2D1 = list(types = list(), value = ""),
          rob2D2 = list(types = list(), value = ""),
          rob2D3 = list(types = list(), value = ""),
          rob2D4 = list(types = list(), value = ""),
          rob2D5 = list(types = list(), value = ""),
          robTool = "rob2",
          robinsID1 = list(types = list(), value = ""),
          robinsID2 = list(types = list(), value = ""),
          robinsID3 = list(types = list(), value = ""),
          robinsID4 = list(types = list(), value = ""),
          robinsID5 = list(types = list(), value = ""),
          robinsID6 = list(types = list(), value = ""),
          robinsID7 = list(types = list(), value = ""),
          studyLabel = list(types = list(), value = ""),
          studyWeights = list(types = list(), value = ""),
          summaryPlot = TRUE,
          summaryPlotWeighted = FALSE,
          trafficLightPlot = TRUE,
          trafficLightPlotPointSize = 10) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("colorScheme", "overallJudgment", "quadas2D1", "quadas2D2", "quadas2D3", "quadas2D4", "rob2D1", "rob2D2", "rob2D3", "rob2D4", "rob2D5", "robTool", "robinsID1", "robinsID2", "robinsID3", "robinsID4", "robinsID5", "robinsID6", "robinsID7", "studyLabel", "studyWeights")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "RiskOfBiasPlot", "RiskOfBiasPlot.qml", options, version, TRUE))
}