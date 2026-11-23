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

#' SEM-Based Meta-Analysis
#'
#' @param modelFitMeasures, Show a summary of the model fit statistics.
#'    Defaults to \code{FALSE}.
#' @param modelSummary, Show a summary of the model coefficients and computed estimates.
#'    Defaults to \code{TRUE}.
#' @param modelSummaryConfidenceIntervalType, Method for computing confidence interval.
#' @param pairwiseModelComparison, Show a pairwise model comparison table.
#'    Defaults to \code{FALSE}.
#' @param pathDiagram, Show a path diagram of the model.
#'    Defaults to \code{FALSE}.
#' @param pathDiagramEdgeLabelSize, Size of the edge labels.
#' @param pathDiagramLabelSize, Size of the labels.
#' @param pathDiagramLatentNodeWidth, Width of latent nodes.
#' @param pathDiagramLayout, Layout of the path diagram.
#' @param pathDiagramManifestNodeWidth, Width of manifest nodes.
#' @param pathDiagramNumberOfDigits, Number of digits for rounding estimates.
#' @param pathDiagramShowParameterNames, Show parameter names instead of the estimates in the path diagram.
#'    Defaults to \code{FALSE}.
#' @param pathDiagramUnitVectorNodeWidth, Width of unit vector nodes.
SemBasedMetaAnalysis <- function(
          data = NULL,
          version = "1",
          modelFitMeasures = FALSE,
          modelSummary = TRUE,
          modelSummaryConfidenceIntervalType = "standardErrors",
          models = list(list(syntax = list(columns = list(), model = "", modelOriginal = ""), value = "Model 1")),
          pairwiseModelComparison = FALSE,
          pathDiagram = FALSE,
          pathDiagramEdgeLabelSize = 0.9,
          pathDiagramLabelSize = 1.3,
          pathDiagramLatentNodeWidth = 8,
          pathDiagramLayout = "tree",
          pathDiagramManifestNodeWidth = 6,
          pathDiagramNumberOfDigits = 4,
          pathDiagramShowParameterNames = FALSE,
          pathDiagramUnitVectorNodeWidth = 8,
          plotHeight = 320,
          plotWidth = 480) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("modelSummaryConfidenceIntervalType", "models", "pathDiagramLayout")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "SemBasedMetaAnalysis", "SemBasedMetaAnalysis.qml", options, version, FALSE))
}