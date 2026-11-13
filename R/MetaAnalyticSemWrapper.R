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

#' Meta-Analytic SEM
#'
#' @param availableVariableNames, Show a summary of the available variable names in the correlation/covariance matrix input.
#'    Defaults to \code{FALSE}.
#' @param correlationCovarianceMatrix, Variables containing the correlations/covariances between the variables. The variable name must be in a form `x_y` where `x` and `y` corresponds to the variables between which the correlation/covariance is reported. The separator used in the variable names (defaults to `_`) can be changed via the `Variable name separator` option.
#' @param means, Variables containing the means of the variables. The variable name must be `x` where `x` corresponds to the variable names in the correlation/covariance matrix input. This input is required only when meta-analytic sem with means is requested.
#' @param modelConvergence, Show a summary of the model convergence.
#'    Defaults to \code{FALSE}.
#' @param modelFitMeasures, Show a summary of the model fit statistics.
#'    Defaults to \code{FALSE}.
#' @param modelSummary, Show a summary of the model coefficients and computed estimates.
#'    Defaults to \code{TRUE}.
#' @param modelSummaryCovariances, Show covariance estimates (S-Matrix) in the model summary.
#'    Defaults to \code{FALSE}.
#' @param modelSummaryMeansIntercepts, Show mean and intercept estimates (M-Matrix) in the model summary.
#'    Defaults to \code{TRUE}.
#' @param modelSummaryRandomEffects, Show random effects in the model summary.
#'    Defaults to \code{FALSE}.
#' @param modelSummaryRegression, Show regression estimates (A-Matrix) in the model summary.
#'    Defaults to \code{TRUE}.
#' @param modelSummaryShowMatrixIndices, Show matrix indices (rows/columns) in the model summary.
#'    Defaults to \code{FALSE}.
#' @param numberOfEstimates, Show a frequency table of the estimates in the correlation/covariance matrix input.
#'    Defaults to \code{FALSE}.
#' @param numberOfObservations, Show a frequency table of the observations in the correlation/covariance matrix input.
#'    Defaults to \code{FALSE}.
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
#' @param pooledCorrelationCovarianceMatrix, Show the pooled correlation/covariance matrix. The pooled correlation/covariance matrix is from the first stage of two-stage meta-analytic SEM. 
#'    Defaults to \code{FALSE}.
#' @param pooledCorrelationCovarianceMatrixRandomEffects, Type of the random effects of the correlation or covariance vectors.
#' @param sampleSize, Variable containing sample sizes for the studies
#' @param semFitMeasures, Show a summary of the SEM goodness-of-fit statistics.
#'    Defaults to \code{FALSE}.
#' @param variableNameSeparator, Separator used in the variable names in the correlation/covariance matrix input. The variable name must be in a form `x_y` where `x` and `y` corresponds to the variables between which the correlation/covariance is reported.
MetaAnalyticSem <- function(
          data = NULL,
          version = "1",
          availableVariableNames = FALSE,
          correlationCovarianceMatrix = list(types = list(), value = list()),
          dataInputType = "correlation",
          means = list(types = list(), value = list()),
          modelConvergence = FALSE,
          modelFitMeasures = FALSE,
          modelSummary = TRUE,
          modelSummaryCovariances = FALSE,
          modelSummaryMeansIntercepts = TRUE,
          modelSummaryRandomEffects = FALSE,
          modelSummaryRegression = TRUE,
          modelSummaryShowMatrixIndices = FALSE,
          models = list(list(fixLatentVarianceTo1 = TRUE, randomEffectsMu = "symmetric", randomEffectsSigma = "diagonal", randomEffectsSigmaMu = "zero", syntax = list(columns = list(), model = "", modelOriginal = ""), value = "Model 1")),
          numberOfEstimates = FALSE,
          numberOfObservations = FALSE,
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
          plotWidth = 480,
          pooledCorrelationCovarianceMatrix = FALSE,
          pooledCorrelationCovarianceMatrixRandomEffects = "diagonal",
          sampleSize = list(types = list(), value = ""),
          semFitMeasures = FALSE,
          variableNameSeparator = "_") {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("correlationCovarianceMatrix", "means", "models", "pathDiagramLayout", "pooledCorrelationCovarianceMatrixRandomEffects", "sampleSize", "variableNameSeparator")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "MetaAnalyticSem", "MetaAnalyticSem.qml", options, version, FALSE))
}