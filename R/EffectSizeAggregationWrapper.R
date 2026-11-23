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

#' Effect Size Aggregation
#'
#' Effect Size Aggregation allows you to combine multiple effect sizes or outcomes from the same study into a single aggregate effect size.This is useful when a study reports multiple outcomes that should be treated as a single unit. Note that multilevel and multivariate models available in the Classical Meta-Analysis (Multilevel/Multivariate) option are preferable for analyzing such structures.
#'
#' @param addClusterSize, Add a column with the number of effect sizes per cluster.
#'    Defaults to \code{FALSE}.
#' @param cluster, Variable identifying the cluster (e.g., study) to aggregate effect sizes within.
#' @param effectSize, Variable containing the observed effect sizes.
#' @param effectSizeStandardError, Variable containing the standard errors of the effect sizes.
#' @param showMetaforRCode, Display the underlying R code used by the metafor package to aggregate the effect sizes.
#'    Defaults to \code{FALSE}.
#' @param varianceCovarianceMatrixCheckPositiveDefiniteness, Check if the variance-covariance matrix is symmetric.
#'    Defaults to \code{TRUE}.
#' @param varianceCovarianceMatrixCluster, Variable specifying clustering of the effect sizes for computing the variance covariance matrix. Effect sizes with different values of the cluster variable are assumed to be independent. Note that this input differs from the 'Clustering' option in the data input which is used to specify cluster-robust standard error. In most cases however, both input should contain the same variable.
#' @param varianceCovarianceMatrixConstruct, Variable specifying the construct measured by the effect size. Corresponds to the `obs` option in the metafor's 'vcalc' function.
#' @param varianceCovarianceMatrixConstructType, Variable specifying the type of construct measured by the effect size. Construct Type corresponds to a higher level grouping of the Constructs. Corresponds to the `type` option in the metafor's 'vcalc' function.
#' @param varianceCovarianceMatrixCorrelationMatrix, Variable specifying the correlation between the individual estimates within cluster. The column order of the variable correspond to the row order of effect size estimates within cluster. Only the lower triangle needs to be specified. Corresponds to the `rvars` option in the metafor's 'vcalc' function.
#' @param varianceCovarianceMatrixFile, CSV file containing the precomputed effect size variance-covariance matrix. The matrix needs to match the dimensions of the data set and cannot contain any other variables or names.
#' @param varianceCovarianceMatrixForcePositiveDefiniteness, Force the variance-covariance matrix to be positive definite. This option shuld be used with caution as non-positive definite matricies often indicate input misspecification.
#'    Defaults to \code{FALSE}.
#' @param varianceCovarianceMatrixGroup1, Variable to specify the group of the first condition when the observed effect sizes or outcomes represent contrasts between two conditions. Corresponds to the `grp1` argument in the metafor's 'vcalc' function.
#' @param varianceCovarianceMatrixGroup2, Variable to specify the group of the second condition when the observed effect sizes or outcomes represent contrasts between two conditions. Corresponds to the `grp1` argument in the metafor's 'vcalc' function.
#' @param varianceCovarianceMatrixGroupSize1, Variable to specify the size of the group (or more generally, the inverse-sampling variance weight) of the first condition when the observed effect sizes or outcomes represent contrasts between two conditions. Corresponds to the `w1` argument in the metafor's 'vcalc' function.
#' @param varianceCovarianceMatrixGroupSize2, Optional numeric vector to specify the size of the group (or more generally, the inverse-sampling variance weight) of the second condition when the observed effect sizes or outcomes represent contrasts between two conditions. Corresponds to the `w2` argument in the metafor's 'vcalc' function.
#' @param varianceCovarianceMatrixSimpleAutoregressiveLag1Correlation, Correlation for lag-1 time points in autoregressive structures.
#' @param varianceCovarianceMatrixSimpleAutoregressiveTime, Variable specifying the time point of each effect size, required for autoregressive (CAR) structures.
#' @param varianceCovarianceMatrixSimpleCompoundSymmetryConstruct, Variable identifying the construct, outcome, or endpoint measured by each effect size. Required for compound symmetry times autoregressive structures.
#' @param varianceCovarianceMatrixSimpleCompoundSymmetryWithinClusterCorrelation, Assumed correlation between sampling errors within clusters (compound symmetry).
#' @param varianceCovarianceMatrixSimpleStructure, Parametric correlation structure for sampling errors within clusters. 'Compound symmetry + autoregressive' assumes a constant baseline correlation plus additional correlation that decays over time. 'Compound symmetry × autoregressive' assumes the constant correlation and the time-based decay act jointly, so correlation vanishes at large time lags.
#' @param varianceCovarianceMatrixSubcluster, Variable specifying additional structure of the subgroups. Effect sizes within the same cluster with different values of the cluster variable are assumed to be independent. Note that this input corresponds to the 'subgroup' option in the metafor's 'vcalc' function and is renamed for differentiation from subgroup analysis.
#' @param varianceCovarianceMatrixTime1, Variable specifying the time point of the effect size measurement. In case multiple time points are specified, the first time corresponds to the first condition.
#' @param varianceCovarianceMatrixTime2, Variable specifying the time point of the effect size measurement. In case multiple time points are specified, the second time corresponds to the second condition.
#' @param varianceCovarianceMatrixType, Type of variance-covariance matrix input method.
#' \itemize{
#'   \item \code{"simple"}: Specify a simple parametric correlation structure for sampling errors within clusters.
#'   \item \code{"correlationMatrix"}: Use a list of variables to specify the correlation matrix of studies corresponding to the same cluster. Corresponds to the `rvars` option in the metafor's 'vcalc' function.
#'   \item \code{"constructsGroupsTimes"}: Specify constructs, groups, and times of the measurement to specify the correlation matrix of studies corresponding to the same cluster.
#'   \item \code{"precomputed"}: Load a csv file containing the precomputed variance-covariance matrix.
#' }
#' @param weighted, Use inverse-variance weighting when aggregating effect sizes.
#'    Defaults to \code{TRUE}.
EffectSizeAggregation <- function(
          data = NULL,
          version = "1",
          addClusterSize = FALSE,
          aggregatedColumnNamesClusterSize = "cluster size",
          aggregatedColumnNamesEffectSize = "aggregated effect size",
          aggregatedColumnNamesSamplingVariance = "aggregated sampling variance",
          aggregatedColumnNamesStandardError = "aggregated standard error",
          cluster = list(types = list(), value = ""),
          computeSamplingVariance = FALSE,
          effectSize = list(types = list(), value = ""),
          effectSizeStandardError = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          showMetaforRCode = FALSE,
          varianceCovarianceMatrixCheckPositiveDefiniteness = TRUE,
          varianceCovarianceMatrixCluster = list(types = list(), value = ""),
          varianceCovarianceMatrixConstruct = list(types = list(), value = ""),
          varianceCovarianceMatrixConstructCorrelationMatrix = "commonCorrelation",
          varianceCovarianceMatrixConstructCorrelationMatrixFilePath = "",
          varianceCovarianceMatrixConstructCorrelationMatrixValue = 0,
          varianceCovarianceMatrixConstructType = list(types = list(), value = ""),
          varianceCovarianceMatrixConstructTypeCorrelationMatrix = "commonCorrelation",
          varianceCovarianceMatrixConstructTypeCorrelationMatrixFilePath = "",
          varianceCovarianceMatrixConstructTypeCorrelationMatrixValue = 0,
          varianceCovarianceMatrixCorrelationMatrix = list(types = list(), value = list()),
          varianceCovarianceMatrixFile = "",
          varianceCovarianceMatrixForcePositiveDefiniteness = FALSE,
          varianceCovarianceMatrixGroup1 = list(types = list(), value = ""),
          varianceCovarianceMatrixGroup2 = list(types = list(), value = ""),
          varianceCovarianceMatrixGroupSize1 = list(types = list(), value = ""),
          varianceCovarianceMatrixGroupSize2 = list(types = list(), value = ""),
          varianceCovarianceMatrixSaveComputedVarianceCovarianceMatrix = "",
          varianceCovarianceMatrixSimpleAutoregressiveLag1Correlation = 0,
          varianceCovarianceMatrixSimpleAutoregressiveTime = list(types = "unknown", value = ""),
          varianceCovarianceMatrixSimpleCompoundSymmetryConstruct = list(types = "unknown", value = ""),
          varianceCovarianceMatrixSimpleCompoundSymmetryWithinClusterCorrelation = 0,
          varianceCovarianceMatrixSimpleStructure = "ID",
          varianceCovarianceMatrixSubcluster = list(types = list(), value = ""),
          varianceCovarianceMatrixTime1 = list(types = list(), value = ""),
          varianceCovarianceMatrixTime2 = list(types = list(), value = ""),
          varianceCovarianceMatrixTimeLag1Correlation = 0,
          varianceCovarianceMatrixType = "simple",
          weighted = TRUE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("cluster", "effectSize", "effectSizeStandardError", "varianceCovarianceMatrixCluster", "varianceCovarianceMatrixConstruct", "varianceCovarianceMatrixConstructType", "varianceCovarianceMatrixCorrelationMatrix", "varianceCovarianceMatrixGroup1", "varianceCovarianceMatrixGroup2", "varianceCovarianceMatrixGroupSize1", "varianceCovarianceMatrixGroupSize2", "varianceCovarianceMatrixSimpleAutoregressiveTime", "varianceCovarianceMatrixSimpleCompoundSymmetryConstruct", "varianceCovarianceMatrixSimpleStructure", "varianceCovarianceMatrixSubcluster", "varianceCovarianceMatrixTime1", "varianceCovarianceMatrixTime2")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspMetaAnalysis", "EffectSizeAggregation", "EffectSizeAggregation.qml", options, version, TRUE))
}