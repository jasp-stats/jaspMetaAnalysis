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

# Multilevel/multivariate meta-analysis entry point and input checks.
#
# Random structures, variance-covariance handling, and random-effect tables are
# implemented in the classicalmetaanalysismultilevelmultivariate-*.R files.

#' @export
ClassicalMetaAnalysisMultilevelMultivariate <- function(jaspResults, dataset = NULL, options, ...) {

  options[["analysis"]] <- "metaAnalysisMultilevelMultivariate"

  if (.maReady(options)) {
    dataset <- .mammCheckData(dataset, options)
    .mammCheckErrors(dataset, options)
  }

  ClassicalMetaAnalysisCommon(jaspResults, dataset, options)

  return()
}

.mammCheckData                   <- function(dataset, options) {

  # model data
  predictorsNominal <- options[["predictors"]][options[["predictors.types"]] == "nominal"]
  predictorsScale   <- options[["predictors"]][options[["predictors.types"]] == "scale"]

  # random effects variables
  randomVariables <- .mammExtractRandomVariableNames(options)

  # variance-covariance variables
  varianceCovarianceVariables <- .mammExtractVarianceCovarianceMatrixNames(options)

  # omit NAs
  omitOnVariables <- c(
    options[["effectSize"]],
    options[["effectSizeStandardError"]],
    unlist(randomVariables),
    varianceCovarianceVariables,
    if (options[["clustering"]] != "") options[["clustering"]],
    if (options[["subgroup"]] != "")   options[["subgroup"]],
    if (length(predictorsNominal) > 0) predictorsNominal,
    if (length(predictorsScale) > 0)   predictorsScale
  )
  anyNaByRows <- apply(dataset[,omitOnVariables], 1, function(x) anyNA(x))
  dataset     <- dataset[!anyNaByRows,]
  attr(dataset, "NAs")    <- sum(anyNaByRows)
  attr(dataset, "NasIds") <- anyNaByRows

  # add se^2 for V^2 input
  dataset$samplingVariance <- dataset[[options[["effectSizeStandardError"]]]]^2

  return(dataset)
}
.mammCheckErrors                 <- function(dataset, options) {

  randomVariables <- .mammExtractRandomVariableNames(options)

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations"),
    all.target           = c(
      options[["effectSize"]],
      options[["effectSizeStandardError"]]
    ),
    observations.amount  = "< 2",
    exitAnalysisIfErrors = TRUE)

  # do not check effect sizes / standard errors for 0 variance
  otherVariable <- c(
    options[["predictors"]],
    c(randomVariables$scale, randomVariables$ordinal)
  )
  if (length(otherVariable) > 0) {
    .hasErrors(
      dataset              = dataset,
      type                 = c("infinity", "observations", "variance", "factorLevels"),
      all.target           = otherVariable,
      observations.amount  = "< 2",
      factorLevels.amount  = "< 2",
      exitAnalysisIfErrors = TRUE)
  }

  .hasErrors(
    dataset              = dataset,
    seCheck.target       = options[["effectSizeStandardError"]],
    custom               = .maCheckStandardErrors,
    exitAnalysisIfErrors = TRUE)
}
