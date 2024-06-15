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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

ClassicalMetaAnalysis <- function(jaspResults, dataset = NULL, options, ...) {

  options[["module"]] <- "metaAnalysis"

  if (.maReady(options)) {
    dataset <- .maReadData(dataset, options)
    .maCheckErrors(dataset, options)
  }

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  .ClassicalMetaAnalysisCommon(jaspResults, dataset, options)

  return()
  options <- readRDS(file = "C:/JASP/options.RDS")
  dataset <- readRDS(file = "C:/JASP/dataset.RDS")
  return()
}

.maDependencies        <- c(
  "effectSize", "effectSizeStandardError", "effectSizeModelTerms", "effectSizeModelIncludeIntercept",
  "heterogeneityModelTerms", "heterogeneityModelIncludeIntercept", "predictors", "predictors.types",
  "clustering", "studyLabel",
  "method", "fixedEffectTest", "confidenceIntervalsLevel",
  "clusteringUseClubSandwich", "clusteringSmallSampleCorrection"
)

.maReady               <- function(options) {

  inputReady <- options[["effectSize"]] != "" && options[["effectSizeStandardError"]] != ""
  termsEffectSizeReady    <- length(options[["effectSizeModelTerms"]]) > 0    || options[["effectSizeModelIncludeIntercept"]]
  termsHeterogeneityReady <- length(options[["heterogeneityModelTerms"]]) > 0 || options[["heterogeneityModelIncludeIntercept"]]

  return(inputReady && termsEffectSizeReady && termsHeterogeneityReady)
}
.maReadData            <- function(dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  predictorsNominal <- options[["predictors"]][options[["predictors.types"]] == "nominal"]
  predictorsScale   <- options[["predictors"]][options[["predictors.types"]] == "scale"]

  # load data
  dataset <- .readDataSetToEnd(
    columns.as.factor = c(
      if (length(predictorsNominal) > 0) predictorsNominal,
      if (options[["clustering"]] != "") options[["clustering"]],
      if (options[["studyLabel"]] != "") options[["studyLabel"]]
    ),
    columns.as.numeric  = c(
      options[["effectSize"]],
      options[["effectSizeStandardError"]],
      if (length(predictorsScale) > 0) predictorsScale
    ))

  # omit NAs
  dataset  <- na.omit(dataset)

  return(dataset)
}
.maCheckErrors         <- function(dataset, options) {

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations", "variance"),
    all.target           = c(
      options[["effectSize"]],
      options[["effectSizeStandardError"]],
      options[["predictors"]][options[["predictors.types"]] == "scale"]
    ),
    observations.amount  = "< 2",
    exitAnalysisIfErrors = TRUE)

  .hasErrors(
    dataset              = dataset,
    type                 = c("modelInteractions"),
    modelInteractions.modelTerms = c(options[["effectSizeModelTerms"]], options[["heterogeneityModelTerms"]]),
    exitAnalysisIfErrors = TRUE)

  .hasErrors(
    dataset              = dataset,
    seCheck.target       = options[["effectSizeStandardError"]],
    custom               = .maCheckStandardErrors,
    exitAnalysisIfErrors = TRUE)
}
.maCheckStandardErrors <- list(seCheck = function(dataset, target) {
    nonPositive <- !all(dataset[,target] > 0)
    if (nonPositive) {
      return(gettext("All standard errors must be positive."))
    }
  })
