#
# Copyright (C) 2019 University of Amsterdam
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

#"successesGroup1", "successesGroup2", "sampleSizeGroup1", "sampleSizeGroup2"

BayesianBinomialMetaAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  options[["analysis"]] <- "BiBMA"
  options[["effectSizeMeasure"]]         <- "logOR"
  options[["publicationBiasAdjustment"]] <- "none"

  if (.maReady(options)) {
    dataset <- .bibmaCheckData(dataset, options)
    .bibmaCheckErrors(dataset, options)
  }

  RobustBayesianMetaAnalysisCommon(jaspResults, dataset, options)

  return()
}


.bibmaCheckData        <- function(dataset, options) {

  # model data
  predictorsNominal <- options[["predictors"]][options[["predictors.types"]] == "nominal"]
  predictorsScale   <- options[["predictors"]][options[["predictors.types"]] == "scale"]

  # omit NAs
  omitOnVariables <- c(
    options[["successesGroup1"]],
    options[["successesGroup2"]],
    options[["sampleSizeGroup1"]],
    options[["sampleSizeGroup2"]],
    if (length(options[["studyLevelMultilevel"]]) > 0 && options[["studyLevelMultilevel"]] != "") options[["studyLevelMultilevel"]],
    if (length(options[["subgroup"]])   > 0 && options[["subgroup"]]   != "") options[["subgroup"]],
    if (length(predictorsNominal)       > 0) predictorsNominal,
    if (length(predictorsScale)         > 0) predictorsScale
  )
  anyNaByRows <- apply(dataset[,omitOnVariables], 1, function(x) anyNA(x))
  dataset     <- dataset[!anyNaByRows,]
  attr(dataset, "NAs")    <- sum(anyNaByRows)
  attr(dataset, "NasIds") <- anyNaByRows

  # drop empty factor levels
  dataset <- droplevels(dataset)

  return(dataset)
}
.bibmaCheckErrors      <- function(dataset, options) {

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations"),
    all.target           = c(
      options[["successesGroup1"]],
      options[["successesGroup2"]],
      options[["sampleSizeGroup1"]],
      options[["sampleSizeGroup2"]]
    ),
    observations.amount  = "< 2",
    exitAnalysisIfErrors = TRUE)

  # do not check effect sizes / standard errors for 0 variance
  otherVariable <- options[["predictors"]][options[["predictors.types"]] == "scale"]
  if (length(otherVariable) > 0) {
    .hasErrors(
      dataset              = dataset,
      type                 = c("infinity", "observations", "variance"),
      all.target           = otherVariable,
      observations.amount  = "< 2",
      exitAnalysisIfErrors = TRUE)
  }

  if (length(options[["effectSizeModelTerms"]]) > 0)
    .hasErrors(
      dataset              = dataset,
      type                 = c("modelInteractions"),
      modelInteractions.modelTerms = options[["effectSizeModelTerms"]],
      exitAnalysisIfErrors = TRUE)

  .hasErrors(
    dataset              = dataset,
    seCheck.target       = c(options[["sampleSizeGroup1"]], options[["sampleSizeGroup2"]]),
    custom               = .bibmaCheckObsevations,
    exitAnalysisIfErrors = TRUE)
}
.bibmaCheckObsevations <- list(seCheck = function(dataset, target) {
  nonPositive <- !all(dataset[,target] > 0)
  if (nonPositive) {
    return(gettext("There must be at least one observation in each group."))
  }
})
