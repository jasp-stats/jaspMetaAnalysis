#
# Copyright (C) 2018 University of Amsterdam
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

# Main function ----

BayesianMetaAnalysis <- function(jaspResults, dataset, options) {

  options[["module"]] <- "metaAnalysis"

  # Ready: variables needed for the analysis (confidence interval missing)
  ready <- options[["effectSize"]] != "" && (options[["effectSizeSe"]] != "" || (all(unlist(options$effectSizeCi) != "") && !is.null(unlist(options[["effectSizeCi"]]))))

  # Dependencies: basically everything
  # dependencies <- .bmaDependencies

  # Dataset with effectSize, effectSizeSe, and studyLabel
  # If data is null stuff is missing
  dataset <- .bmaReadData(jaspResults, options)

  .BayesianMetaAnalysisCommon(jaspResults, dataset, ready, options)
}

.bmaDependencies <- c(
  "effectSize", "effectSizeSe", "effectSizeCi", "model",
  "positive", "negative",
  "priorModelProbabilityFixedNull", "priorModelProbabilityFixedAlternative",
  "priorModelProbabilityRandomNull", "priorModelProbabilityRandomAlternative",
  "priorEffectSize", "cauchyLocation", "cauchyScale",
  "truncationLowerBound", "truncationUpperBound",
  "truncationLowerBoundValue", "truncationUpperBoundValue",
  "normalMean", "normalSd",
  "tLocation", "tScale", "tDf",
  "priorStandardError", "inverseGammaShape", "inverseGammaScale",
  "halfTScale", "halfTDf",
  "bayesFactorComputation", "bridgeSamplingSamples", "samples", "chains"
)

# Get dataset
.bmaReadData <- function(jaspResults, options){
  varES <- options[["effectSize"]]
  varSE <- options[["effectSizeSe"]]
  CI <- unlist(options$effectSizeCi)
  lower <- CI[[1]]
  upper <- CI[[2]]
  study <- options[["studyLabel"]]
  if(varES == "") varES <- NULL
  if(varSE == "") varSE <- NULL
  if(CI[[1]] == ""  || CI[[2]] == "" || is.null(CI)) {
    lower <- NULL
    upper <- NULL
  }
  if(study == "") study <- NULL
  variables.to.read <- c(varES, varSE, lower, upper, study)
  dataset <- .readDataSetToEnd(columns.as.numeric = variables.to.read,
                               exclude.na.listwise = variables.to.read)
  return(dataset)
}