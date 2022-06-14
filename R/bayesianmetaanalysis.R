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
  ready <- options[["effectSize"]] != "" && (options[["standardError"]] != "" || (all(unlist(options$confidenceInterval) != "")  && !is.null(unlist(options[["confidenceInterval"]]))))

  # Dependencies: basically everything
  # dependencies <- .bmaDependencies

  # Dataset with effectSize, standardError, and studyLabels
  # If data is null stuff is missing
  dataset <- .bmaReadData(jaspResults, options)

  .BayesianMetaAnalysisCommon(jaspResults, dataset, ready, options)

}

.bmaDependencies <- c("effectSize", "standardError", "confidenceInterval", "modelSpecification",
                      "allPos", "allNeg", "priorH0FE", "priorH1FE", "priorH0RE", "priorH1RE",
                      "priorES", "informativeCauchyLocation", "informativeCauchyScale",
                      "checkLowerPrior", "checkUpperPrior", "lowerTrunc", "upperTrunc",
                      "informativeNormalMean", "informativeNormalStd",
                      "informativeTLocation", "informativeTScale", "informativeTDf",
                      "priorSE", "inverseGammaShape", "inverseGammaScale",
                      "informativehalfTScale", "informativehalfTDf",
                      "BFComputation", "iterBridge", "iterMCMC", "chainsMCMC")

# Get dataset
.bmaReadData <- function(jaspResults, options){
  varES <- options[["effectSize"]]
  varSE <- options[["standardError"]]
  CI <- unlist(options$confidenceInterval)
  lower <- CI[[1]]
  upper <- CI[[2]]
  study <- options[["studyLabels"]]
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
