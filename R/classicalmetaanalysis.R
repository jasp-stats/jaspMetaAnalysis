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

  ready <- options$dependent != "" && options$wlsWeights != "" && (options$includeConstant || length(options$modelTerms) > 0)
  if(ready) {
    dataset <- .metaAnalysisReadData(dataset, options)
    .metaAnalysisCheckErrors(dataset, options)
  }

  container <- .metaAnalysisGetOutputContainer(jaspResults)

  .ClassicalMetaAnalysisCommon(container, dataset, ready, options)

  return()
}

.metaAnalysisGetOutputContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("dependent", "wlsWeights", "method", "studyLabels", "covariates", "test",
                              "factors", "modelTerms", "includeConstant", "regressionCoefficientsConfidenceIntervalsInterval"))
    jaspResults[["modelContainer"]] <- modelContainer
  }
  return(modelContainer)
}

.metaAnalysisReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else {
    effsizeName <- unlist(options$dependent)
    stderrName  <- unlist(options$wlsWeights)
    covarNames  <- if (length(options$covariates) > 0) unlist(options$covariates)
    factNames   <- if (length(options$factors) > 0) unlist(options$factors)

    numeric.variables <- Filter(function(s) s != "", c(effsizeName, covarNames, stderrName))
    factor.variables  <- Filter(function(s) s != "", c(factNames, options$studyLabels))
    return(.readDataSetToEnd(columns.as.factor   = factor.variables,
                             columns.as.numeric  = numeric.variables,
                             exclude.na.listwise = numeric.variables))
  }
}

.metaAnalysisCheckErrors <- function(dataset, options){
  effsizeName <- unlist(options$dependent)
  stderrName  <- unlist(options$wlsWeights)
  covarNames  <- if (length(options$covariates) > 0) unlist(options$covariates)
  numeric.variables <- Filter(function(s) s != "", c(effsizeName, covarNames, stderrName))
  .hasErrors(dataset              = dataset,
             type                 = c("infinity", "observations", "variance"),
             all.target           = numeric.variables,
             observations.amount  = "< 2",
             exitAnalysisIfErrors = TRUE)
  .hasErrors(dataset              = dataset,
             type                 = c("modelInteractions"),
             modelInteractions.modelTerms = options$modelTerms,
             exitAnalysisIfErrors = TRUE)
  .hasErrors(dataset              = dataset,
             seCheck.target       = options[["wlsWeights"]],
             custom               = .metaAnalysisCheckSE,
             exitAnalysisIfErrors = TRUE)
}

.metaAnalysisCheckSE <- list(
  seCheck = function(dataset, target) {
    nonPositive <- !all(na.omit(dataset[,target]) > 0)
    if (nonPositive) {
      return(gettext("All standard errors/sample sizes must be positive."))
    }
  }
)
