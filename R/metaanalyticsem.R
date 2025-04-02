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

MetaAnalyticSem <- function(jaspResults, dataset, options, state = NULL) {

  # set OpenMx options
  # the JASP options() cleanup does not properly re-sets OpenMx settings
  # consequently, the fitting function crashes with matrix(byrow) error
  OpenMx::mxSetDefaultOptions()

  # read the data set
  dataset <- .masemDecodeData(dataset, options)

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  # create a data-summary of the input
  if (options[["inputSummaryAvailableVariableNames"]])
    .masemVariableSummaryView(jaspResults, dataset, options)
  if (options[["inputSummaryFrequencyTable"]])
    .masemDataSummaryTables(jaspResults, dataset, options)

  return()
  # estimate the models
  .masemFitModels(jaspResults, dataset, options)

  # create summary with model fit statistics (for all models)
  .masemModelFitTable(jaspResults, options)

  # create model-level summaries
  if (options[["modelSummary"]])
    .masemModelSummaryTable(jaspResults, options)

  # create path diagrams
  if (options[["pathDiagram"]])
    .masemModelPathDiagram(jaspResults, options)

  return()
}

.masemDependencies <- c(
  "correlationCovarianceMatrix", "means", "dataInputType", "variableNameSeparator",
  "models", "modelSummaryConfidenceIntervalType"
)
.masemDecodeData           <- function(dataset, options) {

  # masem requires a wide format of the correlation/covariance matrices
  # (because of backwards compatibility with previous tutorial papers etc)
  # therefore, the variable names need to be decoded manually for reconstruction
  # of the matrices

  inputNames <- options[["correlationCovarianceMatrix"]]

  if (length(inputNames) == 0) {
    return()
  }

  # subset the data
  inputDataset <- dataset[,inputNames]

  # decode the correlation matrix names
  decodedInputNames      <- decodeColNames(inputNames)
  colnames(inputDataset) <- decodedInputNames

  # split the variable names into the original variable names
  splitInputNames  <- strsplit(decodedInputNames, options[["variableNameSeparator"]])
  uniqueInputNames <- unique(unlist(splitInputNames))

  # check split variable names
  for (i in seq_along(splitInputNames)) {
    if (length(splitInputNames[[i]]) != 2)
      .quitAnalysis(gettextf("The 'Correlation/Covariance Matrix' variable input contains the following variable that does not contain the variable name separator '%1$s': %2$s.",
                             options[["variableNameSeparator"]], decodedInputNames[i]))
  }

  # create a template matrix with the correct dimensions
  matrixTemplate           <- matrix(NA, nrow = length(uniqueInputNames), ncol = length(uniqueInputNames))
  rownames(matrixTemplate) <- uniqueInputNames
  colnames(matrixTemplate) <- uniqueInputNames

  # construct input matrices for each study
  matricies <- list()
  for (i in 1:nrow(inputDataset)) {

    # use the template matrix
    tempMatrix       <- matrixTemplate

    # fill the off-diagonal matrix elements
    for (j in seq_along(uniqueInputNames)) {
      for (k in seq_along(uniqueInputNames)) {
        if (j > k) {

          # deal with potentially reversed name order
          tempVariableNameJK <- paste0(uniqueInputNames[j], options[["variableNameSeparator"]], uniqueInputNames[k])
          tempVariableNameKJ <- paste0(uniqueInputNames[k], options[["variableNameSeparator"]], uniqueInputNames[j])

          if (tempVariableNameJK %in% decodedInputNames) {
            tempMatrix[j, k] <- inputDataset[i, tempVariableNameJK] -> tempMatrix[k, j]
          } else if (tempVariableNameKJ %in% decodedInputNames) {
            tempMatrix[j, k] <- inputDataset[i, tempVariableNameKJ] -> tempMatrix[k, j]
          }
        }
      }
    }

    # fill the diagonal matrix elements
    if (options[["dataInputType"]] == "correlation") {

      # all diagonals equal 1
      diag(tempMatrix) <- 1

    } else if (options[["dataInputType"]] == "covariance") {

      # diagonals specified in the input
      for (j in seq_along(uniqueInputNames)) {
        tempVariableNameJJ <- paste0(uniqueInputNames[j], options[["variableNameSeparator"]], uniqueInputNames[j])
        if (tempVariableNameJJ %in% decodedInputNames) {
          tempMatrix[j, j] <- inputDataset[i, tempVariableNameJJ]
        }
      }

    }

    # store the matrix
    matricies[[i]] <- tempMatrix
  }

  # store variable names
  attr(matricies, "variableNames") <- uniqueInputNames
  output <- list(
    inputMatricies = matricies,
    sampleSize     = dataset[[options[["sampleSize"]]]]
  )

  # check if means are specified
  meanNames        <- options[["means"]]
  if (length(meanNames) > 0) {

    # select and decode data
    datasetMeans           <- dataset[,meanNames]
    decodedMeanNames       <- decodeColNames(meanNames)
    colnames(datasetMeans) <- decodedMeanNames

    # check if there are some superfluous variables
    if (any(!decodedMeanNames %in% uniqueInputNames))
      .quitAnalysis(gettextf("The 'Means' variable input contains the following variable that are not specified in the 'Correlation/Covariance Matrix' variable input: %1$s.",
                             paste0(setdiff(decodedMeanNames, uniqueInputNames), collapse = ", ")))


    output[["means"]] <- datasetMeans
  }

  return(output)
}
.masemVariableSummaryView  <- function(jaspResults, dataset, options) {

  # create an HTML output listing all available variable names (based on the correlation/covariance matrix input)
  variableSummaryView <- createJaspHtml(title = gettext("Available Variable Names"))
  variableSummaryView$dependOn(c("correlationCovarianceMatrix", "means", "dataInputType", "variableNameSeparator", "inputSummaryAvailableVariableNames"))
  variableSummaryView$position <- 1
  jaspResults[["variableSummaryView"]] <- variableSummaryView

  if (!is.null(dataset[["inputMatricies"]])) {
    variableSummaryView[["text"]] <- paste0(attr(dataset[["inputMatricies"]], "variableNames"), collapse = "<br>")
  }

  return()
}
.masemDataSummaryTables <- function(jaspResults, dataset, options) {

  # create a table with the number of non-missing elements of the covariance matrix (or means if both are specified)
  if (!is.null(jaspResults[["dataSummaryTables"]]))
    return()

  dataSummaryTables <- createJaspContainer(title = gettext("Data Summary"))
  dataSummaryTables$position <- 2
  dataSummaryTables$dependOn(c("correlationCovarianceMatrix", "means", "dataInputType", "variableNameSeparator", "inputSummaryFrequencyTable"))
  jaspResults[["dataSummaryTables"]] <- dataSummaryTables

  ### create correlation/covariance matrix summary
  correlationCovarianceSummaryTable <- createJaspTable(title = gettextf("Number of Estimates: %1$s Matrix", switch(
    options[["dataInputType"]],
    "correlation" = gettext("Correlation"),
    "covariance"  = gettext("Covariance")
  )))
  correlationCovarianceSummaryTable$position <- 1
  dataSummaryTables[["correlationCovarianceSummaryTable"]] <- correlationCovarianceSummaryTable

  if (is.null(dataset[["inputMatricies"]]))
    return()

  # get data
  inputMatricies <- dataset[["inputMatricies"]]
  variableNames  <- attr(inputMatricies, "variableNames")

  # add columns
  correlationCovarianceSummaryTable$addColumnInfo(name = "variable", type = "string", title = gettext("Variable"))
  for (var in variableNames) {
    correlationCovarianceSummaryTable$addColumnInfo(name = var, type = "integer",  title = var)
  }

  # compute the number of non-missing elements
  nonNaMatric <- matrix(NA, nrow = length(variableNames), ncol = length(variableNames))
  colnames(nonNaMatric) <- variableNames
  for (j in seq_along(variableNames)) {
    for (k in seq_along(variableNames)) {
      if (j > k) {
        nonNaMatric[j, k] <- sum(!is.na(sapply(inputMatricies, function(x) x[j, k])))
      }
    }
    if (options[["dataInputType"]] == "covariance") {
      nonNaMatric[j, j] <- sum(!is.na(sapply(inputMatricies, function(x) x[j, j])))
    }
  }
  nonNaMatric <- cbind.data.frame(variable = variableNames, nonNaMatric)
  correlationCovarianceSummaryTable$setData(nonNaMatric)


  ### create means matrix summary
  if (!is.null(dataset[["means"]])) {

    meansSummaryTable <- createJaspTable(title = gettext("Number of Estimates: Means"))
    meansSummaryTable$position <- 2
    dataSummaryTables[["meansSummaryTable"]] <- meansSummaryTable

    # get data
    meansData <- dataset[["means"]]

    # add columns
    meansSummaryTable$addColumnInfo(name = "variable",  type = "string", title = gettext("Variable"))
    meansSummaryTable$addColumnInfo(name = "estimates", type = "string", title = gettext("Estimates"))

    # compute the number of non-missing elements
    nonNaMeans <- apply(meansData, 2, function(x) sum(!is.na(x)))
    nonNaMeans <- cbind.data.frame(variable = colnames(meansData), estimates = nonNaMeans)

    meansSummaryTable$setData(nonNaMeans)
  }


  return()
}
