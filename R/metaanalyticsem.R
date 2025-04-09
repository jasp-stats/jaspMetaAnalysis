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
  library(metaSEM)
  #
  .masemMxOptions()

  # read the data set
  dataset <- .masemDecodeData(dataset, options)

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  # create a data-summary of the input
  if (options[["inputSummaryAvailableVariableNames"]])
    .masemVariableSummaryView(jaspResults, dataset, options)
  if (options[["inputSummaryFrequencyTable"]])
    .masemDataSummaryTables(jaspResults, dataset, options)

  # estimate the models
  .masemFitModels(jaspResults, dataset, options, MASEM = TRUE)
  .masemFitMeasures(jaspResults, options)

  # create summary with model fit statistics (for all models)
  .masemModelFitTable(jaspResults, options)

  if (options[["additionalFitMeasures"]])
    .masemAdditionalFitMeasuresTable(jaspResults, options)

  # create model-level summaries
  if (options[["modelSummary"]])
    .masemModelSummaryTable(jaspResults, options, MASEM = TRUE)

  # create path diagrams
  if (options[["pathDiagram"]])
    .masemModelPathDiagram(jaspResults, options, MASEM = TRUE)

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

  if (length(inputNames) == 0 || options[["sampleSize"]] == "") {
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

    # add variable names if any are missing
    for (i in seq_along(setdiff(uniqueInputNames, decodedMeanNames))) {
      datasetMeans[[setdiff(uniqueInputNames, decodedMeanNames)[i]]] <- NA
    }

    # fix order
    datasetMeans <- datasetMeans[,uniqueInputNames]


    output[["means"]] <- datasetMeans
  }

  return(output)
}
.masemVariableSummaryView  <- function(jaspResults, dataset, options) {

  # create an HTML output listing all available variable names (based on the correlation/covariance matrix input)
  variableSummaryView <- createJaspHtml(title = gettext("Available Variable Names"))
  variableSummaryView$dependOn(c("correlationCovarianceMatrix", "means", "dataInputType", "variableNameSeparator", "inputSummaryAvailableVariableNames"))
  variableSummaryView$position <- 0.1
  jaspResults[["variableSummaryView"]] <- variableSummaryView

  if (!is.null(dataset[["inputMatricies"]])) {
    variableSummaryView[["text"]] <- paste0(attr(dataset[["inputMatricies"]], "variableNames"), collapse = "<br>")
  }

  return()
}
.masemDataSummaryTables    <- function(jaspResults, dataset, options) {

  # create a table with the number of non-missing elements of the covariance matrix (or means if both are specified)
  if (!is.null(jaspResults[["dataSummaryTables"]]))
    return()

  dataSummaryTables <- createJaspContainer(title = gettext("Data Summary"))
  dataSummaryTables$position <- 0.2
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

.masemFitMeasures                <- function(jaspResults, options) {

  # add fit measures on model-by model bases if
  # 1) they were requested
  # 2) a new model was fitted that does not have fit measures
  # the computation takes a bit, so it is worthwhile storing them

  if (!options[["additionalFitMeasures"]])
    return()

  # obtain the model container
  modelContainer <- jaspResults[["modelContainer"]]
  fits           <- modelContainer$object

  # compute the individual model fits
  for (model in options[["models"]]) {

    # current fit
    tempFit <- fits[[model[["value"]]]]

    # skip on error or null
    if (is.null(tempFit) || jaspBase::isTryError(tempFit))
      next

    # check if the fit measures are already computed
    if (!is.null(attr(tempFit, "fitIndices")))
      next

    # compute the fit measures
    jaspBase::startProgressbar(1, label = gettextf("Computing additional fit measures: %1$s", model[["value"]]))
    tempSummary <- summary(tempFit, fitIndices = TRUE)

    # store the fit measures
    attr(tempFit, "fitIndices") <- tempSummary
    fits[[model[["value"]]]]    <- tempFit
    jaspBase::progressbarTick()

  }

  # return all of the fits to the container
  modelContainer$object <- fits

  return()
}
.masemAdditionalFitMeasuresTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["additionalFitMeasures"]]))
    return()

  # prepare table
  additionalFitMeasures <- createJaspTable(gettext("Additional Fit Measures"))
  additionalFitMeasures$position <- 1.1
  additionalFitMeasures$dependOn(c(.semmetaDependencies, "additionalFitMeasures"))
  jaspResults[["additionalFitMeasures"]] <- additionalFitMeasures

  # add columns
  additionalFitMeasures$addColumnInfo(name = "name",        type = "string",  title = "")
  additionalFitMeasures$addColumnInfo(name = "cfi",         type = "number",  title = gettext("CLI"))
  additionalFitMeasures$addColumnInfo(name = "tli",         type = "number",  title = gettext("TLI"))
  additionalFitMeasures$addColumnInfo(name = "rmsea",       type = "number",  title = gettext("RMSEA"))
  additionalFitMeasures$addColumnInfo(name = "prmsea",      type = "number",  title = gettext("p(RMSEA < 0.05)"))

  # exit if not ready
  if(!.masemReady(options) || is.null(jaspResults[["modelContainer"]]))
    return()

  # extract fits
  fits <- jaspResults[["modelContainer"]]$object

  # loop over models and store model fit indicies
  out <- list()
  for (model in options[["models"]]) {

    # extract model fit
    tempFit <- fits[[model[["value"]]]]

    if (jaspBase::isTryError(tempFit) || is.null(tempFit) || is.null(attr(tempFit, "fitIndices"))) {
      out[[model[["value"]]]] <- data.frame(
        name   = model[["value"]],
        cfi    = NA,
        tli    = NA,
        rmsea  = NA,
        prmsea = NA
      )

    } else {
      # extract fit measures
      tempSummary <- attr(tempFit, "fitIndices")
      out[[model[["value"]]]] <- data.frame(
        name   = model[["value"]],
        cfi    = tempSummary$CFI,
        tli    = tempSummary$TLI,
        rmsea  = tempSummary$RMSEA,
        prmsea = tempSummary$RMSEAClose
      )
    }
  }

  # assign output to table
  additionalFitMeasures$setData(do.call(rbind, out))

  return()
}
.masemCreateSummaryTable         <- function(tempOutputContainer, tempFit, options, output) {

  # create summary table
  tempSummaryTable <- createJaspTable(title = switch(
    output,
    "parameters"    = gettext("Parameter Estimates"),
    "covariances"   = gettext("Covariance Estimates"),
    "randomEffects" = gettext("Random Effects Estimates")
  ))
  tempSummaryTable$position <- switch(
    output,
    "parameters"    = 1,
    "covariances"   = 2,
    "randomEffects" = 3
  )
  tempSummaryTable$dependOn(c(.semmetaDependencies, "modelSummary", switch(
    output,
    "parameters"    = "modelSummaryParameters",
    "covariances"   = "modelSummaryCovariances",
    "randomEffects" = "modelSummaryRandomEffects"
  )))
  tempOutputContainer[[output]] <- tempSummaryTable

  # add columns
  tempSummaryTable$addColumnInfo(name = "row",      type = "string",  title = gettext("Row"))
  tempSummaryTable$addColumnInfo(name = "col",      type = "string",  title = gettext("Column"))
  tempSummaryTable$addColumnInfo(name = "estimate", type = "number",  title = gettext("Estimate"))
  if (options[["modelSummaryConfidenceIntervalType"]] == "standardErrors") {
    tempSummaryTable$addColumnInfo(name = "se",        type = "number",  title = gettext("Standard Error"))
    tempSummaryTable$addColumnInfo(name = "z",         type = "number",  title = gettext("z"))
    tempSummaryTable$addColumnInfo(name = "p",         type = "pvalue",  title = gettext("p"))
  }
  tempSummaryTable$addColumnInfo(name = "lCi",       type = "number",  title = gettext("Lower"), overtitle = gettextf("95%% CI"))
  tempSummaryTable$addColumnInfo(name = "uCi",       type = "number",  title = gettext("Upper"), overtitle = gettextf("95%% CI"))

  # skip if not ready
  if (!.masemReady(options))
    return()

  if (is.null(tempFit))
    return()

  # check if the model fit failed
  if (jaspBase::isTryError(tempFit)) {
    tempSummaryTable$setError(gettextf("Model fit failed with the following message %1$s.", tempFit))
    return()
  }

  # extract the parameter estimates
  tempOutput <- summary(tempFit)[["parameters"]]
  colnames(tempOutput) <- c("name", "matrix", "row", "col", "estimate", "se", "lCi", "uCi", "lCiMet", "uCiMet", "z", "p")
  tempOutput <- tempOutput[tempOutput$matrix == switch(
    output,
    "parameters"    = "Amatrix",
    "covariances"   = "Smatrix",
    "randomEffects" = "TauCov"
  ), ,drop=FALSE]

  if (options[["modelSummaryConfidenceIntervalType"]] == "likelihoodBased") {
    tempOutput <- tempOutput[, c("row", "col", "estimate", "lCi", "uCi"),drop=FALSE]
  } else {
    tempOutput <- tempOutput[, c("row", "col", "estimate", "se", "z", "p", "lCi", "uCi"),drop=FALSE]
  }

  # add output to container
  tempSummaryTable$setData(tempOutput)

  return()
}
.masemMxOptions                  <- function(){
  OpenMx::mxSetDefaultOptions()
  OpenMx::mxOption(NULL, "Default optimizer", "SLSQP")
  OpenMx::mxOption(NULL, "Gradient algorithm", "central")
  OpenMx::mxOption(NULL, "Optimality tolerance", "6.3e-14")
  OpenMx::mxOption(NULL, "Gradient iterations", 2)
}

checkMetaModel <- function(model, availableVars) {
  # based on jaspSem:::checkLavaanModel

  # function returns informative printable string if there is an error, else ""
  if (model == "") return("Enter a model")

  # translate to base64 - function from semsimple.R
  vvars    <- availableVars
  usedvars <- vvars #.semGetUsedVars(model, vvars)
  vmodel   <- model # .semTranslateModel(model, usedvars)

  unvvars <- availableVars
  names(unvvars) <- vvars

  # Check model syntax
  parsed <- try(lavaan::lavParseModelString(vmodel, TRUE), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    msg <- attr(parsed, "condition")$message
    if (msg == "NA/NaN argument") {
      return("Enter a model")
    }
    return(stringr::str_replace_all(msg, unvvars))
  }

  # Check variable names
  if (!missing(availableVars)) {
    latents <- unique(parsed[parsed$op == "=~",]$lhs)
    modelVars <- setdiff(unique(c(parsed$lhs, parsed$rhs)), latents)
    modelVars <- modelVars[modelVars != ""] # e.g., x1 ~ 1 yields an empty rhs entry

    modelVarsInAvailableVars <- (modelVars %in% vvars)
    if (!all(modelVarsInAvailableVars)) {
      notRecognized <- modelVars[!modelVarsInAvailableVars]
      return(paste("Variable(s) in model syntax not recognized:",
                   paste(stringr::str_replace_all(notRecognized, unvvars),
                         collapse = ", ")))
    }
  }

  # if checks pass, return empty string
  return("")
}
