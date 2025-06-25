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
  .masemMxOptions()

  # read the data set
  dataset <- .masemDecodeData(dataset, options)

  # create a data-summary of the input
  if (options[["availableVariableNames"]])
    .masemVariableSummaryView(jaspResults, dataset, options)
  if (options[["numberOfEstimates"]])
    .masemDataSummaryTables(jaspResults, dataset, options, type = "estimates")
  if (options[["numberOfObservations"]])
    .masemDataSummaryTables(jaspResults, dataset, options, type = "observations")
  if (options[["pooledCorrelationCovarianceMatrix"]])
    .masemPooledCorrelationCovarianceMatrix(jaspResults, dataset, options)


  # estimate the models
  .masemFitModels(jaspResults, dataset, options, MASEM = TRUE)
  .masemFitMeasures(jaspResults, options)

  if (options[["fitMeasures"]])
    .masemFitMeasuresTable(jaspResults, options)
  if (options[["pairwiseModelComparison"]])
    .masemPairwiseModelComparisonTable(jaspResults, options)

  # create model-level summaries
  if (options[["modelSummary"]])
    .masemModelSummaryTable(jaspResults, options, MASEM = TRUE)

  # create path diagrams
  if (options[["pathDiagram"]])
    .masemModelPathDiagram(jaspResults, options, MASEM = TRUE)

  return()
}

.masemDependencies <- c(
  "correlationCovarianceMatrix", "sampleSize", "means", "dataInputType", "variableNameSeparator", "models"
)
.masemDecodeData           <- function(dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  dataset <- .readDataSetToEnd(all.columns = TRUE)
  # masem requires a wide format of the correlation/covariance matrices
  # (because of backwards compatibility with previous tutorial papers etc)
  # therefore, the variable names need to be decoded manually for reconstruction
  # of the matrices

  inputNames <- options[["correlationCovarianceMatrix"]]

  if (length(inputNames) == 0 || options[["sampleSize"]] == "") {
    return()
  }

  # subset the data
  inputDataset <- dataset[,inputNames,drop=FALSE]

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

      if (any(abs(tempMatrix) > 1, na.rm = TRUE))
        .quitAnalysis(gettext("The correlation matrix contains values outside the range of -1 and 1. Please check your input."))

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
    data = matricies,
    n    = dataset[[options[["sampleSize"]]]]
  )

  # check if means are specified
  meanNames <- options[["means"]]
  if (length(meanNames) > 0) {

    # select and decode data
    datasetMeans           <- dataset[,meanNames,drop=FALSE]
    decodedMeanNames       <- decodeColNames(meanNames)
    colnames(datasetMeans) <- decodedMeanNames

    # check if there are some superfluous variables
    if (any(!decodedMeanNames %in% uniqueInputNames))
      .quitAnalysis(gettextf("The 'Means' variable input contains the following variable that is not specified in the 'Correlation/Covariance Matrix' variable input: %1$s.",
                             paste0(setdiff(decodedMeanNames, uniqueInputNames), collapse = ", ")))

    # add variable names if any are missing
    for (i in seq_along(setdiff(uniqueInputNames, decodedMeanNames))) {
      datasetMeans[[setdiff(uniqueInputNames, decodedMeanNames)[i]]] <- NA
    }

    # fix order
    datasetMeans <- datasetMeans[,uniqueInputNames]

    output[["means"]] <- datasetMeans
  }

  # check if moderators are specified
  varNames <- unique(c(
    # TODO: check whether moderators are always in data.
    #    unlist(lapply(options[["models"]], function(model) model[["syntax"]][["value"]])),
    unlist(lapply(options[["models"]], function(model) model[["syntax"]][["prefixedColumns"]][["data."]]))
  ))
  modNames <- setdiff(varNames, c(inputNames, meanNames))
  if (length(modNames) > 0) {
    # select and decode data
    datasetMods           <- dataset[,modNames,drop=FALSE]
    decodedmodNames       <- decodeColNames(modNames)
    colnames(datasetMods) <- decodedmodNames

    for (var in decodedmodNames){
      output[[var]] <- datasetMods[[var]]
    }
    attr(output, "modNames") <- decodedmodNames
  }

  return(output)
}
.masemVariableSummaryView  <- function(jaspResults, dataset, options) {

  # create an HTML output listing all available variable names (based on the correlation/covariance matrix input)
  variableSummaryView <- createJaspHtml(title = gettext("Available Variable Names"))
  variableSummaryView$dependOn(c("correlationCovarianceMatrix", "means", "dataInputType", "variableNameSeparator", "availableVariableNames"))
  variableSummaryView$position <- 0.1
  jaspResults[["variableSummaryView"]] <- variableSummaryView

  if (!is.null(dataset[["data"]])) {
    variableSummaryView[["text"]] <- paste0(attr(dataset[["data"]], "variableNames"), collapse = "<br>")
  }

  return()
}
.masemDataSummaryTables    <- function(jaspResults, dataset, options, type) {

  # obtain the descriptives container
  if (!is.null(jaspResults[["descriptives"]])) {
    descriptivesContainer <- jaspResults[["descriptives"]]
  } else {
    descriptivesContainer <- createJaspContainer(title = gettext("Descriptives"))
    descriptivesContainer$dependOn(c("correlationCovarianceMatrix", "means", "sampleSize", "dataInputType", "variableNameSeparator"))
    descriptivesContainer$position <- 0.2
    jaspResults[["descriptives"]] <- descriptivesContainer
  }

  # create a table with the number of non-missing elements of the covariance matrix (or means if both are specified)
  if (is.null(descriptivesContainer[[paste0(type, "corCovTable")]])) {

    ### create correlation/covariance matrix summary
    correlationCovarianceSummaryTable <- createJaspTable(title = gettextf(
      "%1$s: %1$s Matrix",
      switch(
        type,
        "estimates"     = gettext("Number of Estimates"),
        "observations"  = gettext("Number of Observations")
      ),
      switch(
        options[["dataInputType"]],
        "correlation" = gettext("Correlation"),
        "covariance"  = gettext("Covariance")
      )))
    correlationCovarianceSummaryTable$position <- switch(
      type,
      "estimates"     = 1,
      "observations"  = 2
    )
    correlationCovarianceSummaryTable$dependOn(switch(
      type,
      "estimates"    = "numberOfEstimates",
      "observations" = "numberOfObservations"
    ))

    descriptivesContainer[[paste0(type, "corCovTable")]] <- correlationCovarianceSummaryTable

    if (is.null(dataset[["data"]]))
      return()

    # get data
    inputMatricies <- dataset[["data"]]
    sampleSize     <- dataset[["n"]]
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
          nonNaMatric[j, k] <- switch(
            type,
            "estimates"    = sum(!is.na(sapply(inputMatricies, function(x) x[j, k]))),
            "observations" = sum(sampleSize * !is.na(sapply(inputMatricies, function(x) x[j, k])), na.rm = TRUE),
          )
        }
      }
      if (options[["dataInputType"]] == "covariance") {
        nonNaMatric[j, j] <- switch(
          type,
          "estimates"    = sum(!is.na(sapply(inputMatricies, function(x) x[j, j]))),
          "observations" = sum(sampleSize * !is.na(sapply(inputMatricies, function(x) x[j, j])), na.rm = TRUE)
        )
      }
    }
    nonNaMatric <- cbind.data.frame(variable = variableNames, nonNaMatric)
    correlationCovarianceSummaryTable$setData(nonNaMatric)

  }

  ### create means matrix summary

  if (is.null(descriptivesContainer[[paste0(type, "meanTable")]]) && !is.null(dataset[["means"]])) {

    meansSummaryTable <- createJaspTable(title = gettextf(
      "%1$s: Means",
      switch(
        type,
        "estimates"     = gettext("Number of Estimates"),
        "observations"  = gettext("Number of Observations")
      )))
    meansSummaryTable$position <- switch(
      type,
      "estimates"     = 3,
      "observations"  = 4
    )
    meansSummaryTable$dependOn(switch(
      type,
      "estimates"    = "numberOfEstimates",
      "observations" = "numberOfObservations"
    ))
    descriptivesContainer[[paste0(type, "meanTable")]] <- meansSummaryTable

    # get data
    meansData <- dataset[["means"]]

    # add columns
    meansSummaryTable$addColumnInfo(name = "variable",  type = "string",  title = gettext("Variable"))
    meansSummaryTable$addColumnInfo(name = "estimates", type = "integer", title = switch(
      type,
      "estimates"    = gettext("Estimates"),
      "observations" = gettext("Observations")
    ))

    # compute the number of non-missing elements
    nonNaMeans <- switch(
      type,
      "estimates"    = apply(meansData, 2, function(x) sum(!is.na(x))),
      "observations" = apply(meansData, 2, function(x) sum(sampleSize * !is.na(x), na.rm = TRUE))
    )
    nonNaMeans <- cbind.data.frame(variable = colnames(meansData), estimates = nonNaMeans)

    meansSummaryTable$setData(nonNaMeans)
  }


  return()
}
.masemPooledCorrelationCovarianceMatrix <- function(jaspResults, dataset, options) {

  # obtain the descriptives container
  if (!is.null(jaspResults[["pooledContainer"]])) {
    return()
  } else {
    pooledContainer <- createJaspContainer(title = gettextf(
      "Pooled %1$s Matrix", switch(
        options[["dataInputType"]],
        "correlation" = gettext("Correlation"),
        "covariance"  = gettext("Covariance")
      )))
    pooledContainer$dependOn(c("correlationCovarianceMatrix", "means", "sampleSize", "dataInputType", "variableNameSeparator",
                               "pooledCorrelationCovarianceMatrix", "pooledCorrelationCovarianceMatrixRandomEffects"))
    pooledContainer$position <- 0.3
    jaspResults[["pooledContainer"]] <- pooledContainer
  }

  pooledMatrix <- createJaspTable(title = gettextf(
    "Pooled %1$s Matrix", switch(
      options[["dataInputType"]],
      "correlation" = gettext("Correlation"),
      "covariance"  = gettext("Covariance")
    )))
  pooledMatrix$position <- 1
  pooledContainer[["pooledMatrix"]] <- pooledMatrix

  # exit if not ready
  if (is.null(dataset[["data"]]))
    return()

  fit <- try(metaSEM::tssem1(
    Cov    = dataset[["data"]],
    n      = dataset[["n"]],
    cor.analysis = options[["dataInputType"]] == "correlation",
    RE.type      = .masemGetRandomEffectsType(options[["pooledCorrelationCovarianceMatrixRandomEffects"]])
  ))
  fitSummary <- try(summary(fit))

  # error handling
  if (jaspBase::isTryError(fit) || jaspBase::isTryError(fitSummary)) {
    pooledMatrix$setError(gettext("The pooled correlation/covariance matrix could not be computed. Consider verifying that all correlation/covariance matrix variables are correctly specified via 'Descriptives'."))
    return()
  }

  # create a table with the pooled correlation/covariance matrix
  variableNames <- attr(dataset[["data"]], "variableNames")
  pooledMatrix$addColumnInfo(name = "variable", type = "string", title = gettext("Variable"))
  for (var in variableNames) {
    pooledMatrix$addColumnInfo(name = var, type = "number", title = var)
  }

  # obtain model summary
  coefficientsSummary           <- fitSummary[["coefficients"]]
  colnames(coefficientsSummary) <- c("estimate", "se", "lCi",  "uCi", "z", "p")
  coefficientsSummaryMat <- coefficientsSummary[grepl("Intercept",rownames(coefficientsSummary)),]
  coefficientsSummaryRan <- coefficientsSummary[grepl("Tau",rownames(coefficientsSummary)),]

  # add correlation/covariance matrix
  covMatrix <- matrix(NA, nrow = length(variableNames), ncol = length(variableNames))
  colnames(covMatrix)  <- variableNames
  if (options[["dataInputType"]] == "correlation") {
    covMatrix[lower.tri(covMatrix)] <- coefficientsSummaryMat[,"estimate"]
  } else if (options[["dataInputType"]] == "covariance") {
    covMatrix[!upper.tri(covMatrix)] <- coefficientsSummaryMat[,"estimate"]
  }
  covMatrix <- cbind.data.frame(variable = variableNames, covMatrix)
  pooledMatrix$setData(covMatrix)

  # fix names (the coefficients are in column major order)
  if (options[["dataInputType"]] == "correlation") {
    matrixIndicies <- which(lower.tri(covMatrix), arr.ind = TRUE)
    coefficientsSummaryMat$name <- gettext("Correlation")
  } else if (options[["dataInputType"]] == "covariance") {
    matrixIndicies <- which(!upper.tri(covMatrix), arr.ind = TRUE)
    coefficientsSummaryMat$name <- gettext("Covariance")
  }
  coefficientsSummaryMat$row  <- variableNames[matrixIndicies[,1]]
  coefficientsSummaryMat$col  <- variableNames[matrixIndicies[,2]]
  if (options[["pooledCorrelationCovarianceMatrixRandomEffects"]] != "zero") {
    if (options[["pooledCorrelationCovarianceMatrixRandomEffects"]] == "diagonal") {
      coefficientsSummaryRan$row  <- variableNames[matrixIndicies[,1]]
      coefficientsSummaryRan$col  <- variableNames[matrixIndicies[,2]]
      coefficientsSummaryRan$name <- gettext("Random Effect")
    }else if (options[["pooledCorrelationCovarianceMatrixRandomEffects"]] == "symmetric") {
      ranNames       <- do.call(rbind, strsplit(rownames(coefficientsSummaryRan), "_"))
      varVectorNames <- paste0(variableNames[matrixIndicies[,1]], ", ", variableNames[matrixIndicies[,2]])
      coefficientsSummaryRan$row  <- varVectorNames[as.numeric(ranNames[,2])]
      coefficientsSummaryRan$col  <- varVectorNames[as.numeric(ranNames[,3])]
      coefficientsSummaryRan$name <- gettext("Random Effect Covariance")
      coefficientsSummaryRan$row[ranNames[,2] == ranNames[,3]]  <- variableNames[matrixIndicies[,1]]
      coefficientsSummaryRan$col[ranNames[,2] == ranNames[,3]]  <- variableNames[matrixIndicies[,2]]
      coefficientsSummaryRan$name[ranNames[,2] == ranNames[,3]] <- gettext("Random Effect")
      coefficientsSummaryRan <- coefficientsSummaryRan[order(coefficientsSummaryRan$name),]
    }

    coefficientsSummaryMat <- rbind(coefficientsSummaryMat, coefficientsSummaryRan)
  }

  # add all parameters table
  pooledParameters <- createJaspTable(title = gettextf(
    "Pooled %1$s Matrix Parameters", switch(
      options[["dataInputType"]],
      "correlation" = gettext("Correlation"),
      "covariance"  = gettext("Covariance")
    )))
  pooledParameters$position <- 2
  pooledContainer[["pooledParameters"]] <- pooledParameters

  # add columns
  pooledParameters$addColumnInfo(name = "name",     type = "string",  title = "")
  pooledParameters$addColumnInfo(name = "row",      type = "string",  title = gettext("Row"))
  pooledParameters$addColumnInfo(name = "col",      type = "string",  title = gettext("Column"))
  pooledParameters$addColumnInfo(name = "estimate", type = "number",  title = gettext("Estimate"))
  pooledParameters$addColumnInfo(name = "se",        type = "number",  title = gettext("Standard Error"))
  pooledParameters$addColumnInfo(name = "lCi",       type = "number",  title = gettext("Lower"), overtitle = gettextf("95%% CI"))
  pooledParameters$addColumnInfo(name = "uCi",       type = "number",  title = gettext("Upper"), overtitle = gettextf("95%% CI"))
  pooledParameters$addColumnInfo(name = "z",         type = "number",  title = gettext("z"))
  pooledParameters$addColumnInfo(name = "p",         type = "pvalue",  title = gettext("p"))

  pooledParameters$setData(coefficientsSummaryMat)

  return()
}
.masemFitMeasures                  <- function(jaspResults, options) {

  # add fit measures on model-by model bases if
  # 1) they were requested
  # 2) a new model was fitted that does not have fit measures
  # the computation takes a bit, so it is worthwhile storing them

  if (!options[["fitMeasures"]])
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
.masemFitMeasuresTable   <- function(jaspResults, options) {

  if (!is.null(jaspResults[["fitMeasures"]]))
    return()

  # prepare table
  fitMeasures <- createJaspTable(gettext("Fit Measures"))
  fitMeasures$position <- 1.1
  fitMeasures$dependOn(c(.masemDependencies, "fitMeasures"))
  jaspResults[["fitMeasures"]] <- fitMeasures

  # add columns
  fitMeasures$addColumnInfo(name = "name",        type = "string",  title = "")
  fitMeasures$addColumnInfo(name = "chi2",        type = "number",  title = "\U03C7\U00B2")
  fitMeasures$addColumnInfo(name = "df",          type = "integer", title = gettext("df"))
  fitMeasures$addColumnInfo(name = "p",           type = "pvalue",  title = gettext("p"))
  fitMeasures$addColumnInfo(name = "cfi",         type = "number",  title = gettext("CLI"))
  fitMeasures$addColumnInfo(name = "tli",         type = "number",  title = gettext("TLI"))
  fitMeasures$addColumnInfo(name = "rmsea",       type = "number",  title = gettext("RMSEA"))
  fitMeasures$addColumnInfo(name = "prmsea",      type = "number",  title = gettext("p(RMSEA < 0.05)"))

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
        prmsea = NA,
        chi2   = NA,
        df     = NA,
        p      = NA
      )

    } else {
      # extract fit measures
      tempSummary <- attr(tempFit, "fitIndices")
      out[[model[["value"]]]] <- data.frame(
        name   = model[["value"]],
        cfi    = tempSummary$CFI,
        tli    = tempSummary$TLI,
        rmsea  = tempSummary$RMSEA,
        prmsea = tempSummary$RMSEAClose,
        chi2   = tempSummary$Chi,
        df     = tempSummary$ChiDoF,
        p      = tempSummary$p
      )
    }
  }

  # assign output to table
  fitMeasures$setData(do.call(rbind, out))

  return()
}
.masemPairwiseModelComparisonTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["pairwiseModelComparison"]]))
    return()

  # prepare table
  pairwiseModelComparison <- createJaspTable(gettext("Pairwise Model Comparison"))
  pairwiseModelComparison$position <- 1.2
  pairwiseModelComparison$dependOn(c(.masemDependencies, "pairwiseModelComparison"))
  jaspResults[["pairwiseModelComparison"]] <- pairwiseModelComparison

  # add columns
  pairwiseModelComparison$addColumnInfo(name = "comparison",  type = "string",  title = "")
  pairwiseModelComparison$addColumnInfo(name = "difChi2",     type = "integer", title = gettext("2\U0394LL"))
  pairwiseModelComparison$addColumnInfo(name = "difDf",       type = "integer", title = gettext("\U0394 df"))
  pairwiseModelComparison$addColumnInfo(name = "p",           type = "pvalue",  title = gettext("p"))

  # exit if not ready
  if(!.masemReady(options) || is.null(jaspResults[["modelContainer"]]))
    return()

  # extract fits
  fits <- jaspResults[["modelContainer"]]$object
  if (length(fits) < 2) {
    pairwiseModelComparison$addFootnote(gettext("At least two models are required for pairwise model comparison."))
    return()
  }

  # loop over models and store model fit indicies
  out <- list()
  for (i in seq_along(fits)) {
    for (j in seq_along(fits)) {

      if (i == j)
        next

      tempFit1 <- fits[[options[["models"]][[i]][["value"]]]]
      tempFit2 <- fits[[options[["models"]][[j]][["value"]]]]

      if (jaspBase::isTryError(tempFit1) || is.null(tempFit1))
        next
      if (jaspBase::isTryError(tempFit2) || is.null(tempFit2))
        next

      tempAnova <- suppressWarnings(anova(tempFit1, tempFit2))

      out[[length(out) + 1]] <- data.frame(
        comparison = paste0(options[["models"]][[i]][["value"]], " vs ", options[["models"]][[j]][["value"]]),
        difChi2    = tempAnova$diffLL[2],
        difDf      = tempAnova$diffdf[2],
        p          = tempAnova$p[2]
      )
    }
  }

  out <- do.call(rbind, out)
  out <- out[out[["difDf"]] > 0,,drop=FALSE]

  if (nrow(out) == 0)
    pairwiseModelComparison$addFootnote(gettext("No pairwise model comparison with at least one degree of freedom difference available."))


  # assign output to table
  pairwiseModelComparison$setData(out)

  return()
}
.masemCreateSummaryTable           <- function(tempOutputContainer, tempFit, options, output) {

  # create summary table
  tempSummaryTable <- createJaspTable(title = switch(
    output,
    "regression"      = gettext("Regression Estimates"),
    "meansIntercepts" = gettext("Mean/Intercept Estimates"),
    "covariances"     = gettext("Covariance Estimates"),
    "randomEffects"   = gettext("Random Effects Estimates")
  ))
  tempSummaryTable$position <- switch(
    output,
    "regression"      = 1,
    "meansIntercepts" = 2,
    "covariances"     = 3,
    "randomEffects"   = 4
  )
  tempSummaryTable$dependOn(c(.masemDependencies, "modelSummary", "modelSummaryShowMatrixIndices", switch(
    output,
    "regression"      = "modelSummaryRegression",
    "meansIntercepts" = "modelSummaryMeansIntercepts",
    "covariances"     = "modelSummaryCovariances",
    "randomEffects"   = "modelSummaryRandomEffects"
  )))
  tempOutputContainer[[output]] <- tempSummaryTable

  # add columns
  tempSummaryTable$addColumnInfo(name = "name",     type = "string",  title = "")
  if (options[["modelSummaryShowMatrixIndices"]]) {
    tempSummaryTable$addColumnInfo(name = "row",      type = "string",  title = gettext("Row"))
    tempSummaryTable$addColumnInfo(name = "col",      type = "string",  title = gettext("Column"))
  }
  tempSummaryTable$addColumnInfo(name = "estimate", type = "number",  title = gettext("Estimate"))
  tempSummaryTable$addColumnInfo(name = "se",        type = "number",  title = gettext("Standard Error"))
  tempSummaryTable$addColumnInfo(name = "lCi",       type = "number",  title = gettext("Lower"), overtitle = gettextf("95%% CI"))
  tempSummaryTable$addColumnInfo(name = "uCi",       type = "number",  title = gettext("Upper"), overtitle = gettextf("95%% CI"))
  tempSummaryTable$addColumnInfo(name = "z",         type = "number",  title = gettext("z"))
  tempSummaryTable$addColumnInfo(name = "p",         type = "pvalue",  title = gettext("p"))

  # skip if not ready
  if (!.masemReady(options))
    return()

  if (is.null(tempFit))
    return()

  # check if the model fit failed
  if (jaspBase::isTryError(tempFit)) {
    tempSummaryTable$setError(gettextf("Model fit failed with the following message: %1$s.", tempFit))
    return()
  }

  # extract the parameter estimates
  tempOutput <- summary(tempFit)[["parameters"]]
  colnames(tempOutput) <- c("name", "matrix", "row", "col", "estimate", "se", "lCi", "uCi", "lCiMet", "uCiMet", "z", "p")
  tempOutput <- tempOutput[tempOutput$matrix %in% switch(
    output,
    "regression"      = c("Amatrix", "Amatrixvars"),
    "meansIntercepts" = c("Mmatrix", "Mmatrixvars"),
    "covariances"     = "Smatrix",
    "randomEffects"   = c("TauCov", "TauMean")
  ), ,drop=FALSE]

  # remove additional columns
  tempOutput <- tempOutput[, c("name", "row", "col", "estimate", "se", "z", "p", "lCi", "uCi"),drop=FALSE]
  tempOutput$lCi <- tempOutput$estimate - 1.96 * tempOutput$se
  tempOutput$uCi <- tempOutput$estimate + 1.96 * tempOutput$se

  if (!options[["modelSummaryShowMatrixIndices"]]) {
    tempOutput <- tempOutput[,!colnames(tempOutput) %in% c("row", "col"),drop=FALSE]
  }

  # add output to container
  tempSummaryTable$setData(tempOutput)

  return()
}
.masemMxOptions                    <- function(){
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
