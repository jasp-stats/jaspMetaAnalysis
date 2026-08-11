# Robust Bayesian estimated marginal means.
#
# Builds and computes model-averaged estimated marginal means.

.robmaEstimatedMarginalMeans             <- function(jaspResults, options) {

  # so, this section is a bit complicated -- all in order to prevent updating of all subcomponents once a new variable is added/removed
  # the main container contains effect size and heterogeneity subcontainers, which contain variable containers with the actual output tables
  # updating of the subtables is skipped unless one of the options specified here is checked:
  # .robmaGetEstimatedMarginalMeansOptions()

  # check whether the section should be created at all
  isReadyEffectSize <- (length(options[["estimatedMarginalMeansEffectSizeSelectedVariables"]]) > 0 || options[["estimatedMarginalMeansEffectSizeAddAdjustedEstimate"]]) &&
    (options[["estimatedMarginalMeansEffectSize"]])

  # disable BF tests if no model-averaging is performed
  if (!(options[["bayesianModelAveragingModerations"]] || options[["bayesianModelAveragingEffectSize"]]))
    options[["estimatedMarginalMeansEffectSizeTestAgainst0"]] <- FALSE

  # remove section if exists
  if (!isReadyEffectSize) {
    if (!is.null(jaspResults[["estimatedMarginalMeansContainer"]]))
      jaspResults[["estimatedMarginalMeansContainer"]] <- NULL

    return()
  }

  # create/extract section otherwise
  estimatedMarginalMeansContainer <- .robmaExtractEstimatedMarginalMeansContainer(jaspResults)

  # fill the section with EMM tables for each variables for the effect size
  if (isReadyEffectSize)
    .robmaEstimatedMarginalMeansFun(jaspResults, options)

  return()
}

.robmaEstimatedMarginalMeansFun          <- function(jaspResults, options) {

  # get the corresponding container
  estimatedMarginalMeansContainer <- jaspResults[["estimatedMarginalMeansContainer"]]

  # create/extract subsection container and meta-data
  if (!is.null(estimatedMarginalMeansContainer[["estimatedMarginalMeansContainer"]])) {
    tempContainer <- estimatedMarginalMeansContainer[["estimatedMarginalMeansContainer"]]
    tempMetaData  <- estimatedMarginalMeansContainer[["metaData"]]$object
  } else {
    # create the output container
    tempContainer <- createJaspContainer()
    tempContainer$position <- 1
    estimatedMarginalMeansContainer[["estimatedMarginalMeansContainer"]] <- tempContainer

    # create the container meta-data
    tempMetaDataState <- createJaspState()
    tempMetaDataState$dependOn(c("estimatedMarginalMeansEffectSize"))
    estimatedMarginalMeansContainer[["metaData"]] <- tempMetaDataState
    tempMetaData      <- list()
  }

  # extract the estimated marginal mean summary
  fit <- .robmaComputeMarginalMeans(.maExtractFit(jaspResults, options), options, conditional = FALSE)
  if (options[["conditionalEstimates"]])
    fitConditional <- .robmaComputeMarginalMeans(.maExtractFit(jaspResults, options), options, conditional = TRUE)

  # add an empty null table in case of an error
  if (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) {
    errorTable <- createJaspContainer(title = if (options[["conditionalEstimates"]]) gettext("Estimated Conditional Marginal Means") else gettext("Estimated Marginal Means"))
    tempContainer[["errorTable"]] <- errorTable
    return()
  }

  # extract a list of already existing variables / to be created variables
  existingVariables <- tempMetaData[["existingVariables"]]
  selectedVariables <- sapply(options[["estimatedMarginalMeansEffectSizeSelectedVariables"]], function(x) paste0(x[["variable"]], collapse = ":"))

  removeVariables <- setdiff(existingVariables, selectedVariables)
  addVariables    <- setdiff(selectedVariables, existingVariables)
  keepVariables   <- intersect(selectedVariables, existingVariables)

  # get information about the output type
  makeEstimatedMarginalMeans <- options[["estimatedMarginalMeansEffectSize"]]
  # TODO: potentially implemented later
  # makeContrasts <- FALSE options[[contrastsEffectSize]]

  # remove variables that are not selected anymore
  for (i in seq_along(removeVariables))
    tempContainer[[removeVariables[i]]] <- NULL

  # if no variables needs to be added, there is no need to reshuffle the order
  if ((length(addVariables) == 0 && length(existingVariables) == length(selectedVariables) && all(existingVariables == selectedVariables)) &&
      (!is.null(tempMetaData[["hasEstimatedMarginalMeans"]]) && tempMetaData[["hasEstimatedMarginalMeans"]] == makeEstimatedMarginalMeans) &&
      # (!is.null(tempMetaData[["hasContrasts"]])              && tempMetaData[["hasContrasts"]] == makeContrasts) &&
      (!is.null(tempMetaData[["selectedOptions"]]) && identical(tempMetaData[["selectedOptions"]], .robmaGetEstimatedMarginalMeansOptions(options)))
  )
    return()

  # add adjusted estimate if requested
  if (options[["estimatedMarginalMeansEffectSizeAddAdjustedEstimate"]] && is.null(tempContainer[["adjustedEstimate"]][["estimatedMarginalMeansTable"]]) && makeEstimatedMarginalMeans){
    tempVariableContainer <- createJaspContainer(title = gettext("Adjusted Estimate"))
    tempVariableContainer$position <- 0
    tempVariableContainer$dependOn("estimatedMarginalMeansEffectSizeAddAdjustedEstimate")
    tempContainer[["adjustedEstimate"]] <- tempVariableContainer
    .robmaEstimatedMarginalMeansTable(tempVariableContainer, fit, options, "", conditional = FALSE)
  }
  if (options[["estimatedMarginalMeansEffectSizeAddAdjustedEstimate"]] && is.null(tempContainer[["adjustedEstimate"]][["conditionalEstimateMarginalMeansTable"]]) && makeEstimatedMarginalMeans
      && options[["conditionalEstimates"]]){
    tempVariableContainer <- tempContainer[["adjustedEstimate"]]
    .robmaEstimatedMarginalMeansTable(tempVariableContainer, fitConditional, options, "", conditional = TRUE)
  }


  # reorder / add variables
  for (i in seq_along(selectedVariables)) {

    # get the variable container
    if (is.null(tempContainer[[selectedVariables[[i]]]])) {
      tempVariableContainer <- createJaspContainer(title = if (options[["conditionalEstimates"]]) gettextf("Conditional %1$s", selectedVariables[[i]]) else selectedVariables[[i]])
      tempContainer[[selectedVariables[[i]]]] <- tempVariableContainer
    } else {
      tempVariableContainer <- tempContainer[[selectedVariables[[i]]]]
    }

    # if output was already created, just reorder the position
    tempVariableContainer$position <- i

    # add the missing outputs
    if (makeEstimatedMarginalMeans && is.null(tempVariableContainer[["estimatedMarginalMeansTable"]]))
      .robmaEstimatedMarginalMeansTable(tempVariableContainer, fit, options, selectedVariables[[i]], conditional = FALSE)

    if (makeEstimatedMarginalMeans && is.null(tempVariableContainer[["conditionalEstimatedMarginalMeansTable"]]) && options[["conditionalEstimates"]])
      .robmaEstimatedMarginalMeansTable(tempVariableContainer, fitConditional, options, selectedVariables[[i]], conditional = TRUE)

    #if (makeContrasts && is.null(tempVariableContainer[["contrastsTable"]]))
    #  .maContrastsTable(tempVariableContainer, fit, options, selectedVariables[[i]])
  }

  # re-write information about existing variables
  estimatedMarginalMeansContainer[["metaData"]]$object <- list(
    existingVariables         = selectedVariables,
    hasEstimatedMarginalMeans = makeEstimatedMarginalMeans,
    # hasContrasts              = makeContrasts,
    selectedOptions           = .robmaGetEstimatedMarginalMeansOptions(options)
  )

  return()
}

.robmaEstimatedMarginalMeansTable        <- function(variableContainer, fit, options, selectedVariable, conditional) {

  estimatedMarginalMeansTable <- createJaspTable(if (conditional) gettext("Conditional Estimated Marginal Means") else gettext("Estimated Marginal Means"))
  estimatedMarginalMeansTable$position <- if (conditional) 2 else 1
  estimatedMarginalMeansTable$dependOn(c("estimatedMarginalMeansEffectSize", "estimatedMarginalMeansEffectSizeTestAgainst0", "transformEffectSize", "bayesFactorType",
                                         if (conditional) "conditionalEstimates"))
  variableContainer[[if (conditional) "conditionalEstimatedMarginalMeansTable" else "estimatedMarginalMeansTable"]] <- estimatedMarginalMeansTable

  # prepare table
  if (selectedVariable != "")
    estimatedMarginalMeansTable$addColumnInfo(name = "value", type = "string", title = gettext("Level"))
  .maAddSubgroupColumn(estimatedMarginalMeansTable, options)
  estimatedMarginalMeansTable$addColumnInfo(name = "mean",    type = "number", title = gettext("Mean"))
  estimatedMarginalMeansTable$addColumnInfo(name = "median",  type = "number", title = gettext("Median"))
  .maAddCiColumn(estimatedMarginalMeansTable, options)
  if (options[["estimatedMarginalMeansEffectSizeTestAgainst0"]])
    .robmaAddBfColumn(estimatedMarginalMeansTable, options)

  # get the estimate
  estimatedMarginalMeans <- .maSafeRbind(lapply(fit, function(x) {
    if (selectedVariable == "") {
      return(data.frame(x[x$value == "intercept",,drop=FALSE]))
    } else {
      return(data.frame(x[grep(selectedVariable, x$value),,drop=FALSE]))
    }
  }))
  # reorder by estimated marginal means estimate
  estimatedMarginalMeans <- .maSafeOrderAndSimplify(estimatedMarginalMeans, "value", options)


  # add footnotes
  if (selectedVariable == "" && !.robmaIsMetaregressionCentered(options) && options[["estimatedMarginalMeansEffectSizeTestAgainst0"]])
    estimatedMarginalMeansTable$addFootnote(gettext("The Bayes factor test for the adjusted estimate is not available for meta-regressions with non-centered parameteriazation."))
  estimatedMarginalMeansMessages <- .maEstimatedMarginalMeansMessages(options, "effectSize", anyNA(sapply(estimatedMarginalMeans[,colnames(estimatedMarginalMeans) %in% c("mean", "median", "lCi", "uCi", "lPi", "uPi")], anyNA)))
  for (i in seq_along(estimatedMarginalMeansMessages))
   estimatedMarginalMeansTable$addFootnote(estimatedMarginalMeansMessages[i])
  if (conditional)
    estimatedMarginalMeansTable$addFootnote(gettext("Conditional estimates are based on models assuming the presence of a given component."))
  if (options[["estimatedMarginalMeansEffectSizeTestAgainst0"]]) {
    estimatedMarginalMeansWarnings <- .robmaEstimatedMarginalMeansWarnings(fit, options, selectedVariable)
    for (i in seq_along(estimatedMarginalMeansWarnings))
      estimatedMarginalMeansTable$addFootnote(estimatedMarginalMeansWarnings[i])
  }

  # set data
  estimatedMarginalMeansTable$setData(estimatedMarginalMeans)
  estimatedMarginalMeansTable$showSpecifiedColumnsOnly <- TRUE

  return()
}

.robmaTermTests                     <- function(fit, options, selectedVariable) {
  termTests <- .robmaRowTermTests(fit, options)
  termTests <- termTests[grep(selectedVariable, termTests$term),, drop = FALSE]
  return(termTests)
}

.robmaComputeMarginalMeans          <- function(fit, options, conditional) {

  # extracts the already pre-computed marginal means from the fit object
  object <- list()
  for (i in seq_along(fit)) {
    object[[names(fit)[i]]] <- attr(fit[[i]], "marginalSummary")
  }

  for (i in seq_along(object)) {
    if (!jaspBase::isTryError(object[[i]])) {
      estimate <- object[[i]][[if (conditional) "estimates_conditional" else "estimates"]]
      # transform the BF
      if (options[["bayesFactorType"]] == "BF01") {
        estimate[["inclusion_BF"]] <- 1 / estimate[["inclusion_BF"]]
      } else if (options[["bayesFactorType"]] == "logBF10") {
        estimate[["inclusion_BF"]] <- log(estimate[["inclusion_BF"]])
      }
      # add parameter names
      colnames(estimate) <- c("mean", "median", "lCi", "uCi", "bf")
      estimate$value     <- rownames(estimate)
      estimate$subgroup  <- attr(fit[[i]], "subgroup")

      # apply effect size transformation
      if (options[["transformEffectSize"]] != "none")
        estimate[,c("mean", "median", "lCi", "uCi")] <- do.call(
          .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
          list(estimate[,c("mean", "median", "lCi", "uCi")]))

      object[[i]] <- estimate
    } else {
      object[[i]] <- NULL
    }
  }

  return(object)
}

.robmaComputeMarginalMeansVariable  <- function(fit, options, selectedVariable, conditional) {

  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  computedMarginalMeans <- data.frame(.robmaComputeMarginalMeans(fit, options, conditional)[[1]])
  computedMarginalMeans <- computedMarginalMeans[grep(selectedVariable, computedMarginalMeans$value),, drop = FALSE]

  computedMarginalMeans$value <- gsub(selectedVariable, "", rownames(computedMarginalMeans))
  computedMarginalMeans$value <- gsub("[", "", gsub("]", "", computedMarginalMeans$value, fixed = TRUE), fixed = TRUE)
  computedMarginalMeans$est   <- computedMarginalMeans$mean

  if (!options[["confidenceIntervals"]])
    computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% c("lCi", "uCi"), drop = FALSE]

  return(computedMarginalMeans)
}

.robmaMakeBubblePlotDataset            <- function(fit, options) {

  # extract options
  separateLines        <- unlist(options[["bubblePlotSeparateLines"]])
  separatePlots        <- unlist(options[["bubblePlotSeparatePlots"]])
  selectedVariable     <- options[["bubblePlotSelectedVariable"]][[1]][["variable"]]
  selectedVariableType <- options[["predictors.types"]][options[["predictors"]] == selectedVariable]
  remainingVariables   <- setdiff(fit[["add_info"]][["predictors"]], c(separateLines, separatePlots, selectedVariable))
  dataset              <- attr(fit, "dataset")

  # create a range of values for continuous predictors to plot the trend but use lvls for factors
  if (selectedVariableType == "scale") {

    xRange <- range(jaspGraphs::getPrettyAxisBreaks(range(dataset[[selectedVariable]])))
    trendSequence <- seq(xRange[1], xRange[2], length.out =  101)

    predictorMatrixEffectSize <- .robmaGetMarginalMeansPredictorMatrix(
      fit                = fit,
      options            = options,
      selectedVariables  = c(separateLines, separatePlots),
      sdFactor           = options[["bubblePlotSdFactorCovariates"]],
      trendVarible       = selectedVariable,
      trendSequence      = trendSequence,
      parameter          = "effectSize"
    )

  } else if (selectedVariableType == "nominal") {

    predictorMatrixEffectSize <- .robmaGetMarginalMeansPredictorMatrix(
      fit                = fit,
      options            = options,
      selectedVariables  = c(selectedVariable, separateLines, separatePlots),
      sdFactor           = options[["bubblePlotSdFactorCovariates"]],
      parameter          = "effectSize"
    )

  }

  predictions <- BayesTools::JAGS_evaluate_formula(
    fit         = fit$model$fit,
    formula     = fit$formula,
    parameter   = "mu",
    data        = as.data.frame(predictorMatrixEffectSize),
    prior_list  = attr(fit$model$fit, "prior_list")
  )

  ### modify and rename selectedGrid
  selectedGrid  <- attr(predictorMatrixEffectSize, "selectedGrid")
  selectedGrid$selectedVariable <- selectedGrid[,selectedVariable]
  # deal with continuous variables dichotomization
  selectedGrid     <- .maDichotomizeVariablesLevels(selectedGrid, c(separateLines, separatePlots), options)
  continuousLevels <- attr(selectedGrid, "continuousLevels")
  # collapse factor levels if multiple selected
  selectedGrid <- .maMergeVariablesLevels(selectedGrid, separateLines, "separateLines")
  selectedGrid <- .maMergeVariablesLevels(selectedGrid, separatePlots, "separatePlots")
  # remove original names
  selectedGrid <- selectedGrid[,setdiff(names(selectedGrid), c(selectedVariable, separateLines, separatePlots)),drop = FALSE]

  ### modify marginal means
  # average across the remaining variables
  for (i in seq_along(remainingVariables)) {
    if (options[["predictors.types"]][options[["predictors"]] == remainingVariables[i]] == "nominal") {
      predictionsSplit <- lapply(unique(dataset[[remainingVariables[i]]]), function(x) {
        predictions[predictorMatrixEffectSize[[remainingVariables[i]]] == x, , drop = FALSE]
      })
      predictions <- matrix(rowMeans(do.call(cbind, lapply(predictionsSplit, as.vector))), nrow = nrow(predictionsSplit[[1]]), ncol = ncol(predictionsSplit[[1]]))
      predictorMatrixEffectSize <- lapply(unique(dataset[[remainingVariables[i]]]), function(x) {
        predictorMatrixEffectSize[predictorMatrixEffectSize[[remainingVariables[i]]] == x, , drop = FALSE]
      })[[1]]
    }
  }
  # compute the estimate and standard error
  computedMarginalMeans <- data.frame(
    est = apply(predictions, 1, mean),
    lCi = apply(predictions, 1, quantile, prob = 0.5 - options[["confidenceIntervalsLevel"]] / 2),
    uCi = apply(predictions, 1, quantile, prob = 0.5 + options[["confidenceIntervalsLevel"]] / 2),
    lPi = NA,
    uPi = NA
  )

  ### merge and add attributes
  dfPlot <- cbind.data.frame(selectedGrid, computedMarginalMeans)

  attr(dfPlot, "selectedVariable")     <- selectedVariable
  attr(dfPlot, "selectedVariableType") <- selectedVariableType
  attr(dfPlot, "separateLines")    <- paste(separateLines, collapse = " | ")
  attr(dfPlot, "separatePlots")    <- paste(separatePlots, collapse = " | ")
  attr(dfPlot, "variablesLines")   <- separateLines
  attr(dfPlot, "variablesPlots")   <- separatePlots
  attr(dfPlot, "continuousLevels") <- continuousLevels[!sapply(continuousLevels, is.null)]
  attr(dfPlot, "xRange")           <- if (selectedVariableType == "scale") xRange

  return(dfPlot)
}

.robmaGetMarginalMeansPredictorMatrix  <- function(fit, options, selectedVariables, trendVarible = NULL, trendSequence = NULL, sdFactor, parameter) {

  dataset <- attr(fit, "dataset")
  priors  <- fit[["priors"]][["terms"]]
  variablesContinuous <- options[["predictors"]][options[["predictors.types"]] == "scale"]
  variablesFactors    <- options[["predictors"]][options[["predictors.types"]] == "nominal"]

  # extract the corresponding formula
  formula      <- fit$formula
  hasIntercept <- TRUE

  # extract the used variables
  terms     <- attr(terms(formula, data = fit[["data"]]), "term.labels")
  variables <- terms[!grepl(":", terms)]

  # average across remaining variables
  remainingVariables <- setdiff(variables, c(selectedVariables, trendVarible))

  ### create model matrix for the remaining predictors
  # (use all factors for levels to average out the predictor matrix later)
  predictorsRemaining <- list()
  for (i in seq_along(remainingVariables)) {
    if (remainingVariables[[i]] %in% variablesFactors) {
      predictorsRemaining[[remainingVariables[i]]] <- factor(levels(dataset[[remainingVariables[[i]]]]), levels = levels(dataset[[remainingVariables[[i]]]]))
      contrasts(predictorsRemaining[[remainingVariables[i]]]) <- contrasts(dataset[[remainingVariables[[i]]]])
    } else if (remainingVariables[[i]] %in% variablesContinuous) {
      predictorsRemaining[[remainingVariables[i]]] <- mean(dataset[[remainingVariables[[i]]]])
    }
  }

  # create complete model matrices including the specified variable
  predictorsSelected <- list()
  predictorsSelectedNames <- list()
  if (length(selectedVariables) > 0) {
    for (selectedVariable in selectedVariables) {
      if (selectedVariable %in% variablesFactors) {
        predictorsSelected[[selectedVariable]] <- factor(levels(dataset[[selectedVariable]]), levels = levels(dataset[[selectedVariable]]))
        predictorsSelectedNames[[selectedVariable]] <- levels(dataset[[selectedVariable]])
        contrasts(predictorsSelected[[selectedVariable]]) <- contrasts(dataset[[selectedVariable]])
      } else if (selectedVariable %in% variablesContinuous) {
        predictorsSelected[[selectedVariable]] <- c(
          mean(dataset[[selectedVariable]]) - sdFactor * sd(dataset[[selectedVariable]]),
          mean(dataset[[selectedVariable]]),
          mean(dataset[[selectedVariable]]) + sdFactor * sd(dataset[[selectedVariable]])
        )
        predictorsSelectedNames[[selectedVariable]] <- c(
          gettextf("Mean - %1$sSD", sdFactor),
          gettext("Mean"),
          gettextf("Mean + %1$sSD", sdFactor)
        )
      }
    }
  }

  # create model matrix for the trend variable
  if (length(trendVarible) != 0) {
    predictorsSelected[[trendVarible]] <- trendSequence
  }

  # add the specified variable
  predictorsSelectedGrid      <- expand.grid(predictorsSelected)
  predictorsSelectedGridNames <- expand.grid(predictorsSelectedNames)
  outMatrix <- do.call(rbind, lapply(1:nrow(predictorsSelectedGrid), function(i) {
    expand.grid(c(predictorsRemaining,  predictorsSelectedGrid[i,,drop = FALSE]))
  }))

  # standardize the continuous variables
  variablesInfo <- attr(fit[["data"]][["predictors"]], "variables_info")
  for (i in seq_along(variablesInfo)) {
    if (variablesInfo[[i]][["type"]] == "continuous") {
      outMatrix[[names(variablesInfo)[[i]]]] <- (outMatrix[[names(variablesInfo)[[i]]]] - variablesInfo[[i]][["mean"]]) / variablesInfo[[i]][["sd"]]
    }
  }

  # selected variables grid
  attr(outMatrix, "selectedGrid") <- predictorsSelectedGrid
  attr(outMatrix, "selectedGridNames") <- predictorsSelectedGridNames

  # add remaining variables
  attr(outMatrix, "variable") <- c(selectedVariables, trendVarible)

  for (selectedVariable in selectedVariables) {
    if (selectedVariable %in% variablesFactors) {
      attr(outMatrix, selectedVariable) <- predictorsSelected[[selectedVariable]]
    } else if (selectedVariable %in% variablesContinuous) {
      attr(outMatrix, selectedVariable) <- c(
        gettextf("Mean - %1$sSD", sdFactor),
        gettext("Mean"),
        gettextf("Mean + %1$sSD", sdFactor))
    }
  }

  if (length(trendVarible) != 0) {
    attr(outMatrix, "trend") <- trendVarible
    attr(outMatrix, "trend") <- trendSequence
  }

  return(outMatrix)
}
