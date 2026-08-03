# Classical meta-analysis estimated marginal means and contrasts.
#
# Contains EMM/contrast builders, computations, option handling, and messages.

# Output orchestration and tables ----

.maEstimatedMarginalMeansAndContrasts    <- function(jaspResults, options) {

  # so, this section is a bit complicated -- all in order to prevent updating of all subcomponents once a new variable is added/removed
  # the main container contains effect size and heterogeneity subcontainers, which contain variable containers with the actual output tables
  # updating of the subtables is skipped unless one of the options specified here is checked:
  # .maGetEstimatedMarginalMeansAndContrastsOptions()

  # check whether the section should be created at all
  isReadyEffectSize    <- .maIsMetaregressionEffectSize(options) &&
                          (length(options[["estimatedMarginalMeansEffectSizeSelectedVariables"]])    > 0 || options[["estimatedMarginalMeansEffectSizeAddAdjustedEstimate"]])    &&
                          (options[["estimatedMarginalMeansEffectSize"]]    || options[["contrastsEffectSize"]])
  isReadyHeterogeneity <- .maIsMetaregressionHeterogeneity(options) &&
                          (length(options[["estimatedMarginalMeansHeterogeneitySelectedVariables"]]) > 0 || options[["estimatedMarginalMeansHeterogeneityAddAdjustedEstimate"]]) &&
                          (options[["estimatedMarginalMeansHeterogeneity"]] || options[["contrastsHeterogeneity"]])

  if (!isReadyEffectSize && !isReadyHeterogeneity) {
    # remove section if exists
    if (!is.null(jaspResults[["estimatedMarginalMeansAndContrastsContainer"]]))
      jaspResults[["estimatedMarginalMeansAndContrastsContainer"]] <- NULL

    return()
  }

  # create/extract section otherwise
  if (!is.null(jaspResults[["estimatedMarginalMeansAndContrastsContainer"]])) {
    estimatedMarginalMeansAndContrastsContainer <- jaspResults[["estimatedMarginalMeansAndContrastsContainer"]]
  } else {
    # create the output container
    estimatedMarginalMeansAndContrastsContainer <- createJaspContainer(gettext("Estimated Marginal Means and Contrasts Summary"))
    estimatedMarginalMeansAndContrastsContainer$dependOn(c(.maDependencies, "confidenceIntervals", "confidenceIntervalsLevel", "includeFullDatasetInSubgroupAnalysis"))
    estimatedMarginalMeansAndContrastsContainer$position <- 4
    jaspResults[["estimatedMarginalMeansAndContrastsContainer"]] <- estimatedMarginalMeansAndContrastsContainer
  }

  # fill the section with EMM/C tables for each variables for the effect size / heterogeneity
  if (isReadyEffectSize)
    .maEstimatedMarginalMeansAndContrastsFun(jaspResults, options, parameter = "effectSize")

  if (isReadyHeterogeneity)
    .maEstimatedMarginalMeansAndContrastsFun(jaspResults, options, parameter = "heterogeneity")

  return()
}

.maEstimatedMarginalMeansAndContrastsFun <- function(jaspResults, options, parameter = "effectSize") {

  # get the corresponding container
  estimatedMarginalMeansAndContrastsContainer <- jaspResults[["estimatedMarginalMeansAndContrastsContainer"]]

  # create/extract subsection container and meta-data
  if (!is.null(estimatedMarginalMeansAndContrastsContainer[[parameter]])) {
    tempContainer <- estimatedMarginalMeansAndContrastsContainer[[parameter]]
    tempMetaData  <- estimatedMarginalMeansAndContrastsContainer[[paste0(parameter, "MetaData")]]$object
  } else {
    # create the output container
    tempContainer <- createJaspContainer()
    tempContainer$position <- switch(
      parameter,
      effectSize    = 1,
      heterogeneity = 2
    )
    estimatedMarginalMeansAndContrastsContainer[[parameter]] <- tempContainer

    # create the container meta-data
    tempMetaDataState <- createJaspState()
    tempMetaDataState$dependOn(c(
      if (parameter == "effectSize") c("estimatedMarginalMeansEffectSize", "contrastsEffectSize")
      else if (parameter == "heterogeneity") c("estimatedMarginalMeansHeterogeneity", "contrastsHeterogeneity")
    ))
    estimatedMarginalMeansAndContrastsContainer[[paste0(parameter, "MetaData")]] <- tempMetaDataState
    tempMetaData      <- list()
  }

  fit <- .maExtractFit(jaspResults, options)

  # add an empty null table in case of an error
  if (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) {
    errorTable <- createJaspContainer(title = gettext("Estimated Marginal Means / Contrasts"))
    tempContainer[["errorTable"]] <- errorTable
    return()
  }

  # extract a list of already existing variables / to be created variables
  existingVariables <- tempMetaData[["existingVariables"]]
  selectedVariables <- sapply(switch(
    parameter,
    effectSize    = options[["estimatedMarginalMeansEffectSizeSelectedVariables"]],
    heterogeneity = options[["estimatedMarginalMeansHeterogeneitySelectedVariables"]]
  ), function(x) paste0(x[["variable"]], collapse = ":"))

  removeVariables <- setdiff(existingVariables, selectedVariables)
  addVariables    <- setdiff(selectedVariables, existingVariables)
  keepVariables   <- intersect(selectedVariables, existingVariables)

  # get information about the output type
  makeEstimatedMarginalMeans <- options[[switch(
    parameter,
    effectSize    = "estimatedMarginalMeansEffectSize",
    heterogeneity = "estimatedMarginalMeansHeterogeneity")]]
  makeContrasts <- options[[switch(
    parameter,
    effectSize    = "contrastsEffectSize",
    heterogeneity = "contrastsHeterogeneity")]]

  # remove variables that are not selected anymore
  for (i in seq_along(removeVariables))
    tempContainer[[removeVariables[i]]] <- NULL

  # if no variables needs to be added, there is no need to reshuffle the order
  if ((length(addVariables) == 0 && length(existingVariables) == length(selectedVariables) && all(existingVariables == selectedVariables)) &&
      (!is.null(tempMetaData[["hasEstimatedMarginalMeans"]]) && tempMetaData[["hasEstimatedMarginalMeans"]] == makeEstimatedMarginalMeans) &&
      (!is.null(tempMetaData[["hasContrasts"]])              && tempMetaData[["hasContrasts"]] == makeContrasts) &&
      (!is.null(tempMetaData[["selectedOptions"]]) && identical(tempMetaData[["selectedOptions"]], .maGetEstimatedMarginalMeansAndContrastsOptions(options)))
  )
    return()

  # add adjusted estimate if requested
  adjustedEstimateOption <- switch(
    parameter,
    effectSize    = "estimatedMarginalMeansEffectSizeAddAdjustedEstimate",
    heterogeneity = "estimatedMarginalMeansHeterogeneityAddAdjustedEstimate"
  )

  if (options[[adjustedEstimateOption]] && is.null(tempContainer[["adjustedEstimate"]][["estimatedMarginalMeansTable"]]) && makeEstimatedMarginalMeans){
    tempVariableContainer <- createJaspContainer(title = sprintf(
      "Adjusted Estimate%1$s",
      if (.maIsMetaregressionHeterogeneity(options)) switch(parameter, effectSize = gettext(" (Effect Size)"), heterogeneity = gettext(" (Heterogeneity)"))
      else ""
    ))
    tempVariableContainer$position <- 0
    tempVariableContainer$dependOn(adjustedEstimateOption)
    tempContainer[["adjustedEstimate"]] <- tempVariableContainer
    .maEstimatedMarginalMeansTable(tempVariableContainer, fit, options, "", parameter)
  }

  # reorder / add variables
  for (i in seq_along(selectedVariables)) {

    # get the variable container
    if (is.null(tempContainer[[selectedVariables[[i]]]])) {
      tempVariableContainer <- createJaspContainer(title = sprintf(
        "%1$s%2$s",
        gsub(":", jaspBase::interactionSymbol, selectedVariables[[i]]),
        if (.maIsMetaregressionHeterogeneity(options)) switch(parameter, effectSize = gettext(" (Effect Size)"), heterogeneity = gettext(" (Heterogeneity)"))
        else ""
      ))
      tempContainer[[selectedVariables[[i]]]] <- tempVariableContainer
    } else {
      tempVariableContainer <- tempContainer[[selectedVariables[[i]]]]
    }

    # if output was already created, just reorder the position
    tempVariableContainer$position <- i

    # add the missing outputs
    if (makeEstimatedMarginalMeans && is.null(tempVariableContainer[["estimatedMarginalMeansTable"]]))
      .maEstimatedMarginalMeansTable(tempVariableContainer, fit, options, selectedVariables[[i]], parameter)

    if (makeContrasts && is.null(tempVariableContainer[["contrastsTable"]]))
      .maContrastsTable(tempVariableContainer, fit, options, selectedVariables[[i]], parameter)
  }

  # re-write information about existing variables
  estimatedMarginalMeansAndContrastsContainer[[paste0(parameter, "MetaData")]]$object <- list(
    existingVariables         = selectedVariables,
    hasEstimatedMarginalMeans = makeEstimatedMarginalMeans,
    hasContrasts              = makeContrasts,
    selectedOptions           = .maGetEstimatedMarginalMeansAndContrastsOptions(options)
  )

  return()
}

.maEstimatedMarginalMeansTable           <- function(variableContainer, fit, options, selectedVariable, parameter = "effectSize") {

  estimatedMarginalMeansTable <- createJaspTable(if (selectedVariable == "") gettext("Adjusted Estimate") else gettext("Estimated Marginal Means"))
  estimatedMarginalMeansTable$position <- 1
  estimatedMarginalMeansTable$showSpecifiedColumnsOnly <- TRUE
  estimatedMarginalMeansTable$dependOn(c(switch(
    parameter,
    effectSize    = c("estimatedMarginalMeansEffectSize", "estimatedMarginalMeansEffectSizeSdFactorCovariates", "estimatedMarginalMeansEffectSizeTestAgainst",
                      "estimatedMarginalMeansEffectSizeTestAgainstValue", "transformEffectSize", "predictionIntervals", "standardErrors"),
    heterogeneity = c("estimatedMarginalMeansHeterogeneity", "estimatedMarginalMeansHeterogeneityTransformation", "estimatedMarginalMeansHeterogeneitySdFactorCovariates")
  )))
  variableContainer[["estimatedMarginalMeansTable"]] <- estimatedMarginalMeansTable

  # prepare table
  if (selectedVariable != "")
    estimatedMarginalMeansTable$addColumnInfo(name = "value",     type = "string", title = gettext("Level"))
  .maAddSubgroupColumn(estimatedMarginalMeansTable, options)
  estimatedMarginalMeansTable$addColumnInfo(name = "est",       type = "number", title = gettext("Estimate"))
  if (parameter == "effectSize")
    .maAddSeColumn(estimatedMarginalMeansTable, options)
  .maAddCiColumn(estimatedMarginalMeansTable, options)
  if (parameter == "effectSize") {
    .maAddPiColumn(estimatedMarginalMeansTable, options)
    if (options[["predictionIntervals"]] && .mammHasMultipleHeterogeneities(options, canAddOutput = TRUE)) {
      for (colName in .mammExtractTauLevelNamesList(fit)) {
        estimatedMarginalMeansTable$addColumnInfo(name = colName, title = colName, type = .maGetVariableColumnType(colName, options), overtitle = gettext("Heterogeneity Level"))
      }
    }
    if (options[["estimatedMarginalMeansEffectSizeTestAgainst"]]) {
      estimatedMarginalMeansTable$addColumnInfo(name = "stat",  type = "number", title = if(.maIsMetaregressionFtest(options)) gettext("t") else gettext("z"))
      if (.maIsMetaregressionFtest(options))
        estimatedMarginalMeansTable$addColumnInfo(name = "df",  type = "number", title = gettext("df"))
      estimatedMarginalMeansTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))
    }
  }

  # get the estimate
  estimatedMarginalMeans <- .maSafeRbind(lapply(fit, .maComputeMarginalMeansVariable,
    options          = options,
    selectedVariable = if (selectedVariable == "") "" else strsplit(selectedVariable, ":")[[1]],
    testAgainst      = options[["estimatedMarginalMeansEffectSizeTestAgainstValue"]],
    parameter        = parameter
  ))

  # reorder by estimated marginal means estimate
  estimatedMarginalMeans <- .maSafeOrderAndSimplify(estimatedMarginalMeans, "value", options)

  # set data
  estimatedMarginalMeansTable$setData(estimatedMarginalMeans)

  # add footnotes
  estimatesContainNA <- !is.null(estimatedMarginalMeans) && anyNA(sapply(
    estimatedMarginalMeans[, colnames(estimatedMarginalMeans) %in% c("est", "lCi", "uCi", "lPi", "uPi"), drop = FALSE],
    anyNA
  ))
  estimatedMarginalMeansMessages <- .maEstimatedMarginalMeansMessages(options, parameter, estimatesContainNA)
  for (i in seq_along(estimatedMarginalMeansMessages))
    estimatedMarginalMeansTable$addFootnote(estimatedMarginalMeansMessages[i])

  return()
}

.maContrastsTable                        <- function(variableContainer, fit, options, selectedVariable, parameter = "effectSize") {

  contrastsTable <- createJaspTable(gettext("Contrasts"))
  contrastsTable$position <- 1
  contrastsTable$showSpecifiedColumnsOnly <- TRUE
  contrastsTable$dependOn(switch(
    parameter,
    effectSize    = c("contrastsEffectSize", "contrastsEffectSizePValueAdjustment", "predictionIntervals", "transformEffectSize", "standardErrors"),
    heterogeneity = c("contrastsHeterogeneity", "contrastsHeterogeneityPValueAdjustment", "estimatedMarginalMeansHeterogeneityTransformation")
  ))
  variableContainer[["contrastsTable"]] <- contrastsTable

  # prepare table
  contrastsTable$addColumnInfo(name = "comparison", type = "string", title = gettext("Comparison"))
  .maAddSubgroupColumn(contrastsTable, options)
  contrastsTable$addColumnInfo(name = "est",        type = "number", title = gettext("Estimate"))
  if (parameter == "effectSize")
    .maAddSeColumn(contrastsTable, options)
  .maAddCiColumn(contrastsTable, options)
  if (parameter == "effectSize") {
    .maAddPiColumn(contrastsTable, options)
    # if (options[["predictionIntervals"]] && .mammHasMultipleHeterogeneities(options, canAddOutput = TRUE)) {
    #   TODO?
    #   for (colName in .mammExtractTauLevelNamesList(fit)) {
    #   contrastsTable$addColumnInfo(name = colName, title = colName, type = .maGetVariableColumnType(colName, options), overtitle = gettext("Heterogeneity Level"))
    #   }
    # }
  }
  contrastsTable$addColumnInfo(name = "stat",  type = "number", title = if(.maIsMetaregressionFtest(options)) gettext("t") else gettext("z"))
  if (.maIsMetaregressionFtest(options))
    contrastsTable$addColumnInfo(name = "df",  type = "number", title = gettext("df"))
  contrastsTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))

  # get the estimate
  contrasts <- .maSafeRbind(lapply(fit, .maComputeContrastVariable,
    options          = options,
    selectedVariable = if (selectedVariable == "") "" else strsplit(selectedVariable, ":")[[1]],
    parameter        = parameter
  ))

  # reorder by estimated marginal means estimate
  contrasts <- .maSafeOrderAndSimplify(contrasts, "comparison", options)

  # set data
  contrastsTable$setData(contrasts)

  # add footnotes
  contrastsMessages <- .macontrastsMessages(options, parameter)
  for (i in seq_along(contrastsMessages))
    contrastsTable$addFootnote(contrastsMessages[i])

  if (.maIsGLMM(options))
    contrastsTable$addFootnote(gettext("Contrast tests based on Wald-type z-tests."))

  return()
}

# Computations ----

.maGetMarginalMeansPredictorMatrix <- function(fit, options, selectedVariables, trendVarible = NULL, trendSequence = NULL, sdFactor, parameter, dropIntercept = TRUE) {

  dataset <- attr(fit, "dataset")
  variablesContinuous <- options[["predictors"]][options[["predictors.types"]] == "scale"]
  variablesFactors    <- options[["predictors"]][options[["predictors.types"]] == "nominal"]

  # extract the corresponding formula
  formula <- switch(
    parameter,
    effectSize    = fit[["formula.mods"]],
    heterogeneity = fit[["formula.scale"]]
  )
  hasIntercept <- switch(
    parameter,
    effectSize    = options[["effectSizeModelIncludeIntercept"]],
    heterogeneity = options[["heterogeneityModelIncludeIntercept"]]
  )

  # extract the used variables
  terms     <- attr(terms(formula, data = fit[["data"]]), "term.labels")
  variables <- unique(unlist(sapply(terms, strsplit, split = ":")))

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

  # add the specified variable and pool across the combinations of the remaining values
  if (length(selectedVariables) == 1 && selectedVariables == "") {
    # empty string creates overall adjusted estimate
    outMatrix <- t(colMeans(model.matrix(formula, data = expand.grid(predictorsRemaining))))
    predictorsSelectedGridNames <- matrix("")
  } else {
    predictorsSelectedGrid      <- expand.grid(predictorsSelected)
    predictorsSelectedGridNames <- expand.grid(predictorsSelectedNames)
    outMatrix <- do.call(rbind, lapply(1:nrow(predictorsSelectedGrid), function(i) {
      colMeans(model.matrix(formula, data = expand.grid(c(predictorsRemaining,  predictorsSelectedGrid[i,,drop = FALSE]))))
    }))
  }

  # remove entries corresponding to omitted coefficients
  if (parameter == "effectSize" && !is.null(fit$coef.na) && any(fit$coef.na)) {
    outMatrix <- outMatrix[, !fit$coef.na, drop=FALSE]
  } else if (parameter == "heterogeneity" && !is.null(fit$coef.na.Z) && any(fit$coef.na.Z)) {
    outMatrix <- outMatrix[, !fit$coef.na.Z, drop=FALSE]
  }

  if (hasIntercept && dropIntercept)
    outMatrix <- outMatrix[, -1, drop=FALSE]

  # keep information about the variable and levels
  if (length(selectedVariables) == 1 && selectedVariables == "") {

    # add intercept
    attr(outMatrix, "variable") <- gettext("Adjusted estimate")
    attr(outMatrix, gettext("Adjusted estimate")) <- ""
    attr(outMatrix, "selectedGridNames") <- predictorsSelectedGridNames

  } else {

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
  }

  if (length(trendVarible) != 0) {
    attr(outMatrix, "trend") <- trendVarible
    attr(outMatrix, "trend") <- trendSequence
  }

  return(outMatrix)

}

.maComputeMarginalMeansVariable    <- function(fit, options, selectedVariable, testAgainst = 0, parameter) {

  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  if (parameter == "effectSize") {

    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
      parameter         = "effectSize"
    )

    if (.maIsMetaregressionHeterogeneity(options)) {

      predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
        fit               = fit,
        options           = options,
        selectedVariables = selectedVariable,
        sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
        parameter         = "heterogeneity"
      )
      computedMarginalMeans <- predict(
        fit,
        newmods  = predictorMatrixEffectSize,
        newscale = predictorMatrixHeterogeneity,
        level    = 100 * options[["confidenceIntervalsLevel"]]
      )
    } else {

      if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {
        tauLevelsMatrix            <- .mammExtractTauLevels(fit)
        tempPredictorMatrixRepeats <- rep(1:nrow(predictorMatrixEffectSize), each = nrow(tauLevelsMatrix)) # repeat the predictors for each tau level
        attr(predictorMatrixEffectSize, attr(predictorMatrixEffectSize, "variable")) <- attr(predictorMatrixEffectSize, attr(predictorMatrixEffectSize, "variable"))[tempPredictorMatrixRepeats]
        computedMarginalMeans <- predict(
          fit,
          newmods = predictorMatrixEffectSize[tempPredictorMatrixRepeats,,drop=FALSE],
          level   = 100 * options[["confidenceIntervalsLevel"]],
          tau2.levels   = if (is.null(dim(predictorMatrixEffectSize))) tauLevelsMatrix[["tau2.levels"]]   else do.call(rbind, lapply(1:nrow(predictorMatrixEffectSize), function(i) tauLevelsMatrix))[["tau2.levels"]],
          gamma2.levels = if (is.null(dim(predictorMatrixEffectSize))) tauLevelsMatrix[["gamma2.levels"]] else do.call(rbind, lapply(1:nrow(predictorMatrixEffectSize), function(i) tauLevelsMatrix))[["gamma2.levels"]]
        )
      } else {
        computedMarginalMeans <- predict(
          fit,
          newmods = predictorMatrixEffectSize,
          level   = 100 * options[["confidenceIntervalsLevel"]]
        )
      }
    }

    if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {
      tauLevels <- list(
        computedMarginalMeans[["tau2.level"]],
        computedMarginalMeans[["gamma2.level"]]
      )
      tauLevels           <- do.call(cbind.data.frame, tauLevels[!sapply(tauLevels, is.null)])
      colnames(tauLevels) <- .mammExtractTauLevelNames(fit)
    }


    # compute test against specified value
    if (.maIsMetaregressionFtest(options)) {

      # extract degrees of freedom (rma.glmm predict may not have ddf)
      tempDf                     <- if (!is.null(computedMarginalMeans$ddf)) computedMarginalMeans$ddf else .maExtractDdf(fit)[1]
      computedMarginalMeans      <- .maExtractAndFormatPrediction(computedMarginalMeans)
      computedMarginalMeans$df   <- tempDf
      computedMarginalMeans$stat <- (computedMarginalMeans$est - testAgainst)  / computedMarginalMeans$se
      computedMarginalMeans$pval <- 2 * pt(abs(computedMarginalMeans$stat), computedMarginalMeans$df, lower.tail = FALSE)

    } else {

      computedMarginalMeans      <- .maExtractAndFormatPrediction(computedMarginalMeans)
      computedMarginalMeans$stat <- (computedMarginalMeans$est - testAgainst)  / computedMarginalMeans$se
      computedMarginalMeans$pval <- 2 * pnorm(abs(computedMarginalMeans$stat), lower.tail = FALSE)

    }

    # apply effect size transformation
    if (options[["transformEffectSize"]] != "none")
      computedMarginalMeans[,c("est", "lCi", "uCi", "lPi", "uPi")] <- do.call(
        .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
        list(computedMarginalMeans[,c("est", "lCi", "uCi", "lPi", "uPi")]))

    # create full data frame
    computedMarginalMeans <- data.frame(
      "variable" = paste0(attr(predictorMatrixEffectSize, "variable"), collapse = jaspBase::interactionSymbol),
      "value"    = apply(attr(predictorMatrixEffectSize, "selectedGridNames"), 1, paste0, collapse = ", "),
      computedMarginalMeans
    )

  } else if (parameter == "heterogeneity") {

    predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansHeterogeneitySdFactorCovariates"]],
      parameter         = "heterogeneity"
    )

    computedMarginalMeans <- predict(
      fit,
      newscale = predictorMatrixHeterogeneity,
      level    = 100 * options[["confidenceIntervalsLevel"]]
    )

    computedMarginalMeans <- .maExtractAndFormatPrediction(computedMarginalMeans)


    # apply link transform
    if (options[["heterogeneityModelLink"]] == "log") {
      computedMarginalMeans <- exp(computedMarginalMeans)
    }

    # apply tau / tau2 transform
    if (options[["estimatedMarginalMeansHeterogeneityTransformation"]] == "tau")
      computedMarginalMeans <- sqrt(computedMarginalMeans)

    # create full data frame
    computedMarginalMeans <- data.frame(
      "variable" = paste0(attr(predictorMatrixHeterogeneity, "variable"), collapse = jaspBase::interactionSymbol),
      "value"    = apply(attr(predictorMatrixHeterogeneity, "selectedGridNames"), 1, paste0, collapse = ", "),
      computedMarginalMeans
    )
  }

  # return the tau levels
  if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]])
    computedMarginalMeans <- cbind(computedMarginalMeans, tauLevels)

  computedMarginalMeans$subgroup <- attr(fit, "subgroup")

  return(computedMarginalMeans)
}

.maComputeContrastVariable         <- function(fit, options, selectedVariable, parameter) {

  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  if (parameter == "effectSize") {

    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
      parameter         = "effectSize",
      dropIntercept     = FALSE
    )

    selectedVariableLevels   <- apply(attr(predictorMatrixEffectSize, "selectedGridNames"), 1, paste0, collapse = ", ")
    contrastMatrixEffectSize <- matrix(NA, nrow = nrow(predictorMatrixEffectSize) * (nrow(predictorMatrixEffectSize) - 1) / 2, ncol = ncol(predictorMatrixEffectSize))
    contrastComparisons      <- character(nrow(contrastMatrixEffectSize))

    thisContrast <- 1
    for (i in 1:length(selectedVariableLevels)){
      for (j in 1:length(selectedVariableLevels)){
        if (j > i) {
          contrastMatrixEffectSize[thisContrast,] <- predictorMatrixEffectSize[i,] - predictorMatrixEffectSize[j,]
          contrastComparisons[thisContrast]       <- paste0(selectedVariableLevels[i], " – ", selectedVariableLevels[j])
          thisContrast <- thisContrast + 1
        }
      }
    }

    if (.maIsMetaregressionHeterogeneity(options)) {

      predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
        fit               = fit,
        options           = options,
        selectedVariables = selectedVariable,
        sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
        parameter         = "heterogeneity"
      )
      contrastMatrixHeterogeneity <- matrix(NA, nrow = nrow(predictorMatrixEffectSize) * (nrow(predictorMatrixEffectSize) - 1) / 2, ncol = ncol(predictorMatrixHeterogeneity))

      thisContrast <- 1
      for (i in 1:length(selectedVariableLevels)){
        for (j in 1:length(selectedVariableLevels)){
          if (j > i) {
            contrastMatrixHeterogeneity[thisContrast,] <- predictorMatrixHeterogeneity[i,] - predictorMatrixHeterogeneity[j,]
            thisContrast <- thisContrast + 1
          }
        }
      }

      computedContrasts <- predict(
        fit,
        newmods  = contrastMatrixEffectSize,
        newscale = contrastMatrixHeterogeneity,
        level    = 100 * options[["confidenceIntervalsLevel"]]
      )
      if (.maIsGLMM(options)) {
        computedContrastsTests <- .maGlmmContrastTest(fit, X = contrastMatrixEffectSize, adjust = .maGetPValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]]))
      } else {
        computedContrastsTests <- anova(
          fit,
          X      = contrastMatrixEffectSize,
          adjust = .maGetPValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]])
        )
      }

    } else {

      if (FALSE){ # .mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {

        # # TODO?
        # tauLevelsMatrix            <- .mammExtractTauLevels(fit)
        # tempPredictorMatrixRepeats <- rep(1:nrow(predictorMatrixEffectSize), each = nrow(tauLevelsMatrix)) # repeat the predictors for each tau level
        # attr(predictorMatrixEffectSize, attr(predictorMatrixEffectSize, "variable")) <- attr(predictorMatrixEffectSize, attr(predictorMatrixEffectSize, "variable"))[tempPredictorMatrixRepeats]
        # computedMarginalMeans <- predict(
        #   fit,
        #   newmods = predictorMatrixEffectSize[tempPredictorMatrixRepeats,,drop=FALSE],
        #   level   = 100 * options[["confidenceIntervalsLevel"]],
        #   tau2.levels   = if (is.null(dim(predictorMatrixEffectSize))) tauLevelsMatrix[["tau2.levels"]]   else do.call(rbind, lapply(1:nrow(predictorMatrixEffectSize), function(i) tauLevelsMatrix))[["tau2.levels"]],
        #   gamma2.levels = if (is.null(dim(predictorMatrixEffectSize))) tauLevelsMatrix[["gamma2.levels"]] else do.call(rbind, lapply(1:nrow(predictorMatrixEffectSize), function(i) tauLevelsMatrix))[["gamma2.levels"]]
        # )


      } else {

        computedContrasts <- predict(
          fit,
          newmods   = contrastMatrixEffectSize,
          level     = 100 * options[["confidenceIntervalsLevel"]]
        )
        if (.maIsGLMM(options)) {
          computedContrastsTests <- .maGlmmContrastTest(fit, X = contrastMatrixEffectSize, adjust = .maGetPValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]]))
        } else {
          computedContrastsTests <- anova(
            fit,
            X         = contrastMatrixEffectSize,
            adjust    = .maGetPValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]])
          )
        }

      }
    }

    if (FALSE) {#.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {
      # # TODO?
      # tauLevels <- list(
      #   computedMarginalMeans[["tau2.level"]],
      #   computedMarginalMeans[["gamma2.level"]]
      # )
      # tauLevels           <- do.call(cbind.data.frame, tauLevels[!sapply(tauLevels, is.null)])
      # colnames(tauLevels) <- .mammExtractTauLevelNames(fit)
    }

  } else if (parameter == "heterogeneity") {

    predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansHeterogeneitySdFactorCovariates"]],
      parameter         = "heterogeneity",
      dropIntercept     = FALSE
    )

    selectedVariableLevels      <- apply(attr(predictorMatrixHeterogeneity, "selectedGridNames"), 1, paste0, collapse = ", ")
    contrastMatrixHeterogeneity <- matrix(NA, nrow = nrow(predictorMatrixHeterogeneity) * (nrow(predictorMatrixHeterogeneity) - 1) / 2, ncol = ncol(predictorMatrixHeterogeneity))
    contrastComparisons         <- character(nrow(contrastMatrixHeterogeneity))

    thisContrast <- 1
    for (i in 1:length(selectedVariableLevels)){
      for (j in 1:length(selectedVariableLevels)){
        if (j > i) {
          contrastMatrixHeterogeneity[thisContrast,] <- predictorMatrixHeterogeneity[i,] - predictorMatrixHeterogeneity[j,]
          contrastComparisons[thisContrast]          <- paste0(selectedVariableLevels[i], " – ", selectedVariableLevels[j])
          thisContrast <- thisContrast + 1
        }
      }
    }

    computedContrasts <- predict(
      fit,
      newscale  = contrastMatrixHeterogeneity,
      level     = 100 * options[["confidenceIntervalsLevel"]]
    )
    computedContrastsTests <- anova(
      fit,
      Z         = contrastMatrixHeterogeneity,
      adjust    = .maGetPValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]])
    )

    # neither link or tau transformation cannot be applied
  }

  # reformat
  computedContrasts <- .maExtractAndFormatPrediction(computedContrasts)

  # TODO: ? return the tau levels
  # if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]])
  #   computedMarginalMeans <- cbind(computedMarginalMeans, tauLevels)

  # add test results
  computedContrasts$comparison <- contrastComparisons
  computedContrasts$stat       <- computedContrastsTests$zval
  if (.maIsMetaregressionFtest(options))
    computedContrasts$df <- if (!is.null(computedContrastsTests$ddf)) computedContrastsTests$ddf else .maExtractDdf(fit)[1]
  computedContrasts$pval <- computedContrastsTests$pval

  computedContrasts$subgroup <- attr(fit, "subgroup")

  return(computedContrasts)
}

# Data preparation ----

.maMergeVariablesLevels               <- function(df, variables, mergedName) {
  if (length(variables) == 1) {
    df[[mergedName]] <- factor(
      df[,variables],
      levels = if (is.null(levels(df[,variables]))) unique(df[,variables]) else levels(df[,variables])
    )
  } else if (length(variables) > 1) {
    df[[mergedName]] <- factor(
      apply(df[,variables], 1, function(x) paste(x, collapse = " | ")),
      levels = unique(apply(df[,variables], 1, function(x) paste(x, collapse = " | ")))
    )
  }
  return(df)
}

.maCleanSelectedIndexesVector         <- function(x) {

  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")

  x <- strsplit(x, ",", fixed = TRUE)[[1]]

  x <- trimws(x, which = "both")
  x <- x[x != ""]

  x <- as.numeric(x)

  return(x)
}

.maDichotomizeVariablesLevels         <- function(df, variables, options) {

  variablesContinuous <- variables[variables %in% options[["predictors"]][options[["predictors.types"]] == "scale"]]
  for (i in seq_along(variablesContinuous)){
    tempUnique <- sort(unique(df[[variablesContinuous[i]]]))
    df[[variablesContinuous[i]]] <- as.character(factor(
      df[[variablesContinuous[i]]],
      levels = tempUnique,
      labels = c(paste0("Mean - ", options[["bubblePlotSdFactorCovariates"]], "SD"), "Mean", paste0("Mean + ", options[["bubblePlotSdFactorCovariates"]], "SD"))
    ))
    attr(df, "continuousLevels") <- list(
      attr(df, "continuousLevels"),
      list(
        variable = variablesContinuous[i],
        levels   = tempUnique
      )
    )
  }
  return(df)
}

.maDichotomizeVariablesDataset        <- function(df, variables, variablesInformation, options) {

  variablesContinuous  <- variables[variables %in% options[["predictors"]][options[["predictors.types"]] == "scale"]]

  for (i in seq_along(variablesContinuous)){

    tempUnique <- variablesInformation[[sapply(variablesInformation, function(x) x[["variable"]]) == variablesContinuous[i]]]

    # cut into the three levels
    df[[variablesContinuous[i]]] <- cut(
      df[[variablesContinuous[i]]],
      breaks = c(-Inf, mean(tempUnique[["levels"]][1:2]), mean(tempUnique[["levels"]][2:3]), Inf),
      labels = c(paste0("Mean - ", options[["bubblePlotSdFactorCovariates"]], "SD"), "Mean", paste0("Mean + ", options[["bubblePlotSdFactorCovariates"]], "SD"))
    )

    # ensure that all levels are present (get dropped if the interval is empty)
    levels(df[[variablesContinuous[i]]]) <- c(paste0("Mean - ", options[["bubblePlotSdFactorCovariates"]], "SD"), "Mean", paste0("Mean + ", options[["bubblePlotSdFactorCovariates"]], "SD"))
  }

  return(df)
}

.maGetTermsIndices                    <- function(fit, parameter) {

  if (parameter == "effectSize") {

    terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")     # get terms indices from the model
    termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")  # get coefficient indices from the model matrix
    if (!is.null(fit$coef.na))
      termsIndex <- termsIndex[!fit$coef.na]                                                   # remove dropped coefficients

    termsIndicies <- lapply(terms, function(term){
      seq_along(termsIndex)[termsIndex == which(terms == term)]
    })
    names(termsIndicies) <- terms

  } else if (parameter == "heterogeneity") {

    terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")      # get terms indices from the model
    termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")   # get coefficient indices from the model matrix
    if (!is.null(fit$coef.na.Z))
      termsIndex <- termsIndex[!fit$coef.na.Z]                                                   # remove dropped coefficients

    termsIndicies <- lapply(terms, function(term){
      seq_along(termsIndex)[termsIndex == which(terms == term)]
    })
    names(termsIndicies) <- terms

  }

  return(termsIndicies)
}

# Messages ----

.maEstimatedMarginalMeansMessages      <- function(options, parameter, anyNA = FALSE) {

  if (options[["subgroup"]] == "") {
    messages <- gettext("Each marginal mean estimate is averaged across the levels of the remaining predictors.")
  } else {
    messages <- gettext("Each marginal mean estimate is averaged across the levels of the remaining predictors in a given subgroup.")
  }

  if (parameter == "effectSize" && options[["transformEffectSize"]] != "none") {
    if (anyNA) {
      messages <- c(messages, gettextf("NAs in the marginal mean estimates were introduced due to the %1$s transformation. Please verify that you are using the correct effect size transformation.", .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))
    } else {
      messages <- c(messages, gettextf("The marginal mean estimates and intervals are transformed using %1$s transformation. For nonlinear transformations, transformed marginal means are interpreted as medians on the transformed scale.", .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))
    }
  }


  if (parameter == "heterogeneity")
    messages <- c(messages, gettextf("The estimates and intervals correspond to %1$s.", switch(
      options[["estimatedMarginalMeansHeterogeneityTransformation"]],
      "tau"  = gettext("\U1D70F"),
      "tau2" = gettext("\U1D70F\U00B2")
    )))

  return(messages)
}

.macontrastsMessages                   <- function(options, parameter) {

  messages <- gettext("Each contrast is averaged across the levels of the remaining predictors.")

  if (parameter == "effectSize" && options[["transformEffectSize"]] != "none") {
    messages <- c(messages, gettextf("Contrasts of estimates marginal means cannot be transformed via the effect size transformation."))

    if (options[["contrastsEffectSizePValueAdjustment"]] != "none") {
      messages <- c(messages, gettextf("Contrasts of estimated marginal means are adjusted for multiple comparisons using %1$s correction.", .maGetOptionsNamePValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]])))
    }
  }

  if (parameter == "heterogeneity") {

    if (options[["heterogeneityModelLink"]] == "log") {
      messages <- c(messages, gettext("Contrasts of estimated marginal means cannot be transformed via the link function. As such, the contrasts are summarized on the model scale: log(\U1D70F\U00B2)."))
    } else if (options[["estimatedMarginalMeansHeterogeneityTransformation"]] == "tau"){
      messages <- c(messages, gettext("Contrasts of estimated marginal means cannot be transformed via the heterogeneity transformation. As such, the contrasts are summarized as \U1D70F\U00B2."))
    }

    if (options[["contrastsEffectSizePValueAdjustment"]] != "none") {
      messages <- c(messages, gettextf("Contrasts of estimated marginal means are adjusted for multiple comparisons using %1$s correction.", .maGetOptionsNamePValueAdjustment(options[["contrastsHeterogeneityPValueAdjustment"]])))
    }
  }

  return(messages)
}
