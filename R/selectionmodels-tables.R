# Selection-model tables.
#
# Builds hypothesis-test, estimate, and p-value frequency tables.

.smMakeTestsTables         <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["fitTests"]])) {
    return()
  } else {
    # create container
    fitTests <- createJaspContainer(title = gettext("Model Tests"))
    fitTests$position <- 1
    fitTests$dependOn(.smDependencies)
    jaspResults[["fitTests"]] <- fitTests
  }

  models   <- jaspResults[["models"]]$object
  errorFE <- jaspBase::isTryError(models[["FE"]])
  errorRE <- jaspBase::isTryError(models[["RE"]])


  ### test of heterogeneity
  heterogeneityTest <- createJaspTable(title = gettext("Test of Heterogeneity"))
  heterogeneityTest$position <- 1
  fitTests[["heterogeneityTest"]] <- heterogeneityTest

  heterogeneityTest$addColumnInfo(name = "stat",  title = "Q",           type = "number")
  heterogeneityTest$addColumnInfo(name = "df",    title = gettext("df"), type = "integer")
  heterogeneityTest$addColumnInfo(name = "pVal",  title = "p",           type = "pvalue")

  if (!is.null(models)) {

    rowHeterogeneity      <- list()

    if (!errorFE) {
      rowHeterogeneity    <- c(rowHeterogeneity, list(
        stat = models[["FE"]][["QE"]],
        df   =(models[["FE"]][["k"]] - models[["FE"]][["npred"]] - 1),
        pVal = models[["FE"]][["QEp"]]
      ))
    } else if (!errorRE) {
      rowHeterogeneity    <- c(rowHeterogeneity, list(
        stat = models[["RE"]][["QE"]],
        df   =(models[["RE"]][["k"]] - models[["RE"]][["npred"]] - 1),
        pVal = models[["RE"]][["QEp"]]
      ))
    }

    heterogeneityTest$addRows(rowHeterogeneity)

    noteMessages <- unique(c(
      .smSetNoteMessages(jaspResults, models[["FE"]], options), .smSetNoteMessages(jaspResults, models[["RE"]], options)
    ))
    warningMessages <- unique(c(
      .smSetWarningMessages(models[["FE"]]), .smSetWarningMessages(models[["RE"]])
    ))
    errorMessages   <- unique(c(
      .smSetErrorMessage(models[["FE"]], "FE"), .smSetErrorMessage(models[["RE"]], "RE")
    ))
    for(i in seq_along(noteMessages)) {
      heterogeneityTest$addFootnote(symbol = gettext("Note:"), noteMessages[i])
    }
    for(i in seq_along(warningMessages)) {
      heterogeneityTest$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
    }
    for(i in seq_along(errorMessages)) {
      heterogeneityTest$addFootnote(symbol = gettext("Error:"),   errorMessages[i])
    }
  } else {
    if (!.smCheckReady(options) && options[["pValue"]] != "")
      heterogeneityTest$addFootnote(symbol = gettext("Note:"), .smSetNoteMessages(NULL, NULL, options))
  }


  ### test of bias
  biasTest <- createJaspTable(title = gettext("Test of Publication Bias"))
  biasTest$position <- 2
  fitTests[["biasTest"]] <- biasTest

  biasTest$addColumnInfo(name = "type",  title = "",                type = "string")
  biasTest$addColumnInfo(name = "stat",  title = gettext("ChiSq"),  type = "number")
  biasTest$addColumnInfo(name = "df",    title = gettext("df"),     type = "integer")
  biasTest$addColumnInfo(name = "pVal",  title = "p",               type = "pvalue")

  if (!is.null(models)) {

    rowBiasHomogeneity   <- list(type = gettext("Assuming homogeneity"))
    rowBiasHeterogeneity <- list(type = gettext("Assuming heterogeneity"))

    if (!errorFE) {
      rowBiasHomogeneity <- c(rowBiasHomogeneity, list(
        stat = 2*abs(models[["FE"]][["output_unadj"]][["value"]] - models[["FE"]][["output_adj"]][["value"]]),
        df   = length(models[["FE"]][["output_adj"]][["par"]]) - length(models[["FE"]][["output_unadj"]][["par"]]),
        pVal = pchisq(
          2*abs(models[["FE"]][["output_unadj"]][["value"]] - models[["FE"]][["output_adj"]][["value"]]),
          length(models[["FE"]][["output_adj"]][["par"]]) - length(models[["FE"]][["output_unadj"]][["par"]]),
          lower.tail = FALSE
        )
      ))
    }
    if (!errorRE) {
      rowBiasHeterogeneity <- c(rowBiasHeterogeneity, list(
        stat = 2*abs(models[["RE"]][["output_unadj"]][["value"]] - models[["RE"]][["output_adj"]][["value"]]),
        df   = length(models[["RE"]][["output_adj"]][["par"]]) - length(models[["RE"]][["output_unadj"]][["par"]]),
        pVal = pchisq(
          2*abs(models[["RE"]][["output_unadj"]][["value"]] - models[["RE"]][["output_adj"]][["value"]]),
          length(models[["RE"]][["output_adj"]][["par"]]) - length(models[["RE"]][["output_unadj"]][["par"]]),
          lower.tail = FALSE
        )
      ))
    }

    biasTest$addRows(rowBiasHomogeneity)
    biasTest$addRows(rowBiasHeterogeneity)

    noteMessages <- unique(c(
      .smSetNoteMessages(jaspResults, models[["FE"]], options), .smSetNoteMessages(jaspResults, models[["RE"]], options)
    ))
    warningMessages <- unique(c(
      .smSetWarningMessages(models[["FE"]]), .smSetWarningMessages(models[["RE"]])
    ))
    errorMessages   <- unique(c(
      .smSetErrorMessage(models[["FE"]], "FE"), .smSetErrorMessage(models[["RE"]], "RE")
    ))
    for(i in seq_along(noteMessages)) {
      biasTest$addFootnote(symbol = gettext("Note:"),    noteMessages[i])
    }
    for(i in seq_along(warningMessages)) {
      biasTest$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
    }
    for(i in seq_along(errorMessages)) {
      biasTest$addFootnote(symbol = gettext("Error:"),   errorMessages[i])
    }

  } else {
    if (!.smCheckReady(options) && options[["pValue"]] != "")
      biasTest$addFootnote(symbol = gettext("Note:"), .smSetNoteMessages(NULL, NULL, options))
  }

  return()
}

.smMakeEstimatesTables     <- function(jaspResults, dataset, options) {

  models   <- jaspResults[["models"]]$object

  ### assuming homogeneity
  if (is.null(jaspResults[["inferenceFixedEffectsMeanEstimatesTable"]])) {
    # create container
    estimatesFE <- createJaspContainer(title = gettext("Fixed Effects Estimates"))
    estimatesFE$position <- 2
    estimatesFE$dependOn(c(.smDependencies, "inferenceFixedEffectsMeanEstimatesTable"))
    jaspResults[["inferenceFixedEffectsMeanEstimatesTable"]] <- estimatesFE
  } else {
    estimatesFE <- jaspResults[["inferenceFixedEffectsMeanEstimatesTable"]]
  }

  # mean estimates
  if (is.null(estimatesFE[["inferenceFixedEffectsMeanEstimatesTable"]]) && options[["inferenceFixedEffectsMeanEstimatesTable"]]) {
    estimatesMeanFE <- createJaspTable(title = gettextf(
      "Mean Estimates (%s)",
      if (options[["measures"]] == "correlation") "\u03C1" else "\u03BC"
    ))
    estimatesMeanFE$position  <- 1
    estimatesFE[["meanFE"]] <- estimatesMeanFE
    meanFE <- .smFillEstimates(jaspResults, estimatesMeanFE, models[["FE"]], options)
  }

  # weights estimates
  if (is.null(estimatesFE[["inferenceFixedEffectsEstimatedWeightsTable"]]) && options[["inferenceFixedEffectsEstimatedWeightsTable"]] && options[["inferenceFixedEffectsMeanEstimatesTable"]]) {
    weightsFE <- createJaspTable(title = gettext("Estimated Weights"))
    weightsFE$position  <- 2
    weightsFE$dependOn("inferenceFixedEffectsEstimatedWeightsTable")
    estimatesFE[["inferenceFixedEffectsEstimatedWeightsTable"]] <- weightsFE
    weightsFE <- .smFillWeights(jaspResults, weightsFE, models[["FE"]], options)
  }


  ### assuming heterogeneity
  if (is.null(jaspResults[["inferenceRandomEffectsMeanEstimatesTable"]])) {
    # create container
    estimatesRE <- createJaspContainer(title = gettext("Random Effects Estimates"))
    estimatesRE$position <- 3
    estimatesRE$dependOn(c(.smDependencies, "inferenceRandomEffectsMeanEstimatesTable"))
    jaspResults[["inferenceRandomEffectsMeanEstimatesTable"]] <- estimatesRE
  } else {
    estimatesRE <- jaspResults[["inferenceRandomEffectsMeanEstimatesTable"]]
  }

  # mean estimates
  if (is.null(estimatesRE[["meanRE"]]) && options[["inferenceRandomEffectsMeanEstimatesTable"]]) {
    estimatesMeanRE <- createJaspTable(title = gettextf(
      "Mean Estimates (%s)",
      if (options[["measures"]] == "correlation") "\u03C1" else "\u03BC"
    ))
    estimatesMeanRE$position <- 1
    estimatesRE[["meanRE"]] <- estimatesMeanRE
    estimatesMeanRE <- .smFillEstimates(jaspResults, estimatesMeanRE, models[["RE"]], options)
  }

  # tau estimates
  if (is.null(estimatesRE[["inferenceRandomEffectsEstimatedHeterogeneityTable"]]) && options[["inferenceRandomEffectsEstimatedHeterogeneityTable"]] && options[["inferenceRandomEffectsMeanEstimatesTable"]]) {
    heterogeneityRE <- createJaspTable(title = gettextf("Heterogeneity Estimates(%s)", "\u03C4"))
    heterogeneityRE$position <- 2
    heterogeneityRE$dependOn("inferenceRandomEffectsEstimatedHeterogeneityTable")
    estimatesRE[["inferenceRandomEffectsEstimatedHeterogeneityTable"]] <- heterogeneityRE
    heterogeneityRE <- .smFillHeterogeneity(jaspResults, heterogeneityRE, models[["RE"]], options)
  }

  # weights estimates
  if (is.null(estimatesRE[["inferenceRandomEffectsEstimatedWeightsTable"]]) && options[["inferenceRandomEffectsEstimatedWeightsTable"]] && options[["inferenceRandomEffectsMeanEstimatesTable"]]) {
    weightsRE <- createJaspTable(title = gettext("Estimated Weights"))
    weightsRE$position  <- 3
    weightsRE$dependOn("inferenceRandomEffectsEstimatedWeightsTable")
    estimatesRE[["inferenceRandomEffectsEstimatedWeightsTable"]] <- weightsRE
    weightsRE <- .smFillWeights(jaspResults, weightsRE, models[["RE"]], options)
  }

  return()
}

.smPFrequencyTable         <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["pFrequency"]])) {
    return()
  } else {
    # create container
    pFrequency <- createJaspTable(title = gettext("p-value Frequency"))
    pFrequency$position <- 4
    pFrequency$dependOn(c(.smDependencies, "modelPValueFrequencyTable"))
    jaspResults[["pFrequency"]] <- pFrequency
  }

  overtitle <- gettext("<em>p</em>-values interval(one-sided)")
  pFrequency$addColumnInfo(name = "lowerPRange", title = gettext("Lower"),     type = "number", overtitle = overtitle)
  pFrequency$addColumnInfo(name = "upperPRange", title = gettext("Upper"),     type = "number", overtitle = overtitle)
  pFrequency$addColumnInfo(name = "frequency",   title = gettext("Frequency"), type = "integer")

  if (!.smCheckReady(options))
    return()


  models <- jaspResults[["models"]]$object

  # get the p-value steps and p-values(so we don't have to search for them in the models)
  steps <- .smGetCutoffs(options)
  pVal  <- .maGetInputPVal(dataset, options)

  # add a note in case that the models failed to conver due to autoreduce
  if (jaspBase::isTryError(models[["FE"]]) || jaspBase::isTryError(models[["RE"]])) {

    if (options[["modelAutomaticallyJoinPValueIntervals"]]) {
      if (jaspBase::isTryError(models[["FE"]]) && jaspBase::isTryError(models[["RE"]])) {
        if (grepl("No steps", models[["FE"]])) {
          pFrequency$addFootnote(gettext("There were no p-value cutoffs after their automatic reduction. The displayed frequencies correspond to the non-reduced p-value cutoffs."))
        }
      } else {
        # the failure wasn't due to the reduce - reduce the p-value cutoffs
        steps <- .smJoinCutoffs(steps, pVal)
      }
    }
  } else {
    if (options[["modelAutomaticallyJoinPValueIntervals"]]) {
      steps <- .smJoinCutoffs(steps, pVal)
    }
  }

  steps <- c(0, steps)
  cutoffsTable <- table(cut(pVal, breaks = steps))

  for(i in 1:length(cutoffsTable)) {
    pFrequency$addRows(list(
      lowerPRange = steps[i],
      upperPRange = steps[i+1],
      frequency   = cutoffsTable[i]
    ))
  }

  return()
}
