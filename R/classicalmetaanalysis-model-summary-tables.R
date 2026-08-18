# Classical meta-analysis model-summary tables.
#
# Builds overall-test, pooled-estimate, and fit-measure output tables and messages.

# Container ----

.maExtractModelSummaryContainer      <- function(jaspResults) {

  if (!is.null(jaspResults[["modelSummaryContainer"]]))
    return(jaspResults[["modelSummaryContainer"]])

  # create the output container
  modelSummaryContainer <- createJaspContainer(gettext("Model Summary"))
  modelSummaryContainer$dependOn(.maDependencies)
  modelSummaryContainer$position <- 1
  jaspResults[["modelSummaryContainer"]] <- modelSummaryContainer

  return(modelSummaryContainer)
}

# Overall tests ----

.maOverallTestsTable                     <- function(jaspResults, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["testsTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # model tests table
  testsTable <- createJaspTable(gettext("Meta-Analytic Tests"))
  testsTable$position <- 1
  testsTable$dependOn(c("addOmnibusModeratorTestEffectSizeCoefficients", "addOmnibusModeratorTestEffectSizeCoefficientsValues",
                        "addOmnibusModeratorTestHeterogeneityCoefficients", "addOmnibusModeratorTestHeterogeneityCoefficientsValues",
                        "includeFullDatasetInSubgroupAnalysis"))
  modelSummaryContainer[["testsTable"]] <- testsTable

  testsTable$addColumnInfo(name = "test",  type = "string",  title = "")
  .maAddSubgroupColumn(testsTable, options)
  testsTable$addColumnInfo(name = "stat",  type = "string",  title = gettext("Test"))
  testsTable$addColumnInfo(name = "pval",  type = "pvalue",  title = gettext("p"))

  if (.maIsPermutation(options)) {
    testsTable$addColumnInfo(name = "pval2",  type = "pvalue",  title = gettext("p (permutation)"))
    testsTable$addFootnote(.maPermutationMessage(options))
  }

  # stop and display errors
  if (is.null(fit))
    return()

  if (!is.null(.maCheckIsPossibleOptions(options))) {
    testsTable$setError(.maCheckIsPossibleOptions(options))
    return()
  }

  # stop with error if only single fit requested and failed
  if (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) {
    testsTable$setError(.maTryCleanErrorMessages(fit[[1]]))
    return()
  }

  # add all the overall model test
  tests <- list()
  if (.maIsGLMM(options)) {
    tempHeterogeneity <- lapply(fit, .maglmmRowHeterogeneityTest, options = options)
    tests[["heterogeneity"]] <- .maSafeRbind(tempHeterogeneity)

    tempFootnotes <- unique(lapply(tempHeterogeneity, attr, which = "footnote"))
    tempFootnotes <- Filter(Negate(is.null), tempFootnotes)
    for (i in seq_along(tempFootnotes))
      testsTable$addFootnote(tempFootnotes[[i]])
  } else {
    tests[["heterogeneity"]] <- .maSafeRbind(lapply(fit, .maRowHeterogeneityTest, options = options))
  }
  tests[["effect"]]        <- .maSafeRbind(lapply(fit, .maRowEffectSizeTest,    options = options))

  # effect size moderation
  if (.maIsMetaregressionEffectSize(options)) {
    # omnibus test
    tests[["moderationEffect"]] <- .maSafeRbind(lapply(fit, .maRowModerationTest, options = options, parameter = "effectSize"))

    # additional custom test
    if (isTRUE(options[["addOmnibusModeratorTestEffectSizeCoefficients"]])) {

      tempModerationEffect2 <- lapply(fit, .maRowModerationTest, options = options, parameter = "effectSize", coefficientsTest = TRUE)
      tests[["moderationEffect2"]] <- .maSafeRbind(tempModerationEffect2)

      if (jaspBase::isTryError(tests[["moderationEffect2"]])) {
        testsTable$setError(tests[["moderationEffect2"]])
        return()
      }

      # add footnotes
      tempFootnotes <- unique(lapply(tempModerationEffect2, attr, which = "footnote"))
      tempFootnotes <- Filter(Negate(is.null), tempFootnotes)
      for (i in seq_along(tempFootnotes))
        testsTable$addFootnote(tempFootnotes[[i]])
    }
  }

  # heterogeneity moderation
  if (.maIsMetaregressionHeterogeneity(options)) {
    # omnibus test
    tests[["moderationHeterogeneity"]] <- .maSafeRbind(lapply(fit, .maRowModerationTest, options = options, parameter = "heterogeneity"))

    # additional custom test
    if (isTRUE(options[["addOmnibusModeratorTestHeterogeneityCoefficients"]])) {

      tempModerationHeterogeneity2 <- lapply(fit, .maRowModerationTest, options = options, parameter = "heterogeneity", coefficientsTest = TRUE)
      tests[["moderationHeterogeneity2"]] <- .maSafeRbind(tempModerationHeterogeneity2)

      if (jaspBase::isTryError(tests[["moderationHeterogeneity2"]])) {
        testsTable$setError(tests[["moderationHeterogeneity2"]])
        return()
      }

      # add footnotes
      tempFootnotes <- unique(lapply(tempModerationHeterogeneity2, attr, which = "footnote"))
      tempFootnotes <- Filter(Negate(is.null), tempFootnotes)
      for (i in seq_along(tempFootnotes))
        testsTable$addFootnote(tempFootnotes[[i]])

    }
  }

  # additional tests for Mantel-Haenszel
  if (options[["analysis"]] == "mantelHaenszelPeto" &&
      ((options[["method"]] == "mantelHaenszelFrequencies" && options[["effectSizeMeasure"]] == "OR") ||
       (options[["method"]] == "mantelHaenszelEvents"      && options[["effectSizeMeasure"]] == "IRR"))) {

    # add the Mantel-Haenszel test
    tests[["mantelHaenszel"]] <- .maSafeRbind(lapply(fit, .mamhpRowMantelHaenszelTest, options = options))
  }

  # additional tests for Mantel-Haenszel
  if (options[["analysis"]] == "mantelHaenszelPeto" &&
      (options[["method"]] == "mantelHaenszelFrequencies" && options[["effectSizeMeasure"]] == "OR")) {

    # add the Tarone's test
    tests[["Tarone"]] <- .maSafeRbind(lapply(fit, .mamhpRowTaroneTest, options = options))
  }

  # additional tests for subgroup differences
  if (options[["subgroup"]] != "") {
    tests[["subgroup"]] <- .maRowSubgroupTest(fit, options = options)
  }


  # add errors messages for failed fits
  for (i in seq_along(fit)[sapply(fit, jaspBase::isTryError)]) {
    testsTable$addFootnote(
      gettextf("The model for subgroup '%1$s' failed with the following error: %2$s",
               attr(fit[[i]], "subgroup"),
               .maTryCleanErrorMessages(fit[[i]])),
      symbol = gettext("Error:")
    )
  }

  # add multivariate settings notes
  if (.maIsMultilevelMultivariate(options)) {
    multivariateReadyNotes <- attr(.mammVarianceCovarianceMatrixReady(options), "messages")
    for (i in seq_along(multivariateReadyNotes)) {
      testsTable$addFootnote(multivariateReadyNotes[i])
    }
  }

  if (.maIsGLMM(options) && .maIsMetaregressionEffectSize(options)) {
    if (options[["fixedEffectTest"]] == "t")
      testsTable$addFootnote(gettext("Moderation test based on a Wald-type F-test."))
    else
      testsTable$addFootnote(gettext("Moderation test based on a Wald-type chi-squared test."))
  }

  .maAddLowDdfWarning(testsTable, fit, options)

  # bind and clean rows
  tests <- .maSafeRbind(tests)
  tests <- .maSafeOrderAndSimplify(tests, "test", options)

  # add the rows to the table
  testsTable$setData(tests)

  return()
}

# Pooled estimates ----

.maPooledEstimatesTable                  <- function(jaspResults, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["pooledEstimatesTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # pooled estimates
  pooledEstimatesTable          <- createJaspTable(gettext("Meta-Analytic Estimates"))
  pooledEstimatesTable$showSpecifiedColumnsOnly <- TRUE
  pooledEstimatesTable$position <- 4
  pooledEstimatesTable$dependOn(c("heterogeneityTau", "heterogeneityTau2", "heterogeneityI2", "heterogeneityH2",
                                  "confidenceIntervals", "confidenceIntervalsLevel", "predictionIntervals", "transformEffectSize",
                                  "standardErrors", "includeFullDatasetInSubgroupAnalysis"))
  modelSummaryContainer[["pooledEstimatesTable"]] <- pooledEstimatesTable

  pooledEstimatesTable$addColumnInfo(name = "par",  type = "string", title = "")
  .maAddSubgroupColumn(pooledEstimatesTable, options)
  pooledEstimatesTable$addColumnInfo(name = "est",  type = "number", title = gettext("Estimate"))
  .maAddSeColumn(pooledEstimatesTable, options)
  .maAddCiColumn(pooledEstimatesTable, options)
  .maAddPiColumn(pooledEstimatesTable, options)

  if (.maIsGLMM(options) && options[["glmmModel"]] == "UM.RS") {
    showSigma <- options[["heterogeneityTau"]] || options[["heterogeneityTau2"]]
    showRho   <- options[["glmmCorrelatedEffects"]] &&
      (showSigma || options[["heterogeneityI2"]] || options[["heterogeneityH2"]])

    if (showSigma && showRho) {
      pooledEstimatesTable$addFootnote(gettext("For unconditional models with random study effects, \u03C3\u00B2 denotes study-level variability and \u03C1 denotes the correlation between study and group random effects."))
    } else if (showSigma) {
      pooledEstimatesTable$addFootnote(gettext("For unconditional models with random study effects, \u03C3\u00B2 denotes study-level variability."))
    } else if (showRho) {
      pooledEstimatesTable$addFootnote(gettext("For unconditional models with random study effects, \u03C1 denotes the correlation between study and group random effects."))
    }
  }

  if (options[["predictionIntervals"]] && .mammHasMultipleHeterogeneities(options, canAddOutput = TRUE)) {
    for (colName in .mammExtractTauLevelNamesList(fit)) {
      pooledEstimatesTable$addColumnInfo(name = colName, title = colName, type = .maGetVariableColumnType(colName, options), overtitle = gettext("Heterogeneity Level"))
    }
  }

  # skip on error
  if (length(fit) == 0 || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  estimates <- list()

  # pooled effect size
  estimates[["effect"]] <- .maSafeRbind(lapply(fit, .maRowPooledEffectEstimate, options = options))

  # pooled heterogeneity
  if (.maIsGLMM(options) && .maGetMethodOptions(options) != "FE" &&
      (options[["heterogeneityTau"]] || options[["heterogeneityTau2"]] || options[["heterogeneityI2"]] || options[["heterogeneityH2"]])) {
    estimates[["heterogeneity"]] <- .maSafeRbind(lapply(fit, .maRowPooledHeterogeneity, options = options))
  } else if (!.maGetMethodOptions(options) %in% c("EE", "FE", "MH", "PETO") && !.maIsMultilevelMultivariate(options) &&
      (options[["heterogeneityTau"]] ||options[["heterogeneityTau2"]] || options[["heterogeneityI2"]] || options[["heterogeneityH2"]])) {

    # requires non-clustered fit
    fitNonClustered <- .maExtractFit(jaspResults, options, nonClustered = TRUE)
    estimates[["heterogeneity"]] <- .maSafeRbind(lapply(fitNonClustered, .maRowPooledHeterogeneity, options = options))
  } else if (options[["analysis"]] %in% "mantelHaenszelPeto" &&
             (options[["heterogeneityI2"]] || options[["heterogeneityH2"]])) {
    # requires non-clustered fit
    fitNonClustered <- .maExtractFit(jaspResults, options, nonClustered = TRUE)
    estimates[["heterogeneity"]] <- .maSafeRbind(lapply(fitNonClustered, .maRowPooledHeterogeneity, options = options))
  }

  # add messages
  pooledEstimatesMessages <- .maPooledEstimatesMessages(fit, options, anyNA(estimates[["effect"]]))
  for (i in seq_along(pooledEstimatesMessages))
    pooledEstimatesTable$addFootnote(pooledEstimatesMessages[i])

  # merge and clean estimates
  estimates <- .maSafeRbind(estimates)
  estimates <- .maSafeOrderAndSimplify(estimates, "par", options)

  pooledEstimatesTable$setData(estimates)

  return()
}

# Fit measures ----

.maFitMeasuresTable                      <- function(jaspResults, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["fitMeasuresTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # fit measures table
  fitMeasuresTable          <- createJaspTable(gettext("Fit Measures"))
  fitMeasuresTable$position <- 4
  fitMeasuresTable$dependOn(c(.maDependencies, "fitMeasures", "includeFullDatasetInSubgroupAnalysis"))
  modelSummaryContainer[["fitMeasuresTable"]] <- fitMeasuresTable


  fitMeasuresTable$addColumnInfo(name = "model",         title = "",                      type = "string")
  .maAddSubgroupColumn(fitMeasuresTable, options)
  fitMeasuresTable$addColumnInfo(name = "observations",  title = gettext("Observations"), type = "integer")
  fitMeasuresTable$addColumnInfo(name = "ll",            title = gettext("Log Lik."),     type = "number")
  if (!.maIsUnrestrictedWeightedLeastSquares(options))
    fitMeasuresTable$addColumnInfo(name = "dev",         title = gettext("Deviance"),     type = "number")
  fitMeasuresTable$addColumnInfo(name = "AIC",           title = gettext("AIC"),          type = "number")
  fitMeasuresTable$addColumnInfo(name = "BIC",           title = gettext("BIC"),          type = "number")
  if (!.maIsUnrestrictedWeightedLeastSquares(options))
    fitMeasuresTable$addColumnInfo(name = "AICc",        title = gettext("AICc"),         type = "number")

  if (!.maIsUnrestrictedWeightedLeastSquares(options) &&
      .maIsMetaregressionEffectSize(options) && !.maIsMultilevelMultivariate(options) && !.maIsGLMM(options))
    fitMeasuresTable$addColumnInfo(name = "R2",  title = gettext("R\U00B2"),   type = "number")

  # skip on error
  if ((length(fit) == 1 && jaspBase::isTryError(fit[[1]]))  || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # fit measures rows
  fitMeasures <- .maSafeRbind(lapply(fit, .maRowFitMeasures, options = options))
  fitMeasures <- .maSafeOrderAndSimplify(fitMeasures, "model", options)

  fitMeasuresTable$setData(fitMeasures)

  return()
}
















# Messages and footnotes ----

.maFixedEffectTextMessage              <- function(options) {
  return(switch(
    .maGetFixedEffectTestOptions(options),
    "z"    = gettext("Fixed effects tested using z-distribution."),
    "t"    = gettext("Fixed effects tested using t-distribution."),
    "knha" = gettext("Fixed effects tested using Knapp and Hartung adjustment."),
    stop(paste0("Unknown fixed effect test.", options[["fixedEffectTest"]]))
  ))
}

.maAddFixedEffectTestFootnote          <- function(table, options) {
  if (!.maIsUnrestrictedWeightedLeastSquares(options))
    table$addFootnote(.maFixedEffectTextMessage(options))
}

.meMetaregressionHeterogeneityMessages <- function(options) {

  if (options[["heterogeneityModelLink"]] == "log")
    return(gettext("The heterogeneity model for \U1D70F\U00B2 is specified on the log scale."))
  else if (options[["heterogeneityModelLink"]] == "identity")
    return(gettext("The heterogeneity model for \U1D70F\U00B2 is specified on the identity scale."))
}

.maPooledEstimatesMessages             <- function(fit, options, anyNA = FALSE) {

  messages <- NULL

  if (.maIsMetaregressionEffectSize(options)) {
    if (.maIsClassical(options)) {
      effectSizeName <- gettext("pooled effect")
    } else {
      effectSizeName <- gettext("adjusted effect")
    }
  } else {
    effectSizeName <- gettext("pooled effect")
  }


  if (options[["subgroup"]] == "") {

    tempFit     <- fit[[1]]
    tempDataset <- attr(tempFit, "dataset")

    if (attr(tempDataset, "NAs") > 0)
      messages <- c(messages, gettextf("%1$i observations were ommited due to missing values.", attr(tempDataset, "NAs")))

    if (!is.null(options[["clustering"]]) && options[["clustering"]] != "") {
      if (!jaspBase::isTryError(tempFit) && !is.null(tempFit)){
        messages <- c(messages, .maClusterRobustInferenceMessage(tempFit, options))
      }
    }
  } else {
    for (i in seq_along(fit)) {

      tempFit     <- fit[[i]]
      tempDataset <- attr(tempFit, "dataset")

      if (attr(tempDataset, "NAs") > 0)
        messages <- c(messages, gettextf("%1$s: %2$i observations were ommited due to missing values.", gettextf("Subgroup %1$s", attr(tempFit, "subgroup")), attr(tempDataset, "NAs")))

      if (!is.null(options[["clustering"]]) && options[["clustering"]] != "") {
        if (!jaspBase::isTryError(tempFit) && !is.null(tempFit)) {
          clusterRobustMessage <- .maClusterRobustInferenceMessage(tempFit, options)
          if (!is.null(clusterRobustMessage))
            messages <- c(messages, gettextf("%1$s: %2$s", gettextf("Subgroup %1$s", attr(tempFit, "subgroup")), clusterRobustMessage))
        }
      }
    }
  }

  if (.maIsMultilevelMultivariate(options)) {
    varianceCovarianceMatrixMessage <- .mammVarianceCovarianceMatrixMessage(options)
    if (!is.null(varianceCovarianceMatrixMessage))
      messages <- c(messages, varianceCovarianceMatrixMessage)
  }

  if (options[["transformEffectSize"]] != "none") {
    if (anyNA) {
      messages <- c(messages, gettextf("NAs in the %1$s were introduced due to the %2$s transformation. Please verify that you are using the correct effect size transformation.", effectSizeName, .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))
    } else {
      messages <- c(messages, gettextf("The %1$s is transformed using %2$s transformation. For nonlinear transformations, the transformed estimate is interpreted as a median on the transformed scale.", effectSizeName, .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))
    }
  }

  if (.maIsMetaregressionEffectSize(options)) {
    if (.maIsClassical(options)) {
      messages <- c(messages, gettext("The pooled effect size corresponds to the weighted average effect across studies."))
    } else {
      messages <- c(messages, gettext("The adjusted effect corresponds to the averaged effect size estimate across the levels of all moderators."))
    }
  }

  if (.maIsMetaregressionHeterogeneity(options))
    messages <- c(messages, gettext("The pooled heterogeneity estimate corresponds to the heterogeneity at the average of predictor values."))

  if (.maIsMetaregressionHeterogeneity(options) && (options[["heterogeneityI2"]] || options[["heterogeneityH2"]]))
    messages <- c(messages, gettext("The I\U00B2 and H\U00B2 statistics are not available for heterogeneity models."))

  if (.maIsGLMM(options) && options[["confidenceIntervals"]] &&
      (options[["heterogeneityTau"]] || options[["heterogeneityTau2"]] || options[["heterogeneityI2"]] || options[["heterogeneityH2"]]) &&
      !jaspBase::isTryError(fit[[1]]) && is.na(fit[[1]][["ci.lb.tau2"]])) {
    messages <- c(messages, gettext("Heterogeneity confidence intervals are not available for this GLMM model type."))
  }

  if (.maIsMultilevelMultivariate(options) && any(attr(fit[[1]], "skipped")) && !jaspBase::isTryError(fit[[1]]))
    messages <- c(messages, gettextf("The Model Structure %1$s was not completely specified and was skipped.", paste0(which(attr(fit[[1]], "skipped")), collapse = " and ")))

  if (.mammAnyStructureGen(options) && options[["predictionIntervals"]])
    messages <- c(messages, gettextf("Prediction interval for the %1$s is not available for models with multiple heterogeneity estimates.", effectSizeName))

  return(messages)
}

.maClusterRobustInferenceMessage <- function(fit, options) {

  if (!.maIsClustered(options) || jaspBase::isTryError(fit) || is.null(fit))
    return(NULL)

  clusterSummary <- .maClusterRobustSummaryMessage(fit)
  if (is.null(clusterSummary))
    return(NULL)

  return(gettextf(
    "Cluster-robust tests and confidence intervals were computed using %1$s as the clustering variable (%2$s). %3$s",
    decodeColNames(options[["clustering"]]),
    clusterSummary,
    .maClusterRobustMethodMessage(options)
  ))
}

.maClusterRobustSummaryMessage   <- function(fit) {

  clusterCounts <- fit[["tcl"]]
  nClusters     <- fit[["n"]]

  if (is.null(clusterCounts) || is.null(nClusters) || length(clusterCounts) == 0)
    return(NULL)

  if (all(clusterCounts[1] == clusterCounts)) {
    return(gettextf(
      "%1$i clusters; %2$i estimates per cluster",
      nClusters,
      clusterCounts[1]
    ))
  }

  return(gettextf(
    "%1$i clusters; min/median/max %2$i/%3$i/%4$i estimates per cluster",
    nClusters,
    min(clusterCounts),
    round(stats::median(clusterCounts)),
    max(clusterCounts)
  ))
}

.maClusterRobustMethodMessage    <- function(options) {

  if (isTRUE(options[["clusteringUseClubSandwich"]]))
    return(gettext("clubSandwich was used with CR2 adjustment and Satterthwaite degrees of freedom."))

  if (isTRUE(options[["clusteringSmallSampleCorrection"]]))
    return(gettext("A CR1 small-sample correction was applied."))

  return(gettext("Unadjusted CR0 cluster-robust standard errors were used."))
}

.maPermutationMessage                  <- function(options) {
  return(gettextf("Permutation p-value is based on %1$s permutations.", switch(
    options[["permutationTestType"]],
    "exact"       = gettext("exact"),
    "approximate" = options[["permutationTestIteration"]]
  )))
}
