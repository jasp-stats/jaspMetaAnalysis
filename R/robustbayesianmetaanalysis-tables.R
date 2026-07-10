# Robust Bayesian meta-analysis tables.
#
# Builds specification, test, estimate, coefficient, and publication-bias tables.

# Model specification ----

.robmaModelSpecificationTables                    <- function(jaspResults, options) {

  # create / access the container
  # the dependencies could be set for specific tables
  # but this is very simple computation so it's not worth doing
  if (!is.null(jaspResults[["modelSpecification"]])) {
    return()
  } else {
    modelSpecification <- createJaspContainer(title = gettext("Model Specification"))
    modelSpecification$dependOn(c(.robmaDependencies, "shortenPriorName", "showModelSpecification"))
    modelSpecification$position <- 1
    jaspResults[["modelSpecification"]] <- modelSpecification
  }

  ### add component overview table
  modelSpecification[["overallSummary"]] <- .robmaModelSpecificationTablesComponents(jaspResults, options)

  ### create models overview table
  modelSpecification[["componentPriors"]]     <- .robmaModelSpecificationTablesComponentsPriors(jaspResults, options)
  modelSpecification[["componentPriorsNull"]] <- .robmaModelSpecificationTablesComponentsPriors(jaspResults, options, null = TRUE)

  ### moderation priors
  modelSpecification[["moderators"]] <- .robmaModelSpecificationTablesModeratorsPriors(jaspResults, options)

  return()
}

.robmaModelSpecificationTablesComponents          <- function(jaspResults, options) {

  priors     <- attr(options, "priors")
  components <- switch(
    options[["analysis"]],
    "RoBMA" = c("effect", "heterogeneity", "bias"),
    "NoBMA" = c("effect", "heterogeneity"),
    "BiBMA" = c("effect", "heterogeneity", "baseline")
  )

  ### create overview table
  tempTable <- createJaspTable(title = gettext("Model Components"))
  tempTable$position <- 1

  tempTable$addColumnInfo(name = "component", title = "",                type = "string")
  tempTable$addColumnInfo(name = "models",    title = gettext("Models"), type = "string")
  tempTable$addColumnInfo(name = "priorProb", title = gettext("P(M)"),   type = "number")

  if (is.null(priors))
    return(tempTable)

  # fill rows
  out <- list()
  for (component in components) {

    tempPriorsNull <- priors[[paste0(component, "Null")]]
    tempPriors     <- priors[[component]]

    tempWeightsNull <- if (length(tempPriorsNull) > 0) sum(sapply(tempPriorsNull, \(x) x[["prior_weights"]])) else 0
    tempWeights     <- if (length(tempPriors) > 0)     sum(sapply(tempPriors,     \(x) x[["prior_weights"]])) else 0

    out[[component]] <- data.frame(
      component   = .robmaComponentNames(component, options),
      models      = sprintf("%1$i/%2$i", length(tempPriors), length(tempPriorsNull) + length(tempPriors)),
      priorProb   = tempWeights / (tempWeights + tempWeightsNull)
    )
  }

  tempTable$setData(do.call(rbind, out))

  return(tempTable)
}

.robmaModelSpecificationTablesComponentsPriors    <- function(jaspResults, options, null = FALSE) {

  priors     <- attr(options, "priors")
  components <- switch(
    options[["analysis"]],
    "RoBMA" = c("effect", "heterogeneity", "bias"),
    "NoBMA" = c("effect", "heterogeneity"),
    "BiBMA" = c("effect", "heterogeneity", "baseline")
  )

  ### create overview table
  tempTable <- createJaspTable(title = gettextf("Prior Distributions %1$s", if (null) gettext("(Null)") else gettext("(Alternative)")))
  tempTable$position <- if (null) 3 else 2

  # select the corresponding priors
  componentNames <- paste0(components, if (null) "Null" else "")
  priors         <- priors[names(priors) %in% componentNames]
  priors         <- priors[!sapply(priors, is.null)]
  names(priors)  <- gsub("Null", "", names(priors)) # remove the null from the names for simpler handling

  if (length(priors) == 0)
    return(tempTable)

  for (component in components) {
    tempTable$addColumnInfo(name = component, title = .robmaComponentNames(component, options), type = "string")
  }

  # print notes & fill empty spots
  priors <- lapply(priors, function(x) c(
    if (length(x) > 0) sapply(x, print, short_name = options[["shortenPriorName"]], silent = TRUE),
    rep("", max(lengths(priors)) - length(x))
  ))

  tempTable$setData(do.call(cbind.data.frame, priors))

  return(tempTable)
}

.robmaModelSpecificationTablesModeratorsPriors    <- function(jaspResults, options) {

  if (length(options[["effectSizeModelTerms"]]) == 0)
    return()

  priors <- attr(options, "priors")

  ### create overview table
  tempTable <- createJaspTable(title = gettext("Prior Distributions Moderators"))
  tempTable$position <- 4

  tempTable$addColumnInfo(name = "term",         title = "",                     type = "string")
  tempTable$addColumnInfo(name = "alternative",  title = gettext("Alternative"), type = "string")
  tempTable$addColumnInfo(name = "null",         title = gettext("Null"),        type = "string")

  # select the corresponding priors
  priors <- priors[["moderators"]]

  if (length(priors) == 0)
    return(tempTable)

  # print notes & fill empty spots
  priors <- lapply(names(priors), function(x) {
    data.frame(
      term        = x,
      alternative = if (!is.null(priors[[x]][["alt"]]))  print(priors[[x]][["alt"]], short_name = options[["shortenPriorName"]], silent = TRUE)  else "",
      null        = if (!is.null(priors[[x]][["null"]])) print(priors[[x]][["null"]], short_name = options[["shortenPriorName"]], silent = TRUE) else ""
    )
  })

  tempTable$setData(do.call(rbind.data.frame, priors))

  return(tempTable)
}

# Results tables ----

.robmaOverallTestsTable                           <- function(jaspResults, options) {

  modelSummaryContainer <- .robmaExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["testsTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  ### create overview table
  testsTable <- createJaspTable(gettext("Meta-Analytic Tests"))
  testsTable$position <- 1
  testsTable$dependOn(c("includeFullDatasetInSubgroupAnalysis", "bayesFactorType"))
  modelSummaryContainer[["testsTable"]] <- testsTable

  testsTable$addColumnInfo(name = "test",  type = "string",  title = "")
  .maAddSubgroupColumn(testsTable, options)
  testsTable$addColumnInfo(name = "priorProb", title = gettext("P(M)"),      type = "number")
  testsTable$addColumnInfo(name = "postProb",  title = gettext("P(M|data)"), type = "number")
  .robmaAddBfColumn(testsTable, options)

  # display waiting message on no fit
  if (is.null(fit)) {
    if (options[["analysis"]] == "RoBMA")
      testsTable$addFootnote(gettext("The analysis will estimate a complex meta-analytic model ensemble using MCMC and might require a prolonged time to complete."), symbol = "\u26A0")
    return()
  }

  # stop with error if only single fit requested and failed
  if (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) {
    testsTable$setError(fit[[1]])
    return()
  }

  tests <- .maSafeRbind(lapply(fit, .robmaRowTests, options = options))

  # add errors messages for failed fits
  for (i in seq_along(fit)[sapply(fit, jaspBase::isTryError)]) {
    testsTable$addFootnote(
      gettextf("The model for subgroup '%1$s' failed with the following error: %2$s",
               attr(fit[[i]], "subgroup"),
               fit[[1]]),
      symbol = gettext("Error:")
    )
  }

  # add errors and messages for successful fits
  for (i in seq_along(fit)[!sapply(fit, jaspBase::isTryError)]) {
    errorsAndWarnings <- RoBMA::check_RoBMA(fit[[i]])
    for (j in seq_along(errorsAndWarnings)) {
      if (options[["subgroup"]] != "") {
        testsTable$addFootnote(symbol = gettext("Warning:"), gettextf(
          "The model fit for subgroup '%1$s' resulted in the following warning: %2$s",
          attr(fit[[i]], "subgroup"),
          errorsAndWarnings[j]
        ))
      } else {
        testsTable$addFootnote(symbol = gettext("Warning:"), gettextf(
          "The model fit resulted in the following warning: %1$s",
          errorsAndWarnings[j]
        ))
      }
    }
  }

  # add notes
  if (any(tests[["bf"]] > 100 | tests[["bf"]] < 1/100))
    testsTable$addFootnote(.robmaLargeBayesFactorWarning())

  # clean rows
  tests <- .maSafeOrderAndSimplify(tests, "test", options)

  # add the rows to the table
  testsTable$setData(tests)

  return()
}

.robmaPooledEstimatesTable                        <- function(jaspResults, options, conditional = FALSE) {

  modelSummaryContainer <- .robmaExtractModelSummaryContainer(jaspResults)

  # get table settings
  if (conditional) {
    tableName     <- "coefficientsStandardizedConditional"
    tableTitle    <- gettext("Conditional Meta-Analytic Estimates")
    tablePosition <- 3
  } else {
    tableName     <- "pooledEstimatesTable"
    tableTitle    <- gettext("Meta-Analytic Estimates")
    tablePosition <- 2
  }

  if (!is.null(modelSummaryContainer[[tableName]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # pooled estimates
  pooledEstimatesTable          <- createJaspTable(tableTitle)
  pooledEstimatesTable$position <- tablePosition
  pooledEstimatesTable$dependOn(c("heterogeneityTau", "heterogeneityTau2", "heterogeneityI2", "heterogeneityH2",
                                  "confidenceIntervals", "confidenceIntervalsLevel", "predictionIntervals", "transformEffectSize",
                                  "includeFullDatasetInSubgroupAnalysis", if (conditional) "conditionalEstimates"))
  modelSummaryContainer[[tableName]] <- pooledEstimatesTable

  pooledEstimatesTable$addColumnInfo(name = "par",  type = "string", title = "")
  .maAddSubgroupColumn(pooledEstimatesTable, options)
  pooledEstimatesTable$addColumnInfo(name = "mean",    type = "number", title = gettext("Mean"))
  pooledEstimatesTable$addColumnInfo(name = "median",  type = "number", title = gettext("Median"))
  .maAddCiColumn(pooledEstimatesTable, options)
  .maAddPiColumn(pooledEstimatesTable, options)

  # skip on error
  if (length(fit) == 0 || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  estimates <- .maSafeRbind(lapply(fit, .robmaRowPooledEstimates, options = options, conditional = conditional))

  # add messages
  pooledEstimatesMessages <- .maPooledEstimatesMessages(fit, options, FALSE)
  for (i in seq_along(pooledEstimatesMessages))
    pooledEstimatesTable$addFootnote(pooledEstimatesMessages[i])

  if (conditional)
    pooledEstimatesTable$addFootnote(gettext("Conditional estimates are based on models assuming the presence of a given component."))

  # merge and clean estimates
  estimates <- .maSafeOrderAndSimplify(estimates, "par", options)

  pooledEstimatesTable$setData(estimates)
  pooledEstimatesTable$showSpecifiedColumnsOnly <- TRUE

  return()
}

.robmaTermsTable                                  <- function(jaspResults, options) {

  metaregressionContainer <- .robmaExtractMetaregressionContainer(jaspResults)

  if (!is.null(metaregressionContainer[["termsTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  termsTable <- createJaspTable(gettext("Effect Size Meta-Regression Terms Tests"))
  termsTable$position <- 1
  termsTable$dependOn(c("metaregressionTermTests", "includeFullDatasetInSubgroupAnalysis", "bayesFactorType"))
  metaregressionContainer[["termsTable"]] <- termsTable

  termsTable$addColumnInfo(name = "term",  type = "string",  title = "")
  .maAddSubgroupColumn(termsTable, options)
  termsTable$addColumnInfo(name = "priorProb", title = gettext("P(M)"),      type = "number")
  termsTable$addColumnInfo(name = "postProb",  title = gettext("P(M|data)"), type = "number")
  .robmaAddBfColumn(termsTable, options)

  # skip on error
  if ((length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  # term tests rows
  termTests <- .maSafeRbind(lapply(fit, .robmaRowTermTests, options = options))
  termTests <- .maSafeOrderAndSimplify(termTests, "term", options)

  # add notes
  if (any(termTests[["bf"]] > 100 | termTests[["bf"]] < 1/100))
    termsTable$addFootnote(.robmaLargeBayesFactorWarning())

  termsTable$setData(termTests)
  termsTable$showSpecifiedColumnsOnly <- TRUE

  return()
}

.robmaCoefficientEstimatesTable                   <- function(jaspResults, options, standardized = FALSE, conditional = FALSE) {

  metaregressionContainer <- .robmaExtractMetaregressionContainer(jaspResults)

  # get table settings
  if (standardized && conditional) {
    tableName     <- "coefficientsStandardizedConditional"
    tableTitle    <- gettext("Standardized Conditional Meta-Regression Coefficients")
    tablePosition <- 5
  } else if (standardized && !conditional) {
    tableName     <- "coefficientsStandardized"
    tableTitle    <- gettext("Standardized Meta-Regression Coefficients")
    tablePosition <- 4
  } else if (!standardized && conditional) {
    tableName     <- "coefficientsConditional"
    tableTitle    <- gettext("Conditional Meta-Regression Coefficients")
    tablePosition <- 3
  } else if (!standardized && !conditional) {
    tableName     <- "coefficients"
    tableTitle    <- gettext("Meta-Regression Coefficients")
    tablePosition <- 2
  }

  if (!is.null(metaregressionContainer[[tableName]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  coefficientsTable <- createJaspTable(tableTitle)
  coefficientsTable$position <- tablePosition
  coefficientsTable$dependOn(c("confidenceIntervals", "confidenceIntervalsLevels", "includeFullDatasetInSubgroupAnalysis",
                               if (conditional) "conditionalEstimates",
                               if (standardized) "metaregressionStandardizedCoefficientEstimates" else "metaregressionCoefficientEstimates"))
  metaregressionContainer[[tableName]] <- coefficientsTable

  coefficientsTable$addColumnInfo(name = "par",  type = "string", title = "")
  .maAddSubgroupColumn(coefficientsTable, options)
  coefficientsTable$addColumnInfo(name = "mean",    type = "number", title = gettext("Mean"))
  coefficientsTable$addColumnInfo(name = "median",  type = "number", title = gettext("Median"))
  .maAddCiColumn(coefficientsTable, options)

  # skip on error
  if ((length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  estimates <- .maSafeRbind(lapply(fit, .robmaRowCoefficientsEstimates, options = options, standardized = standardized, conditional = conditional))
  estimates <- .maSafeOrderAndSimplify(estimates, "par", options)

  # add messages
  if (conditional)
    coefficientsTable$addFootnote(gettext("Conditional estimates are based on models assuming the presence of a given component."))

  coefficientsTable$setData(estimates)
  coefficientsTable$showSpecifiedColumnsOnly <- TRUE

  return()
}

.robmaPublicationBiasWeightfunctionEstimatesTable <- function(jaspResults, options, conditional = FALSE) {

  publicationBiasContainer <- .robmaExtractPublicationBiasContainer(jaspResults)

  # get table settings
  if (conditional) {
    tableName     <- "coefficientsConditionalWeightfunction"
    tableTitle    <- gettext("Conditional Publication Bias Adjustment Estimates (Weight Function)")
    tablePosition <- 3
  } else {
    tableName     <- "coefficientsWeightfunction"
    tableTitle    <- gettext("Publication Bias Adjustment Estimates (Weight Function)")
    tablePosition <- 1
  }

  if (!is.null(publicationBiasContainer[[tableName]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  coefficientsTable <- createJaspTable(tableTitle)
  coefficientsTable$position <- tablePosition
  coefficientsTable$dependOn(c("confidenceIntervals", "includeFullDatasetInSubgroupAnalysis",
                               if (conditional) "conditionalEstimates",
                               "publicationBiasAdjustmentWeightfunctionEstimates"))
  publicationBiasContainer[[tableName]] <- coefficientsTable

  coefficientsTable$addColumnInfo(name = "lowerRange", type = "number", title = gettext("Lower"), overtitle = gettext("<em>p</em>-Values Interval"))
  coefficientsTable$addColumnInfo(name = "upperRange", type = "number", title = gettext("Upper"), overtitle = gettext("<em>p</em>-Values Interval"))
  .maAddSubgroupColumn(coefficientsTable, options)
  coefficientsTable$addColumnInfo(name = "mean",    type = "number", title = gettext("Mean"))
  coefficientsTable$addColumnInfo(name = "median",  type = "number", title = gettext("Median"))
  .maAddCiColumn(coefficientsTable, options)

  # skip on error
  if ((length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  estimates <- .maSafeRbind(lapply(fit, .robmaRowCoefficientsWeightfunctionEstimates, options = options, conditional = conditional))
  estimates <- .maSafeOrderAndSimplify(estimates, "par", options)

  # add messages
  if (conditional)
    coefficientsTable$addFootnote(gettext("Conditional estimates are based on models assuming the presence of a given component."))

  coefficientsTable$setData(estimates)
  coefficientsTable$showSpecifiedColumnsOnly <- TRUE

  return()
}

.robmaPublicationBiasPetPeeseEstimatesTable       <- function(jaspResults, options, conditional = FALSE) {

  publicationBiasContainer <- .robmaExtractPublicationBiasContainer(jaspResults)

  # get table settings
  if (conditional) {
    tableName     <- "coefficientsConditionalPetPeese"
    tableTitle    <- gettext("Conditional Publication Bias Adjustment Estimates (PET-PEESE)")
    tablePosition <- 4
  } else {
    tableName     <- "coefficientsPetPeese"
    tableTitle    <- gettext("Publication Bias Adjustment Estimates (PET-PEESE)")
    tablePosition <- 2
  }

  if (!is.null(publicationBiasContainer[[tableName]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  coefficientsTable <- createJaspTable(tableTitle)
  coefficientsTable$position <- tablePosition
  coefficientsTable$dependOn(c("confidenceIntervals", "includeFullDatasetInSubgroupAnalysis",
                               if (conditional) "conditionalEstimates",
                               "publicationBiasAdjustmentPetPeeseEstimates"))
  publicationBiasContainer[[tableName]] <- coefficientsTable

  coefficientsTable$addColumnInfo(name = "par", type = "string", title = "")
  .maAddSubgroupColumn(coefficientsTable, options)
  coefficientsTable$addColumnInfo(name = "mean",    type = "number", title = gettext("Mean"))
  coefficientsTable$addColumnInfo(name = "median",  type = "number", title = gettext("Median"))
  .maAddCiColumn(coefficientsTable, options)

  # skip on error
  if ((length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  estimates <- .maSafeRbind(lapply(fit, .robmaRowCoefficientsPetPeeseEstimates, options = options, conditional = conditional))
  estimates <- .maSafeOrderAndSimplify(estimates, "par", options)

  # add messages
  if (conditional)
    coefficientsTable$addFootnote(gettext("Conditional estimates are based on models assuming the presence of a given component."))

  coefficientsTable$setData(estimates)
  coefficientsTable$showSpecifiedColumnsOnly <- TRUE

  return()
}

# Row builders ----

.robmaRowTests                               <- function(fit, options) {
  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(data.frame(
      subgroup = attr(fit, "subgroup")
    ))
  }

  fitSummary <- summary(
    fit,
    logBF = options[["bayesFactorType"]] == "LogBF10",
    BF01  = options[["bayesFactorType"]] == "BF01"
  )[["components"]]

  # remove hierarchical if exists
  fitSummary <- fitSummary[!grepl("Hierarchical", rownames(fitSummary)),, drop = FALSE]

  row <- data.frame(
    subgroup  = attr(fit, "subgroup"),
    test      = sapply(rownames(fitSummary), .robmaComponentNames, options = options),
    priorProb = fitSummary[["prior_prob"]],
    postProb  = fitSummary[["post_prob"]],
    bf        = fitSummary[["inclusion_BF"]]
  )

  return(row)
}

.robmaRowPooledEstimates                     <- function(fit, options, conditional) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(data.frame(
      subgroup = attr(fit, "subgroup")
    ))
  }

  # construct the rows
  tempRows <- list()
  tempRows[["effectSize"]] <- data.frame(.robmaComputePooledEffect(fit, options, conditional))

  # add adjusted effect size for meta-regression since they match the meta-analytic test
  if (.maIsMetaregression(options) && .robmaIsMetaregressionCentered(options)) {
    tempRows[["adjustedEffectSize"]] <- data.frame(.robmaComputeAdjustedEffect(fit, options, conditional))
  } else if (.maIsMetaregression(options) && !.robmaIsMetaregressionCentered(options)) {
    tempRows[["interceptEffectSize"]] <- data.frame(.robmaComputeInterceptEffect(fit, options, conditional))
  }

  # heterogeneity summary
  hetSummary <- RoBMA::summary_heterogeneity(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]

  if (options[["heterogeneityTau"]])
    tempRows[["heterogeneityTau"]] <- data.frame(
      par    = "\U1D70F",
      mean   = hetSummary["tau", "Mean"],
      median = hetSummary["tau", "Median"],
      lCi    = hetSummary["tau", 3],
      uCi    = hetSummary["tau", 4]
    )
  if (options[["heterogeneityTau"]] && options[["studyLevelMultilevel"]] != "") {
    effSummary <- summary(
      fit,
      conditional = conditional,
      probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
    )[[if (conditional) "estimates_conditional" else "estimates"]]
    tempRows[["heterogeneityRho"]] <- data.frame(
      par    = "\U03C1",
      mean   = effSummary["rho", "Mean"],
      median = effSummary["rho", "Median"],
      lCi    = effSummary["rho", 3],
      uCi    = effSummary["rho", 4]
    )
  }
  if (options[["heterogeneityTau2"]])
    tempRows[["heterogeneityTau2"]] <- data.frame(
      par    = "\U1D70F\U00B2",
      mean   = hetSummary["tau2", "Mean"],
      median = hetSummary["tau2", "Median"],
      lCi    = hetSummary["tau2", 3],
      uCi    = hetSummary["tau2", 4]
    )
  if (options[["heterogeneityI2"]])
    tempRows[["heterogeneityI2"]] <- data.frame(
      par    = "I\U00B2",
      mean   = hetSummary["I2", "Mean"],
      median = hetSummary["I2", "Median"],
      lCi    = hetSummary["I2", 3],
      uCi    = hetSummary["I2", 4]
    )
  if (options[["heterogeneityH2"]])
    tempRows[["heterogeneityH2"]] <- data.frame(
      par    = "H\U00B2",
      mean   = hetSummary["H2", "Mean"],
      median = hetSummary["H2", "Median"],
      lCi    = hetSummary["H2", 3],
      uCi    = hetSummary["H2", 4]
    )

  tempRows <- .maSafeRbind(tempRows)
  tempRows$subgroup <- attr(fit, "subgroup")

  return(tempRows)
}

.robmaRowTermTests                           <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  fitSummary <- summary(
    fit,
    logBF = options[["bayesFactorType"]] == "LogBF10",
    BF01  = options[["bayesFactorType"]] == "BF01"
  )[["components_predictors"]]

  if (is.null(fitSummary)) {
    return(NULL)
  }

  row <- data.frame(
    subgroup  = attr(fit, "subgroup"),
    term      = .maVariableNames(rownames(fitSummary), variables = options[["predictors"]]),
    priorProb = fitSummary[["prior_prob"]],
    postProb  = fitSummary[["post_prob"]],
    bf        = fitSummary[["inclusion_BF"]]
  )

  return(row)
}

.robmaRowCoefficientsEstimates               <- function(fit, options, standardized, conditional) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  fitSummary <- summary(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_predictors_conditional" else "estimates_predictors"]]

  estimates <- data.frame(
    par    = rownames(fitSummary),
    mean   = fitSummary[["Mean"]],
    median = fitSummary[["Median"]],
    lCi    = fitSummary[[3]],
    uCi    = fitSummary[[4]]
  )

  if (!standardized) {

    tempData <- attr(fit[["data"]][["predictors"]], "variables_info")
    for (i in seq_along(tempData)) {
      if (tempData[[i]][["type"]] == "continuous") {
        estimates[estimates$par == "intercept", c("mean", "median", "lCi", "uCi")]        <- estimates[estimates$par == "intercept", c("mean", "median", "lCi", "uCi")] -
          estimates[estimates$par == names(tempData)[i], "mean"] * (tempData[[i]][["mean"]] / tempData[[i]][["sd"]])
        estimates[estimates$par == names(tempData)[i], c("mean", "median", "lCi", "uCi")] <- estimates[estimates$par == names(tempData)[i], c("mean", "median", "lCi", "uCi")] / tempData[[i]][["sd"]]
      }
    }
  }

  estimates$par      <- .robmaVariableNames(estimates$par, variables = options[["predictors"]])
  estimates$subgroup <- attr(fit, "subgroup")

  return(estimates)
}

.robmaRowCoefficientsWeightfunctionEstimates <- function(fit, options, conditional) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  fitSummary <- summary(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]

  estimates <- data.frame(
    par    = rownames(fitSummary),
    mean   = fitSummary[["Mean"]],
    median = fitSummary[["Median"]],
    lCi    = fitSummary[[3]],
    uCi    = fitSummary[[4]]
  )
  estimates <- estimates[grepl("omega", estimates$par),,drop=FALSE]

  # get p-value intervals
  estimatesInterval <- gsub("omega", "", estimates$par)
  estimatesInterval <- gsub("[", "", gsub("]", "", estimatesInterval, fixed = TRUE), fixed = TRUE)
  estimatesInterval <- data.frame(do.call(rbind, lapply(estimatesInterval, function(x) strsplit(x, split = ",", fixed = TRUE)[[1]])))
  estimates$lowerRange <- estimatesInterval[,1]
  estimates$upperRange <- estimatesInterval[,2]

  estimates$subgroup <- attr(fit, "subgroup")

  return(estimates)
}

.robmaRowCoefficientsPetPeeseEstimates       <- function(fit, options, conditional) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  fitSummary <- summary(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]

  estimates <- data.frame(
    par    = rownames(fitSummary),
    mean   = fitSummary[["Mean"]],
    median = fitSummary[["Median"]],
    lCi    = fitSummary[[3]],
    uCi    = fitSummary[[4]]
  )
  estimates <- estimates[grepl("PET", estimates$par) | grepl("PEESE", estimates$par),,drop=FALSE]

  estimates$subgroup <- attr(fit, "subgroup")

  return(estimates)
}
