# Robust Bayesian meta-analysis model fitting.
#
# Fits RoBMA models and attaches marginal summaries used by output builders.

.robmaFitModelFun            <- function(dataset, options, subgroupName) {
  # --------------------------------------------------------------------------- #
  # when updating don't forget to update the '.robmaMakeRobmaCallText' function! #
  # --------------------------------------------------------------------------- #

  # obtain prior distributions
  priors <- attr(options, "priors")

  # dispatch between a meta-regression and a meta-analysis data specification
  if (.maIsMetaregression(options)) {

    # dispatch the specified effect size measure
    if (options[["analysis"]] %in% c("RoBMA", "NoBMA")) {
      fitData <- dataset[, c(options[["effectSize"]], options[["effectSizeStandardError"]], options[["predictors"]])]
      colnames(fitData)[1:2] <- switch(
        options[["effectSizeMeasure"]],
        "SMD"      = c("d", "se"),
        "fishersZ" = c("z", "se"),
        "logOR"    = c("logOR", "se"),
        c("y", "se")
      )
    } else if (options[["analysis"]] == "BiBMA") {
      fitData <- dataset[, c(options[["successesGroup1"]], options[["successesGroup2"]], options[["sampleSizeGroup1"]], options[["sampleSizeGroup2"]], options[["predictors"]])]
      colnames(fitData)[1:4] <- c("x1", "x2", "n1", "n2")
    }


    # specify meta-regression
    fitFormula <- .maGetFormula(options[["effectSizeModelTerms"]], TRUE)

    # get moderation priors
    priorsModerators <- priors[["moderators"]]

    # core of the meta-regression call
    fitCall <- list(
      formula = fitFormula,
      data    = fitData,
      priors  = priorsModerators
    )

  } else {

    # dispatch the specified effect size measure
    if (options[["analysis"]] %in% c("RoBMA", "NoBMA")) {
      fitCall <- list(
        "es" = dataset[[options[["effectSize"]]]],
        "se" = dataset[[options[["effectSizeStandardError"]]]]
      )
      names(fitCall)[1] <- switch(
        options[["effectSizeMeasure"]],
        "SMD"      = "d",
        "fishersZ" = "z",
        "logOR"    = "logOR",
        "y"
      )
    } else if (options[["analysis"]] == "BiBMA") {
      fitCall <- list(
        "x1" = dataset[[options[["successesGroup1"]]]],
        "x2" = dataset[[options[["successesGroup2"]]]],
        "n1" = dataset[[options[["sampleSizeGroup1"]]]],
        "n2" = dataset[[options[["sampleSizeGroup2"]]]]
      )
    }

  }

  # add 3rd level
  if (options[["studyLevelMultilevel"]] != "")
    fitCall$study_id <- dataset[[options[["studyLevelMultilevel"]]]]

  # add prior settings
  if (options[["analysis"]] %in% c("RoBMA", "NoBMA")) {
    fitCall$prior_scale <- switch(
      options[["effectSizeMeasure"]],
      "SMD"      = "cohens_d",
      "fishersZ" = "fishers_z",
      "logOR"    = "logOR",
      "none"
    )
    if (options[["effectSizeMeasure"]] %in% c("SMD", "fishersZ", "logOR")) {
      fitCall$transformation <- "fishers_z"
    } else {
      fitCall$transformation <- "none"
    }
  }

  fitCall$priors_effect             <- if (is.null(priors[["effect"]]))            list() else priors[["effect"]]
  fitCall$priors_heterogeneity      <- if (is.null(priors[["heterogeneity"]]))     list() else priors[["heterogeneity"]]
  fitCall$priors_effect_null        <- if (is.null(priors[["effectNull"]]))        list() else priors[["effectNull"]]
  fitCall$priors_heterogeneity_null <- if (is.null(priors[["heterogeneityNull"]])) list() else priors[["heterogeneityNull"]]

  if (options[["analysis"]] == "RoBMA") {
    fitCall$priors_bias       <- if (is.null(priors[["bias"]]))     list() else priors[["bias"]]
    fitCall$priors_bias_null  <- if (is.null(priors[["biasNull"]])) list() else priors[["biasNull"]]
    fitCall$effect_direction  <- switch(
      options[["modelExpectedDirectionOfTheEffect"]],
      "detect" = if (median(dataset[[options[["effectSize"]]]]) >= 0) "positive" else "negative",
      options[["modelExpectedDirectionOfTheEffect"]]
    )
  }
  if (options[["analysis"]] == "BiBMA") {
    fitCall$priors_baseline       <- if (is.null(priors[["baseline"]]))     list() else priors[["baseline"]]
    fitCall$priors_baseline_null  <- if (is.null(priors[["baselineNull"]])) list() else priors[["baselineNull"]]
  }


  # sampling settings
  fitCall$chains <- options[["advancedMcmcChains"]]
  fitCall$adapt  <- options[["advancedMcmcAdaptation"]]
  fitCall$burnin <- options[["advancedMcmcBurnin"]]
  fitCall$sample <- options[["advancedMcmcSamples"]]
  fitCall$thin   <- options[["advancedMcmcThin"]]

  # autofit settings
  fitCall$autofit         <- options[["autofit"]]
  fitCall$autofit_control <- RoBMA::set_autofit_control(
    max_Rhat      = if (options[["advancedAutofitRHat"]])        options[["advancedAutofitRHatTarget"]],
    min_ESS       = if (options[["advancedAutofitEss"]])         options[["advancedAutofitEssTarget"]],
    max_error     = if (options[["advancedAutofitMcmcError"]])   options[["advancedAutofitMcmcErrorTarget"]],
    max_SD_error  = if (options[["advancedAutofitMcmcErrorSd"]]) options[["advancedAutofitMcmcErrorSdTarget"]],
    max_time      = if (options[["advancedAutofitMaximumFittingTime"]]) list(
      time = options[["advancedAutofitMaximumFittingTimeTarget"]],
      unit = options[["advancedAutofitMaximumFittingTimeTargetUnit"]]),
    sample_extend = options[["advancedAutofitExtendSamples"]]
  )

  # additional settings
  fitCall$seed      <- .getSeedJASP(options)
  fitCall$algorithm <- "ss"
  fitCall$silent    <- TRUE

  # add progress bar settings
  fitCall$is_JASP        <- TRUE
  fitCall$is_JASP_prefix <- if (subgroupName != gettext("Full dataset")) subgroupName

  # select fitting function
  fitFunc <- switch (
    options[["analysis"]],
    "RoBMA" = if (.maIsMetaregression(options)) RoBMA::RoBMA.reg else RoBMA::RoBMA,
    "NoBMA" = if (.maIsMetaregression(options)) RoBMA::NoBMA.reg else RoBMA::NoBMA,
    "BiBMA" = if (.maIsMetaregression(options)) RoBMA::BiBMA.reg else RoBMA::BiBMA
  )

  fit <- try(do.call(fitFunc, fitCall))

  # add attributes
  attr(fit, "subgroup") <- paste0(subgroupName)
  attr(fit, "dataset")  <- dataset

  # return the results
  return(list(fit = fit))
}

.robmaAddMarginalSummary     <- function(jaspResults, options) {

  # check whether it was already computed
  if (!is.null(jaspResults[["marginalSummary"]]))
   return()

  # skip if no meta-regression is performed
  if (!.maIsMetaregression(options))
    return()

  # do not use .maExtractFit as all fits needs to be always updated because of forest plot
  fit <- jaspResults[["fit"]]$object

  # skip if no fit, or only single fit requested and failed
  if (length(fit) == 0 || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  ### compute and add the marginal summary
  # create the output container
  marginalSummary <- createJaspState()
  marginalSummary$dependOn(c(.robmaDependencies, "confidenceIntervalsLevel"))
  jaspResults[["marginalSummary"]] <- marginalSummary

  startProgressbar(expectedTicks = length(fit), label = gettext("Estimating Marginal Means"))
  for (i in seq_along(fit)) {

    ### compute the marginal summary
    marginalSummary <- try(RoBMA::marginal_summary(
      object      = fit[[i]][["fit"]],
      conditional = TRUE,
      probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
    ))

    ### add the adjusted estimate
    adjustedEstimate            <- .robmaComputeAdjustedEffect(fit[[i]][["fit"]], options, conditional = FALSE)
    adjustedEstimateConditional <- .robmaComputeAdjustedEffect(fit[[i]][["fit"]], options, conditional = TRUE)

    if (.robmaIsMetaregressionCentered(options)) {
      # the BF for the adjusted estimate is available only for the centered model parameterization
      fitSummary <- summary(
        fit[[i]][["fit"]]
      )[["components"]]
      adjustedEstimatBf <- fitSummary[rownames(fitSummary) == "Effect", "inclusion_BF"]
    } else {
      adjustedEstimatBf <- NA
    }
    marginalSummary[["estimates"]]["intercept",1:5]             <- c(unlist(adjustedEstimate[2:5]),            adjustedEstimatBf)
    marginalSummary[["estimates_conditional"]]["intercept",1:5] <- c(unlist(adjustedEstimateConditional[2:5]), adjustedEstimatBf)


    attr(fit[[i]][["fit"]], "marginalSummary") <- marginalSummary
    progressbarTick()
  }

  jaspResults[["fit"]]$object <- fit
  marginalSummary$object <- TRUE

  return()
}
