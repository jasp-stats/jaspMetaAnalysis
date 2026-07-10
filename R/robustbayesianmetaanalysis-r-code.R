# Robust Bayesian meta-analysis R-code output.
#
# Constructs and displays reproducible RoBMA calls.

.robmaShowRobmaRCode                     <- function(jaspResults, options) {

  if (!.maReady(options) || !is.null(jaspResults[["robmaRCode"]]))
    return()

  robmaRCode <- createJaspHtml(title = gettext("RoBMA R Code"))
  robmaRCode$dependOn(c(.robmaDependencies, "showRoBMARCode"))
  robmaRCode$position <- 99

  robmaRCode$text <- .maTransformToHtml(.robmaMakeRobmaCallText(options))

  jaspResults[["robmaRCode"]] <- robmaRCode

  return()
}

.robmaMakeRobmaCallText   <- function(options) {

  # obtain prior distributions
  priors <- attr(options, "priors")

  # dispatch between a meta-regression and a meta-analysis data specification
  if (.maIsMetaregression(options)) {

    # dispatch the specified effect size measure
    if (options[["analysis"]] %in% c("RoBMA", "NoBMA")) {

      colnamesData <- switch(
        options[["effectSizeMeasure"]],
        "SMD"      = c("d", "se"),
        "fishersZ" = c("z", "se"),
        "logOR"    = c("logOR", "se"),
        c("y", "se")
      )

      fitData <- paste0(
        "colnames(dataset)[colnames(dataset) == '", options[["effectSize"]], "'] <- '", colnamesData[1], "'\n",
        "colnames(dataset)[colnames(dataset) == '", options[["effectSizeStandardError"]], "'] <- '", colnamesData[2], "'\n\n"
      )
      effSizeName <- colnamesData[1]

    } else if (options[["analysis"]] == "BiBMA") {

      fitData <- paste0(
        "colnames(dataset)[colnames(dataset) == '", options[["successesGroup1"]], "'] <- 'x1'\n",
        "colnames(dataset)[colnames(dataset) == '", options[["successesGroup2"]], "'] <- 'x2'\n",
        "colnames(dataset)[colnames(dataset) == '", options[["sampleSizeGroup1"]], "'] <- 'n1'\n",
        "colnames(dataset)[colnames(dataset) == '", options[["sampleSizeGroup2"]], "'] <- 'n2'\n\n"
      )
    }


    # specify meta-regression
    fitFormula <- .maGetFormula(options[["effectSizeModelTerms"]], TRUE)

    # get moderation priors
    priorsModerators <- priors[["moderators"]]

    # core of the meta-regression call
    fitCall <- list(
      formula = fitFormula,
      data    = "dataset",
      priors  = .robmaPrintPriorList(priorsModerators)
    )

  } else {

    fitData <- ""

    # dispatch the specified effect size measure
    if (options[["analysis"]] %in% c("RoBMA", "NoBMA")) {
      fitCall <- list(
        "es" = paste0("dataset[['", options[["effectSize"]], "']]"),
        "se" = paste0("dataset[['", options[["effectSizeStandardError"]], "']]")
      )
      names(fitCall)[1] <- switch(
        options[["effectSizeMeasure"]],
        "SMD"      = "d",
        "fishersZ" = "z",
        "logOR"    = "logOR",
        "y"
      )
      effSizeName <- options[["effectSize"]]

    } else if (options[["analysis"]] == "BiBMA") {
      fitCall <- list(
        "x1" = paste0("dataset[['", options[["successesGroup1"]], "']]"),
        "x2" = paste0("dataset[['", options[["successesGroup2"]], "']]"),
        "n1" = paste0("dataset[['", options[["sampleSizeGroup1"]], "']]"),
        "n2" = paste0("dataset[['", options[["sampleSizeGroup2"]], "']]")
      )
    }

  }

  # add 3rd level
  if (options[["studyLevelMultilevel"]] != "")
    fitCall$study_id <- paste0("dataset[['", options[["studyLevelMultilevel"]], "']]")

  # add prior settings
  if (options[["analysis"]] %in% c("RoBMA", "NoBMA")) {
    fitCall$prior_scale <- switch(
      options[["effectSizeMeasure"]],
      "SMD"      =  paste0("'cohens_d'"),
      "fishersZ" =  paste0("'fishers_z'"),
      "logOR"    =  paste0("'logOR'"),
      "none"
    )
    if (options[["effectSizeMeasure"]] %in% c("SMD", "fishersZ", "logOR")) {
      fitCall$transformation <- "'fishers_z'"
    } else {
      fitCall$transformation <- "'none'"
    }
  }

  fitCall$priors_effect             <- if (is.null(priors[["effect"]]))            list() else .robmaPrintPriorComponent(priors[["effect"]])
  fitCall$priors_heterogeneity      <- if (is.null(priors[["heterogeneity"]]))     list() else .robmaPrintPriorComponent(priors[["heterogeneity"]])
  fitCall$priors_effect_null        <- if (is.null(priors[["effectNull"]]))        list() else .robmaPrintPriorComponent(priors[["effectNull"]])
  fitCall$priors_heterogeneity_null <- if (is.null(priors[["heterogeneityNull"]])) list() else .robmaPrintPriorComponent(priors[["heterogeneityNull"]])

  if (options[["analysis"]] == "RoBMA") {
    fitCall$priors_bias       <- if (is.null(priors[["bias"]]))     list() else .robmaPrintPriorComponent(priors[["bias"]])
    fitCall$priors_bias_null  <- if (is.null(priors[["biasNull"]])) list() else .robmaPrintPriorComponent(priors[["biasNull"]])
    fitCall$effect_direction  <- switch(
      options[["modelExpectedDirectionOfTheEffect"]],
      "detect" = paste0("if (median(dataset[['", effSizeName, "']]) >= 0) 'positive' else 'negative'"),
      paste0("'", options[["modelExpectedDirectionOfTheEffect"]], "'")
    )
  }
  if (options[["analysis"]] == "BiBMA") {
    fitCall$priors_baseline       <- if (is.null(priors[["baseline"]]))     list() else .robmaPrintPriorComponent(priors[["baseline"]])
    fitCall$priors_baseline_null  <- if (is.null(priors[["baselineNull"]])) list() else .robmaPrintPriorComponent(priors[["baselineNull"]])
  }


  # sampling settings
  fitCall$chains <- options[["advancedMcmcChains"]]
  fitCall$adapt  <- options[["advancedMcmcAdaptation"]]
  fitCall$burnin <- options[["advancedMcmcBurnin"]]
  fitCall$sample <- options[["advancedMcmcSamples"]]
  fitCall$thin   <- options[["advancedMcmcThin"]]

  # autofit settings
  fitCall$autofit         <- options[["autofit"]]
  if (options[["autofit"]]) {
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
  }


  # additional settings
  fitCall$seed      <- .getSeedJASP(options)
  fitCall$algorithm <- paste0("'ss'")
  # fitCall$silent    <- TRUE

  # select fitting function
  fitFunc <- switch (
    options[["analysis"]],
    "RoBMA" = if (.maIsMetaregression(options)) "RoBMA.reg" else "RoBMA",
    "NoBMA" = if (.maIsMetaregression(options)) "NoBMA.reg" else "NoBMA",
    "BiBMA" = if (.maIsMetaregression(options)) "BiBMA.reg" else "BiBMA"
  )

  fit <- paste0(
    fitData,
    paste0("fit <- ", fitFunc, "(\n\t", paste(names(fitCall), "=", fitCall, collapse = ",\n\t"), "\n)\n")
  )

  return(fit)
}
