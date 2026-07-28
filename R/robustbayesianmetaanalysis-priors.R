# Robust Bayesian meta-analysis priors.
#
# Maps, rescales, attaches, and prints prior specifications.

.robmaAttachPriors             <- function(options) {

  object <- list()

  # the "General" Cochrane prior distribution needs to be renamed to "Cochrane" for proper dispatching
  if (options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]] == "general")
    options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]] <- "Cochrane"

  # effect size & heterogeneity ----
  # the default (= psychology) priors are defined on smd scale, but they can be transformed to logOR/Fisher's z
  # the medicine priors for logOR, and SMD are defined on logOR and SMD scales, we use the SMD ones to be transformed into Fisher's z
  # the remaining medicine priors (logRR, logHR, RD) are on the appropriate scales already and cannot be transformed
  # (note that also all publication bias priors but PEESE prior is scale independent)
  if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] %in% c("default", "psychology")) {

    object[["effect"]]        <- list(RoBMA::set_default_priors("effect",        rescale = options[["priorDistributionsScale"]]))
    object[["heterogeneity"]] <- list(RoBMA::set_default_priors("heterogeneity", rescale = options[["priorDistributionsScale"]]))

    if (options[["effectSizeMeasure"]] == "fishersZ") {
      object[["effect"]]        <- lapply(object[["effect"]]       , .robmaRescalePriorDistribution, scale = 0.5)
      object[["heterogeneity"]] <- lapply(object[["heterogeneity"]], .robmaRescalePriorDistribution, scale = 0.5)
    } else if (options[["effectSizeMeasure"]] == "logOR") {
      object[["effect"]]        <- lapply(object[["effect"]]       , .robmaRescalePriorDistribution, scale = 1.813799)
      object[["heterogeneity"]] <- lapply(object[["heterogeneity"]], .robmaRescalePriorDistribution, scale = 1.813799)
    }

  } else if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] == "medicine") {

    if (options[["effectSizeMeasure"]] == "fishersZ") {
      object[["effect"]]        <- list(.robmaRescalePriorDistribution(RoBMA::prior_informed(options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]], parameter = "effect",        type = "SMD"), options[["priorDistributionsScale"]]))
      object[["heterogeneity"]] <- list(.robmaRescalePriorDistribution(RoBMA::prior_informed(options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]], parameter = "heterogeneity", type = "SMD"), options[["priorDistributionsScale"]]))
      object[["effect"]]        <- lapply(object[["effect"]]       , .robmaRescalePriorDistribution, scale = 0.5)
      object[["heterogeneity"]] <- lapply(object[["heterogeneity"]], .robmaRescalePriorDistribution, scale = 0.5)
    } else {
      object[["effect"]]        <- list(.robmaRescalePriorDistribution(RoBMA::prior_informed(options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]], parameter = "effect",        type = options[["effectSizeMeasure"]]), options[["priorDistributionsScale"]]))
      object[["heterogeneity"]] <- list(.robmaRescalePriorDistribution(RoBMA::prior_informed(options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]], parameter = "heterogeneity", type = options[["effectSizeMeasure"]]), options[["priorDistributionsScale"]]))
    }

  } else if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] == "custom") {

    object[["effect"]]        <- lapply(options[["priorsEffect"]],        .robmaExtractPriorsFromOptions, type = "continuous")
    object[["heterogeneity"]] <- lapply(options[["priorsHeterogeneity"]], .robmaExtractPriorsFromOptions, type = "continuous")

  }

  # null prior distributions
  if (options[["bayesianModelAveragingEffectSize"]] && options[["priorDistributionsEffectSizeAndHeterogeneity"]] != "custom") {
    object[["effectNull"]]    <- list(RoBMA::set_default_priors("effect", null = TRUE))
  } else if (options[["bayesianModelAveragingEffectSize"]]) {
    object[["effectNull"]]    <- lapply(options[["priorsEffectNull"]], .robmaExtractPriorsFromOptions, type = "continuous")
  } else {
    object[["effectNull"]]    <- list()
  }
  if (options[["bayesianModelAveragingHeterogeneity"]] && options[["priorDistributionsEffectSizeAndHeterogeneity"]] != "custom") {
    object[["heterogeneityNull"]] <- list(RoBMA::set_default_priors("heterogeneity", null = TRUE))
  } else if (options[["bayesianModelAveragingHeterogeneity"]]) {
    object[["heterogeneityNull"]] <- lapply(options[["priorsHeterogeneityNull"]], .robmaExtractPriorsFromOptions, type = "continuous")
  } else {
    object[["heterogeneityNull"]] <- list()
  }

  # publication bias ----
  # the PEESE prior distribution scales with the inverse of the effect size transformation
  # (it's on the relationship between es and se^2; the PET prior is effect size transformation invariant)
  # (it also scales with 1/scale^2 for width of the prior distribution, as smaller prior means lower association between es and se^2)
  if (options[["analysis"]] == "RoBMA") {
    if (options[["publicationBiasAdjustment"]] == "PSMA") {

      # select all priors
      tempPriors <- RoBMA::set_default_priors("bias", rescale = options[["priorDistributionsScale"]])

      # effect size re-scaling for PEESE
      if (options[["effectSizeMeasure"]] == "fishersZ") {
        tempPriors[[8]] <- .robmaRescalePriorDistribution(tempPriors[[8]], 1/0.5)
      } else if (options[["effectSizeMeasure"]] == "logOR") {
        tempPriors[[8]] <- .robmaRescalePriorDistribution(tempPriors[[8]], 1/1.813799)
      }

      object[["bias"]] <- tempPriors

    } else if (options[["publicationBiasAdjustment"]] == "PP") {

      # select PET-PEESE priors only
      tempPriors <- RoBMA::set_default_priors("bias", rescale = .robmaPriorBiasScale(options))[7:8]

      # re-standardize the prior model probability
      for (i in seq_along(tempPriors)) {
        tempPriors[[i]][["prior_weights"]] <- 1/2
      }

      # effect size re-scaling for PEESE
      if (options[["effectSizeMeasure"]] == "fishersZ") {
        tempPriors[[2]] <- .robmaRescalePriorDistribution(tempPriors[[2]], 1/0.5)
      } else if (options[["effectSizeMeasure"]] == "logOR") {
        tempPriors[[2]] <- .robmaRescalePriorDistribution(tempPriors[[2]], 1/1.813799)
      }

      object[["bias"]] <- tempPriors

    } else if (options[["publicationBiasAdjustment"]] == "original") {

      # select the first two weight functions only
      tempPriors <- RoBMA::set_default_priors("bias", rescale = .robmaPriorBiasScale(options))[1:2]

      # re-standardize the prior model probability
      for (i in seq_along(tempPriors)) {
        tempPriors[[i]][["prior_weights"]] <- 1/2
      }

      object[["bias"]] <- tempPriors

    } else if (options[["publicationBiasAdjustment"]] == "custom") {

      object[["bias"]] <- c(
        lapply(options[["priorsBiasSelectionModels"]], .robmaExtractPriorsFromOptions, type = "weightfunction"),
        lapply(options[["priorsBiasPet"]],             .robmaExtractPriorsFromOptions, type = "pet"),
        lapply(options[["priorsBiasPeese"]],           .robmaExtractPriorsFromOptions, type = "peese")
      )

    } else if (options[["publicationBiasAdjustment"]] == "none") {

      object[["bias"]] <- list()

    }

    # null prior distributions
    if (options[["publicationBiasAdjustment"]] == "none") {
      object[["biasNull"]] <- list(RoBMA::set_default_priors("bias", null = TRUE))
    } else if (options[["bayesianModelAveragingPublicationBias"]] && options[["publicationBiasAdjustment"]] != "custom") {
      object[["biasNull"]] <- list(RoBMA::set_default_priors("bias", null = TRUE))
    } else if (options[["bayesianModelAveragingPublicationBias"]]) {
      object[["biasNull"]] <- c(
        lapply(options[["priorsBiasSelectionModelsNull"]], .robmaExtractPriorsFromOptions, type = "weightfunction"),
        lapply(options[["priorsBiasPetNull"]],             .robmaExtractPriorsFromOptions, type = "pet"),
        lapply(options[["priorsBiasPeeseNull"]],           .robmaExtractPriorsFromOptions, type = "peese")
      )
    } else {
      object[["biasNull"]] <- NULL
    }
  }


  # baseline ----
  if (options[["analysis"]] == "BiBMA") {

    if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] != "custom") {
      object[["baseline"]]     <- NULL
      object[["baselineNull"]] <- list(RoBMA::set_default_binomial_priors("baseline", null = TRUE))
    } else {
      object[["baseline"]]     <- lapply(options[["priorsBaseline"]],     .robmaExtractPriorsFromOptions, type = "baseline")
      object[["baselineNull"]] <- lapply(options[["priorsBaselineNull"]], .robmaExtractPriorsFromOptions, type = "baseline")
    }
  }


  # moderation ----
  # the same effect size transformations as to effect sizes apply
  tempObject <- list()
  for (i in seq_along(options[["effectSizeModelTerms"]])) {

    # TODO: enable interactions later on
    # - this will required identifying whether the interaction contains a factor term in the GUI (to be slotted into the proper prior type)

    tempPrior    <- list()
    tempTerm     <- options[["effectSizeModelTerms"]][[i]]$components
    tempTermType <- options[["predictors.types"]][options[["predictors"]] == tempTerm]

    ### alternative distributions
    if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] %in% c("default", "psychology")) {

      tempPrior[["alt"]] <- switch(
        tempTermType,
        "nominal" = RoBMA::set_default_priors("factors",    rescale = options[["priorDistributionsScale"]]),
        "scale"   = RoBMA::set_default_priors("covariates", rescale = options[["priorDistributionsScale"]])
      )

      if (options[["effectSizeMeasure"]] == "fishersZ") {
        tempPrior[["alt"]] <- .robmaRescalePriorDistribution(tempPrior[["alt"]], scale = 0.5)
      } else if (options[["effectSizeMeasure"]] == "logOR") {
        tempPrior[["alt"]] <- .robmaRescalePriorDistribution(tempPrior[["alt"]], scale = 1.813799)
      }

    } else if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] == "medicine") {

      # medicine priors are more narrow than psychology priors (there are no default priors for moderator yet - use 1/2 of the effect size prior scaling)
      if (options[["effectSizeMeasure"]] == "fishersZ") {

        tempPrior[["alt"]] <- switch(
          tempTermType,
          "nominal" = .robmaCochraneFactorPrior(options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]], type = "SMD",  options[["priorDistributionsScale"]] / 2),
          "scale"   = .robmaRescalePriorDistribution(RoBMA::prior_informed(options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]], parameter = "effect", type = "SMD"), options[["priorDistributionsScale"]] / 2)
        )
        tempPrior[["alt"]] <- .robmaRescalePriorDistribution(tempPrior[["alt"]], scale = 0.5)

      } else {

        tempPrior[["alt"]] <- switch(
          tempTermType,
          "nominal" = .robmaCochraneFactorPrior(options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]], type = options[["effectSizeMeasure"]], options[["priorDistributionsScale"]] / 2),
          "scale"   = .robmaRescalePriorDistribution(RoBMA::prior_informed(options[["priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"]], parameter = "effect", type = options[["effectSizeMeasure"]]), options[["priorDistributionsScale"]] / 2)
        )

      }

    } else if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] == "custom") {

      tempPrior[["alt"]] <- switch(
        tempTermType,
        "nominal" = .robmaExtractPriorsFromOptions(options[["priorsModeratorsFactor"]][[which(sapply(options[["priorsModeratorsFactor"]], "[[", "value") == tempTerm)]], type = "factor"),
        "scale"   = .robmaExtractPriorsFromOptions(options[["priorsModeratorsContinuous"]][[which(sapply(options[["priorsModeratorsContinuous"]], "[[", "value") == tempTerm)]], type = "continuous")
      )

    }


    ### null distribution prior (make sure that the contrast type matches between the alternative and the null hypothesis)
    if (options[["bayesianModelAveragingModerations"]] && options[["priorDistributionsEffectSizeAndHeterogeneity"]] != "custom") {

      tempPrior[["null"]] <- switch(
        tempTermType,
        "nominal" = RoBMA::prior_factor("spike", list(0), contrast = .robmaPriorGetContrast(tempPrior[["alt"]])),
        "scale"   = RoBMA::set_default_priors("covariates", null = TRUE),
      )

    } else if (options[["bayesianModelAveragingModerations"]]) {

      tempPrior[["null"]] <- switch(
        tempTermType,
        "nominal" = .robmaExtractPriorsFromOptions(options[["priorsModeratorsFactorNull"]][[which(sapply(options[["priorsModeratorsFactorNull"]], "[[", "value") == tempTerm)]], type = "factor"),
        "scale"   = .robmaExtractPriorsFromOptions(options[["priorsModeratorsContinuousNull"]][[which(sapply(options[["priorsModeratorsContinuousNull"]], "[[", "value") == tempTerm)]], type = "continuous"),
      )

      # assert the proper contrast (based on the alternative prior distribution)
      if (tempTermType == "nominal" && BayesTools::is.prior.point(tempPrior[["null"]])) {
        tempPrior[["null"]] <- RoBMA::prior_factor("spike", list(tempPrior[["null"]][["parameters"]][["location"]]), contrast = .robmaPriorGetContrast(tempPrior[["alt"]]))
      }
    }

    # enlist
    tempObject[[tempTerm]] <- tempPrior

  }
  object[["moderators"]] <- tempObject

  # verify that all required priors were specified ----
  if (length(object[["effectNull"]]) == 0 && length(object[["effect"]]) == 0)
    .quitAnalysis(gettext("At least one prior distribution for the Effect component has to be specified."))
  if (length(object[["heterogeneityNull"]]) == 0 && length(object[["heterogeneity"]]) == 0)
    .quitAnalysis(gettext("At least one prior distribution for the Heterogeneity component has to be specified."))
  for (i in seq_along(object[["moderators"]])) {
    if (length(object[["moderators"]][[i]][["alt"]]) == 0 && length(object[["moderators"]][[i]][["null"]]) == 0)
      .quitAnalysis(gettextf("At least one prior distribution for the %1$s moderator component has to be specified.", names(object[["moderators"]])[i]))
    if (length(object[["moderators"]][[i]]) == 2 && BayesTools::is.prior.factor(object[["moderators"]][[i]][[1]]) &&
        !(
          (BayesTools::is.prior.meandif(object[["moderators"]][[i]][[1]]) && BayesTools::is.prior.meandif(object[["moderators"]][[i]][[2]])) ||
          (BayesTools::is.prior.orthonormal(object[["moderators"]][[i]][[1]]) && BayesTools::is.prior.orthonormal(object[["moderators"]][[i]][[2]])) ||
          (BayesTools::is.prior.independent(object[["moderators"]][[i]][[1]]) && BayesTools::is.prior.independent(object[["moderators"]][[i]][[2]])) ||
          (BayesTools::is.prior.treatment(object[["moderators"]][[i]][[1]]) && BayesTools::is.prior.treatment(object[["moderators"]][[i]][[2]]))
        ))
      .quitAnalysis(gettextf("Both priors distributions for the %1$s moderator component must have the same contrast type.", names(object[["moderators"]])[i]))
  }
  if (options[["analysis"]] == "RoBMA" && length(object[["bias"]]) == 0 && length(object[["biasNull"]]) == 0)
    .quitAnalysis(gettext("At least one prior distribution for the Publication Bias component has to be specified."))
  if (options[["analysis"]] == "BiBMA" && length(object[["baseline"]]) == 0 && length(object[["baselineNull"]]) == 0)
    .quitAnalysis(gettext("At least one prior distribution for the Baseline component has to be specified."))


  # attach and return
  attr(options, "priors") <- object
  return(options)
}

.robmaRescalePriorDistribution <- function(prior, scale) {

  # rescale priors as needed
  if (prior[["distribution"]] %in% c("normal", "mnormal")) {
    prior$parameters[["sd"]]   <- prior$parameters[["sd"]]     * scale
  } else if (prior[["distribution"]] %in% c("t", "mt", "invgamma")) {
    prior$parameters[["scale"]] <- prior$parameters[["scale"]] * scale
  } else if (scale != 1) {
    stop("Selected prior distribution cannot be rescaled.")
  }

  return(prior)
}

.robmaExtractPriorsFromOptions <- function(optionsPrior, type) {

  optionsPrior   <- .robmaEvalOptionsToPriors(optionsPrior)

  if (optionsPrior[["type"]] == "none")
    return(switch(
      type,
      "continuous"      = NULL,
      "factor"          = NULL,
      "baseline"        = NULL,
      "weightfunction"  = RoBMA::prior_none(prior_weights = optionsPrior[["priorWeight"]]),
      "pet"             = RoBMA::prior_none(prior_weights = optionsPrior[["priorWeight"]]),
      "peese"           = RoBMA::prior_none(prior_weights = optionsPrior[["priorWeight"]])
    ))
  else
    return(do.call(
      what = switch(
        type,
        "continuous"      = RoBMA::prior,
        "factor"          = RoBMA::prior_factor,
        "weightfunction"  = RoBMA::prior_weightfunction,
        "pet"             = RoBMA::prior_PET,
        "peese"           = RoBMA::prior_PEESE,
        "baseline"        = RoBMA::prior_factor # more magic happens in the function .robmaMapOptionsToPriors function
      ),
      args = .robmaMapOptionsToPriors(optionsPrior, type)
    ))
}

.robmaCleanOptionsToPriors     <- function(x, message = gettext("The priors for publication bias were set incorrectly.")) {

  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")

  x <- strsplit(x, ",", fixed = TRUE)[[1]]

  x <- trimws(x, which = "both")
  x <- x[x != ""]

  if (anyNA(as.numeric(x)))
    .quitAnalysis(message)
  return(as.numeric(x))
}

.robmaEvalOptionsToPriors      <- function(x) {

  if (x[["type"]] %in% c("twoSided", "oneSided")) {
    x[["priorWeight"]] <- eval(parse(text = x[["priorWeight"]]))
    x[["alpha"]]       <- .robmaCleanOptionsToPriors(x[["alpha"]])
    x[["pValues"]]     <- .robmaCleanOptionsToPriors(x[["pValues"]])
  } else if (x[["type"]] %in% c("twoSidedFixed", "oneSidedFixed")) {
    x[["priorWeight"]] <- eval(parse(text = x[["priorWeight"]]))
    x[["omega"]]       <- .robmaCleanOptionsToPriors(x[["omega"]])
    x[["pValues"]]     <- .robmaCleanOptionsToPriors(x[["pValues"]])
  } else if (x[["type"]] == "none") {
    x[["priorWeight"]] <- eval(parse(text = x[["priorWeight"]]))
  } else {
    evalNames <-
      c(
        "a",
        "b",
        "alpha",
        "beta",
        "nu",
        "x0",
        "mu",
        "sigma",
        "theta",
        "k",
        "priorWeight",
        "truncationLower",
        "truncationUpper"
      )
    for (n in evalNames) {
      if (!is.null(x[[n]])) {
        x[[n]] <- gsub("inf", "Inf", x[[n]])
        x[[n]] <- eval(parse(text = x[[n]]))
      }
    }
  }

  return(x)
}

.robmaMapOptionsToPriors       <- function(optionsPrior, type) {

  arguments <- list()

  arguments[["distribution"]] <- switch(
    optionsPrior[["type"]],
    "gammaAB" = "gamma",
    "gammaK0" = "gamma",
    "spike0"  = "spike",
    optionsPrior[["type"]]
  )

  arguments[["parameters"]] <- switch(
    optionsPrior[["type"]],
    "normal"    = list("mean" = optionsPrior[["mu"]], "sd" = optionsPrior[["sigma"]]),
    "mnormal"   = list("mean" = 0, "sd" = optionsPrior[["sigma"]]),
    "t"         = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["sigma"]], "df" = optionsPrior[["nu"]]),
    "mt"        = list("location" = 0, "scale" = optionsPrior[["sigma"]], "df" = optionsPrior[["nu"]]),
    "cauchy"    = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["theta"]]),
    "gammaAB"   = list("shape" = optionsPrior[["alpha"]], "rate" = optionsPrior[["beta"]]),
    "gammaK0"   = list("shape" = optionsPrior[["k"]], "rate" = 1/optionsPrior[["theta"]]),
    "invgamma"  = list("shape" = optionsPrior[["alpha"]], "scale" = optionsPrior[["beta"]]),
    "lognormal" = list("meanlog" = optionsPrior[["mu"]], "sdlog" = optionsPrior[["sigma"]]),
    "beta"      = list("alpha" = optionsPrior[["alpha"]], "beta" = optionsPrior[["beta"]]),
    "uniform"   = list("a" = optionsPrior[["a"]], "b" = optionsPrior[["b"]]),
    "spike"     = list("location" = optionsPrior[["x0"]]),,
    "spike0"    = list("location" = 0),
    "oneSided"  = list("steps" = optionsPrior[["pValues"]], alpha = optionsPrior[["alpha"]]),
    "twoSided"  = list("steps" = optionsPrior[["pValues"]], alpha = optionsPrior[["alpha"]]),
    "oneSidedFixed" = list("steps" = optionsPrior[["pValues"]], omega = optionsPrior[["omega"]]),
    "twoSidedFixed" = list("steps" = optionsPrior[["pValues"]], omega = optionsPrior[["omega"]])
  )

  if (!arguments[["distribution"]] %in% c("oneSided", "twoSided", "oneSidedFixed", "twoSidedFixed", "spike", "uniform", "mnormal", "mt", "spike0")) {
    arguments[["truncation"]] <- list(
      lower   = optionsPrior[["truncationLower"]],
      upper   = optionsPrior[["truncationUpper"]]
    )
  }

  arguments[["prior_weights"]] <- optionsPrior[["priorWeight"]]

  if (type == "factor") {
    arguments[["contrast"]] <- optionsPrior[["contrast"]]
  }

  if (type == "baseline") {
    arguments[["contrast"]] <- "independent"
  }

  return(arguments)
}

.robmaCochraneFactorPrior      <- function(name, type, scale) {

  effectPrior <- RoBMA::prior_informed(name, parameter = "effect", type = type)
  if (effectPrior[["distribution"]] == "t") {
    factorPrior <- RoBMA::prior_factor("mt", list(location = 0, scale = effectPrior[["parameters"]][["scale"]] * scale, df = effectPrior[["parameters"]][["df"]]), contrast = "meandif")
  } else if (effectPrior[["distribution"]] == "normal") {
    factorPrior <- RoBMA::prior_factor("mnormal", list(location = 0, sd = effectPrior[["parameters"]][["sd"]] * scale), contrast = "meandif")
  }

  return(factorPrior)
}

.robmaPriorBiasScale           <- function(options) {

  # default rescale based on cohen's d
  rescale <- options[["priorDistributionsScale"]]
  if (options[["effectSizeMeasure"]] == "SMD") {
    rescale <- rescale
  } else if (options[["effectSizeMeasure"]] == "fishersZ") {
    rescale <- rescale * 1/0.5
  } else if (options[["effectSizeMeasure"]] == "logOR") {
    rescale <- rescale * 1/1.813799
  }

  return(rescale)
}

.robmaPriorGetContrast         <- function(prior) {
  if (BayesTools::is.prior.meandif(prior)) {
    return("meandif")
  } else if (BayesTools::is.prior.orthonormal(prior)) {
    return("orthonormal")
  } else if (BayesTools::is.prior.independent(prior)) {
    return("independent")
  } else {
    stop("Unknown prior type.")
  }
}

.robmaPrintPrior          <- function(thisPrior) {

  if (BayesTools::is.prior.weightfunction(thisPrior)) {
    thisOut <- "prior_weightfunction("
  } else if (BayesTools::is.prior.PET(thisPrior)) {
    thisOut <- "prior_PET("
  } else if (BayesTools::is.prior.PEESE(thisPrior)) {
    thisOut <- "prior_PEESE("
  } else if (BayesTools::is.prior.none(thisPrior)) {
    thisOut <- "prior_none("
  } else if (BayesTools::is.prior.factor(thisPrior)) {
    thisOut <- "prior_factor("
  } else if (BayesTools::is.prior.simple(thisPrior)) {
    thisOut <- "prior("
  }

  if (BayesTools::is.prior.none(thisPrior)) {

    thisOut <- paste0(thisOut, "prior_weights = ", thisPrior$prior_weights, ")")

  } else {

    thisOut <- paste0(thisOut, "distribution = '", thisPrior$distribution, "'")

    if (BayesTools::is.prior.factor(thisPrior)) {
      thisPrior$parameters[["K"]] <- NULL
    }

    if (!is.null(thisPrior$parameters[["steps"]]))
      thisPrior$parameters[["steps"]] <- rev(thisPrior$parameters[["steps"]])

    thisOut <- paste0(thisOut, ", parameters = list(", paste0(names(thisPrior$parameters), " = ", thisPrior$parameters,  collapse = ", "), ")")

    if (!BayesTools::is.prior.weightfunction(thisPrior) && !BayesTools::is.prior.point(thisPrior)) {
      thisOut <- paste0(thisOut, ", truncation = list(", paste0(names(thisPrior$truncation), " = ", thisPrior$truncation,  collapse = ", "), ")")
    }

    if (BayesTools::is.prior.factor(thisPrior)) {
      if (BayesTools::is.prior.orthonormal(thisPrior)) {
        thisOut <- paste0(thisOut, ", contrast = 'orthonormal'")
      } else if (BayesTools::is.prior.meandif(thisPrior)) {
        thisOut <- paste0(thisOut, ", contrast = 'meandif'")
      } else if (BayesTools::is.prior.treatment(thisPrior)) {
        thisOut <- paste0(thisOut, ", contrast = 'treatment'")
      } else if (BayesTools::is.prior.independent(thisPrior)) {
        thisOut <- paste0(thisOut, ", contrast = 'independent'")
      }
    }

    thisOut <- paste0(thisOut, ", prior_weights = ", thisPrior$prior_weights, ")")
  }

  return(thisOut)
}

.robmaPrintPriorComponent <- function(priorList) {

  outList <- "list(\n"

  for (i in seq_along(priorList)) {
    outList <- paste0(outList, "\t\t", .robmaPrintPrior(priorList[[i]]))
    if (i != length(priorList)) {
      outList <- paste0(outList, ",\n")
    }else {
      outList <- paste0(outList, "\n")
    }
  }

  outList <- paste0(outList, "\t)")

  return(outList)
}

.robmaPrintPriorList      <- function(priorList) {

  outList <- "list(\n"

  for (i in seq_along(priorList)) {

    thisPrior <- priorList[[i]]
    thisOut   <- "\t\t"

    outList <- paste0(outList, "\t\t", names(priorList)[i], " = list(\n")

    if (length(priorList[[i]][["alt"]]) != 0) {
      outList <- paste0(outList, "\t\t\talt = ", .robmaPrintPrior(priorList[[i]][["alt"]]))
      if (length(priorList[[i]][["null"]]) != 0) {
        outList <- paste0(outList, ",\n")
        outList <- paste0(outList, "\t\t\tnull = ", .robmaPrintPrior(priorList[[i]][["null"]]))
      }
      outList <- paste0(outList, "\n\t\t)")
    } else if (length(priorList[[i]][["null"]]) != 0) {
      outList <- paste0(outList, "\t\t\tnull = ", .robmaPrintPrior(priorList[[i]][["null"]]))
      outList <- paste0(outList, "\n\t\t)")
    }

    if (i != length(priorList)) {
      outList <- paste0(outList, ",\n")
    }else {
      outList <- paste0(outList, "\n")
    }
  }

  outList <- paste0(outList, "\t)")

  return(outList)
}
