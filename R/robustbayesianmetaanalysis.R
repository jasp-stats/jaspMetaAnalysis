#
# Copyright (C) 2019 University of Amsterdam
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

# TODO
# - custom prior for factor moderators does not work

RobustBayesianMetaAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  library(RoBMA)

  # devel settings
  options[["advancedMcmcChains"]]     <- 2
  options[["advancedMcmcAdaptation"]] <- 500
  options[["advancedMcmcBurnin"]]     <- 1000
  options[["advancedMcmcSamples"]]    <- 2000


  options[["module"]] <- "RoBMA"
  saveRDS(dataset, file = "C:/JASP-Packages/dataset0.RDS")
  if (.maReady(options)) {
    dataset <- .maCheckData(dataset, options)
    .maCheckErrors(dataset, options)
  }
  saveRDS(options, file = "C:/JASP-Packages/options.RDS")
  saveRDS(dataset, file = "C:/JASP-Packages/dataset.RDS")
  # get priors and show model specification table

  if (options[["showModelSpecification"]])
    .robmaModelSpecificationTables(jaspResults, options)

  # fit the model
  .maFitModel(jaspResults, dataset, options)
  .maUpdateFitModelDataset(jaspResults, dataset, options)
  .robmaAddMarginalSummary(jaspResults, options)

  # model summary
  .robmaOverallTestsTable(jaspResults, options)
  .robmaPooledEstimatesTable(jaspResults, options)
  if (options[["conditionalEstimates"]])
    .robmaPooledEstimatesTable(jaspResults, options, conditional = TRUE)

  # meta-regression tables
  if (.maIsMetaregression(options)) {
    if (options[["metaregressionTermTests"]])
      .robmaTermsTable(jaspResults, options)
    if (options[["metaregressionCoefficientEstimates"]])
      .robmaCoefficientEstimatesTable(jaspResults, options)
    if (options[["metaregressionCoefficientEstimates"]] && options[["conditionalEstimates"]])
      .robmaCoefficientEstimatesTable(jaspResults, options, conditional = TRUE)
    if (options[["metaregressionStandardizedCoefficientEstimates"]])
      .robmaCoefficientEstimatesTable(jaspResults, options, standardized = TRUE)
    if (options[["metaregressionStandardizedCoefficientEstimates"]] && options[["conditionalEstimates"]])
      .robmaCoefficientEstimatesTable(jaspResults, options, standardized = TRUE, conditional = TRUE)
  }

  # publication bias adjustment table
  if (options[["publicationBiasAdjustmentWeightfunctionEstimates"]])
    .robmaPublicationBiasWeightfunctionEstimatesTable(jaspResults, options)
  if (options[["publicationBiasAdjustmentWeightfunctionEstimates"]] && options[["conditionalEstimates"]])
    .robmaPublicationBiasWeightfunctionEstimatesTable(jaspResults, options, conditional = TRUE)
  if (options[["publicationBiasAdjustmentPetPeeseEstimates"]])
    .robmaPublicationBiasPetPeeseEstimatesTable(jaspResults, options)
  if (options[["publicationBiasAdjustmentPetPeeseEstimates"]] && options[["conditionalEstimates"]])
    .robmaPublicationBiasPetPeeseEstimatesTable(jaspResults, options, conditional = TRUE)

  # estimated marginal means and contrasts (the whole section is created within the dispatch)
  .robmaEstimatedMarginalMeans(jaspResults, options)

  # pooled estimates plots
  if (options[["priorAndPosteriorPlotEffectSize"]])
    .robmaPriorAndPosteriorPlot(jaspResults, options, "pooledEffect")
  if (options[["priorAndPosteriorPlotHeterogeneity"]])
    .robmaPriorAndPosteriorPlot(jaspResults, options, "heterogeneity")
  if (options[["priorAndPosteriorPlotModeration"]])
    .robmaPriorAndPosteriorPlot(jaspResults, options, "moderation")
  if (options[["priorAndPosteriorPlotWeightFunction"]])
    .robmaPriorAndPosteriorPlot(jaspResults, options, "weightFunction")
  if (options[["priorAndPosteriorPlotPetPeese"]])
    .robmaPriorAndPosteriorPlot(jaspResults, options, "petPeese")

  # plots
  .maUltimateForestPlot(jaspResults, options)

return()
  .maBubblePlot(jaspResults, options)
  #------------
  # get the priors
  .robmaGetPriors(jaspResults, options)

  # show the model Specification
  if (is.null(jaspResults[["model"]]))
    .robmaModelSpecificationTable(jaspResults, options)

  # fit model
  if (is.null(jaspResults[["modelNotifier"]]) && .robmaCheckReady(options))
    .robmaFitModel(jaspResults, dataset, options)

  ### Priors plot
  if (options[["priorDistributionPlot"]])
    .robmaPriorsPlots(jaspResults, options)


  ### Inference
  # default summary
  .robmaSummaryTable(jaspResults, options, type = "RoBMA")
  # models overview
  if (options[["inferenceModelsOverview"]])
    .robmaModelsOverviewTable(jaspResults, options, type = "RoBMA")
  # models summary
  if (options[["inferenceIndividualModels"]])
    .robmaModelsSummaryTable(jaspResults, options, type = "RoBMA")


  ### Plots
  # forest plot
  if (options[["plotsForestPlot"]])
    .robmaForestPlot(jaspResults, options, type = "RoBMA")

  # pooled estimates plots
  if (options[["plotsPooledEstimatesEffect"]])
    .robmaEstimatesPlot(jaspResults, options, "mu", type = "RoBMA")
  if (options[["plotsPooledEstimatesHeterogeneity"]])
    .robmaEstimatesPlot(jaspResults, options, "tau", type = "RoBMA")
  if (options[["plotsPooledEstimatesWeightFunction"]])
    .robmaEstimatesPlot(jaspResults, options, "weightFunction", type = "RoBMA")
  if (options[["plotsPooledEstimatesPetPeese"]])
    .robmaEstimatesPlot(jaspResults, options, "petPeese", type = "RoBMA")

  # individual models
  if (options[["plotsIndividualModelsEffect"]])
    .robmaModelsPlot(jaspResults, options, "mu", type = "RoBMA")
  if (options[["plotsIndividualModelsHeterogeneity"]])
    .robmaModelsPlot(jaspResults, options, "tau", type = "RoBMA")

  ### Diagnostics
  # overview
  if (options[["mcmcDiagnosticsOverviewTable"]])
    .robmaDiagnosticsOverviewTable(jaspResults, options, type = "RoBMA")
  # plots
  if (.robmaCheckDiagnostics(options, any = TRUE))
    .robmaDiagnosticsPlots(jaspResults, options, type = "RoBMA")

  ### Save the model
  if (options[["advancedSaveFittedModel"]] != "" && is.null(jaspResults[["modelSaved"]]))
    .robmaSaveModel(jaspResults, options)


  return()
}

.robmaDependencies <- c(
  "effectSize", "effectSizeStandardError", "predictors", "predictors.types", "subgroup", "effectSizeMeasure",
  "effectSizeModelTerms", "effectSizeModelIncludeIntercept",
  "bayesianModelAveragingEffectSize", "bayesianModelAveragingHeterogeneity", "bayesianModelAveragingModerations", "bayesianModelAveragingPublicationBias",
  "priorDistributionsEffectSizeAndHeterogeneity", "priorDistributionsScale", "publicationBiasAdjustment", "modelExpectedDirectionOfTheEffect",
  # prior distributions
  "modelsEffect", "modelsEffectNull", "modelsHeterogeneity", "modelsHeterogeneityNull",
  "modelsSelectionModels", "modelsSelectionModelsNull", "modelsPet", "modelsPetNull", "modelsPeese", "modelsPeeseNull",

  # MCMC settings
  "advancedMcmcAdaptation", "advancedMcmcBurnin", "advancedMcmcSamples", "advancedMcmcChains", "advancedMcmcThin",
  "autofit", "advancedAutofitRHat", "advancedAutofitRHatTarget", "advancedAutofitEss", "advancedAutofitEssTarget", "advancedAutofitMcmcError",
  "advancedAutofitMcmcErrorTarget", "advancedAutofitMcmcErrorSd", "advancedAutofitMcmcErrorSdTarget", "advancedAutofitMaximumFittingTime",
  "advancedAutofitMaximumFittingTimeTarget", "advancedAutofitMaximumFittingTimeTargetUnit", "advancedAutofitExtendSamples",

  "seed", "setSeed"
)

# model fitting function
.robmaFitModelFun            <- function(dataset, options, subgroupName) {

  # obtain prior distributions
  priors <- .robmaGetPriors(options)

  # dispatch between a meta-regression and a meta-analysis data specification
  if (.maIsMetaregression(options)) {

    # dispatch the specified effect size measure
    fitData <- dataset[, c(options[["effectSize"]], options[["effectSizeStandardError"]], options[["predictors"]])]
    colnames(fitData)[1:2] <- switch(
      options[["effectSizeMeasure"]],
      "SMD"      = c("d", "se"),
      "fishersZ" = c("z", "se"),
      "logOR"    = c("logOR", "se"),
      c("y", "se")
    )

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
    fitCall <- list(
      "es" = dataset[, options[["effectSize"]]],
      "se" = dataset[, options[["effectSizeStandardError"]]]
    )
    names(fitCall)[1] <- switch(
      options[["effectSizeMeasure"]],
      "SMD"      = "d",
      "fishersZ" = "z",
      "logOR"    = "logOR",
      "y"
    )
  }

  # add prior settings
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
  fitCall$priors_effect             <- if (is.null(priors[["effect"]]))            list() else priors[["effect"]]
  fitCall$priors_heterogeneity      <- if (is.null(priors[["heterogeneity"]]))     list() else priors[["heterogeneity"]]
  fitCall$priors_bias               <- if (is.null(priors[["bias"]]))              list() else priors[["bias"]]
  fitCall$priors_effect_null        <- if (is.null(priors[["effectNull"]]))        list() else priors[["effectNull"]]
  fitCall$priors_heterogeneity_null <- if (is.null(priors[["heterogeneityNull"]])) list() else priors[["heterogeneityNull"]]
  fitCall$priors_bias_null          <- if (is.null(priors[["biasNull"]]))          list() else priors[["biasNull"]]

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

  # select fitting function
  if (.maIsMetaregression(options)) {
    fit <- try(do.call(RoBMA::RoBMA.reg, fitCall))
  } else {
    fit <- try(do.call(RoBMA::RoBMA, fitCall))
  }

  # add attributes
  attr(fit, "subgroup") <- paste0(subgroupName)
  attr(fit, "dataset")  <- dataset

  # return the results
  return(list(fit = fit))
}
.robmaAddMarginalSummary     <- function(jaspResults, options) {

  # add the marginal summary to the results only if it was requested down the line
  if (!(length(options[["estimatedMarginalMeansEffectSizeSelectedVariables"]]) > 0 || length(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]]) > 0))
    return()

  # check whether it was already computed
  if (!is.null(jaspResults[["marginalSummary"]]))
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
    attr(fit[[i]][["fit"]], "marginalSummary") <- try(RoBMA::marginal_summary(
      object      = fit[[i]][["fit"]],
      conditional = TRUE,
      probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
    ))
    progressbarTick()
  }

  jaspResults[["fit"]]$object <- fit
  marginalSummary$object <- TRUE

  return()
}

#contr.meandif     <<- BayesTools::contr.meandif
#contr.orthonormal <<- BayesTools::contr.orthonormal
#contr.independent <<- BayesTools::contr.independent

# priors related functions
.robmaGetPriors                <- function(options) {

  object <- list()

  # treat fishersZ input scale as smd for setting a prior distribution (as the same prior type is going to be set up within RoBMA)
  if (options[["effectSizeMeasure"]] == "fishersZ") {
    options[["effectSizeMeasure"]] <- "SMD"
  }

  ### effect size & heterogeneity
  if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] == "default") {

    object[["effect"]]        <- list(RoBMA::set_default_priors("effect",        rescale = options[["priorDistributionsScale"]]))
    object[["heterogeneity"]] <- list(RoBMA::set_default_priors("heterogeneity", rescale = options[["priorDistributionsScale"]]))

  } else if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] == "psychology") {

    object[["effect"]]        <- list(RoBMA::set_default_priors("effect",        rescale = options[["priorDistributionsScale"]]))
    object[["heterogeneity"]] <- list(RoBMA::set_default_priors("heterogeneity", rescale = options[["priorDistributionsScale"]]))

  } else if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] == "medicine") {

    object[["effect"]]        <- list(.robmaRescalePriorDistribution(RoBMA::prior_informed("Cochrane", parameter = "effect",        type = options[["effectSizeMeasure"]]), options[["priorDistributionsScale"]]))
    object[["heterogeneity"]] <- list(.robmaRescalePriorDistribution(RoBMA::prior_informed("Cochrane", parameter = "heterogeneity", type = options[["effectSizeMeasure"]]), options[["priorDistributionsScale"]]))

  } else if (options[["priorDistributionsEffectSizeAndHeterogeneity"]] == "custom") {

    object[["effect"]]        <- lapply(options[["modelsEffect"]],        .robmaExtractPriorsFromOptions, type = "continuous")
    object[["heterogeneity"]] <- lapply(options[["modelsHeterogeneity"]], .robmaExtractPriorsFromOptions, type = "continuous")

  }

  # null prior distributions
  if (options[["bayesianModelAveragingEffectSize"]] && options[["priorDistributionsEffectSizeAndHeterogeneity"]] != "custom") {
    object[["effectNull"]]    <- list(RoBMA::set_default_priors("effect", null = TRUE))
  } else if (options[["bayesianModelAveragingEffectSize"]]) {
    object[["effectNull"]]    <- lapply(options[["modelsEffectNull"]], .robmaExtractPriorsFromOptions, type = "continuous")
  } else {
    object[["effectNull"]]    <- NULL
  }
  if (options[["bayesianModelAveragingHeterogeneity"]] && options[["priorDistributionsEffectSizeAndHeterogeneity"]] != "custom") {
    object[["heterogeneityNull"]] <- list(RoBMA::set_default_priors("heterogeneity", null = TRUE))
  } else if (options[["bayesianModelAveragingHeterogeneity"]]) {
    object[["heterogeneityNull"]] <- lapply(options[["modelsHeterogeneityNull"]], .robmaExtractPriorsFromOptions, type = "continuous")
  } else {
    object[["heterogeneityNull"]] <- NULL
  }

  ### publication bias
  if (options[["publicationBiasAdjustment"]] == "PSMA") {
    object[["bias"]] <- RoBMA::set_default_priors("bias", rescale = options[["priorDistributionsScale"]])
  } else if (options[["publicationBiasAdjustment"]] == "PP") {
    tempPriors <- RoBMA::set_default_priors("bias", rescale = options[["priorDistributionsScale"]])[7:8]
    for (i in seq_along(tempPriors)) {
      tempPriors[[i]][["prior_weights"]] <- 1/2
    }
    object[["bias"]] <- tempPriors
  } else if (options[["publicationBiasAdjustment"]] == "PP") {
    tempPriors <- RoBMA::set_default_priors("bias", rescale = options[["priorDistributionsScale"]])[1:2]
    for (i in seq_along(tempPriors)) {
      tempPriors[[i]][["prior_weights"]] <- 1/2
    }
    object[["bias"]] <- tempPriors
  } else if (options[["publicationBiasAdjustment"]] == "custom") {
    object[["bias"]] <- c(
      lapply(options[["modelsSelectionModels"]], .robmaExtractPriorsFromOptions, type = "weightfunction"),
      lapply(options[["modelsPet"]],             .robmaExtractPriorsFromOptions, type = "pet"),
      lapply(options[["modelsPeese"]],           .robmaExtractPriorsFromOptions, type = "peese")
    )
  } else if (options[["publicationBiasAdjustment"]] == "none") {
    object[["bias"]] <- NULL
  }

  # null prior distributions
  if (options[["publicationBiasAdjustment"]] == "none") {
    object[["biasNull"]] <- list(RoBMA::set_default_priors("bias", null = TRUE))
  } else if (options[["bayesianModelAveragingPublicationBias"]] && options[["publicationBiasAdjustment"]] != "custom") {
    object[["biasNull"]] <- list(RoBMA::set_default_priors("bias", null = TRUE))
  } else if (options[["bayesianModelAveragingPublicationBias"]]) {
    object[["biasNull"]] <- c(
      lapply(options[["modelsSelectionModelsNull"]], .robmaExtractPriorsFromOptions, type = "weightfunction"),
      lapply(options[["modelsPetNull"]],             .robmaExtractPriorsFromOptions, type = "pet"),
      lapply(options[["modelsPeeseNull"]],           .robmaExtractPriorsFromOptions, type = "peese")
    )
  } else {
    object[["biasNull"]] <- NULL
  }

  ### moderation
  tempObject <- list()
  for (i in seq_along(options[["effectSizeModelTerms"]])) {

    # TODO: enable interactions later on
    # - this will required identifying whether the interaction contains a factor term in the GUI (to be slotted into the proper prior type)

    tempPrior    <- list()
    tempTerm     <- options[["effectSizeModelTerms"]][[i]]$components
    tempTermType <- options[["predictors.types"]][options[["predictors"]] == tempTerm]

    if (tempTermType == "nominal") {

      # alternative distribution prior
      tempPrior[["alt"]] <- switch(
        # medicine priors are more narrow than psychology priors (there are no default priors for moderatior yet - use 1/2 of the effect size prior scaling)
        options[["priorDistributionsEffectSizeAndHeterogeneity"]],
        "default"    = RoBMA::set_default_priors("factors", rescale = options[["priorDistributionsScale"]]),
        "psychology" = RoBMA::set_default_priors("factors", rescale = options[["priorDistributionsScale"]]),
        "medicine"   = .robmaCochraneFactorPrior(type = options[["effectSizeMeasure"]], options[["priorDistributionsScale"]] / 2),
        "custom"     = .robmaExtractPriorsFromOptions(options[["modelsFactorModerators"]][[which(sapply(options[["modelsFactorModerators"]], "[[", "value") == tempTerm)]], type = "factor")
      )

      # null distribution prior (make sure that the contrast type matches between the alternative and the null hypothesis)
      if (options[["bayesianModelAveragingModerations"]] && options[["priorDistributionsEffectSizeAndHeterogeneity"]] != "custom") {
        tempPrior[["null"]] <- RoBMA::prior_factor("spike", list(0), contrast = .robmaPriorGetContrast(tempPrior[["alt"]]))
      } else if (options[["bayesianModelAveragingModerations"]]) {
        tempPrior[["null"]] <- .robmaExtractPriorsFromOptions(options[["modelsFactorModeratorsNull"]][[which(sapply(options[["modelsFactorModeratorsNull"]], "[[", "value") == tempTerm)]], type = "factor")
        if (BayesTools::is.prior.point(tempPrior[["null"]])) {
          tempPrior[["null"]] <- RoBMA::prior_factor("spike", list(tempPrior[["alt"]][["parameters"]][["location"]]), contrast = .robmaPriorGetContrast(tempPrior[["alt"]]))
        }
      }

    } else if (tempTermType == "scale") {

      # alternative distribution prior
      tempPrior[["alt"]] <- switch(
        # medicine priors are more narrow than psychology priors (there are no default priors for moderatior yet - use 1/2 of the effect size prior scaling)
        options[["priorDistributionsEffectSizeAndHeterogeneity"]],
        "default"    = RoBMA::set_default_priors("covariates", rescale = options[["priorDistributionsScale"]]),
        "psychology" = RoBMA::set_default_priors("covariates", rescale = options[["priorDistributionsScale"]]),
        "medicine"   = .robmaRescalePriorDistribution(RoBMA::prior_informed("Cochrane", parameter = "effect", type = options[["effectSizeMeasure"]]), options[["priorDistributionsScale"]] / 2),
        "custom"     = .robmaExtractPriorsFromOptions(options[["modelsContinuousModerators"]][[which(sapply(options[["modelsContinuousModerators"]], "[[", "value") == tempTerm)]], type = "continuous")
      )

      # null distribution prior
      if (options[["bayesianModelAveragingModerations"]] && options[["priorDistributionsEffectSizeAndHeterogeneity"]] != "custom") {
        tempPrior[["null"]] <- RoBMA::set_default_priors("covariates", null = TRUE)
      } else if (options[["bayesianModelAveragingModerations"]]) {
        tempPrior[["null"]] <- .robmaExtractPriorsFromOptions(options[["modelsContinuousModeratorsNull"]][[which(sapply(options[["modelsContinuousModeratorsNull"]], "[[", "value") == tempTerm)]], type = "continuous")
      }
    }

    # enlist
    tempObject[[tempTerm]] <- tempPrior
  }
  object[["moderators"]] <- tempObject


  return(object)
}
.robmaRescalePriorDistribution <- function(prior, scale) {

  # rescale priors as needed
  if (prior[["distribution"]] %in% c("normal", "mnormal")) {
    prior$parameters[["sd"]]   <- prior$parameters[["sd"]]     * scale
  } else if (prior[["distribution"]] %in% c("t", "mt")) {
    prior$parameters[["scale"]] <- prior$parameters[["scale"]] * scale
  } else if (prior[["distribution"]] == "invgamma") {
    prior$parameters[["rate"]]  <- prior$parameters[["scale"]] * scale
  } else if (scale != 1) {
    stop("Selected prior distribution cannot be rescaled.")
  }

  return(prior)
}
.robmaExtractPriorsFromOptions <- function(optionsPrior, type) {

  optionsPrior   <- .robmaEvalOptionsToPriors(optionsPrior)

  if (optionsPrior[["type"]] == "none")
    return(RoBMA::prior_none(prior_weights = optionsPrior[["priorWeight"]]))
  else
    return(do.call(
      what = switch(
        type,
        "continuous"      = RoBMA::prior,
        "factor"          = RoBMA::prior_factor,
        "weightfunction"  = RoBMA::prior_weightfunction,
        "pet"             = RoBMA::prior_PET,
        "peese"           = RoBMA::prior_PEESE
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

  if(!arguments[["distribution"]] %in% c("oneSided", "twoSided", "oneSidedFixed", "twoSidedFixed", "spike", "uniform", "mnormal", "mt", "spike0")) {
    arguments[["truncation"]] <- list(
      lower   = optionsPrior[["truncationLower"]],
      upper   = optionsPrior[["truncationUpper"]]
    )
  }

  arguments[["prior_weights"]] <- optionsPrior[["priorWeight"]]

  if(type == "factor") {
    arguments[["contrast"]] <- optionsPrior[["contrast"]]
  }

  return(arguments)
}
.robmaCochraneFactorPrior      <- function(type, scale) {

  effectPrior <- RoBMA::prior_informed("Cochrane", parameter = "effect", type = type)
  if (effectPrior[["distribution"]] == "t") {
    factorPrior <- RoBMA::prior_factor("mt", list(location = 0, scale = effectPrior[["parameters"]][["scale"]] * scale, df = effectPrior[["parameters"]][["df"]]), contrast = "meandif")
  } else if (effectPrior[["distribution"]] == "normal") {
    factorPrior <- RoBMA::prior_factor("mnormal", list(location = 0, sd = effectPrior[["parameters"]][["sd"]] * scale), contrast = "meandif")
  }

  return(factorPrior)
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

# naming related functions
.robmaComponentNames   <- function(component, options) {
  return(switch(
    component,
    # from options
    "effectSize"    = if (.maIsMetaregression(options)) gettext("Adjusted effect") else gettext("Pooled effect"),
    "effect"        = if (.maIsMetaregression(options)) gettext("Adjusted effect") else gettext("Pooled effect"),
    "heterogeneity" = gettext("Heterogeneity"),
    "bias"          = gettext("Publication bias"),
    "Baseline"      = gettext("Baseline"),
    # from package
    "Effect"        = if (.maIsMetaregression(options)) gettext("Adjusted effect") else gettext("Pooled effect"),
    "Heterogeneity" = gettext("Heterogeneity"),
    "Bias"          = gettext("Publication bias"),
    "Baseline"      = gettext("Baseline")
  ))
}
.robmaVariableNames    <- function(varNames, variables) {

  return(sapply(varNames, function(varName){

    if (varName %in% c("intrcpt", "intercept"))
      return(gettext("Intercept"))
#     # TODO: figure out how to handle this when interactions are present
#     # (will need ignoring inside of square brackets for [dif: A] or [A])
#     for (vn in variables) {
#       inf <- regexpr(vn, varName, fixed = TRUE)
#
#       if (inf[1] != -1) {
#         varName <- paste0(
#           substr(varName, 0, inf[1] - 1),
#           substr(varName, inf[1], inf[1] + attr(inf, "match.length") - 1),
#           " (",
#           substr(varName, inf[1] + attr(inf, "match.length"), nchar(varName))
#         )
#       }
#
#     }
#
#     varName <- gsub(":", paste0(")", jaspBase::interactionSymbol), varName, fixed = TRUE)
#     varName <- paste0(varName, ")")
#     varName <- gsub(" ()", "", varName, fixed = TRUE)
#     varName <- gsub(" (/", "/", varName, fixed = TRUE)

    return(varName)

  }))
}
.robmaHasHeterogeneity <- function(options) {

  priors <- .robmaGetPriors(options)

  for (i in seq_along(priors[["heterogeneity"]])) {
    if (priors[["heterogeneity"]][[i]][["distribution"]] != "point" ||
        (priors[["heterogeneity"]][[i]][["distribution"]] == "point" && priors[["heterogeneity"]][[i]][["parameters"]][["location"]] != 0))
      return(TRUE)
  }

  return(FALSE)
}
# temp helpers
.robmaAddBfColumn <- function(tempTable, options) {

  titleBF <- switch(
    options[["bayesFactorType"]],
    "BF10"    = gettext("Inclusion BF"),
    "BF01"    = gettext("Exclusion BF"),
    "LogBF10" = gettext("log(Inclusion BF)")
  )

  tempTable$addColumnInfo(name = "bf", title = titleBF, type = "number")

  return()
}

.robmaPrintBf             <- function(bf) {

  bf <- sapply(bf, function(x) {
    if (is.infinite(x) && x > 0) {
      return("\U221E")
    } else if (is.infinite(x) && x < 0) {
      return("-\U221E")
    } else if (x < 1) {
      return(sprintf("%1$.3f", x))
    } else if (x < 10) {
      return(sprintf("%1$.2f", x))
    } else if (x < 100) {
      return(sprintf("%1$.1f", x))
    } else if (x < 100) {
      return(sprintf("%1$.1f", x))
    } else if (x < 1e5){
      return(sprintf("%1$.0f", x))
    } else {
      return(sprintf("%1$.3g", x))
    }
  })
  return(bf)
}
.robmaPrintBfTest         <- function(out, options) {

  bfText <- switch(
    options[["bayesFactorType"]],
    "BF10"    = "BF\U2081\U2080",
    "BF01"    = "BF\U2080\U2081",
    "LogBF10" = "logBF\U2081\U2080"
  )

  return(sprintf("%1$s = %2$s", bfText, .robmaPrintBf(out[["bf"]])))
}
.robmaPrintTest           <- function(fit, options, component, includeName = TRUE) {

  out <- .robmaRowTests(fit, options)
  out <- out[out$test == .robmaComponentNames(component, options),, drop = FALSE]

  if (includeName) {
    return(sprintf("%1$s: %2$s", out[["test"]], .robmaPrintBfTest(out, options)))
  } else {
    return(.robmaPrintBfTest(out, options))
  }
}
.robmaPrintPooledEstimate <- function(fit, options, digits, parameter, conditional) {

  options[["heterogeneityTau"]]    <- parameter == "tau"
  options[["heterogeneityTau2"]]   <- parameter == "tau2"
  options[["heterogeneityI2"]]     <- parameter == "I2"
  options[["heterogeneityH2"]]     <- parameter == "H2"

  out <- .robmaRowPooledEstimates(fit, options, conditional)

  if (parameter == "effect") {
    out <- out[ 1,,drop = FALSE]
  } else {
    out <- out[-1,,drop = FALSE]
  }

  return(sprintf(paste0(
    "%1$s  = ",
    "%2$.", digits, "f",
    " [",
    "%3$.", digits, "f",
    ", ",
    "%4$.", digits, "f",
    "]"
  ), out$par, out$mean, out$lCi, out$uCi))
}

.robmaComputePooledEffect           <- function(fit, options, conditional, returnRaw = FALSE) {

  # effect size summary
  fitSummary <- summary(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]
  fitSummary <- fitSummary[rownames(fitSummary) == "mu",,drop=FALSE]

  estimate <- list(
    par    = .robmaComponentNames("effect", options),
    mean   = fitSummary[["Mean"]],
    median = fitSummary[["Median"]],
    lCi    = fitSummary[[3]],
    uCi    = fitSummary[[4]]
  )

  # prediction intervals
  if (options[["predictionIntervals"]]) {

    hetSummary <- RoBMA::summary_heterogeneity(
      fit,
      conditional = conditional,
      probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
    )[[if (conditional) "estimates_conditional" else "estimates"]]

    estimate$lPi <- hetSummary["PI", 3]
    estimate$uPi <- hetSummary["PI", 4]

  } else {

    estimate$lPi <- NA
    estimate$uPi <- NA

  }


  # return for the plotting function: requires different post-formatting
  if (returnRaw) {
    return(estimate)
  }

  # to data.frame
  estimate <- data.frame(estimate)

  # apply effect size transformation
  if (options[["transformEffectSize"]] != "none")
    estimate[,c("mean", "median", "lCi", "uCi", "lPi", "uPi")] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(estimate[,c("mean", "median", "lCi", "uCi", "lPi", "uPi")]))

  # remove non-requested columns
  estimate <- estimate[,c(
    "par", "mean", "median",
    if (options[["confidenceIntervals"]]) c("lCi", "uCi"),
    if (options[["predictionIntervals"]]) c("lPi", "uPi")
  )]

  return(as.list(estimate))
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

  # heterogeneity summary
  hetSummary <- RoBMA::summary_heterogeneity(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]

  # construct the rows
  tempRows <- list()
  tempRows[["effectSize"]] <- data.frame(.robmaComputePooledEffect(fit, options, conditional))

  if (options[["heterogeneityTau"]])
    tempRows[["heterogeneityTau"]] <- data.frame(
      par    = "\U1D70F",
      mean   = hetSummary["tau", "Mean"],
      median = hetSummary["tau", "Median"],
      lCi    = hetSummary["tau", 3],
      uCi    = hetSummary["tau", 4]
    )
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

# tables and figures
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
.robmaModelSpecificationTablesComponents          <- function(jaspResults, options, components = c("effect", "heterogeneity", "bias")) {

  priors <- .robmaGetPriors(options)

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

    # skip unspecified components (unless they are part of effect / heterogeneity)
    if (!component %in% c("effect", "heterogeneity") && length(tempPriorsNull) == 0 && length(tempPriors) == 0)
      next

    tempWeightsNull <- if (length(tempPriorsNull) > 0) sum(sapply(tempPriorsNull, \(x) x[["prior_weights"]])) else 0
    tempWeights     <- if (length(tempPriors) > 0) sum(sapply(tempPriors, \(x) x[["prior_weights"]]))         else 0

    out[[component]] <- data.frame(
      component   = .robmaComponentNames(component, options),
      models      = sprintf("%1$i/%2$i", length(tempPriors), length(tempPriorsNull) + length(tempPriors)),
      priorProb   = tempWeights / (tempWeights + tempWeightsNull)
    )

    if (length(tempPriorsNull) == 0 && length(tempPriors) == 0)
      tempTable$addFootnote(gettextf("At least one prior distribution for %1$s component has to be specified.", .robmaComponentNames(component, options)))
  }

  tempTable$setData(do.call(rbind, out))
  tempTable$addFootnote(gettext("The analysis will estimate multiple meta-analytic models using MCMC and might require a prolonged time to complete."), symbol = "\u26A0")

  return(tempTable)
}
.robmaModelSpecificationTablesComponentsPriors    <- function(jaspResults, options, components = c("effect", "heterogeneity", "bias"), null = FALSE) {

  priors <- .robmaGetPriors(options)

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

  # always keep effect and heterogeneity, but remove other components if unspecified
  components <- components[components %in% unique(c("effect", "heterogeneity", names(priors)))]
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

  priors <- .robmaGetPriors(options)

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
.robmaOverallTestsTable                           <- function(jaspResults, options) {

  modelSummaryContainer <- .robmaExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["testsTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)
  saveRDS(fit, file = "C:/JASP-Packages/fit.RDS")

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

  # stop and display errors
  if (is.null(fit))
    return()

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
          "The model fit resulted in the following warning: %2$s",
          attr(fit[[i]], "subgroup"),
          errorsAndWarnings[j]
        ))
      }
    }
  }

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
.robmaEstimatedMarginalMeans             <- function(jaspResults, options) {

  # so, this section is a bit complicated -- all in order to prevent updating of all subcomponents once a new variable is added/removed
  # the main container contains effect size and heterogeneity subcontainers, which contain variable containers with the actual output tables
  # updating of the subtables is skipped unless one of the options specified here is checked:
  # .robmaGetEstimatedMarginalMeansOptions()

  # check whether the section should be created at all
  isReadyEffectSize    <- (length(options[["estimatedMarginalMeansEffectSizeSelectedVariables"]]) > 0 || options[["estimatedMarginalMeansEffectSizeAddAdjustedEstimate"]]) &&
    (options[["estimatedMarginalMeansEffectSize"]])

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
      return(x[1,,drop=FALSE])
    } else {
      return(data.frame(x[grep(selectedVariable, x$value),,drop=FALSE]))
    }
  }))

  # reorder by estimated marginal means estimate
  estimatedMarginalMeans <- .maSafeOrderAndSimplify(estimatedMarginalMeans, "value", options)

  # add footnotes
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
.robmaPriorAndPosteriorPlot              <- function(jaspResults, options, parameter) {

  # section container
  priorAndPosteriorPlotContainer <- .robmaExtractPriorAndPosteriorPlotContainer(jaspResults)

  # plot container
  if (!is.null(priorAndPosteriorPlotContainer[[parameter]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # dispatch options
  if (parameter == "pooledEffect") {
    tempTitle        <- if (.maIsMetaregression(options)) gettext("Adjusted Effect") else gettext("Pooled Effect")
    tempPosition     <- 1
    tempFunction     <- .robmaPriorAndPosteriorPlotEstimateFun
    tempDependencies <- "priorAndPosteriorPlotEffectSize"
  } else if (parameter == "heterogeneity") {
    tempTitle        <- gettext("Heterogeneity")
    tempPosition     <- 2
    tempFunction     <- .robmaPriorAndPosteriorPlotEstimateFun
    tempDependencies <- "priorAndPosteriorPlotHeterogeneity"
  } else if (parameter == "moderation") {
    tempTitle        <- if (options[["priorAndPosteriorPlotModerationEstimatedMarginalMeans"]]) gettext("Moderation: Estimated Marginal Means") else gettext("Moderation")
    tempPosition     <- 3
    tempFunction     <- .robmaPriorAndPosteriorPlotModerationFun
    tempDependencies <- c("priorAndPosteriorPlotModeration", "priorAndPosteriorPlotModerationEstimatedMarginalMeans")
    fit              <- list(fit) # trick to forward the fit to a futher dispatch later
  } else if (parameter == "weightFunction") {
    tempTitle        <- gettext("Weight Function")
    tempPosition     <- 4
    tempFunction     <- .robmaPriorAndPosteriorPlotEstimateFun
    tempDependencies <- c("priorAndPosteriorPlotWeightFunction", "priorAndPosteriorPlotWeightFunctionRescaleXAxis")
  } else if (parameter == "petPeese") {
    tempTitle        <- gettext("PET-PEESE")
    tempPosition     <- 5
    tempFunction     <- .robmaPriorAndPosteriorPlotEstimateFun
    tempDependencies <- "priorAndPosteriorPlotPetPeese"
  }

  if (options[["priorAndPosteriorPlotType"]] == "conditional")
    tempTitle <- gettextf("Conditional %1$s", tempTitle)


  # create individual plots for each subgroup
  if (options[["subgroup"]] == "" || parameter == "moderation") {

    tempPlot       <- do.call(tempFunction, list(fit = fit[[1]], options = options, parameter = parameter))
    tempPlot$title <- gettext(tempTitle)
    tempPlot$dependOn(c(.robmaDependencies, tempDependencies))
    tempPlot$position <- tempPosition
    priorAndPosteriorPlotContainer[[parameter]] <- tempPlot
    return()

  } else {

    # create the output container
    tempPlot       <- createJaspContainer()
    tempPlot$title <- gettext(tempTitle)
    tempPlot$dependOn(c(.robmaDependencies, tempDependencies))
    tempPlot$position <- tempPosition
    priorAndPosteriorPlotContainer[[parameter]] <- tempPlot

    for (i in seq_along(fit)) {
      tempPlot[[names(fit)[i]]]          <- do.call(tempFunction, list(fit = fit[[i]], options = options, parameter = parameter))
      tempPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      tempPlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}
.robmaPriorAndPosteriorPlotEstimateFun   <- function(fit, options, parameter) {

  # create plot
  tempPlot <- createJaspPlot(
    width  = if (!parameter %in% c("pooledEffect", "heterogeneity")) 500 else 400,
    height = 320)

  if (jaspBase::isTryError(fit)) {
    return()
  }

  if (!parameter %in% c("pooledEffect", "heterogeneity", "weightFunction", "petPeese") &&
      options[["priorAndPosteriorPlotModerationEstimatedMarginalMeans"]]) {
    p <- try(RoBMA::marginal_plot(
      fit,
      parameter    = parameter,
      prior        = options[["priorAndPosteriorPlotIncludePriorDistribution"]],
      conditional  = options[["priorAndPosteriorPlotType"]] == "conditional",
      plot_type    = "ggplot",
      xlab         = parameter
    ))
  } else {
    p <- try(plot(
      fit,
      parameter    = switch(
        parameter,
        "pooledEffect"   = "mu",
        "heterogeneity"  = "tau",
        "weightFunction" = "weightfunction",
        "petPeese"       = "petpeese",
        parameter
      ),
      prior        = options[["priorAndPosteriorPlotIncludePriorDistribution"]],
      conditional  = options[["priorAndPosteriorPlotType"]] == "conditional",
      rescale_x    = options[["priorAndPosteriorPlotWeightFunctionRescaleXAxis"]],
      plot_type    = "ggplot",
      xlab         = switch(
        parameter,
        "pooledEffect"   = if (.maIsMetaregression(options)) gettext("Adjusted Effect") else gettext("Pooled Effect"),
        "heterogeneity"  = gettext("Heterogeneity"),
        "weightFunction" = gettext("P-Value"),
        "petPeese"       = gettext("Standard Error"),
        parameter
      ),
      ylab         = if (parameter == "petPeese") gettext("Effect Size")
    ))
  }



  if (jaspBase::isTryError(p)) {
    tempPlot$setError(p)
    return(tempPlot)
  }

  if (attr(p, "sec_axis"))
    p <- p + jaspGraphs::geom_rangeframe(sides = "blr") + jaspGraphs::themeJaspRaw(legend.position = "right", legend.title = ggplot2::element_blank()) + ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(vjust = 3.25),
      plot.margin        = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))
  else
    p <- p + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw(legend.position = "right", legend.title = ggplot2::element_blank())

  # obtain residual funnel plot
  tempPlot$plotObject <- p

  return(tempPlot)
}
.robmaPriorAndPosteriorPlotModerationFun <- function(fit, options, parameter) {

  moderators          <- unlist(options[["effectSizeModelTerms"]])
  moderationContainer <- createJaspContainer()

  for (j in seq_along(moderators)) {

    parameter <- moderators[j]

    # create individual plots for each subgroup
    if (options[["subgroup"]] == "") {

      tempPlot       <- do.call(.robmaPriorAndPosteriorPlotEstimateFun, list(fit = fit[[1]], options = options, parameter = parameter))
      tempPlot$title <- parameter
      tempPlot$dependOn(.robmaDependencies)
      tempPlot$position <- j
      moderationContainer[[parameter]] <- tempPlot

    } else {

      # create the output container
      tempPlot       <- createJaspContainer()
      tempPlot$title <- parameter
      tempPlot$dependOn(.robmaDependencies)
      tempPlot$position <- j
      moderationContainer[[parameter]] <- tempPlot

      for (i in seq_along(fit)) {
        tempPlot[[names(fit)[i]]]          <- do.call(.robmaPriorAndPosteriorPlotEstimateFun, list(fit = fit[[i]], options = options, parameter = parameter))
        tempPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
        tempPlot[[names(fit)[i]]]$position <- i
      }

    }
  }

  return(moderationContainer)
}



# additional help functions
.robmaGetEstimatedMarginalMeansOptions <- function(options){

  return(options[c(
    "estimatedMarginalMeansEffectSizeTestAgainst0",
    "estimatedMarginalMeansEffectSizeAddAdjustedEstimate",

    "conditionalEstimates",
    "bayesFactorType",
    "confidenceIntervals",
    "confidenceIntervalsLevel",
    "transformEffectSize"
  )])
}


.robmaEstimatedMarginalMeansWarnings   <- function(fit, options, parameter) {

  if (parameter == "")
    parameter <- "intercept"

  messages <- NULL

  if (options[["subgroup"]] == "") {

    tempFit      <- fit[[1]]
    tempWarnings <- attr(tempFit, "warnings")
    tempWarnings <- gsub("mu_", "", tempWarnings)
    messages     <- tempWarnings[grep(parameter, tempWarnings)]

  } else {
    for (i in seq_along(fit)) {

      tempFit      <- fit[[1]]
      tempWarnings <- attr(tempFit, "warnings")
      tempWarnings <- gsub("mu_", "", tempWarnings)

      if (length(tempWarnings) > 0) {
        messages <- c(messages, sapply(gettextf("Subgroup %1$s, %2$s", attr(tempFit, "subgroup")), tempWarnings))
      }
    }
  }

  return(messages)
}
# containers
.robmaExtractModelSummaryContainer      <- function(jaspResults) {

  if (!is.null(jaspResults[["modelSummaryContainer"]]))
    return(jaspResults[["modelSummaryContainer"]])

  # create the output container
  modelSummaryContainer <- createJaspContainer(gettext("Model Summary"))
  modelSummaryContainer$dependOn(.robmaDependencies)
  modelSummaryContainer$position <- 2
  jaspResults[["modelSummaryContainer"]] <- modelSummaryContainer

  return(modelSummaryContainer)
}
.robmaExtractMetaregressionContainer    <- function(jaspResults) {

  if (!is.null(jaspResults[["metaregressionContainer"]]))
    return(jaspResults[["metaregressionContainer"]])

  # create the output container
  metaregressionContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  metaregressionContainer$dependOn(c(.robmaDependencies))
  metaregressionContainer$position <- 3
  jaspResults[["metaregressionContainer"]] <- metaregressionContainer

  return(metaregressionContainer)
}
.robmaExtractPublicationBiasContainer   <- function(jaspResults) {

  if (!is.null(jaspResults[["publicationBiasContainer"]]))
    return(jaspResults[["publicationBiasContainer"]])

  # create the output container
  publicationBiasContainer <- createJaspContainer(gettext("Publication Bias Adjustment Summary"))
  publicationBiasContainer$dependOn(c(.robmaDependencies, "confidenceIntervals"))
  publicationBiasContainer$position <- 4
  jaspResults[["publicationBiasContainer"]] <- publicationBiasContainer

  return(publicationBiasContainer)
}
.robmaExtractEstimatedMarginalMeansContainer   <- function(jaspResults) {

  if (!is.null(jaspResults[["estimatedMarginalMeansContainer"]]))
    return(jaspResults[["estimatedMarginalMeansContainer"]])

  # create the output container
  estimatedMarginalMeansContainer <- createJaspContainer(gettext("Estimated Marginal Means Summary"))
  estimatedMarginalMeansContainer$dependOn(c(.robmaDependencies, "confidenceIntervals", "confidenceIntervalsLevel", "includeFullDatasetInSubgroupAnalysis"))
  estimatedMarginalMeansContainer$position <- 5
  jaspResults[["estimatedMarginalMeansContainer"]] <- estimatedMarginalMeansContainer

  return(estimatedMarginalMeansContainer)
}
.robmaExtractPriorAndPosteriorPlotContainer    <- function(jaspResults) {

  if (!is.null(jaspResults[["priorAndPosteriorPlotContainer"]]))
    return(jaspResults[["priorAndPosteriorPlotContainer"]])

  # create the output container
  priorAndPosteriorPlotContainer <- createJaspContainer(gettext("Prior and Posterior Plots"))
  priorAndPosteriorPlotContainer$dependOn(c(.robmaDependencies, "includeFullDatasetInSubgroupAnalysis", "priorAndPosteriorPlotType", "priorAndPosteriorPlotIncludePriorDistribution"))
  priorAndPosteriorPlotContainer$position <- 6
  jaspResults[["priorAndPosteriorPlotContainer"]] <- priorAndPosteriorPlotContainer

  return(priorAndPosteriorPlotContainer)
}

### OLD -------------------------------------------------------------------------------------------------



# table filling functions
.robmaAddPriorColumn      <- function(jaspTable, robmaTable) {

  # identify prior columns
  priorColumns <- which(attr(robmaTable, "type") == "prior")
  if(length(priorColumns) == 0)
    return(jaspTable)

  # translate prior names (there will be custom names with addition of meta-regression later)
  priorNamesTranslate  <- function(name) switch(
    name,
    "Effect"        = gettext("Effect Size"),
    "Heterogeneity" = gettext("Heterogeneity"),
    "Bias"          = gettext("Publication Bias"),
    "Baseline"      = gettext("Baseline"),
    name
  )

  for(i in priorColumns)
    jaspTable$addColumnInfo(name = colnames(robmaTable)[i], title = priorNamesTranslate(colnames(robmaTable)[i]), type = "string",  overtitle = gettext("Prior Distribution"))

  return(jaspTable)
}
.robmaFillPriorColumn     <- function(jaspRow, robmaRow) {

  # identify prior columns
  priorColumns <- which(attr(robmaRow, "type") == "prior")
  if(length(priorColumns) == 0)
    return(jaspRow)

  for(i in priorColumns)
    jaspRow[[colnames(robmaRow)[i]]] <- robmaRow[,colnames(robmaRow)[i]]

  return(jaspRow)
}
.robmaTableFillCoef       <- function(jaspTable, resultsTable, options, individual = FALSE) {

  overtitleCi <- gettextf("%s%% CI", 100 * options[["inferenceCiWidth"]])
  # add columns
  jaspTable$addColumnInfo(name = "terms",  title = "",                type = "string")
  jaspTable$addColumnInfo(name = "mean",   title = gettext("Mean"),   type = "number")
  jaspTable$addColumnInfo(name = "median", title = gettext("Median"), type = "number")
  jaspTable$addColumnInfo(name = "lowerCI",title = gettext("Lower"),  type = "number", overtitle = overtitleCi)
  jaspTable$addColumnInfo(name = "upperCI",title = gettext("Upper"),  type = "number", overtitle = overtitleCi)

  if (individual) {
    jaspTable$addColumnInfo(name = "mcmcError",   title = gettext("MCMC error"),    type = "number")
    jaspTable$addColumnInfo(name = "mcmcErrorSd", title = gettext("MCMC error/SD"), type = "number")
    jaspTable$addColumnInfo(name = "ess",         title = gettext("ESS"),           type = "integer")
    jaspTable$addColumnInfo(name = "rHat",        title = gettext("R-hat"),         type = "number")
  }


  if (is.null(resultsTable))
    return(jaspTable)

  # fill rows
  for (i in c(1:nrow(resultsTable))[rownames(resultsTable) %in% c("mu", "tau")]) {
    tempRow <- list(
      terms    = .robmaCoefNames(rownames(resultsTable)[i], options),
      mean     = resultsTable[i, "Mean"],
      median   = resultsTable[i, "Median"],
      lowerCI  = resultsTable[i, if (individual) "lCI" else as.character(.5 - options[["inferenceCiWidth"]] / 2)],
      upperCI  = resultsTable[i, if (individual) "uCI" else as.character(.5 + options[["inferenceCiWidth"]] / 2)]
    )
    if (individual) {
      tempRow[["mcmcError"]]   <- resultsTable[i, "MCMC_error"]
      tempRow[["mcmcErrorSd"]] <- resultsTable[i, "MCMC_SD_error"]
      tempRow[["ess"]]         <- resultsTable[i, "ESS"]
      tempRow[["rHat"]]        <- resultsTable[i, "R_hat"]
    }

    jaspTable$addRows(tempRow)
  }

  # add footnote
  footnotes       <- attr(resultsTable, "footnotes")
  for(i in seq_along(footnotes[!grepl("publication weights omega", footnotes)])) {
    jaspTable$addFootnote(footnotes[!grepl("publication weights omega", footnotes)][i])
  }

  return(jaspTable)
}
.robmaTableFillPetPeese   <- function(jaspTable, resultsTable, options, individual = FALSE) {

  overtitleCi <- gettextf("%s%% CI", 100 * options[["inferenceCiWidth"]])
  # add columns
  jaspTable$addColumnInfo(name = "terms",  title = "",                type = "string")
  jaspTable$addColumnInfo(name = "mean",   title = gettext("Mean"),   type = "number")
  jaspTable$addColumnInfo(name = "median", title = gettext("Median"), type = "number")
  jaspTable$addColumnInfo(name = "lowerCI",title = gettext("Lower"),  type = "number", overtitle = overtitleCi)
  jaspTable$addColumnInfo(name = "upperCI",title = gettext("Upper"),  type = "number", overtitle = overtitleCi)

  if (individual) {
    jaspTable$addColumnInfo(name = "mcmcError",   title = gettext("MCMC error"),    type = "number")
    jaspTable$addColumnInfo(name = "mcmcErrorSd", title = gettext("MCMC error/SD"), type = "number")
    jaspTable$addColumnInfo(name = "ess",         title = gettext("ESS"),           type = "integer")
    jaspTable$addColumnInfo(name = "rHat",        title = gettext("R-hat"),         type = "number")
  }


  if (is.null(resultsTable))
    return(jaspTable)

  # fill rows
  for (i in c(1:nrow(resultsTable))[rownames(resultsTable) %in% c("PET", "PEESE")]) {
    tempRow <- list(
      terms    = rownames(resultsTable)[i],
      mean     = resultsTable[i, "Mean"],
      median   = resultsTable[i, "Median"],
      lowerCI  = resultsTable[i, if (individual) "lCI" else as.character(.5 - options[["inferenceCiWidth"]] / 2)],
      upperCI  = resultsTable[i, if (individual) "uCI" else as.character(.5 + options[["inferenceCiWidth"]] / 2)]
    )
    if (individual) {
      tempRow[["mcmcError"]]   <- resultsTable[i, "MCMC_error"]
      tempRow[["mcmcErrorSd"]] <- resultsTable[i, "MCMC_SD_error"]
      tempRow[["ess"]]         <- resultsTable[i, "ESS"]
      tempRow[["rHat"]]        <- resultsTable[i, "R_hat"]
    }

    jaspTable$addRows(tempRow)
  }


  return(jaspTable)
}
.robmaTableFillWeights    <- function(jaspTable, resultsTable, options, individual = FALSE) {

  overtitleP  <- gettext("<em>p</em>-values interval")
  overtitleCi <- gettextf("%s%% CI", 100 * options[["inferenceCiWidth"]])
  # add columns
  jaspTable$addColumnInfo(name = "lowerRange", title = gettext("Lower"),  type = "number", overtitle = overtitleP)
  jaspTable$addColumnInfo(name = "upperRange", title = gettext("Upper"),  type = "number", overtitle = overtitleP)
  jaspTable$addColumnInfo(name = "mean",       title = gettext("Mean"),   type = "number")
  jaspTable$addColumnInfo(name = "median",     title = gettext("Median"), type = "number")
  jaspTable$addColumnInfo(name = "lowerCI",    title = gettext("Lower"),  type = "number", overtitle = overtitleCi)
  jaspTable$addColumnInfo(name = "upperCI",    title = gettext("Upper"),  type = "number", overtitle = overtitleCi)

  if (individual) {
    jaspTable$addColumnInfo(name = "mcmcError",   title = gettext("MCMC error"),    type = "number")
    jaspTable$addColumnInfo(name = "mcmcErrorSd", title = gettext("MCMC error/SD"), type = "number")
    jaspTable$addColumnInfo(name = "ess",         title = gettext("ESS"),           type = "integer")
    jaspTable$addColumnInfo(name = "rHat",        title = gettext("R-hat"),         type = "number")
  }


  if (is.null(resultsTable))
    return(jaspTable)


  # fill rows
  for (i in c(1:nrow(resultsTable))[grepl("omega", rownames(resultsTable))]) {
    tempRow <- list(
      lowerRange = as.numeric(substr(
        rownames(resultsTable)[i],
        7,
        regexec(",", rownames(resultsTable)[i], fixed = TRUE)[[1]] - 1
      )),
      upperRange = as.numeric(substr(
        rownames(resultsTable)[i],
        regexec(",", rownames(resultsTable)[i], fixed = TRUE)[[1]] + 1,
        nchar(rownames(resultsTable)[i]) - 1
      )),
      mean       = resultsTable[i, "Mean"],
      median     = resultsTable[i, "Median"],
      lowerCI    = resultsTable[i, if (individual) "lCI" else as.character(.5 - options[["inferenceCiWidth"]] / 2)],
      upperCI    = resultsTable[i, if (individual) "uCI" else as.character(.5 + options[["inferenceCiWidth"]] / 2)]
    )
    if (individual) {
      tempRow[["mcmcError"]]   <- resultsTable[i, "MCMC_error"]
      tempRow[["mcmcErrorSd"]] <- resultsTable[i, "MCMC_SD_error"]
      tempRow[["ess"]]         <- resultsTable[i, "ESS"]
      tempRow[["rHat"]]        <- resultsTable[i, "R_hat"]
    }

    jaspTable$addRows(tempRow)
  }

  # add footnote
  footnotes       <- attr(resultsTable, "footnotes")
  for(i in seq_along(footnotes[grepl("publication weights omega", footnotes)])) {
    jaspTable$addFootnote(footnotes[grepl("publication weights omega", footnotes)][i])
  }

  return(jaspTable)
}
.robmaCoefNames           <- function(coefficient, options) {
  if (coefficient == "mu" && (!is.null(options[["inputType"]]) && options[["inputType"]] != "unstandardizedEffectSizes"))
    return(gettextf("Effect size (%s)", "\u03BC"))
  else if (coefficient == "mu")
    return(gettextf("Effect size (%s)", .robmaCoefLetters(options[["inferenceOutputScale"]])))
  else if (coefficient == "tau")
    return(gettextf("Heterogeneity (%s)","\u03C4"))
}
.robmaCompNames           <- function(component) {
  return(switch(
    component,
    "Effect"        = gettext("Effect"),
    "Heterogeneity" = gettext("Heterogeneity"),
    "Bias"          = gettext("Publication bias"),
    "Baseline"      = gettext("Baseline")
  ))
}
.robmaCoefLetters         <- function(effectSize) {
  return(switch(
    effectSize,
    "correlation" = "\u03C1",
    "cohensD"     = "\u03B4",
    "fishersZ"    = "\u007a",
    "logOr"       = gettext("log(OR)"),
    "or"          = gettext("OR")
  ))
}
# helper functions
.robmaCheckDiagnostics    <- function(options, any) {
  parametersAny <-
    options[["mcmcDiagnosticsPlotEffect"]]          ||
    options[["mcmcDiagnosticsPlotHeterogeneity"]]   ||
    options[["mcmcDiagnosticsPlotWeights"]]         ||
    options[["mcmcDiagnosticsPlotPet"]]             ||
    options[["mcmcDiagnosticsPlotPeese"]]
  typeAny <- options[["mcmcDiagnosticsPlotTypeTrace"]]  ||
    options[["mcmcDiagnosticsPlotTypeAutocorrelation"]] ||
    options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]

  if (any)
    return(parametersAny || typeAny)
  else
    return(parametersAny && typeAny)
}
.robmaModelNotifier       <- function(jaspResults) {
  # We don't wanna delete the RoBMA modele every time settings is change since RoBMA takes a lot of time to fit.
  # Therefore, we don't create dependencies on the fitted model (in cases when the model can be updated), but
  # on a notifier that tells us when there was the change. If possible, we don't refit the whole model,
  # just update the neccessary parts.

  if (is.null(jaspResults[["modelNotifier"]])) {
    modelNotifier <- createJaspState()
    modelNotifier$dependOn(.robmaDependencies)
    jaspResults[["modelNotifier"]] <- modelNotifier
  }

  return()

}

.robmaTypeDependencies    <- function(type) {
  return(switch(
    type,
    "RoBMA" = .robmaDependencies,
    "BiBMA" = .bibmaDependencies
  ))
}
# main functions
.robmaPriorsPlots              <- function(jaspResults, options) {

  # create / access the container
  if (!is.null(jaspResults[["priorPlots"]]))
    priorPlots <- jaspResults[["priorPlots"]]
  else {
    priorPlots <- createJaspContainer(title = gettext("Prior Plots"))
    priorPlots$dependOn(c("priorDistributionPlot", "inputType", "pathToFittedModel", "priorScale", "modelEnsembleType"))
    priorPlots$position <- 2
    jaspResults[["priorPlots"]] <- priorPlots
  }

  # extract the priors
  if (is.null(jaspResults[["model"]]) && options[["modelEnsembleType"]] != "custom")
    priors <- .robmaPriorsToOptionsNames(RoBMA::check_setup(model_type = .robmaGetModelTypeOption(options), silent = TRUE)$priors)
  else if (is.null(jaspResults[["model"]]))
    priors <- .robmaPriorsToOptionsNames(jaspResults[["priors"]][["object"]])
  else
    priors <- .robmaPriorsToOptionsNames(jaspResults[["model"]][["object"]][["priors"]])


  # create container for each of the parameters
  for (parameter in c("effect", "heterogeneity", "bias")) {

    if (!is.null(priorPlots[[parameter]]))
      parameterContainer <- priorPlots[[parameter]]
    else {
      parameterContainer <- createJaspContainer(title = switch(
        parameter,
        "effect"          = gettext("Effect"),
        "heterogeneity"   = gettext("Heterogeneity"),
        "bias"            = gettext("Publication Bias")
      ))
      parameterContainer$position <- switch(
        parameter,
        "effect"          = 1,
        "heterogeneity"   = 2,
        "bias"            = 3
      )
      priorPlots[[parameter]] <- parameterContainer
    }

    # create container for null and alternative models
    for (type in c("null", "alternative")) {

      if (!is.null(parameterContainer[[type]])) {
        next
      } else {
        typeContainer <- createJaspContainer(title = switch(
          type,
          "null"         = gettext("Null"),
          "alternative"  = gettext("Alternative")
        ))
        typeContainer$position <- switch(
          type,
          "null"         = 1,
          "alternative"  = 2
        )
        typeContainer$dependOn(switch(
          parameter,
          "effect"        = c("modelsEffect", "modelsEffectNull"),
          "heterogeneity" = c("modelsHeterogeneity", "modelsHeterogeneityNull"),
          "bias"          = c("modelsSelectionModels", "modelsSelectionModelsNull", "modelsPet", "modelsPetNull", "modelsPeese", "modelsPeeseNull")
        ))
        parameterContainer[[type]] <- typeContainer
      }

      tempPriors <- priors[[paste0(switch(
        parameter,
        "effect"        = "modelsEffect",
        "heterogeneity" = "modelsHeterogeneity",
        "bias"          = "modelsBias"
      ), if (type == "null") "Null")]]

      if (length(tempPriors) == 0)
        next

      # generate the actual plots
      for (i in seq_along(tempPriors)) {

        if(BayesTools::is.prior.none(tempPriors[[i]]))
          next

        if (parameter == "bias")
          tempPlot <- createJaspPlot(width = 500,  height = 400)
        else
          tempPlot <- createJaspPlot(width = 400,  height = 300)

        typeContainer[[paste0(parameter, type, i)]] <- tempPlot

        p <- plot(tempPriors[[i]], plot_type = "ggplot", rescale_x = TRUE, par_name = switch(
          parameter,
          "effect"        = bquote(mu),
          "heterogeneity" = bquote(tau),
          NULL
        ))
        p <- jaspGraphs::themeJasp(p)

        typeContainer[[paste0(parameter, type, i)]][["plotObject"]] <- p

      }
    }
  }

  return()
}

.robmaReadFittedModel          <- function(options) {
  if (tolower(gsub(" ", "", options[["pathToFittedModel"]])) == "examplerobmalui2015") {
    data("exampleRobmaLui2015")
    return(exampleRobmaLui2015)
  } else {
    try(readRDS(file = options[["pathToFittedModel"]]))
  }
}


.robmaModelsOverviewTable      <- function(jaspResults, options, type) {

  ### create overview table
  modelsSummary <- createJaspTable(title = gettext("Models Overview"))
  modelsSummary$position <- 8
  modelsSummary$dependOn(c(.robmaTypeDependencies(type), "bayesFactorType", "inferenceModelsOverview", "inferenceModelsOverviewBF", "inferenceModelsOverviewOrder", "inferenceShortenPriorName"))
  jaspResults[["mainSummary"]][["modelsSummary"]] <- modelsSummary

  if (options[["inferenceModelsOverviewBfComparison"]] == "inclusion")
    titleBF <- switch(
      options[["bayesFactorType"]],
      "BF10"    = gettext("Inclusion BF"),
      "BF01"    = gettext("Exclusion BF"),
      "LogBF10" = gettext("log(Inclusion BF)")
    )
  else
    titleBF <- switch(
      options[["bayesFactorType"]],
      "BF10"    = gettextf("BF%s",     "\u2081\u2080"),
      "BF01"    = gettextf("BF%s",     "\u2080\u2081"),
      "LogBF10" = gettextf("log(BF%s)","\u2081\u2080")
    )


  if (is.null(jaspResults[["model"]]))
    fitSummary <- NULL
  else
    fitSummary <- summary(
      jaspResults[["model"]][["object"]],
      type       = "models",
      short_name = options[["inferenceShortenPriorName"]]
    )

  # create the table
  modelsSummary$addColumnInfo(name = "number",             title = "#",                        type = "integer")
  if (!is.null(fitSummary))
    modelsSummary <- .robmaAddPriorColumn(modelsSummary, fitSummary[["summary"]])
  modelsSummary$addColumnInfo(name = "priorProb",          title = gettext("P(M)"),             type = "number")
  modelsSummary$addColumnInfo(name = "postProb",           title = gettext("P(M|data)"),        type = "number")
  modelsSummary$addColumnInfo(name = "marglik",            title = gettext("log(MargLik)"),     type = "number")
  modelsSummary$addColumnInfo(name = "BF",                 title = titleBF,                     type = "number")

  if (is.null(fitSummary))
    return()

  # do ordering
  if (options[["inferenceModelsOverviewOrder"]] == "marginalLikelihood")
    fitSummary[["summary"]] <- fitSummary[["summary"]][order(fitSummary[["summary"]][["marglik"]], decreasing = TRUE),]
  else if (options[["inferenceModelsOverviewOrder"]] == "posteriorProbability")
    fitSummary[["summary"]] <- fitSummary[["summary"]][order(fitSummary[["summary"]][["post_prob"]], decreasing = TRUE),]

  # compute the BF requested
  if (options[["inferenceModelsOverviewBfComparison"]] == "inclusion") {
    bf <- fitSummary[["summary"]][, 8]
  } else if (options[["inferenceModelsOverviewBfComparison"]] == "best") {
    bf <- exp(fitSummary[["summary"]][["marglik"]] - max(fitSummary[["summary"]][["marglik"]]))
  } else if (options[["inferenceModelsOverviewBfComparison"]] == "previous") {
    tempThisMargLik <- fitSummary[["summary"]][["marglik"]][-length(fitSummary[["summary"]][["marglik"]])]
    tempPrevMargLik <- fitSummary[["summary"]][["marglik"]][-1]
    bf <- c(1, exp(tempPrevMargLik - tempThisMargLik))
  }

  # fill the rows
  for (i in 1:nrow(fitSummary[["summary"]])) {
    tempRow <- list(
      number             = fitSummary[["summary"]][i, "Model"],
      priorProb          = fitSummary[["summary"]][i, "prior_prob"],
      postProb           = fitSummary[["summary"]][i, "post_prob"],
      marglik            = fitSummary[["summary"]][i, "marglik"],
      BF                 = BayesTools::format_BF(bf[i], logBF = options[["bayesFactorType"]] == "LogBF10", BF01 = options[["bayesFactorType"]] == "BF01")
    )
    tempRow <- .robmaFillPriorColumn(tempRow, fitSummary[["summary"]][i,])
    modelsSummary$addRows(tempRow)
  }

  return()
}
.robmaModelsSummaryTable       <- function(jaspResults, options, type) {

  if (!is.null(jaspResults[["individualModels"]])) {
    return()
  } else {
    individualModels <- createJaspContainer(title = gettext("Individual Models Summary"))
    individualModels$position <- 9
    individualModels$dependOn(c(.robmaTypeDependencies(type), "bayesFactorType", "inferenceIndividualModels", "inferenceIndividualModelsSingleModel", "inferenceIndividualModelsSingleModelNumber", "inferenceShortenPriorName", "inferenceOutputScale"))
    jaspResults[["individualModels"]] <- individualModels
  }

  titleBF <- switch(
    options[["bayesFactorType"]],
    "BF10"    = gettext("Inclusion BF"),
    "BF01"    = gettext("Exclusion BF"),
    "LogBF10" = gettext("log(Inclusion BF)")
  )

  if (is.null(jaspResults[["model"]])) {

    tempModel <- createJaspContainer(title = gettext("Model #"))
    individualModels[["modelI"]] <- tempModel

    tempPriors <- createJaspTable(title = gettext("Priors"))
    if (type == "RoBMA") {
      tempPriors$addColumnInfo(name = "priorMu",       title = gettext("Effect Size"),      type = "string")
      tempPriors$addColumnInfo(name = "priorTau",      title = gettext("Heterogeneity"),    type = "string")
      tempPriors$addColumnInfo(name = "priorBias",     title = gettext("Publication Bias"), type = "string")
    } else if (type == "BiBMA") {
      tempPriors$addColumnInfo(name = "priorMu",       title = gettext("Effect Size"),      type = "string")
      tempPriors$addColumnInfo(name = "priorTau",      title = gettext("Heterogeneity"),    type = "string")
      tempPriors$addColumnInfo(name = "priorBaseline", title = gettext("Baseline"),         type = "string")
    }
    tempModel[["tempPriors"]] <- tempPriors

    tempInfo <- createJaspTable(title = gettext("Information"))
    tempInfo$addColumnInfo(name = "priorProb",   title = gettext("P(M)"),          type = "number")
    tempInfo$addColumnInfo(name = "postProb",    title = gettext("P(M|data)"),     type = "number")
    tempInfo$addColumnInfo(name = "marglik",     title = gettext("log(MargLik)"),  type = "number")
    tempInfo$addColumnInfo(name = "BF",          title = titleBF,                  type = "number")
    tempModel[["tempInfo"]] <- tempInfo

    tempCoef <- createJaspTable(title = gettext("Model Estimates"))
    tempCoef <- .robmaTableFillCoef(tempCoef, NULL, options, individual = TRUE)
    tempModel[["tempCoef"]] <- tempCoef

    return()
  }

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # some shared info
  fitSummary <- summary(
    fit,
    type          = "individual",
    output_scale  = .robmaGetOutputScaleOption(options),
    short_name    = options[["inferenceShortenPriorName"]]
  )

  ### create tables for individual models

  # select models to iterate over
  if (options[["inferenceIndividualModelsSingleModel"]]) {
    modelsI <- options[["inferenceIndividualModelsSingleModelNumber"]]
    if (modelsI < 1 || modelsI > length(fit[["models"]])) {
      tempModel  <- createJaspContainer(title = gettextf("Model %i", modelsI))
      tempError  <- createJaspTable(title = "")
      tempError$setError(gettextf("Model %1$i does not exist. Select one of the models between 1 and %2$i.", modelsI, length(fit[["models"]])))
      tempModel[["tempError"]]                     <- tempError
      individualModels[[paste0("model", modelsI)]] <- tempModel
      return()
    }
  } else {
    modelsI <- 1:length(fit[["models"]])
  }


  # do the iteration
  for (i in modelsI) {

    tempModel <- createJaspContainer(title = gettextf("Model %i", i))
    individualModels[[paste0("model", i)]] <- tempModel

    ### model priors
    tempPriors <- createJaspTable(title = gettext("Priors"))

    if (type == "RoBMA") {

      tempPriors$addColumnInfo(name = "priorMu",    title = gettext("Effect Size"),      type = "string")
      tempPriors$addColumnInfo(name = "priorTau",   title = gettext("Heterogeneity"),    type = "string")
      tempPriors$addColumnInfo(name = "priorBias",  title = gettext("Publication Bias"), type = "string")

      tempPriors$addRows(list(
        priorMu     = print(fit[["models"]][[i]][["priors"]][["mu"]],    silent = TRUE, short_name = options[["inferenceShortenPriorName"]]),
        priorTau    = print(fit[["models"]][[i]][["priors"]][["tau"]],   silent = TRUE, short_name = options[["inferenceShortenPriorName"]]),
        priorBias   = if (!is.null(fit[["models"]][[i]][["priors"]][["omega"]]))
          print(fit[["models"]][[i]][["priors"]][["omega"]], silent = TRUE, short_name = options[["inferenceShortenPriorName"]])
        else if (!is.null(fit[["models"]][[i]][["priors"]][["PET"]]))
          print(fit[["models"]][[i]][["priors"]][["PET"]],   silent = TRUE, short_name = options[["inferenceShortenPriorName"]])
        else if (!is.null(fit[["models"]][[i]][["priors"]][["PEESE"]]))
          print(fit[["models"]][[i]][["priors"]][["PEESE"]], silent = TRUE, short_name = options[["inferenceShortenPriorName"]])
      ))

    } else if (type == "BiBMA") {

      tempPriors$addColumnInfo(name = "priorMu",       title = gettext("Effect Size"),      type = "string")
      tempPriors$addColumnInfo(name = "priorTau",      title = gettext("Heterogeneity"),    type = "string")
      tempPriors$addColumnInfo(name = "priorBaseline", title = gettext("Baseline"),         type = "string")

      tempPriors$addRows(list(
        priorMu       = print(fit[["models"]][[i]][["priors"]][["mu"]],   silent = TRUE, short_name = options[["inferenceShortenPriorName"]]),
        priorTau      = print(fit[["models"]][[i]][["priors"]][["tau"]],  silent = TRUE, short_name = options[["inferenceShortenPriorName"]]),
        priorBaseline = print(fit[["models"]][[i]][["priors"]][["pi"]],   silent = TRUE, short_name = options[["inferenceShortenPriorName"]])
      ))

    }

    tempModel[["tempPriors"]] <- tempPriors


    ### model information
    tempInfo <- createJaspTable(title = gettext("Information"))

    tempInfo$addColumnInfo(name = "priorProb",   title = gettext("P(M)"),          type = "number")
    tempInfo$addColumnInfo(name = "postProb",    title = gettext("P(M|data)"),     type = "number")
    tempInfo$addColumnInfo(name = "marglik",     title = gettext("log(MargLik)"),  type = "number")
    tempInfo$addColumnInfo(name = "BF",          title = titleBF,                  type = "number")

    tempInfo$addRows(list(
      priorProb    = fit[["models"]][[i]][["inference"]][["prior_prob"]],
      postProb     = fit[["models"]][[i]][["inference"]][["post_prob"]],
      marglik      = fit[["models"]][[i]][["inference"]][["marglik"]],
      BF           = BayesTools::format_BF(fit[["models"]][[i]][["inference"]][["inclusion_BF"]], logBF = options[["bayesFactorType"]] == "LogBF10", BF01 = options[["bayesFactorType"]] == "BF01")
    ))

    tempModel[["tempInfo"]] <- tempInfo


    ### model coefficients
    # estimate table
    tempCoef <- createJaspTable(title = gettext("Model Estimates"))
    tempCoef <- .robmaTableFillCoef(tempCoef, fitSummary$models[[i]][["estimates"]], options, individual = TRUE)
    tempModel[["tempCoef"]] <- tempCoef

    ### weights and studies effects
    if (type == "RoBMA" && !is.null(fitSummary[["models"]][[i]][["estimates"]])) {

      # weights table
      if (any(grepl("omega", rownames(fitSummary[["models"]][[i]][["estimates"]])))) {
        tempWeights <- createJaspTable(title = gettextf("Estimated Weights (%s)", "\u03C9"))
        tempWeights <- .robmaTableFillWeights(tempWeights, fitSummary$models[[i]][["estimates"]], options, individual = TRUE)
        tempModel[["tempWeights"]] <- tempWeights
      }

      # estimated studies table
      if (any(grepl("PET", rownames(fitSummary[["models"]][[i]][["estimates"]])) | grepl("PEESE", rownames(fitSummary[["models"]][[i]][["estimates"]])))) {
        tempPetPeese <- createJaspTable(title = gettextf("PET-PEESE Estimates"))
        tempPetPeese <- .robmaTableFillPetPeese(tempPetPeese, fitSummary[["models"]][[i]][["estimates"]], options, individual = TRUE)
        tempModel[["tempPetPeese"]] <- tempPetPeese
      }
    }
  }

  return()
}
.robmaForestPlot               <- function(jaspResults, options, type) {

  if (!is.null(jaspResults[["forestPlot"]]))
    return()

  # prepare the plot object
  title  <- gettextf("%1$s Forest Plot", switch(
    options[["plotsForestPlotType"]],
    "conditional" = gettext("Conditional"),
    "averaged"    = gettext("Model Averaged")
  ))

  if (is.null(jaspResults[["model"]])) {
    jaspResults[["forestPlot"]] <- createJaspPlot(title = title, dependencies = "plotsForestPlot")
    return()
  }

  # extract the model
  fit <- jaspResults[["model"]][["object"]]

  height <- 100 + nrow(fit[["data"]]) * 50
  width  <- 800

  forestPlot <- createJaspPlot(title = title, width = width, height = height)
  forestPlot$position <- 10
  forestPlot$dependOn(c(.robmaTypeDependencies(type), "plotsForestPlot", "plotsForestPlotOrder", "plotsForestPlotType", "inferenceOutputScale"))
  jaspResults[["forestPlot"]] <- forestPlot


  # plot
  p <- try(RoBMA::forest(
    fit,
    conditional  = options[["plotsForestPlotType"]] == "conditional",
    order        = options[["plotsForestPlotOrder"]],
    output_scale = .robmaGetOutputScaleOption(options),
    plot_type    = "ggplot"
  ))

  if (jaspBase::isTryError(p)) {
    forestPlot$setError(p)
    return()
  }

  p <- jaspGraphs::themeJasp(p, sides = "b")
  jaspResults[["forestPlot"]]$plotObject <- p

  return()
}
.robmaEstimatesPlot            <- function(jaspResults, options, parameter, type) {

  # create / access the container
  if (is.null(jaspResults[["estimatesPlots"]])) {
    estimatesPlots <- createJaspContainer(title = gettext("Posterior Distribution Plots"))
    estimatesPlots$position <- 11
    estimatesPlots$dependOn(c(.robmaTypeDependencies(type), "plotsPooledEstimatesType", "plotsPooledEstimatesPriorDistribution", "inferenceOutputScale"))
    jaspResults[["estimatesPlots"]] <- estimatesPlots
  } else {
    estimatesPlots <- jaspResults[["estimatesPlots"]]
  }

  # don't redo an already created plot
  if (!is.null(estimatesPlots[[parameter]]))
    return()

  # prepare the plot object
  title  <- sprintf(
    "%1$s %2$s",
    switch(
      options[["plotsPooledEstimatesType"]],
      "conditional" = gettext("Conditional"),
      "averaged"    = gettext("Model Averaged")
    ),
    switch(
      parameter,
      "mu"             = gettext("Effect Size Estimate"),
      "tau"            = gettext("Heterogeneity Estimate"),
      "weightFunction" = gettext("Weight Function Estimate"),
      "petPeese"       = gettext("PET-PEESE Regression Estimate")
    ))
  height <- 350
  width  <- 600

  tempPlot <- createJaspPlot(title = title, width = width, height = height)
  tempPlot$position <- switch(
    parameter,
    "mu"             = 1,
    "tau"            = 2,
    "weightFunction" = 3,
    "petPeese"       = 4
  )
  tempPlot$dependOn(switch(
    parameter,
    "mu"             = "plotsPooledEstimatesEffect",
    "tau"            = "plotsPooledEstimatesHeterogeneity",
    "weightFunction" = c("plotsPooledEstimatesWeightFunction", "plotsPooledEstimatesWeightFunctionRescaleXAxis"),
    "petPeese"       = "plotsPooledEstimatesPetPeese"
  ))
  estimatesPlots[[parameter]] <- tempPlot

  if (is.null(jaspResults[["model"]]))
    return()

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # plot
  p <- try(plot(
    fit,
    parameter    = parameter,
    prior        = options[["plotsPooledEstimatesPriorDistribution"]],
    output_scale = .robmaGetOutputScaleOption(options),
    rescale_x    = options[["plotsPooledEstimatesWeightFunctionRescaleXAxis"]],
    conditional  = options[["plotsPooledEstimatesType"]] == "conditional",
    plot_type    = "ggplot"
  ))

  if (jaspBase::isTryError(p)) {
    tempPlot$setError(p)
    return()
  }

  if (attr(p, "sec_axis"))
    p <- jaspGraphs::themeJasp(p, sides = "blr") + ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(vjust = 3.25),
      plot.margin        = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))
  else
    p <- jaspGraphs::themeJasp(p, sides = "bl")

  estimatesPlots[[parameter]]$plotObject <- p

  return()
}
.robmaModelsPlot               <- function(jaspResults, options, parameter, type) {

  # create / access the container
  if (is.null(jaspResults[["modelsPlots"]])) {
    modelsPlots <- createJaspContainer(title = gettext("Posterior Model Estimates Plots"))
    modelsPlots$position <- 12
    modelsPlots$dependOn(c(.robmaTypeDependencies(type), "plotsIndividualModelsType", "plotsIndividualModelsOrder", "plotsIndividualModelsOrderBy", "plotsIndividualModelsShowBayesianUpdating", "plotsIndividualModelsShowPosteriorEstimates", "inferenceOutputScale"))
    jaspResults[["modelsPlots"]] <- modelsPlots
  } else {
    modelsPlots <- jaspResults[["modelsPlots"]]
  }

  # don't redo an already created plot
  if (!is.null(modelsPlots[[parameter]]))
    return()

  # prepare the plot object
  title  <- sprintf(
    "%1$s %2$s",
    switch(
      options[["plotsIndividualModelsType"]],
      "conditional" = gettext("Conditional"),
      "averaged"    = gettext("Model Averaged")
    ),
    switch(
      parameter,
      "mu"  = gettext("Effect Size Estimates"),
      "tau" = gettext("Heterogeneity Estimates")
    ))

  if (is.null(jaspResults[["model"]])) {
    modelsPlots[[parameter]] <- createJaspPlot(title = title, dependencies = switch(
      parameter,
      "mu"  = "plotsIndividualModelsEffect",
      "tau" = "plotsIndividualModelsHeterogeneity"
    ))
    return()
  }

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  height_multiplier <- if(options[["plotsIndividualModelsShowBayesianUpdating"]] && options[["plotsIndividualModelsShowPosteriorEstimates"]]) 65 else 30
  height <- switch(
    options[["plotsIndividualModelsType"]],
    "averaged"    = 50 + length(fit[["models"]]) * height_multiplier,
    "conditional" = 50 + sum(sapply(fit[["models"]], function(model) RoBMA:::.is_component_null(model[["priors"]], component = switch(parameter, "mu" = "effect", "tau" = "heterogeneity")))) * height_multiplier
  )
  width  <- 800

  tempPlot <- createJaspPlot(title = title, width = width, height = height)
  tempPlot$position <- switch(
    parameter,
    "mu"  = 1,
    "tau" = 2
  )
  tempPlot$dependOn(switch(
    parameter,
    "mu"  = "plotsIndividualModelsEffect",
    "tau" = "plotsIndividualModelsHeterogeneity"
  ))
  modelsPlots[[parameter]] <- tempPlot

  # plot
  p <- try(RoBMA::plot_models(
    fit,
    parameter      = parameter,
    order          = options[["plotsIndividualModelsOrder"]],
    order_by       = .robmaGetIndividualModelOrderOption(options),
    conditional    = options[["plotsIndividualModelsType"]] == "conditional",
    output_scale   = .robmaGetOutputScaleOption(options),
    show_updating  = options[["plotsIndividualModelsShowBayesianUpdating"]],
    show_estimates = options[["plotsIndividualModelsShowPosteriorEstimates"]],
    y_axis2        = options[["plotsIndividualModelsShowBayesianUpdating"]] || options[["plotsIndividualModelsShowPosteriorEstimates"]],
    plot_type      = "ggplot"
  ))

  if (jaspBase::isTryError(p)) {
    tempPlot$setError(p)
    return()
  }

  p <- jaspGraphs::themeJasp(p, sides = "b")
  modelsPlots[[parameter]]$plotObject <- p

  return()
}
.robmaDiagnosticsOverviewTable <- function(jaspResults, options, type) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 13
    diagnostics$dependOn(.robmaTypeDependencies(type))
    jaspResults[["diagnostics"]] <- diagnostics
  } else {
    diagnostics <- jaspResults[["diagnostics"]]
  }

  if (!is.null(diagnostics[["diagosticsTable"]])) {
    return()
  }


  if (is.null(jaspResults[["model"]]))
    fitSummary <- NULL
  else
    fitSummary <- summary(
      jaspResults[["model"]][["object"]],
      type       = "diagnostics",
      short_name = options[["inferenceShortenPriorName"]]
    )


  ### create overview table
  diagosticsTable <-  createJaspTable(title = gettext("Models Diagnostics Overview"))
  diagosticsTable$position <- 1
  diagosticsTable$dependOn(c(.robmaTypeDependencies(type), "mcmcDiagnosticsOverviewTable", "inferenceShortenPriorName"))
  diagnostics[["diagosticsTable"]] <- diagosticsTable

  overtitlePrior <- gettext("Prior Distribution")

  diagosticsTable$addColumnInfo(name = "number",             title = "#",                           type = "integer")
  if (!is.null(fitSummary))
    diagosticsTable <- .robmaAddPriorColumn(diagosticsTable, fitSummary[["diagnostics"]])
  diagosticsTable$addColumnInfo(name = "mcmcError",          title = gettext("max(MCMC error)"),    type = "number")
  diagosticsTable$addColumnInfo(name = "mcmcErrorSd",        title = gettext("max(MCMC error/SD)"), type = "number")
  diagosticsTable$addColumnInfo(name = "ess",                title = gettext("min(ESS)"),           type = "integer")
  diagosticsTable$addColumnInfo(name = "rHat",               title = gettext("max(R-hat)"),         type = "number")


  if (is.null(fitSummary))
    return()


  for (i in 1:nrow(fitSummary[["diagnostics"]])) {
   tempRow <- list(
      number             = fitSummary[["diagnostics"]][i, "Model"],
      mcmcError          = fitSummary[["diagnostics"]][i, "max_MCMC_error"],
      mcmcErrorSd        = fitSummary[["diagnostics"]][i, "max_MCMC_SD_error"],
      ess                = fitSummary[["diagnostics"]][i, "min_ESS"],
      rHat               = fitSummary[["diagnostics"]][i, "max_R_hat"]
    )
    tempRow <- .robmaFillPriorColumn(tempRow, fitSummary[["diagnostics"]][i,])
    diagosticsTable$addRows(tempRow)
  }

  return()
}
.robmaDiagnosticsPlots         <- function(jaspResults, options, type) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 14
    diagnostics$dependOn(.robmaTypeDependencies(type))
    jaspResults[["diagnostics"]] <- diagnostics
  } else {
    diagnostics <- jaspResults[["diagnostics"]]
  }


  ### create waiting plot
  if (is.null(jaspResults[["model"]]))
    wait <- TRUE
  else if (type == "RoBMA" && !.robmaCheckDiagnostics(options, any = FALSE))
    wait <- TRUE
  else if (type == "BiBMA" && !.bibmaCheckDiagnostics(options, any = FALSE))
    wait <- TRUE
  else
    wait <- FALSE

  if (wait) {
    tempWait  <- createJaspHtml(text = gettext("MCMC Diagnostics plots are created once both the plotted parameter ('Plot') and the plot type ('Type') options are selected."))
    tempWait$dependOn(c("mcmcDiagnosticsPlotEffect", "mcmcDiagnosticsPlotHeterogeneity", "mcmcDiagnosticsPlotWeights", "mcmcDiagnosticsPlotPet", "mcmcDiagnosticsPlotPeese",
                        "mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation", "mcmcDiagnosticsPlotTypePosteriorSamplesDensity"))
    diagnostics[["tempWait"]] <- tempWait
    return()
  }


  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # select models to iterate over
  if (options[["mcmcDiagnosticsPlotSingleModel"]]) {
    modelsI <- options[["mcmcDiagnosticsPlotSingleModelNumber"]]
    if (modelsI < 1 || modelsI > length(fit[["models"]])) {
      tempModel  <- createJaspContainer(title = gettextf("Model %i", modelsI))
      diagnostics[[paste0("model", modelsI)]] <- tempModel
      tempError  <- createJaspPlot(title = "")
      tempError$dependOn("mcmcDiagnosticsPlotSingleModelNumber", "mcmcDiagnosticsPlotSingleModel")
      tempError$setError(gettextf("Model %1$i does not exist. Select one of the models between 1 and %2$i.", modelsI, length(fit[["models"]])))
      tempModel[["tempError"]] <- tempError
      return()
    }
  } else {
    modelsI <- 1:length(fit[["models"]])
  }

  # collect the parameters
  parameters <- NULL
  if (options[["mcmcDiagnosticsPlotEffect"]])
    parameters <- c(parameters, "mu")
  if (options[["mcmcDiagnosticsPlotHeterogeneity"]])
    parameters <- c(parameters, "tau")
  if (options[["mcmcDiagnosticsPlotWeights"]])
    parameters <- c(parameters, "omega")
  if (options[["mcmcDiagnosticsPlotPet"]])
    parameters <- c(parameters, "PET")
  if (options[["mcmcDiagnosticsPlotPeese"]])
    parameters <- c(parameters, "PEESE")


  # do the iterations
  for (i in modelsI) {
    # create / access container for individual models
    if (is.null(diagnostics[[paste0("model_", i)]])) {
      tempModel <- createJaspContainer(title = gettextf("Model %i", i))
      tempModel$position <- i
      tempModel$dependOn(c("mcmcDiagnosticsPlotSingleModelNumber", "mcmcDiagnosticsPlotSingleModel"))
      diagnostics[[paste0("model", i)]] <- tempModel
    } else {
      tempModel <- diagnostics[[paste0("model", i)]]
    }

    noPars <- TRUE # tracker for checking whether any parameter was plotted

    for (par in parameters) {
      # create / access container for individual parameters
      if (is.null(tempModel[[par]])) {
        tempPar <- createJaspContainer(title = switch(
          par,
          "mu"    = gettext("Effect"),
          "tau"   = gettext("Heterogeneity"),
          "omega" = gettext("Weights"),
          "PET"   = gettext("PET"),
          "PEESE" = gettext("PEESE")
        ))
        tempPar$position <- switch(
          par,
          "mu"    = 1,
          "tau"   = 2,
          "omega" = 3,
          "PET"   = 4,
          "PEESE" = 5
        )
        tempPar$dependOn(switch(
          par,
          "mu"    = "mcmcDiagnosticsPlotEffect",
          "tau"   = "mcmcDiagnosticsPlotHeterogeneity",
          "omega" = "mcmcDiagnosticsPlotWeights",
          "PET"   = "mcmcDiagnosticsPlotPet",
          "PEESE" = "mcmcDiagnosticsPlotPeese"
        ))
        tempModel[[par]] <- tempPar
      } else {
        tempPar <- tempModel[[par]]
      }


      # add trace plots
      if (options[["mcmcDiagnosticsPlotTypeTrace"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["trace"]])) {
          tempPlots <- createJaspContainer(gettext("Trace plots"))
          tempPlots$position <- 1
          tempPlots$dependOn("mcmcDiagnosticsPlotTypeTrace")
          tempPar[["trace"]] <- tempPlots
        } else {
          tempPlots <- tempPar[["trace"]]
        }

        # create plots
        newPlots <- RoBMA::diagnostics(
          fit,
          parameter     = par,
          type          = "chains",
          plot_type     = "ggplot",
          show_models   = i,
          title         = FALSE
        )

        # (temporal) fix for the package returning a list with empty plots on the remaining spots
        newPlots <- newPlots[!sapply(newPlots, is.null)]

        if (length(newPlots) == 0)
          next

        noPars <- FALSE

        # add them to the container
        if (!ggplot2::is.ggplot(newPlots)) {
          for (pi in 1:length(newPlots)) {
            tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
            tempPlots[[paste0("trace", pi)]] <- tempPlot
            tempPlot[["plotObject"]] <-
              jaspGraphs::themeJasp(newPlots[[pi]])
          }
        } else {
          tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
          tempPlots[[paste0("trace", 1)]] <- tempPlot
          tempPlot[["plotObject"]] <- jaspGraphs::themeJasp(newPlots)
        }

      }


      # add autocorrelation plots
      if (options[["mcmcDiagnosticsPlotTypeAutocorrelation"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["autocor"]])) {
          tempPlots <- createJaspContainer(gettext("Average autocorrelations"))
          tempPlots$position <- 2
          tempPlots$dependOn("mcmcDiagnosticsPlotTypeAutocorrelation")
          tempPar[["autocor"]] <- tempPlots
        } else {
          tempPlots <- tempPar[["autocor"]]
        }

        # create plots
        newPlots <- RoBMA::diagnostics(
          fit,
          parameter     = par,
          type          = "autocorrelations",
          plot_type     = "ggplot",
          show_models   = i,
          title         = FALSE
        )

        # (temporal) fix for the package returning a list with empty plots on the remaining spots
        newPlots <- newPlots[!sapply(newPlots, is.null)]

        if (length(newPlots) == 0)
          next

        noPars <- FALSE

        # add them to the container
        if (!ggplot2::is.ggplot(newPlots)) {
          for (pi in 1:length(newPlots)) {
            tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
            tempPlots[[paste0("autocor", pi)]] <- tempPlot
            tempPlot[["plotObject"]] <-
              jaspGraphs::themeJasp(newPlots[[pi]])
          }

        } else {
          tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
          tempPlots[[paste0("autocor", 1)]] <- tempPlot
          tempPlot[["plotObject"]] <- jaspGraphs::themeJasp(newPlots)
        }

      }


      # add sample densities plots
      if (options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["samples"]])) {
          tempPlots <- createJaspContainer(gettext("Posterior samples densities"))
          tempPlots$position <- 3
          tempPlots$dependOn("mcmcDiagnosticsPlotTypePosteriorSamplesDensity")
          tempPar[["samples"]] <- tempPlots
        } else {
          tempPlots <- tempPar[["samples"]]
        }

        # create plots
        newPlots <- RoBMA::diagnostics(
          fit,
          parameter     = par,
          type          = "densities",
          plot_type     = "ggplot",
          show_models   = i,
          title         = FALSE
        )

        # (temporal) fix for the package returning a list with empty plots on the remaining spots
        newPlots <- newPlots[!sapply(newPlots, is.null)]

        if (length(newPlots) == 0)
          next

        noPars <- FALSE

        # add them to the container
        if (!ggplot2::is.ggplot(newPlots)) {
          for (pi in 1:length(newPlots)) {
            tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
            tempPlots[[paste0("samples", pi)]] <- tempPlot
            tempPlot[["plotObject"]] <- jaspGraphs::themeJasp(newPlots[[pi]])
          }

        } else {
          tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
          tempPlots[[paste0("samples", 1)]] <- tempPlot
          tempPlot[["plotObject"]] <- jaspGraphs::themeJasp(newPlots)
        }

      }

    }

    # show error if only one model is selected but doesn't contain any of the diagnostics
    if (noPars && options[["mcmcDiagnosticsPlotSingleModelNumber"]]) {
      tempError  <- createJaspHtml(text = gettextf("Model %i does not contain any of the selected parameters.", i))
      tempError$dependOn(c("mcmcDiagnosticsPlotEffect", "mcmcDiagnosticsPlotHeterogeneity", "mcmcDiagnosticsPlotWeights", "mcmcDiagnosticsPlotPet", "mcmcDiagnosticsPlotPeese",
                           "mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation", "mcmcDiagnosticsPlotTypePosteriorSamplesDensity"))
      tempModel[["tempError"]] <- tempError
    }
  }

  return()
}
.robmaGetModelTypeOption       <- function(options) {
  return(switch(
    options[["modelEnsembleType"]],
    "PSMA"     = "PSMA",
    "PP"       = "PP",
    "original" = "2w",
    "custom"   = "custom"
  ))
}
.robmaGetPriorScaleOption      <- function(options) {
  if (options[["inputType"]] == "unstandardizedEffectSizes")
    return("none")
  else
    return(switch(
      options[["priorScale"]],
      "cohensD"   = "cohens_d",
      "fishersZ"  = "fishers_z",
      "logOr"     = "logOR"
    ))
}
.robmaGetFittingScaleOption    <- function(options) {
  if (options[["inputType"]] == "unstandardizedEffectSizes")
    return("none")
  else
    return(switch(
      options[["advancedEstimationScale"]],
      "cohensD"   = "cohens_d",
      "fishersZ"  = "fishers_z",
      "logOr"     = "logOR"
    ))
}
.robmaGetOutputScaleOption     <- function(options) {
  if (!is.null(options[["inputType"]]) && options[["inputType"]] != "unstandardizedEffectSizes")
    return(NULL)
  else
    return(switch(
      options[["inferenceOutputScale"]],
      "cohensD"     = "cohens_d",
      "fishersZ"    = "fishers_z",
      "logOr"       = "logOR",
      "or"          = "OR",
      "correlation" = "r"
    ))
}
.robmaGetIndividualModelOrderOption <- function(options){
  return(switch(
    options[["plotsIndividualModelsOrderBy"]],
    "modelNumber"          = "model",
    "estimate"             = "estimate",
    "bayesFactor"          = "BF",
    "posteriorProbability" = "probability"
  ))
}
.robmaSaveModel                <- function(jaspResults, options) {
  if (is.null(jaspResults[["modelSaved"]])) {
    modelSaved <- createJaspState()
    modelSaved$dependOn(c(.robmaDependencies, "advancedSaveFittedModel"))
    jaspResults[["modelSaved"]] <- modelSaved

  }

  modelSaved[["object"]] <- TRUE
}
.robmaPriorsToOptionsNames     <- function(priors) {
  priorNames               <- names(priors)
  substr(priorNames, 1, 1) <- toupper(substr(priorNames, 1, 1))
  priorNames               <- paste0("models", priorNames)
  names(priors)            <- priorNames
  return(priors)
}
.robmaErrorHandling            <- function(fit, options) {

  if (jaspBase::isTryError(fit))
    .quitAnalysis(fit)

  if (all(!RoBMA:::.get_model_convergence(fit))) {
    if (options[["advancedRemoveFailedModels"]])
      .quitAnalysis(gettext("All models failed to converge under the MCMC convergence criteria. Please update the MCMC settings."))
    else
      .quitAnalysis(gettext("All models failed to converge. Please update the MCMC settings."))
  }

  return()
}
