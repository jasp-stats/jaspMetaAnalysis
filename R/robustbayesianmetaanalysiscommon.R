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

RobustBayesianMetaAnalysisCommon <- function(jaspResults, dataset, options, state = NULL) {

  # this is needed to register the contrasts till BayesTools is updated
  contr.meandif     <<- BayesTools::contr.meandif
  contr.orthonormal <<- BayesTools::contr.orthonormal
  contr.independent <<- BayesTools::contr.independent

  # devel settings
  # options[["advancedMcmcChains"]]     <- 2
  # options[["advancedMcmcAdaptation"]] <- 500
  # options[["advancedMcmcBurnin"]]     <- 1000
  # options[["advancedMcmcSamples"]]    <- 2000

  # attach priors to options
  options <- .robmaAttachPriors(options)

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
    if (options[["metaregressionTermTests"]] && options[["bayesianModelAveragingModerations"]])
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

  # publication bias adjustment tables
  if (options[["publicationBiasAdjustmentWeightfunctionEstimates"]] && .robmaHasWeightfunction(options))
    .robmaPublicationBiasWeightfunctionEstimatesTable(jaspResults, options)
  if (options[["publicationBiasAdjustmentWeightfunctionEstimates"]] && options[["conditionalEstimates"]] && .robmaHasWeightfunction(options))
    .robmaPublicationBiasWeightfunctionEstimatesTable(jaspResults, options, conditional = TRUE)
  if (options[["publicationBiasAdjustmentPetPeeseEstimates"]] && .robmaHasPetPeese(options))
    .robmaPublicationBiasPetPeeseEstimatesTable(jaspResults, options)
  if (options[["publicationBiasAdjustmentPetPeeseEstimates"]] && options[["conditionalEstimates"]] && .robmaHasPetPeese(options))
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
  if (options[["priorAndPosteriorPlotWeightFunction"]] && .robmaHasWeightfunction(options))
    .robmaPriorAndPosteriorPlot(jaspResults, options, "weightFunction")
  if (options[["priorAndPosteriorPlotPetPeese"]] && .robmaHasPetPeese(options))
    .robmaPriorAndPosteriorPlot(jaspResults, options, "petPeese")

  # plots
  .maUltimateForestPlot(jaspResults, options)
  .maBubblePlot(jaspResults, options)

  # diagnostics
  if (options[["mcmcDiagnosticsOverviewTable"]])
    .robmaDiagnosticsTable(jaspResults, options)
  if (options[["mcmcDiagnosticsPlotEffectSize"]])
    .robmaDiagnosticsPlot(jaspResults, options, "pooledEffect")
  if (options[["mcmcDiagnosticsPlotHeterogeneity"]])
    .robmaDiagnosticsPlot(jaspResults, options, "heterogeneity")
  if (options[["mcmcDiagnosticsPlotModeration"]])
    .robmaDiagnosticsPlot(jaspResults, options, "moderation")
  if (options[["mcmcDiagnosticsPlotWeights"]] && .robmaHasWeightfunction(options))
    .robmaDiagnosticsPlot(jaspResults, options, "weights")
  if (options[["mcmcDiagnosticsPlotPet"]]     && .robmaHasPet(options))
    .robmaDiagnosticsPlot(jaspResults, options, "pet")
  if (options[["mcmcDiagnosticsPlotPeese"]]   && .robmaHasPeese(options))
    .robmaDiagnosticsPlot(jaspResults, options, "peese")


  # additional
  if (options[["showRoBMARCode"]])
    .robmaShowRobmaRCode(jaspResults, options)

  return()
}

.robmaDependencies <- c(
  "effectSize", "effectSizeStandardError", "effectSizeMeasure",                   # RoBMA / NoBMA
  "successesGroup1", "successesGroup2", "sampleSizeGroup1", "sampleSizeGroup2",   # BiBMA
  "predictors", "predictors.types", "studyLevelMultilevel", "subgroup",
  "effectSizeModelTerms", "effectSizeModelIncludeIntercept",
  "bayesianModelAveragingEffectSize", "bayesianModelAveragingHeterogeneity", "bayesianModelAveragingModerations", "bayesianModelAveragingPublicationBias",
  "priorDistributionsEffectSizeAndHeterogeneity", "priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield", "priorDistributionsScale", "publicationBiasAdjustment", "modelExpectedDirectionOfTheEffect",
  # prior distributions
  "priorsEffect", "priorsEffectNull", "priorsHeterogeneity", "priorsHeterogeneityNull",
  "priorsModeratorsFactor", "priorsModeratorsFactorNull", "priorsModeratorsContinuous", "priorsModeratorsContinuousNull",
  "priorsBiasSelectionModels", "priorsBiasSelectionModelsNull", "priorsBiasPet", "priorsBiasPetNull", "priorsBiasPeese", "priorsBiasPeeseNull",
  "priorsBaseline", "priorsBaselineNull",

  # MCMC settings
  "advancedMcmcAdaptation", "advancedMcmcBurnin", "advancedMcmcSamples", "advancedMcmcChains", "advancedMcmcThin",
  "autofit", "advancedAutofitRHat", "advancedAutofitRHatTarget", "advancedAutofitEss", "advancedAutofitEssTarget", "advancedAutofitMcmcError",
  "advancedAutofitMcmcErrorTarget", "advancedAutofitMcmcErrorSd", "advancedAutofitMcmcErrorSdTarget", "advancedAutofitMaximumFittingTime",
  "advancedAutofitMaximumFittingTimeTarget", "advancedAutofitMaximumFittingTimeTargetUnit", "advancedAutofitExtendSamples",
  "seed", "setSeed"
)

# model fitting function
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

# priors related functions
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
.robmaPriorAndPosteriorPlot              <- function(jaspResults, options, parameter) {

  # section container
  priorAndPosteriorPlotContainer <- .robmaExtractPriorAndPosteriorPlotContainer(jaspResults)

  # plot container
  if (!is.null(priorAndPosteriorPlotContainer[[parameter]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
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
    tempPlot$title <- tempTitle
    tempPlot$dependOn(c(.robmaDependencies, tempDependencies))
    tempPlot$position <- tempPosition
    priorAndPosteriorPlotContainer[[parameter]] <- tempPlot

  } else {

    # create the output container
    tempPlot       <- createJaspContainer()
    tempPlot$title <- tempTitle
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
.robmaDiagnosticsTable                   <- function(jaspResults, options) {

  # create/extract section otherwise
  diagnosticsContainer <- .robmaExtractDiagnosticsContainer(jaspResults)

  if (!is.null(diagnosticsContainer[["diagnosticsTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    tempTable       <- .robmaDiagnosticsTableFun(fit = fit[[1]], options = options)
    tempTable$title <- gettext("Diagnostics Summary")
    tempTable$dependOn(c(.robmaDependencies, "mcmcDiagnosticsOverviewTable"))
    tempTable$position <- 1
    diagnosticsContainer[["diagnosticsTable"]] <- tempTable

  } else {

    # create the output container
    tempTable       <- createJaspContainer()
    tempTable$title <- gettext("Diagnostics Summary")
    tempTable$dependOn(c(.robmaDependencies, "mcmcDiagnosticsOverviewTable"))
    tempTable$position <- 1
    diagnosticsContainer[["diagnosticsTable"]] <- tempTable

    for (i in seq_along(fit)) {
      tempTable[[names(fit)[i]]]          <- .robmaDiagnosticsTableFun(fit = fit[[i]], options = options)
      tempTable[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      tempTable[[names(fit)[i]]]$position <- i
    }
  }

  return()
}
.robmaDiagnosticsTableFun                <- function(fit, options) {

  # create table
  tempTable <- createJaspTable()

  tempTable$addColumnInfo(name = "par",            type = "string",   title = "")
  tempTable$addColumnInfo(name = "MCMC_error",     type = "number",   title = gettext("MCMC error"))
  tempTable$addColumnInfo(name = "MCMC_SD_error",  type = "number",   title = gettext("MCMC error/SD"))
  tempTable$addColumnInfo(name = "ESS",            type = "integer",  title = gettext("ESS"))
  tempTable$addColumnInfo(name = "R_hat",          type = "number",   title = gettext("R-hat"))

  # stop on error
  if (jaspBase::isTryError(fit)) {
    return(tempTable)
  }

  diagnosticsSummary     <- data.frame(summary(fit, type = "diagnostics")[["estimates"]])
  diagnosticsSummary$par <- rownames(diagnosticsSummary)
  diagnosticsSummary     <- diagnosticsSummary[,c("par", "MCMC_error", "MCMC_SD_error", "ESS", "R_hat")]
  diagnosticsSummary$ESS <- round(diagnosticsSummary$ESS)

  # obtain residual funnel plot
  tempTable$setData(diagnosticsSummary)

  return(tempTable)
}
.robmaDiagnosticsPlot                    <- function(jaspResults, options, parameter) {

  # section container
  diagnosticsContainer <- .robmaExtractDiagnosticsContainer(jaspResults)

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  # dispatch options
  if (parameter == "pooledEffect") {
    tempTitle        <- if (.maIsMetaregression(options)) gettext("Adjusted Effect") else gettext("Pooled Effect")
    tempPosition     <- 2
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotEffectSize"
    parameter        <- "mu"
  } else if (parameter == "heterogeneity") {
    tempTitle        <- gettext("Heterogeneity")
    tempPosition     <- 3
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotHeterogeneity"
    parameter        <- "tau"
  } else if (parameter == "moderation") {
    tempTitle        <- gettext("Moderation")
    tempPosition     <- 4
    tempFunction     <- .robmaDiagnosticsPlotModerationFun
    tempDependencies <- "mcmcDiagnosticsPlotModeration"
    fit              <- list(fit) # trick to forward the fit to a futher dispatch later
  } else if (parameter == "weights") {
    tempTitle        <- gettext("Weights")
    tempPosition     <- 5
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotWeights"
    parameter        <- "omega"
  } else if (parameter == "pet") {
    tempTitle        <- gettext("PET")
    tempPosition     <- 6
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotPet"
    parameter        <- "PET"
  } else if (parameter == "peese") {
    tempTitle        <- gettext("PEESE")
    tempPosition     <- 7
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotPeese"
    parameter        <- "PEESE"
  }


  # create individual plots for each subgroup
  if (options[["subgroup"]] == "" || parameter == "moderation") {

    tempContainer <- .robmaExtractDiagnosticsSubContainer(diagnosticsContainer, parameter, tempTitle, tempPosition, tempDependencies)
    do.call(tempFunction, list(container = tempContainer, options = options, fit = fit[[1]], parameter = parameter))

  } else {

    # create the output container
    tempContainerBlock       <- createJaspContainer()
    tempContainerBlock$title <- tempTitle
    tempContainerBlock$dependOn(tempDependencies)
    tempContainerBlock$position <- tempPosition
    diagnosticsContainer[[parameter]] <- tempContainerBlock

    for (i in seq_along(fit)) {

      tempContainer <- .robmaExtractDiagnosticsSubContainer(tempContainerBlock, names(fit)[i], gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup")), i, "")
      do.call(tempFunction, list(container = tempContainer, options = options, fit = fit[[i]], parameter = parameter))

    }

  }

  return()
}
.robmaDiagnosticsPlotFun                 <- function(container, options, fit, parameter) {

  if (jaspBase::isTryError(fit))
    return()

  # create a waiting plot for at least one type of the plot to be selected
  if (!options[["mcmcDiagnosticsPlotTypeTrace"]] && !options[["mcmcDiagnosticsPlotTypeAutocorrelation"]] && !options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]) {
    .robmaDiagnosticsStoreWaitingPlot(container)
    return()
  }

  .robmaDiagnosticsStorePlot(container, options, fit, parameter, "chains",          gettext("Trace Plot"),           1, "mcmcDiagnosticsPlotTypeTrace")
  .robmaDiagnosticsStorePlot(container, options, fit, parameter, "autocorrelation", gettext("Autocorrelation Plot"), 2, "mcmcDiagnosticsPlotTypeAutocorrelation")
  .robmaDiagnosticsStorePlot(container, options, fit, parameter, "densityPlot",     gettext("Density Plot"),         3, "mcmcDiagnosticsPlotTypePosteriorSamplesDensity")

  return()
}
.robmaDiagnosticsPlotModerationFun       <- function(container, options, fit, parameter) {

  moderators <- unlist(options[["effectSizeModelTerms"]])

  for (j in seq_along(moderators)) {

    parameter <- moderators[j]

    # create individual plots for each subgroup
    if (options[["subgroup"]] == "" || parameter == "moderation") {

      tempContainer <- .robmaExtractDiagnosticsSubContainer(container, parameter, parameter, 1, "")
      do.call(.robmaDiagnosticsPlotFun, list(container = tempContainer, options = options, fit = fit[[1]], parameter = parameter))

    } else {

      # create the output container
      tempContainerBlock       <- createJaspContainer()
      tempContainerBlock$title <- parameter
      tempContainerBlock$position <- 3
      container[[parameter]] <- tempContainerBlock

      for (i in seq_along(fit)) {

        tempContainer <- .robmaExtractDiagnosticsSubContainer(tempContainerBlock, names(fit)[i], gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup")), i, "")
        do.call(.robmaDiagnosticsPlotFun, list(container = tempContainer, options = options, fit = fit[[i]], parameter = parameter))

      }

    }
  }

  return()
}
.robmaDiagnosticsStorePlot               <- function(container, options, fit, parameter, type, title, position, dependency) {

  if (!(options[[dependency]] && is.null(container[[type]])))
    return()

  tempPlot <- RoBMA::diagnostics(
    fit,
    parameter     = parameter,
    type          = type,
    plot_type     = "ggplot"
  )

  # add them to the container
  if (!ggplot2::is.ggplot(tempPlot)) {

    subcontainer <- createJaspContainer(title = title)
    subcontainer$position <- position
    subcontainer$dependOn(dependency)
    container[[type]] <- subcontainer

    for (i in 1:length(tempPlot)) {

      subplot <- createJaspPlot(width = 320, height = 250)
      subplot$position <- i
      subplot$dependOn(dependency)
      subplot$plotObject <- tempPlot[[i]] + ggplot2::labs(title = "") + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()
      subcontainer[[paste0(type, i)]] <- subplot

    }

  } else {

    subplot <- createJaspPlot(title = title, width = 320, height = 250)
    subplot$position <- position
    subplot$dependOn(dependency)
    subplot$plotObject <- tempPlot + ggplot2::labs(title = "") + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()
    container[[type]] <- subplot

  }

  return()
}
.robmaDiagnosticsStoreWaitingPlot        <- function(container) {

  if (!is.null(container[["waitingPlot"]]))
    return()

  subplot <- createJaspPlot(title = "", width = 320, height = 250)
  subplot$position <- 1
  subplot$setError(gettext("Please select at least one 'Type' of diagnostics plot (e.g., 'Trace', 'Autocorrelation' or 'Posterior samples density')."))
  subplot$dependOn(c("mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation", "mcmcDiagnosticsPlotTypePosteriorSamplesDensity"))

  container[["waitingPlot"]] <- subplot

  return()
}
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

# containers
.robmaExtractModelSummaryContainer           <- function(jaspResults) {

  if (!is.null(jaspResults[["modelSummaryContainer"]]))
    return(jaspResults[["modelSummaryContainer"]])

  # create the output container
  modelSummaryContainer <- createJaspContainer(gettext("Model Summary"))
  modelSummaryContainer$dependOn(.robmaDependencies)
  modelSummaryContainer$position <- 2
  jaspResults[["modelSummaryContainer"]] <- modelSummaryContainer

  return(modelSummaryContainer)
}
.robmaExtractMetaregressionContainer         <- function(jaspResults) {

  if (!is.null(jaspResults[["metaregressionContainer"]]))
    return(jaspResults[["metaregressionContainer"]])

  # create the output container
  metaregressionContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  metaregressionContainer$dependOn(c(.robmaDependencies))
  metaregressionContainer$position <- 3
  jaspResults[["metaregressionContainer"]] <- metaregressionContainer

  return(metaregressionContainer)
}
.robmaExtractPublicationBiasContainer        <- function(jaspResults) {

  if (!is.null(jaspResults[["publicationBiasContainer"]]))
    return(jaspResults[["publicationBiasContainer"]])

  # create the output container
  publicationBiasContainer <- createJaspContainer(gettext("Publication Bias Adjustment Summary"))
  publicationBiasContainer$dependOn(c(.robmaDependencies, "confidenceIntervals"))
  publicationBiasContainer$position <- 4
  jaspResults[["publicationBiasContainer"]] <- publicationBiasContainer

  return(publicationBiasContainer)
}
.robmaExtractEstimatedMarginalMeansContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["estimatedMarginalMeansContainer"]]))
    return(jaspResults[["estimatedMarginalMeansContainer"]])

  # create the output container
  estimatedMarginalMeansContainer <- createJaspContainer(gettext("Estimated Marginal Means Summary"))
  estimatedMarginalMeansContainer$dependOn(c(.robmaDependencies, "confidenceIntervals", "confidenceIntervalsLevel", "includeFullDatasetInSubgroupAnalysis"))
  estimatedMarginalMeansContainer$position <- 5
  jaspResults[["estimatedMarginalMeansContainer"]] <- estimatedMarginalMeansContainer

  return(estimatedMarginalMeansContainer)
}
.robmaExtractPriorAndPosteriorPlotContainer  <- function(jaspResults) {

  if (!is.null(jaspResults[["priorAndPosteriorPlotContainer"]]))
    return(jaspResults[["priorAndPosteriorPlotContainer"]])

  # create the output container
  priorAndPosteriorPlotContainer <- createJaspContainer(gettext("Prior and Posterior Plots"))
  priorAndPosteriorPlotContainer$dependOn(c(.robmaDependencies, "includeFullDatasetInSubgroupAnalysis", "priorAndPosteriorPlotType", "priorAndPosteriorPlotIncludePriorDistribution"))
  priorAndPosteriorPlotContainer$position <- 6
  jaspResults[["priorAndPosteriorPlotContainer"]] <- priorAndPosteriorPlotContainer

  return(priorAndPosteriorPlotContainer)
}
.robmaExtractDiagnosticsContainer            <- function(jaspResults) {

  if (!is.null(jaspResults[["diagnosticsContainer"]]))
    return(jaspResults[["diagnosticsContainer"]])

  # create the output container
  diagnosticsContainer <- createJaspContainer(gettext("Diagnostics"))
  diagnosticsContainer$dependOn(.robmaDependencies)
  diagnosticsContainer$position <- 8
  jaspResults[["diagnosticsContainer"]] <- diagnosticsContainer

  return(diagnosticsContainer)
}
.robmaExtractDiagnosticsSubContainer         <- function(diagnosticsContainer, parameter, title, position, dependencies) {

  if (!is.null(diagnosticsContainer[[parameter]]))
    return(diagnosticsContainer[[parameter]])

  # create the output container
  diagnosticsSubContainer <- createJaspContainer(title)
  diagnosticsSubContainer$dependOn(dependencies)
  diagnosticsSubContainer$position <- position
  diagnosticsContainer[[parameter]] <- diagnosticsSubContainer

  return(diagnosticsSubContainer)
}

# additional compute functions
.robmaComputePooledEffect           <- function(fit, options, conditional, returnRaw = FALSE) {

  if (!.maIsMetaregression(options)) {
    # the adjusted estimate corresponds to the pooled estimate for non-regression models
    return(.robmaComputeInterceptEffect(fit, options, conditional, returnRaw))
  }

  # the following function is necessary only for a meta-regression
  estimate <- RoBMA::pooled_effect(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]

  estimate <- data.frame(
    "par"    = gettext("Pooled effect"),
    "mean"   = estimate[["Mean"]][1],
    "median" = estimate[["Median"]][1],
    "lCi"    = estimate[[3]][1],
    "uCi"    = estimate[[4]][1],
    "lPi"    = estimate[[3]][2],
    "uPi"    = estimate[[4]][2]
  )

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
.robmaComputeAdjustedEffect         <- function(fit, options, conditional, returnRaw = FALSE) {

  if (!.maIsMetaregression(options) || (.maIsMetaregression(options) && .robmaIsMetaregressionCentered(options))) {
    # the adjusted estimate corresponds to the pooled estimate for non-regression models
    # and to the intercept estimate for centered regression models
    return(.robmaComputeInterceptEffect(fit, options, conditional, returnRaw))
  }

  # the following function is necessary only for a meta-regression
  estimate <- RoBMA::adjusted_effect(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]

  estimate <- data.frame(
    "par"    = gettext("Adjusted effect"),
    "mean"   = estimate[["Mean"]][1],
    "median" = estimate[["Median"]][1],
    "lCi"    = estimate[[3]][1],
    "uCi"    = estimate[[4]][1],
    "lPi"    = estimate[[3]][2],
    "uPi"    = estimate[[4]][2]
  )

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
.robmaComputeInterceptEffect        <- function(fit, options, conditional, returnRaw = FALSE) {

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

# additional help functions
.robmaIsMetaregressionCentered         <- function(options){

  if (!.maIsMetaregression(options))
    return(FALSE)

  priors          <- attr(options, "priors")
  priorModerators <- priors[["moderators"]]

  for (priorModerator in priorModerators) {
    for (i in seq_along(priorModerator)) {
      if (BayesTools::is.prior.treatment(priorModerator[[i]]) || BayesTools::is.prior.independent(priorModerator[[i]]))
        return(FALSE)
    }
  }

  return(TRUE)
}
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

      tempFit      <- fit[[i]]
      tempWarnings <- attr(tempFit, "warnings")
      tempWarnings <- gsub("mu_", "", tempWarnings)
      tempWarnings <- tempWarnings[grep(parameter, tempWarnings)]

      if (length(tempWarnings) > 0) {
        messages <- c(messages, sapply(tempWarnings, function(x) gettextf("Subgroup %1$s, %2$s", tempFit$subgroup[1], x)))
      }
    }
  }

  if (parameter == "intercept")
    messages <- messages[!grepl("do not span", messages)]

  return(messages)
}
.robmaComponentNames      <- function(component, options) {
  return(switch(
    tolower(component),
    "effectsize"    = if (.maIsMetaregression(options) && .robmaIsMetaregressionCentered(options)) gettext("Adjusted effect") else if (.maIsMetaregression(options)) gettext("Effect intercept") else gettext("Pooled effect"),
    "effect"        = if (.maIsMetaregression(options) && .robmaIsMetaregressionCentered(options)) gettext("Adjusted effect") else if (.maIsMetaregression(options)) gettext("Effect intercept") else gettext("Pooled effect"),
    "heterogeneity" = gettext("Heterogeneity"),
    "bias"          = gettext("Publication bias"),
    "baseline"      = gettext("Baseline")
  ))
}
.robmaVariableNames       <- function(varNames, variables) {

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
.robmaHasHeterogeneity    <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["heterogeneity"]])) {
    if (priors[["heterogeneity"]][[i]][["distribution"]] != "point" ||
        (priors[["heterogeneity"]][[i]][["distribution"]] == "point" && priors[["heterogeneity"]][[i]][["parameters"]][["location"]] != 0))
      return(TRUE)
  }

  return(FALSE)
}
.robmaHasWeightfunction   <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["bias"]])) {
    if (BayesTools::is.prior.weightfunction(priors[["bias"]][[i]]))
      return(TRUE)
  }

  return(FALSE)
}
.robmaHasPetPeese         <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["bias"]])) {
    if (BayesTools::is.prior.PET(priors[["bias"]][[i]]) || BayesTools::is.prior.PEESE(priors[["bias"]][[i]]))
      return(TRUE)
  }

  return(FALSE)
}
.robmaHasPet              <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["bias"]])) {
    if (BayesTools::is.prior.PET(priors[["bias"]][[i]]))
      return(TRUE)
  }

  return(FALSE)
}
.robmaHasPeese            <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["bias"]])) {
    if (BayesTools::is.prior.PEESE(priors[["bias"]][[i]]))
      return(TRUE)
  }

  return(FALSE)
}
.robmaAddBfColumn         <- function(tempTable, options) {

  titleBF <- switch(
    options[["bayesFactorType"]],
    "BF10"    = gettext("Inclusion BF"),
    "BF01"    = gettext("Exclusion BF"),
    "LogBF10" = gettext("log(Inclusion BF)")
  )

  tempTable$addColumnInfo(name = "bf", title = titleBF, type = "number")

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


# print functions
.robmaPrintBf             <- function(bf) {

  bf <- sapply(bf, function(x) {
    if (is.na(x)) {
      return("NA")
    } else if (is.infinite(x) && x > 0) {
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
  out <- out[rownames(out) == switch(
    parameter,
    "effect" = "effectSize",
    "tau"    = "heterogeneityTau",
    "tau2"   = "heterogeneityTau2",
    "I2"     = "heterogeneityI2",
    "H2"     = "heterogeneityH2"
  ),,drop = FALSE]

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
