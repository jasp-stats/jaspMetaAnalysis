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


BayesianMetaAnalysisBinomial <- function(jaspResults, dataset, options, state = NULL) {

  # clean fitted model if it was changed
  if (!.bibmaCheckReady(options))
    .bibmaCleanModel(jaspResults)

  # load data
  if (.bibmaCheckReady(options))
    dataset <- .bibmaGetData(options, dataset)

  # get the priors
  .bibmaGetPriors(jaspResults, options)

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(jaspResults[["priors"]][["object"]], file = "C:/JASP/priors.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  return()
  # show the model preview
  if (is.null(jaspResults[["model"]]))
    .bibmaModelPreviewTable(jaspResults, options)

  # fit model model
  if (is.null(jaspResults[["modelNotifier"]]) && .bibmaCheckReady(options))
    .bibmaFitModel(jaspResults, dataset, options)

  ### Priors plot
  if (options[["priorDistributionPlot"]])
    .bibmaPriorsPlots(jaspResults, options)


  ### Inference
  # default summary
  .bibmaSummaryTable(jaspResults, options)
  # models overview
  if (options[["inferenceModelsOverview"]])
    .bibmaModelsOvervievTable(jaspResults, options)
  # models summary
  if (options[["inferenceIndividualModels"]])
    .bibmaModelsSummaryTable(jaspResults, options)


  ### Plots
  # forest plot
  if (options[["plotsForestPlot"]])
    .robmaForestPlot(jaspResults, options)

  # pooled estimates plots
  if (options[["plotsPooledEstimatesEffect"]])
    .robmaEstimatesPlot(jaspResults, options, "mu")
  if (options[["plotsPooledEstimatesHeterogeneity"]])
    .robmaEstimatesPlot(jaspResults, options, "tau")

  # individual models
  if (options[["plotsIndividualModelsEffect"]])
    .bibmaModelsPlot(jaspResults, options, "mu")
  if (options[["plotsIndividualModelsHeterogeneity"]])
    .bibmaModelsPlot(jaspResults, options, "tau")

  ### Diagnostics
  # overview
  if (options[["mcmcDiagnosticsOverviewTable"]])
    .bibmaDiagnosticsOverviewTable(jaspResults, options)
  # plots
  if ((
    options[["mcmcDiagnosticsPlotEffect"]]    ||
    options[["mcmcDiagnosticsPlotHeterogeneity"]]   ||
    options[["mcmcDiagnosticsPlotWeights"]] ||
    options[["mcmcDiagnosticsPlotPet"]]   ||
    options[["mcmcDiagnosticsPlotPeese"]]
  ) ||
  (
    options[["mcmcDiagnosticsPlotTypeTrace"]]           ||
    options[["mcmcDiagnosticsPlotTypeAutocorrelation"]] ||
    options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]
  ))
    .bibmaDiagnosticsPlots(jaspResults, options)

  ### Save the model
  if (options[["advancedSaveFittedModel"]] != "" && is.null(jaspResults[["modelSaved"]]))
    .bibmaSaveModel(jaspResults, options)


  return()
}

.bibmaDependencies <- c(
  "successesGroup1", "successesGroup2", "observationsGroup1", "observationsGroup2", "studyLabel",
  "modelsEffect", "modelsEffectNull", "modelsHeterogeneity", "modelsHeterogeneityNull", "modelsBaseline", "modelsBaselineNull",
  "advancedMcmcAdaptation", "advancedMcmcBurnin", "advancedMcmcSamples", "advancedMcmcChains", "advancedMcmcThin",
  "autofit", "advancedAutofitRHat", "advancedAutofitRHatTarget", "advancedAutofitEss", "advancedAutofitEssTarget", "advancedAutofitMcmcError", "advancedAutofitMcmcErrorTarget", "advancedAutofitMcmcErrorSd", "advancedAutofitMcmcErrorSdTarget", "advancedAutofitMaximumFittingTime", "advancedAutofitMaximumFittingTimeTarget", "advancedAutofitMaximumFittingTimeTargetUnit", "advancedAutofitExtendSamples",
  "advancedAutofitRemoveFailedModels", "advancedAutofitRebalanceComponentProbabilityOnModelFailure", "seed", "setSeed"
)

# helper functions
.bibmaCheckReady          <- function(options) {
  return(options[["successesGroup1"]] != "" && options[["successesGroup2"]] != "" && options[["observationsGroup1"]] != "" && options[["observationsGroup2"]] != "")
}
.bibmaModelNotifier       <- function(jaspResults) {
  # We don't wanna delete the bibma modele every time settings is change since bibma takes a lot of time to fit.
  # Therefore, we don't create dependencies on the fitted model (in cases when the model can be updated), but
  # on a notifier that tells us when there was the change. If possible, we don't refit the whole model,
  # just update the neccessary parts.

  if (is.null(jaspResults[["modelNotifier"]])) {
    modelNotifier <- createJaspState()
    modelNotifier$dependOn(.bibmaDependencies)
    jaspResults[["modelNotifier"]] <- modelNotifier
  }

  return()

}
.bibmaCleanModel          <- function(jaspResults) {

  if (!is.null(jaspResults[["model"]])) {
    jaspResults[["model"]] <- NULL
  }

  return()
}
.bibmaGetData             <- function(options, dataset) {

  if (!is.null(dataset)) {
    return(dataset)
  } else {
    varNames <- c(options[["successesGroup1"]], options[["successesGroup2"]], options[["observationsGroup1"]], unlist(options[["observationsGroup2"]]))
    varNames <- varNames[varNames != ""]

    dataset <- readDataSetToEnd(
      columns.as.numeric = varNames,
      columns = if (options[["studyLabel"]] != "") options[["studyLabel"]]
    )

    if (options[["studyLabel"]] != "") {
      dataset[[options[["studyLabel"]]]] <- as.character(dataset[[options[["studyLabel"]]]])
      if (any(!validUTF8(dataset[[options[["studyLabel"]]]])))
        .quitAnalysis(gettext("The study labels contain invalid characters. Please, remove them before running the analysis."))
    }

  }

  return(dataset)
}
.bibmaGetPriors           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["priors"]])) {
    return()
  } else {
    priors <- createJaspState()
    priors$dependOn(.bibmaDependencies)
    jaspResults[["priors"]] <- priors
  }

  object <- list()

  for(type in c("", "Null")) {
    priorElements <- paste0(c("modelsEffect", "modelsHeterogeneity", "modelsBaseline"), type)
    for (i in seq_along(priorElements)) {
      tmp <- NULL
      for (j in seq_along(options[[priorElements[i]]])) {
        tmpPrior <- try(.robmaExtractPriorsFromOptions(options[[priorElements[i]]][[j]], parameter = if(grepl("modelsBaseline", priorElements[i])) "factor" else "normal"))
        if (jaspBase::isTryError(tmpPrior))
          .quitAnalysis(tmpPrior)
        else
          tmp <- c(tmp, list(tmpPrior))
      }
      object[[priorElements[i]]] <- tmp
    }
  }


  priors[["object"]] <- object

  return()
}
# main functions
.bibmaPriorsPlots              <- function(jaspResults, options) {

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
    priors <- bibma::check_setup(model_type = options[["modelEnsembleType"]], silent = TRUE)$priors
  else if (is.null(jaspResults[["model"]]))
    priors <- jaspResults[["priors"]][["object"]]
  else
    priors <- jaspResults[["model"]][["object"]][["priors"]]


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

      tempPriors <- priors[[paste0(parameter, if (type == "null") "Null")]]

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
.bibmaModelPreviewTable        <- function(jaspResults, options) {

  # create / access the container
  if (!is.null(jaspResults[["modelPreview"]])) {
    return()
  } else {
    modelPreview <- createJaspContainer(title = gettext("Model Preview"))
    modelPreview$dependOn(.bibmaDependencies)
    modelPreview$position <- 1
    jaspResults[["modelPreview"]] <- modelPreview
  }


  # extract the priors
  priors  <- jaspResults[["priors"]][["object"]]


  # set error if no priors are specified
  if (
    (length(priors[["modelsEffect"]])        == 0 && length(priors[["modelsEffectNull"]])        == 0) ||
    (length(priors[["modelsHeterogeneity"]]) == 0 && length(priors[["modelsHeterogeneityNull"]]) == 0) ||
    (length(priors[["modelsBaseline"]])      == 0 && length(priors[["modelsBaselineNull"]])      == 0)
  ) {
    priorsError <- createJaspTable()
    priorsError$setError(gettext("Please specify a prior distribution for each parameter in the Models specification section (either null or alternative)."))
    modelPreview[["priorsError"]] <- priorsError
    return()
  }

  # create the setup table
  fitSummary   <- RoBMA::check_setup.BiBMA(
    priors_effect             = priors[["modelsEffect"]],
    priors_heterogeneity      = priors[["modelsHeterogeneity"]],
    priors_baseline           = priors[["modelsBaseline"]],
    priors_effect_null        = priors[["modelsEffectNull"]],
    priors_heterogeneity_null = priors[["modelsHeterogeneityNull"]],
    priors_baseline_null      = priors[["modelsBaselineNull"]],
    models                    = TRUE,
    silent                    = TRUE
  )


  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1

  overallSummary$addColumnInfo(name = "terms",     title = "",                type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"), type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),   type = "number")

  if (options[["inputType"]] != "fittedModel") {
    for (i in 1:nrow(fitSummary[["components"]])) {
      tempRow <- list(
        terms     = .robmaCompNames(rownames(fitSummary[["components"]])[i]),
        models    = paste0(fitSummary[["components"]][[i, "models"]], "/", attr(fitSummary[["components"]], "n_models")),
        priorProb = fitSummary[["components"]][[i, "prior_prob"]]
      )

      overallSummary$addRows(tempRow)
    }
    overallSummary$addFootnote(gettext("The analysis will estimate multiple meta-analytic models using MCMC and might require a prolonged time to complete."), symbol = "\u26A0")
  }

  modelPreview[["overallSummary"]] <- overallSummary


  ### create models overview table
  modelsSummary <- createJaspTable(title = gettext("Model Specification Preview"))
  modelsSummary$position <- 2

  overtitlePrior <- gettext("Prior Distribution")

  modelsSummary$addColumnInfo(name = "number",              title = "#",                         type = "integer")
  modelsSummary$addColumnInfo(name = "priorEffect",         title = gettext("Effect Size"),      type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorHeterogeneity",  title = gettext("Heterogeneity"),    type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorBaseline",       title = gettext("Baseline"),         type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorProb",           title = gettext("P(M)"),             type = "number")

  if (options[["inputType"]] != "fittedModel") {
    for (i in 1:nrow(fitSummary[["summary"]])) {
      tempRow <- list(
        number             = fitSummary[["summary"]][i, "Model"],
        priorEffect        = fitSummary[["summary"]][i, "Effect"],
        priorHeterogeneity = fitSummary[["summary"]][i, "Heterogeneity"],
        priorBaseline      = fitSummary[["summary"]][i, "Baseline"],
        priorProb          = fitSummary[["summary"]][i, "prior_prob"]
      )

      modelsSummary$addRows(tempRow)
    }
    modelsSummary$addFootnote(gettext("The analysis will estimate multiple meta-analytic models using MCMC and might require a prolonged time to complete."), symbol = "\u26A0")
  }

  modelPreview[["modelsSummary"]] <- modelsSummary


  return()
}
.bibmaFitModel                 <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["model"]])) {
    model <- createJaspState()
    model$dependOn(.bibmaDependencies[.bibmaDependencies != "studyLabel"])
    jaspResults[["model"]] <- model
    fit                    <- NULL
  } else {
    model <- jaspResults[["model"]]
    fit   <- model[["object"]]
  }

  if (is.null(fit)) {

    priors <- jaspResults[["priors"]]$object
    fit    <- try(RoBMA::BiBMA(
      # data
      x1          = dataset[[options[["successesGroup1"]]]],
      x2          = dataset[[options[["successesGroup2"]]]],
      n1          = dataset[[options[["observationsGroup1"]]]],
      n2          = dataset[[options[["observationsGroup2"]]]],
      study_names = if (options[["studyLabel"]] != "") dataset[[options[["studyLabel"]]]],
      # priors
      priors_effect             = priors[["modelsEffect"]],
      priors_heterogeneity      = priors[["modelsHeterogeneity"]],
      priors_baseline_null      = priors[["modelsBaseline"]],
      priors_effect_null        = priors[["modelsEffectNull"]],
      priors_heterogeneity_null = priors[["modelsHeterogeneityNull"]],
      priors_baseline_null      = priors[["modelsBaselineNull"]],
      # sampling settings
      chains  = options[["advancedMcmcChains"]],
      adapt   = options[["advancedMcmcAdaptation"]],
      burnin  = options[["advancedMcmcBurnin"]],
      sample  = options[["advancedMcmcSamples"]],
      thin    = options[["advancedMcmcThin"]],
      # additional settings
      autofit         = options[["autofit"]],
      autofit_control = bibma::set_autofit_control(
        max_Rhat      = if (options[["advancedAutofitRHat"]])        options[["advancedAutofitRHatTarget"]],
        min_ESS       = if (options[["advancedAutofitEss"]])         options[["advancedAutofitEssTarget"]],
        max_error     = if (options[["advancedAutofitMcmcError"]])   options[["advancedAutofitMcmcErrorTarget"]],
        max_SD_error  = if (options[["advancedAutofitMcmcErrorSd"]]) options[["advancedAutofitMcmcErrorSdTarget"]],
        max_time      = if (options[["advancedAutofitMaximumFittingTime"]])        list(time = options[["advancedAutofitMaximumFittingTimeTarget"]] , unit = options[["advancedAutofitMaximumFittingTimeTargetUnit"]]),
        sample_extend = options[["advancedAutofitExtendSamples"]]),
      convergence_checks = bibma::set_convergence_checks(
        max_Rhat            = if (options[["advancedAutofitRHat"]])        options[["advancedAutofitRHatTarget"]],
        min_ESS             = if (options[["advancedAutofitEss"]])         options[["advancedAutofitEssTarget"]],
        max_error           = if (options[["advancedAutofitMcmcError"]])   options[["advancedAutofitMcmcErrorTarget"]],
        max_SD_error        = if (options[["advancedAutofitMcmcErrorSd"]]) options[["advancedAutofitMcmcErrorSdTarget"]],
        remove_failed       = options[["advancedAutofitRemoveFailedModels"]],
        balance_probability = options[["advancedAutofitRebalanceComponentProbabilityOnModelFailure"]]
      ),
      save     = "all",
      seed     = .getSeedJASP(options),
      silent   = TRUE,
      is_JASP  = TRUE
    ))

  } else {
    # only tries to update labels if they weren't supplied originally
    fit <- update(fit, study_names = if (options[["studyLabel"]] != "") dataset[, options[["studyLabel"]]])

  }


  # error handling
  if (jaspBase::isTryError(fit))
    .quitAnalysis(fit)


  # update the fit and reset notifier
  model[["object"]] <- fit
  .bibmaModelNotifier(jaspResults)

  return()
}
.bibmaSummaryTable             <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainSummary"]])) {
    return()
  } else {
    # create container
    mainSummary <- createJaspContainer(title = gettext("Summary"))
    mainSummary$position <- 3
    summaryDependencies  <- c(.bibmaDependencies, "bayesFactorType", "inferenceCiWidth", "inferenceConditionalParameterEstimates", "inferenceOutputScale")
    mainSummary$dependOn(summaryDependencies)
    jaspResults[["mainSummary"]] <- mainSummary
  }

  if (is.null(jaspResults[["model"]])) {
    if (options[["inferenceConditionalParameterEstimates"]]) {
      conditionalSummary <- createJaspTable(title = gettext("Conditional Estimates"), dependencies = "inferenceConditionalParameterEstimates")
      conditionalSummary <- .robmaTableFillCoef(conditionalSummary, NULL, options)
      jaspResults[["mainSummary"]][["conditionalSummary"]] <- conditionalSummary
    }
    return()
  }

  # remove the model preview
  jaspResults[["modelPreview"]] <- NULL

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # some shared info
  fitSummary <- summary(
    fit,
    logBF        = options[["bayesFactorType"]] == "LogBF10",
    BF01         = options[["bayesFactorType"]] == "BF01",
    probs        = c(.5 + c(-1, 1) * options[["inferenceCiWidth"]] / 2),
    conditional  = options[["inferenceConditionalParameterEstimates"]],
    output_scale = .robmaGetOutputScaleOption(options)
  )

  titleBF <- switch(
    options[["bayesFactorType"]],
    "BF10"    = gettext("Inclusion BF"),
    "BF01"    = gettext("Exclusion BF"),
    "LogBF10" = gettext("log(Inclusion BF)")
  )

  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1

  overallSummary$addColumnInfo(name = "terms",     title = "",                   type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"),    type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),      type = "number")
  overallSummary$addColumnInfo(name = "postProb",  title = gettext("P(M|data)"), type = "number")
  overallSummary$addColumnInfo(name = "BF",        title = titleBF,              type = "number")

  for (i in 1:nrow(fitSummary[["components"]])) {
    overallSummary$addRows(list(
      terms     = .robmaCompNames(rownames(fitSummary[["components"]])[i]),
      models    = paste0(fitSummary[["components"]][i, "models"], "/",  attr(fitSummary[["components"]], "n_models")[i]),
      priorProb = fitSummary[["components"]][i, "prior_prob"],
      postProb  = fitSummary[["components"]][i, "post_prob"],
      BF        = fitSummary[["components"]][i, 4]
    ))
  }

  errorsAndWarnings <- bibma::check_bibma(fit)
  for (i in seq_along(errorsAndWarnings)) {
    overallSummary$addFootnote(symbol = gettext("Warning:"), errorsAndWarnings[i])
  }

  mainSummary[["overallSummary"]] <- overallSummary


  ### create model averaged results tables
  # estimate table
  averagedSummary <- createJaspTable(title = gettext("Model Averaged Estimates"))
  averagedSummary$position <- 2
  averagedSummary <- .robmaTableFillCoef(averagedSummary, fitSummary[["estimates"]], options)
  mainSummary[["averagedSummary"]] <- averagedSummary


  ### create conditional models results tables
  if (options[["inferenceConditionalParameterEstimates"]]) {
    # estimate table
    conditionalSummary <- createJaspTable(title = gettext("Conditional Estimates"))
    conditionalSummary$position <- 5
    conditionalSummary <- .robmaTableFillCoef(conditionalSummary, fitSummary[["estimates_conditional"]], options)
    mainSummary[["conditionalSummary"]] <- conditionalSummary
  }

  return()
}
.bibmaModelsOvervievTable      <- function(jaspResults, options) {

  ### create overview table
  modelsSummary <- createJaspTable(title = gettext("Models Overview"))
  modelsSummary$position <- 6
  modelsSummary$dependOn(c(.bibmaDependencies, "bayesFactorType", "inferenceModelsOverview", "inferenceModelsOverviewBF", "inferenceModelsOverviewOrder", "inferenceShortenPriorName"))
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

  overtitlePrior <- gettext("Prior Distribution")

  modelsSummary$addColumnInfo(name = "number",             title = "#",                        type = "integer")
  modelsSummary$addColumnInfo(name = "priorEffect",        title = gettext("Effect Size"),      type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorHeterogeneity", title = gettext("Heterogeneity"),    type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorBaseline",      title = gettext("Baseline"),         type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorProb",          title = gettext("P(M)"),             type = "number")
  modelsSummary$addColumnInfo(name = "postProb",           title = gettext("P(M|data)"),        type = "number")
  modelsSummary$addColumnInfo(name = "marglik",            title = gettext("log(MargLik)"),     type = "number")
  modelsSummary$addColumnInfo(name = "BF",                 title = titleBF,                     type = "number")

  if (is.null(jaspResults[["model"]]))
    return()

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # some shared info
  fitSummary <- summary(
    fit,
    type       = "models",
    short_name = options[["inferenceShortenPriorName"]]
  )

  # do ordering
  if (options[["inferenceModelsOverviewOrder"]] == "marginalLikelihood")
    fitSummary[["summary"]] <- fitSummary[["summary"]][order(fitSummary[["summary"]][["marglik"]], decreasing = TRUE),]
  else if (options[["inferenceModelsOverviewOrder"]] == "posteriorProbability")
    fitSummary[["summary"]] <- fitSummary[["summary"]][order(fitSummary[["summary"]][["post_prob"]], decreasing = TRUE),]

  # compute the BF requested
  if (options[["inferenceModelsOverviewBfComparison"]] == "inclusion") {
    bf <- fitSummary[["summary"]][, 7]
  } else if (options[["inferenceModelsOverviewBfComparison"]] == "best") {
    bf <- exp(fitSummary[["summary"]][["marglik"]] - max(fitSummary[["summary"]][["marglik"]]))
  } else if (options[["inferenceModelsOverviewBfComparison"]] == "previous") {
    tempThisMargLik <- fitSummary[["summary"]][["marglik"]][-length(fitSummary[["summary"]][["marglik"]])]
    tempPrevMargLik <- fitSummary[["summary"]][["marglik"]][-1]
    bf <- c(1, exp(tempPrevMargLik - tempThisMargLik))
  }

  # fill the rows
  for (i in 1:nrow(fitSummary[["summary"]])) {
    modelsSummary$addRows(list(
      number             = fitSummary[["summary"]][i, "Model"],
      priorEffect        = fitSummary[["summary"]][i, "Effect"],
      priorHeterogeneity = fitSummary[["summary"]][i, "Heterogeneity"],
      priorBaseline      = fitSummary[["summary"]][i, "Baseline"],
      priorProb          = fitSummary[["summary"]][i, "prior_prob"],
      postProb           = fitSummary[["summary"]][i, "post_prob"],
      marglik            = fitSummary[["summary"]][i, "marglik"],
      BF                 = BayesTools::format_BF(bf[i], logBF = options[["bayesFactorType"]] == "LogBF10", BF01 = options[["bayesFactorType"]] == "BF01")
    ))
  }

  return()
}
.bibmaModelsSummaryTable       <- function(jaspResults, options) {

  if (!is.null(jaspResults[["individualModels"]])) {
    return()
  } else {
    individualModels <- createJaspContainer(title = gettext("Individual Models Summary"))
    individualModels$position <- 5
    individualModels$dependOn(c(.bibmaDependencies, "bayesFactorType", "inferenceIndividualModels", "inferenceIndividualModelsSingleModel", "inferenceIndividualModelsSingleModelNumber", "inferenceShortenPriorName", "inferenceOutputScale"))
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
    tempPriors$addColumnInfo(name = "priorMu",    title = gettext("Effect Size"),      type = "string")
    tempPriors$addColumnInfo(name = "priorTau",   title = gettext("Heterogeneity"),    type = "string")
    tempPriors$addColumnInfo(name = "priorPi",    title = gettext("Baseline"),         type = "string")
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
    output_scale  = if (options[["inputType"]] != "unstandardizedEffectSizes") .robmaGetOutputScaleOption(options),
    short_name    = options[["inferenceShortenPriorName"]]
  )

  ### create tables for individual models

  # select models to iterate over
  if (options[["inferenceIndividualModelsSingleModel"]]) {
    modelsI <- options[["inferenceIndividualModelsSingleModelNumber"]]
    if (modelsI < 1 || modelsI > length(fit[["models"]])) {
      tempModel  <- createJaspContainer(title = gettextf("Model %i", modelsI))
      tempError  <- createJaspTable(title = "")
      tempError$setError(gettextf("Model %i does not exist. Select one of the models between 1 and %i.", modelsI, length(fit[["models"]])))
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
    tempPriors$addColumnInfo(name = "priorMu",    title = gettext("Effect Size"),      type = "string")
    tempPriors$addColumnInfo(name = "priorTau",   title = gettext("Heterogeneity"),    type = "string")
    tempPriors$addColumnInfo(name = "priorPi",    title = gettext("Baseline"),         type = "string")

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

  }

  return()
}
.bibmaDiagnosticsOverviewTable <- function(jaspResults, options) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 9
    diagnostics$dependOn(.bibmaDependencies)
    jaspResults[["diagnostics"]] <- diagnostics
  } else {
    diagnostics <- jaspResults[["diagnostics"]]
  }

  if (!is.null(diagnostics[["diagosticsTable"]])) {
    return()
  }


  ### create overview table
  diagosticsTable <-  createJaspTable(title = gettext("Models Diagnostics Overview"))
  diagosticsTable$position <- 1
  diagosticsTable$dependOn(c(.bibmaDependencies, "mcmcDiagnosticsOverviewTable", "inferenceShortenPriorName"))
  diagnostics[["diagosticsTable"]] <- diagosticsTable

  overtitlePrior <- gettext("Prior Distribution")

  diagosticsTable$addColumnInfo(name = "number",             title = "#",                           type = "integer")
  diagosticsTable$addColumnInfo(name = "priorEffect",        title = gettext("Effect Size"),        type = "string",  overtitle = overtitlePrior)
  diagosticsTable$addColumnInfo(name = "priorHeterogeneity", title = gettext("Heterogeneity"),      type = "string",  overtitle = overtitlePrior)
  diagosticsTable$addColumnInfo(name = "priorBias",          title = gettext("Publication Bias"),   type = "string",  overtitle = overtitlePrior)
  diagosticsTable$addColumnInfo(name = "mcmcError",          title = gettext("max(MCMC error)"),    type = "number")
  diagosticsTable$addColumnInfo(name = "mcmcErrorSd",        title = gettext("max(MCMC error/SD)"), type = "number")
  diagosticsTable$addColumnInfo(name = "ess",                title = gettext("min(ESS)"),           type = "integer")
  diagosticsTable$addColumnInfo(name = "rHat",               title = gettext("max(R-hat)"),         type = "number")


  if (is.null(jaspResults[["model"]]))
    return()

  # extract the model
  fit   <- jaspResults[["model"]][["object"]]

  # get the diagnostics summary
  fitSummary <- summary(
    fit,
    type       = "diagnostics",
    short_name = options[["inferenceShortenPriorName"]]
  )

  for (i in 1:nrow(fitSummary[["diagnostics"]])) {
    diagosticsTable$addRows(list(
      number             = fitSummary[["diagnostics"]][i, "Model"],
      priorEffect        = fitSummary[["diagnostics"]][i, "Effect"],
      priorHeterogeneity = fitSummary[["diagnostics"]][i, "Heterogeneity"],
      priorBias          = fitSummary[["diagnostics"]][i, "Bias"],
      mcmcError          = fitSummary[["diagnostics"]][i, "max_MCMC_error"],
      mcmcErrorSd        = fitSummary[["diagnostics"]][i, "max_MCMC_SD_error"],
      ess                = fitSummary[["diagnostics"]][i, "min_ESS"],
      rHat               = fitSummary[["diagnostics"]][i, "max_R_hat"]
    ))
  }

  return()
}
.bibmaDiagnosticsPlots         <- function(jaspResults, options) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 9
    diagnostics$dependOn(.bibmaDependencies)
    jaspResults[["diagnostics"]] <- diagnostics
  } else {
    diagnostics <- jaspResults[["diagnostics"]]
  }


  # create waiting plot
  if (!(options[["mcmcDiagnosticsPlotEffect"]] || options[["mcmcDiagnosticsPlotHeterogeneity"]] || options[["mcmcDiagnosticsPlotWeights"]] || options[["mcmcDiagnosticsPlotPet"]] || options[["mcmcDiagnosticsPlotPeese"]]) && (options[["mcmcDiagnosticsPlotTypeTrace"]] || options[["mcmcDiagnosticsPlotTypeAutocorrelation"]] || options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]) || is.null(jaspResults[["model"]])) {
    tempWait  <- createJaspPlot(title = "")
    tempWait$dependOn(c("mcmcDiagnosticsPlotEffect", "mcmcDiagnosticsPlotHeterogeneity", "mcmcDiagnosticsPlotWeights", "mcmcDiagnosticsPlotPet", "mcmcDiagnosticsPlotPeese", "mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation", "mcmcDiagnosticsPlotTypePosteriorSamplesDensity"))
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
          "PET"   = "diagnosticsPET",
          "PEESE" = "diagnosticsPEESE"
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
        newPlots <- bibma::diagnostics(
          fit,
          parameter     = par,
          type          = "chains",
          plot_type     = "ggplot",
          show_models   = i,
          title         = FALSE
        )

        if (is.null(newPlots))
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
        newPlots <- bibma::diagnostics(
          fit,
          parameter     = par,
          type          = "autocorrelations",
          plot_type     = "ggplot",
          show_models   = i,
          title         = FALSE
        )

        if (is.null(newPlots))
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
        newPlots <- bibma::diagnostics(
          fit,
          parameter     = par,
          type          = "densities",
          plot_type     = "ggplot",
          show_models   = i,
          title         = FALSE
        )

        if (is.null(newPlots))
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
      tempError  <- createJaspPlot(title = "")
      tempError$dependOn(c("mcmcDiagnosticsPlotEffect", "mcmcDiagnosticsPlotHeterogeneity", "mcmcDiagnosticsPlotWeights", "diagnosticsPET", "diagnosticsPEESE", "mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation", "mcmcDiagnosticsPlotTypePosteriorSamplesDensity"))
      tempError$setError(gettextf("Model %i does not contain any of the selected parameters.", i))
      tempModel[["tempError"]] <- tempError
    }
  }

  return()
}

