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


BayesianBinomialMetaAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  # clean fitted model if it was changed
  if (!.bibmaCheckReady(options))
    .bibmaCleanModel(jaspResults)

  # load data
  if (.bibmaCheckReady(options))
    dataset <- .bibmaGetData(options, dataset)

  # get the priors
  .bibmaGetPriors(jaspResults, options)

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
  .robmaSummaryTable(jaspResults, options, type = "BiBMA")
  # models overview
  if (options[["inferenceModelsOverview"]])
    .robmaModelsOverviewTable(jaspResults, options, type = "BiBMA")
  # models summary
  if (options[["inferenceIndividualModels"]])
    .robmaModelsSummaryTable(jaspResults, options, type = "BiBMA")


  ### Plots
  # forest plot
  if (options[["plotsForestPlot"]])
    .robmaForestPlot(jaspResults, options, type = "BiBMA")

  # pooled estimates plots
  if (options[["plotsPooledEstimatesEffect"]])
    .robmaEstimatesPlot(jaspResults, options, "mu", type = "BiBMA")
  if (options[["plotsPooledEstimatesHeterogeneity"]])
    .robmaEstimatesPlot(jaspResults, options, "tau", type = "BiBMA")

  # individual models
  if (options[["plotsIndividualModelsEffect"]])
    .robmaModelsPlot(jaspResults, options, "mu", type = "BiBMA")
  if (options[["plotsIndividualModelsHeterogeneity"]])
    .robmaModelsPlot(jaspResults, options, "tau", type = "BiBMA")

  ### Diagnostics
  # overview
  if (options[["mcmcDiagnosticsOverviewTable"]])
    .robmaDiagnosticsOverviewTable(jaspResults, options, type = "BiBMA")
  # plots
  if (.bibmaCheckDiagnostics(options, any = TRUE))
    .robmaDiagnosticsPlots(jaspResults, options, type = "BiBMA")


  return()
}

.bibmaDependencies <- c(
  "successesGroup1", "successesGroup2", "observationsGroup1", "observationsGroup2", "studyLabel",
  "modelsEffect", "modelsEffectNull", "modelsHeterogeneity", "modelsHeterogeneityNull", "modelsBaseline", "modelsBaselineNull",
  "advancedMcmcAdaptation", "advancedMcmcBurnin", "advancedMcmcSamples", "advancedMcmcChains", "advancedMcmcThin",
  "autofit", "advancedAutofitRHat", "advancedAutofitRHatTarget", "advancedAutofitEss", "advancedAutofitEssTarget", "advancedAutofitMcmcError", "advancedAutofitMcmcErrorTarget", "advancedAutofitMcmcErrorSd", "advancedAutofitMcmcErrorSdTarget", "advancedAutofitMaximumFittingTime", "advancedAutofitMaximumFittingTimeTarget", "advancedAutofitMaximumFittingTimeTargetUnit", "advancedAutofitExtendSamples",
  "advancedRemoveFailedModels", "advancedRemoveFailedModelsRHat",  "advancedRemoveFailedModelsRHatTarget", "advancedRemoveFailedModelsEss", "advancedRemoveFailedModelsEssTarget", "advancedRemoveFailedModelsMcmcError", "advancedRemoveFailedModelsMcmcErrorTarget", "advancedRemoveFailedModelsMcmcErrorSd", "advancedRemoveFailedModelsMcmcErrorSdTarget",
  "advancedRebalanceComponentProbabilityOnModelFailure", "seed", "setSeed"
)

# helper functions
.bibmaCheckReady          <- function(options) {
  return(options[["successesGroup1"]] != "" && options[["successesGroup2"]] != "" && options[["observationsGroup1"]] != "" && options[["observationsGroup2"]] != "")
}
.bibmaCheckDiagnostics    <- function(options, any) {
  parametersAny <-
    options[["mcmcDiagnosticsPlotEffect"]]          ||
    options[["mcmcDiagnosticsPlotHeterogeneity"]]
  typeAny <- options[["mcmcDiagnosticsPlotTypeTrace"]]  ||
    options[["mcmcDiagnosticsPlotTypeAutocorrelation"]] ||
    options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]

  if (any)
    return(parametersAny || typeAny)
  else
    return(parametersAny && typeAny)
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

  for (parameter in c("effect", "heterogeneity", "baseline")) {
    for (type in c("", "Null")) {

      optionName <- switch(
        paste0(parameter, type),
        "effect"            = "modelsEffect",
        "effectNull"        = "modelsEffectNull",
        "heterogeneity"     = "modelsHeterogeneity",
        "heterogeneityNull" = "modelsHeterogeneityNull",
        "baseline"          = "modelsBaseline",
        "baselineNull"      = "modelsBaselineNull"
      )

      tmp <- NULL
      for (j in seq_along(options[[optionName]])) {
        tmpPrior <- try(.robmaExtractPriorsFromOptions(options[[optionName]][[j]], parameter = parameter))
        if (jaspBase::isTryError(tmpPrior))
          .quitAnalysis(tmpPrior)
        else
          tmp <- c(tmp, list(tmpPrior))
      }
      object[[paste0(parameter, type)]] <- tmp

    }
  }

  priors[["object"]] <- object

  return()
}
# main functions
.bibmaPriorsPlots              <- function(jaspResults, options) {

  # create / access the container
  if (!is.null(jaspResults[["priorPlots"]])) {
    priorPlots <- jaspResults[["priorPlots"]]
  } else {
    priorPlots <- createJaspContainer(title = gettext("Prior Plots"))
    priorPlots$dependOn("priorDistributionPlot")
    priorPlots$position <- 2
    jaspResults[["priorPlots"]] <- priorPlots
  }

  # extract the priors
  if (is.null(jaspResults[["model"]]))
    priors <- jaspResults[["priors"]][["object"]]
  else
    priors <- jaspResults[["model"]][["object"]][["priors"]]

  # create container for each of the parameters
  for (parameter in c("effect", "heterogeneity")) {

    if (!is.null(priorPlots[[parameter]]))
      parameterContainer <- priorPlots[[parameter]]
    else {
      parameterContainer <- createJaspContainer(title = switch(
        parameter,
        "effect"          = gettext("Effect"),
        "heterogeneity"   = gettext("Heterogeneity")
      ))
      parameterContainer$position <- switch(
        parameter,
        "effect"          = 1,
        "heterogeneity"   = 2
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
          "heterogeneity" = c("modelsHeterogeneity", "modelsHeterogeneityNull")
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

        tempPlot <- createJaspPlot(width = 400,  height = 300)

        typeContainer[[paste0(parameter, type, i)]] <- tempPlot

        p <- plot(tempPriors[[i]], plot_type = "ggplot", par_name = switch(
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
    (length(priors[["effect"]])        == 0 && length(priors[["effectNull"]])        == 0) ||
    (length(priors[["heterogeneity"]]) == 0 && length(priors[["heterogeneityNull"]]) == 0) ||
    (length(priors[["baseline"]])      == 0 && length(priors[["baselineNull"]])      == 0)
  ) {
    priorsError <- createJaspTable()
    priorsError$setError(gettext("Please specify a prior distribution for each parameter in the Models specification section (either null or alternative)."))
    modelPreview[["priorsError"]] <- priorsError
    return()
  }

  # create the setup table
  fitSummary   <- RoBMA::check_setup.BiBMA(
    priors_effect             = priors[["effect"]],
    priors_heterogeneity      = priors[["heterogeneity"]],
    priors_baseline           = priors[["baseline"]],
    priors_effect_null        = priors[["effectNull"]],
    priors_heterogeneity_null = priors[["heterogeneityNull"]],
    priors_baseline_null      = priors[["baselineNull"]],
    models                    = TRUE,
    silent                    = TRUE
  )


  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1

  overallSummary$addColumnInfo(name = "terms",     title = "",                type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"), type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),   type = "number")

  for (i in 1:nrow(fitSummary[["components"]])) {
    tempRow <- list(
      terms     = .robmaCompNames(rownames(fitSummary[["components"]])[i]),
      models    = paste0(fitSummary[["components"]][[i, "models"]], "/", attr(fitSummary[["components"]], "n_models")),
      priorProb = fitSummary[["components"]][[i, "prior_prob"]]
    )

    overallSummary$addRows(tempRow)
  }
  overallSummary$addFootnote(gettext("The analysis will estimate multiple meta-analytic models using MCMC and might require a prolonged time to complete."), symbol = "\u26A0")

  modelPreview[["overallSummary"]] <- overallSummary


  ### create models overview table
  modelsSummary <- createJaspTable(title = gettext("Model Specification Preview"))
  modelsSummary$position <- 2

  overtitlePrior <- gettext("Prior Distribution")

  modelsSummary$addColumnInfo(name = "number",              title = "#",                         type = "integer")
  modelsSummary <- .robmaAddPriorColumn(modelsSummary, fitSummary[["summary"]])
  modelsSummary$addColumnInfo(name = "priorProb",           title = gettext("P(M)"),             type = "number")

  for (i in 1:nrow(fitSummary[["summary"]])) {
    tempRow <- list(
      number             = fitSummary[["summary"]][i, "Model"],
      priorProb          = fitSummary[["summary"]][i, "prior_prob"]
    )
    tempRow <- .robmaFillPriorColumn(tempRow, fitSummary[["summary"]][i,])
    modelsSummary$addRows(tempRow)
  }
  modelsSummary$addFootnote(gettext("The analysis will estimate multiple meta-analytic models using MCMC and might require a prolonged time to complete."), symbol = "\u26A0")

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
      priors_effect             = priors[["effect"]],
      priors_heterogeneity      = priors[["heterogeneity"]],
      priors_baseline           = priors[["baseline"]],
      priors_effect_null        = priors[["effectNull"]],
      priors_heterogeneity_null = priors[["heterogeneityNull"]],
      priors_baseline_null      = priors[["baselineNull"]],
      # sampling settings
      chains  = options[["advancedMcmcChains"]],
      adapt   = options[["advancedMcmcAdaptation"]],
      burnin  = options[["advancedMcmcBurnin"]],
      sample  = options[["advancedMcmcSamples"]],
      thin    = options[["advancedMcmcThin"]],
      # additional settings
      autofit         = options[["autofit"]],
      autofit_control = RoBMA::set_autofit_control(
        max_Rhat      = if (options[["advancedAutofitRHat"]])        options[["advancedAutofitRHatTarget"]],
        min_ESS       = if (options[["advancedAutofitEss"]])         options[["advancedAutofitEssTarget"]],
        max_error     = if (options[["advancedAutofitMcmcError"]])   options[["advancedAutofitMcmcErrorTarget"]],
        max_SD_error  = if (options[["advancedAutofitMcmcErrorSd"]]) options[["advancedAutofitMcmcErrorSdTarget"]],
        max_time      = if (options[["advancedAutofitMaximumFittingTime"]]) list(
          time = options[["advancedAutofitMaximumFittingTimeTarget"]],
          unit = options[["advancedAutofitMaximumFittingTimeTargetUnit"]]),
          sample_extend = options[["advancedAutofitExtendSamples"]]),
      convergence_checks = RoBMA::set_convergence_checks(
        max_Rhat            = if (options[["advancedRemoveFailedModelsRHat"]])        options[["advancedRemoveFailedModelsRHatTarget"]],
        min_ESS             = if (options[["advancedRemoveFailedModelsEss"]])         options[["advancedRemoveFailedModelsEssTarget"]],
        max_error           = if (options[["advancedRemoveFailedModelsMcmcError"]])   options[["advancedRemoveFailedModelsMcmcErrorTarget"]],
        max_SD_error        = if (options[["advancedRemoveFailedModelsMcmcErrorSd"]]) options[["advancedRemoveFailedModelsMcmcErrorSdTarget"]],
        remove_failed       = options[["advancedRemoveFailedModels"]],
        balance_probability = options[["advancedRebalanceComponentProbabilityOnModelFailure"]]
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
  .robmaErrorHandling(fit, options)


  # update the fit and reset notifier
  model[["object"]] <- fit
  .bibmaModelNotifier(jaspResults)

  return()
}
