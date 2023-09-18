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


RobustBayesianMetaAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  # clean fitted model if it was changed
  if (!.robmaCheckReady(options))
    .robmaCleanModel(jaspResults)

  # load data
  if (.robmaCheckReady(options))
    dataset <- .robmaGetData(options, dataset)

  # get the priors
  .robmaGetPriors(jaspResults, options)

  # show the model preview
  if (is.null(jaspResults[["model"]]))
    .robmaModelPreviewTable(jaspResults, options)

  # fit model model
  if (is.null(jaspResults[["modelNotifier"]]) && .robmaCheckReady(options))
    .robmaFitModel(jaspResults, dataset, options)

  ### Priors plot
  if (options[["priorDistributionPlot"]])
    .robmaPriorsPlots(jaspResults, options)


  ### Inference
  # default summary
  .robmaSummaryTable(jaspResults, options)
  # models overview
  if (options[["inferenceModelsOverview"]])
    .robmaModelsOvervievTable(jaspResults, options)
  # models summary
  if (options[["inferenceIndividualModels"]])
    .robmaModelsSummaryTable(jaspResults, options)


  ### Plots
  # forest plot
  if (options[["plotsForestPlot"]])
    .robmaForestPlot(jaspResults, options)

  # pooled estimates plots
  if (options[["plotsPooledEstimatesEffect"]])
    .robmaEstimatesPlot(jaspResults, options, "mu")
  if (options[["plotsPooledEstimatesHeterogeneity"]])
    .robmaEstimatesPlot(jaspResults, options, "tau")
  if (options[["plotsPooledEstimatesWeightFunction"]])
    .robmaEstimatesPlot(jaspResults, options, "weightFunction")
  if (options[["plotsPooledEstimatesPetPeese"]])
    .robmaEstimatesPlot(jaspResults, options, "petPeese")

  # individual models
  if (options[["plotsIndividualModelsEffect"]])
    .robmaModelsPlot(jaspResults, options, "mu")
  if (options[["plotsIndividualModelsHeterogeneity"]])
    .robmaModelsPlot(jaspResults, options, "tau")

  ### Diagnostics
  # overview
  if (options[["mcmcDiagnosticsOverviewTable"]])
    .robmaDiagnosticsOverviewTable(jaspResults, options)
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
    .robmaDiagnosticsPlots(jaspResults, options)

  ### Save the model
  if (options[["advancedSaveFittedModel"]] != "" && is.null(jaspResults[["modelSaved"]]))
    .robmaSaveModel(jaspResults, options)


  return()
}

.robmaDependencies <- c(
  "inputType", "pathToFittedModel",
  "effectSize", "effectSizeSe", "effectSizeCi", "sampleSize", "studyLabel",
  "modelExpectedDirectionOfEffectSizes", "modelEnsembleType", "priorScale", "advancedEstimationScale",
  "modelsEffect", "modelsEffectNull", "modelsHeterogeneity", "modelsHeterogeneityNull", "modelsSelectionModels", "modelsSelectionModelsNull", "modelsPet", "modelsPetNull", "modelsPeese", "modelsPeeseNull",
  "advancedMcmcAdaptation", "advancedMcmcBurnin", "advancedMcmcSamples", "advancedMcmcChains", "advancedMcmcThin",
  "autofit", "advancedAutofitRHat", "advancedAutofitRHatTarget", "advancedAutofitEss", "advancedAutofitEssTarget", "advancedAutofitMcmcError", "advancedAutofitMcmcErrorTarget", "advancedAutofitMcmcErrorSd", "advancedAutofitMcmcErrorSdTarget", "advancedAutofitMaximumFittingTime", "advancedAutofitMaximumFittingTimeTarget", "advancedAutofitMaximumFittingTimeTargetUnit", "advancedAutofitExtendSamples",
  "advancedAutofitRemoveFailedModels", "advancedAutofitRebalanceComponentProbabilityOnModelFailure", "seed", "setSeed"
)
# priors related functions
.robmaExtractPriorsFromOptions <- function(optionsPrior, parameter) {

  optionsPrior   <- .robmaEvalOptionsToPriors(optionsPrior)

  if (optionsPrior[["type"]] == "none")
    return(RoBMA::prior_none(prior_weights = optionsPrior[["priorWeight"]]))
  else
    return(do.call(
      what = switch(
        parameter,
        "normal"                 = RoBMA::prior,
        "modelsSelectionModels"  = RoBMA::prior_weightfunction,
        "modelsPet"              = RoBMA::prior_PET,
        "modelsPeese"            = RoBMA::prior_PEESE
      ),
      args = .robmaMapOptionsToPriors(optionsPrior)
    ))
}
.robmaCleanOptionsToPriors     <- function(x) {

  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")

  x <- strsplit(x, ",", fixed = TRUE)[[1]]

  x <- trimws(x, which = "both")
  x <- x[x != ""]

  if (anyNA(as.numeric(x)))
    .quitAnalysis(gettext("The priors for publication bias were set incorrectly."))
  return(as.numeric(x))
}
.robmaEvalOptionsToPriors      <- function(x) {

  if (x[["type"]] %in% c("twoSided", "oneSided")) {
    x[["priorWeight"]] <- eval(parse(text = x[["priorWeight"]]))
    x[["alpha"]]    <- .robmaCleanOptionsToPriors(x[["alpha"]])
    x[["pValues"]]     <- .robmaCleanOptionsToPriors(x[["pValues"]])
  } else if (x[["type"]] %in% c("twoSidedFixed", "oneSidedFixed")) {
    x[["priorWeight"]] <- eval(parse(text = x[["priorWeight"]]))
    x[["omega"]]    <- .robmaCleanOptionsToPriors(x[["omega"]])
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
      if (!is.null(x[[n]]))
        x[[n]] <- eval(parse(text = x[[n]]))
    }
  }

  return(x)
}
.robmaMapOptionsToPriors       <- function(optionsPrior) {

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
    "t"         = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["sigma"]], "df" = optionsPrior[["nu"]]),
    "cauchy"    = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["theta"]]),
    "gammaAB"   = list("shape" = optionsPrior[["alpha"]], "rate" = optionsPrior[["beta"]]),
    "gammaK0"   = list("shape" = optionsPrior[["k"]], "rate" = 1/optionsPrior[["theta"]]),
    "invgamma"  = list("shape" = optionsPrior[["alpha"]], "scale" = optionsPrior[["beta"]]),
    "lognormal" = list("meanlog" = optionsPrior[["mu"]], "sdlog" = optionsPrior[["sigma"]]),
    "beta"      = list("alpha" = optionsPrior[["alpha"]], "beta" = optionsPrior[["beta"]]),
    "uniform"   = list("a" = optionsPrior[["a"]], "b" = optionsPrior[["b"]]),
    "spike"     = list("location" = optionsPrior[["x0"]]),
    "oneSided"  = list("steps" = optionsPrior[["pValues"]], alpha = optionsPrior[["alpha"]]),
    "twoSided"  = list("steps" = optionsPrior[["pValues"]], alpha = optionsPrior[["alpha"]]),
    "oneSidedFixed" = list("steps" = optionsPrior[["pValues"]], omega = optionsPrior[["omega"]]),
    "twoSidedFixed" = list("steps" = optionsPrior[["pValues"]], omega = optionsPrior[["omega"]])
  )

  if(!arguments[["distribution"]] %in% c("oneSided", "twoSided", "oneSidedFixed", "twoSidedFixed", "spike", "uniform")) {
    arguments[["truncation"]] <- list(
      lower   = optionsPrior[["truncationLower"]],
      upper   = optionsPrior[["truncationUpper"]]
    )
  }

  arguments[["prior_weights"]] = optionsPrior[["priorWeight"]]

  return(arguments)
}
# table filling functions
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
  if (coefficient == "mu")
    return(gettextf("Effect size (%s)", if (options[["inputType"]] != "unstandardizedEffectSizes") .robmaCoefLetters(options[["inferenceOutputScale"]]) else "\u03BC"))
  else if (coefficient == "tau")
    return(gettextf("Heterogeneity (%s)","\u03C4"))
}
.robmaCoefLetters         <- function(effectSize) {
  return(switch(
    effectSize,
    "correlation" = "\u03C1",
    "cohensD"     = "\u03B4",
    "fishersZ"    = "\u007a",
    "logOr"       = "log(OR)"
  ))
}
# helper functions
.robmaCheckReady          <- function(options) {

  if (options[["inputType"]] == "fittedModel") {
    return(options[["pathToFittedModel"]] != "")
  } else if (options[["inputType"]] == "unstandardizedEffectSizes") {
    readyArg1 <- options[["effectSize"]] != ""
    readyArg2 <- any(options[["effectSizeSe"]] != "", sum(unlist(options[["effectSizeCi"]]) != "") == 2)
    return(readyArg1 && readyArg2)
  } else if (options[["inputType"]] == "logOr") {
    readyArg1 <- options[["effectSize"]] != ""
    readyArg2 <- sum(unlist(options[["effectSizeCi"]]) != "") == 2
    return(readyArg1 && readyArg2)
  } else {
    readyArg1 <- options[["effectSize"]] != ""
    readyArg2 <- any(c(options[["effectSizeSe"]], options[["sampleSize"]]) != "", sum(unlist(options[["effectSizeCi"]]) != "") == 2)
    return(readyArg1 && readyArg2)
  }

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
.robmaCleanModel          <- function(jaspResults) {

  if (!is.null(jaspResults[["model"]])) {
    jaspResults[["model"]] <- NULL
  }

  return()
}
.robmaGetData             <- function(options, dataset) {

  if (options[["inputType"]] == "fittedModel") {
    return(NULL)
  } else {
    if (!is.null(dataset)) {
      return(dataset)
    } else {
      varNames <- c(options[["effectSize"]], options[["effectSizeSe"]], options[["sampleSize"]], unlist(options[["effectSizeCi"]]))
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

  }

  return(dataset)
}
.robmaGetPriors           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["priors"]])) {
    return()
  } else {
    priors <- createJaspState()
    priors$dependOn(.robmaDependencies)
    jaspResults[["priors"]] <- priors
  }

  object <- list()

  if (options[["modelEnsembleType"]] == "custom") {

    # effect and heterogeneity are simply
    for(type in c("", "Null")) {
      priorElements <- paste0(c("modelsEffect", "modelsHeterogeneity"), type)
      for (i in seq_along(priorElements)) {
        tmp <- NULL
        for (j in seq_along(options[[priorElements[i]]])) {
          tmpPrior <- try(.robmaExtractPriorsFromOptions(options[[priorElements[i]]][[j]], parameter = "normal"))
          if (jaspBase::isTryError(tmpPrior))
            .quitAnalysis(tmpPrior)
          else
            tmp <- c(tmp, list(tmpPrior))
        }
        object[[priorElements[i]]] <- tmp
      }
    }

    # publication bias can be composite
    for(type in c("", "Null")) {
      tmp <- NULL
      for (parameter in c("modelsSelectionModels", "modelsPet", "modelsPeese")) {
        priorElements <- paste0(parameter, type)
        for (i in seq_along(priorElements)) {
          for (j in seq_along(options[[priorElements[i]]])) {
            tmpPrior <- try(.robmaExtractPriorsFromOptions(options[[priorElements[i]]][[j]], parameter = parameter))
            if (jaspBase::isTryError(tmpPrior))
              .quitAnalysis(tmpPrior)
            else
              tmp <- c(tmp, list(tmpPrior))
          }
        }
      }
      object[[paste0("modelsBias", type)]] <- tmp
    }
  }


  priors[["object"]] <- object

  return()
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
    priors <- RoBMA::check_setup(model_type = options[["modelEnsembleType"]], silent = TRUE)$priors
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
.robmaModelPreviewTable        <- function(jaspResults, options) {

  # create / access the container
  if (!is.null(jaspResults[["modelPreview"]])) {
    return()
  } else {
    modelPreview <- createJaspContainer(title = gettext("Model Preview"))
    modelPreview$dependOn(.robmaDependencies)
    modelPreview$position <- 1
    jaspResults[["modelPreview"]] <- modelPreview
  }


  # extract the priors
  priors  <- jaspResults[["priors"]][["object"]]


  if (options[["modelEnsembleType"]] == "custom") {

    # set error if no priors are specified
    if (
      (length(priors[["modelsEffect"]])        == 0 && length(priors[["modelsEffectNull"]])        == 0) ||
      (length(priors[["modelsHeterogeneity"]]) == 0 && length(priors[["modelsHeterogeneityNull"]]) == 0) ||
      (length(priors[["modelsBias"]])          == 0 && length(priors[["modelsBiasNull"]])          == 0)
    ) {
      priorsError <- createJaspTable()
      priorsError$setError(gettext("Please specify a prior distribution for each parameter in the Models specification section (either null or alternative)."))
      modelPreview[["priorsError"]] <- priorsError
      return()
    }

    # create the setup table
    fitSummary   <- RoBMA::check_setup(
      priors_effect             = priors[["modelsEffect"]],
      priors_heterogeneity      = priors[["modelsHeterogeneity"]],
      priors_bias               = priors[["modelsBias"]],
      priors_effect_null        = priors[["modelsEffectNull"]],
      priors_heterogeneity_null = priors[["modelsHeterogeneityNull"]],
      priors_bias_null          = priors[["modelsBiasNull"]],
      models                    = TRUE,
      silent                    = TRUE
    )

  } else {

    fitSummary   <- RoBMA::check_setup(
      model_type = .robmaGetModelTypeOption(options),
      models     = TRUE,
      silent     = TRUE
    )
  }



  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1

  overallSummary$addColumnInfo(name = "terms",     title = "",                type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"), type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),   type = "number")

  if (options[["inputType"]] != "fittedModel") {
    for (i in 1:nrow(fitSummary[["components"]])) {
      tempRow <- list(
        terms     = if (i == 1) gettext("Effect") else if (i == 2) gettext("Heterogeneity") else if (i == 3) gettext("Publication bias"),
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
  modelsSummary$addColumnInfo(name = "priorBias",           title = gettext("Publication Bias"), type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorProb",           title = gettext("P(M)"),             type = "number")

  if (options[["inputType"]] != "fittedModel") {
    for (i in 1:nrow(fitSummary[["summary"]])) {
      tempRow <- list(
        number             = fitSummary[["summary"]][i, "Model"],
        priorEffect        = fitSummary[["summary"]][i, "Effect"],
        priorHeterogeneity = fitSummary[["summary"]][i, "Heterogeneity"],
        priorBias          = fitSummary[["summary"]][i, "Bias"],
        priorProb          = fitSummary[["summary"]][i, "prior_prob"]
      )

      modelsSummary$addRows(tempRow)
    }
    modelsSummary$addFootnote(gettext("The analysis will estimate multiple meta-analytic models using MCMC and might require a prolonged time to complete."), symbol = "\u26A0")
  }

  modelPreview[["modelsSummary"]] <- modelsSummary


  return()
}
.robmaFitModel                 <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["model"]])) {
    model <- createJaspState()
    model$dependOn(.robmaDependencies[.robmaDependencies != "studyLabel"])
    jaspResults[["model"]] <- model
    fit                    <- NULL
  } else {
    model <- jaspResults[["model"]]
    fit   <- model[["object"]]

  }

  if (options[["inputType"]] == "fittedModel") {

    fit <- readRDS(file = options[["pathToFittedModel"]])

    if (!RoBMA::is.RoBMA(fit))
      .quitAnalysis(gettext("The loaded object is not a RoBMA model."))

  } else if (is.null(fit)) {

    priors <- jaspResults[["priors"]]$object
    fit    <- try(RoBMA::RoBMA(
      # data
      d     = if (options[["inputType"]] == "cohensD" && options[["effectSize"]] != "")                     dataset[, options[["effectSize"]]],
      r     = if (options[["inputType"]] == "correlation" && options[["effectSize"]] != "")                 dataset[, options[["effectSize"]]],
      logOR = if (options[["inputType"]] == "logOr")                                                        dataset[, options[["effectSize"]]],
      y     = if (options[["inputType"]] == "unstandardizedEffectSizes" && options[["effectSize"]] != "")   dataset[, options[["effectSize"]]],
      se    = if (options[["effectSizeSe"]] != "")                                                          dataset[, options[["effectSizeSe"]]],
      lCI   = if (sum(unlist(options[["effectSizeCi"]]) != "") == 2)                                        dataset[, options[["effectSizeCi"]][[1]][1]],
      uCI   = if (sum(unlist(options[["effectSizeCi"]]) != "") == 2)                                        dataset[, options[["effectSizeCi"]][[1]][2]],
      n     = if (options[["inputType"]] %in% c("cohensD", "correlation") && options[["sampleSize"]] != "") dataset[, options[["sampleSize"]]],
      study_names = if (options[["studyLabel"]] != "")                                                      dataset[, options[["studyLabel"]]],
      # model settings
      transformation   = .robmaGetFittingScaleOption(options),
      prior_scale      = .robmaGetPriorScaleOption(options),
      effect_direction = options[["modelExpectedDirectionOfEffectSizes"]],
      # priors
      model_type                = if (options[["modelEnsembleType"]] != "custom") .robmaGetModelTypeOption(options),
      priors_effect             = if (options[["modelEnsembleType"]] == "custom") priors[["modelsEffect"]],
      priors_heterogeneity      = if (options[["modelEnsembleType"]] == "custom") priors[["modelsHeterogeneity"]],
      priors_bias               = if (options[["modelEnsembleType"]] == "custom") priors[["modelsBias"]],
      priors_effect_null        = if (options[["modelEnsembleType"]] == "custom") priors[["modelsEffectNull"]],
      priors_heterogeneity_null = if (options[["modelEnsembleType"]] == "custom") priors[["modelsHeterogeneityNull"]],
      priors_bias_null          = if (options[["modelEnsembleType"]] == "custom") priors[["modelsBiasNull"]],
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
        max_time      = if (options[["advancedAutofitMaximumFittingTime"]])        list(time = options[["advancedAutofitMaximumFittingTimeTarget"]] , unit = options[["advancedAutofitMaximumFittingTimeTargetUnit"]]),
        sample_extend = options[["advancedAutofitExtendSamples"]]),
      convergence_checks = RoBMA::set_convergence_checks(
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
  .robmaModelNotifier(jaspResults)

  return()
}
.robmaSummaryTable             <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainSummary"]])) {
    return()
  } else {
    # create container
    mainSummary <- createJaspContainer(title = gettext("Summary"))
    mainSummary$position <- 3
    summaryDependencies  <- c(.robmaDependencies, "bayesFactorType", "inferenceCiWidth", "inferenceConditionalParameterEstimates", "inferenceOutputScale")
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
    output_scale = if (options[["inputType"]] != "unstandardizedEffectSizes") .robmaGetOutputScaleOption(options)
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
      terms     = if (i == 1) gettext("Effect") else if (i == 2) gettext("Heterogeneity") else if (i == 3) gettext("Publication bias"),
      models    = paste0(fitSummary[["components"]][i, "models"], "/",  attr(fitSummary[["components"]], "n_models")[i]),
      priorProb = fitSummary[["components"]][i, "prior_prob"],
      postProb  = fitSummary[["components"]][i, "post_prob"],
      BF        = fitSummary[["components"]][i, 4]
    ))
  }

  errorsAndWarnings <- RoBMA::check_RoBMA(fit)
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

  # weights table
  if (any(grepl("omega", rownames(fitSummary[["estimates"]])))) {
    averagedWeights <- createJaspTable(title = gettextf("Model Averaged Weights (%s)", "\u03C9"))
    averagedWeights$position <- 3
    averagedWeights <- .robmaTableFillWeights(averagedWeights, fitSummary[["estimates"]], options)
    mainSummary[["averagedWeights"]] <- averagedWeights
  }

  # PET-PEESE table
  if (any(grepl("PET", rownames(fitSummary[["estimates"]])) | grepl("PEESE", rownames(fitSummary[["estimates"]])))) {
    averagedPetPeese <- createJaspTable(title = gettextf("Model Averaged PET-PEESE Estimates"))
    averagedPetPeese$position <- 4
    averagedPetPeese <- .robmaTableFillPetPeese(averagedPetPeese, fitSummary[["estimates"]], options)
    mainSummary[["averagedPetPeese"]] <- averagedPetPeese
  }



  ### create conditional models results tables
  if (options[["inferenceConditionalParameterEstimates"]]) {
    # estimate table
    conditionalSummary <- createJaspTable(title = gettext("Conditional Estimates"))
    conditionalSummary$position <- 5
    conditionalSummary <- .robmaTableFillCoef(conditionalSummary, fitSummary[["estimates_conditional"]], options)
    mainSummary[["conditionalSummary"]] <- conditionalSummary

    # weights table
    if (any(grepl("omega", rownames(fitSummary[["estimates_conditional"]])))) {
      conditionalWeights <- createJaspTable(title = gettextf("Conditional Weights (%s)", "\u03C9"))
      conditionalWeights$position <- 6
      conditionalWeights <- .robmaTableFillWeights(conditionalWeights, fitSummary[["estimates_conditional"]], options)
      mainSummary[["conditionalWeights"]] <- conditionalWeights
    }

    # PET-PEESE table
    if (any(grepl("PET", rownames(fitSummary[["estimates_conditional"]])) | grepl("PEESE", rownames(fitSummary[["estimates_conditional"]])))) {
      conditionalPetPeese <- createJaspTable(title = gettextf("Conditional PET-PEESE Estimates"))
      conditionalPetPeese$position <- 7
      conditionalPetPeese <- .robmaTableFillPetPeese(conditionalPetPeese, fitSummary[["estimates_conditional"]], options)
      mainSummary[["conditionalPetPeese"]] <- conditionalPetPeese
    }

  }

  return()
}
.robmaModelsOvervievTable      <- function(jaspResults, options) {

  ### create overview table
  modelsSummary <- createJaspTable(title = gettext("Models Overview"))
  modelsSummary$position <- 8
  modelsSummary$dependOn(c(.robmaDependencies, "bayesFactorType", "inferenceModelsOverview", "inferenceModelsOverviewBF", "inferenceModelsOverviewOrder", "inferenceShortenPriorName"))
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
  modelsSummary$addColumnInfo(name = "priorBias",          title = gettext("Publication Bias"), type = "string",  overtitle = overtitlePrior)
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
      priorBias          = fitSummary[["summary"]][i, "Bias"],
      priorProb          = fitSummary[["summary"]][i, "prior_prob"],
      postProb           = fitSummary[["summary"]][i, "post_prob"],
      marglik            = fitSummary[["summary"]][i, "marglik"],
      BF                 = BayesTools::format_BF(bf[i], logBF = options[["bayesFactorType"]] == "LogBF10", BF01 = options[["bayesFactorType"]] == "BF01")
    ))
  }

  return()
}
.robmaModelsSummaryTable       <- function(jaspResults, options) {

  if (!is.null(jaspResults[["individualModels"]])) {
    return()
  } else {
    individualModels <- createJaspContainer(title = gettext("Individual Models Summary"))
    individualModels$position <- 9
    individualModels$dependOn(c(.robmaDependencies, "bayesFactorType", "inferenceIndividualModels", "inferenceIndividualModelsSingleModel", "inferenceIndividualModelsSingleModelNumber", "inferenceShortenPriorName", "inferenceOutputScale"))
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
    tempPriors$addColumnInfo(name = "priorBias",  title = gettext("Publication Bias"), type = "string")
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
    if (!is.null(fitSummary[["models"]][[i]][["estimates"]])) {

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
.robmaForestPlot               <- function(jaspResults, options) {

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
  forestPlot$dependOn(c(.robmaDependencies, "plotsForestPlot", "plotsForestPlotOrder", "plotsForestPlotType", "inferenceOutputScale"))
  jaspResults[["forestPlot"]] <- forestPlot


  # plot
  p <- try(RoBMA::forest(
    fit,
    conditional  = options[["plotsForestPlotType"]] == "conditional",
    order        = options[["plotsForestPlotOrder"]],
    output_scale = if (options[["inputType"]] != "unstandardizedEffectSizes") .robmaGetOutputScaleOption(options),
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
.robmaEstimatesPlot            <- function(jaspResults, options, parameter) {

  # create / access the container
  if (is.null(jaspResults[["estimatesPlots"]])) {
    estimatesPlots <- createJaspContainer(title = gettext("Posterior Distribution Plots"))
    estimatesPlots$position <- 11
    estimatesPlots$dependOn(c(.robmaDependencies, "plotsPooledEstimatesType", "plotsPooledEstimatesPriorDistribution", "inferenceOutputScale"))
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
    output_scale = if (options[["inputType"]] != "unstandardizedEffectSizes") .robmaGetOutputScaleOption(options),
    rescale_x    = options[["plotsPooledEstimatesWeightFunctionRescaleXAxis"]],
    conditional  = options[["plotsPooledEstimatesType"]] == "conditional",
    plot_type    = "ggplot",
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
.robmaModelsPlot               <- function(jaspResults, options, parameter) {

  # create / access the container
  if (is.null(jaspResults[["modelsPlots"]])) {
    modelsPlots <- createJaspContainer(title = gettext("Posterior Model Estimates Plots"))
    modelsPlots$position <- 12
    modelsPlots$dependOn(c(.robmaDependencies, "plotsIndividualModelsType", "plotsIndividualModelsOrder", "plotsIndividualModelsOrderBy", "plotsIndividualModelsShowBayesianUpdating", "plotsIndividualModelsShowPosteriorEstimates", "inferenceOutputScale"))
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
    order_by       = .robmaGerIndividualModelOrderOption(options),
    conditional    = options[["plotsIndividualModelsType"]] == "conditional",
    output_scale   = if (options[["inputType"]] != "unstandardizedEffectSizes") .robmaGetOutputScaleOption(options),
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
.robmaDiagnosticsOverviewTable <- function(jaspResults, options) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 13
    diagnostics$dependOn(.robmaDependencies)
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
  diagosticsTable$dependOn(c(.robmaDependencies, "mcmcDiagnosticsOverviewTable", "inferenceShortenPriorName"))
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
.robmaDiagnosticsPlots         <- function(jaspResults, options) {

  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 14
    diagnostics$dependOn(.robmaDependencies)
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
        newPlots <- RoBMA::diagnostics(
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
        newPlots <- RoBMA::diagnostics(
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
        newPlots <- RoBMA::diagnostics(
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
.robmaGetModelTypeOption       <- function(options) {
  return(switch(
    options[["modelEnsembleType"]],
    "PSMA"     = "PSMA",
    "PP"       = "PP",
    "original" = "2w",
    "custom"   = "custom"
  ))
}
.robmaGetPriorScaleOption     <- function(options) {
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
.robmaGetFittingScaleOption   <- function(options) {
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
.robmaGetOutputScaleOption   <- function(options) {
  return(switch(
    options[["inferenceOutputScale"]],
    "cohensD"     = "cohens_d",
    "fishersZ"    = "fishers_z",
    "logOr"       = "logOR",
    "correlation" = "r"
  ))
}
.robmaGerIndividualModelOrderOption <- function(options){
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

  saveRDS(jaspResults[["model"]][["object"]], file = options[["advancedSaveFittedModel"]])

  modelSaved[["object"]] <- TRUE

}
