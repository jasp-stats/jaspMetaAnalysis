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
  if(!.robmaReady(options))
    .robmaCleanModel(jaspResults)
 
  # load data
  if (.robmaReady(options))
    dataset <- .robmaGetData(options, dataset)
  
  # get the priors
  .robmaGetPriors(jaspResults, options)
  
  # show the model preview
  if (is.null(jaspResults[["model"]]))
    .robmaModelPreviewTable(jaspResults, options)

  # fit model model
  if (is.null(jaspResults[["modelNotifier"]]) && .robmaReady(options))
    .robmaFitModel(jaspResults, dataset, options)
  
  ### Priors plot
  if (options[["priorsPlot"]])
    .robmaPriorsPlots(jaspResults, options)
  
  ### Inference, Plots, and Diagnostics are accessible only if a model is fitted
  if (!is.null(jaspResults[["model"]])) {
    ### Inference
    # defaul summary
    .robmaSummaryTable(jaspResults, options)
    # models overview
    if (options[["resultsModels"]])
      .robmaModelsOvervievTable(jaspResults, options)
    # models summary
    if (options[["resultsIndividual"]])
      .robmaModelsSummaryTable(jaspResults, options)

    ### Plots
    # pooled estimates plots
    if (options[["plotsTheta"]])
      .robmaPlots(jaspResults, options, "theta")
    if (options[["plotsMu"]])
      .robmaPlots(jaspResults, options, "mu")
    if (options[["plotsTau"]])
      .robmaPlots(jaspResults, options, "tau")
    if (options[["plotsTau"]]                &&
        options[["plotsMu"]]                 &&
        options[["plotsType"]] == "averaged" &&
        !options[["plotsPriors"]])
      .robmaPlots(jaspResults, options, c("mu", "tau"))
    if (options[["plotsOmega"]])
      .robmaPlots(jaspResults, options, "omega")
    
    # individual models
    if (options[["plotsIndividualMu"]])
      .robmaModelsPlots(jaspResults, options, "mu")
    if (options[["plotsIndividualTau"]])
      .robmaModelsPlots(jaspResults, options, "tau")
    if (options[["plotsIndividualOmega"]])
      .robmaModelsPlots(jaspResults, options, "omega")
    
    ### Diagnostics
    # overview
    if (options[["diagnosticsOverview"]])
      .robmaDiagnosticsOverviewTable(jaspResults, options)
    # plots
    if ((
      options[["diagnosticsMu"]]    ||
      options[["diagnosticsTau"]]   ||
      options[["diagnosticsOmega"]] ||
      options[["diagnosticsTheta"]]
    ) &&
    (
      options[["diagnosticsTrace"]]           ||
      options[["diagnosticsAutocorrelation"]] ||
      options[["diagnosticsSamples"]]
    ))
      .robmaDiagnosticsPlots(jaspResults, options)
    
    ### Save the model
    if (options[["savePath"]] != "" &&
        is.null(jaspResults[["modelSaved"]]))
      .robmaSaveModel(jaspResults, options)
  }
  
  return()
}

.robmaDependencies <- c(
  "measures",
  "fittedPath",
  "cohensDTestType",
  "inputES",
  "inputT",
  "inputSE",
  "inputCI",
  "inputN",
  "inputN1",
  "inputN2",
  "inputLabels",
  "effectDirection",
  "priorsMu",
  "priorsTau",
  "priorsOmega",
  "priorsMuNull",
  "priorsTauNull",
  "priorsOmegaNull",
  "advancedControl",
  "advancedOmitPrior",
  "advancedOmitMarglik",
  "advancedOmitTheta",
  "advancedOmitError",
  "advancedOmitErrorValue",
  "advancedOmitESSValue",
  "advancedOmitESS",
  "advancedOmitRhatValue",
  "advancedOmitRhat",
  "advancedOmit",
  "advancedAutofitError",
  "advancedAutofitRhat",
  "advancedAutofitTimeUnit",
  "advancedAutofitTime",
  "advancedAutofit",
  "advancedThin",
  "advancedChains",
  "advancedIteration",
  "advancedBurnin",
  "advancedAdapt",
  "advancedBridgeIter",
  "advancedMuTransform"
)
# priors related functions
.robmaOptions2Priors      <- function(optionsPrior) {
  optionsPrior <- .robmaOptions2PriorsEval(optionsPrior)
  
  if (optionsPrior[["type"]] == "normal") {
    return(
      RoBMA::prior(
        distribution = "normal",
        parameters   = list(
          mean    = optionsPrior[["parMean"]],
          sd      = optionsPrior[["parScale"]]
        ),
        truncation   = list(
          lower   = optionsPrior[["truncationLower"]],
          upper   = optionsPrior[["truncationUpper"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] == "t") {
    return(
      RoBMA::prior(
        distribution = "t",
        parameters = list(
          location   = optionsPrior[["parMean"]],
          scale      = optionsPrior[["parScale"]],
          df         = optionsPrior[["parDF"]]
        ),
        truncation = list(
          lower      = optionsPrior[["truncationLower"]],
          upper      = optionsPrior[["truncationUpper"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] == "cauchy") {
    return(
      RoBMA::prior(
        distribution = "cauchy",
        parameters = list(
          location   = optionsPrior[["parLocation"]],
          scale      = optionsPrior[["parScale2"]]
        ),
        truncation = list(
          lower      = optionsPrior[["truncationLower"]],
          upper      = optionsPrior[["truncationUpper"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] == "gammaAB") {
    return(
      RoBMA::prior(
        distribution = "gamma",
        parameters = list(
          shape      = optionsPrior[["parAlpha"]],
          rate       = optionsPrior[["parBeta"]]
        ),
        truncation = list(
          lower      = optionsPrior[["truncationLower"]],
          upper      = optionsPrior[["truncationUpper"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] == "gammaK0") {
    return(
      RoBMA::prior(
        distribution = "gamma",
        parameters = list(
          shape      = optionsPrior[["parShape"]],
          scale      = optionsPrior[["parScale2"]]
        ),
        truncation = list(
          lower      = optionsPrior[["truncationLower"]],
          upper      = optionsPrior[["truncationUpper"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] == "invgamma") {
    return(
      RoBMA::prior(
        distribution = "invgamma",
        parameters = list(
          shape      = optionsPrior[["parAlpha"]],
          scale      = optionsPrior[["parBeta"]]
        ),
        truncation = list(
          lower      = optionsPrior[["truncationLower"]],
          upper      = optionsPrior[["truncationUpper"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] == "spike") {
    return(
      RoBMA::prior(
        distribution = "point",
        parameters = list(
          location   = optionsPrior[["parLocation"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] == "uniform") {
    return(
      RoBMA::prior(
        distribution = "uniform",
        parameters = list(
          a = optionsPrior[["parA"]],
          b = optionsPrior[["parB"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] %in% c("Two-sided", "Two-sided2")) {
    return(
      RoBMA::prior(
        distribution = "two.sided",
        parameters = list(
          alpha      = optionsPrior[["parAlpha"]],
          steps      = optionsPrior[["parCuts"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] == "One-sided (mon.)") {
    return(
      RoBMA::prior(
        distribution = "one.sided",
        parameters = list(
          alpha      = optionsPrior[["parAlpha"]],
          steps      = optionsPrior[["parCuts"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  } else if (optionsPrior[["type"]] == "One-sided") {
    return(
      RoBMA::prior(
        distribution = "one.sided",
        parameters = list(
          alpha1     = optionsPrior[["parAlpha1"]],
          alpha2     = optionsPrior[["parAlpha2"]],
          steps      = optionsPrior[["parCuts"]]
        ),
        prior_odds = optionsPrior[["priorOdds"]]
      )
    )
  }
}
.robmaOptions2PriorsClean <- function(x) {
  
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
.robmaOptions2PriorsEval  <- function(x) {
  if (x[["type"]] %in% c("Two-sided", "One-sided (mon.)", "One-sided")) {
    x[["priorOdds"]] <- eval(parse(text = x[["priorOdds"]]))
    x[["parAlpha"]]  <- .robmaOptions2PriorsClean(x[["parAlpha"]])
    x[["parAlpha1"]] <- .robmaOptions2PriorsClean(x[["parAlpha1"]])
    x[["parAlpha2"]] <- .robmaOptions2PriorsClean(x[["parAlpha2"]])
    x[["parCuts"]]   <- .robmaOptions2PriorsClean(x[["parCuts"]])
    
  } else if (x[["type"]] == "spike" &&  any(names(x) %in% c("parAlpha2"))) {
    x[["priorOdds"]]   <- eval(parse(text = x[["priorOdds"]]))
    x[["parLocation"]] <- 1
    
  } else{
    evalNames <-
      c(
        "parA",
        "parB",
        "parAlpha",
        "parBeta",
        "parDF",
        "parLocation",
        "parMean",
        "parScale",
        "parScale2",
        "parShape",
        "priorOdds",
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
# table filling functions
.robmaTableFillCoef       <- function(jaspTable, resultsTable, add_info, options, individual = FALSE) {
  
  overtitleCI <- gettextf("%s%% CI", 100 * options[["resultsCI"]])
  # add columns
  jaspTable$addColumnInfo(name = "terms",  title = "",                type = "string")
  jaspTable$addColumnInfo(name = "mean",   title = gettext("Mean"),   type = "number")
  jaspTable$addColumnInfo(name = "median", title = gettext("Median"), type = "number")
  jaspTable$addColumnInfo(name = "lowerCI",title = gettext("Lower"),  type = "number", overtitle = overtitleCI)
  jaspTable$addColumnInfo(name = "upperCI",title = gettext("Upper"),  type = "number", overtitle = overtitleCI)
  
  if (individual) {
    jaspTable$addColumnInfo(name = "error", title = gettext("MCMC error"), type = "number")
    jaspTable$addColumnInfo(name = "ess",   title = gettext("ESS"),        type = "integer")
    jaspTable$addColumnInfo(name = "rhat",  title = gettext("Rhat"),       type = "number")
  }
  
  
  if (is.null(resultsTable)) 
    return(jaspTable)
  
  # fill rows
  for (i in c(1:nrow(resultsTable))[rownames(resultsTable) %in% c("mu", "tau")]) {
    tempRow <- list(
      terms    = .robmaCoefNames(rownames(resultsTable)[i], add_info),
      mean     = resultsTable[i, "Mean"],
      median   = resultsTable[i, "Median"],
      lowerCI  = resultsTable[i, if(individual) ".025" else as.character(.5 - options[["resultsCI"]] / 2)],
      upperCI  = resultsTable[i, if(individual) ".975" else as.character(.5 + options[["resultsCI"]] / 2)]
    )
    if (individual) {
      tempRow[["error"]] <- resultsTable[i, "MCMC error"]
      tempRow[["ess"]]   <- resultsTable[i, "ESS"]
      tempRow[["rhat"]]  <- resultsTable[i, "Rhat"]
    }
    
    jaspTable$addRows(tempRow)
  }
  
  
  # add footnote
  if (any(rownames(resultsTable) == "tau")) {
    if (add_info[["effect_size"]] %in% c("r", "OR")){
      jaspTable$addFootnote(gettextf(
        "%1$s is on %2$s scale.", 
        "\u03C4",
        switch(
          add_info[["mu_transform"]],
          "cohens_d"  = gettext("Cohen's <em>d</em>"),
          "fishers_z" = gettext("Fisher's <em>z</em>"),
          "log_OR"    = gettext("log(<em>OR</em>)")
        )
      ))
    } 
    
  }
  
  return(jaspTable)
}
.robmaTableFillWeights    <- function(jaspTable, resultsTable, add_info, options, individual = FALSE) {
  
  overtitleP  <- gettextf("<em>p</em>-values interval %s","\u002A")
  overtitleCI <- gettextf("%s%% CI", 100 * options[["resultsCI"]])
  # add columns
  jaspTable$addColumnInfo(name = "lowerRange", title = gettext("Lower"),  type = "number", overtitle = overtitleP)
  jaspTable$addColumnInfo(name = "upperRange", title = gettext("Upper"),  type = "number", overtitle = overtitleP)
  jaspTable$addColumnInfo(name = "mean",       title = gettext("Mean"),   type = "number")
  jaspTable$addColumnInfo(name = "median",     title = gettext("Median"), type = "number")
  jaspTable$addColumnInfo(name = "lowerCI",    title = gettext("Lower"),  type = "number", overtitle = overtitleCI)
  jaspTable$addColumnInfo(name = "upperCI",    title = gettext("Upper"),  type = "number", overtitle = overtitleCI)
  
  if (individual) {
    jaspTable$addColumnInfo(name = "error",   title = gettext("MCMC error"), type = "number")
    jaspTable$addColumnInfo(name = "ess",     title = gettext("ESS"),        type = "integer")
    jaspTable$addColumnInfo(name = "rhat",    title = gettext("Rhat"),       type = "number")
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
      lowerCI    = resultsTable[i, if(individual) ".025" else as.character(.5 - options[["resultsCI"]] / 2)],
      upperCI    = resultsTable[i, if(individual) ".975" else as.character(.5 + options[["resultsCI"]] / 2)]
    )
    if (individual) {
      tempRow[["error"]] <- resultsTable[i, "MCMC error"]
      tempRow[["ess"]]   <- resultsTable[i, "ESS"]
      tempRow[["rhat"]]  <- resultsTable[i, "Rhat"]
    }
    
    jaspTable$addRows(tempRow)
  }
  
  
  # add footnote
  jaspTable$addFootnote(
    symbol = "\u002A",
    gettextf("The weights (%1$s) correspond to %2$s <em>p</em>-values.", "\u03C9", add_info[["weight_type"]])
  )
  
  return(jaspTable)
}
.robmaTableFillStudies    <- function(jaspTable, resultsTable, add_info, options, individual = FALSE) {
  
  overtitleCI <- gettextf("%s%% CI", 100 * options[["resultsCI"]])
  # add columns
  jaspTable$addColumnInfo(name = "terms",   title = "",               type = "string")
  jaspTable$addColumnInfo(name = "mean",    title = gettext("Mean"),  type = "number")
  jaspTable$addColumnInfo(name = "median",  title = gettext("Median"),type = "number")
  jaspTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number", overtitle = overtitleCI)
  jaspTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number", overtitle = overtitleCI)
  
  if (individual) {
    jaspTable$addColumnInfo(name = "error",   title = gettext("MCMC error"), type = "number")
    jaspTable$addColumnInfo(name = "ess",     title = gettext("ESS"),        type = "integer")
    jaspTable$addColumnInfo(name = "rhat",    title = gettext("Rhat"),       type = "number")
  }
  
  
  if (is.null(resultsTable))
    return(jaspTable)
  
  # fill rows
  tempI <- 0
  for (i in c(1:nrow(resultsTable))[grepl("theta", rownames(resultsTable))]) {
    tempI <- tempI + 1
    tempRow <- list(
      terms    = add_info[["study_names"]][tempI],
      mean     = resultsTable[i, "Mean"],
      median   = resultsTable[i, "Median"],
      lowerCI  = resultsTable[i, if(individual) ".025" else as.character(.5 - options[["resultsCI"]] / 2)],
      upperCI  = resultsTable[i, if(individual) ".975" else as.character(.5 + options[["resultsCI"]] / 2)]
    )
    if (individual) {
      tempRow[["error"]] <- resultsTable[i, "MCMC error"]
      tempRow[["ess"]]   <- resultsTable[i, "ESS"]
      tempRow[["rhat"]]  <- resultsTable[i, "Rhat"]
    }
    
    jaspTable$addRows(tempRow)
  }
  
  
  # add footnote
  if (add_info[["effect_size"]] %in% c("r", "d", "OR")){
    jaspTable$addFootnote(
      gettextf(
        "Estimated studies' effects (%1$s) correspond to effect size %2$s.",
        "\u03B8",
        .robmaCoefLetters(add_info[["effect_size"]])
    ))
  }
  
  return(jaspTable)
}
.robmaCoefNames           <- function(name, add_info) {
  if (name == "mu")
    return(gettextf(
      "Effect size (%s)",
      ifelse(add_info[["effect_size"]] %in% c("r", "d", "OR"), .robmaCoefLetters(add_info[["effect_size"]]), "\u03BC")
    ))
  if (name == "tau")
    return(gettextf("Heterogeneity (%s)","\u03C4"))
}
.robmaCoefLetters         <- function(effect_size){
  switch(
    effect_size,
    "r"   = "\u03C1",
    "d"   = "\u03B4",
    "OR"  = "OR"
  )
}
# helper functions
.robmaReady               <- function(options) {
  
  if (options[["measures"]] == "fitted") {
    return(options[["fittedPath"]] != "")
  }
  
  
  if (options[["measures"]] == "cohensd") {
    readyArg1 <- any(c(options[["inputES"]], options[["inputT"]]) != "")
    
    if (options[["cohensDTestType"]] == "one.sample") {
      readyArg2 <- any(c(
        options[["inputSE"]] != "",
        options[["inputN"]]  != "",
        sum(unlist(options[["inputCI"]]) != "") == 2
      ))
    } else if (options[["cohensDTestType"]] == "two.sample") {
      readyArg2 <- any(c(
        options[["inputSE"]] != "",
        options[["inputN"]] != "",
        sum(unlist(options[["inputCI"]]) != "") == 2,
        all(c(options[["inputN1"]], options[["inputN2"]]) != "")
      ))
    }
  } else if (options[["measures"]] == "correlation") {
    readyArg1 <- options[["inputES"]] != ""
    readyArg2 <- any(c(options[["inputSE"]], options[["inputN"]]) != "", sum(unlist(options[["inputCI"]]) != "") == 2)
  } else if (options[["measures"]] == "OR") {
    readyArg1 <- options[["inputES"]] != ""
    readyArg2 <- sum(unlist(options[["inputCI"]]) != "") == 2
  } else if (options[["measures"]] == "general") {
    readyArg1 <- options[["inputES"]] != ""
    readyArg2 <- any(options[["inputSE"]] != "", sum(unlist(options[["inputCI"]]) != "") == 2)
  }
  
  return(readyArg1 && readyArg2)
  
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
  
  if(!is.null(jaspResults[["model"]])){
    jaspResults[["model"]] <- NULL
  }
  
  return()
}
.robmaGetData             <- function(options, dataset) {
  if (options[["measures"]] == "fitted") {
    return(NULL)
  } else{
    if (!is.null(dataset)) {
      return(dataset)
    } else{
      varNames <-
        c(
          options[["inputT"]],
          options[["inputES"]],
          options[["inputSE"]],
          options[["inputN"]],
          options[["inputN1"]],
          options[["inputN2"]],
          unlist(options[["inputCI"]])
        )
      varNames <- varNames[varNames != ""]
      
      dataset <- readDataSetToEnd(
        columns.as.numeric = varNames,
        columns = if (options[["inputLabels"]] != "") options[["inputLabels"]]
      )

      if (options[["inputLabels"]] != ""){
        dataset[[.v(options[["inputLabels"]])]] <- as.character(dataset[[.v(options[["inputLabels"]])]])
        if (!validUTF8(dataset[[.v(options[["inputLabels"]])]]))
          .quitAnalysis(gettext("The study labels contain invalid characters. Please, remove them before running the analysis.")) 
      }

    }
    
  }
  
  return(dataset)
}
.robmaGetPriors           <- function(jaspResults, options) {
  if (!is.null(jaspResults[["priors"]])) {
    return()
  } else{
    priors <- createJaspState()
    priors$dependOn(.robmaDependencies)
    jaspResults[["priors"]] <- priors
  }
  
  
  object <- list()
  priorElements <- c("priorsMu", "priorsTau", "priorsOmega", "priorsMuNull", "priorsTauNull", "priorsOmegaNull")
  priorNames    <- c("mu", "tau", "omega", "mu_null", "tau_null", "omega_null")
  for (i in seq_along(priorElements)) {
    tmp <- NULL
    for (elem in options[[priorElements[i]]]) {
      tmpPrior <- tryCatch(.robmaOptions2Priors(elem), error = function(e)e)
      if(class(tmpPrior) %in% c("simpleError", "error")){
        .quitAnalysis(tmpPrior$message)
      }
      tmp <- c(tmp, list(tmpPrior))
    }
    object[[priorNames[i]]] <- tmp
  }
  
  
  priors[["object"]] <- object
  
  return()
}
# main functions
.robmaPriorsPlots              <- function(jaspResults, options) {
  # create / access the container
  if (!is.null(jaspResults[["priorPlots"]]))
    priorPlots <- jaspResults[["priorPlots"]]
  else{
    priorPlots <- createJaspContainer(title = gettext("Prior Plots"))
    priorPlots$dependOn(c("priorsPlot", "measures", "advancedMuTransform"))
    priorPlots$position <- 2
    jaspResults[["priorPlots"]] <- priorPlots
  }
  
  # extract the priors
  if (is.null(jaspResults[["model"]]))
    priors  <- jaspResults[["priors"]][["object"]]
  else{
    fit     <- jaspResults[["model"]][["object"]]
    priors  <- fit[["priors"]]
  }
  
  # create conitainer for each of the parameters
  for (parameter in c("mu", "tau", "omega")) {
    
    if (!is.null(priorPlots[[parameter]]))
      parameterContainer <- priorPlots[[parameter]]
    else{
      parameterContainer <- createJaspContainer(title = switch(
        parameter,
        "mu"    = gettext("Effect"),
        "tau"   = gettext("Heterogeneity"),
        "omega" = gettext("Weight Function")
      ))
      parameterContainer$position <- switch(
        parameter,
        "mu"    = 1,
        "tau"   = 2,
        "omega" = 3
      )
      priorPlots[[parameter]] <- parameterContainer
    }
    
    # create container for null and alternative models
    for (type in c("null", "alternative")) {
      
      if (!is.null(parameterContainer[[type]]))
        next
      else{
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
        typeContainer$dependOn(paste0("priors_", parameter, if (type == "null") "_null"))
        parameterContainer[[type]] <- typeContainer
      }
      
      tempPriors <- switch(
        paste0(parameter, "_", type),
        "mu_null"           = priors[["mu_null"]],
        "mu_alternative"    = priors[["mu"]],
        "tau_null"          = priors[["tau_null"]],
        "tau_alternative"   = priors[["tau"]],
        "omega_null"        = priors[["omega_null"]],
        "omega_alternative" = priors[["omega"]]
      )
      
      if (length(tempPriors) == 0)
        next
      
      # generate the actual plots
      for (i in 1:length(tempPriors)) {
        
        if (parameter == "omega")
          tempPlot <- createJaspPlot(width = 500,  height = 400)
        else
          tempPlot <- createJaspPlot(width = 400,  height = 300)
        
        typeContainer[[paste0(parameter, "_", type, "_", i)]] <- tempPlot
        
        
        if (is.null(jaspResults[["model"]])) {
          p <- RoBMA::plot.RoBMA.prior(
            tempPriors[[i]],
            plot_type    = "ggplot",
            par_name     = parameter,
            effect_size  = if(parameter == "mu") switch(
              options[["measures"]],
              "cohensd"     = "d",
              "correlation" = "r",
              "OR"          = "OR",
              "general"     = "y"
            ),
            mu_transform = if (options[["measures"]] %in% c("correlation", "OR") && parameter == "mu") options[["advancedMuTransform"]],
            samples      = 1e6,
          )
        } else{
          p <- RoBMA::plot.RoBMA.prior(
            tempPriors[[i]],
            plot_type    = "ggplot",
            par_name     = parameter,
            effect_size  = if(parameter == "mu") fit[["add_info"]][["effect_size"]],
            samples      = 1e6,
            mu_transform = if (fit[["add_info"]][["effect_size"]] %in% c("r", "OR") && parameter == "mu") fit[["add_info"]][["mu_transform"]]
          )
        }
        
        p <- jaspGraphs::themeJasp(p)
        
        typeContainer[[paste0(parameter, "_", type, "_", i)]][["plotObject"]] <- p
        
      }
    }
  }
  
  return()
}
.robmaModelPreviewTable        <- function(jaspResults, options) {
  # create / access the container
  if (!is.null(jaspResults[["modelPreview"]])) {
    return()
  } else{
    modelPreview <- createJaspContainer(title = gettext("Model Preview"))
    modelPreview$dependOn(.robmaDependencies)
    modelPreview$position <- 1
    jaspResults[["modelPreview"]] <- modelPreview
  }
  
  
  # extract the priors
  priors  <- jaspResults[["priors"]][["object"]]
  
  # set error if no priors are specified
  if ((length(priors[["mu"]]) == 0 &&
       length(priors[["mu_null"]]) == 0) ||
      (length(priors[["tau"]]) == 0 &&
       length(priors[["tau_null"]]) == 0) ||
      (length(priors[["omega"]]) == 0 &&
       length(priors[["omega_null"]]) == 0)) {
    priorsError <- createJaspTable()
    priorsError$setError(gettext(
        "At least one prior distribution per parameter must be specified (either null or alternative)."
    ))
    modelPreview[["priorsError"]] <- priorsError
    return()
  }
  
  
  # create the setup table
  fitSummary   <- RoBMA::check_setup(
    priors_mu         = priors[["mu"]],
    priors_tau        = priors[["tau"]],
    priors_omega      = priors[["omega"]],
    priors_mu_null    = priors[["mu_null"]],
    priors_tau_null   = priors[["tau_null"]],
    priors_omega_null = priors[["omega_null"]],
    models            = TRUE,
    silent            = TRUE
  )
  
  
  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1
  
  overallSummary$addColumnInfo(name = "terms",     title = "",                type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"), type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),   type = "number")

  if (options[["measures"]] != "fitted") {
    for (i in 1:nrow(fitSummary[["overview"]])) {
      tempRow <- list(
        terms     = if (i == 1) gettext("Effect") else if (i == 2) gettext("Heterogeneity") else if (i == 3) gettext("Publication bias"),
        models    = paste0(fitSummary[["overview"]][["Models"]][i], "/", fitSummary[["add_info"]][["n_models"]]),
        priorProb = fitSummary[["overview"]][["Prior prob."]][i]
      )

      overallSummary$addRows(tempRow)
    }
    overallSummary$addFootnote(gettext("The analysis will estimate multiple meta-analytic models using MCMC and might require a prolonged time to complete."))
  }
  
  modelPreview[["overallSummary"]] <- overallSummary
  
  
  ### create models overview table
  modelsSummary <- createJaspTable(title = gettext("Models Overview"))
  modelsSummary$position <- 2
  
  overtitlePrior <- gettext("Prior Distribution")
  
  modelsSummary$addColumnInfo(name = "number",      title = "#", type = "integer")
  modelsSummary$addColumnInfo(name = "priorMu",    title = gettext("Effect Size"),      type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorTau",   title = gettext("Heterogeneity"),    type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorOmega", title = gettext("Publication Bias"), type = "string", overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorProb",   title = gettext("P(M)"),             type = "number")

  if (options[["measures"]] != "fitted") {
    for (i in 1:nrow(fitSummary[["models"]])) {
      tempRow <- list(
        number      = as.numeric(rownames(fitSummary[["models"]]))[i],
        priorMu     = fitSummary[["models"]][i, "Prior mu"],
        priorTau    = fitSummary[["models"]][i, "Prior tau"],
        priorOmega  = fitSummary[["models"]][i, "Prior omega"],
        priorProb   = fitSummary[["models"]][i, "Prior prob."]
      )

      modelsSummary$addRows(tempRow)
    }
    modelsSummary$addFootnote(gettext("The analysis will estimate multiple meta-analytic models using MCMC and might require a prolonged time to complete."))
  }
  
  modelPreview[["modelsSummary"]] <- modelsSummary
  
  
  return()
}
.robmaFitModel                 <- function(jaspResults, dataset, options) {
  
  if (is.null(jaspResults[["model"]])) {
    model <- createJaspState()
    model$dependOn(c(
      "measures", 
      "fittedPath", "inputES", "inputT", "inputSE", "inputCI", "inputN", "inputN1", "inputN2",
      "effectDirection", "advancedMuTransform",
      "priorsMu", "priorsTau", "priorsOmega", "priorsMuNull", "priorsTauNull", "priorsOmegaNull"))
    jaspResults[["model"]] <- model
    fit                    <- NULL
  } else{
    model <- jaspResults[["model"]]
    fit   <- model[["object"]]
    
  }
  
  if (options[["measures"]] == "fitted") {
    
    fit <- tryCatch({
      fit <- readRDS(file = options[["fittedPath"]])
      if (!RoBMA::is.RoBMA(fit))
        .quitAnalysis(gettext("The loaded object is not a RoBMA model."))
      fit
    },error = function(e)e)
    
  } else{
    
    if (is.null(fit) || options[["advancedControl"]] == "refit") {
      # extract priors
      priors <- jaspResults[["priors"]][["object"]]
      
      fit <- tryCatch(RoBMA::RoBMA(
        likelihood = "normal",
        # data
        t   = if (options[["measures"]] == "cohensd" && options[["inputT"]] != "")                     dataset[, .v(options[["inputT"]])],
        d   = if (options[["measures"]] == "cohensd" && options[["inputES"]] != "")                    dataset[, .v(options[["inputES"]])],
        r   = if (options[["measures"]] == "correlation" && options[["inputES"]] != "")                dataset[, .v(options[["inputES"]])],
        OR  = if (options[["measures"]] == "OR")                                                        dataset[, .v(options[["inputES"]])],
        y   = if (options[["measures"]] == "general" && options[["inputES"]] != "")                    dataset[, .v(options[["inputES"]])],
        se  = if (options[["inputSE"]] != "")                                                          dataset[, .v(options[["inputSE"]])],
        lCI = if (sum(unlist(options[["inputCI"]]) != "") == 2)                                        dataset[, .v(options[["inputCI"]][[1]][1])],
        uCI = if (sum(unlist(options[["inputCI"]]) != "") == 2)                                        dataset[, .v(options[["inputCI"]][[1]][2])],
        n   = if (options[["measures"]] %in% c("cohensd", "correlation") && options[["inputN"]] != "") dataset[, .v(options[["inputN"]])],
        n1  = if (options[["measures"]] == "cohensd" && options[["inputN1"]] != "")                    dataset[, .v(options[["inputN1"]])],
        n2  = if (options[["measures"]] == "cohensd" && options[["inputN2"]] != "")                    dataset[, .v(options[["inputN2"]])],
        study_names = if (options[["inputLabels"]] != "")                                              dataset[, .v(options[["inputLabels"]])],
        # model settings
        test_type        = if (options[["measures"]] == "cohensd")                 options[["cohensDTestType"]],
        mu_transform     = if (options[["measures"]] %in% c("correlation", "OR"))  options[["advancedMuTransform"]],
        effect_direction = options[["effectDirection"]],
        # priors
        priors_mu         = priors[["mu"]],
        priors_tau        = priors[["tau"]],
        priors_omega      = priors[["omega"]],
        priors_mu_null    = priors[["mu_null"]],
        priors_tau_null   = priors[["tau_null"]],
        priors_omega_null = priors[["omega_null"]],
        # sampling settings
        chains  = options[["advancedChains"]],
        iter    = options[["advancedIteration"]],
        burnin  = options[["advancedBurnin"]],
        thin    = options[["advancedThin"]],
        # additional settings
        control = list(
          autofit         = options[["advancedAutofit"]],
          max_error       = if (options[["advancedAutofit"]]) options[["advancedAutofitError"]],
          max_rhat        = if (options[["advancedAutofit"]]) options[["advancedAutofitRhat"]],
          max_time        = if (options[["advancedAutofit"]]) paste0(options[["advancedAutofitTime"]], options[["advancedAutofitTimeUnit"]]),
          adapt           = options[["advancedAdapt"]],
          bridge_max_iter = options[["advancedBridgeIter"]],
          allow_max_error = if (options[["advancedOmit"]] && options[["advancedOmitError"]]) options[["advancedOmitErrorValue"]],
          allow_max_rhat  = if (options[["advancedOmit"]] && options[["advancedOmitRhat"]])  options[["advancedOmitRhatValue"]],
          allow_min_ESS   = if (options[["advancedOmit"]] && options[["advancedOmitESS"]])   options[["advancedOmitESSValue"]],
          allow_inc_theta = options[["advancedOmitTheta"]],
          balance_prob    = options[["advancedOmitPrior"]] == "conditional",
          silent          = FALSE,
          progress_start  = 'startProgressbar(length(object$models))',
          progress_tick   = 'progressbarTick()'
        ),
        save    = "all",
        seed    = if (options[["setSeed"]]) options[["seed"]],
      ),error = function(e)e)
      
    } else{

      fit <- tryCatch(RoBMA::update.RoBMA(
        object  = fit,
        study_names  = if (options[["inputLabels"]] != "") dataset[, .v(options[["inputLabels"]])],
        chains  = options[["advancedChains"]],
        iter    = options[["advancedIteration"]],
        burnin  = options[["advancedBurnin"]],
        thin    = options[["advancedThin"]],
        control = list(
          autofit         = options[["advancedAutofit"]],
          max_error       = if (options[["advancedAutofit"]]) options[["advancedAutofitError"]],
          max_time        = if (options[["advancedAutofit"]]) paste0(options[["advancedAutofitTime"]], options[["advancedAutofitTimeUnit"]]),
          adapt           = options[["advancedAdapt"]],
          bridge_max_iter = options[["advancedBridgeIter"]],
          allow_max_error = if (options[["advancedOmit"]] && options[["advancedOmitError"]]) options[["advancedOmitErrorValue"]],
          allow_max_rhat  = if (options[["advancedOmit"]] && options[["advancedOmitRhat"]])  options[["advancedOmitRhatValue"]],
          allow_min_ESS   = if (options[["advancedOmitESS"]]) options[["advancedOmitESSValue"]],
          allow_inc_theta = options[["advancedOmitTheta"]],
          balance_prob    = options[["advancedOmitPrior"]] == "conditional",
          silent          = FALSE,
          progress_start  = 'startProgressbar(sum(converged_models))',
          progress_tick   = 'progressbarTick()'
        ),
        refit_failed = options[["advancedControl"]] != "no_refit"
      ),error = function(e)e)
      
    }
    
  }
  
  
  # error handling
  if(any(class(fit) %in% c("simpleError", "error"))){
    .quitAnalysis(fit[["message"]])
  }
  
  
  # update the fit and reset notifier
  model[["object"]] <- fit
  .robmaModelNotifier(jaspResults)
  
  return()
}
.robmaSummaryTable             <- function(jaspResults, options) {
  if (!is.null(jaspResults[["mainSummary"]])) {
    return()
  } else{
    # create container
    mainSummary <- createJaspContainer(title = gettext("Summary"))
    mainSummary$position <- 3
    summaryDependencies <-
      c(
        .robmaDependencies,
        "bayesFactorType",
        "resultsCI",
        "resultsConditional",
        "resultsTheta"
      )
    mainSummary$dependOn(summaryDependencies)
    jaspResults[["mainSummary"]] <- mainSummary
  }
  
  # remove the model preview
  jaspResults[["modelPreview"]] <- NULL
  
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  # some shared info
  fitSummary <- RoBMA::summary.RoBMA(
    fit,
    logBF    = options[["bayesFactorType"]] == "LogBF10",
    BF01     = options[["bayesFactorType"]] == "BF01",
    probs    = c(.5 + c(-1, 1) * options[["resultsCI"]] / 2),
    conditional   = options[["resultsConditional"]],
    include_theta = options[["resultsTheta"]]
  )
  
  ### create overview table
  overallSummary <- createJaspTable(title = gettext("Model Summary"))
  overallSummary$position <- 1
  
  titleBF <- paste0(
    ifelse(
      options[["bayesFactorType"]] == "BF01",
      gettext("Exclusion"),
      gettext("Inclusion")
    ),
    " ",
    ifelse(
      options[["bayesFactorType"]] == "LogBF10",
      gettext("log(BF)"),
      gettext("BF")
    )
  )
  
  overallSummary$addColumnInfo(name = "terms",     title = "",                   type = "string")
  overallSummary$addColumnInfo(name = "models",    title = gettext("Models"),    type = "string")
  overallSummary$addColumnInfo(name = "priorProb", title = gettext("P(M)"),      type = "number")
  overallSummary$addColumnInfo(name = "postProb",  title = gettext("P(M|data)"), type = "number")
  overallSummary$addColumnInfo(name = "BF",        title = titleBF,              type = "number")
  
  if (!any(fit[["add_info"]][["converged"]])) {
    overallSummary$setError(
      gettext("All models failed to converge. Please, consider inspecting the 'MCMC diagnostics' and changing the 'Advanced options'.")
    )
    return()
  }
  
  for (i in 1:nrow(fitSummary[["overview"]])) {
    tempRow <- list(
      terms     = if (i == 1) gettext("Effect") else if (i == 2) gettext("Heterogeneity") else if (i == 3) gettext("Publication bias"),
      models    = paste0(fitSummary[["overview"]][i, "Models"], "/",  fitSummary[["add_info"]][["n_models"]] - fitSummary[["add_info"]][["failed"]]),
      priorProb = fitSummary[["overview"]][i, "Prior prob."],
      postProb  = fitSummary[["overview"]][i, "Post. prob."],
      BF        = fitSummary[["overview"]][i, 4]
    )
    
    overallSummary$addRows(tempRow)
  }
  if (fitSummary[["add_info"]][["failed"]] != 0)
    overallSummary$addFootnote(symbol = gettext("Warning:"), gettextf("%i models failed to converge.", fitSummary[["add_info"]][["failed"]]))
  if (!is.null(fit[["add_info"]][["warnings"]])) {
    for (w in fit[["add_info"]][["warnings"]])
      overallSummary$addFootnote(symbol = gettext("Warning:"), w)
  }
  if (options$measures == "OR")
    overallSummary$addFootnote(symbol = gettext("Warning:"), gettext("Analyzing odds ratios is an experimental feature. The performance of default prior distributions was not evaluated."))
  
  mainSummary[["overallSummary"]] <- overallSummary
  
  
  ### create model averaged results tables
  # estimate table
  averagedSummary <- createJaspTable(title = gettext("Model Averaged Estimates"))
  averagedSummary$position <- 2
  averagedSummary <- .robmaTableFillCoef(averagedSummary, fitSummary[["averaged"]], fitSummary[["add_info"]], options)
  mainSummary[["averagedSummary"]] <- averagedSummary
  
  # weights table
  if (any(grepl("omega", rownames(fitSummary[["averaged"]])))) {
    averagedWeights <- createJaspTable(title = gettextf("Model Averaged Weights (%s)", "\u03C9"))
    averagedWeights$position <- 3
    averagedWeights <- .robmaTableFillWeights(averagedWeights, fitSummary[["averaged"]], fitSummary[["add_info"]], options)
    mainSummary[["averagedWeights"]] <- averagedWeights
  }
  
  # estimated studies table
  if (options[["resultsTheta"]]) {
    studiesSummary <- createJaspTable(title = gettextf("Model Averaged Estimated Studies' Effects (%s)", "\u03B8"))
    studiesSummary$position <- 4
    studiesSummary <- .robmaTableFillStudies(studiesSummary, fitSummary[["averaged"]], fitSummary[["add_info"]], options)
    mainSummary[["studiesSummary"]] <- studiesSummary
  }
  
  
  ### create conditional models results tables
  if (options[["resultsConditional"]]) {
    # estimate table
    conditionalSummary <- createJaspTable(title = gettext("Conditional Estimates"))
    conditionalSummary$position <- 5
    conditionalSummary <-.robmaTableFillCoef(conditionalSummary, fitSummary[["conditional"]], fitSummary[["add_info"]], options)
    conditionalSummary$addFootnote(gettext("Estimates are model averaged over models assuming existence of effect / heterogeneity."))
    mainSummary[["conditionalSummary"]] <- conditionalSummary
    
    # weights table
    if (any(grepl("omega", rownames(fitSummary[["conditional"]])))) {
      conditionalWeights <- createJaspTable(title = gettextf("Conditional Weights (%s)", "\u03C9"))
      conditionalWeights$position <- 6
      conditionalWeights <- .robmaTableFillWeights(conditionalWeights, fitSummary[["conditional"]], fitSummary[["add_info"]], options)
      conditionalWeights$addFootnote(gettextf("Estimated weights (%s) are model averaged over models assuming existence of publication bias.", "\u03C9"))
      mainSummary[["conditionalWeights"]] <- conditionalWeights
    }
    
    # add the estimated studies effects
    if (options[["resultsTheta"]]) {
      conditionalStudiesSummary <- createJaspTable(title = gettextf("Conditional Estimated Studies' Effects (%s)","\u03B8"))
      conditionalStudiesSummary$position <- 7
      conditionalStudiesSummary <- .robmaTableFillStudies(conditionalStudiesSummary,fitSummary[["conditional"]],fit[["add_info"]], options)
      conditionalStudiesSummary$addFootnote(gettextf("Estimated studies effects (%s) are model averaged over models assuming existence of effect.", "\u03B8"))
      mainSummary[["conditionalStudiesSummary"]] <- conditionalStudiesSummary
    }
    
  }
  
  return()
}
.robmaModelsOvervievTable      <- function(jaspResults, options) {
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  # some shared info
  fitSummary <- RoBMA::summary.RoBMA(
    fit,
    type     = "models",
    logBF    = options[["bayesFactorType"]] == "LogBF10",
    BF01     = options[["bayesFactorType"]] == "BF01",
    probs    = c(.5 + c(-1, 1) * options[["resultsCI"]] / 2)
  )
  
  # do ordering
  if (options[["resultsModelsOrder"]] == "marglik") {
    fitSummary[["overview"]] <- fitSummary[["overview"]][order(fitSummary[["overview"]][["log(MargLik)"]], decreasing = TRUE),]
  } else if (options[["resultsModelsOrder"]] == "posterior") {
    fitSummary[["overview"]] <- fitSummary[["overview"]][order(fitSummary[["overview"]][["Post. prob."]], decreasing = TRUE),]
  }
  
  # compute the BF requested
  if (options[["resultsModelsBF"]] == "inclusion") {
    bf <- fitSummary[["overview"]][, 7]
  } else if (options[["resultsModelsBF"]] == "best") {
    bf <- exp(fitSummary[["overview"]][["log(MargLik)"]]) / exp(max(fitSummary[["overview"]][["log(MargLik)"]]))
  } else if (options[["resultsModelsBF"]] == "previous") {
    temp_this <- exp(fitSummary[["overview"]][["log(MargLik)"]])[-length(fitSummary[["overview"]][["log(MargLik)"]])]
    temp_prev <- exp(fitSummary[["overview"]][["log(MargLik)"]])[-1]
    bf <- c(1, temp_prev / temp_this)
  }
  
  
  summaryDependencies <- c(
      .robmaDependencies,
      "bayesFactorType",
      "resultsCI",
      "resultsModels",
      "resultsModelsBF",
      "resultsModelsOrder"
    )
  
  
  ### create overview table
  modelsSummary <- createJaspTable(title = gettext("Models Overview"))
  modelsSummary$position <- 6
  modelsSummary$dependOn(summaryDependencies)
  
  if (options[["resultsModelsBF"]] == "inclusion") {
    titleBF <- paste0(
      ifelse(options[["bayesFactorType"]] == "BF01",    gettext("Exclusion"), gettext("Inclusion")),
      " ",
      ifelse(options[["bayesFactorType"]] == "LogBF10", gettext("log(BF)"),   gettext("BF"))
    )
  } else{
    
    titleBF <- switch(
      options[["bayesFactorType"]],
      "BF10"    = gettextf("BF%s",     "\u2081\u2080"),
      "BF01"    = gettextf("BF%s",     "\u2080\u2081"),
      "LogBF10" = gettextf("log(BF%s)","\u2081\u2080")
    )
    bf <- switch(
      options[["bayesFactorType"]],
      "BF10"    = bf,
      "BF01"    = 1/bf,
      "LogBF10" = log(bf)
    )
    
  }
  
  overtitlePrior <- gettext("Prior Distribution")
  
  modelsSummary$addColumnInfo(name = "number",     title = "#",type = "integer")
  modelsSummary$addColumnInfo(name = "priorMu",    title = gettext("Effect Size"),      type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorTau",   title = gettext("Heterogeneity"),    type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorOmega", title = gettext("Publication Bias"), type = "string",  overtitle = overtitlePrior)
  modelsSummary$addColumnInfo(name = "priorProb",  title = gettext("P(M)"),             type = "number")
  modelsSummary$addColumnInfo(name = "postProb",   title = gettext("P(M|data)"),        type = "number")
  modelsSummary$addColumnInfo(name = "marglik",    title = gettext("log(MargLik)"),     type = "number")
  modelsSummary$addColumnInfo(name = "BF",         title = titleBF,                     type = "number")
  
  for (i in 1:nrow(fitSummary[["overview"]])) {
    tempRow <- list(
      number       = as.numeric(rownames(fitSummary[["overview"]]))[i],
      priorMu     = fitSummary[["overview"]][i, "Prior mu"],
      priorTau    = fitSummary[["overview"]][i, "Prior tau"],
      priorOmega  = fitSummary[["overview"]][i, "Prior omega"],
      priorProb    = fitSummary[["overview"]][i, "Prior prob."],
      postProb     = fitSummary[["overview"]][i, "Post. prob."],
      marglik      = fitSummary[["overview"]][i, "log(MargLik)"],
      BF           = bf[i]
    )
    
    modelsSummary$addRows(tempRow)
  }
  
  jaspResults[["mainSummary"]][["modelsSummary"]] <-  modelsSummary
  
  return()
}
.robmaModelsSummaryTable       <- function(jaspResults, options) {
  if (!is.null(jaspResults[["individualModels"]])) {
    return()
  } else{
    individualModels <- createJaspContainer(title = gettext("Individual Models Summary"))
    individualModels$position <- 5
    summaryDependencies <- c(
        .robmaDependencies,
        "bayesFactorType",
        "resultsIndividual",
        "resultsTheta",
        "resultsIndividualSingle" ,
        "resultsIndividualSingleNumber"
      )
    individualModels$dependOn(summaryDependencies)
    jaspResults[["individualModels"]] <- individualModels
  }
  
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  # some shared info
  fitSummary <- RoBMA::summary.RoBMA(
    fit,
    type     = "individual",
    logBF    = options[["bayesFactorType"]] == "LogBF10",
    BF01     = options[["bayesFactorType"]] == "BF01",
    include_theta = options[["resultsTheta"]]
  )
  
  titleBF <- paste0(
    ifelse(
      options[["bayesFactorType"]] == "BF01",
      gettext("Exclusion"),
      gettext("Inclusion")
    ),
    " ",
    ifelse(
      options[["bayesFactorType"]] == "LogBF10",
      gettext("log(BF)"),
      gettext("BF")
    )
  )
  
  ### create tables for individual models
  
  # select models to iterate over
  if (options[["resultsIndividualSingle"]]) {
    modelsI <- options[["resultsIndividualSingleNumber"]]
    if (modelsI < 1 || modelsI > length(fit[["models"]])) {
      tempModel  <- createJaspContainer(title = gettextf("Model %i", modelsI))
      tempError  <- createJaspTable(title = "")
      tempError$setError(gettextf("Model %i does not exist. Select one of the models between 1 and %i.", modelsI, length(fit[["models"]])))
      tempModel[["tempError"]]                      <- tempError
      individualModels[[paste0("model_", modelsI)]] <- tempModel
      return()
    }
  } else{
    modelsI <- 1:length(fitSummary[["overview"]])
  }
  
  
  # do the iteration
  for (i in modelsI) {
    tempModel <- createJaspContainer(title = gettextf("Model %i", i))
    individualModels[[paste0("model_", i)]] <- tempModel
    
    ### model priors
    tempPriors <- createJaspTable(title = gettext("Priors"))
    tempPriors$addColumnInfo(name = "priorMu",    title = gettext("Effect Size"),      type = "string")    
    tempPriors$addColumnInfo(name = "priorTau",   title = gettext("Heterogeneity"),    type = "string")
    tempPriors$addColumnInfo(name = "priorOmega", title = gettext("Publication Bias"), type = "string")
    
    tempRow <- list(
      priorMu     = RoBMA::print.RoBMA.prior(fitSummary[["overview"]][[i]][["priors"]][["mu"]], silent = TRUE),
      priorTau    = RoBMA::print.RoBMA.prior(fitSummary[["overview"]][[i]][["priors"]][["tau"]], silent = TRUE),
      priorOmega  = RoBMA::print.RoBMA.prior(fitSummary[["overview"]][[i]][["priors"]][["omega"]], silent = TRUE)
    )
    tempPriors$addRows(tempRow)
    
    tempModel[["tempPriors"]] <- tempPriors
    
    
    ### model information
    tempInfo <- createJaspTable(title = gettext("Information"))
    
    tempInfo$addColumnInfo(name = "priorProb",   title = gettext("P(M)"),          type = "number")
    tempInfo$addColumnInfo(name = "postProb",    title = gettext("P(M|data)"),     type = "number")
    tempInfo$addColumnInfo(name = "marglik",     title = gettext("log(MargLik)"),  type = "number")
    tempInfo$addColumnInfo(name = "BF",          title = titleBF,                 type = "number")
    
    tempRow <- list(
      priorProb    = fitSummary[["overview"]][[i]][["prior_prob"]],
      postProb     = fitSummary[["overview"]][[i]][["posterior_prob"]],
      marglik      = fitSummary[["overview"]][[i]][["marg_lik"]],
      BF           = fitSummary[["overview"]][[i]][["BF"]]
    )
    tempInfo$addRows(tempRow)
    
    tempModel[["tempInfo"]] <- tempInfo
    
    
    ### model coeficients
    # estimate table
    tempCoef <- createJaspTable(title = gettext("Model Estimates"))
    tempCoef <- .robmaTableFillCoef(tempCoef,
                             fitSummary[["overview"]][[i]][["tab"]],
                             fitSummary[["overview"]][[i]][["add_info"]],
                             options,
                             individual = TRUE)
    tempModel[["tempCoef"]] <- tempCoef
    
    ### weights and Studies's effects
    if (!is.null(fitSummary[["overview"]][[i]][["tab"]])) {
      # weights table
      if (any(grepl("omega", rownames(fitSummary[["overview"]][[i]][["tab"]])))) {
        tempWeights <- createJaspTable(title = gettextf("Estimated Weights (%s)", "\u03C9"))
        tempWeights <- .robmaTableFillWeights(
            tempWeights,
            fitSummary[["overview"]][[i]][["tab"]],
            fitSummary[["overview"]][[i]][["add_info"]],
            options,
            individual = TRUE
          )
        tempModel[["tempWeights"]] <- tempWeights
      }
      
      # estimated studies table
      if (any(grepl("theta", rownames(fitSummary[["overview"]][[i]][["tab"]])))) {
        tempStudies <- createJaspTable(title = gettextf("Estimated Studies' Effects (%s)", "\u03B8"))
        tempStudies <- .robmaTableFillStudies(
            tempStudies,
            fitSummary[["overview"]][[i]][["tab"]],
            fitSummary[["overview"]][[i]][["add_info"]],
            options,
            individual = TRUE
          )
        tempModel[["tempStudies"]] <- tempStudies
        
      }
    }
    
  }
  
  return()
}
.robmaPlots                    <- function(jaspResults, options, parameters) {
  # create / access the container
  if (is.null(jaspResults[["plots"]])) {
    plots <- createJaspContainer(title = gettext("Plots"))
    plots$position <- 6
    jaspResults[["plots"]] <- plots
  } else{
    plots <- jaspResults[["plots"]]
  }
  
  if (!is.null(plots[[paste(parameters, collapse = "")]])) {
    return()
  }
  
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  tempS <- RoBMA::summary.RoBMA(fit)
  
  # get overall settings
  dependencies <- c(
    .robmaDependencies,
    "plotsType",
    "plotsPriors",
    if (any(parameters %in% "mu"))
      "plotsMu",
    if (any(parameters %in% "tau"))
      "plotsTau",
    if (any(parameters %in% "omega"))
      c("plotsOmega", "rescaleWeightfunction", "plotsOmegaFunction"),
    if (any(parameters %in% "theta"))
      c("plotsTheta", "plotsThetaShow", "plotsThetaOrder")
  )
  
  if (all(parameters %in% "mu")) {
    title    <- gettext("Effect size")
    position <- 2
  } else if (all(parameters %in% "tau")) {
    title    <- gettext("Heterogeneity")
    position <- 3
  } else if (all(parameters %in% "omega")) {
    title    <-
      if (options[["plotsOmegaFunction"]])
        gettext("Weight function")
    else
      gettext("Weights")
    position <- 5
  } else if (all(parameters %in% "theta")) {
    title    <- gettext("Forest plot")
    position <- 1
  } else if (all(parameters %in% c("mu", "tau"))) {
    title    <- gettext("Effect size vs Heterogeneity")
    position <- 4
  }
  title <- gettextf("%1$s (%2$s)", title, ifelse(options[["plotsType"]] == "conditional", gettext("Conditional"), gettext("Model Averaged")))
  
  if (all(parameters %in% "theta")) {
    height <- 250 + ncol(fit[["RoBMA"]][["samples"]][["averaged"]][["theta"]]) * if (options[["plotsThetaShow"]] == "both") 55 else  20
    width  <- 350 + 9 * if (!is.null(fit[["add_info"]][["study_names"]])) max(nchar(fit[["add_info"]][["study_names"]])) else 10
    if (options[["plotsThetaShow"]] == "both") {
      pars <- c("forest", "theta")
    } else if (options[["plotsThetaShow"]] == "observed") {
      pars <- "forest"
    } else if (options[["plotsThetaShow"]] == "estimated") {
      pars <- "theta"
    }
  } else{
    height <- 370
    width  <- 500
    pars   <- parameters
  }
  
  
  # plot
  p <- tryCatch(
    RoBMA::plot.RoBMA(
      fit,
      parameter = pars,
      type      = options[["plotsType"]],
      plot_type = "ggplot",
      prior     = options[["plotsPriors"]],
      weights   = if (parameters == "omega") !options[["plotsOmegaFunction"]] else  FALSE,
      order     = if (parameters == "theta") {if (options[["plotsThetaOrder"]] == "labels") NULL else options[["plotsThetaOrder"]]},
      rescale_x = if (parameters == "omega") options[["rescaleWeightfunction"]] else  FALSE,
    ),
    error = function(e) e
  )
  
  if (any(class(p) %in% "error")) {
    tempPlot <- createJaspPlot(title = title, width = width, height = height)
    tempPlot$position <- position
    tempPlot$dependOn(dependencies)
    tempPlot$setError(p[["message"]])
    plots[[paste(parameters, collapse = "")]] <- tempPlot
    return()
  }
  
  if (ggplot2::is.ggplot(p)) {
    tempPlot <- createJaspPlot(title = title, width = width, height = height)
    tempPlot$position <- position
    tempPlot$dependOn(dependencies)
    plots[[paste(parameters, collapse = "")]] <- tempPlot
    
    if (all(parameters %in% "theta"))
      p <- jaspGraphs::themeJasp(p, sides = "b")
    else if (!is.null(p[["double_y_axis"]]))
      p <- jaspGraphs::themeJasp(p, sides = "blr")  + ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(vjust = 3),
        plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))
    else
      p <- jaspGraphs::themeJasp(p, sides = "bl")
    
    plots[[paste(parameters, collapse = "")]][["plotObject"]] <- p
    
  } else{
    tempPlots <- createJaspContainer(title = title)
    tempPlots$position <- position
    tempPlots$dependOn(dependencies)
    plots[[paste(parameters, collapse = "")]] <- tempPlots
    
    for (i in 1:length(p)) {
      tempPlot <- createJaspPlot(width = width, height = height)
      tempPlots[[paste(parameters, "_", i, collapse = "")]] <- tempPlot
      
      if (all(parameters %in% "theta"))
        p[[i]] <- jaspGraphs::themeJasp(p[[i]], sides = "b")
      else if(!is.null(p[[i]][["double_y_axis"]]))
        p[[i]] <- jaspGraphs::themeJasp(p[[i]], sides = "blr") + ggplot2::theme(
          axis.title.y.right = ggplot2::element_text(vjust = 3.25),
          plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))
      else
        p[[i]] <- jaspGraphs::themeJasp(p[[i]], sides = "bl")
      
      
      tempPlots[[paste(parameters, "_", i, collapse = "")]][["plotObject"]] <- p[[i]]
      
    }
  }
  
  
  return()
}
.robmaModelsPlots              <- function(jaspResults, options, parameters) {
  # create / access the container
  if (is.null(jaspResults[["plotsIndividual"]])) {
    plotsIndividual <- createJaspContainer(title = gettext("Individual Models Plots"))
    plotsIndividual$position <- 7
    jaspResults[["plotsIndividual"]] <- plotsIndividual
  } else{
    plotsIndividual <- jaspResults[["plotsIndividual"]]
  }
  
  if (!is.null(plotsIndividual[[paste(parameters, collapse = "")]])) {
    return()
  }
  
  # extract the model
  fit    <- jaspResults[["model"]][["object"]]
  tempS <- RoBMA::summary.RoBMA(fit)
  
  # get overall settings
  dependencies <- c(
    .robmaDependencies,
    "plotsTypeIndividualConditional",
    "plotsTypeIndividualOrder",
    "plotsTypeIndividualBy",
    if (any(parameters %in% "mu"))    "plotsIndividualMu",
    if (any(parameters %in% "tau"))   "plotsIndividualTau",
    if (any(parameters %in% "omega")) "plotsIndividualOmega"
  )
  
  if (all(parameters == "mu")) {
    title    <- gettext("Effect size")
    position <- 1
  } else if (parameters == "tau") {
    title    <- gettext("Heterogeneity")
    position <- 2
  } else if (parameters == "omega") {
    title    <- gettext("Weights")
    position <- 3
  }
  title <- gettextf(
    "%1$s (%2$s)",
    title,
    ifelse(options[["plotsTypeIndividualConditional"]], gettext("Conditional Models"), gettext("Models")))
  
  height <- 250 + 70 * if (options[["plotsTypeIndividualConditional"]]) {
    tempS[["overview"]][["Models"]][if (parameters == "mu") 1 else if (parameters == "tau") 2 else if (parameters == "omega") 3]
  } else {
    tempS[["add_info"]][["n_models"]]
  }
  
  width  <- 750
  pars   <- parameters
  
  
  # plot
  if (pars == "omega" && sum(grepl(pars, rownames(tempS[["averaged"]]))) > 2) {
    tempPlots <- createJaspContainer(title = title)
    tempPlots$position <- position
    tempPlots$dependOn(dependencies)
    plotsIndividual[[paste(parameters, collapse = "")]] <- tempPlots
    
    # nota that this creates a list of ggplot objects
    p <- tryCatch(
      RoBMA::plot.RoBMA(
        fit,
        parameter = pars,
        type      = c("individual", if (options[["plotsTypeIndividualConditional"]]) "conditional"),
        plot_type = "ggplot",
        order     = c(options[["plotsTypeIndividualOrder"]], options[["plotsTypeIndividualBy"]])
      ),
      error = function(e) e
    )
    
    if (any(class(p) %in% "error")) {
      tempPlot <- createJaspPlot(title = title, width = width, height = height)
      tempPlot$position <- position
      tempPlot$dependOn(dependencies)
      tempPlot$setError(p[["message"]])
      plotsIndividual[[paste(parameters, collapse = "")]] <-
        tempPlot
      return()
    }
    
    for (i in 1:length(p)) {
      tempPlot <- createJaspPlot(title = "", width = width, height = height)
      tempPlots[[paste0("plot_", i)]] <- tempPlot
      
      p[[i]] <- jaspGraphs::themeJasp(p[[i]], sides = "b")
      
      tempPlots[[paste0("plot_", i)]][["plotObject"]] <- p[[i]]
    }
    
  } else{
    tempPlot <- createJaspPlot(title = title, width = width, height = height)
    tempPlot$position <- position
    tempPlot$dependOn(dependencies)
    plotsIndividual[[paste(parameters, collapse = "")]] <- tempPlot
    
    p <- tryCatch(
      RoBMA::plot.RoBMA(
        fit,
        parameter = pars,
        type      = c("individual", if (options[["plotsTypeIndividualConditional"]]) "conditional"),
        plot_type = "ggplot",
        order     = c(options[["plotsTypeIndividualOrder"]], options[["plotsTypeIndividualBy"]]
        )
      ),
      error = function(e)e
    )
    
    if (any(class(p) %in% "error")) {
      tempPlot$setError(p[["message"]])
      return()
    }
    
    p <- jaspGraphs::themeJasp(p, sides = "b")
    
    plotsIndividual[[paste(parameters, collapse = "")]][["plotObject"]] <- p
  }
  
  return()
}
.robmaDiagnosticsOverviewTable <- function(jaspResults, options) {
  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 8
    diagnostics$dependOn(.robmaDependencies)
    jaspResults[["diagnostics"]] <- diagnostics
  } else{
    diagnostics <- jaspResults[["diagnostics"]]
  }
  
  if (!is.null(diagnostics[["modelsDiagnostics"]])) {
    return()
  }
  
  
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  
  # some shared info
  fitSummary <- RoBMA::summary.RoBMA(
    fit,
    type          = "models",
    diagnostics   = TRUE,
    include_theta = options[["diagnosticsOverviewTheta"]]
  )
  
  # do ordering
  diagnostics_dependencies <- c(.robmaDependencies, "diagnosticsOverview", "diagnosticsOverviewTheta")
  
  ### create overview table
  modelsDiagnostics <-  createJaspTable(title = gettext("Models Diagnostics Overview"))
  modelsDiagnostics$position <- 1
  modelsDiagnostics$dependOn(diagnostics_dependencies)
  
  overtitlePrior <- gettext("Prior Distribution")
    
  modelsDiagnostics$addColumnInfo(name = "number",      title = "#",                         type = "integer")
  modelsDiagnostics$addColumnInfo(name = "priorMu",    title = gettext("Effect Size"),      type = "string", overtitle = overtitlePrior)
  modelsDiagnostics$addColumnInfo(name = "priorTau",   title = gettext("Heterogeneity"),    type = "string", overtitle = overtitlePrior)
  modelsDiagnostics$addColumnInfo(name = "priorOmega", title = gettext("Publication Bias"), type = "string", overtitle = overtitlePrior)
  modelsDiagnostics$addColumnInfo(name = "error",       title = gettext("max(MCMC error)"),  type = "number")
  modelsDiagnostics$addColumnInfo(name = "ESS",         title = gettext("min(ESS)"),         type = "integer")
  modelsDiagnostics$addColumnInfo(name = "Rhat",        title = gettext("max(Rhat)"),        type = "number")
  
  for (i in 1:nrow(fitSummary[["diagnostics"]])) {
    tempRow <- list(
      number       = as.numeric(rownames(fitSummary[["diagnostics"]]))[i],
      priorMu     = fitSummary[["diagnostics"]][i, "Prior mu"],
      priorTau    = fitSummary[["diagnostics"]][i, "Prior tau"],
      priorOmega  = fitSummary[["diagnostics"]][i, "Prior omega"],
      error        = fitSummary[["diagnostics"]][i, "max(MCMC error)"],
      ESS          = fitSummary[["diagnostics"]][i, "min(ESS)"],
      Rhat         = fitSummary[["diagnostics"]][i, "max(Rhat)"]
    )
    
    modelsDiagnostics$addRows(tempRow)
  }
  
  diagnostics[["modelsDiagnostics"]] <- modelsDiagnostics
  
  return()
}
.robmaDiagnosticsPlots         <- function(jaspResults, options) {
  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 8
    diagnostics$dependOn(.robmaDependencies)
    jaspResults[["diagnostics"]] <- diagnostics
  } else{
    diagnostics <- jaspResults[["diagnostics"]]
  }
  
  
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  # select models to iterate over
  if (options[["diagnosticsSingle"]]) {
    modelsI <- options[["diagnosticsSingleModel"]]
    if (modelsI < 1 || modelsI > length(fit[["models"]])) {
      tempModel  <- createJaspContainer(title = gettextf("Model %i", modelsI))
      diagnostics[[paste0("model_", modelsI)]] <- tempModel
      tempError  <- createJaspPlot(title = "")
      tempError$dependOn("diagnosticsSingleModel", "diagnosticsSingle")
      tempError$setError(gettextf("Model %1$i does not exist. Select one of the models between 1 and %2$i.", modelsI, length(fit[["models"]])))
      tempModel[["tempError"]] <- tempError
      return()
    }
  } else{
    modelsI <- 1:length(fit[["models"]])
  }
  
  # collect the parameters
  parameters <- c(if (options[["diagnosticsMu"]])
    "mu",
    if (options[["diagnosticsTau"]])
      "tau",
    if (options[["diagnosticsOmega"]])
      "omega",
    if (options[["diagnosticsTheta"]])
      "theta")
  
  # do the iteration
  for (i in modelsI) {
    # create / access container for individual models
    if (is.null(diagnostics[[paste0("model_", i)]])) {
      tempModel <- createJaspContainer(title = gettextf("Model %i", i))
      tempModel$position <- i
      tempModel$dependOn(c("diagnosticsSingleModel", "diagnosticsSingle"))
      diagnostics[[paste0("model_", i)]] <- tempModel
    } else{
      tempModel <- diagnostics[[paste0("model_", i)]]
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
          "theta" = gettext("Random effects")
        ))
        tempPar$position <- switch(
          par,
          "mu"    = 1,
          "tau"   = 2,
          "omega" = 3,
          "theta" = 4
        )
        tempPar$dependOn(
          c(
            if (par == "mu")
              c("diagnosticsMu", "diagnosticsTransformed"),
            if (par == "tau")
              "diagnosticsTau",
            if (par == "omega")
              "diagnosticsOmega",
            if (par == "theta")
              "diagnosticsTheta"
          )
        )
        tempModel[[par]] <- tempPar
      } else{
        tempPar <- tempModel[[par]]
      }
      
      
      # add traceplots
      if (options[["diagnosticsTrace"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["trace"]])) {
          tempPlots <- createJaspContainer(gettext("Trace plots"))
          tempPlots$position <- 1
          tempPlots$dependOn("diagnosticsTrace")
          tempPar[["trace"]] <- tempPlots
        } else{
          tempPlots <- tempPar[["trace"]]
        }
        
        # create plots
        newPlots <- RoBMA::diagnostics(
          fit,
          parameter     = par,
          type          = "chains",
          plot_type     = "ggplot",
          show_models   = i,
          par_transform = options[["diagnosticsTransformed"]],
          title         = FALSE
        )
        
        if (is.null(newPlots))
          next
        noPars <- FALSE
        
        # add them to the container
        if (!ggplot2::is.ggplot(newPlots)) {
          for (pi in 1:length(newPlots)) {
            tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
            tempPlots[[paste0("trace_", pi)]] <- tempPlot
            tempPlot[["plotObject"]] <-
              jaspGraphs::themeJasp(newPlots[[pi]])
          }
          
        } else{
          tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
          tempPlots[[paste0("trace_", 1)]] <- tempPlot
          tempPlot[["plotObject"]] <- jaspGraphs::themeJasp(newPlots)
        }
        
      }
      
      
      # add autocorrelation plots
      if (options[["diagnosticsAutocorrelation"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["autocor"]])) {
          tempPlots <- createJaspContainer(gettext("Average autocorrelations"))
          tempPlots$position <- 2
          tempPlots$dependOn("diagnosticsAutocorrelation")
          tempPar[["autocor"]] <- tempPlots
        } else{
          tempPlots <- tempPar[["autocor"]]
        }
        
        # create plots
        newPlots <- RoBMA::diagnostics(
          fit,
          parameter     = par,
          type          = "autocorrelations",
          plot_type     = "ggplot",
          show_models   = i,
          par_transform = options[["diagnosticsTransformed"]],
          title         = FALSE
        )
        
        if (is.null(newPlots))
          next
        noPars <- FALSE
        
        # add them to the container
        if (!ggplot2::is.ggplot(newPlots)) {
          for (pi in 1:length(newPlots)) {
            tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
            tempPlots[[paste0("autocor_", pi)]] <- tempPlot
            tempPlot[["plotObject"]] <-
              jaspGraphs::themeJasp(newPlots[[pi]])
          }
          
        } else{
          tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
          tempPlots[[paste0("autocor_", 1)]] <- tempPlot
          tempPlot[["plotObject"]] <- jaspGraphs::themeJasp(newPlots)
        }
        
      }
      
      
      # add sample densities plots
      if (options[["diagnosticsSamples"]]) {
        # create / access container for trace plots
        if (is.null(tempPar[["samples"]])) {
          tempPlots <- createJaspContainer(gettext("Posterior samples densities"))
          tempPlots$position <- 3
          tempPlots$dependOn("diagnosticsSamples")
          tempPar[["samples"]] <- tempPlots
        } else{
          tempPlots <- tempPar[["samples"]]
        }
        
        # create plots
        newPlots <- RoBMA::diagnostics(
          fit,
          parameter     = par,
          type          = "densities",
          plot_type     = "ggplot",
          show_models   = i,
          par_transform = options[["diagnosticsTransformed"]],
          title         = FALSE
        )
        
        if (is.null(newPlots))
          next
        noPars <- FALSE
        
        # add them to the container
        if (!ggplot2::is.ggplot(newPlots)) {
          for (pi in 1:length(newPlots)) {
            tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
            tempPlots[[paste0("samples_", pi)]] <- tempPlot
            tempPlot[["plotObject"]] <-
              jaspGraphs::themeJasp(newPlots[[pi]])
          }
          
        } else{
          tempPlot  <- createJaspPlot(width = 400, aspectRatio = .7)
          tempPlots[[paste0("samples_", 1)]] <- tempPlot
          tempPlot[["plotObject"]] <- jaspGraphs::themeJasp(newPlots)
        }
        
      }
      
    }
    
    # show error if only one model is selected but doesn't contain any of the diagnostics
    if (noPars && options[["diagnosticsSingleModel"]]) {
      tempError  <- createJaspPlot(title = "")
      tempError$dependOn(
        c(
          "diagnosticsMu",
          "diagnosticsTransformed",
          "diagnosticsTau",
          "diagnosticsOmega",
          "diagnosticsTheta",
          "diagnosticsTrace",
          "diagnosticsAutocorrelation",
          "diagnosticsSamples"
        )
      )
      tempError$setError(gettextf("Model %i does not contain any of the selected parameters.", i))
      tempModel[["tempError"]] <- tempError
    }
  }
  
  return()
}
.robmaSaveModel                <- function(jaspResults, options) {
  if (is.null(jaspResults[["modelSaved"]])) {
    modelSaved <- createJaspState()
    modelSaved$dependOn(c(.robmaDependencies, "savePath"))
    jaspResults[["modelSaved"]] <- modelSaved
    
  }
  
  saveRDS(jaspResults[["model"]][["object"]], file = options[["savePath"]])
  
  modelSaved[["object"]] <- TRUE
  
}
