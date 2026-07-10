# Classical meta-analysis diagnostics.
#
# Computes and presents casewise, profile, Baujat, and variance-inflation diagnostics.

.maExtractVarianceInflationContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["varianceInflationContainer"]]))
    return(jaspResults[["varianceInflationContainer"]])

  # create the output container
  varianceInflationContainer <- createJaspContainer(gettext("Variance Inflation Summary"))
  varianceInflationContainer$dependOn(c(.maDependencies, "diagnosticsVarianceInflationFactor", "diagnosticsVarianceInflationFactorAggregate", "includeFullDatasetInSubgroupAnalysis"))
  varianceInflationContainer$position <- 7
  jaspResults[["varianceInflationContainer"]] <- varianceInflationContainer

  return(varianceInflationContainer)
}

.maDiagnostics                   <- function(jaspResults, options) {

  # extract precomputed diagnostics if done before:
  if (!is.null(jaspResults[["diagnosticsResults"]])) {

    out <- jaspResults[["diagnosticsResults"]]$object

  } else {

    # the fit diagnostics work only for the non-clustered fit
    fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)

    # create the output container
    diagnosticsResults <- createJaspState()
    diagnosticsResults$dependOn(.maDependencies)
    jaspResults[["diagnosticsResults"]] <- diagnosticsResults

    out <- list()

    for (i in seq_along(fit)) {

      if (jaspBase::isTryError(fit[[i]])) {
        influenceResultsDfbs <- list()
        influenceResultsInf  <- list()
      } else {
        if (.maIsMultilevelMultivariate(options)) {
          # only a subset of diagnostics is available for rma.mv
          influenceResultsDfbs <- data.frame(dfbetas(fit[[i]], code1 = "jaspBase::startProgressbar(x$k, label = 'Casewise diagnostics: DFBETAS')", code2 = "jaspBase::progressbarTick()"))
          influenceResultsInf  <- data.frame(
            rstudent = rstudent(fit[[i]],       code1 = "jaspBase::startProgressbar(x$k, label = 'Casewise diagnostics: Studentized residuals')", code2 = "jaspBase::progressbarTick()")[["resid"]],
            cook.d   = cooks.distance(fit[[i]], code1 = "jaspBase::startProgressbar(x$k, label = 'Casewise diagnostics: Cooks distance')",        code2 = "jaspBase::progressbarTick()"),
            hat      = hatvalues(fit[[i]])
          )
        } else if (.maIsMetaregressionHeterogeneity(options)) {
          influenceResultsDfbs <- data.frame()
          influenceResultsInf  <- data.frame(
            rstudent = stats::rstandard(fit[[i]])[["z"]],
            hat      = hatvalues(fit[[i]]),
            weight   = stats::weights(fit[[i]], type = "diagonal")
          )
        } else {
          # the complete suite of influence diagnostics is only available for rma.uni
          influenceResults     <- influence(fit[[i]], code1 = "jaspBase::startProgressbar(x$k, label = 'Casewise diagnostics')", code2 = "jaspBase::progressbarTick()")
          influenceResultsDfbs <- data.frame(influenceResults$dfbs)
          influenceResultsInf  <- data.frame(influenceResults$inf)
          influenceResultsInf$tau.del <- sqrt(influenceResultsInf$tau2.del)
          influenceResultsInf$inf[influenceResultsInf$inf == "*"] <- "Yes"
        }
      }

      out[[attr(fit[[i]], "subgroup")]] <- list(
        "influenceResultsDfbs" = influenceResultsDfbs,
        "influenceResultsInf"  = influenceResultsInf
      )
    }

    # store the results
    jaspResults[["diagnosticsResults"]]$object <- out
  }

  return(out)
}

.maProfile                       <- function(jaspResults, options) {

  # extract precomputed profile likelihood if done before:
  if (!is.null(jaspResults[["profileLikelihoodResults"]])) {

    out <- jaspResults[["profileLikelihoodResults"]]$object

  } else {

    # create the output container
    profileLikelihoodResults <- createJaspState()
    profileLikelihoodResults$dependOn(.maDependencies)
    jaspResults[["profileLikelihoodResults"]] <- profileLikelihoodResults


    fit <- .maExtractFit(jaspResults, options)
    out <- list()

    for (i in seq_along(fit)) {

      if (jaspBase::isTryError(fit[[i]])) {

        dfProfile <- list()

      } else if (.maIsMultilevelMultivariate(options)) {

        # use the defaults (too many possible parameter combinations to control)
        dfProfile <- try(metafor::profile.rma.mv(
          fit[[i]],
          plot    = FALSE,
          progbar = FALSE,
          code1   = "jaspBase::startProgressbar(length(vcs), label = 'Profile likelihood')",
          code2   = "jaspBase::progressbarTick()"
        ))

        # deal with a single component (not a list)
        if (!jaspBase::isTryError(dfProfile) && dfProfile[["comps"]] == 1) {
          dfProfile <- list(dfProfile)
          dfProfile[["comps"]] <- 1
        }

      } else {

        # proceed with some nice formatting for rma.uni (too difficult to implement for rma.mv)
        xTicks    <- jaspGraphs::getPrettyAxisBreaks(c(0, max(0.1, 2*fit[[i]][["tau2"]])))
        dfProfile <- try(profile(
          fit[[i]],
          xlim    = range(xTicks),
          plot    = FALSE,
          progbar = FALSE,
          code1   = "jaspBase::startProgressbar(length(vcs), label = 'Profile likelihood')",
          code2   = "jaspBase::progressbarTick()"
        ))
        attr(dfProfile, "xTicks")   <- xTicks
      }

      out[[attr(fit[[i]], "subgroup")]] <- dfProfile
    }

    jaspResults[["profileLikelihoodResults"]]$object <- out
  }


  return(out)
}

.maBaujat                        <- function(jaspResults, options) {

  # extract precomputed profile likelihood if done before:
  if (!is.null(jaspResults[["baujatResults"]])) {

    out <- jaspResults[["baujatResults"]]$object

  } else {

    # create the output container
    baujatResults <- createJaspState()
    baujatResults$dependOn(.maDependencies)
    jaspResults[["baujatResults"]] <- baujatResults


    baujatOptions <- options
    baujatOptions[["includeFullDatasetInSubgroupAnalysis"]] <- TRUE
    fit <- .maExtractFit(jaspResults, baujatOptions, nonClustered = TRUE)
    out <- list()

    for (i in seq_along(fit)) {

      if (jaspBase::isTryError(fit[[i]])) {
        dfBaujat <- list()
      } else {
        dfBaujat <- try(.maSuppressPlot(metafor::baujat(fit[[i]])))
      }

      out[[names(fit)[i]]] <- dfBaujat
    }

    jaspResults[["baujatResults"]]$object <- out
  }


  return(out)
}

.maVarianceInflationTable                <- function(jaspResults, options, parameter = "effectSize") {

  varianceInflationContainer <- .maExtractVarianceInflationContainer(jaspResults)

  if (!is.null(varianceInflationContainer[[parameter]]))
    return()

  if (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  termsTable <- createJaspTable(switch(
    parameter,
    effectSize    = gettext("Effect Size Meta-Regression Variance Inflation"),
    heterogeneity = gettext("Heterogeneity Meta-Regression Variance Inflation")
  ))
  termsTable$position <- switch(
    parameter,
    effectSize    = 1,
    heterogeneity = 2
  )
  varianceInflationContainer[[parameter]] <- termsTable

  termsTable$addColumnInfo(name = "term",  type = "string",  title = "")
  .maAddSubgroupColumn(termsTable, options)
  if (options[["diagnosticsVarianceInflationFactorAggregate"]])
    termsTable$addColumnInfo(name = "m", type = "integer", title = gettext("Parameters"))

  termsTable$addColumnInfo(name = "vif",  type = "number", title = gettext("VIF"))
  termsTable$addColumnInfo(name = "sif",  type = "number", title = gettext("SIF"))

  if (length(fit) == 1 && jaspBase::isTryError(fit[[1]]))
    return()

  terms <- .maSafeRbind(lapply(fit, .maComputeVifSummary, options = options, parameter = parameter))
  terms <- .maSafeOrderAndSimplify(terms, "term", options)

  termsTable$setData(terms)

  return()
}

.maCasewiseDiagnosticsTable              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["casewiseDiagnosticsTable"]]))
    return()

  # the fit diagnostics work only for the non-clustered fit
  fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # fit measures table
  casewiseDiagnosticsTable          <- createJaspTable(gettext("Casewise Diagnostics Table"))
  casewiseDiagnosticsTable$position <- 7
  casewiseDiagnosticsTable$dependOn(c(.maDependencies, "diagnosticsCasewiseDiagnostics", "diagnosticsCasewiseDiagnosticsShowInfluentialOnly",
                                      "diagnosticsCasewiseDiagnosticsIncludePredictors", "diagnosticsCasewiseDiagnosticsDifferenceInCoefficients",
                                      "studyLabels"))
  jaspResults[["casewiseDiagnosticsTable"]] <- casewiseDiagnosticsTable

  ### the computation needs to be done before the table to get all the necessary information on column names
  # export diagnostics
  diagnostics <- .maDiagnostics(jaspResults, options)

  # always drop the full from subgroups
  if (options[["subgroup"]] != "" && options[["includeFullDatasetInSubgroupAnalysis"]]) {
    fit         <- fit[-1]
    diagnostics <- diagnostics[-1]
  }

  diagnosticsTable <- .maSafeRbind(lapply(seq_along(fit), function(i) .maRowDiagnosticsTable(
    fit         = fit[[i]],
    diagnostics = diagnostics[[attr(fit[[i]], "subgroup")]],
    options     = options
  )))

  # table information
  predictorNames      <- if (options[["diagnosticsCasewiseDiagnosticsIncludePredictors"]]) paste0("pred", options[["predictors"]]) else character(0)
  coefDifferenceNames <- setdiff(colnames(diagnosticsTable), c("subgroup", "label", predictorNames, .maCasewiseDiagnosticsNames()))
  coefDifferenceNames <- if (is.null(coefDifferenceNames)) character(0) else coefDifferenceNames

  # prepare table
  .maAddSubgroupColumn(casewiseDiagnosticsTable, options)
  if (options[["studyLabels"]] != "") {
    casewiseDiagnosticsTable$addColumnInfo(name = "label", type  = "string", title = gettext("Label"))
  }
  if (options[["diagnosticsCasewiseDiagnosticsIncludePredictors"]]) {
    for (var in options[["predictors"]]) {
      casewiseDiagnosticsTable$addColumnInfo(name = paste0("pred", var), type  = .maGetVariableColumnType(var, options), title = var, overtitle = gettext("Predictor"))
    }
  }
  casewiseDiagnosticsTable$addColumnInfo(name = "rstudent",  title = gettext("Standardized Residual"),  type = "number")
  if (!.maIsMultilevelMultivariate(options) && !.maIsMetaregressionHeterogeneity(options))
    casewiseDiagnosticsTable$addColumnInfo(name = "dffits",  title = gettext("DFFITS"),                 type = "number")
  if (!.maIsMetaregressionHeterogeneity(options))
    casewiseDiagnosticsTable$addColumnInfo(name = "cook.d",  title = gettext("Cook's Distance"),        type = "number")
  if (!.maIsMultilevelMultivariate(options) && !.maIsMetaregressionHeterogeneity(options)) {
    casewiseDiagnosticsTable$addColumnInfo(name = "cov.r",   title = gettext("Covariance ratio"),       type = "number")
    casewiseDiagnosticsTable$addColumnInfo(name = "tau.del", title = gettext("\U1D70F"),                type = "number", overtitle = gettext("Leave One Out"))
    casewiseDiagnosticsTable$addColumnInfo(name = "tau2.del",title = gettext("\U1D70F\U00B2"),          type = "number", overtitle = gettext("Leave One Out"))
    casewiseDiagnosticsTable$addColumnInfo(name = "QE.del",  title = gettext("Q\U2091"),                type = "number", overtitle = gettext("Leave One Out"))
  }
  casewiseDiagnosticsTable$addColumnInfo(name = "hat",       title = gettext("Hat"),                    type = "number")
  if (!.maIsMultilevelMultivariate(options))
    casewiseDiagnosticsTable$addColumnInfo(name = "weight",  title = gettext("Weight"),                 type = "number")
  if (options[["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"]] && !.maIsMetaregressionHeterogeneity(options)) {
    for (par in coefDifferenceNames) {
      casewiseDiagnosticsTable$addColumnInfo(name = par, title = .maVariableNames(par, c(unlist(options[["effectSizeModelTerms"]]), unlist(options[["heterogeneityModelTerms"]]))),
                                             type = "number", overtitle = gettext("Difference in coefficients"))
    }
  }
  if (!.maIsMultilevelMultivariate(options) && !.maIsMetaregressionHeterogeneity(options))
    casewiseDiagnosticsTable$addColumnInfo(name = "inf", title = gettext("Influential"), type = "string")



  # keep influential only
  if (options[["diagnosticsCasewiseDiagnosticsShowInfluentialOnly"]] &&
      !.maIsMetaregressionHeterogeneity(options) &&
      !is.null(diagnosticsTable) &&
      "inf" %in% colnames(diagnosticsTable)) {

    diagnosticsTable <- diagnosticsTable[!is.na(diagnosticsTable[["inf"]]) ,,drop=FALSE]
    diagnosticsTable <- diagnosticsTable[diagnosticsTable[["inf"]] == "Yes",,drop=FALSE]

    # add note if some results are completly ommited
    if (options[["subgroup"]] == "" && nrow(diagnosticsTable) == 0) {
      casewiseDiagnosticsTable$addFootnote(gettext("No influential cases found."))
    } else if (options[["subgroup"]] != "") {
      influentialTable <- table(diagnosticsTable[["subgroup"]])
      for (i in seq_along(influentialTable)[influentialTable == 0]) {
        casewiseDiagnosticsTable$addFootnote(gettextf(
          "%1$sNo influential cases found.",
          if (options[["subgroup"]] != "") gettextf("Subgroup %1$s: ", attr(fit[[i]], "subgroup")) else ""
        ))
      }
    }
  }

  # simplify and store results
  diagnosticsTable <- .maSafeOrderAndSimplify(diagnosticsTable, "subgroup", options)
  casewiseDiagnosticsTable$setData(diagnosticsTable)

  if (options[["subgroup"]] != "")
    casewiseDiagnosticsTable$addFootnote(gettext("Diagnostics are based on the the subgroup models."))
  if (.maIsClustered(options))
    casewiseDiagnosticsTable$addFootnote(gettext("Diagnostics are based on the non-clustered model."))

  return()
}

.maComputeVifSummary               <- function(fit, options, parameter = "effectSize") {

  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  if (options[["diagnosticsVarianceInflationFactorAggregate"]]) {

    # obtain terms indicies
    if (parameter == "effectSize") {
      terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")
      termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")
      tableVif   <- do.call(rbind, lapply(seq_along(terms), function(i) {
        cbind.data.frame(
          term = terms[i],
          .maExtractVifResults(try(metafor::vif(fit, btt = seq_along(termsIndex)[termsIndex == i])), options, parameter)
        )
      }))
    } else if (parameter == "heterogeneity") {
      terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")
      termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")
      tableVif   <- do.call(rbind, lapply(seq_along(terms), function(i) {
        cbind.data.frame(
          term = terms[i],
          .maExtractVifResults(try(metafor::vif(fit, att = seq_along(termsIndex)[termsIndex == i])), options, parameter)
        )
      }))
    }

  } else {

    tableVif      <- .maExtractVifResults(try(metafor::vif(fit)), options, parameter)
    tableVif$term <- .maVariableNames(rownames(tableVif), c(unlist(options[["effectSizeModelTerms"]]), unlist(options[["heterogeneityModelTerms"]])))
  }

  tableVif$subgroup <- attr(fit, "subgroup")

  return(tableVif)
}

.maRowDiagnosticsTable                <- function(fit, diagnostics, options, forExport = FALSE) {

  # first create the data part of the output
  # (in case the fit failed there are no diagnostics)

  fitData <- attr(fit, "data")
  rows    <- list()

  # add export specific settings (for adding to the dataset)
  if (forExport) {
    rows[["datasetOrder"]] <- as.numeric(names(attr(fitData, "NasIds"))[!attr(fitData, "NasIds")])

    # no variable information necessary
    options[["studyLabels"]] <- ""
    options[["diagnosticsCasewiseDiagnosticsIncludePredictors"]] <- FALSE
  }

  # include study labels
  if (options[["studyLabels"]] != "") {
    rows[["label"]] <- fitData[[options[["studyLabels"]]]]
  }

  # include predictors
  if (options[["diagnosticsCasewiseDiagnosticsIncludePredictors"]]) {
    for (var in intersect(options[["predictors"]], colnames(fitData)))
      rows[[paste0("pred", var)]] <- fitData[[var]]
  }

  # return on error
  if (jaspBase::isTryError(fit)) {
    return(do.call(cbind.data.frame, rows))
  }

  # main diagnostics section
  rows <- do.call(cbind.data.frame, c(
    rows,
    diagnostics[["influenceResultsInf"]],
    if (options[["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"]] && !.maIsMetaregressionHeterogeneity(options)) diagnostics[["influenceResultsDfbs"]]
  ))
  rows$subgroup <- attr(fit, "subgroup")

  return(rows)
}

.maExtractVifResults                  <- function(vifResults, options, parameter) {

  if (jaspBase::isTryError(vifResults)) {
    if (options[["diagnosticsVarianceInflationFactorAggregate"]]) {
      return(data.frame(
        m   = NA,
        vif = NA,
        sif = NA
      ))
    } else {
      return(data.frame(
        vif = NA,
        sif = NA
      ))
    }
  }

  if (.maIsMetaregressionHeterogeneity(options))
    vifResults <- vifResults[[switch(
      parameter,
      "effectSize"    = "beta",
      "heterogeneity" = "alpha"
    )]]

  vifResults <- data.frame(vifResults)

  if (options[["diagnosticsVarianceInflationFactorAggregate"]])
    vifResults <- vifResults[,c("m", "vif", "sif"),drop = FALSE]
  else
    vifResults <- vifResults[,c("vif", "sif"),drop = FALSE]

  return(vifResults)
}
