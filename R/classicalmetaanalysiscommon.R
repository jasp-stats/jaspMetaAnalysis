#
# Copyright (C) 2013-2018 University of Amsterdam
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

# This analysis runs
# - classical meta-analysis (using rma.uni)
# - classical multilevel/multivariate meta-analysis (using rma.mv; custom function prefix .mamm)
# - classical binimal meta-analysis (using rma.; custom function prefix .mab)


# TODO:
# Estimated Marginal Means
# - add variable interactions
# - specify and test contrasts
# Bubble plot
# - binning of continuous covariates (requires returning continous levels returned in gridMatrix)
# - allow factors as dependent variables
# AIC/BIC Model-averaging
# Diagnostics
# - model re-run on presence of influential cases
# Generic
# - allow different covariates factoring across all settings

# TODO fix QML
# - remove selected variables in estimated marginal means when removed from the model components
# - check that the variables types are always propagated throughout the options

.ClassicalMetaAnalysisCommon <- function(jaspResults, dataset, options, ...) {

  # fit the model
  .maFitModel(jaspResults, dataset, options)

  # # remove influential observations and refit the model if requested
  # if (options[["diagnosticsCasewiseDiagnostics"]] && options[["diagnosticsCasewiseDiagnosticsRerunWithoutInfluentialCases"]]) {
  #   dataset <- .maRemoveInfluentialObservations(jaspResults, dataset, options)
  #   .maFitModel(jaspResults, dataset, options, objectName = "fitNoInfluence")
  # }

  # model summary
  .maResidualHeterogeneityTable(jaspResults, dataset, options)
  .maModeratorsTable(jaspResults, dataset, options)
  .maPooledEstimatesTable(jaspResults, dataset, options)
  if (.maIsMultilevelMultivariate(options))
    .mammRandomEstimatesTable(jaspResults, dataset, options)

  if (options[["fitMeasures"]])
    .maFitMeasuresTable(jaspResults, dataset, options)

  # meta-regression tables
  if (.maIsMetaregression(options)) {
    if (options[["metaregressionTermTests"]]) {
      .maTermsTable(jaspResults, dataset, options, "effectSize")
      .maTermsTable(jaspResults, dataset, options, "heterogeneity")
    }
    if (options[["metaregressionCoefficientEstimates"]]) {
      .maCoefficientEstimatesTable(jaspResults, dataset, options, "effectSize")
      .maCoefficientEstimatesTable(jaspResults, dataset, options, "heterogeneity")
    }
    if (options[["metaregressionCoefficientCorrelationMatrix"]]) {
      .maCoefficientCorrelationMatrixTable(jaspResults, dataset, options, "effectSize")
      .maCoefficientCorrelationMatrixTable(jaspResults, dataset, options, "heterogeneity")
    }
  }

  # estimated marginal means
  .maEstimatedMarginalMeansTable(jaspResults, dataset, options, "effectSize")
  .maEstimatedMarginalMeansTable(jaspResults, dataset, options, "heterogeneity")

  # plots
  .maUltimateForestPlot(jaspResults, dataset, options)
  .maBubblePlot(jaspResults, dataset, options)

  # diagnostics
  if (.maIsMetaregression(options) && options[["diagnosticsVarianceInflationFactor"]]) {
    .maVarianceInflationTable(jaspResults, dataset, options, "effectSize")
    .maVarianceInflationTable(jaspResults, dataset, options, "heterogeneity")
  }
  if (options[["diagnosticsCasewiseDiagnostics"]]) {
    .maCasewiseDiagnosticsTable(jaspResults, dataset, options)
    .maCasewiseDiagnosticsExportColumns(jaspResults, dataset, options)
  }
  if (options[["diagnosticsPlotsProfileLikelihood"]])
    .maProfileLikelihoodPlot(jaspResults, dataset, options)
  if (options[["diagnosticsPlotsBaujat"]])
    .maBaujatPlot(jaspResults, dataset, options)


  # additional
  if (options[["showMetaforRCode"]])
    .maShowMetaforRCode(jaspResults, options)

  return()
}

# fitting functions
.maGetFormula                    <- function(modelTerms, includeIntercept) {

  predictors <- unlist(lapply(modelTerms, function(x) {
    if (length(x[["components"]]) > 1)
      return(paste(x[["components"]], collapse = ":"))
    else
      return(x[["components"]])
  }))

  if (length(predictors) == 0)
    return(NULL)

  if (includeIntercept)
    formula <- paste("~", paste(predictors, collapse = "+"))
  else
    formula <- paste("~", paste(predictors, collapse = "+"), "-1")

  return(as.formula(formula, env = parent.frame(1)))
}
.maFitModel                      <- function(jaspResults, dataset, options, objectName = "fit") {
  # --------------------------------------------------------------------------- #
  # when updating don't forget to update the '.maMakeMetaforCallText' function! #
  # --------------------------------------------------------------------------- #
  if (!.maReady(options) || !is.null(jaspResults[[objectName]]))
    return()

  # create the output container
  fitContainer <- createJaspState()
  fitContainer$dependOn(.maDependencies)
  jaspResults[[objectName]] <- fitContainer

  # specify the effect size and outcome
  if (options[["module"]] == "metaAnalysis") {
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      sei  = as.name(options[["effectSizeStandardError"]]),
      data = dataset
    )
  } else if (options[["module"]] == "metaAnalysisMultilevelMultivariate") {
    # TODO: extend to covariance matrices
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      V    = as.name("samplingVariance"), # precomputed on data load
      data = dataset
    )
  }

  # add formulas if specified
  rmaInput$mods  <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  rmaInput$scale <- .maGetFormula(options[["heterogeneityModelTerms"]], options[["heterogeneityModelIncludeIntercept"]])

  # add random effects
  if (.maIsMultilevelMultivariate(options)) {
    randomFormulaList <- .mammGetRandomFormulaList(options)
    if (length(randomFormulaList) != 0) {
      rmaInput$random <- randomFormulaList
      rmaInput$struct <- do.call(c, lapply(randomFormulaList, attr, which = "structure"))
    }
  }

  # specify method and fixed effect terms test
  rmaInput$method <- .maGetMethodOptions(options)
  rmaInput$test   <- options[["fixedEffectTest"]]

  if (!options[["weightedEstimation"]])
    rmaInput$weighted <- FALSE

  # add fixed parameters if needed
  if (options[["fixParametersWeights"]])
    rmaInput$weights <- as.name(options[["fixParametersWeightsVariable"]])
  if (options[["fixParametersTau2"]])
    rmaInput$tau2 <- .maGetFixedTau2Options(options) # TODO: add multiple possible fixed taus

  # add link function if needed
  if (.maIsMetaregressionHeterogeneity(options))
    rmaInput$link <- options[["heterogeneityModelLink"]]

  if (.maIsMultilevelMultivariate(options)) {
    rmaInput$sparse <- if (options[["useSparseMatricies"]])       options[["useSparseMatricies"]]
    rmaInput$cvvc   <- if (!options[["computeCovarianceMatrix"]]) !options[["computeCovarianceMatrix"]]
  }

  # add control options if needed
  control <- .maGetControlOptions(options)
  if (length(control) != 0)
    rmaInput$control <- control

  # additional input
  rmaInput$level <- 100 * options[["confidenceIntervalsLevel"]]

  ### fit the model
  if (options[["module"]] == "metaAnalysis") {
    fit <- try(do.call(metafor::rma, rmaInput))
  } else if (options[["module"]] == "metaAnalysisMultilevelMultivariate") {
    fit <- try(do.call(metafor::rma.mv, rmaInput))
  }


  # add clustering if specified
  if (options[["clustering"]] != "") {
    fitClustered <- try(metafor::robust(
      fit,
      cluster      = dataset[[options[["clustering"]]]],
      clubSandwich = options[["clusteringUseClubSandwich"]],
      adjust       = options[["clusteringSmallSampleCorrection"]]
    ))
  } else {
    fitClustered <- NULL
  }


  # add information about dropped levels to the fit
  if (.maIsMultilevelMultivariate(options)) {
    attr(fit, "skipped") <- attr(randomFormulaList, "skipped")
    if (options[["clustering"]] != "") {
      attr(fitClustered, "skipped") <- attr(randomFormulaList, "skipped")
    }
  }


  # return the results
  jaspResults[[objectName]]$object <- list(
    fit          = fit,
    fitClustered = fitClustered
  )
  saveRDS(fit, file = "C:/JASP/fit.RDS")
  return()
}
.maRemoveInfluentialObservations <- function(jaspResults, dataset, options) {

  if (!.maReady(options) || !is.null(jaspResults[["fit"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  if (jaspBase::isTryError(fit))
    return()

  # remove influential observations
  influenceResults       <- influence.rma.uni(fit)
  influentialObservation <- influenceResults$inf$inf == "*"

  dataset <- dataset[!influentialObservation, ]
  attr(dataset, "influentialObservations") <- sum(influentialObservation)

  if (nrow(dataset) == 0)
    return(.quitAnalysis(gettext("All observations were removed as influential.")))

  return(dataset)
}

# output tables
.maResidualHeterogeneityTable         <- function(jaspResults, dataset, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["residualHeterogeneityTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # residual heterogeneity table
  residualHeterogeneityTable          <- createJaspTable(gettext("Residual Heterogeneity Test"))
  residualHeterogeneityTable$position <- 1
  modelSummaryContainer[["residualHeterogeneityTable"]] <- residualHeterogeneityTable

  residualHeterogeneityTable$addColumnInfo(name = "qstat", type = "number",  title = gettext("Q\U2091"))
  residualHeterogeneityTable$addColumnInfo(name = "df",    type = "integer", title = gettext("df"))
  residualHeterogeneityTable$addColumnInfo(name = "pval",  type = "pvalue",  title = gettext("p"))

  # stop and display errors
  if (is.null(fit))
    return()

  if (!is.null(.maCheckIsPossibleOptions(options))) {
    residualHeterogeneityTable$setError(.maCheckIsPossibleOptions(options))
    return()
  }

  if (jaspBase::isTryError(fit)) {
    residualHeterogeneityTable$setError(fit)
    return()
  }

  # residual heterogeneity
  residualHeterogeneityTable$addRows(list(
    qstat = fit[["QE"]],
    df    = fit[["k"]] - fit[["p"]],
    pval  = fit[["QEp"]]
  ))

  return()
}
.maModeratorsTable                    <- function(jaspResults, dataset, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["moderatorsTable"]]))
    return()

  if (!.maIsMetaregression(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # omnibus moderator table
  moderatorsTable          <- createJaspTable(gettext("Omnibus Moderation Test"))
  moderatorsTable$position <- 2
  moderatorsTable$dependOn(c("addOmnibusModeratorTestEffectSizeCoefficients", "addOmnibusModeratorTestEffectSizeCoefficientsValues",
                             "addOmnibusModeratorTestHeterogeneityCoefficients", "addOmnibusModeratorTestHeterogeneityCoefficientsValues"))
  modelSummaryContainer[["moderatorsTable"]] <- moderatorsTable

  moderatorsTable$addColumnInfo(name = "parameter", type = "string",  title = gettext("Parameter"))
  moderatorsTable$addColumnInfo(name = "stat", type = "number",   title = if(.maIsMetaregressionFtest(options)) gettext("F")   else gettext("Q\U2098"))
  moderatorsTable$addColumnInfo(name = "df1",  type = "integer",  title = if(.maIsMetaregressionFtest(options)) gettext("df\U2081") else gettext("df"))
  if (.maIsMetaregressionFtest(options))
    moderatorsTable$addColumnInfo(name = "df2", type = "number", title = gettext("df\U2082"))
  moderatorsTable$addColumnInfo(name = "pval",  type = "pvalue",  title = gettext("p"))

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # effect size moderation
  if (.maIsMetaregression(options)) {

    testEffectSize <- .maOmnibusTest(fit, options, parameter = "effectSize")
    moderatorsTable$addRows(testEffectSize)

    if (options[["addOmnibusModeratorTestEffectSizeCoefficients"]]) {
      testEffectSizeCoefficients <- .maOmnibusTestCoefficients(fit, options, parameter = "effectSize")
      if (length(testEffectSizeCoefficients) == 1) {
        moderatorsTable$setError(testEffectSizeCoefficients)
        return()
      } else {
        moderatorsTable$addRows(testEffectSizeCoefficients)
      }
    }
  }

  # heterogeneity moderation
  if (.maIsMetaregressionHeterogeneity(options)) {

    testHeterogeneity <- .maOmnibusTest(fit, options, parameter = "heterogeneity")
    moderatorsTable$addRows(testHeterogeneity)

    if (options[["addOmnibusModeratorTestHeterogeneityCoefficients"]]) {
      testHeterogeneityCoefficients <- .maOmnibusTestCoefficients(fit, options, parameter = "heterogeneity")
      if (length(testHeterogeneityCoefficients) == 1) {
        moderatorsTable$setError(testHeterogeneityCoefficients)
        return()
      } else {
        moderatorsTable$addRows(testHeterogeneityCoefficients)
      }
    }
  }

  return()
}
.maPooledEstimatesTable               <- function(jaspResults, dataset, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["pooledEstimatesTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # pooled estimates
  pooledEstimatesTable          <- createJaspTable(gettext("Pooled Estimates"))
  pooledEstimatesTable$position <- 4
  pooledEstimatesTable$dependOn(c("heterogeneityTau", "heterogeneityTau2", "heterogeneityI2", "heterogeneityH2",
                                  "confidenceIntervals", "confidenceIntervalsLevel", "predictionIntervals", "transformEffectSize"))
  modelSummaryContainer[["pooledEstimatesTable"]] <- pooledEstimatesTable

  pooledEstimatesTable$addColumnInfo(name = "par",  type = "string", title = "")
  pooledEstimatesTable$addColumnInfo(name = "est",  type = "number", title = gettext("Estimate"))
  if (options[["confidenceIntervals"]]) {
    overtitleCi <- gettextf("%s%% CI", 100 * options[["confidenceIntervalsLevel"]])
    pooledEstimatesTable$addColumnInfo(name = "lCi", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    pooledEstimatesTable$addColumnInfo(name = "uCi", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }
  if (options[["predictionIntervals"]]) {
    overtitleCi <- gettextf("%s%% PI", 100 * options[["confidenceIntervalsLevel"]])
    pooledEstimatesTable$addColumnInfo(name = "lPi", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    pooledEstimatesTable$addColumnInfo(name = "uPi", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }


  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # pooled effect size
  pooledEffect <- .maComputePooledEffect(fit, options)
  pooledEstimatesTable$addRows(pooledEffect)

  # pooled heterogeneity
  if (!.maGetMethodOptions(options) %in% c("EE", "FE") && !.maIsMultilevelMultivariate(options)) {

    # requires non-clustered fit
    pooledHeterogeneity <- .maComputePooledHeterogeneity(.maExtractFit(jaspResults, options, nonClustered = TRUE), options)

    if (nrow(pooledHeterogeneity) > 0) {
      for (i in 1:nrow(pooledHeterogeneity))
        pooledEstimatesTable$addRows(pooledHeterogeneity[i,])
    }
  }

  # add messages
  pooledEstimatesMessages <- .maPooledEstimatesMessages(fit, dataset, options)
  for (i in seq_along(pooledEstimatesMessages))
    pooledEstimatesTable$addFootnote(pooledEstimatesMessages[i])

  return()
}
.maFitMeasuresTable                   <- function(jaspResults, dataset, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["fitMeasuresTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # fit measures table
  fitMeasuresTable          <- createJaspTable(gettext("Fit Measures"))
  fitMeasuresTable$position <- 4
  fitMeasuresTable$dependOn(c(.maDependencies, "fitMeasures"))
  modelSummaryContainer[["fitMeasuresTable"]] <- fitMeasuresTable


  fitMeasuresTable$addColumnInfo(name = "model", title = "",                  type = "string")
  fitMeasuresTable$addColumnInfo(name = "ll",    title = gettext("Log Lik."), type = "number")
  fitMeasuresTable$addColumnInfo(name = "dev",   title = gettext("Deviance"), type = "number")
  fitMeasuresTable$addColumnInfo(name = "AIC",   title = gettext("AIC"),      type = "number")
  fitMeasuresTable$addColumnInfo(name = "BIC",   title = gettext("BIC"),      type = "number")
  fitMeasuresTable$addColumnInfo(name = "AICc",  title = gettext("AICc"),     type = "number")

  if (.maIsMetaregressionEffectSize(options) && !.maIsMultilevelMultivariate(options))
    fitMeasuresTable$addColumnInfo(name = "R2",  title = gettext("R\U00B2"),   type = "number")

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  fitSummary <- cbind("model" = colnames(fit[["fit.stats"]]), data.frame(t(fit[["fit.stats"]])))

  if (.maIsMetaregressionEffectSize(options) && !.maIsMultilevelMultivariate(options))
    fitSummary$R2 <- fit[["R2"]]

  fitMeasuresTable$setData(fitSummary)

  return()
}
.maTermsTable                         <- function(jaspResults, dataset, options, parameter = "effectSize") {

  metaregressionContainer <- .maExtractMetaregressionContainer(jaspResults)

  if (!is.null(metaregressionContainer[[paste0(parameter, "TermsTable")]]))
    return()

  if (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  termsTable <- createJaspTable(switch(
    parameter,
    effectSize    = gettext("Effect Size Meta-Regression Terms Tests"),
    heterogeneity = gettext("Heterogeneity Meta-Regression Terms Tests")
  ))
  termsTable$position <- switch(
    parameter,
    effectSize    = 1,
    heterogeneity = 2
  )
  termsTable$dependOn("metaregressionTermTests")
  metaregressionContainer[[paste0(parameter, "TermsTable")]] <- termsTable

  termsTable$addColumnInfo(name = "term",  type = "string",  title = "")
  termsTable$addColumnInfo(name = "stat",  type = "number",  title = if(.maIsMetaregressionFtest(options)) gettext("F")   else gettext("Q\U2098"))
  termsTable$addColumnInfo(name = "df1",   type = "integer", title = if(.maIsMetaregressionFtest(options)) gettext("df\U2081") else gettext("df"))
  if (.maIsMetaregressionFtest(options)) {
    termsTable$addColumnInfo(name = "df2", type = "number", title = gettext("df\U2082"))
  }
  termsTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))
  termsTable$addFootnote(.maFixedEffectTextMessage(options))

  if (is.null(fit) || jaspBase::isTryError(fit))
    return()

  if (parameter == "effectSize") {

    if (!.maIsMetaregressionEffectSize(options))
      return()

    terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")
    termsTests <- do.call(rbind.data.frame, lapply(terms, function(term)
      .maTermTests(fit, options, term, parameter = "effectSize")
    ))
    termsTable$setData(termsTests)

  } else if (parameter == "heterogeneity") {

    if (!.maIsMetaregressionHeterogeneity(options))
      return()

    terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")
    termsTests <- do.call(rbind.data.frame, lapply(terms, function(term)
      .maTermTests(fit, options, term, parameter = "heterogeneity")
    ))
    termsTable$setData(termsTests)

  }

  return()
}
.maCoefficientEstimatesTable          <- function(jaspResults, dataset, options, parameter = "effectSize") {

  metaregressionContainer <- .maExtractMetaregressionContainer(jaspResults)

  if (!is.null(metaregressionContainer[[paste0(parameter, "CoefficientTable")]]))
    return()

  if (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  coefficientsTable <- createJaspTable(switch(
    parameter,
    effectSize    = gettext("Effect Size Meta-Regression Coefficients"),
    heterogeneity = gettext("Heterogeneity Meta-Regression Coefficients")
  ))
  coefficientsTable$position <- switch(
    parameter,
    effectSize    = 3,
    heterogeneity = 4
  )
  coefficientsTable$dependOn(c("metaregressionCoefficientEstimates", "confidenceIntervals"))
  metaregressionContainer[[paste0(parameter, "CoefficientTable")]] <- coefficientsTable

  coefficientsTable$addColumnInfo(name = "name",  type = "string", title = "")
  coefficientsTable$addColumnInfo(name = "est",   type = "number", title = gettext("Estimate"))
  coefficientsTable$addColumnInfo(name = "se",    type = "number", title = gettext("Standard Error"))
  coefficientsTable$addColumnInfo(name = "stat",  type = "number", title = if(.maIsMetaregressionFtest(options)) gettext("t") else gettext("z"))
  if (.maIsMetaregressionFtest(options))
    coefficientsTable$addColumnInfo(name = "df",  type = "number", title = gettext("df"))
  coefficientsTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))

  if (options[["confidenceIntervals"]]) {
    overtitleCi <- gettextf("%s%% CI", 100 * options[["confidenceIntervalsLevel"]])
    coefficientsTable$addColumnInfo(name = "lCi", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    coefficientsTable$addColumnInfo(name = "uCi", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }

  coefficientsTable$addFootnote(.maFixedEffectTextMessage(options))

  if (is.null(fit) || jaspBase::isTryError(fit))
    return()

  if (parameter == "effectSize") {

    estimates <- data.frame(
      name = .maVariableNames(rownames(fit[["beta"]]), options[["predictors"]]),
      est  = fit[["beta"]][,1],
      se   = fit[["se"]],
      stat = fit[["zval"]],
      pval = fit[["pval"]]
    )

    if (.maIsMetaregressionFtest(options))
      estimates$df <- fit[["ddf"]]

    if (options[["confidenceIntervals"]]) {
      estimates$lCi <- fit[["ci.lb"]]
      estimates$uCi <- fit[["ci.ub"]]
    }

    coefficientsTable$setData(estimates)

  } else if (parameter == "heterogeneity") {

    estimates <- data.frame(
      name = .maVariableNames(rownames(fit[["alpha"]]), options[["predictors"]]),
      est  = fit[["alpha"]][,1],
      se   = fit[["se.alpha"]],
      stat = fit[["zval.alpha"]],
      pval = fit[["pval.alpha"]]
    )

    if (.maIsMetaregressionFtest(options))
      estimates$df <- fit[["ddf.alpha"]]

    if (options[["confidenceIntervals"]]) {
      estimates$lCi <- fit[["ci.lb.alpha"]]
      estimates$uCi <- fit[["ci.ub.alpha"]]
    }

    coefficientsTable$setData(estimates)

  }

  if (parameter == "heterogeneity")
    coefficientsTable$addFootnote(.meMetaregressionHeterogeneityMessages(options))

  return()
}
.maCoefficientCorrelationMatrixTable  <- function(jaspResults, dataset, options, parameter = "effectSize") {

  metaregressionContainer <- .maExtractMetaregressionContainer(jaspResults)

  if (!is.null(metaregressionContainer[[paste0(parameter, "CorrelationTable")]]))
    return()

  if (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  correlationMatrixTable <- createJaspTable(switch(
    parameter,
    effectSize    = gettext("Effect Size Meta-Regression Correlation Matrix"),
    heterogeneity = gettext("Heterogeneity Meta-Regression Correlation Matrix")
  ))
  correlationMatrixTable$position <- switch(
    parameter,
    effectSize    = 5,
    heterogeneity = 6
  )
  correlationMatrixTable$dependOn("metaregressionCoefficientCorrelationMatrix")
  metaregressionContainer[[paste0(parameter, "CorrelationTable")]] <- correlationMatrixTable


  if (is.null(fit) || jaspBase::isTryError(fit))
    return()

  if (parameter == "effectSize")
    correlationMatrix <- data.frame(cov2cor(fit[["vb"]]))
  else if (parameter == "heterogeneity")
    correlationMatrix <- data.frame(cov2cor(fit[["va"]]))

  correlationMatrixNames      <- .maVariableNames(colnames(correlationMatrix), options[["predictors"]])
  colnames(correlationMatrix) <- correlationMatrixNames
  correlationMatrix$name      <- correlationMatrixNames

  correlationMatrixTable$addColumnInfo(name = "name", type = "string", title = "")
  for (correlationMatrixName in correlationMatrixNames)
    correlationMatrixTable$addColumnInfo(name = correlationMatrixName, type = "number")

  correlationMatrixTable$setData(correlationMatrix)

  return()
}
.maEstimatedMarginalMeansTable        <- function(jaspResults, dataset, options, parameter = "effectSize") {

  estimatedMarginalMeansContainer <- .maExtractEstimatedMarginalMeansContainer(jaspResults)

  if (!is.null(estimatedMarginalMeansContainer[[parameter]]))
    return()

  if (parameter == "effectSize" && length(options[["estimatedMarginalMeansEffectSizeSelectedVariables"]]) == 0)
    return()
  if (parameter == "heterogeneity" && length(options[["estimatedMarginalMeansHeterogeneitySelectedVariables"]]) == 0)
    return()

  fit <- .maExtractFit(jaspResults, options)

  estimatedMarginalMeansTable <- createJaspTable(switch(
    parameter,
    effectSize    = gettext("Estimated Marginal Means: Effect Size"),
    heterogeneity = gettext("Estimated Marginal Means: Heterogeneity")
  ))
  estimatedMarginalMeansTable$position <- switch(
    parameter,
    effectSize    = 1,
    heterogeneity = 2
  )
  estimatedMarginalMeansTable$dependOn(switch(
    parameter,
    effectSize    = c("estimatedMarginalMeansEffectSizeSelectedVariables", "transformEffectSize", "estimatedMarginalMeansEffectSizeSdFactorCovariates",
                      "estimatedMarginalMeansEffectSizeTestAgainst", "estimatedMarginalMeansEffectSizeTestAgainstValue", "predictionIntervals",
                      "estimatedMarginalMeansEffectSizeAddAdjustedEstimate"),
    heterogeneity = c("estimatedMarginalMeansHeterogeneitySelectedVariables", "estimatedMarginalMeansHeterogeneityTransformation", "estimatedMarginalMeansHeterogeneitySdFactorCovariates",
                      "estimatedMarginalMeansHeterogeneityAddAdjustedEstimate")
  ))
  estimatedMarginalMeansContainer[[parameter]] <- estimatedMarginalMeansTable


  if (is.null(fit) || jaspBase::isTryError(fit))
    return()


  estimatedMarginalMeansTable$addColumnInfo(name = "variable",  type = "string", title = gettext("Variable"))
  estimatedMarginalMeansTable$addColumnInfo(name = "value",     type = "string", title = gettext("Level"))
  estimatedMarginalMeansTable$addColumnInfo(name = "est",       type = "number", title = gettext("Estimate"))

  if (options[["confidenceIntervals"]]) {
    overtitleCi <- gettextf("%s%% CI", 100 * options[["confidenceIntervalsLevel"]])
    estimatedMarginalMeansTable$addColumnInfo(name = "lCi", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    estimatedMarginalMeansTable$addColumnInfo(name = "uCi", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }

  if (parameter == "effectSize") {

    if (options[["predictionIntervals"]]) {
      overtitleCi <- gettextf("%s%% PI", 100 * options[["confidenceIntervalsLevel"]])
      estimatedMarginalMeansTable$addColumnInfo(name = "lPi", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
      estimatedMarginalMeansTable$addColumnInfo(name = "uPi", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
    }

    if (options[["estimatedMarginalMeansEffectSizeTestAgainst"]]) {
      estimatedMarginalMeansTable$addColumnInfo(name = "stat",  type = "number", title = if(.maIsMetaregressionFtest(options)) gettext("t") else gettext("z"))
      if (.maIsMetaregressionFtest(options))
        estimatedMarginalMeansTable$addColumnInfo(name = "df",  type = "number", title = gettext("df"))

      estimatedMarginalMeansTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))
    }
  }


  selectedVariables <- switch(
    parameter,
    effectSize    = c(
      if (options[["estimatedMarginalMeansEffectSizeAddAdjustedEstimate"]]) "",
      unlist(options[["estimatedMarginalMeansEffectSizeSelectedVariables"]])
    ),
    heterogeneity = c(
      if (options[["estimatedMarginalMeansHeterogeneityAddAdjustedEstimate"]]) "",
      unlist(options[["estimatedMarginalMeansHeterogeneitySelectedVariables"]])
    )
  )
  estimatedMarginalMeans <- do.call(rbind, lapply(selectedVariables, function(selectedVariable)
    .maComputeMarginalMeansVariable(fit, options, dataset, selectedVariable, options[["estimatedMarginalMeansEffectSizeTestAgainstValue"]], parameter)))

  # drop non-required columns
  if (parameter == "effectSize" && !options[["estimatedMarginalMeansEffectSizeTestAgainst"]])
    estimatedMarginalMeans <- estimatedMarginalMeans[,!colnames(estimatedMarginalMeans) %in% c("df", "stat", "pval")]

  estimatedMarginalMeansTable$setData(estimatedMarginalMeans)

  estimatedMarginalMeansMessages <- .maEstimatedMarginalMeansMessages(options, parameter)
  for (i in seq_along(estimatedMarginalMeansMessages))
    estimatedMarginalMeansTable$addFootnote(estimatedMarginalMeansMessages[i])

  return()
}
.maUltimateForestPlot                 <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["forestPlot"]]))
    return()

  if (!any(c(
    options[["forestPlotStudyInformation"]],
    (options[["forestPlotEstimatedMarginalMeans"]] && (
      length(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]]) > 0 ||
      options[["forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"]]
    )),
    options[["forestPlotModelInformation"]]
  )))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # try execute!
  plotOut <- try(.maMakeTheUltimateForestPlot(fit, dataset, options))

  if (inherits(plotOut, "try-error")) {
    forestPlot <- createJaspPlot(title = gettext("Forest Plot"))
    forestPlot$position <- 4
    forestPlot$dependOn(.maForestPlotDependencies)
    forestPlot$setError(plotOut)
    jaspResults[["forestPlot"]] <- forestPlot
    return()
  }

  # try adjusting height and width
  height <- 200 + (attr(plotOut, "rows")) * 10
  if (!attr(plotOut, "isPanel"))
    width <- 500
  else
    width <- 500 + 500 * attr(plotOut, "panelRatio")

  forestPlot <- createJaspPlot(
    title  = gettext("Forest Plot"),
    width  = width,
    height = height
  )
  forestPlot$position <- 5
  forestPlot$dependOn(.maForestPlotDependencies)

  if (!attr(plotOut, "isPanel")) {
    forestPlot$plotObject <- plotOut
  } else {
    plotOut <- jaspGraphs:::jaspGraphsPlot$new(
      subplots = plotOut,
      layout   = attr(plotOut, "layout"),
      heights  = 1,
      widths   = attr(plotOut, "widths")
    )
    forestPlot$plotObject <- plotOut
  }

  jaspResults[["forestPlot"]] <- forestPlot


  return()
}
.maBubblePlot                         <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["bubblePlot"]]))
    return()

  if (length(options[["bubblePlotSelectedVariable"]]) == 0)
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # set dimensions
  width  <- if (length(options[["bubblePlotSeparateLines"]]) == 0 || options[["bubblePlotLegendPosition"]] == "none") 450 else 550
  height <- 350

  # create containers / figure
  if (length(options[["bubblePlotSeparatePlots"]]) > 0) {
    bubblePlotContainer <- createJaspContainer(title = gettext("Bubble Plots"))
    bubblePlotContainer$dependOn(.maBubblePlotDependencies)
    bubblePlotContainer$position <- 5
    jaspResults[["bubblePlot"]] <- bubblePlotContainer
  } else {
    bubblePlot <- createJaspPlot(title = gettext("Bubble Plot"), width = width, height = height)
    bubblePlot$dependOn(.maBubblePlotDependencies)
    bubblePlot$position <- 6
    jaspResults[["bubblePlot"]] <- bubblePlot
  }

  # make bubble plots
  dfPlot <- .maMakeBubblePlotDataset(fit, options, dataset)

  if (attr(dfPlot, "separatePlots") == "") {
    tempPlots <- list(.maMakeBubblePlot(fit, options, dfPlot))
  } else {
    tempPlots <- lapply(unique(dfPlot[["separatePlots"]]), function(lvl) {
      .maMakeBubblePlot(fit, options, dfPlot[dfPlot[["separatePlots"]] == lvl,], separatePlotsLvl = lvl)
    })
  }

  # modify all generated plots simultaneously
  yRange <- do.call(rbind, lapply(tempPlots, attr, which = "yRange"))
  yRange <- c(min(yRange[, 1]), max(yRange[, 2]))
  yRange <- range(jaspGraphs::getPrettyAxisBreaks(yRange))

  tempPlots <- lapply(tempPlots, function(plot) {
    .maAddBubblePlotTheme(plot, options, dfPlot, yRange)
  })

  if (length(options[["bubblePlotSeparatePlots"]]) > 0) {
    for (i in seq_along(tempPlots)) {
      bubblePlot <- createJaspPlot(title = gettextf("%1$s (%2$s)", attr(dfPlot, "separatePlots"), unique(dfPlot[["separatePlots"]])[i]), width = width, height = height)
      bubblePlot$position      <- i
      bubblePlot$plotObject    <- tempPlots[[i]]
      bubblePlotContainer[[paste0("plot", i)]] <- bubblePlot
    }
  } else {
    bubblePlot$plotObject <- tempPlots[[1]]
  }

  return()
}
.maShowMetaforRCode                   <- function(jaspResults, options) {

  if (!.maReady(options) || !is.null(jaspResults[["metaforRCode"]]))
    return()

  metaforRCode <- createJaspHtml(title = gettext("Metafor R Code"))
  metaforRCode$dependOn(c(.maDependencies, "showMetaforRCode"))
  metaforRCode$position <- 99

  metaforRCode$text <- .maTransformToHtml(.maMakeMetaforCallText(options))

  jaspResults[['metaforRCode']] <- metaforRCode

  return()
}
.maVarianceInflationTable             <- function(jaspResults, dataset, options, parameter = "effectSize") {

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
  if (options[["diagnosticsVarianceInflationFactorAggregate"]])
    termsTable$addColumnInfo(name = "m", type = "integer", title = gettext("Parameters"))

  termsTable$addColumnInfo(name = "vif",  type = "number", title = gettext("VIF"))
  termsTable$addColumnInfo(name = "sif",  type = "number", title = gettext("SIF"))

  if (is.null(fit) || jaspBase::isTryError(fit))
    return()

  termsTable$setData(.maComputeVifSummary(fit, options, parameter))

  return()
}
.maCasewiseDiagnosticsTable           <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["casewiseDiagnosticsTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # extract precomputed diagnostics if done before:
  if (!is.null(jaspResults[["diagnosticsResults"]])) {

    diagnosticsResults <- jaspResults[["diagnosticsResults"]]$object

    influenceResultsDfbs <- diagnosticsResults[["influenceResultsDfbs"]]
    influenceResultsInf  <- diagnosticsResults[["influenceResultsInf"]]

  } else {

    # create the output container
    diagnosticsResults <- createJaspState()
    diagnosticsResults$dependOn(.maDependencies)
    jaspResults[["diagnosticsResults"]] <- diagnosticsResults

    # compute the results
    influenceResults     <- influence(fit)
    influenceResultsDfbs <- data.frame(influenceResults$dfbs)
    influenceResultsInf  <- data.frame(influenceResults$inf)
    influenceResultsInf$tau.del <- sqrt(influenceResultsInf$tau2.del)
    influenceResultsInf$inf[influenceResultsInf$inf == "*"] <- "Yes"

    # store the results
    jaspResults[["diagnosticsResults"]]$object <- list(
      "influenceResultsDfbs" = influenceResultsDfbs,
      "influenceResultsInf"  = influenceResultsInf
    )
  }

  # extract fit data
  fitData <- fit[["data"]]

  # fit measures table
  casewiseDiagnosticsTable          <- createJaspTable(gettext("Casewise Diagnostics Table"))
  casewiseDiagnosticsTable$position <- 7
  casewiseDiagnosticsTable$dependOn(c(.maDependencies, "diagnosticsCasewiseDiagnostics", "diagnosticsCasewiseDiagnosticsShowInfluentialOnly",
                                      "diagnosticsCasewiseDiagnosticsIncludePredictors", "diagnosticsCasewiseDiagnosticsDifferenceInCoefficients",
                                      "studyLabels"))
  jaspResults[["casewiseDiagnosticsTable"]] <- casewiseDiagnosticsTable

  if (options[["diagnosticsCasewiseDiagnosticsShowInfluentialOnly"]] && sum(influenceResultsInf$inf != "Yes") == 0) {
    casewiseDiagnosticsTable$addFootnote(gettext("No influential cases found."))
    return()
  }

  if (options[["studyLabels"]] != "") {
    influenceResultsInf$label <- dataset[[options[["studyLabels"]]]]
    casewiseDiagnosticsTable$addColumnInfo(name = "label", type  = "string", title = gettext("Label"))
  }

  if (options[["diagnosticsCasewiseDiagnosticsIncludePredictors"]]) {
    for (var in colnames(fitData)) {
      casewiseDiagnosticsTable$addColumnInfo(name = paste0("pred_", var), type  = .maGetVariableColumnType(var, options), title = var, overtitle = gettext("Predictor"))
    }
    colnames(fitData)   <- paste0("pred_", colnames(fitData))
    influenceResultsInf <- cbind(fitData, influenceResultsInf)
  }

  casewiseDiagnosticsTable$addColumnInfo(name = "rstudent",  title = gettext("Standardized Residual"),  type = "number")
  casewiseDiagnosticsTable$addColumnInfo(name = "dffits",    title = gettext("DFFITS"),                 type = "number")
  casewiseDiagnosticsTable$addColumnInfo(name = "cook.d",    title = gettext("Cook's Distance"),        type = "number")
  casewiseDiagnosticsTable$addColumnInfo(name = "cov.r",     title = gettext("Covariance ratio"),       type = "number")
  casewiseDiagnosticsTable$addColumnInfo(name = "tau.del",   title = gettext("\U1D70F"),                type = "number", overtitle = gettext("Leave One Out"))
  casewiseDiagnosticsTable$addColumnInfo(name = "tau2.del",  title = gettext("\U1D70F\U00B2"),          type = "number", overtitle = gettext("Leave One Out"))
  casewiseDiagnosticsTable$addColumnInfo(name = "QE.del",    title = gettext("Q\U2091"),                type = "number", overtitle = gettext("Leave One Out"))
  casewiseDiagnosticsTable$addColumnInfo(name = "hat",       title = gettext("Hat"),                    type = "number")
  casewiseDiagnosticsTable$addColumnInfo(name = "weight",    title = gettext("Weight"),                 type = "number")

  if (options[["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"]]) {
    for (par in colnames(influenceResultsDfbs)) {
      casewiseDiagnosticsTable$addColumnInfo(name = par, title = .maVariableNames(par, options[["predictors"]]), type = "number", overtitle = gettext("Difference in coefficients"))
    }
    influenceResultsInf <- cbind(influenceResultsInf, influenceResultsDfbs)
  }

  casewiseDiagnosticsTable$addColumnInfo(name = "inf", title = gettext("Influential"), type = "string")

  if (options[["diagnosticsCasewiseDiagnosticsShowInfluentialOnly"]])
      influenceResultsInf <- influenceResultsInf[influenceResultsInf$inf == "Yes",,drop=FALSE]

  casewiseDiagnosticsTable$setData(influenceResultsInf)

  return()
}
.maCasewiseDiagnosticsExportColumns   <- function(jaspResults, dataset, options) {

  if (!options[["diagnosticsCasewiseDiagnosticsExportToDataset"]])
    return()

  # extract diagnostics already computed in '.maCasewiseDiagnosticsTable'
  diagnosticsResults <- jaspResults[["diagnosticsResults"]]$object

  influenceResultsDfbs <- diagnosticsResults[["influenceResultsDfbs"]]
  influenceResultsInf  <- diagnosticsResults[["influenceResultsInf"]]

  # export columns:
  if (options[["diagnosticsCasewiseDiagnosticsExportToDatasetInfluentialIndicatorOnly"]]) {

    columnName <- "Diagnostics: Influential"
    if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
      .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))

    jaspResults[[columnName]] <- createJaspColumn(columnName = columnName, dependencies = .maDependencies)
    jaspResults[[columnName]]$setNominal(influenceResultsInf[["inf"]])

  } else {

    # export diagnostics
    for (diagnosticName in colnames(influenceResultsInf)) {

      columnName <- paste0("Diagnostics: ", .maCasewiseDiagnosticsExportColumnsNames(diagnosticName))

      if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
        .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))

      jaspResults[[columnName]] <- createJaspColumn(columnName = columnName, dependencies = .maDependencies)
      if (diagnosticName == "inf") {
        jaspResults[[columnName]]$setNominal(influenceResultsInf[[diagnosticName]])
      } else {
        jaspResults[[columnName]]$setScale(influenceResultsInf[[diagnosticName]])
      }
    }

    # export change in coefficients
    if (options[["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"]]) {

      for (diagnosticName in colnames(influenceResultsDfbs)) {

        columnName <- decodeColNames(paste0("Difference in coefficients: ", .maVariableNames(diagnosticName, options[["predictors"]])))

        if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
          .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))

        jaspResults[[columnName]] <- createJaspColumn(columnName = columnName, dependencies = .maDependencies)
        jaspResults[[columnName]]$setScale(influenceResultsDfbs[[diagnosticName]])
      }
    }

  }

  return()
}
.maProfileLikelihoodPlot              <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["profileLikelihoodPlot"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # create plot
  profileLikelihoodPlot <- createJaspPlot(title = gettext("Profile Likelihood Plot"), width = 400, height = 320)
  profileLikelihoodPlot$dependOn(c(.maDependencies, "diagnosticsPlotsProfileLikelihood"))
  profileLikelihoodPlot$position <- 8
  jaspResults[["profileLikelihoodPlot"]] <- profileLikelihoodPlot

  if (.maIsMetaregressionHeterogeneity(options)) {
    profileLikelihoodPlot$setError(gettext("Profile likelihood is not available for models that contain meta-regression on heterogeneity."))
    return()
  }

  # obtain profile likelihood
  xTicks <- jaspGraphs::getPrettyAxisBreaks(c(0, 2*fit[["tau2"]]))
  dfProfile <- try(profile(fit, xlim = range(xTicks), plot = FALSE, progbar = FALSE))
  if (jaspBase::isTryError(dfProfile)) {
    profileLikelihoodPlot$setError(dfProfile)
    return()
  }

  yTicks <- jaspGraphs::getPrettyAxisBreaks(c(min(dfProfile$ll), max(dfProfile$ll)))

  # create plot
  plotOut <- ggplot2::ggplot(
    data = data.frame(
      x = dfProfile$tau2,
      y = dfProfile$ll
    ),
    ggplot2::aes(
      x = x,
      y = y)
    ) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point() +
    ggplot2::geom_line(
      data = data.frame(
        x = rep(fit[["tau2"]], 2),
        y = range(yTicks)),
      linetype = "dotted") +
    ggplot2::geom_line(
      data = data.frame(
        x = range(xTicks),
        y = rep(max(dfProfile$ll), 2)),
      linetype = "dotted") +
    ggplot2::labs(x = expression(tau^2), y = gettext("Profile Likelihood")) +
    jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks)) +
    jaspGraphs::scale_y_continuous(breaks = yTicks, limits = range(yTicks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  profileLikelihoodPlot$plotObject <- plotOut

  return()
}
.maBaujatPlot                         <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["baujatPlot"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # create plot
  baujatPlot <- createJaspPlot(title = gettext("Baujat Plot"), width = 400, height = 320)
  baujatPlot$dependOn(c(.maDependencies, "diagnosticsPlotsBaujat", "studyLabels"))
  baujatPlot$position <- 9
  jaspResults[["baujatPlot"]] <- baujatPlot

  # extract precomputed baujat data if done before:
  if (!is.null(jaspResults[["baujatResults"]])) {

    dfBaujat <- jaspResults[["baujatResults"]]$object

  } else {

    # create the output container
    baujatResults <- createJaspState()
    baujatResults$dependOn(.maDependencies)
    jaspResults[["baujatResults"]] <- baujatResults

    # compute the results and save them in the container
    dfBaujat <- try(.maSuppressPlot(metafor::baujat(fit)))

    # store in the container
    jaspResults[["baujatResults"]]$object <- dfBaujat
  }


  # if (.maIsMetaregressionHeterogeneity(options)) {
  #   baujatPlot$setError(gettext("Baujat plot is not available for models that contain meta-regression on heterogeneity."))
  #   return()
  # }

  if (jaspBase::isTryError(dfBaujat)) {
    dfBaujat$setError(dfBaujat)
    return()
  }

  if (options[["studyLabels"]] != "")
    dfBaujat$label <- as.character(dataset[[options[["studyLabels"]]]])

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(dfBaujat$x))
  yTicks <- jaspGraphs::getPrettyAxisBreaks(range(dfBaujat$y))


  aesCall <- list(
    x     = as.name("x"),
    y     = as.name("y"),
    label = if (options[["studyLabels"]] != "") as.name("label")
  )
  geomCall <- list(
    data    = dfBaujat,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)])
  )

  # create plot
  plotOut <- do.call(ggplot2::ggplot, geomCall) +
    jaspGraphs::geom_point(
      size = if (options[["studyLabels"]] != "") 2 else 3
    )

  if (options[["studyLabels"]] != "")
    plotOut <- plotOut + ggplot2::geom_text(hjust = 0, vjust = 0)

  plotOut <- plotOut +
    ggplot2::labs(x = gettext("Squared Pearson Residual"), y = gettext("Influence on Fitted Value")) +
    jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks)) +
    jaspGraphs::scale_y_continuous(breaks = yTicks, limits = range(yTicks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  baujatPlot$plotObject <- plotOut

  return()
}

# containers/state functions
.maExtractFit                             <- function(jaspResults, options, nonClustered = FALSE) {

  if (is.null(jaspResults[["fit"]]$object))
    return()

  if (!is.null(jaspResults[["fitNoInfluence"]]$object)) {
    # extract clustered model if specified
    if (options[["clustering"]] == "" || nonClustered) {
      return(jaspResults[["fitNoInfluence"]]$object[["fit"]])
    } else {
      return(jaspResults[["fitNoInfluence"]]$object[["fitClustered"]])
    }
  } else {
    # extract clustered model if specified
    if (options[["clustering"]] == "" || nonClustered) {
      return(jaspResults[["fit"]]$object[["fit"]])
    } else {
      return(jaspResults[["fit"]]$object[["fitClustered"]])
    }
  }
}
.maExtractModelSummaryContainer           <- function(jaspResults) {

  if (!is.null(jaspResults[["modelSummaryContainer"]]))
    return(jaspResults[["modelSummaryContainer"]])

  # create the output container
  modelSummaryContainer <- createJaspContainer(gettext("Model Summary"))
  modelSummaryContainer$dependOn(.maDependencies)
  modelSummaryContainer$position <- 1
  jaspResults[["modelSummaryContainer"]] <- modelSummaryContainer

  return(modelSummaryContainer)
}
.maExtractMetaregressionContainer         <- function(jaspResults) {

  if (!is.null(jaspResults[["metaregressionContainer"]]))
    return(jaspResults[["metaregressionContainer"]])

  # create the output container
  metaregressionContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  metaregressionContainer$dependOn(c(.maDependencies, "confidenceInterval"))
  metaregressionContainer$position <- 3
  jaspResults[["metaregressionContainer"]] <- metaregressionContainer

  return(metaregressionContainer)
}
.maExtractEstimatedMarginalMeansContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["estimatedMarginalMeansContainer"]]))
    return(jaspResults[["estimatedMarginalMeansContainer"]])

  # create the output container
  estimatedMarginalMeansContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  estimatedMarginalMeansContainer$dependOn(c(.maDependencies, "confidenceIntervals", "confidenceIntervalsLevel"))
  estimatedMarginalMeansContainer$position <- 4
  jaspResults[["estimatedMarginalMeansContainer"]] <- estimatedMarginalMeansContainer

  return(estimatedMarginalMeansContainer)
}
.maExtractVarianceInflationContainer      <- function(jaspResults) {

  if (!is.null(jaspResults[["varianceInflationContainer"]]))
    return(jaspResults[["varianceInflationContainer"]])

  # create the output container
  varianceInflationContainer <- createJaspContainer(gettext("Variance Inflation Summary"))
  varianceInflationContainer$dependOn(c(.maDependencies, "diagnosticsVarianceInflationFactor", "diagnosticsVarianceInflationFactorAggregate"))
  varianceInflationContainer$position <- 7
  jaspResults[["varianceInflationContainer"]] <- varianceInflationContainer

  return(varianceInflationContainer)
}

# help compute functions
.maComputePooledEffect             <- function(fit, options) {

  if (!.maIsMetaregressionEffectSize(options)) {
    predictedEffect <- predict(fit, level = 100 * options[["confidenceIntervalsLevel"]])
  } else {
    if (.maIsMetaregressionHeterogeneity(options)) {
      predictedEffect <- predict(
        fit,
        newmods  = colMeans(model.matrix(fit)$location)[-1],
        newscale = colMeans(model.matrix(fit)$scale)[-1],
        level    = 100 * options[["confidenceIntervalsLevel"]]
      )
    } else {
      predictedEffect <- predict(
        fit,
        newmods = colMeans(model.matrix(fit))[-1],
        level   = 100 * options[["confidenceIntervalsLevel"]]
      )
    }
  }

  # to data.frame
  predictedEffect <- data.frame(predictedEffect)

  # add empty prediction interval for FE and EE, or models without
  if (!"pi.lb" %in% colnames(predictedEffect)) {
    predictedEffect$pi.lb <- NA
    predictedEffect$pi.ub <- NA
  }

  # fix column names
  colnames(predictedEffect) <- c("est", "se", "lCi", "uCi", "lPi", "uPi")
  predictedEffect$par       <- "Effect Size"

  # apply effect size transformation
  if (options[["transformEffectSize"]] != "none")
    predictedEffect[,c("est", "lCi", "uCi", "lPi", "uPi")] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(predictedEffect[,c("est", "lCi", "uCi", "lPi", "uPi")]))

  # remove non-requested columns
  keepResults <- c(
    "par",
    "est",
    if (options[["confidenceIntervals"]]) "lCi",
    if (options[["confidenceIntervals"]]) "uCi",
    if (options[["predictionIntervals"]]) "lPi",
    if (options[["predictionIntervals"]]) "uPi"
  )

  predictedEffect <- predictedEffect[,keepResults]
  return(as.list(predictedEffect))
}
.maComputePooledEffectPlot         <- function(fit, options) {

  if (!.maIsMetaregressionEffectSize(options)) {
    predictedEffect <- predict(fit)
  } else {
    if (.maIsMetaregressionHeterogeneity(options)) {
      predictedEffect <- predict(
        fit,
        newmods  = colMeans(model.matrix(fit)$location)[-1],
        newscale = colMeans(model.matrix(fit)$scale)[-1]
      )
    } else {
      predictedEffect <- predict(
        fit,
        newmods = colMeans(model.matrix(fit))[-1]
      )
    }
  }

  # compute test against specified value
  if (.maIsMetaregressionFtest(options)) {
    predictedEffect      <- cbind(data.frame(predictedEffect), "df" = predictedEffect$ddf)
    predictedEffect$stat <- (predictedEffect$pred - 0)  / predictedEffect$se
    predictedEffect$pval <- 2 * pt(abs(predictedEffect$stat), predictedEffect$df, lower.tail = FALSE)

    # add empty prediction interval for FE and EE
    if (.maGetMethodOptions(options) %in% c("FE", "EE")) {
      predictedEffect$pi.lb <- predictedEffect$ci.lb
      predictedEffect$pi.ub <- predictedEffect$ci.ub
    }

    colnames(predictedEffect) <- c("est", "se", "lCi", "uCi", "lPi", "uPi", "df", "stat", "pval")
  } else {
    predictedEffect      <- data.frame(predictedEffect)
    predictedEffect$stat <- (predictedEffect$pred - 0)  / predictedEffect$se
    predictedEffect$pval <- 2 * pnorm(abs(predictedEffect$stat), lower.tail = FALSE)

    # add empty prediction interval for FE and EE
    if (.maGetMethodOptions(options) %in% c("FE", "EE")) {
      predictedEffect$pi.lb <- predictedEffect$ci.lb
      predictedEffect$pi.ub <- predictedEffect$ci.ub
    }

    colnames(predictedEffect) <- c("est", "se", "lCi", "uCi", "lPi", "uPi", "stat", "pval")
  }

  # fix column names
  predictedEffect$par       <- "Effect Size"

  # apply effect size transformation
  if (options[["transformEffectSize"]] != "none")
    predictedEffect[,c("est", "lCi", "uCi", "lPi", "uPi")] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(predictedEffect[,c("est", "lCi", "uCi", "lPi", "uPi")]))


  return(as.list(predictedEffect))
}
.maComputePooledHeterogeneity      <- function(fit, options) {

  if (options[["fixParametersTau2"]]) {

    confIntHeterogeneity <- data.frame(
      par = c("\U1D70F", "\U1D70F\U00B2"),
      est = c(sqrt(.maGetFixedTau2Options(options)), .maGetFixedTau2Options(options)),
      lCi = c(NA, NA),
      uCi = c(NA, NA)
    )

    # keep only the requested parameters (other than tau and tau^2 are not possible)
    heterogeneityShow <- c(
      if (options[["heterogeneityTau"]])  1,
      if (options[["heterogeneityTau2"]]) 2
    )

    confIntHeterogeneity <- confIntHeterogeneity[heterogeneityShow,,drop = FALSE]

  } else if (.maIsMetaregressionHeterogeneity(options)) {
    # no confint support
    # predict the scale on the average value
    predScale <- predict(fit, newscale = colMeans(model.matrix(fit)$scale)[-1], level = 100 * options[["confidenceIntervalsLevel"]])

    if (options[["heterogeneityModelLink"]] == "log") {
      confIntHeterogeneity <- data.frame(
        par = c("\U1D70F", "\U1D70F\U00B2"),
        est = exp(c(predScale[["pred"]]  / 2, predScale[["pred"]])),
        lCi = exp(c(predScale[["ci.lb"]] / 2, predScale[["ci.lb"]])),
        uCi = exp(c(predScale[["ci.ub"]] / 2, predScale[["ci.ub"]]))
      )
    } else if (options[["heterogeneityModelLink"]] == "identity") {
      confIntHeterogeneity <- data.frame(
        par = c("\U1D70F", "\U1D70F\U00B2"),
        est = c(sqrt(predScale[["pred"]]),  predScale[["pred"]]),
        lCi = c(sqrt(predScale[["ci.lb"]]), predScale[["ci.lb"]]),
        uCi = c(sqrt(predScale[["ci.ub"]]), predScale[["ci.ub"]])
      )
    }

    # keep only the requested parameters (other than tau and tau^2 are not possible)
    heterogeneityShow <- c(
      if (options[["heterogeneityTau"]])  1,
      if (options[["heterogeneityTau2"]]) 2
    )

    confIntHeterogeneity <- confIntHeterogeneity[heterogeneityShow,,drop = FALSE]

  } else {

    confIntHeterogeneity <- confint(fit, level = 100 * options[["confidenceIntervalsLevel"]])
    confIntHeterogeneity <- data.frame(confIntHeterogeneity[["random"]])[c(2,1,3,4),]
    colnames(confIntHeterogeneity) <- c("est", "lCi", "uCi")
    confIntHeterogeneity$par       <- c("\U1D70F", "\U1D70F\U00B2", "I\U00B2", "H\U00B2")

    # keep only the requested parameters
    heterogeneityShow <- c(
      if (options[["heterogeneityTau"]])  1,
      if (options[["heterogeneityTau2"]]) 2,
      if (options[["heterogeneityI2"]])   3,
      if (options[["heterogeneityH2"]])   4
    )

    confIntHeterogeneity <- confIntHeterogeneity[heterogeneityShow,,drop = FALSE]

  }

  if (!options[["confidenceIntervals"]])
    confIntHeterogeneity <- confIntHeterogeneity[,c("par", "est")]

  return(confIntHeterogeneity)
}
.maComputePooledHeterogeneityPlot  <- function(fit, options) {

  if (options[["fixParametersTau2"]]) {

    confIntHeterogeneity <- list(
      est = sqrt(.maGetFixedTau2Options(options)),
      lCi = NA,
      uCi = NA
    )

  } else if (.maIsMetaregressionHeterogeneity(options)) {

    # no confint support
    # predict the scale on the average value
    predScale <- predict(fit, newscale = colMeans(model.matrix(fit)$scale)[-1], level = 100 * options[["confidenceIntervalsLevel"]])

    if (options[["heterogeneityModelLink"]] == "log") {
      confIntHeterogeneity <- data.frame(
        est = exp(predScale[["pred"]]  / 2),
        lCi = exp(predScale[["ci.lb"]] / 2),
        uCi = exp(predScale[["ci.ub"]] / 2)
      )
    } else if (options[["heterogeneityModelLink"]] == "identity") {
      confIntHeterogeneity <- data.frame(
        est = sqrt(predScale[["pred"]]),
        lCi = sqrt(predScale[["ci.lb"]]),
        uCi = sqrt(predScale[["ci.ub"]])
      )
    }

  } else {

    confIntHeterogeneity <- confint(fit)
    confIntHeterogeneity <- data.frame(confIntHeterogeneity[["random"]])[2,]
    colnames(confIntHeterogeneity) <- c("est", "lCi", "uCi")
  }

  return(confIntHeterogeneity)
}
.maOmnibusTest                     <- function(fit, options, parameter = "effectSize") {

  if (parameter == "effectSize") {
    row <- list(
      parameter = gettext("Effect Size"),
      stat      = fit[["QM"]],
      df1       = fit[["QMdf"]][1],
      pval      = fit[["QMp"]]
    )
  } else if (parameter == "heterogeneity") {
    row <- list(
      parameter = gettext("Heterogeneity"),
      stat      = fit[["QS"]],
      df1       = fit[["QSdf"]][1],
      pval      = fit[["QSp"]]
    )
  }

  if (.maIsMetaregressionFtest(options)) {
    if (parameter == "effectSize")
      row$df2 <- fit[["QMdf"]][2]
    else if (parameter == "heterogeneity")
      row$df2 <- fit[["QSdf"]][2]
  }

  return(row)
}
.maOmnibusTestCoefficients         <- function(fit, options, parameter = "effectSize") {

  if (parameter == "effectSize") {
    maxCoef <- nrow(fit$beta)
    selCoef <- .parseRCodeInOptions(options[["addOmnibusModeratorTestEffectSizeCoefficientsValues"]])
  } else if (parameter == "heterogeneity") {
    maxCoef <- nrow(fit$alpha)
    selCoef <- .parseRCodeInOptions(options[["addOmnibusModeratorTestHeterogeneityCoefficientsValues"]])
  }

  if (!is.numeric(selCoef) || any(!(abs(selCoef - round(selCoef)) < .Machine$double.eps^0.5)))
    return(gettext("The selected coefficients must be an integer vector."))
  if (any(selCoef < 1) || any(selCoef > maxCoef))
    return(gettextf("The selected coefficients must be between 1 and %1$i (i.e., the number of regression parameters).", maxCoef))

  if (parameter == "effectSize") {
    out <- anova(fit, btt = selCoef)
  } else if (parameter == "heterogeneity") {
    out <- anova(fit, btt = selCoef)
  }

  row <- list(
    stat = out[["QM"]],
    df1  = out[["QMdf"]][1],
    pval = out[["QMp"]]
  )

  if (.maIsMetaregressionFtest(options))
    row$df2 <- fit[["QMdf"]][2]

  if (.maIsMetaregressionHeterogeneity(options))
    row$parameter <- gettextf("Effect Size (coef: %1$s)", paste(selCoef, collapse = ","))
  else if (.maIsMetaregressionEffectSize(options))
    row$parameter <- gettextf("Heterogeneity (coef: %1$s)", paste(selCoef, collapse = ","))

  return(row)
}
.maTermTests                       <- function(fit, options, term, parameter = "effectSize") {

  # obtain terms indicies
  if (parameter == "effectSize") {
    terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")
    termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")
    termsAnova <- anova(fit, btt = seq_along(termsIndex)[termsIndex == which(terms == term)])

    out <- list(
      term = .maVariableNames(term, options[["predictors"]]),
      stat = termsAnova[["QM"]],
      df1  = termsAnova[["QMdf"]][1],
      pval = termsAnova[["QMp"]]
    )

    if (.maIsMetaregressionFtest(options))
      out$df2 <- termsAnova[["QMdf"]][2]

  } else if (parameter == "heterogeneity") {
    terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")
    termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")
    termsAnova <- anova(fit, att = seq_along(termsIndex)[termsIndex == which(terms == term)])

    out <- list(
      term = .maVariableNames(term, options[["predictors"]]),
      stat = termsAnova[["QS"]],
      df1  = termsAnova[["QSdf"]][1],
      pval = termsAnova[["QSp"]]
    )

    if (.maIsMetaregressionFtest(options))
      out$df2 <- termsAnova[["QSdf"]][2]

  }

  return(out)
}
.maGetMarginalMeansPredictorMatrix <- function(fit, options, dataset, selectedVariables, trendVarible = NULL, trendSequence = NULL, sdFactor, parameter) {

  variablesContinuous <- options[["predictors"]][options[["predictors.types"]] == "scale"]
  variablesFactors    <- options[["predictors"]][options[["predictors.types"]] == "nominal"]

  # extract the corresponding formula
  formula <- switch(
    parameter,
    effectSize    = fit[["formula.mods"]],
    heterogeneity = fit[["formula.scale"]]
  )

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
  if (length(selectedVariables) > 0) {
    for (selectedVariable in selectedVariables) {
      if (selectedVariable %in% variablesFactors) {
        predictorsSelected[[selectedVariable]] <- factor(levels(dataset[[selectedVariable]]), levels = levels(dataset[[selectedVariable]]))
        contrasts(predictorsSelected[[selectedVariable]]) <- contrasts(dataset[[selectedVariable]])
      } else if (selectedVariable %in% variablesContinuous) {
        predictorsSelected[[selectedVariable]] <- c(
          mean(dataset[[selectedVariable]]) - sdFactor * sd(dataset[[selectedVariable]]),
          mean(dataset[[selectedVariable]]),
          mean(dataset[[selectedVariable]]) + sdFactor * sd(dataset[[selectedVariable]])
        )
      }
    }
  }


  # create model matrix for the trend variable
  if (length(trendVarible) != 0) {
    predictorsSelected[[trendVarible]] <- trendSequence
  }

  # add the specified variable and pool across the combinations of the remaining values
  if (length(selectedVariables) == 1 && selectedVariables == "") {
    # empty string creates overall adjusted estimate
    outMatrix <- colMeans(model.matrix(formula, data = expand.grid(predictorsRemaining)))[-1]
  } else {
    predictorsSelectedGrid <- expand.grid(predictorsSelected)
    outMatrix <- do.call(rbind, lapply(1:nrow(predictorsSelectedGrid), function(i) {
      colMeans(model.matrix(formula, data = expand.grid(c(predictorsRemaining,  predictorsSelectedGrid[i,,drop = FALSE]))))[-1]
    }))
  }


  # keep information about the variable and levels
  if (length(selectedVariables) == 1 && selectedVariables == "") {

    # add intercept
    attr(outMatrix, "variable") <- gettext("Adjusted Estimate")
    attr(outMatrix, gettext("Adjusted Estimate")) <- ""

  } else {

    # selected variables grid
    attr(outMatrix, "selectedGrid") <- predictorsSelectedGrid

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
  }

  if (length(trendVarible) != 0) {
    attr(outMatrix, "trend") <- trendVarible
    attr(outMatrix, "trend") <- trendSequence
  }

  return(outMatrix)

}
.maComputeMarginalMeansVariable    <- function(fit, options, dataset, selectedVariable, testAgainst = 0, parameter) {

  if (parameter == "effectSize") {

    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
      parameter         = "effectSize"
    )

    if (.maIsMetaregressionHeterogeneity(options)) {

      predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
        fit               = fit,
        options           = options,
        dataset           = dataset,
        selectedVariables = selectedVariable,
        sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
        parameter         = "heterogeneity"
      )
      computedMarginalMeans <- predict(
        fit,
        newmods  = predictorMatrixEffectSize,
        newscale = predictorMatrixHeterogeneity,
        level    = 100 * options[["confidenceIntervalsLevel"]]
      )
    } else {

      computedMarginalMeans <- predict(
        fit,
        newmods = predictorMatrixEffectSize,
        level   = 100 * options[["confidenceIntervalsLevel"]]
      )
    }

    # compute test against specified value
    if (.maIsMetaregressionFtest(options)) {
      computedMarginalMeans      <- cbind(data.frame(computedMarginalMeans), "df" = computedMarginalMeans$ddf)
      computedMarginalMeans$stat <- (computedMarginalMeans$pred - testAgainst)  / computedMarginalMeans$se
      computedMarginalMeans$pval <- 2 * pt(abs(computedMarginalMeans$stat), computedMarginalMeans$df, lower.tail = FALSE)
      colnames(computedMarginalMeans) <- c("est", "se", "lCi", "uCi", "lPi", "uPi", "df", "stat", "pval")
    } else {
      computedMarginalMeans      <- data.frame(computedMarginalMeans)
      computedMarginalMeans$stat <- (computedMarginalMeans$pred - testAgainst)  / computedMarginalMeans$se
      computedMarginalMeans$pval <- 2 * pnorm(abs(computedMarginalMeans$stat), lower.tail = FALSE)
      colnames(computedMarginalMeans) <- c("est", "se", "lCi", "uCi", "lPi", "uPi", "stat", "pval")
    }

    # apply effect size transformation
    if (options[["transformEffectSize"]] != "none")
      computedMarginalMeans[,c("est", "lCi", "uCi", "lPi", "uPi")] <- do.call(
        .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
        list(computedMarginalMeans[,c("est", "lCi", "uCi", "lPi", "uPi")]))

    # create full data frame
    computedMarginalMeans <- data.frame(
      "variable" = attr(predictorMatrixEffectSize, "variable"),
      "value"    = attr(predictorMatrixEffectSize, attr(predictorMatrixEffectSize, "variable")),
      computedMarginalMeans
    )

  } else if (parameter == "heterogeneity") {

    predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansHeterogeneitySdFactorCovariates"]],
      parameter         = "heterogeneity"
    )

    computedMarginalMeans <- predict(
      fit,
      newscale = predictorMatrixHeterogeneity,
      level    = 100 * options[["confidenceIntervalsLevel"]]
    )

    computedMarginalMeans <- data.frame(computedMarginalMeans)
    colnames(computedMarginalMeans) <- c("est", "se", "lCi", "uCi")

    # apply link transform
    if (options[["heterogeneityModelLink"]] == "log") {
      computedMarginalMeans <- exp(computedMarginalMeans)
    }

    # apply tau / tau2 transform
    if (options[["estimatedMarginalMeansHeterogeneityTransformation"]] == "tau")
      computedMarginalMeans <- sqrt(computedMarginalMeans)

    # create full data frame
    computedMarginalMeans <- data.frame(
      "variable" = attr(predictorMatrixHeterogeneity, "variable"),
      "value"    = attr(predictorMatrixHeterogeneity, attr(predictorMatrixHeterogeneity, "variable")),
      computedMarginalMeans
    )
  }


  # remove unnecessary columns
  computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% "se"]

  if (!options[["confidenceIntervals"]])
    computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% c("lCi", "uCi")]

  if (!options[["predictionIntervals"]])
    computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% c("lPi", "uPi")]

  return(computedMarginalMeans)
}
.maMakeBubblePlotDataset           <- function(fit, options, dataset) {

  # extract options
  separateLines     <- unlist(options[["bubblePlotSeparateLines"]])
  separatePlots     <- unlist(options[["bubblePlotSeparatePlots"]])
  selectedVariable  <- options[["bubblePlotSelectedVariable"]][[1]][["variable"]]

  # create nice plotting range
  xRange <- range(jaspGraphs::getPrettyAxisBreaks(range(dataset[[selectedVariable]])))
  trendSequence <- seq(xRange[1], xRange[2], length.out =  101)

  predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
    fit               = fit,
    options           = options,
    dataset           = dataset,
    selectedVariables = c(separateLines, separatePlots),
    sdFactor          = options[["bubblePlotSdFactorCovariates"]],
    trendVarible      = selectedVariable,
    trendSequence     = trendSequence,
    parameter         = "effectSize"
  )

  if (.maIsMetaregressionHeterogeneity(options)) {

    predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = c(separateLines, separatePlots),
      sdFactor          = options[["bubblePlotSdFactorCovariates"]],
      trendVarible      = selectedVariable,
      trendSequence     = trendSequence,
      parameter         = "heterogeneity"
    )

    computedMarginalMeans <- predict(
      fit,
      newmods  = predictorMatrixEffectSize,
      newscale = predictorMatrixHeterogeneity,
      level    = 100 * options[["confidenceIntervalsLevel"]]
    )
  } else {

    computedMarginalMeans <- predict(
      fit,
      newmods = predictorMatrixEffectSize,
      level   = 100 * options[["confidenceIntervalsLevel"]]
    )
  }

  ### modify and rename selectedGrid
  selectedGrid <- attr(predictorMatrixEffectSize, "selectedGrid")
  selectedGrid$selectedVariable <- selectedGrid[,selectedVariable]
  # collapse factor levels if multiple selected
  selectedGrid <- .maMergeVariablesLevels(selectedGrid, separateLines, "separateLines")
  selectedGrid <- .maMergeVariablesLevels(selectedGrid, separatePlots, "separatePlots")
  # remove original names
  selectedGrid <- selectedGrid[,setdiff(names(selectedGrid), c(selectedVariable, separateLines, separatePlots)),drop = FALSE]

  ### modify marginal means
  if (.maGetMethodOptions(options) %in% c("EE", "FE")) {
    computedMarginalMeans$pi.lb <- computedMarginalMeans$ci.lb
    computedMarginalMeans$pi.ub <- computedMarginalMeans$ci.ub
  }
  computedMarginalMeans <- data.frame(computedMarginalMeans)
  colnames(computedMarginalMeans) <- c("y", "se", "lCi", "uCi", "lPi", "uPi")

  ### merge and add attributes
  dfPlot <- cbind.data.frame(selectedGrid, computedMarginalMeans)

  attr(dfPlot, "selectedVariable") <- selectedVariable
  attr(dfPlot, "separateLines")  <- paste(separateLines, collapse = " | ")
  attr(dfPlot, "separatePlots")  <- paste(separatePlots, collapse = " | ")
  attr(dfPlot, "variablesLines") <- separateLines
  attr(dfPlot, "variablesPlots") <- separatePlots
  attr(dfPlot, "xRange")         <- xRange

  return(dfPlot)
}
.maMakeBubblePlot                  <- function(fit, options, dfPlot, separatePlotsLvl = NULL) {

  bubblePlot <- ggplot2::ggplot()
  yRange     <- NULL

  hasSeparateLines <- attr(dfPlot, "separateLines") != ""
  hasSeparatePlots <- attr(dfPlot, "separatePlots") != ""

  ### add prediction bads
  if (options[["bubblePlotPredictionIntervals"]]) {
    aesCall <- list(
      x     = as.name("selectedVariable"),
      y     = as.name("y"),
      fill  = if (hasSeparateLines) as.name("separateLines"),
      group = if (hasSeparateLines) as.name("separateLines")
    )
    dfPiBands <-  .maBubblePlotMakeConfidenceBands(dfPlot, lCi = "lPi", uCi = "uPi")
    dfPiBands[["y"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfPiBands[["y"]]))
    geomCall <- list(
      data    = dfPiBands,
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
      alpha   = options[["bubblePlotPredictionIntervalsTransparency"]]
    )
    bubblePlot <- bubblePlot + do.call(ggplot2::geom_polygon, geomCall)
    yRange <- range(c(yRange, dfPiBands$y))
  }

  ### add confidence bands
  if (options[["bubblePlotCondifenceIntervals"]]) {
    aesCall <- list(
      x     = as.name("selectedVariable"),
      y     = as.name("y"),
      fill  = if (hasSeparateLines) as.name("separateLines"),
      group = if (hasSeparateLines) as.name("separateLines")
    )
    dfCiBands <- .maBubblePlotMakeConfidenceBands(dfPlot)
    dfCiBands[["y"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfCiBands[["y"]]))
    geomCall <- list(
      data    = dfCiBands,
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
      alpha   = options[["bubblePlotCondifenceIntervalsTransparency"]]
    )
    bubblePlot <- bubblePlot + do.call(ggplot2::geom_polygon, geomCall)
    yRange <- range(c(yRange, dfCiBands$y))
  }

  ### add predictiction line
  aesCall <- list(
    x     = as.name("selectedVariable"),
    y     = as.name("y"),
    color = if (hasSeparateLines) as.name("separateLines")
  )
  dfPlot[["y"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfPlot[["y"]]))
  geomCall <- list(
    data    = dfPlot,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)])
  )
  bubblePlot <- bubblePlot + do.call(jaspGraphs::geom_line, geomCall)
  yRange <- range(c(yRange, dfPlot$pred))

  ### add studies as bubbles
  dfStudies <- data.frame(
    effectSize       = fit[["yi"]],
    inverseVariance  = 1/fit[["vi"]],
    weight           = weights(fit),
    constant         = rep(options[["bubblePlotBubblesRelativeSize"]], nrow(fit[["data"]])),
    selectedVariable = fit[["data"]][[attr(dfPlot, "selectedVariable")]]
  )

  # add separate lines and plots
  if (hasSeparateLines)
    dfStudies[attr(dfPlot, "variablesLines")] <- fit[["data"]][attr(dfPlot, "variablesLines")]
  if (hasSeparatePlots)
    dfStudies[attr(dfPlot, "variablesPlots")] <- fit[["data"]][attr(dfPlot, "variablesPlots")]

  # make same encoding
  dfStudies <- .maMergeVariablesLevels(dfStudies, variablesLines <- attr(dfPlot, "variablesLines"), "separateLines")
  dfStudies <- .maMergeVariablesLevels(dfStudies, variablesLines <- attr(dfPlot, "variablesPlots"), "separatePlots")

  # subset original data across plots
  if (!is.null(separatePlotsLvl))
    dfStudies <- dfStudies[dfStudies$separatePlots == separatePlotsLvl,]

  aesCall <- list(
    x     = as.name("selectedVariable"),
    y     = as.name("effectSize"),
    size  = switch(
      options[["bubblePlotBubblesSize"]],
      "weight"          = as.name("weight"),
      "inverseVariance" = as.name("inverseVariance"),
      "equal"           = as.name("constant")
    ),
    color = if (hasSeparateLines) as.name("separateLines"),
    fill  = if (hasSeparateLines) as.name("separateLines"),
    alpha = options[["bubblePlotBubblesTransparency"]]
  )
  dfStudies[["effectSize"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfStudies[["effectSize"]]))
  geomCall <- list(
    data    = dfStudies,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
    show.legend = FALSE
  )
  bubblePlot <- bubblePlot + do.call(jaspGraphs::geom_point, geomCall) +
    ggplot2::scale_size(range = c(1.5, 10) * options[["bubblePlotBubblesRelativeSize"]])
  yRange     <- range(c(yRange, dfStudies[["effectSize"]]))

  # add color palette
  bubblePlot <- bubblePlot +
    jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]]) +
    jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])

  attr(bubblePlot, "yRange") <- yRange
  return(bubblePlot)
}
.maAddBubblePlotTheme              <- function(plot, options, dfPlot, yRange) {

  plot <- plot +
    jaspGraphs::scale_x_continuous(
      name   = attr(dfPlot, "selectedVariable"),
      breaks = jaspGraphs::getPrettyAxisBreaks(attr(dfPlot, "xRange")),
      limits = attr(dfPlot, "xRange")
    ) +
    jaspGraphs::scale_y_continuous(
      name   = if (options[["transformEffectSize"]] == "none") gettext("Effect Size") else .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]]),
      breaks = jaspGraphs::getPrettyAxisBreaks(yRange),
      limits = yRange
    )  +
    ggplot2::labs(fill = attr(dfPlot, "separateLines"), color = attr(dfPlot, "separateLines"))

  if (options[["bubblePlotTheme"]] == "jasp") {

    plot <- plot +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = if (attr(dfPlot, "separateLines") == "") "none" else options[["bubblePlotLegendPosition"]])

  } else {

    plot <- plot +
      switch(
        options[["bubblePlotTheme"]],
        "whiteBackground" = ggplot2::theme_bw()       + ggplot2::theme(legend.position = "bottom"),
        "light"           = ggplot2::theme_light()    + ggplot2::theme(legend.position = "bottom"),
        "minimal"         = ggplot2::theme_minimal()  + ggplot2::theme(legend.position = "bottom"),
        "pubr"            = jaspGraphs::themePubrRaw(legend = options[["bubblePlotLegendPosition"]]),
        "apa"             = jaspGraphs::themeApaRaw(legend.pos = switch(
          options[["bubblePlotLegendPosition"]],
          "none"   = "none",
          "bottom" = "bottommiddle",
          "right"  = "bottomright",
          "top"    = "topmiddle",
          "left"   = "bottomleft"
        ))
      )

    plot <- plot + ggplot2::theme(
      legend.text  = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      legend.title = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      axis.text    = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      axis.title   = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      legend.position = if (attr(dfPlot, "separateLines") == "") "none" else options[["bubblePlotLegendPosition"]])
  }

  return(plot)
}
.maMakeMetaforCallText             <- function(options) {

  if (options[["module"]] == "metaAnalysis") {
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      sei  = as.name(options[["effectSizeStandardError"]]),
      data = as.name("dataset")
    )
  } else if (options[["module"]] == "metaAnalysisMultilevelMultivariate") {
    # TODO: extend to covariance matrices
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      V    = paste0(options[["effectSizeStandardError"]], "^2"), # precomputed on data load
      data = as.name("dataset")
    )
  }

  # add formulas if specified
  rmaInput$mods  <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  rmaInput$scale <- .maGetFormula(options[["heterogeneityModelTerms"]], options[["heterogeneityModelIncludeIntercept"]])

  # add random effects
  if (.maIsMultilevelMultivariate(options)) {
    randomFormulaList <- .mammGetRandomFormulaList(options)
    if (length(randomFormulaList) != 0) {
      struct <- do.call(c, lapply(randomFormulaList, attr, "structure"))
      if (length(randomFormulaList) > 1)
        randomFormulaList <- paste0("list(\n\t\t", paste0("'", names(randomFormulaList), "' = ", randomFormulaList, collapse = "\n\t\t"),")")
      rmaInput$random <- randomFormulaList
      if (length(struct) != 0)
        struct <- paste0("c(", paste0("'", names(struct), "' = '", struct, "'", collapse = ""),")")
      rmaInput$struct <- struct
    }
  }

  # specify method and fixed effect terms test
  rmaInput$method <- paste0("'", .maGetMethodOptions(options), "'")
  rmaInput$test   <- paste0("'", options[["fixedEffectTest"]], "'")

  if (!options[["weightedEstimation"]])
    rmaInput$weighted <- FALSE

  # add fixed parameters if needed
  if (options[["fixParametersWeights"]])
    rmaInput$weights <- as.name(options[["fixParametersWeightsVariable"]])
  if (options[["fixParametersTau2"]])
    rmaInput$tau2 <- .maGetFixedTau2Options(options)

  # add link function if needed
  if (.maIsMetaregressionHeterogeneity(options))
    rmaInput$link <- paste0("'", options[["heterogeneityModelLink"]], "'")

  if (.maIsMultilevelMultivariate(options)) {
    rmaInput$sparse <- if (options[["useSparseMatricies"]])       options[["useSparseMatricies"]]
    rmaInput$cvvc   <- if (!options[["computeCovarianceMatrix"]]) !options[["computeCovarianceMatrix"]]
  }

  # add control options if needed
  control <- .maGetControlOptions(options)
  if (length(control) != 0)
    rmaInput$control <- control

  # additional input
  rmaInput$level <- 100 * options[["confidenceIntervalsLevel"]]

  ### fit the model
  fit <- paste0("fit <- rma(\n\t", paste(names(rmaInput), "=", rmaInput, collapse = ",\n\t"), "\n)")

  # add clustering if specified
  if (options[["clustering"]] != "") {

    robustInput <- list(
      cluster      = as.name(options[["clustering"]]),
      clubSandwich = options[["clusteringUseClubSandwich"]],
      adjust       = options[["clusteringSmallSampleCorrection"]]
    )

    fit <- paste0(
      fit, "\n\n",
      "fit <- robust(\n",
      "\tfit,\n\t",
      paste(names(robustInput), "=", robustInput, collapse = ",\n\t"), "\n)"
    )
  }

  return(fit)
}
.maComputeVifSummary               <- function(fit, options, parameter = "effectSize") {

  if (options[["diagnosticsVarianceInflationFactorAggregate"]]) {

    # obtain terms indicies
    if (parameter == "effectSize") {
      terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")
      termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")
      tableVif   <- do.call(rbind, lapply(seq_along(terms), function(i) {
        cbind.data.frame(
          term = terms[i],
          .maExtractVifResults(metafor::vif(fit, btt = seq_along(termsIndex)[termsIndex == i]), options, parameter)
        )
      }))
    } else if (parameter == "heterogeneity") {
      terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")
      termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")
      tableVif   <- do.call(rbind, lapply(seq_along(terms), function(i) {
        cbind.data.frame(
          term = terms[i],
          .maExtractVifResults(metafor::vif(fit, att = seq_along(termsIndex)[termsIndex == i]), options, parameter)
        )
      }))
    }

  } else {

    tableVif      <- .maExtractVifResults(metafor::vif(fit), options, parameter)
    tableVif$term <- .maVariableNames(rownames(tableVif), options[["predictors"]])
  }

  return(tableVif)
}

# check functions
.maIsMetaregression               <- function(options) {
  return(.maIsMetaregressionEffectSize(options) || .maIsMetaregressionHeterogeneity(options))
}
.maIsMetaregressionEffectSize     <- function(options) {
  return(length(options[["effectSizeModelTerms"]]) > 0)
}
.maIsMetaregressionHeterogeneity  <- function(options) {
  return(length(options[["heterogeneityModelTerms"]]) > 0)
}
.maIsMetaregressionFtest          <- function(options) {
  return(options[["fixedEffectTest"]] %in% c("knha", "t"))
}
.maIsMultilevelMultivariate       <- function(options) {
  return(options[["module"]] == "metaAnalysisMultilevelMultivariate")
}
.maCheckIsPossibleOptions         <- function(options) {

  if (length(options[["heterogeneityModelTerms"]]) > 0 && options[["clustering"]] != "") {
    return(gettext("Clustering is not supported when specifying a heterogeneity meta-regression model."))
  }

  return(NULL)
}

# extract options
.maGetMethodOptions                   <- function(options) {

  switch(
    options[["method"]],
    "equalEffects"       = "EE",
    "fixedEffects"       = "FE",
    "maximumLikelihood"  = "ML",
    "restrictedML"       = "REML",
    "derSimonianLaird"   = "DL",
    "hedges"             = "HE",
    "hunterSchmidt"      = "HS",
    "hunterSchmidtSSC"   = "HSk",
    "sidikJonkman"       = "SJ",
    "empiricalBayes"     = "EB",
    "pauleMandel"        = "PM",
    "qeneralizedQStat"   = "GENQ",
    "qeneralizedQStatMu" = "GENQM",
    NA
  )
}
.maGetFixedTau2Options                <- function(options) {

  tau2 <- .parseRCodeInOptions(options[["fixParametersTau2Value"]])

  if (!is.numeric(tau2) || length(tau2) != 1 || tau2 < 0)
    .quitAnalysis(gettext("The fixed value for tau2 must be a positive number."))
  else
    return(tau2)
}
.maGetControlOptions                  <- function(options) {

  if (.maIsMetaregressionHeterogeneity(options)) {
    out <- list(
      optimizer = options[["optimizerMethod"]],
      iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
      rel.tol   = if (options[["optimizerConvergenceRelativeTolerance"]]) options[["optimizerConvergenceRelativeToleranceValue"]]
    )
  } else {
    if (.maGetMethodOptions(options) %in% c("REML", "ML", "EB")) {
      out <- list(
        tau2.init = if (options[["optimizerInitialTau2"]]) options[["optimizerInitialTau2Value"]],
        iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
        threshold = if (options[["optimizerConvergenceTolerance"]]) options[["optimizerConvergenceToleranceValue"]],
        stepadj   = if (options[["optimizerStepAdjustment"]]) options[["optimizerStepAdjustmentValue"]]
      )
    } else if (.maGetMethodOptions(options) %in% c("PM", "PMM", "GENQM")) {
      out <- list(
        iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
        tol       = if (options[["optimizerConvergenceTolerance"]]) options[["optimizerConvergenceToleranceValue"]],
        tau2.min  = if (options[["optimizerMinimumTau2"]]) options[["optimizerMinimumTau2Value"]],
        tau2.max  = if (options[["optimizerMaximumTau2"]]) options[["optimizerMaximumTau2Value"]]
      )
    } else if (.maGetMethodOptions(options) %in% c("SD")) {
      out <- list(
        tau2.init = if (options[["optimizerInitialTau2"]]) options[["optimizerInitialTau2Value"]]
      )
    } else {
      out <- list()
    }
  }
  return(out[!sapply(out, is.null)])
}
.maGetEffectSizeTransformationOptions <- function(effectSizeTransformation) {

  switch(
    effectSizeTransformation,
    none                          = function(x) x,
    fishersZToCorrelation         = metafor::transf.ztor,
    exponential                   = exp,
    logOddsToProportions          = metafor::transf.logit,
    logOddsToSmdNormal            = metafor::transf.lnortod.norm,
    logOddsToSmdLogistic          = metafor::transf.lnortod.logis,
    smdToLogOddsNormal            = metafor::transf.dtolnor.norm,
    smdToLogOddsLogistic          = metafor::transf.dtolnor.logis,
    hakstianAndWhalenInverseAlpha = metafor::transf.iahw,
    bonettInverseAlpha            = metafor::transf.iabt,
    zToR2                         = metafor::transf.ztor2,
    smdToCohensU1                 = metafor::transf.dtou1,
    smdToCohensU2                 = metafor::transf.dtou2,
    smdToCohensU3                 = metafor::transf.dtou3,
    smdToCles                     = metafor::transf.dtocles,
    stop(paste0("Unknown effect size transformation: ", effectSizeTransformation))
  )
}

# options names
.maGetOptionsNameEffectSizeTransformation <- function(effectSizeTransformation) {

  return(switch(
    effectSizeTransformation,
    "none"                           = NULL,
    "fishersZToCorrelation"          = gettext("Fisher's z to r"),
    "exponential"                    = gettext("Exponential"),
    "logOddsToProportions"           = gettext("Log odds to proportions"),
    "logOddsToSmdNormal"             = gettext("Log odds to SMD (normal)"),
    "logOddsToSmdLogistic"           = gettext("Log odds to SMD (logistic)"),
    "smdToLogOddsNormal"             = gettext("SMD to log odds (normal)"),
    "smdToLogOddsLogistic"           = gettext("SMD to log odds (logistic)"),
    "hakstianAndWhalenInverseAlpha"  = gettext("Hakstian & Whalen inverse α"),
    "bonettInverseAlpha"             = gettext("Bonett inverse α"),
    "zToR2"                          = gettext("Z to R²"),
    "smdToCohensU1"                  = gettext("SMD to Cohen's U₁"),
    "smdToCohensU2"                  = gettext("SMD to Cohen's U₂"),
    "smdToCohensU3"                  = gettext("SMD to Cohen's U₃"),
    "smdToCles"                      = gettext("SMD to CLES, Pr(supperiority)")
  ))
}
.maCasewiseDiagnosticsExportColumnsNames  <- function(columnName) {

  return(switch(
    columnName,
    "rstudent"  = "Standardized Residual",
    "dffits"    = "DFFITS",
    "cook.d"    = "Cook's Distance",
    "cov.r"     = "Covariance Ratio",
    "tau.del"   = "Tau",
    "tau2.del"  = "Tau2 LOO",
    "QE.del"    = "QE LOO",
    "hat"       = "Hat",
    "weight"    = "Weight",
    "inf"       = "Influential"
  ))
}

# misc
.maVariableNames                  <- function(varNames, variables) {

  return(sapply(varNames, function(varName){

    if (varName == "intrcpt")
      return("Intercept")

    for (vn in variables) {
      inf <- regexpr(vn, varName, fixed = TRUE)

      if (inf[1] != -1) {
        varName <- paste0(
          substr(varName, 0, inf[1] - 1),
          substr(varName, inf[1], inf[1] + attr(inf, "match.length") - 1),
          " (",
          substr(varName, inf[1] + attr(inf, "match.length"), nchar(varName))
        )
      }

    }

    varName <- gsub(":", paste0(")", jaspBase::interactionSymbol), varName, fixed = TRUE)
    varName <- paste0(varName, ")")
    varName <- gsub(" ()", "", varName, fixed = TRUE)
    varName <- gsub(" (/", "/", varName, fixed = TRUE)

    return(varName)

  }))
}
.maPrintQTest                     <- function(fit) {

  return(sprintf("Heterogeneity: Q(%1$i) = %2$.2f, %3$s", fit[["k"]] - fit[["p"]], fit[["QE"]], .maPrintPValue(fit[["QEp"]])))
}
.maPrintModerationTest            <- function(fit, options, parameter) {

  out      <- .maOmnibusTest(fit, options, parameter)
  outPrint <- .maPrintTermTest(out, testStatistic = TRUE)

  if (parameter == "effectSize")
    return(gettextf("Moderation: %1$s", outPrint))
  else if (parameter == "effectSize")
    return(gettextf("Moderation (Heterogeneity): %1$s", outPrint))
}
.maPrintHeterogeneityEstimate     <- function(fit, options, digits, keepText) {

  out <- .maComputePooledHeterogeneityPlot(fit, options)

  if (keepText)
    prefix <- gettext("Heterogeneity: ")
  else
    prefix <- "" # paste0(rep(" ", nchar(gettext("Heterogeneity: "))), collapse = "")

  return(sprintf(paste0(
    "%1$s tau = ",
    "%2$.", digits, "f",
    " [",
    "%3$.", digits, "f",
    ", ",
    "%4$.", digits, "f",
    "]"
    ), prefix, out$est, out$lCi, out$uCi))
}
.maPrintTermTest                  <- function(out, testStatistic = TRUE) {

  if (testStatistic) {
    if (!is.null(out[["df2"]])) {
      return(sprintf("F(%1$i, %2$.2f) = %3$.2f, %4$s", out[["df1"]], out[["df2"]], out[["stat"]], .maPrintPValue(out[["pval"]])))
    } else {
      return(sprintf("Q\U2098(%1$i) = %2$.2f, %3$s", out[["df1"]], out[["stat"]], .maPrintPValue(out[["pval"]])))
    }
  } else {
    return(.maPrintPValue(out[["pval"]]))
  }
}
.maPrintCoefficientTest           <- function(out, testStatistic = TRUE) {

  if (testStatistic) {
    if (!is.null(out[["df"]])) {
      return(sprintf("t(%1$.2f) = %2$.2f, %3$s", out[["df"]], out[["stat"]], .maPrintPValue(out[["pval"]])))
    } else {
      return(sprintf("z = %1$.2f, %2$s", out[["df1"]], out[["stat"]], .maPrintPValue(out[["pval"]])))
    }
  } else {
    return(.maPrintPValue(out[["pval"]]))
  }
}
.maPrintPValue                    <- function(pValue) {
  if (pValue < 0.001) {
    return("p < 0.001")
  } else {
    return(sprintf("p = %.3f", pValue))
  }
}
.maPrintEstimateAndInterval       <- function(est, lCi, uCi, digits) {
  return(sprintf(paste0(
    .maAddSpaceForPositiveValue(est), "%1$.", digits, "f",
    " [",
    .maAddSpaceForPositiveValue(lCi), "%2$.", digits, "f",
    ", ",
    .maAddSpaceForPositiveValue(uCi), "%3$.", digits, "f",
    "]"), est, lCi, uCi))
}
.maPrintPredictionInterval        <- function(est, lCi, uCi, digits) {
  return(sprintf(paste0(
    "   ", "%1$.", digits, "f",
    " [",
    .maAddSpaceForPositiveValue(lCi), "%2$.", digits, "f",
    ", ",
    .maAddSpaceForPositiveValue(uCi), "%3$.", digits, "f",
    "]"), est, lCi, uCi))
}
.maAddSpaceForPositiveValue       <- function(value) {
  if (value >= 0)
    return(" ")
  else
    return("")
}
.maMakeDiamondDataFrame           <- function(est, lCi, uCi, row, id, adj = 1/3) {
  return(data.frame(
    id       = id,
    x        = c(lCi,  est,     uCi,  est),
    y        = c(row,  row-adj, row,  row+adj),
    type     = "diamond",
    mapColor = NA
  ))
}
.maMakeRectangleDataFrame         <- function(lCi, uCi, row, id, adj = 1/5) {
  return(data.frame(
    id       = id,
    x        = c(lCi,     uCi,      uCi,      lCi),
    y        = c(row-adj, row-adj,  row+adj,  row+adj),
    type     = "rectangle",
    mapColor = NA
  ))
}
.maGetDigitsBeforeDecimal         <- function(x) {

  dNAs <- is.na(x)
  dPos <- floor(log10(x[!dNAs & x >= 0])) + 1
  dNeg <- floor(log10(-x[!dNAs & x < 0])) + 2

  # account for missing zeros
  dPos[dPos <= 1] <- 1
  dNeg[dNeg <= 1] <- 2 # (+2 because of minus sign)

  nDigits <- rep(NA, length(x))
  nDigits[!dNAs & x >= 0] <- dPos
  nDigits[!dNAs & x < 0]  <- dNeg

  return(nDigits)
}
.maFormatDigits                   <- function(x, digits) {

  xOut <- rep("", length(x))
  xNa  <- is.na(x)

  # compute the character width
  nDigits    <- .maGetDigitsBeforeDecimal(x[!xNa])
  nDigitsMax <- max(nDigits, na.rm = TRUE)
  addDigits  <- nDigitsMax - nDigits

  # add the missing widths
  xOut[!xNa] <- sprintf(paste0("%1$s%2$.", digits,"f"), sapply(addDigits, function(i) paste(rep(" ", i), collapse = "")), x[!xNa])
  xOut[ xNa] <- paste(rep(" ", nDigitsMax + 1 + digits), collapse = "")

  return(xOut)
}
.maBubblePlotMakeConfidenceBands  <- function(dfPlot, lCi = "lCi", uCi = "uCi") {

  if (!is.null(dfPlot[["separateLines"]])) {
    dfBands <- do.call(rbind, lapply(unique(dfPlot[["separateLines"]]), function(lvl) {
      dfSubset  <- dfPlot[dfPlot[["separateLines"]] == lvl,]
      dfPolygon <- data.frame(
        selectedVariable  = c(dfSubset$selectedVariable, rev(dfSubset$selectedVariable)),
        y                 = c(dfSubset[[lCi]],           rev(dfSubset[[uCi]]))
      )
      dfPolygon$separateLines <- lvl
      return(dfPolygon)
    }))
  } else {
    dfBands <- data.frame(
      selectedVariable = c(dfPlot$selectedVariable, rev(dfPlot$selectedVariable)),
      y                = c(dfPlot[[lCi]],           rev(dfPlot[[uCi]]))
    )
  }

  return(dfBands)
}
.maMergeVariablesLevels           <- function(df, variables, mergedName) {
  if (length(variables) == 1) {
    df[[mergedName]] <- df[,variables]
  } else if (length(variables) > 1) {
    df[[mergedName]] <- apply(df[,variables], 1, function(x) paste(x, collapse = " | "))
  }
  return(df)
}
.maTransformToHtml                <- function(rCode) {

  # Replace special characters with HTML entities
  htmlCode <- gsub("&", "&amp;", rCode)
  htmlCode <- gsub("<", "&lt;", htmlCode)
  htmlCode <- gsub(">", "&gt;", htmlCode)

  # Wrap the code in <pre> and <code> tags
  htmlCode <- paste0(
    "<pre><code>", htmlCode, "\n</code></pre>"
  )

  return(htmlCode)
}
.maExtractVifResults              <- function(vifResults, options, parameter) {

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
.maGetVariableColumnType          <- function(variable, options) {

  if (variable %in% c(options[["effectSize"]], options[["effectSizeStandardError"]])) {
    return("number")
  } else if (variable == options[["clustering"]]) {
    return("string")
  } else if (variable %in% options[["predictors"]]){
    return(switch(
      options[["predictors.types"]][variable == options[["predictors"]]],
      "scale"   = "number",
      "nominal" = "string"
    ))
  } else {
    return("string")
  }
}
.maSuppressPlot                   <- function(plotExpression) {
  temp <- tempfile()
  pdf(file = temp)
  dfOut <- plotExpression
  dev.off()
  unlink(temp)
  return(dfOut)
}

# messages
.maFixedEffectTextMessage              <- function(options) {
  return(switch(
    options[["fixedEffectTest"]],
    "z"    = gettext("Fixed effect tested using z-distribution."),
    "t"    = gettext("Fixed effect tested using t-distribution."),
    "knha" = gettext("Fixed effect tested using Knapp and Hartung adjustment."),
    stop(paste0("Unknown fixed effect test.", options[["fixedEffectTest"]]))
  ))
}
.meMetaregressionHeterogeneityMessages <- function(options) {

  if (options[["heterogeneityModelLink"]] == "log")
    return(gettext("The heterogeneity model for \U1D70F\U00B2 is specified on the log scale."))
  else if (options[["heterogeneityModelLink"]] == "identity")
    return(gettext("The heterogeneity model for \U1D70F\U00B2 is specified on the identity scale."))
}
.maPooledEstimatesMessages             <- function(fit, dataset, options) {

  messages <- NULL

  if (options[["clustering"]] != "") {
    if (all(fit[["tcl"]][1] == fit[["tcl"]]))
      messages <- c(messages, gettextf("%1$i clusters with %2$i estimates each.", fit[["n"]],  fit[["tcl"]][1]))
    else
      messages <- c(messages, gettextf("%1$i clusters with min/median/max %2$i/%3$i/%4$i estimates.", fit[["n"]],  min(fit[["tcl"]]), median(fit[["tcl"]]), max(fit[["tcl"]])))
  }

  if (options[["transformEffectSize"]] != "none")
    messages <- c(messages, gettextf("The pooled effect size is transformed using %1$s transformation.", .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))

  if (.maIsMetaregressionEffectSize(options))
    messages <- c(messages, gettext("The pooled effect size corresponds to the weighted average effect across studies."))

  if (.maIsMetaregressionHeterogeneity(options))
    messages <- c(messages, gettext("The pooled heterogeneity estimate corresponds to the heterogeneity at the average of predictor values."))

  if (.maIsMetaregressionHeterogeneity(options) && (options[["heterogeneityI2"]] || options[["heterogeneityH2"]]))
    messages <- c(messages, gettext("The I² and H² statistics are not available for heterogeneity models."))

  if (attr(dataset, "NAs") > 0)
    messages <- c(messages, gettextf("%1$i observations were ommited due to missing values.", attr(dataset, "NAs")))

  if (!is.null(attr(dataset, "influentialObservations")) && attr(dataset, "influentialObservations") > 0)
    messages <- c(messages, gettextf("%1$i influential observations were detected and removed.", attr(dataset, "influentialObservations")))

  if (.maIsMultilevelMultivariate(options) && any(attr(fit, "skipped")))
    messages <- c(messages, gettextf("The random component %1$s was not completely specified and was skipped.", paste0(which(attr(fit, "skipped")), collapse = " and ")))

  return(messages)
}
.maEstimatedMarginalMeansMessages      <- function(options, parameter) {

  messages <- gettext("Each marginal mean estimate is averaged across the levels of the remaining predictors.")

  if (parameter == "effectSize" && options[["transformEffectSize"]] != "none")
    messages <- c(messages, gettextf("The estimates and intervals are transformed using %1$s transformation.", .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))

  if (parameter == "heterogeneity")
    messages <- c(messages, gettextf("The estimates and intervals correspond to %1$s.", switch(
      options[["estimatedMarginalMeansHeterogeneityTransformation"]],
      "tau"  = gettext("\U1D70F"),
      "tau2" = gettext("\U1D70F\U00B2")
    )))

  return(messages)
}
