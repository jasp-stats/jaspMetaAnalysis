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

.ClassicalMetaAnalysisCommon <- function(jaspResults, dataset, options, ...) {

  .maFitModel(jaspResults, dataset, options)

  # model summary
  .maResidualHeterogeneityTable(jaspResults, dataset, options)
  .maModeratorsTable(jaspResults, dataset, options)
  .maPooledEstimatesTable(jaspResults, dataset, options)
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

  return()
}

# fitting functions
.maGetFormula       <- function(modelTerms, includeIntercept) {

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
.maFitModel         <- function(jaspResults, dataset, options) {

  if (!.maReady(options) || !is.null(jaspResults[["fit"]]))
    return()

  # create the output container
  fitContainer <- createJaspState()
  fitContainer$dependOn(.maDependencies)
  jaspResults[["fit"]] <- fitContainer

  # specify the effect size and outcome
  rmaInput <- list(
    yi   = as.name(options[["effectSize"]]),
    sei  = as.name(options[["effectSizeStandardError"]]),
    data = dataset
  )

  # add formulas if specified
  rmaInput$mods  <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  rmaInput$scale <- .maGetFormula(options[["heterogeneityModelTerms"]], options[["heterogeneityModelIncludeIntercept"]])

  # specify method and fixed effect terms test
  rmaInput$method <- .maGetMethodOptions(options)
  rmaInput$test   <- options[["fixedEffectTest"]]

  if (!options[["weightedEstimation"]])
    rmaInput$weighted <- FALSE

  # add fixed parameters if needed
  if (options[["fixParametersWeights"]])
    rmaInput$weights <- as.name(options[["fixParametersWeightsVariable"]])
  if (options[["fixParametersTau2"]])
    rmaInput$tau2 <- .maGetFixedTau2Options(options)

  # add link function if needed
  if (.maIsMetaregressionHeterogeneity(options))
    rmaInput$link <- options[["heterogeneityModelLink"]]

  # add control options if needed
  control <- .maGetControlOptions(options)
  if (length(control) != 0)
    rmaInput$control <- control

  # additional input
  rmaInput$level <- 100 * options[["confidenceIntervalsLevel"]]

  ### fit the model
  fit <- try(do.call(metafor::rma, rmaInput))

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

  # return the results
  jaspResults[["fit"]]$object <- list(
    fit          = fit,
    fitClustered = fitClustered
  )

  return()
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
  pooledEstimatesTable$position <- 3
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
  if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {

    # requires non-clustered fit
    pooledHeterogeneity <- .maComputePooledHeterogeneity(jaspResults[["fit"]]$object[["fit"]], options)

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


  fitMeasuresTable$addColumnInfo(name = "ll",   title = gettext("Log Lik."), type = "number")
  fitMeasuresTable$addColumnInfo(name = "dev",  title = gettext("Deviance"), type = "number")
  fitMeasuresTable$addColumnInfo(name = "AIC",  title = gettext("AIC"),      type = "number")
  fitMeasuresTable$addColumnInfo(name = "BIC",  title = gettext("BIC"),      type = "number")
  fitMeasuresTable$addColumnInfo(name = "AICc", title = gettext("AICc"),     type = "number")

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  fitMeasuresTable$setData(t(fit[["fit.stats"]]))

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

# containers/state functions
.maExtractFit                             <- function(jaspResults, options) {

  if (is.null(jaspResults[["fit"]]$object))
    return()

  # extract clustered model if specified
  if (options[["clustering"]] != "") {
    return(jaspResults[["fit"]]$object[["fitClustered"]])
  } else {
    return(jaspResults[["fit"]]$object[["fit"]])
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
  metaregressionContainer$position <- 2
  jaspResults[["metaregressionContainer"]] <- metaregressionContainer

  return(metaregressionContainer)
}
.maExtractEstimatedMarginalMeansContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["estimatedMarginalMeansContainer"]]))
    return(jaspResults[["estimatedMarginalMeansContainer"]])

  # create the output container
  estimatedMarginalMeansContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  estimatedMarginalMeansContainer$dependOn(c(.maDependencies, "confidenceIntervals", "confidenceIntervalsLevel"))
  estimatedMarginalMeansContainer$position <- 3
  jaspResults[["estimatedMarginalMeansContainer"]] <- estimatedMarginalMeansContainer

  return(estimatedMarginalMeansContainer)
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

  # add empty prediction interval for FE and EE
  if (.maGetMethodOptions(options) %in% c("FE", "EE")) {
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
  } else if (parameter == "heterogeneity") {
    terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")
    termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")
    termsAnova <- anova(fit, att = seq_along(termsIndex)[termsIndex == which(terms == term)])
  }

  out <- list(
    term = .maVariableNames(term, options[["predictors"]]),
    stat = termsAnova[["QM"]],
    df1  = termsAnova[["QMdf"]][1],
    pval = termsAnova[["QMp"]]
  )

  if (.maIsMetaregressionFtest(options))
    out$df2 <- termsAnova[["QMdf"]][2]

  return(out)
}
.maGetMarginalMeansPredictorMatrix <- function(fit, options, dataset, selectedVariable, parameter) {

  variablesContinuous <- options[["predictors"]][options[["predictors.types"]] == "scale"]
  variablesFactors    <- options[["predictors"]][options[["predictors.types"]] == "nominal"]

  # extract the corresponding formula
  formula <- switch(
    parameter,
    effectSize    = fit[["formula.mods"]],
    heterogeneity = fit[["formula.scale"]]
  )

  # select SD factor for covariates
  sdFactor <- switch(
    parameter,
    effectSize    = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
    heterogeneity = options[["estimatedMarginalMeansHeterogeneitySdFactorCovariates"]]
  )

  # extract the used variables
  terms     <- attr(terms(formula, data = fit[["data"]]), "term.labels")
  variables <- terms[!grepl(":", terms)]

  # average across remaining variables
  remainingVariables <- variables[variables != selectedVariable]

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
  if (selectedVariable %in% variablesFactors) {
    selectedPredictor <- factor(levels(dataset[[selectedVariable]]), levels = levels(dataset[[selectedVariable]]))
    contrasts(selectedPredictor) <- contrasts(dataset[[selectedVariable]])
  } else if (selectedVariable %in% variablesContinuous) {
    selectedPredictor <- c(
      mean(dataset[[selectedVariable]]) - sdFactor * sd(dataset[[selectedVariable]]),
      mean(dataset[[selectedVariable]]),
      mean(dataset[[selectedVariable]]) + sdFactor * sd(dataset[[selectedVariable]])
    )
  }

  # add the specified variable and pool across the combinations of the remaining values
  if (selectedVariable == "") {
    # empty string creates overall adjusted estimate
    outMatrix <- colMeans(model.matrix(formula, data = expand.grid(predictorsRemaining)))[-1]
  } else {
    outMatrix <- do.call(rbind, lapply(seq_along(selectedPredictor), function(i) {
      predictorsRemaining[[selectedVariable]] <- selectedPredictor[i]
      outMatrix <- model.matrix(formula, data = expand.grid(predictorsRemaining))
      return(colMeans(outMatrix)[-1])
    }))
  }


  # keep information about the variable and levels
  if (selectedVariable == "")
    attr(outMatrix, "variable") <- gettext("Adjusted Estimate")
  else
    attr(outMatrix, "variable") <- selectedVariable

  if (selectedVariable %in% variablesFactors)
    attr(outMatrix, "at") <- selectedPredictor
  else if (selectedVariable %in% variablesContinuous)
    attr(outMatrix, "at") <- c(
      gettextf("Mean - %1$sSD", sdFactor),
      gettext("Mean"),
      gettextf("Mean + %1$sSD", sdFactor))
  else
    attr(outMatrix, "at") <- ""

  return(outMatrix)
}
.maComputeMarginalMeansVariable    <- function(fit, options, dataset, selectedVariable, testAgainst = 0, parameter) {

  if (parameter == "effectSize") {
    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(fit, options, dataset, selectedVariable, "effectSize")

    if (.maIsMetaregressionHeterogeneity(options)) {

      predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(fit, options, dataset, selectedVariable, "heterogeneity")
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
    computedMarginalMeans <- cbind(data.frame("variable" = attr(predictorMatrixEffectSize, "variable"), "value" = attr(predictorMatrixEffectSize, "at")), computedMarginalMeans)

  } else if (parameter == "heterogeneity") {

    predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(fit, options, dataset, selectedVariable, "heterogeneity")

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
    computedMarginalMeans <- cbind(data.frame("variable" = attr(predictorMatrixHeterogeneity, "variable"), "value" = attr(predictorMatrixHeterogeneity, "at")), computedMarginalMeans)
  }


  # remove unnecessary columns
  computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% "se"]

  if (!options[["confidenceIntervals"]])
    computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% c("lCi", "uCi")]

  if (!options[["predictionIntervals"]])
    computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% c("lPi", "uPi")]

  return(computedMarginalMeans)
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
    none                          = NULL,
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
    prefix <- paste0(rep(" ", nchar(gettext("Heterogeneity: "))), collapse = "")

  return(sprintf(paste0(
    "%1$s \U1D70F = ",
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
.maMakeRectangleDataFrame         <- function(lCi, uCi, row, id, adj = 1/4) {
  return(data.frame(
    id       = id,
    x        = c(lCi,     uCi,      uCi,      lCi),
    y        = c(row-adj, row-adj,  row+adj,  row+adj),
    type     = "rectangle",
    mapColor = NA
  ))
}
.maGetMaxDigitsBeforeDecimal      <- function(x) {

  dPos <- floor(log10(x[x >= 0])) + 1
  dNeg <- floor(log10(-x[x < 0])) + 2 # (+2 because of minus sign)

  # account for missing zeros
  dPos[dPos == 0] <- 1
  dNeg[dNeg == 0] <- 1

  return(max(c(dPos, dNeg)))
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
