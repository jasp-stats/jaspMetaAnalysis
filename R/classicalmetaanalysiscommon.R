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
    if (options[["metaregressionTermsTests"]]) {
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

  # add fixed parameters if needed
  if (options[["fixParametersWeights"]])
    rmaInput$weights <- as.name(options[["fixParametersWeightsVariable"]])
  if (options[["fixParametersTau2"]])
    rmaInput$tau2 <- .maGetFixedTau2Options(options)
# TODO: add link: heterogeneityModelLink
  # add labels if specified
  if (options[["studyLabel"]] != "")
    rmaInput$slab <- as.name(options[["studyLabel"]])

  # add formulas if specified
  rmaInput$mods  <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  rmaInput$scale <- .maGetFormula(options[["heterogeneityModelTerms"]], options[["heterogeneityModelIncludeIntercept"]])

  # specify method and fixed effect terms test
  rmaInput$method <- .maGetMethodOptions(options)
  rmaInput$test   <- options[["fixedEffectTest"]]

  # additional input
  rmaInput$level <- 100 * options[["confidenceIntervalsLevel"]]

  # fit the model
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

  residualHeterogeneityTable$addColumnInfo(name = "qstat", type = "number",  title = gettext("QE"))
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
  moderatorsTable$addColumnInfo(name = "stat", type = "number",   title = if(.maIsMetaregressionFtest(options)) gettext("F")   else gettext("QM"))
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
  pooledEstimatesTable$dependOn(c("heterogeneityTau", "heterogeneityTau2", "heterogeneityI2", "heterogeneityH2", "confidenceIntervals", "confidenceIntervalsLevel", "heterogeneityPredictionInterval"))
  modelSummaryContainer[["pooledEstimatesTable"]] <- pooledEstimatesTable

  pooledEstimatesTable$addColumnInfo(name = "par",  type = "string", title = "")
  pooledEstimatesTable$addColumnInfo(name = "est",  type = "number", title = gettext("Estimate"))
  if (options[["confidenceIntervals"]]) {
    overtitleCi <- gettextf("%s%% CI", 100 * options[["confidenceIntervalsLevel"]])
    pooledEstimatesTable$addColumnInfo(name = "lCi", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    pooledEstimatesTable$addColumnInfo(name = "uCi", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }
  if (options[["heterogeneityPredictionInterval"]]) {
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
  termsTable$dependOn("metaregressionTermsTests")
  metaregressionContainer[[paste0(parameter, "TermsTable")]] <- termsTable

  termsTable$addColumnInfo(name = "term",  type = "string",  title = "")
  termsTable$addColumnInfo(name = "stat",  type = "number",  title = if(.maIsMetaregressionFtest(options)) gettext("F")   else gettext("QM"))
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
    termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")
    termsTests <- do.call(rbind.data.frame, lapply(seq_along(terms), function(i) {

      termsAnova <- anova(fit, btt = seq_along(termsIndex)[termsIndex == i])

      out <- list(
        term = .maVariableNames(terms[i], options[["predictors"]]),
        stat = termsAnova[["QM"]],
        df1  = termsAnova[["QMdf"]][1],
        pval = termsAnova[["QMp"]]
      )

      if (.maIsMetaregressionFtest(options))
        out$df2 <- termsAnova[["QMdf"]][2]

      return(out)
    }))

    termsTable$setData(termsTests)

  } else if (parameter == "heterogeneity") {

    if (!.maIsMetaregressionHeterogeneity(options))
      return()

    terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")
    termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")
    termsTests <- do.call(rbind.data.frame, lapply(seq_along(terms), function(i) {

      termsAnova <- anova(fit, btt = seq_along(termsIndex)[termsIndex == i])

      out <- list(
        term = .maVariableNames(terms[i], options[["predictors"]]),
        stat = termsAnova[["QM"]],
        df1  = termsAnova[["QMdf"]][1],
        pval = termsAnova[["QMp"]]
      )

      if (.maIsMetaregressionFtest(options))
        out$df2 <- termsAnova[["QMdf"]][2]

      return(out)
    }))

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

# containers/state functions
.maExtractFit                     <- function(jaspResults, options) {

  if (is.null(jaspResults[["fit"]]$object))
    return()

  # extract clustered model if specified
  if (options[["clustering"]] != "") {
    return(jaspResults[["fit"]]$object[["fitClustered"]])
  } else {
    return(jaspResults[["fit"]]$object[["fit"]])
  }
}
.maExtractModelSummaryContainer   <- function(jaspResults) {

  if (!is.null(jaspResults[["modelSummaryContainer"]]))
    return(jaspResults[["modelSummaryContainer"]])

  # create the output container
  modelSummaryContainer <- createJaspContainer(gettext("Model Summary"))
  modelSummaryContainer$dependOn(.maDependencies)
  modelSummaryContainer$position <- 1
  jaspResults[["modelSummaryContainer"]] <- modelSummaryContainer

  return(modelSummaryContainer)
}
.maExtractMetaregressionContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["metaregressionContainer"]]))
    return(jaspResults[["metaregressionContainer"]])

  # create the output container
  metaregressionContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  metaregressionContainer$dependOn(c(.maDependencies, "confidenceInterval"))
  metaregressionContainer$position <- 2
  jaspResults[["metaregressionContainer"]] <- metaregressionContainer

  return(metaregressionContainer)
}

# help compute functions
.maComputePooledEffect            <- function(fit, options) {

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

  keepResults <- c(
    "par",
    "est",
    if (options[["confidenceIntervals"]]) "lCi",
    if (options[["confidenceIntervals"]]) "uCi",
    if (options[["heterogeneityPredictionInterval"]]) "lPi",
    if (options[["heterogeneityPredictionInterval"]]) "uPi"
  )

  predictedEffect <- predictedEffect[,keepResults]
  return(as.list(predictedEffect))
}
.maComputePooledHeterogeneity     <- function(fit, options) {

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

    confIntHeterogeneity <- data.frame(
      par = c("\U1D70F", "\U1D70F\U00B2"),
      est = exp(c(predScale[["pred"]]  / 2, predScale[["pred"]])),
      lCi = exp(c(predScale[["ci.lb"]] / 2, predScale[["ci.lb"]])),
      uCi = exp(c(predScale[["ci.ub"]] / 2, predScale[["ci.ub"]]))
    )

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
.maOmnibusTest                    <- function(fit, options, parameter = "effectSize") {

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
.maOmnibusTestCoefficients        <- function(fit, options, parameter = "effectSize") {

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
.maGetMethodOptions               <- function(options) {
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
.maGetFixedTau2Options            <- function(options) {

  tau2 <- .parseRCodeInOptions(options[["fixParametersTau2Value"]])

  if (!is.numeric(tau2) || length(tau2) != 1 || tau2 < 0)
    .quitAnalysis(gettext("The fixed value for tau2 must be a positive number."))
  else
    return(tau2)
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

# messages
.maFixedEffectTextMessage         <- function(options) {
  return(switch(
    options[["fixedEffectTest"]],
    "z"    = gettext("Fixed effect tested using z-distribution."),
    "t"    = gettext("Fixed effect tested using t-distribution."),
    "knha" = gettext("Fixed effect tested using Knapp and Hartung adjustment.")
  ))
}
.maPooledEstimatesMessages        <- function(fit, dataset, options) {

  messages <- NULL

  if (options[["clustering"]] != "") {
    if (all(fit[["tcl"]][1] == fit[["tcl"]]))
      messages <- c(messages, gettextf("%1$i clusters with %2$i estimates each.", fit[["n"]],  fit[["tcl"]][1]))
    else
      messages <- c(messages, gettextf("%1$i clusters with min/median/max %2$i/%3$i/%4$i estimates.", fit[["n"]],  min(fit[["tcl"]]), median(fit[["tcl"]]), max(fit[["tcl"]])))
  }

  if (.maIsMetaregressionEffectSize(options))
    messages <- c(messages, gettext("The pooled effect size corresponds to the weighted average effect across studies."))

  if (.maIsMetaregressionHeterogeneity(options))
    messages <- c(messages, gettext("The pooled heterogeneity estimate corresponds to the weighted average heterogeneity across studies."))

  if (.maIsMetaregressionHeterogeneity(options) && (options[["heterogeneityI2"]] || options[["heterogeneityH2"]]))
    messages <- c(messages, gettext("The I² and H² statistics are not available for heterogeneity models."))

  if (length(attr(dataset, "na.action")) > 0)
    messages <- c(messages, gettextf("%1$i observations were ommited due to missing values.", length(attr(dataset, "na.action"))))

  return(messages)
}
