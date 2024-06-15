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
  .maSummaryTable(jaspResults, dataset, options)

  # meta-regression tables
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

  return()
}


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
.maSummaryTable     <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["modelSummaryContainer"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  modelSummaryContainer <- createJaspContainer(gettext("Model Summary"))
  modelSummaryContainer$dependOn(.maDependencies)
  jaspResults[["modelSummaryContainer"]] <- modelSummaryContainer


  ### residual heterogeneity table
  residualHeterogeneityTable          <- createJaspTable(gettext("Residual Heterogeneity Test"))
  residualHeterogeneityTable$position <- 1

  residualHeterogeneityTable$addColumnInfo(name = "qstat", type = "number",  title = gettext("QE"))
  residualHeterogeneityTable$addColumnInfo(name = "df",    type = "integer", title = gettext("df"))
  residualHeterogeneityTable$addColumnInfo(name = "pval",  type = "pvalue",  title = gettext("p"))

  modelSummaryContainer[["residualHeterogeneityTable"]] <- residualHeterogeneityTable


  ### moderators table
  if (.maIsMetaregression(options)) {

    moderatorsTable          <- createJaspTable(gettext("Omnibus Moderation Test"))
    moderatorsTable$position <- 2

    # add column name for the omnibus test if both effect size and scale moderators are specified
    if (.maIsMetaregressionHeterogeneity(options))
      moderatorsTable$addColumnInfo(name = "parameter", type = "string",  title = gettext("Parameter"))

    # dispatch columns based on the test type
    moderatorsTable$addColumnInfo(name = "stat", type = "number",   title = if(.maIsMetaregressionFtest(options)) gettext("F")   else gettext("QM"))
    moderatorsTable$addColumnInfo(name = "df1",  type = "integer",  title = if(.maIsMetaregressionFtest(options)) gettext("df1") else gettext("df"))
    if (.maIsMetaregressionFtest(options))
      moderatorsTable$addColumnInfo(name = "df2", type = "number", title = gettext("df2"))
    moderatorsTable$addColumnInfo(name = "pval",  type = "pvalue",  title = gettext("p"))

    modelSummaryContainer[["moderatorsTable"]] <- moderatorsTable
  }


  ### stop on error
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


  ### fill tables
  residualHeterogeneityTable$addRows(list(
    qstat = fit[["QE"]],
    df    = fit[["k"]] - fit[["p"]],
    pval  = fit[["QEp"]]
  ))

  if (.maIsMetaregression(options)) {

    row1 <- list(
      stat = fit[["QM"]],
      df1   = fit[["QMdf"]][1],
      pval  = fit[["QMp"]]
    )

    if (.maIsMetaregressionFtest(options))
      row1$df2 <- fit[["QMdf"]][2]

    if (.maIsMetaregressionHeterogeneity(options)) {

      row2 <- list(
        stat  = fit[["QS"]],
        df1   = fit[["QSdf"]][1],
        pval  = fit[["QSp"]]
      )

      if (.maIsMetaregressionFtest(options))
        row2$df2 <- fit[["QSdf"]][2]

      row1$parameter <- gettext("Effect size")
      row2$parameter <- gettext("Heterogeneity")
    }

    moderatorsTable$addRows(row1)

    if (.maIsMetaregressionHeterogeneity(options))
      moderatorsTable$addRows(row2)

    if (options[["clustering"]] != "")
      moderatorsTable$addFootnote(.maClusteringMessage(fit), symbol = gettext("Clustering:"))
  }

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

  termsTable$addColumnInfo(name = "term",  type = "string", title = "")
  termsTable$addColumnInfo(name = "stat", type = "number",   title = if(.maIsMetaregressionFtest(options)) gettext("F")   else gettext("QM"))
  termsTable$addColumnInfo(name = "df1",  type = "integer",  title = if(.maIsMetaregressionFtest(options)) gettext("df1") else gettext("df"))
  if (.maIsMetaregressionFtest(options)) {
    termsTable$addColumnInfo(name = "df2", type = "number", title = gettext("df2"))
  }
  termsTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))
  termsTable$addFootnote(.maFixedEffectTextMessage(options))

  if (is.null(fit) || jaspBase::isTryError(fit))
    return()

  if (parameter == "effectSize") {

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
.maExtractMetaregressionContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["metaregressionContainer"]]))
    return(jaspResults[["metaregressionContainer"]])

  # create the output container
  metaregressionContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  metaregressionContainer$dependOn(c(.maDependencies, "confidenceInterval"))
  jaspResults[["metaregressionContainer"]] <- metaregressionContainer

  return(metaregressionContainer)
}
.maIsMetaregression               <- function(options) {
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
.maClusteringMessage              <- function(fit) {

  if (all(fit[["tcl"]][1] == fit[["tcl"]])) {
    return(gettextf("%1$i clusters with %2$i estimates each.", fit[["n"]],  fit[["tcl"]][1]))
  } else {
    return(gettextf("%1$i clusters with min/median/max %2$i/%3$i/%4$i estimates.", fit[["n"]],  min(fit[["tcl"]]), median(fit[["tcl"]]), max(fit[["tcl"]])))
  }
}
.maFixedEffectTextMessage         <- function(options) {
  return(switch(
    options[["fixedEffectTest"]],
    "z"    = gettext("Fixed effect tested using z-distribution."),
    "t"    = gettext("Fixed effect tested using t-distribution."),
    "knha" = gettext("Fixed effect tested using Knapp and Hartung adjustment.")
  ))
}
.maGetMethodOptions               <- function(options) {
  switch(
    options[["method"]],
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
