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


  return()
}


.maGetFormula      <- function(modelTerms, includeIntercept) {

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
.maFitModel        <- function(jaspResults, dataset, options) {

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
.maSummaryTable    <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["modelSummary"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  modelSummary <- createJaspContainer(gettext("Model Summary"))
  modelSummary$dependOn(.maDependencies)
  jaspResults[["modelSummary"]] <- modelSummary


  ### residual heterogeneity table
  residualHeterogeneityTable          <- createJaspTable(gettext("Residual Heterogeneity Test"))
  residualHeterogeneityTable$position <- 1

  residualHeterogeneityTable$addColumnInfo(name = "qstat", type = "number",  title = gettext("QE"))
  residualHeterogeneityTable$addColumnInfo(name = "df",    type = "integer", title = gettext("df"))
  residualHeterogeneityTable$addColumnInfo(name = "pval",  type = "pvalue",  title = gettext("p"))

  modelSummary[["residualHeterogeneityTable"]] <- residualHeterogeneityTable


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
    if (.maIsMetaregressionFtest(options)) {
      moderatorsTable$addColumnInfo(name = "df2", type = if(options$clustering == "") "integer" else "number", title = gettext("df2"))
    }
    moderatorsTable$addColumnInfo(name = "pval",  type = "pvalue",  title = gettext("p"))

    modelSummary[["moderatorsTable"]] <- moderatorsTable
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
      df2   = if(.maIsMetaregressionFtest(options)) fit[["QMdf"]][2],
      pval  = fit[["QMp"]]
    )

    if (.maIsMetaregressionHeterogeneity(options)) {

      row2 <- list(
        stat  = fit[["QS"]],
        df1   = fit[["QSdf"]][1],
        df2   = if(.maIsMetaregressionFtest(options)) fit[["QSdf"]][2],
        pval  = fit[["QSp"]]
      )

      row1$parameter <- gettext("Effect size")
      row2$parameter <- gettext("Hetereogeneity")
    }

    moderatorsTable$addRows(row1)

    if (.maIsMetaregressionHeterogeneity(options))
      moderatorsTable$addRows(row2)

    if (options[["clustering"]] != "")
      moderatorsTable$addFootnote(.maClusteringMessage(fit), symbol = gettext("Clustering:"))
  }
}
.maCoeffTable      <- function(container, dataset, options, ready) {
  if (!options$coefficientEstimate || !is.null(container[["coeffTable"]]))
    return()

  coeffTable <- createJaspTable(gettext("Coefficients"))
  coeffTable$dependOn(c("coefficientEstimate", "coefficientCi"))
  coeffTable$position <- 2
  coeffTable$showSpecifiedColumnsOnly <- TRUE
  coeffTable$addCitation("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/")

  coeffTable$addColumnInfo(name = "name",  type = "string", title = "")
  coeffTable$addColumnInfo(name = "est",   type = "number", title = gettext("Estimate"))
  coeffTable$addColumnInfo(name = "se",    type = "number", title = gettext("Standard Error"))
  if (options[["estimateTest"]] == "z")
    coeffTable$addColumnInfo(name = "zval",  type = "number", title = gettext("z"))
  else if (options[["estimateTest"]] == "knha") {
    coeffTable$addColumnInfo(name = "tval",  type = "number", title = gettext("t"))
    coeffTable$addColumnInfo(name = "df",    type = "number", title = gettext("df"))
  }
  coeffTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))
  .metaAnalysisConfidenceInterval(options, coeffTable)

  coeffTable$addFootnote(switch(options$estimateTest, z = gettext("Wald test."), knha = gettext("Knapp and Hartung test adjustment.")))

  container[["coeffTable"]] <- coeffTable
  if(!ready)
    return()

  res <- try(.metaAnalysisCoeffFill(container, dataset, options))

  .metaAnalysisSetError(res, coeffTable)
}


.maExtractFit                    <- function(jaspResults, options) {

  if (is.null(jaspResults[["fit"]]$object))
    return()

  # extract clustered model if specified
  if (options[["clustering"]] != "") {
    return(jaspResults[["fit"]]$object[["fitClustered"]])
  } else {
    return(jaspResults[["fit"]]$object[["fit"]])
  }
}
.maIsMetaregression              <- function(options) {
  return(length(options[["effectSizeModelTerms"]]) > 0)
}
.maIsMetaregressionHeterogeneity <- function(options) {
  return(length(options[["heterogeneityModelTerms"]]) > 0)
}
.maIsMetaregressionFtest         <- function(options) {
  return(options[["fixedEffectTest"]] %in% c("knha", "r"))
}
.maCheckIsPossibleOptions        <- function(options) {

  if (length(options[["heterogeneityModelTerms"]]) > 0 && options[["clustering"]] != "") {
    return(gettext("Clustering is not supported when specifying a heterogeneity meta-regression model."))
  }

  return(NULL)
}
.maClusteringMessage             <- function(fit) {

  if (all(fit[["tcl"]][1] == fit[["tcl"]])) {
    return(gettextf("%1$i clusters with %2$i estimates each.", fit[["n"]],  fit[["tcl"]][1]))
  } else {
    return(gettextf("%1$i clusters with min/median/max %2$i/%3$i/%4$i estimates.", fit[["n"]],  min(fit[["tcl"]]), median(fit[["tcl"]]), max(fit[["tcl"]])))
  }
}
.maGetMethodOptions <- function(options) {
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
