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


ClassicalMetaAnalysisMultilevelMultivariate <- function(jaspResults, dataset = NULL, options, ...) {

  options[["module"]] <- "metaAnalysisMultilevelMultivariate"
  saveRDS(options, file = "C:/JASP/options.RDS")

  if (.maReady(options)) {
    dataset <- .mammReadData(dataset, options)
    .mammCheckErrors(dataset, options)
  }

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  .ClassicalMetaAnalysisCommon(jaspResults, dataset, options)

  return()
  options <- readRDS(file = "C:/JASP/options.RDS")
  dataset <- readRDS(file = "C:/JASP/dataset.RDS")
  return()
}

.mammReadData                    <- function(dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  # model data
  predictorsNominal <- options[["predictors"]][options[["predictors.types"]] == "nominal"]
  predictorsScale   <- options[["predictors"]][options[["predictors.types"]] == "scale"]

  ### main model variables
  asFactors <- c(
    if (length(predictorsNominal) > 0) predictorsNominal,
    if (options[["clustering"]] != "") options[["clustering"]]
  )
  asNumeric <- c(
    options[["effectSize"]],
    options[["effectSizeStandardError"]],
    if (length(predictorsScale) > 0) predictorsScale,
    if (options[["fixParametersWeights"]]) options[["fixParametersWeightsVariable"]]
  )

  ### add random effects variables
  randomVariables <- .mammExtractRandomVariableNames(options)

  # check variable types cross-loading
  if (length(randomVariables$nominal) > 0 && any(randomVariables$nominal %in% c(randomVariables$scale, randomVariables$ordinal, asNumeric)))
    .quitAnalysis(gettextf("The following variable was specified both as nominal and scale: %1$s", randomVariables$nominal[randomVariables$nominal %in% c(randomVariables$scale, randomVariables$ordinal, asNumeric)]))
  if (length(randomVariables$scale) > 0 && any(randomVariables$scale %in% c(randomVariables$nominal, asFactors)))
    .quitAnalysis(gettextf("The following variable was specified both as scale and nominal: %1$s", randomVariables$nominal[randomVariables$scale %in% c(randomVariables$nominal, asFactors)]))

  asNumeric <- c(asNumeric, randomVariables$scale, randomVariables$ordinal)
  asFactors <- c(asFactors, randomVariables$nominal)

  # forest plotting data
  additionalVariables <- unique(c(
    if (options[["studyLabels"]] != "") options[["studyLabels"]],
    if (length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) unlist(options[["forestPlotStudyInformationSelectedVariables"]]),
    if (options[["forestPlotMappingColor"]] != "") options[["forestPlotMappingColor"]],
    if (options[["forestPlotMappingShape"]] != "") options[["forestPlotMappingShape"]],
    if (options[["forestPlotStudyInformationOrderBy"]] != "")    options[["forestPlotStudyInformationOrderBy"]]
  ))
  # remove variables already specified in the model
  additionalVariables <- setdiff(
    additionalVariables,
    c(asNumeric, asFactors)
  )


  # load data
  dataset <- .readDataSetToEnd(
    columns.as.factor   = c(asFactors, additionalVariables),
    columns.as.numeric  = asNumeric)

  # omit NAs
  omitOnVariables <- c(asNumeric, asFactors)
  anyNaByRows     <- apply(dataset[,omitOnVariables], 1, function(x) anyNA(x))
  dataset         <- dataset[!anyNaByRows,]
  attr(dataset, "NAs") <- sum(anyNaByRows)

  # add se^2 for V^2 input
  dataset$samplingVariance <- dataset[[options[["effectSizeStandardError"]]]]^2

  return(dataset)
}
.mammCheckErrors                 <- function(dataset, options) {

  randomVariables <- .mammExtractRandomVariableNames(options)

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations", "variance"),
    all.target           = c(
      options[["effectSize"]],
      options[["effectSizeStandardError"]],
      options[["predictors"]][options[["predictors.types"]] == "scale"],
      c(randomVariables$scale, randomVariables$ordinal)
    ),
    observations.amount  = "< 2",
    exitAnalysisIfErrors = TRUE)

  .hasErrors(
    dataset              = dataset,
    type                 = c("modelInteractions"),
    modelInteractions.modelTerms = c(options[["effectSizeModelTerms"]], options[["heterogeneityModelTerms"]]),
    exitAnalysisIfErrors = TRUE)

  .hasErrors(
    dataset              = dataset,
    seCheck.target       = options[["effectSizeStandardError"]],
    custom               = .maCheckStandardErrors,
    exitAnalysisIfErrors = TRUE)
}
.mammGetRandomFormulaList        <- function(options) {

  if (length(options[["randomEffects"]]) == 0)
    return(NULL)

  # extract the random effects
  randomFormulas       <- list()
  for (i in seq_along(options[["randomEffects"]])) {

    tempType <- options[["randomEffects"]][[i]][["type"]]

    if (tempType == "simple") {

      tempValue <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]][["value"]]

      if (tempValue != "") {
        randomFormulas[[i]] <- as.formula(paste0("~ 1 | ", .encodeColNamesLax(tempValue)), env = parent.frame(1))
      }

    } else if (tempType == "nested") {

      tempValues <- c(
        options[["randomEffectsSpecification"]][[i]][["level1"]][["value"]],
        options[["randomEffectsSpecification"]][[i]][["level2"]][["value"]],
        options[["randomEffectsSpecification"]][[i]][["level3"]][["value"]],
        options[["randomEffectsSpecification"]][[i]][["level4"]][["value"]]
      )
      tempValues <- tempValues[tempValues != ""]

      if (length(tempValues) > 0) {
        randomFormulas[[i]] <- as.formula(paste0("~ 1 | ", paste(sapply(tempValues, .encodeColNamesLax), collapse = "/")), env = parent.frame(1))
      }

    } else if (tempType == "randomSlopes") {

      tempValuesSlopes  <- unlist(options[["randomEffectsSpecification"]][[i]][["randomSlopeTerms"]][["value"]])
      tempValueGrouping <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]][["value"]]

      if (length(tempValuesSlopes) > 0 && tempValueGrouping != "") {
        randomFormulas[[i]] <- as.formula(paste0("~ ", paste(sapply(tempValuesSlopes, .encodeColNamesLax), collapse = "+")," | ", .encodeColNamesLax(tempValueGrouping)), env = parent.frame(1))
        attr(randomFormulas[[i]], "structure") <- "GEN"
      }

    } else if (tempType %in% c("structured", "autoregressive")) {

      tempValueInner <- switch(
        tempType,
        "structured"     = options[["randomEffectsSpecification"]][[i]][["factorLevels"]][["value"]],
        "autoregressive" = options[["randomEffectsSpecification"]][[i]][["time"]][["value"]]
      )
      tempValueOuter <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]][["value"]]

      if (tempValueInner != "" && tempValueOuter != "") {
        randomFormulas[[i]] <- as.formula(paste0("~ ", .encodeColNamesLax(tempValueInner), " | ", .encodeColNamesLax(tempValueOuter)), env = parent.frame(1))
        attr(randomFormulas[[i]], "structure") <-  options[["randomEffects"]][[i]][["type"]]
      }

    }  else if (tempType == "spatial") {

      tempValueInner <- paste0("computedSpatialDistance", i)
      tempValueOuter <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]][["value"]]

      if (!is.null(unlist(options[["randomEffectsSpecification"]][[i]][["spatialCoordinates"]][["value"]])) && tempValueOuter != "") {
        randomFormulas[[i]] <- as.formula(paste0("~ ", .encodeColNamesLax(tempValueInner), " | ", .encodeColNamesLax(tempValueOuter)), env = parent.frame(1))
        attr(randomFormulas[[i]], "structure") <-  options[["randomEffects"]][[i]][["type"]]
      }

    } else if (tempType == "knownCorrelation") {

      stop("Not implemented yet.")

    }
  }

  randomFormulasSkipped <- sapply(randomFormulas, is.null)

  if (all(randomFormulasSkipped))
    return(NULL)

  randomFormulas        <- randomFormulas[!randomFormulasSkipped]
  # add missing null elements in case the last random effects was skipped
  if (length(options[["randomEffectsSpecification"]]) > length(randomFormulasSkipped))
    randomFormulasSkipped[(length(randomFormulasSkipped)+1):length(options[["randomEffectsSpecification"]])] <- TRUE
  attr(randomFormulas, "skipped") <-  randomFormulasSkipped
  names(randomFormulas) <- paste("Component", seq_along(randomFormulas))

  return(randomFormulas)
}
.mammExtractRandomVariableNames  <- function(options) {

  if (length(options[["randomEffects"]]) == 0)
    return(NULL)

  # extract the random effects
  variablesNominal   <- NULL
  variablesOrdinal   <- NULL
  variablesScale     <- NULL

  for (i in seq_along(options[["randomEffects"]])) {

    tempType <- options[["randomEffects"]][[i]][["type"]]

    if (tempType == "simple") {

      variablesNominal <- c(variablesNominal, options[["randomEffectsSpecification"]][[i]][["groupingFactor"]][["value"]])

    } else if (tempType == "nested") {

      variablesNominal <- c(
        variablesNominal,
        options[["randomEffectsSpecification"]][[i]][["level1"]][["value"]],
        options[["randomEffectsSpecification"]][[i]][["level2"]][["value"]],
        options[["randomEffectsSpecification"]][[i]][["level3"]][["value"]],
        options[["randomEffectsSpecification"]][[i]][["level4"]][["value"]]
      )

    } else if (tempType == "randomSlopes") {

      tempValuesSlopes       <- unlist(options[["randomEffectsSpecification"]][[i]][["randomSlopeTerms"]][["value"]])
      tempValuesSlopesTypes  <- options[["randomEffectsSpecification"]][[i]][["randomSlopeTerms"]][["types"]]

      variablesNominal <- c(variablesNominal, tempValuesSlopes[tempValuesSlopesTypes == "nominal"])
      variablesScale   <- c(variablesScale,   tempValuesSlopes[tempValuesSlopesTypes == "scale"])
      variablesNominal <- c(variablesNominal, options[["randomEffectsSpecification"]][[i]][["groupingFactor"]][["value"]])

    } else if (tempType == "structured") {

      variablesNominal <- c(
        variablesNominal,
        options[["randomEffectsSpecification"]][[i]][["factorLevels"]][["value"]],
        options[["randomEffectsSpecification"]][[i]][["groupingFactor"]][["value"]]
      )

    }else if (tempType == "autoregressive") {

      if (options[["randomEffects"]][[i]][["structure"]] == "continuousTimeAr") {
        variablesScale   <- c(variablesScale,   options[["randomEffectsSpecification"]][[i]][["time"]][["value"]])
      } else {
        variablesOrdinal <- c(variablesOrdinal, options[["randomEffectsSpecification"]][[i]][["time"]][["value"]])
      }
      variablesNominal <- c(variablesNominal, options[["randomEffectsSpecification"]][[i]][["groupingFactor"]][["value"]])

    }  else if (tempType == "spatial") {

      variablesScale <- c(variablesScale, unlist(options[["randomEffectsSpecification"]][[i]][["spatialCoordinates"]][["value"]]))
      variablesNominal <- c(variablesNominal, options[["randomEffectsSpecification"]][[i]][["groupingFactor"]][["value"]])

    } else if (tempType == "knownCorrelation") {

      stop("Not implemented yet.")

    }
  }

  variablesScale   <- unique(variablesScale)
  variablesNominal <- unique(variablesNominal)
  variablesOrdinal <- unique(variablesOrdinal)

  variablesScale   <- variablesScale[variablesScale != ""]
  variablesNominal <- variablesNominal[variablesNominal != ""]
  variablesOrdinal <- variablesOrdinal[variablesOrdinal != ""]


  # TODO: remove variable translation hotfix
  return(list(
    scale   = if (length(variablesScale)   != 0) sapply(variablesScale,   .encodeColNamesLax),
    nominal = if (length(variablesNominal) != 0) sapply(variablesNominal, .encodeColNamesLax),
    ordinal = if (length(variablesOrdinal) != 0) sapply(variablesOrdinal, .encodeColNamesLax)
  ))
}
.mammRandomEstimatesTable        <- function(jaspResults, dataset, options) {

  modelSummaryContainer    <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["randomEstimatesContainer"]]))
    return()

  randomEstimatesContainer <- createJaspContainer(gettext("Model Summary"))
  randomEstimatesContainer$dependOn(.maDependencies)
  randomEstimatesContainer$position <- 1
  modelSummaryContainer[["randomEstimatesContainer"]] <- randomEstimatesContainer

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  ### create table for each structure type
  # nested random effects
  if (fit[["withS"]]) {

    tableS <- createJaspTable(gettext("Simple / Nested Estimates"))
    tableS$position <- 1

    tableS$addColumnInfo(name = "factor",  type = "string")
    tableS$addColumnInfo(name = "sigma",   type = "number",  title = gettext("\U03C3"))
    tableS$addColumnInfo(name = "sigma2",  type = "number",  title = gettext("\U03C3\U00B2"))
    tableS$addColumnInfo(name = "nlvls",   type = "integer", title = gettext("Levels"))
    tableS$addColumnInfo(name = "fixed",   type = "string",  title = gettext("Fixed"))
    # tableS$addColumnInfo(name = "R",       type = "string",  title = gettext("R")) # whether supplied via known correlation matrix

    resultsS <- data.frame(
      factor = .maVariableNames(fit[["s.names"]], unlist(.mammExtractRandomVariableNames(options))),
      sigma  = sqrt(fit[["sigma2"]]),
      sigma2 = fit[["sigma2"]],
      nlvls  = fit[["s.nlevels"]],
      fixed  = ifelse(fit[["vc.fix"]]$sigma2, "yes", "no")
      # R      = ifelse(fit[["Rfix"]] , "yes", "no")
    )

    tableS$setData(resultsS)
  }

  # random slopes
  if (fit[["withG"]] && fit$struct[1] == "GEN") {

    tableG <- createJaspTable(gettext("Random Slopes Estimates"))
    tableG$position <- 2

    tableB$addColumnInfo(name = "factor",  type = "string")
    tableB$addColumnInfo(name = "sigma",   type = "number",  title = gettext("\U03C3"))
    tableB$addColumnInfo(name = "sigma2",  type = "number",  title = gettext("\U03C3\U00B2"))
    tableB$addColumnInfo(name = "fixed",   type = "string",  title = gettext("Fixed"))



    # variances
    vc <- cbind.data.frame(sqrt(fit$tau2), fit$tau2, ifelse(fit$vc.fix$tau2, "yes", "no"))
    colnames(vc) <- c("sigma", "sigma2", "fixed")
    vc$factor <- .maVariableNames(fit$g.names[-length(fit$g.names)], unlist(.mammExtractRandomVariableNames(options)))

    # covariance matrix
    G.info <- cov2cor(fit$G)
    diag(G.info) <- NA
    G.info[lower.tri(G.info)] <- NA
    # TODO: adds info about whether the correlation is fixed or estimated
    # G.info[lower.tri(G.info)] <- ifelse(fit$vc.fix$rho, "yes", "no")
    colnames(G.info) <- .maVariableNames(fit$g.names[-length(fit$g.names)], unlist(.mammExtractRandomVariableNames(options)))


   tableB$setData(resultsB)


   inner <- trimws(paste0(strsplit(paste0(fit$formulas[[1]], collapse = ""), "|", fixed = TRUE)[[1]][1], collapse = ""))
   outer <- tail(fit$g.names, 1)

   tableB$addFooter(paste0(inner, " | ", outer), symbol = gettext("Component: "))
   tableB$addFooter(paste0(x$g.nlevels.f[1], " | ", x$g.nlevels[2]), symbol = gettext("Levels: "))
  }


  return()
}
