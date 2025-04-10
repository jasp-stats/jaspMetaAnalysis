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

  if (.maReady(options)) {
    dataset <- .mammCheckData(dataset, options)
    .mammCheckErrors(dataset, options)
  }

  .ClassicalMetaAnalysisCommon(jaspResults, dataset, options)

  return()
}

.mammCheckData                   <- function(dataset, options) {

  # model data
  predictorsNominal <- options[["predictors"]][options[["predictors.types"]] == "nominal"]
  predictorsScale   <- options[["predictors"]][options[["predictors.types"]] == "scale"]

  # random effects variables
  randomVariables <- .mammExtractRandomVariableNames(options)

  # omit NAs
  omitOnVariables <- c(
    options[["effectSize"]],
    options[["effectSizeStandardError"]],
    unlist(randomVariables),
    if (options[["clustering"]] != "") options[["clustering"]],
    if (length(predictorsNominal) > 0) predictorsNominal,
    if (length(predictorsScale) > 0)   predictorsScale
  )
  anyNaByRows <- apply(dataset[,omitOnVariables], 1, function(x) anyNA(x))
  dataset     <- dataset[!anyNaByRows,]
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
    modelInteractions.modelTerms = options[["effectSizeModelTerms"]],
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

      tempValue <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]]

      if (tempValue != "") {
        randomFormulas[[i]] <- as.formula(paste0("~ 1 | ", tempValue), env = parent.frame(1))
      }

    } else if (tempType == "nested") {

      tempValues <- c(
        options[["randomEffectsSpecification"]][[i]][["level1"]],
        options[["randomEffectsSpecification"]][[i]][["level2"]],
        options[["randomEffectsSpecification"]][[i]][["level3"]],
        options[["randomEffectsSpecification"]][[i]][["level4"]],
        options[["randomEffectsSpecification"]][[i]][["level5"]]
      )
      tempValues <- tempValues[tempValues != ""]

      if (length(tempValues) > 0) {
        randomFormulas[[i]] <- as.formula(paste0("~ 1 | ", paste(tempValues, collapse = "/")), env = parent.frame(1))
      }

    } else if (tempType == "randomSlopes") {

      tempValuesSlopes  <- unlist(options[["randomEffectsSpecification"]][[i]][["randomSlopeTerms"]])
      tempValueGrouping <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]]

      if (length(tempValuesSlopes) > 0 && tempValueGrouping != "") {
        randomFormulas[[i]] <- as.formula(paste0("~ ", paste(tempValuesSlopes, collapse = "+")," | ", tempValueGrouping), env = parent.frame(1))
        attr(randomFormulas[[i]], "structure") <- "GEN"
      }

    } else if (tempType %in% c("structured", "autoregressive")) {

      tempValueInner <- switch(
        tempType,
        "structured"     = options[["randomEffectsSpecification"]][[i]][["factorLevels"]],
        "autoregressive" = options[["randomEffectsSpecification"]][[i]][["time"]]
      )
      tempValueOuter <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]]

      if (tempValueInner != "" && tempValueOuter != "") {
        randomFormulas[[i]] <- as.formula(paste0("~ ", tempValueInner, " | ", tempValueOuter), env = parent.frame(1))
        attr(randomFormulas[[i]], "structure") <- .mammGetStructureOptions(options[["randomEffects"]][[i]][["structure"]])
      }

    }  else if (tempType == "spatial") {

      tempDistanceMetric <- .mammGetDistanceOptions(options[["randomEffectsSpecification"]][[i]][["distanceMetric"]])

      if (tempDistanceMetric != "loadFromFile") {

        # dispatch distance type
        if (tempDistanceMetric == "gcd") {
          tempValueInner <- c(
            if (options[["randomEffectsSpecification"]][[i]][["longitude"]] != "") options[["randomEffectsSpecification"]][[i]][["longitude"]],
            if (options[["randomEffectsSpecification"]][[i]][["latitude"]]  != "") options[["randomEffectsSpecification"]][[i]][["latitude"]]
          )
        } else {
          tempValueInner <- unlist(options[["randomEffectsSpecification"]][[i]][["spatialCoordinates"]])
        }

        tempValueOuter <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]]

        # spatial does not require a grouping factor
        if (tempValueOuter == "")
          tempValueOuter <- "constant"

        if ((tempDistanceMetric == "gcd" && length(tempValueInner) == 2) || (tempDistanceMetric != "gcd" && length(tempValueInner) > 0)) {
          randomFormulas[[i]] <- as.formula(paste0("~ ", paste(tempValueInner, collapse = "+")," | ", tempValueOuter), env = parent.frame(1))
          attr(randomFormulas[[i]], "structure")    <- .mammGetStructureOptions(options[["randomEffects"]][[i]][["structure"]])
          attr(randomFormulas[[i]], "dist")         <- tempDistanceMetric
          attr(randomFormulas[[i]], "addConstant")  <- tempValueOuter == "constant"
        }

      } else {

        # requires the inner term, the matrix needs to be a row & columns named file
        tempValueInner         <- options[["randomEffectsSpecification"]][[i]][["locationIdentifier"]]
        distanceMatrixFileName <- options[["randomEffectsSpecification"]][[i]][["distanceMatrixFile"]]

        if (distanceMatrixFileName != "" && tempValueInner != "") {

          # try regular csv loading
          if (tolower(gsub(" ", "", distanceMatrixFileName)) == "examplemaire2019distancematrix") {
            # allow to load example data for data library
            distanceMatrix <- .mammGetExampleMaire2019DistanceMatrix()
          }else{
            distanceMatrix <- try(as.matrix(read.csv(file = distanceMatrixFileName, row.names = 1)))
          }

          if (inherits(distanceMatrix, "try-error"))
            .quitAnalysis(gettextf("Error reading the distance matrix file: %1$s", distanceMatrix))

          # if there is only one column, try csv2 (indicates different decimals enconding)
          if (ncol(distanceMatrix) == 1)
            distanceMatrix <- try(as.matrix(read.csv2(file = distanceMatrixFileName, row.names = 1)))

          if (inherits(distanceMatrix, "try-error"))
            .quitAnalysis(gettextf("Error reading the distance matrix file: %1$s", distanceMatrix))

          if (nrow(distanceMatrix) != ncol(distanceMatrix))
            .quitAnalysis(gettextf("The distance matrix must be square. The number of rows (%1$i) does not match the number of columns (%2$i).",
                                  nrow(distanceMatrix), ncol(distanceMatrix)))

          # spatial does not require a grouping factor
          tempValueOuter <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]]
          if (tempValueOuter == "")
            tempValueOuter <- "constant"

          randomFormulas[[i]] <- as.formula(paste0("~ ", tempValueInner, " | ", tempValueOuter), env = parent.frame(1))
          attr(randomFormulas[[i]], "structure")    <- .mammGetStructureOptions(options[["randomEffects"]][[i]][["structure"]])
          attr(randomFormulas[[i]], "dist")         <- list(distanceMatrix)
          names(attr(randomFormulas[[i]], "dist"))  <- tempValueInner
          attr(randomFormulas[[i]], "addConstant")  <- tempValueOuter == "constant"

        }
      }

    } else if (tempType == "knownCorrelation") {

      # requires the outer term, the matrix needs to be a row & columns named file
      tempValueOuter         <- options[["randomEffectsSpecification"]][[i]][["groupingFactor"]]
      distanceMatrixFileName <- options[["randomEffectsSpecification"]][[i]][["correlationMatrixFile"]]
      if (tempValueOuter != "" && distanceMatrixFileName != "") {
        # try regular csv loading
        correlationMatrix <- try(as.matrix(read.csv(file = distanceMatrixFileName, row.names = 1)))

        if (inherits(correlationMatrix, "try-error"))
          .quitAnalysis(gettextf("Error reading the correlation matrix file: %1$s", correlationMatrix))

        # if there is only one column, try csv2 (indicates different decimals encoding)
        if (ncol(correlationMatrix) == 1)
          correlationMatrix <- try(as.matrix(read.csv2(file = distanceMatrixFileName, row.names = 1)))

        if (inherits(correlationMatrix, "try-error"))
          .quitAnalysis(gettextf("Error reading the correlation matrix file: %1$s", correlationMatrix))

        if (nrow(correlationMatrix) != ncol(correlationMatrix))
          .quitAnalysis(gettextf("The distance matrix must be square. The number of rows (%1$i) does not match the number of columns (%2$i).",
                                nrow(correlationMatrix), ncol(correlationMatrix)))

        randomFormulas[[i]] <- as.formula(paste0("~ 1 | ", tempValueOuter), env = parent.frame(1))
        attr(randomFormulas[[i]], "R")           <- list(correlationMatrix)
        names(attr(randomFormulas[[i]], "R"))    <- tempValueOuter
      }
    }
  }

  randomFormulasSkipped <- sapply(randomFormulas, is.null)

  if (all(randomFormulasSkipped))
    return(NULL)

  randomFormulas <- randomFormulas[!randomFormulasSkipped]
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

      variablesNominal <- c(variablesNominal, options[["randomEffectsSpecification"]][[i]][["groupingFactor"]])

    } else if (tempType == "nested") {

      variablesNominal <- c(
        variablesNominal,
        options[["randomEffectsSpecification"]][[i]][["level1"]],
        options[["randomEffectsSpecification"]][[i]][["level2"]],
        options[["randomEffectsSpecification"]][[i]][["level3"]],
        options[["randomEffectsSpecification"]][[i]][["level4"]],
        options[["randomEffectsSpecification"]][[i]][["level5"]]
      )

    } else if (tempType == "randomSlopes") {

      tempValuesSlopes       <- unlist(options[["randomEffectsSpecification"]][[i]][["randomSlopeTerms"]])
      tempValuesSlopesTypes  <- options[["randomEffectsSpecification"]][[i]][["randomSlopeTerms.types"]]

      variablesNominal <- c(variablesNominal, tempValuesSlopes[tempValuesSlopesTypes == "nominal"])
      variablesScale   <- c(variablesScale,   tempValuesSlopes[tempValuesSlopesTypes == "scale"])
      variablesNominal <- c(variablesNominal, options[["randomEffectsSpecification"]][[i]][["groupingFactor"]])

    } else if (tempType == "structured") {

      variablesNominal <- c(
        variablesNominal,
        options[["randomEffectsSpecification"]][[i]][["factorLevels"]],
        options[["randomEffectsSpecification"]][[i]][["groupingFactor"]]
      )

    }else if (tempType == "autoregressive") {

      if (options[["randomEffects"]][[i]][["structure"]] == "continuousTimeAr") {
        variablesScale   <- c(variablesScale,   options[["randomEffectsSpecification"]][[i]][["time"]])
      } else {
        variablesOrdinal <- c(variablesOrdinal, options[["randomEffectsSpecification"]][[i]][["time"]])
      }
      variablesNominal <- c(variablesNominal, options[["randomEffectsSpecification"]][[i]][["groupingFactor"]])

    }  else if (tempType == "spatial") {

      variablesScale   <- c(variablesScale, unlist(options[["randomEffectsSpecification"]][[i]][["spatialCoordinates"]]),
                            options[["randomEffectsSpecification"]][[i]][["longitude"]],
                            options[["randomEffectsSpecification"]][[i]][["latitude"]])
      variablesNominal <- c(variablesNominal, options[["randomEffectsSpecification"]][[i]][["groupingFactor"]],
                            options[["randomEffectsSpecification"]][[i]][["locationIdentifier"]])

    } else if (tempType == "knownCorrelation") {

      variablesNominal <- c(variablesNominal, options[["randomEffectsSpecification"]][[i]][["groupingFactor"]])
    }
  }

  variablesScale   <- unique(variablesScale)
  variablesNominal <- unique(variablesNominal)
  variablesOrdinal <- unique(variablesOrdinal)

  variablesScale   <- variablesScale[variablesScale != ""]
  variablesNominal <- variablesNominal[variablesNominal != ""]
  variablesOrdinal <- variablesOrdinal[variablesOrdinal != ""]

  return(list(
    scale   = if (length(variablesScale)   != 0) variablesScale,
    nominal = if (length(variablesNominal) != 0) variablesNominal,
    ordinal = if (length(variablesOrdinal) != 0) variablesOrdinal
  ))
}
.mammRandomEstimatesTable        <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["randomEstimatesContainer"]])) {
    randomEstimatesContainer <- jaspResults[["randomEstimatesContainer"]]
  } else {
    randomEstimatesContainer <- createJaspContainer(title = gettext("Random Effects / Model Stucture Summary"))
    randomEstimatesContainer$dependOn(.maDependencies)
    randomEstimatesContainer$position <- 2
    jaspResults[["randomEstimatesContainer"]] <- randomEstimatesContainer
  }

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || jaspBase::isTryError(fit) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  ### create table for nested random effects
  if (fit[["withS"]] && is.null(randomEstimatesContainer[["containerS"]])) {

    containerS <- createJaspContainer(title = gettext("Simple / Nested Summary"))
    containerS$position <- 1
    randomEstimatesContainer[["containerS"]] <- containerS

    tableS <- createJaspTable(title = gettext("Estimates"))
    tableS$position <- 1

    tableS$addColumnInfo(name = "factor",  type = "string",  title = "")
    tableS$addColumnInfo(name = "sigma2",  type = "number",  title = gettext("\U03C3\U00B2"))
    tableS$addColumnInfo(name = "sigma",   type = "number",  title = gettext("\U03C3"))
    tableS$addColumnInfo(name = "nlvls",   type = "integer", title = gettext("Levels"))
    if (.mammAddIsFixedRandom(options, 3))
      tableS$addColumnInfo(name = "fixed",   type = "string",  title = gettext("Fixed"))

    # tableS$addColumnInfo(name = "R",       type = "string",  title = gettext("R")) # whether supplied via known correlation matrix
    containerS[["tableS"]] <- tableS

    resultsS <- data.frame(
      factor = .maVariableNames(fit[["s.names"]], unlist(.mammExtractRandomVariableNames(options))),
      sigma  = sqrt(fit[["sigma2"]]),
      sigma2 = fit[["sigma2"]],
      nlvls  = fit[["s.nlevels"]],
      fixed  = ifelse(fit[["vc.fix"]]$sigma2, "yes", "no")
      # R      = ifelse(fit[["Rfix"]] , "yes", "no")
    )

    if (!.mammAddIsFixedRandom(options, indx))
      resultsS <- resultsS[,colnames(resultsS) != "fixed", drop = FALSE]

    tableS$setData(resultsS)
  }

  ### create summary for the remaining types
  if (fit[["withG"]] && is.null(randomEstimatesContainer[["containerG"]])) {

    # create jasp containers
    containerG <- createJaspContainer(title = .mammGetRandomEstimatesTitle(fit[["struct"]][1]))
    containerG$position <- 2
    randomEstimatesContainer[["containerG"]] <- containerG
    .mammExtractRandomTables(containerG, options, fit, indx = 1)

  }

  if (fit[["withH"]] && is.null(randomEstimatesContainer[["containerH"]])) {

    containerH <- createJaspContainer(title = .mammGetRandomEstimatesTitle(fit[["struct"]][2]))
    containerH$position <- 3
    randomEstimatesContainer[["containerH"]] <- containerH
    .mammExtractRandomTables(containerH, options, fit, indx = 2)

  }

  ### create random structure inclusion summary
  if (options[["randomEffectsTestInclusion"]] && is.null(randomEstimatesContainer[["tableInclusion"]])) {

    tableInclusion <- createJaspTable(title = gettext("Inclusion Test"))
    tableInclusion$position <- 4
    tableInclusion$dependOn("randomEffectsTestInclusion")
    randomEstimatesContainer[["tableInclusion"]] <- tableInclusion

    tableInclusion$addColumnInfo(name = "model",  title = gettext("Removed Component"), type = "string")
    tableInclusion$addColumnInfo(name = "logLik", title = gettext("Log Lik."),          type = "number")
    tableInclusion$addColumnInfo(name = "df",     title = gettext("df"),                type = "integer")
    tableInclusion$addColumnInfo(name = "AIC",    title = gettext("AIC"),               type = "number")
    tableInclusion$addColumnInfo(name = "BIC",    title = gettext("BIC"),               type = "number")
    tableInclusion$addColumnInfo(name = "AICc",   title = gettext("AICc"),              type = "number")
    tableInclusion$addColumnInfo(name = "LRT",    title = gettext("LRT"),               type = "number")
    tableInclusion$addColumnInfo(name = "pval",   title = gettext("p"),                 type = "pvalue")

    dropOneFits <- .mammFitDropOneRandom(jaspResults, options)

    if (length(dropOneFits) == 0)
      return()

    fit      <- .maExtractFit(jaspResults, options)
    fitTests <- lapply(dropOneFits, function(fitB) data.frame(anova(fit, fitB)))
    fitTests <- rbind(
      cbind(model = "", fitTests[[1]][1,]),
      cbind(model = names(fitTests), do.call(rbind, lapply(fitTests, function(fitTest) fitTest[2,])))
    )

    fitTests <- fitTests[,!colnames(fitTests) %in% "QE"]
    tableInclusion$setData(fitTests)
    tableInclusion$addFootnote(gettext("Likelihood Ratio Test (LRT) and p-value are based on a comparison with the complete model."))
  }

  return()
}
.mammGetRandomEstimatesTitle     <- function(structure) {

  if (structure == "GEN")
    return(gettext("Random Slopes Summary"))
  else if (structure %in% c("CS", "HCS", "UN", "ID", "DIAG"))
    return(paste0(gettext("Structured"), " (", .mammGetOptionsNameStructure(structure), ") ", gettext("Summary")))
  else if (structure %in% c("AR", "HAR", "CAR"))
    return(paste0(gettext("Autoregressive"), " (", .mammGetOptionsNameStructure(structure), ") ", gettext("Summary")))
  else if (structure %in% c("SPEXP", "SPGAU", "SPLIN", "SPRAT", "SPSPH"))
    return(paste0(gettext("Spatial"), " (", .mammGetOptionsNameStructure(structure), ") ", gettext("Summary")))
  else
    return(gettext("Known Correlation Summary"))
}
.mammFitDropOneRandom            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["dropOneFits"]]))
    return(jaspResults[["dropOneFits"]]$object)

  dropOneFitsContainer <- createJaspState()
  dropOneFitsContainer$dependOn(.maDependencies)
  jaspResults[["dropOneFits"]] <- dropOneFitsContainer

  fit <- .maExtractFit(jaspResults, options)

  # create list of all structures
  randomFormulaLists <- .mammGetRandomFormulaList(options)
  dropOneFits        <- vector("list", length = length(randomFormulaLists))
  names(dropOneFits) <- names(randomFormulaLists)

  startProgressbar(expectedTicks = length(randomFormulaLists), label = gettext("Testing Inclusion of Random Effects / Model Structure"))

  # perform drop one re-estimation
  for (i in seq_along(randomFormulaLists)) {

    randomFormulaList <- randomFormulaLists[-i]
    randomFormulaList <- unname(randomFormulaList)

    random <- NULL
    struct <- NULL
    dist   <- NULL
    R      <- NULL

    if (length(randomFormulaList) != 0) {
      random <- randomFormulaList
      struct <- do.call(c, lapply(randomFormulaList, attr, which = "structure"))
      dist   <- unlist(lapply(randomFormulaList, attr, which = "dist"), recursive = FALSE)
      R      <- unlist(lapply(randomFormulaList, attr, which = "R"), recursive = FALSE)
    }

    # set default struct if unspecified
    if (is.null(struct))
      struct <- "CS"

    tempFit <- try(update(fit, random = random, struct = struct, dist = dist, R = R))

    dropOneFits[[i]] <- tempFit
    progressbarTick()
  }

  dropOneFitsContainer$object <- dropOneFits
  return(dropOneFits)
}
.mammGetStructureOptions         <- function(structure) {

  return(switch(
    structure,
    "compoundSymmetry"                  = "CS",
    "heteroscedasticCompoundSymmetry"   = "HCS",
    "unstructured"                      = "UN",
    "identity"                          = "ID",
    "diagonal"                          = "DIAG",
    "ar1"                               = "AR",
    "heteroskedasticAr1"                = "HAR",
    "continuousTimeAr"                  = "CAR",
    "exponential"                       = "SPEXP",
    "gaussian"                          = "SPGAU",
    "linear"                            = "SPLIN",
    "rationalQuadratic"                 = "SPRAT",
    "spherical"                         = "SPSPH",
    stop(paste0("Unknown structure: ", structure))
  ))
}
.mammGetOptionsNameStructure     <- function(structure) {

  return(switch(
    structure,
    "GEN"   = gettextf("Random Slopes"),
    "CS"    = gettextf("Compound Symmetry"),
    "HCS"   = gettextf("Heteroscedastic Compound Symmetry"),
    "UN"    = gettextf("Unstructured"),
    "ID"    = gettextf("Identity"),
    "DIAG"  = gettextf("Diagonal"),
    "AR"    = gettextf("AR(1)"),
    "HAR"   = gettextf("Heteroskedastic AR(1)"),
    "CAR"   = gettextf("Continuous-Time AR"),
    "SPEXP" = gettextf("Exponential"),
    "SPGAU" = gettextf("Gaussian"),
    "SPLIN" = gettextf("Linear"),
    "SPRAT" = gettextf("Rational Quadratic"),
    "SPSPH" = gettextf("Spherical"),
    stop(paste0("Unknown value: ", structure))
  ))
}
.mammGetDistanceOptions          <- function(distance) {

  return(switch(
    distance,
    "euclidean"     = "euclidean",
    "manhattan"     = "manhattan",
    "maximum"       = "maximum",
    "greatCircle"   = "gcd",
    "loadFromFile"  = "loadFromFile",
    stop(paste0("Unknown value: ", distance))
  ))
}
.mammAnyStructureGen             <- function(options) {
  # only relevant for multivariate
  if (options[["module"]] != "metaAnalysisMultilevelMultivariate")
    return(FALSE)

  # get all the active components types
  randomFormulaList <- .mammGetRandomFormulaList(options)
  if (length(randomFormulaList) == 0)
    return(FALSE)

  structures <- unlist(lapply(randomFormulaList, attr, which = "structure"))

  return(any(structures %in% "GEN"))
}
.mammHasMultipleHeterogeneities  <- function(options, canAddOutput = FALSE) {
  # only relevant for multivariate
  if (options[["module"]] != "metaAnalysisMultilevelMultivariate")
    return(FALSE)

  # get all the active components types
  randomFormulaList <- .mammGetRandomFormulaList(options)
  if (length(randomFormulaList) == 0)
    return(FALSE)

  structures <- unlist(lapply(randomFormulaList, attr, which = "structure"))

  if (canAddOutput)
    return(any(structures %in% c("HCS", "UN", "DIAG", "HAR")) && !any(structures %in% "GEN"))
  else
    return(any(structures %in% c("GEN", "HCS", "UN", "DIAG", "HAR")))
}
.mammExtractTauLevelNames        <- function(fit) {

  levelNames <- c()

  if (fit[["withG"]] && fit[["struct"]][1] %in% c("HCS", "UN", "DIAG", "HAR"))
    levelNames <- c(levelNames, fit$g.names[[1]])

  if (fit[["withH"]] && fit[["struct"]][2] %in% c("HCS", "UN", "DIAG", "HAR"))
    levelNames <- c(levelNames, fit$h.names[[1]])

  return(levelNames)
}
.mammExtractTauLevels            <- function(fit, expanded = TRUE) {

  levels <- list()

  if (fit[["withG"]] && fit[["struct"]][1] %in% c("HCS", "UN", "DIAG", "HAR"))
    levels[["tau2.levels"]]   <- fit$g.levels.f[[1]]

  if (fit[["withH"]] && fit[["struct"]][2] %in% c("HCS", "UN", "DIAG", "HAR"))
    levels[["gamma2.levels"]] <- fit$h.levels.f[[1]]

  if (expanded)
    levels <- expand.grid(levels)
  else
    levels <- do.call(cbind.data.frame, levels)

  return(levels)
}
.mammExtractRandomTables         <- function(tempContainer, options, x, indx = 1) {

  # dispatching
  struct <- x$struct[indx]

  tau2 <- if (indx == 1) x[["tau2"]] else x[["gamma2"]]
  tau  <- sqrt(tau2)
  rho  <- if (indx == 1) x[["rho"]] else x[["phi"]]

  tau2Name <- if (indx == 1) "tau2" else "gamma2"
  rhoName  <- if (indx == 1) "rho"  else "phi"

  GName               <- if (indx == 1) "G" else "H"
  g.levels.kName      <- if (indx == 1) "g.levels.k" else "h.levels.k"
  g.levels.fName      <- if (indx == 1) "g.levels.f" else "h.levels.f"
  g.nlevels.kName     <- if (indx == 1) "g.nlevels.k" else "h.nlevels.k"
  g.nlevels.fName     <- if (indx == 1) "g.nlevels.f" else "h.nlevels.f"
  g.levels.comb.kName <- if (indx == 1) "g.levels.comb.k" else "h.levels.comb.k"
  g.nlevelsName       <- if (indx == 1) "g.nlevels" else "h.nlevels"
  g.namesName         <- if (indx == 1) "g.names" else "h.names"


  # create information messages
  if (is.element(struct, c("SPEXP", "SPGAU", "SPLIN", "SPRAT", "SPSPH", "PHYBM", "PHYPL", "PHYPD", "GEN", "GDIAG"))) {
    inner <- trimws(paste0(strsplit(paste0(x$formulas[[indx]], collapse = ""), "|", fixed = TRUE)[[1]][1], collapse = ""))
  } else {
    inner <- x[[g.namesName]][1]
  }
  outer    <- tail(x[[g.namesName]], 1)
  innerLvl <- x[[g.nlevels.fName]][1]
  outerLvl <- x[[g.nlevelsName]][2]

  message1 <- paste0(x[[g.nlevels.fName]][1], " | ", outerLvl)
  message2 <- paste0(inner, " | ", outer)

  if (is.element(struct, c("CS", "AR", "CAR", "ID", "SPEXP", "SPGAU", "SPLIN", "SPRAT", "SPSPH", "PHYBM", "PHYPL", "PHYPD"))) {

    vc <- cbind(tau2, tau, ifelse(x$vc.fix[[tau2Name]], "yes", "no"))
    vc <- rbind(vc, c(rho, "", ifelse(x$vc.fix[[rhoName]], "yes", "no")))

    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "fixed")
    vc$parameter <- c("\U1D70F\U00B2", "\U03C1")
    for(colName in c("estimate", "estimateSqrt")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (struct == "ID") {
      vc <- vc[1, , drop = FALSE]
    }

   if (!.mammAddIsFixedRandom(options, indx))
     vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))
    tempContainer[["table1"]] <- tempTable

    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))

    }

  if (is.element(struct, c("HCS", "HAR", "DIAG"))) {

    vc <- cbind(tau2, tau, x[[g.levels.kName]], ifelse(x$vc.fix$tau2, "yes", "no"), x[[g.levels.fName]][[1]])
    vc <- rbind(vc, c(rho, "", "", ifelse(x$vc.fix[[rhoName]], "yes", "no"), ""))

    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "nLevels", "fixed", "level")
    for(colName in c("estimate", "estimateSqrt", "nLevels")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (length(x[[tau2Name]]) == 1L) {
      vc$parameter <- c("\U1D70F\U00B2", "\U03C1")
    } else {
      vc$parameter <- c(paste0("\U1D70F\U00B2[",seq_along(x[[tau2Name]]),"]"), "\U03C1")
    }

    if (struct == "DIAG")
      vc <- vc[seq_along(tau2), , drop = FALSE]

    if (!.mammAddIsFixedRandom(options, indx))
      vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "level",          type = "string",  title = gettext("Level"))
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    tempTable$addColumnInfo(name = "nLevels",        type = "integer", title = gettext("Levels"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))

    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table1"]] <- tempTable

  }

  if (is.element(struct, c("UN", "UNR"))) {

    if (struct == "UN") {
      vc <- cbind(tau2, tau, x[[g.levels.kName]], ifelse(x$vc.fix[[tau2Name]], "yes", "no"), x[[g.levels.fName]][[1]])
    } else {
      vc <- cbind(rep(tau2, length(x[[g.levels.kName]])),
                  rep(tau, length(x[[g.levels.kName]])), x[[g.levels.kName]],
                  ifelse(rep(x$vc.fix[[tau2Name]], length(x[[g.levels.kName]])), "yes", "no"),
                  x[[g.levels.fName]][[1]])
    }
    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "nLevels", "fixed", "level")
    for(colName in c("estimate", "estimateSqrt", "nLevels")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (length(x[[g.levels.kName]]) == 1L) {
      vc$parameter <- c("\U1D70F\U00B2")
    } else {
      vc$parameter <-paste0("\U1D70F\U00B2[",seq_along(x[[g.levels.kName]]),"]")
    }

    if (!.mammAddIsFixedRandom(options, indx))
      vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates \U1D70F\U00B2"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "level",          type = "string",  title = gettext("Level"))
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    tempTable$addColumnInfo(name = "nLevels",        type = "string",  title = gettext("Levels"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))

    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table1"]] <- tempTable


    if (length(x[[rhoName]]) == 1L) {
      G <- matrix(NA_real_, nrow = 2, ncol = 2)
    } else {
      G <- matrix(NA_real_, nrow = x[[g.nlevels.fName]][1], ncol = x[[g.nlevels.fName]][1])
    }

    G[lower.tri(G)] <- rho
    G[upper.tri(G)] <- t(G)[upper.tri(G)]
    diag(G) <- 1
    G[upper.tri(G)] <- NA

    G <- data.frame(G)
    colnames(G) <- paste0("rho", 1:(ncol(G)))
    G$parameter <- sprintf("\U03C1[%1$i,]", 1:nrow(G))


    if (length(x$rho) == 1L) {
      G.info <- matrix(NA_real_, nrow = 2, ncol = 2)
    } else {
      G.info <- matrix(NA_real_, nrow = x[[g.nlevels.fName]][1], ncol = x[[g.nlevels.fName]][1])
    }
    G.infoLevels <- G.info
    G.infoLevels[lower.tri(G.infoLevels)] <- x[[g.levels.comb.kName]]
    G.infoLevels[upper.tri(G.infoLevels)] <- t(G.infoLevels)[upper.tri(G.infoLevels)]
    G.infoLevels[lower.tri(G.infoLevels)] <- NA
    diag(G.infoLevels) <- NA

    G.infoLevels <- data.frame(G.infoLevels)
    colnames(G.infoLevels) <- paste0("rhoLevel", 1:ncol(G.infoLevels))

    G.infoEstimated <- G.info
    G.infoEstimated[upper.tri(G.infoEstimated)] <- ifelse(x$vc.fix[[rhoName]], "yes", "no")

    G.infoEstimated <- data.frame(G.infoEstimated)
    colnames(G.infoEstimated) <- paste0("rhoEstimated", 1:ncol(G.infoEstimated))

    if (!.mammAddIsFixedRandom(options, indx))
      Gmat <- cbind(G, G.infoLevels)
    else
      Gmat <- cbind(G, G.infoLevels, G.infoEstimated)

    tempTable2 <- createJaspTable(title = gettext("Estimates \U03C1"))
    tempTable2$position <- 2
    tempTable2$addColumnInfo(name = "parameter", type = "string",  title = "")
    for(i in 1:ncol(G)){
      tempTable2$addColumnInfo(name = paste0("rho",i), type = "number", title = sprintf("[,%1$i]", i), overtitle = gettext("Estimates"))
    }
    for(i in 1:ncol(G.infoLevels)){
      tempTable2$addColumnInfo(name = paste0("rhoLevel",i), type = "integer", title = sprintf("[,%1$i]", i), overtitle = gettext("Levels"))
    }
    if (.mammAddIsFixedRandom(options, indx)) {
      for(i in 1:ncol(G.infoEstimated)){
        tempTable2$addColumnInfo(name = paste0("rhoEstimated",i), type = "string", title = sprintf("[,%1$i]", i), overtitle = gettext("Fixed"))
      }
    }

    tempTable2$setData(Gmat)
    tempTable2$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable2$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table2"]] <- tempTable2
  }

  if (is.element(struct, c("GEN"))) {

    vc <- cbind(tau2, tau, ifelse(x$vc.fix[[tau2Name]], "yes", "no"))

    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "fixed")
    vc$parameter <- .maVariableNames(x[[g.namesName]][-length(x[[g.namesName]])], unlist(.mammExtractRandomVariableNames(options)))
    for(colName in c("estimate", "estimateSqrt")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (!.mammAddIsFixedRandom(options, indx))
      vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates \U1D70F\U00B2"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))

    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table1"]] <- tempTable


    G.info <- cov2cor(x[[GName]])
    diag(G.info) <- NA
    G.info[upper.tri(G.info)] <- NA

    G.info <- data.frame(G.info)
    colnames(G.info) <- paste0("rho", 1:ncol(G.info))


    G.infoFixed <- G.info
    G.infoFixed[lower.tri(G.infoFixed)] <- NA
    G.infoFixed[upper.tri(G.infoFixed)] <- ifelse(x$vc.fix[[rhoName]], "yes", "no")

    G.infoFixed <- data.frame(G.infoFixed)
    colnames(G.infoFixed) <- paste0("rhoFixed", 1:ncol(G.infoFixed))

    if (!.mammAddIsFixedRandom(options, indx))
      Gmat <- G.info
    else
      Gmat <- cbind(G.info, G.infoFixed)

    Gmat$parameter <- .maVariableNames(x[[g.namesName]][-length(x[[g.namesName]])], unlist(.mammExtractRandomVariableNames(options)))


    tempTable2 <- createJaspTable(title = gettext("Estimates \U03C1"))
    tempTable2$position <- 2
    tempTable2$addColumnInfo(name = "parameter", type = "string",  title = "")
    for(i in 1:ncol(G.info)){
      tempTable2$addColumnInfo(name = paste0("rho",i), type = "number", title = Gmat$parameter[i], overtitle = gettext("Estimates"))
    }
    if (.mammAddIsFixedRandom(options, indx)) {
      for(i in 1:ncol(G.infoFixed)){
        tempTable2$addColumnInfo(name = paste0("rhoFixed",i), type = "string", title = Gmat$parameter[i], overtitle = gettext("Fixed"))
      }
    }

    tempTable2$setData(Gmat)
    tempTable2$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable2$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table2"]] <- tempTable2
  }

  if (is.element(struct, c("GDIAG"))) {

    vc <- cbind(tau2, tau, ifelse(x$vc.fix[["tau2"]], "yes", "no"))

    vc <- data.frame(vc)
    colnames(vc) <- c("estimate", "estimateSqrt", "fixed")
    vc$parameter <- .maVariableNames(x[[g.namesName]][-length(x[[g.namesName]])], unlist(.mammExtractRandomVariableNames(options)))
    for(colName in c("estimate", "estimateSqrt")) {
      vc[,colName] <- as.numeric(vc[,colName])
    }

    if (!.mammAddIsFixedRandom(options, indx))
      vc <- vc[,colnames(vc) != "fixed", drop = FALSE]

    tempTable <- createJaspTable(title = gettext("Estimates \U1D70F\U00B2"))
    tempTable$position <- 1
    tempTable$addColumnInfo(name = "parameter",      type = "string",  title = "")
    tempTable$addColumnInfo(name = "estimate",       type = "number",  title = gettext("Estimate"))
    tempTable$addColumnInfo(name = "estimateSqrt",   type = "number",  title = gettext("Sqrt. Estimate"))
    if (.mammAddIsFixedRandom(options, indx))
      tempTable$addColumnInfo(name = "fixed",          type = "string",  title = gettext("Fixed"))


    tempTable$setData(vc)
    tempTable$addFootnote(message1, symbol = gettext("Levels: "))
    tempTable$addFootnote(message2, symbol = gettext("Component: "))
    tempContainer[["table1"]] <- tempTable
  }

  return()
}
.mammAddIsFixedRandom            <- function(options, indx) {

  return(FALSE)

  # TODO: show / hide information on whether the random effects are fixed by the user
}

