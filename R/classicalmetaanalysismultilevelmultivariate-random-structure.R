# Multilevel/multivariate random-effect structure helpers.
#
# Builds random formulas, reduced fits, structure options, and tau-level metadata.

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

        if (length(tempValues) > 1) {
          # store the levels only if they imply a nested structure
          # allows for discriminating from a simple random effect in level inclusion tests
          attr(randomFormulas[[i]], "levels") <- tempValues
        }
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

.mammEmbedLevelRandom            <- function(dataset, levels) {

  for (i in seq_along(levels)[-1]) {
    dataset[[levels[i]]] <- paste0(as.character(dataset[[levels[i-1]]]), "-", as.character(dataset[[levels[i]]]))
  }

  return(dataset)
}

.mammFitDropOneRandom            <- function(jaspResults, options) {

  # extract precomputed drop one fits
  if (!is.null(jaspResults[["dropOneFits"]])) {

    out <- jaspResults[["dropOneFits"]]$object

  } else {

    # create the output container
    confintRandomContainer <- createJaspState()
    confintRandomContainer$dependOn(.maDependencies)
    jaspResults[["dropOneFits"]] <- confintRandomContainer

    fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)
    out <- list()

    for (i in seq_along(fit)) {
      if (jaspBase::isTryError(fit[[i]])) {
        out[[attr(fit[[i]], "subgroup")]] <- list()
      } else {
        out[[attr(fit[[i]], "subgroup")]] <- .mammFitDropOneRandomFun(fit[[i]], options)
      }
    }

    jaspResults[["dropOneFits"]]$object <- out
  }


  return(out)
}

.mammFitDropOneRandomFun         <- function(fit, options) {

  # create list of all structures
  randomFormulaLists <- .mammGetRandomFormulaList(options)
  dropOneFits        <- vector("list", length = length(randomFormulaLists))
  names(dropOneFits) <- names(randomFormulaLists)

  if (options[["subgroup"]] == "") {
    startProgressbar(expectedTicks = length(randomFormulaLists), label = gettext("Testing Inclusion of Random Effects / Model Structure"))
  } else {
    startProgressbar(expectedTicks = length(randomFormulaLists), label = gettextf("Subgroup %1$s: Testing Inclusion of Random Effects / Model Structure", attr(fit, "subgroup")))
  }


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

  return(dropOneFits)
}

.mammFitDropLevelRandom          <- function(jaspResults, options) {

  # extract precomputed drop one fits
  if (!is.null(jaspResults[["dropLevelFits"]])) {

    out <- jaspResults[["dropLevelFits"]]$object

  } else {

    # create the output container
    confintRandomContainer <- createJaspState()
    confintRandomContainer$dependOn(.maDependencies)
    jaspResults[["dropLevelFits"]] <- confintRandomContainer

    fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)
    out <- list()

    for (i in seq_along(fit)) {
      if (jaspBase::isTryError(fit[[i]])) {
        out[[attr(fit[[i]], "subgroup")]] <- list()
      } else {
        out[[attr(fit[[i]], "subgroup")]] <- .mammFitDropLevelRandomFun(fit[[i]], options)
      }
    }

    jaspResults[["dropLevelFits"]]$object <- out
  }


  return(out)
}

.mammFitDropLevelRandomFun       <- function(fit, options) {

  # create list of all structures & keep hierarchical structures
  randomFormulaLists             <- .mammGetRandomFormulaList(options)
  randomFormulaHierarchicalLists <- randomFormulaLists[sapply(randomFormulaLists, function(x) !is.null(attr(x, which = "levels")))]

  dropLevelsList        <- vector("list", length = length(randomFormulaHierarchicalLists))
  names(dropLevelsList) <- names(randomFormulaHierarchicalLists)

  # perform drop one re-estimation
  for (i in seq_along(randomFormulaHierarchicalLists)) {

    # create combination of all level inclusions
    tempLevels     <- attr(randomFormulaHierarchicalLists[[i]], which = "levels")
    tempLevelsGrid <- expand.grid(rep(list(c(TRUE, FALSE)), length(tempLevels)))
    tempLevelsGrid <- tempLevelsGrid[-c(1, nrow(tempLevelsGrid)), , drop = FALSE] # first and the last fits are the full and null models
    colnames(tempLevelsGrid) <- tempLevels

    if (options[["subgroup"]] == "") {
      startProgressbar(expectedTicks = nrow(tempLevelsGrid), label = gettextf("Testing Inclusion of Nested Random Effects: %1$s", names(randomFormulaHierarchicalLists)[i]))
    } else {
      startProgressbar(expectedTicks = nrow(tempLevelsGrid), label = gettextf("Subgroup %1$s: Testing Inclusion of Nested Random Effects: %2$s", attr(fit, "subgroup"), names(randomFormulaHierarchicalLists)[i]))
    }

    dropOneFits <- list()

    for (j in 1:nrow(tempLevelsGrid)) {

      # get the original random formula
      randomFormulaList <- randomFormulaLists
      # replace the current random formula with the new one
      randomFormulaList[[names(randomFormulaHierarchicalLists)[i]]] <- as.formula(paste0("~ 1 | ", paste(tempLevels[unlist(tempLevelsGrid[j,])], collapse = "/")), env = parent.frame(1))
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

      dropOneFits[[j]] <- tempFit
      progressbarTick()
    }

    dropLevelsList[[i]] <- dropOneFits
    attr(dropLevelsList[[i]], "levelsGrid") <- tempLevelsGrid
  }

  return(dropLevelsList)
}

.mammFitConfintRandom            <- function(jaspResults, options) {

  # extract precomputed confidence intervals
  if (!is.null(jaspResults[["confintRandom"]])) {

    out <- jaspResults[["confintRandom"]]$object

  } else {

    # create the output container
    confintRandomContainer <- createJaspState()
    confintRandomContainer$dependOn(.maDependencies)
    jaspResults[["confintRandom"]] <- confintRandomContainer

    fit <- .maExtractFit(jaspResults, options)
    out <- list()

    for (i in seq_along(fit)) {
      if (jaspBase::isTryError(fit[[i]])) {
        out[[attr(fit[[i]], "subgroup")]] <- list()
      } else {
        out[[attr(fit[[i]], "subgroup")]] <- .mammFitConfintRandomFun(fit[[i]], options)
      }
    }

    jaspResults[["confintRandom"]]$object <- out
  }

  return(out)
}

.mammFitConfintRandomFun         <- function(fit, options) {

  if (options[["subgroup"]] == "") {
    progressBarCode <- paste0("jaspBase::startProgressbar(",.mammConfintIterations(fit),", label = 'Random effects / model components: Confidence intervals')")
  } else {
    progressBarCode <- paste0("jaspBase::startProgressbar(",.mammConfintIterations(fit),", label = 'Subgroup ", attr(fit, "subgroup")," :Random effects / model components: Confidence intervals')")
  }

  confintRandom <- confint(
    fit,
    level = 100 * options[["confidenceIntervalsLevel"]],
    code1 = progressBarCode,
    code2 = "jaspBase::progressbarTick()"
  )

  # when multiple elements are present the last one is an `attribute` with information
  confintRandom <- confintRandom[!names(confintRandom) == "digits"]
  if (any(names(confintRandom) == "random")) {
    confintRandom <- list(confintRandom)
  }

  # flatten
  confintRandom <- do.call(rbind, lapply(confintRandom, function(x) {
    cbind.data.frame(parameter = rownames(x[[1]]), data.frame(x[[1]]))
  }))


  return(confintRandom)
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
  if (options[["analysis"]] != "metaAnalysisMultilevelMultivariate")
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
  if (options[["analysis"]] != "metaAnalysisMultilevelMultivariate")
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

.mammExtractTauLevelNamesList    <- function(fit) {

  levelNames <- list()

  for (i in seq_along(fit)) {
    if (jaspBase::isTryError(fit[[i]]) || is.null(fit[[i]]))
      next
    levelNames[[length(levelNames) + 1]] <- .mammExtractTauLevelNames(fit[[i]])
  }

  levelNames <- unique(unlist(levelNames))
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
