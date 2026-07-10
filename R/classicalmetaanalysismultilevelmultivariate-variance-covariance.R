# Multilevel/multivariate variance-covariance helpers.
#
# Builds, describes, exports, and names sampling variance-covariance matrices.

.mammGetVarianceCovarianceMatrix    <- function(dataset, options, returnCall = FALSE) {

  if (options[["varianceCovarianceMatrixType"]] == "precomputed") {

    # load a pre-computed correlation matrix and check the dimensions against the dataset
    vMatrixFileName <- options[["varianceCovarianceMatrixFile"]]

    if (vMatrixFileName == "")
      return()

    vMatrix <- try(as.matrix(read.csv(file = vMatrixFileName, header = FALSE)))

    if (returnCall)
      return(list(file = vMatrixFileName))

    if (inherits(vMatrix, "try-error"))
      .quitAnalysis(gettextf("Error reading the variance-covariance matrix file: %1$s", vMatrix))

    # if there is only one column, try csv2 (indicates different decimals enconding)
    if (ncol(vMatrix) == 1)
      vMatrix <- try(as.matrix(read.csv2(file = vMatrixFileName, header = FALSE)))

    if (inherits(vMatrix, "try-error"))
      .quitAnalysis(gettextf("Error reading the variance-covariance matrix file: %1$s", vMatrix))

    if (nrow(vMatrix) != ncol(vMatrix))
      .quitAnalysis(gettextf("The variance-covariance matrix must be square. The number of rows (%1$i) does not match the number of columns (%2$i).",
                             nrow(vMatrix), ncol(vMatrix)))

    # extract the subgroup relevant section if subgroups are specified
    if (options[["subgroup"]] != "" && !is.null(attr(dataset, "subgroupIndx")))
      vMatrix <- vMatrix[attr(dataset, "subgroupIndx"), attr(dataset, "subgroupIndx")]

    nasIds <- attr(dataset, "NasIds")
    if (!is.null(nasIds) && nrow(vMatrix) == length(nasIds))
      vMatrix <- vMatrix[!nasIds, !nasIds, drop = FALSE]

    if (nrow(vMatrix) != nrow(dataset))
      .quitAnalysis(gettextf("The variance-covariance matrix must match the dimensions of the data set. The number of the matrix rows/columns (%1$i) does not match the number of observations in the dataset (%2$i).",
                             nrow(vMatrix), nrow(dataset)))


    if (options[["varianceCovarianceMatrixForcePositiveDefiniteness"]]) {
      isPD <- !any(eigen(vMatrix, symmetric = TRUE, only.values = TRUE)$values <= .Machine$double.eps)
      if (!isPD) {
        # only force into PD if is not PD
        vMatrix <- Matrix::nearPD(vMatrix, corr = FALSE)
      }
    }

    if (options[["varianceCovarianceMatrixCheckPositiveDefiniteness"]]) {
      isPD <- !any(eigen(vMatrix, symmetric = TRUE, only.values = TRUE)$values <= .Machine$double.eps)
      if (!isPD)
        .quitAnalysis(gettextf("The supplied variance-covariance matrix is not positive definite."))
    }

    return(vMatrix)

  } else if (options[["varianceCovarianceMatrixType"]] == "correlationMatrix") {

    vcalcCal <- list(
      vi      = as.name("samplingVariance"),
      rvars   = as.call(c(quote(c), lapply(options[["varianceCovarianceMatrixCorrelationMatrix"]], as.name))),
      cluster = as.name(options[["varianceCovarianceMatrixCluster"]]),
      data    = dataset,
      checkpd = options[["varianceCovarianceMatrixCheckPositiveDefiniteness"]],
      nearpd  = options[["varianceCovarianceMatrixForcePositiveDefiniteness"]]
    )

    if (options[["varianceCovarianceMatrixSubcluster"]] != "")
      vcalcCal$subgroup <- as.name(options[["varianceCovarianceMatrixSubcluster"]])

    if (returnCall) {
      return(vcalcCal)
    }
    vMatrix <- try(do.call(metafor::vcalc, vcalcCal))

    # try cleaning the error message before returning
    if (jaspBase::isTryError(vMatrix)) {

      if (grepl("(max(k.cluster) > length(rvars))", vMatrix, fixed = TRUE))
        .quitAnalysis(gettext("The Variance-Covariance Matrix could not be computed: There must be as many 'Correlation Matrix' entries as is the number of estimates within the largest 'Cluster'."))

      vMatrix <- gsub("rvars",     "Correlation Matrix", vMatrix)
      vMatrix <- gsub("k.cluster", "Number of Clusters",  vMatrix)

      .quitAnalysis(gettextf("Error computing the variance-covariance matrix: %1$s", vMatrix))
    }

    return(vMatrix)

  } else if (options[["varianceCovarianceMatrixType"]] == "constructsGroupsTimes") {

    vcalcCal <- list(
      vi      = as.name("samplingVariance"),
      data    = dataset
    )

    # add clusters and subclusters
    if (options [["varianceCovarianceMatrixSubcluster"]] != "") {
      vcalcCal$subgroup <- as.name(options[["varianceCovarianceMatrixSubcluster"]])
    }
    if (options [["varianceCovarianceMatrixCluster"]] != "") {
      vcalcCal$cluster  <- as.name(options[["varianceCovarianceMatrixCluster"]])
    }

    # resolve construsts and subconstructs
    if (options[["varianceCovarianceMatrixConstruct"]] != "") {

      vcalcCal$obs <- as.name(options[["varianceCovarianceMatrixConstruct"]])

      # get the rho
      if (options[["varianceCovarianceMatrixConstructCorrelationMatrix"]] == "commonCorrelation") {
        # set a single value
        rho1 <- options[["varianceCovarianceMatrixConstructCorrelationMatrixValue"]]
      } else if(options[["varianceCovarianceMatrixConstructCorrelationMatrix"]] == "correlationMatrix") {
        # load a complete matrix
        rho1File <- options[["varianceCovarianceMatrixConstructCorrelationMatrixFilePath"]]

        if (rho1File != "") {
          if (returnCall) {
            rho1 <- rho1File
          } else  {
            rho1 <- try(as.matrix(read.csv(file = rho1File, row.names = 1)))

            if (inherits(rho1, "try-error"))
              .quitAnalysis(gettextf("Error reading the variance-covariance matrix file: %1$s", rho1))

            # if there is only one column, try csv2 (indicates different decimals enconding)
            if (ncol(rho1) == 1)
              rho1 <- try(as.matrix(read.csv2(file = rho1File, row.names = 1)))

            if (inherits(rho1, "try-error"))
              .quitAnalysis(gettextf("Error reading the variance-covariance matrix file: %1$s", rho1))

            if (nrow(rho1) != ncol(rho1))
              .quitAnalysis(gettextf("The variance-covariance matrix must be square. The number of rows (%1$i) does not match the number of columns (%2$i).",
                                     nrow(rho1), ncol(rho1)))
          }
        } else {
          rho1 <- 0
        }
      }
      # set rho
      vcalcCal$rho <- rho1
    }

    if (options[["varianceCovarianceMatrixConstructType"]] != "") {

      vcalcCal$type <- as.name(options[["varianceCovarianceMatrixConstructType"]])

      if (options[["varianceCovarianceMatrixConstructTypeCorrelationMatrix"]] == "commonCorrelation") {
        # set a single value
        rho2 <- options[["varianceCovarianceMatrixConstructTypeCorrelationMatrixValue"]]
      } else if(options[["varianceCovarianceMatrixConstructTypeCorrelationMatrix"]] == "correlationMatrix") {
        # load a complete matrix
        rho2File <- options[["varianceCovarianceMatrixConstructTypeCorrelationMatrixFilePath"]]

        if (rho2File != "") {
          if (returnCall) {
            rho2 <- rho2File
          } else  {
            rho2 <- try(as.matrix(read.csv(file = rho2File, row.names = 1)))

            if (inherits(rho2, "try-error"))
              .quitAnalysis(gettextf("Error reading the variance-covariance matrix file: %1$s", rho2))

            # if there is only one column, try csv2 (indicates different decimals enconding)
            if (ncol(rho2) == 1)
              rho2 <- try(as.matrix(read.csv2(file = rho2File, row.names = 1)))

            if (inherits(rho2, "try-error"))
              .quitAnalysis(gettextf("Error reading the variance-covariance matrix file: %1$s", rho2))

            if (nrow(rho2) != ncol(rho2))
              .quitAnalysis(gettextf("The variance-covariance matrix must be square. The number of rows (%1$i) does not match the number of columns (%2$i).",
                                     nrow(rho2), ncol(rho2)))
          }
        } else {
          rho2 <- 0
        }
      }
      # add rho
      if (!is.null(vcalcCal$rho)) {
        vcalcCal$rho <- list(vcalcCal$rho, rho2)
      } else {
        vcalcCal$rho <- rho2
      }
    }

    # add time lags
    if (options[["varianceCovarianceMatrixTime1"]] != "") {

      vcalcCal$time1 <- as.name(options[["varianceCovarianceMatrixTime1"]])

      if (options[["varianceCovarianceMatrixTime2"]] != "") {
        vcalcCal$time2 <- as.name(options[["varianceCovarianceMatrixTime2"]])
      }

      vcalcCal$phi <- options[["varianceCovarianceMatrixTimeLag1Correlation"]]
    }

    # add weights
    if (options[["varianceCovarianceMatrixGroup1"]] != "") {
      vcalcCal$grp1 <- as.name(options[["varianceCovarianceMatrixGroup1"]])
      if (options[["varianceCovarianceMatrixGroupSize1"]] != "") {
        vcalcCal$w1 <- as.name(options[["varianceCovarianceMatrixGroupSize1"]])
      }

      if (options[["varianceCovarianceMatrixGroup1"]] != "") {
        vcalcCal$grp2 <- as.name(options[["varianceCovarianceMatrixGroup2"]])
        if (options[["varianceCovarianceMatrixGroupSize2"]] != "") {
          vcalcCal$w2 <- as.name(options[["varianceCovarianceMatrixGroupSize2"]])
        }
      }
    }

    # make the correlation matrix call
    if (returnCall) {
      return(vcalcCal)
    }
    vMatrix <- try(do.call(metafor::vcalc, vcalcCal))

    # try cleaning the error message before returning
    if (jaspBase::isTryError(vMatrix)) {
      if (grepl("'rho'", vMatrix) && grepl("'obs'", vMatrix)) {
        vMatrix <- gsub("'rho'",  "'Construct Correlation Matrix'", vMatrix)
        vMatrix <- gsub("'obs'",  "'Construct Correlation Matrix'", vMatrix)
      }
      if (grepl("'rho'", vMatrix) && grepl("'type'", vMatrix)) {
        vMatrix <- gsub("'rho'",  "'Construct Type Correlation Matrix'", vMatrix)
        vMatrix <- gsub("'type'", "'Construct Type Correlation Matrix'", vMatrix)
      }
      if (grepl("'phi'", vMatrix)) {
        vMatrix <- gsub("'time1'",  "'Time 1'", vMatrix)
        vMatrix <- gsub("'time2'",  "'Time 2'", vMatrix)
        vMatrix <- gsub("'phi'",  "'Time lag 1 correlation'", vMatrix)
      }
      if (grepl("'grp", vMatrix)) {
        vMatrix <- gsub("'grp1'",  "'Group 1'", vMatrix)
        vMatrix <- gsub("'grp2'",  "'Group 2'", vMatrix)
      }
      if (grepl("'w", vMatrix)) {
        vMatrix <- gsub("'w1'",  "'Group Size 1'", vMatrix)
        vMatrix <- gsub("'w2'",  "'Group Size 2'", vMatrix)
      }
      .quitAnalysis(gettextf("Error computing the variance-covariance matrix: %1$s", vMatrix))
    }

    return(vMatrix)
  }
}

.mammVarianceCovarianceMatrixReady  <- function(options) {

  ready    <- FALSE
  messages <- c()

  if (options[["varianceCovarianceMatrixType"]] == "precomputed") {

    # load a pre-computed correlation matrix and check the dimensions against the dataset
    vMatrixFileName <- options[["varianceCovarianceMatrixFile"]]

    if (vMatrixFileName == "") {
      messages <- c(messages, gettext("Please provide a file with the precomputed variance-covariance matrix."))
    } else {
      ready <- TRUE
    }


  } else if (options[["varianceCovarianceMatrixType"]] == "correlationMatrix") {

    if (options[["varianceCovarianceMatrixCluster"]] == "" && length(options[["varianceCovarianceMatrixCorrelationMatrix"]]) > 0) {
      messages <- c(messages, gettext("Please provide a clustering variable for the correlation matrix."))
    } else if (options[["varianceCovarianceMatrixCluster"]] != "" && length(options[["varianceCovarianceMatrixCorrelationMatrix"]]) == 0) {
      messages <- c(messages, gettext("Please provide correlation matrix variables."))
    } else if (options[["varianceCovarianceMatrixCluster"]] != "" && length(options[["varianceCovarianceMatrixCorrelationMatrix"]]) > 0) {
      ready <- TRUE
    }

  } else if (options[["varianceCovarianceMatrixType"]] == "constructsGroupsTimes") {

    if (options[["varianceCovarianceMatrixCluster"]] == "" &&
        (options[["varianceCovarianceMatrixSubcluster"]]    != "" ||
         options[["varianceCovarianceMatrixConstructType"]] != "" ||
         options[["varianceCovarianceMatrixConstruct"]]     != "" ||
         options[["varianceCovarianceMatrixTime1"]]         != "" ||
         options[["varianceCovarianceMatrixGroup1"]]        != "")) {
      messages <- c(messages, gettext("Please provide a clustering variable for the correlation matrix."))
    }

    if (options[["varianceCovarianceMatrixCluster"]] != "" && options[["varianceCovarianceMatrixConstructType"]] != "") {
      if (options[["varianceCovarianceMatrixConstructTypeCorrelationMatrix"]] == "commonCorrelation") {
        ready <- TRUE
        if (options[["varianceCovarianceMatrixConstructTypeCorrelationMatrixValue"]] == 0) {
          # ready but with odd settings
          messages <- c(messages, gettext("The value of the correlation between construct types is set to 0. This corresponds to no adjustment for dependency between construct types."))
        }
      } else if (options[["varianceCovarianceMatrixConstructTypeCorrelationMatrix"]] == "correlationMatrix") {
        if (options[["varianceCovarianceMatrixConstructTypeCorrelationMatrixFilePath"]] == "") {
          messages <- c(messages, gettext("Please provide a file with the correlation matrix for the construct types."))
        } else {
          ready <- TRUE
        }
      }
    }
    if (options[["varianceCovarianceMatrixCluster"]] != "" && options[["varianceCovarianceMatrixConstruct"]] != "") {
      if (options[["varianceCovarianceMatrixConstructCorrelationMatrix"]] == "commonCorrelation") {
        ready <- TRUE
        if (options[["varianceCovarianceMatrixConstructCorrelationMatrixValue"]] == 0) {
          # ready but with odd settings
          messages <- c(messages, gettext("The value of the correlation between construct types is set to 0. This corresponds to no adjustment for dependency between constructs."))
        }
      } else if (options[["varianceCovarianceMatrixConstructCorrelationMatrix"]] == "correlationMatrix") {
        if (options[["varianceCovarianceMatrixConstructCorrelationMatrixFilePath"]] == "") {
          messages <- c(messages, gettext("Please provide a file with the correlation matrix for the construct."))
        } else {
          ready <- TRUE
        }
      }
    }
    if (options[["varianceCovarianceMatrixCluster"]] != "" && options[["varianceCovarianceMatrixTime1"]] != "") {

      ready <- TRUE
      if (options[["varianceCovarianceMatrixTimeLag1Correlation"]] == 0) {
        # ready but with odd settings
        messages <- c(messages, gettext("The value of the correlation between time lags is set to 0. This corresponds to no adjustment for dependency between time lags."))
      }
    }
    if (options[["varianceCovarianceMatrixCluster"]] != "" && options[["varianceCovarianceMatrixGroup1"]] != "") {

      ready <- TRUE
      if (options[["varianceCovarianceMatrixGroupSize1"]] == "") {
        # ready with default settings
        messages <- c(messages, gettext("The 'Group Size' for specifying dependency due to common group comparison is unspecified. As default, the groups are assumed to be equal."))
      }
    }

  }

  attr(ready, "messages") <- messages
  return(ready)
}

.mammVarianceCovarianceMatrixMessage <- function(options) {

  if (!.mammVarianceCovarianceMatrixReady(options))
    return(NULL)

  if (options[["varianceCovarianceMatrixType"]] == "precomputed")
    return(gettext("The model was estimated using the selected precomputed working effect-size variance-covariance matrix."))

  details <- .mammVarianceCovarianceMatrixDetails(options)
  if (is.null(details))
    return(NULL)

  return(gettextf(
    "The model was estimated using a working effect-size variance-covariance matrix based on %1$s.",
    .mammVarianceCovarianceMatrixJoin(details)
  ))
}

.mammVarianceCovarianceMatrixDetails <- function(options) {

  details <- switch(
    options[["varianceCovarianceMatrixType"]],
    "correlationMatrix" = c(
      .mammVarianceCovarianceMatrixVariablePhrase(
        options[["varianceCovarianceMatrixCorrelationMatrix"]],
        gettext("correlation matrix variables")
      ),
      .mammVarianceCovarianceMatrixClusterPhrases(options)
    ),
    "constructsGroupsTimes" = c(
      .mammVarianceCovarianceMatrixCorrelationPhrase(
        options,
        "varianceCovarianceMatrixConstruct",
        gettext("construct"),
        "varianceCovarianceMatrixConstructCorrelationMatrix",
        "varianceCovarianceMatrixConstructCorrelationMatrixValue"
      ),
      .mammVarianceCovarianceMatrixCorrelationPhrase(
        options,
        "varianceCovarianceMatrixConstructType",
        gettext("construct type"),
        "varianceCovarianceMatrixConstructTypeCorrelationMatrix",
        "varianceCovarianceMatrixConstructTypeCorrelationMatrixValue"
      ),
      .mammVarianceCovarianceMatrixTimePhrase(options),
      .mammVarianceCovarianceMatrixVariablePairPhrase(
        options[["varianceCovarianceMatrixGroup1"]],
        gettext("group 1"),
        options[["varianceCovarianceMatrixGroup2"]],
        gettext("group 2")
      ),
      .mammVarianceCovarianceMatrixVariablePairPhrase(
        options[["varianceCovarianceMatrixGroupSize1"]],
        gettext("group size 1"),
        options[["varianceCovarianceMatrixGroupSize2"]],
        gettext("group size 2")
      ),
      .mammVarianceCovarianceMatrixClusterPhrases(options)
    )
  )

  details <- details[details != ""]
  if (length(details) == 0)
    return(NULL)

  return(details)
}

.mammVarianceCovarianceMatrixCorrelationPhrase <- function(options, variableOption, variableRole, correlationMatrixOption, correlationValueOption) {
  variableName <- .mammVarianceCovarianceMatrixVariableNames(options[[variableOption]])
  if (is.null(variableName))
    return(NULL)

  correlationDescription <- switch(
    options[[correlationMatrixOption]],
    "commonCorrelation" = gettextf(
      "with a common correlation of %1$s",
      .mammVarianceCovarianceMatrixCorrelationValue(options[[correlationValueOption]])
    ),
    "correlationMatrix" = gettext("with a known correlation matrix")
  )

  return(gettextf("%1$s as %2$s %3$s", variableName, variableRole, correlationDescription))
}

.mammVarianceCovarianceMatrixTimePhrase <- function(options) {
  time1 <- .mammVarianceCovarianceMatrixVariableNames(options[["varianceCovarianceMatrixTime1"]])
  if (is.null(time1))
    return(NULL)

  timePhrases <- gettextf("%1$s as %2$s", time1, gettext("time 1"))

  time2 <- .mammVarianceCovarianceMatrixVariableNames(options[["varianceCovarianceMatrixTime2"]])
  if (!is.null(time2))
    timePhrases <- c(timePhrases, gettextf("%1$s as %2$s", time2, gettext("time 2")))

  return(gettextf(
    "%1$s with lag-1 correlation of %2$s",
    .mammVarianceCovarianceMatrixJoin(timePhrases),
    .mammVarianceCovarianceMatrixCorrelationValue(options[["varianceCovarianceMatrixTimeLag1Correlation"]])
  ))
}

.mammVarianceCovarianceMatrixVariablePairPhrase <- function(variable1, role1, variable2, role2) {
  variable1 <- .mammVarianceCovarianceMatrixVariableNames(variable1)
  if (is.null(variable1))
    return(NULL)

  phrases <- gettextf("%1$s as %2$s", variable1, role1)

  variable2 <- .mammVarianceCovarianceMatrixVariableNames(variable2)
  if (!is.null(variable2))
    phrases <- c(phrases, gettextf("%1$s as %2$s", variable2, role2))

  return(.mammVarianceCovarianceMatrixJoin(phrases))
}

.mammVarianceCovarianceMatrixVariablePhrase <- function(variables, role) {
  variableNames <- .mammVarianceCovarianceMatrixVariableNames(variables)
  if (is.null(variableNames))
    return(NULL)

  return(gettextf("%1$s as %2$s", .mammVarianceCovarianceMatrixJoin(variableNames), role))
}

.mammVarianceCovarianceMatrixClusterPhrases <- function(options) {
  cluster <- .mammVarianceCovarianceMatrixVariableNames(options[["varianceCovarianceMatrixCluster"]])
  if (is.null(cluster))
    return(NULL)

  phrases <- gettextf("%1$s as %2$s", cluster, gettext("cluster"))

  subcluster <- .mammVarianceCovarianceMatrixVariableNames(options[["varianceCovarianceMatrixSubcluster"]])
  if (!is.null(subcluster))
    phrases <- c(phrases, gettextf("%1$s as %2$s", subcluster, gettext("subcluster")))

  return(phrases)
}

.mammVarianceCovarianceMatrixVariableNames <- function(variables) {
  variables <- unlist(variables, use.names = FALSE)
  variables <- variables[!is.na(variables) & variables != ""]

  if (length(variables) == 0)
    return(NULL)

  return(decodeColNames(variables))
}

.mammVarianceCovarianceMatrixCorrelationValue <- function(value) {
  return(sprintf("%.3g", value))
}

.mammVarianceCovarianceMatrixJoin <- function(values) {
  values <- values[values != ""]

  if (length(values) == 0)
    return(NULL)

  if (length(values) == 1)
    return(values)

  if (length(values) == 2)
    return(gettextf("%1$s and %2$s", values[1], values[2]))

  return(gettextf("%1$s, and %2$s", paste(values[-length(values)], collapse = ", "), values[length(values)]))
}

.mammExportVarianceCovarianceMatrix <- function(dataset, options) {

  if (!.mammVarianceCovarianceMatrixReady(options))
    return()

  V <- .mammGetVarianceCovarianceMatrix(dataset, options)
  try(write.table(V, file = options[["varianceCovarianceMatrixSaveComputedVarianceCovarianceMatrix"]], sep = ",", row.names = FALSE, col.names = FALSE))

  if (jaspBase::isTryError(V))
    .quitAnalysis(gettextf("Error writing the variance-covariance matrix file: %1$s", V))

  return()
}

.mammExtractRandomVariableNames           <- function(options) {

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

.mammExtractVarianceCovarianceMatrixNames <- function(options) {

  if (options[["varianceCovarianceMatrixType"]] == "precomputed") {
    return()
  } else if (options[["varianceCovarianceMatrixType"]] == "correlationMatrix") {

    # varianceCovarianceMatrixCorrelationMatrix can contain NA's as it is only lower triangular
    variableNames <- c(
      options[["varianceCovarianceMatrixCluster"]],
      options[["varianceCovarianceMatrixSubcluster"]]
    )
    variableNames <- variableNames[variableNames != ""]
    return(variableNames)

  } else if (options[["varianceCovarianceMatrixType"]] == "constructsGroupsTimes") {

    variableNames <- c(
      options[["varianceCovarianceMatrixCluster"]],
      options[["varianceCovarianceMatrixSubcluster"]],
      options[["varianceCovarianceMatrixConstructType"]],
      options[["varianceCovarianceMatrixConstruct"]],
      options[["varianceCovarianceMatrixTime1"]],
      options[["varianceCovarianceMatrixTime2"]],
      options[["varianceCovarianceMatrixGroup1"]],
      options[["varianceCovarianceMatrixGroup2"]],
      options[["varianceCovarianceMatrixGroupSize1"]],
      options[["varianceCovarianceMatrixGroupSize2"]]
    )
    variableNames <- variableNames[variableNames != ""]
    return(variableNames)
  }
}
