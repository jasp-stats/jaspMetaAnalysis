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

EffectSizeAggregation <- function(jaspResults, dataset, options, state = NULL) {

  ready <- .esaReady(options)

  .esaComputeAggregation(jaspResults, dataset, options, ready)
  .esaCreateSummaryTable(jaspResults, dataset, options, ready)
  .esaExportData(jaspResults, dataset, options, ready)

  if (!is.null(options[["showMetaforRCode"]]) && options[["showMetaforRCode"]])
    .esaShowMetaforRCode(jaspResults, options, ready)

  return()
}

.esaDependencies <- c(
  # input columns
  "effectSize", "effectSizeStandardError", "cluster",
  # aggregation options
  "weighted", "addClusterSize", "computeSamplingVariance",
  "aggregatedColumnNamesEffectSize", "aggregatedColumnNamesStandardError",
  "aggregatedColumnNamesSamplingVariance", "aggregatedColumnNamesClusterSize",
  .effectSizeVarianceCovarianceMatrixDependencies
)

.esaReady <- function(options) {
  effectSizeReady <- options[["effectSize"]] != ""
  varianceReady   <- options[["effectSizeStandardError"]] != ""
  clusterReady    <- options[["cluster"]] != ""

  return(effectSizeReady && varianceReady && clusterReady)
}

.esaComputeAggregation <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["esaState"]]))
    return()

  esaState <- createJaspState()
  esaState$dependOn(.esaDependencies)
  jaspResults[["esaState"]] <- esaState

  if (!ready)
    return()

  # build sampling variance from the SE column
  vi <- dataset[[options[["effectSizeStandardError"]]]]^2

  # construct escalc object
  dat <- metafor::escalc(
    measure = "GEN",
    yi      = dataset[[options[["effectSize"]]]],
    vi      = vi,
    data    = dataset
  )

  # build aggregate call arguments
  aggArgs <- list(
    x        = dat,
    cluster  = dataset[[options[["cluster"]]]],
    weighted = options[["weighted"]],
    na.rm    = TRUE,
    addk     = options[["addClusterSize"]]
  )

  if (options[["varianceCovarianceMatrixType"]] == "simple") {

    # check simple readiness
    simpleReady  <- .esaSimpleReady(options)
    vcovMessages <- attr(simpleReady, "messages")
    if (!simpleReady) {
      esaState$object <- list(vcovMessages = vcovMessages)
      return()
    }

    # simple path: pass struct/rho/phi/time directly to aggregate.escalc
    aggArgs$struct <- options[["varianceCovarianceMatrixSimpleStructure"]]

    rho <- options[["varianceCovarianceMatrixSimpleWithinClusterCorrelation"]]
    if (length(rho) > 0) {
      aggArgs$rho <- rho
    }

    phi <- options[["varianceCovarianceMatrixSimpleTimeLag1Correlation"]]
    if (length(phi) > 0) {
      aggArgs$phi <- phi
    }

    timeVar <- options[["varianceCovarianceMatrixSimpleTimeVariable"]]
    if (length(timeVar) > 0 && !is.na(timeVar) && timeVar != "") {
      aggArgs$time <- dataset[[timeVar]]
    }
      

  } else {
    # check non-simple readiness
    vcovReady    <- tryCatch(.mammVarianceCovarianceMatrixReady(options), error = function(e) FALSE)
    vcovMessages <- attr(vcovReady, "messages")
    if (!vcovReady) {
      esaState$object <- list(vcovMessages = vcovMessages)
      return()
    }

    # add samplingVariance column for .mammGetVarianceCovarianceMatrix compatibility
    dat$samplingVariance <- vi

    vMatrix <- tryCatch(
      .mammGetVarianceCovarianceMatrix(dat, options),
      error = function(e) e
    )

    if (inherits(vMatrix, "error")) {
      esaState$object <- list(vcovMessages = c(vcovMessages, vMatrix$message))
      return()
    }

    aggArgs$V <- vMatrix

    # save V matrix if requested
    savePath <- options[["varianceCovarianceMatrixSaveComputedVarianceCovarianceMatrix"]]
    if (!is.null(savePath) && savePath != "") {
      try(write.table(vMatrix, file = savePath, sep = ",", row.names = FALSE, col.names = FALSE))
    }
      
  }

  aggResult <- try(do.call(metafor::aggregate.escalc, aggArgs))

  if (inherits(aggResult, "try-error")) {
    esaState$object <- list(error = .esaCleanError(aggResult))
    return()
  }

  esaState$object <- list(
    result       = aggResult,
    cluster      = dataset[[options[["cluster"]]]],
    vcovMessages = vcovMessages
  )
}

.esaSimpleReady <- function(options) {

  ready     <- TRUE
  messages  <- c()
  structure <- options[["varianceCovarianceMatrixSimpleStructure"]]
  usesCS    <- structure %in% c("CS", "CS+CAR", "CS*CAR")
  usesCAR   <- structure %in% c("CAR", "CS+CAR", "CS*CAR")

  rho <- options[["varianceCovarianceMatrixSimpleWithinClusterCorrelation"]]
  if (usesCS && length(rho) > 0 && rho == 0) {
    messages <- c(messages, gettext("The within-cluster correlation is set to 0. This corresponds to no adjustment for dependency between effect sizes within clusters."))
  }
    

  if (usesCAR) {
    timeVar <- options[["varianceCovarianceMatrixSimpleTimeVariable"]]
    if (length(timeVar) == 0 || is.na(timeVar) || timeVar == "") {
      ready    <- FALSE
      messages <- c(messages, gettext("Please provide a time variable for autoregressive correlation structures."))
    }

    phi <- options[["varianceCovarianceMatrixSimpleTimeLag1Correlation"]]
    if (length(phi) > 0 && phi == 0) {
      messages <- c(messages, gettext("The lag-1 correlation is set to 0. This corresponds to no time-based dependency adjustment."))
    }
  }

  attr(ready, "messages") <- messages
  return(ready)
}

.esaCleanError <- function(err) {
  msg <- attr(err, "condition")$message
  if (is.null(msg)) {
    msg <- as.character(err)
  }
  msg <- gsub("\\n", " ", msg)
  return(msg)
}

.esaCreateSummaryTable <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  summaryTable <- createJaspTable(title = gettext("Aggregation Summary"))
  summaryTable$dependOn(.esaDependencies)
  summaryTable$position <- 1

  summaryTable$addColumnInfo(name = "nInput",    title = gettext("Input Rows"),    type = "integer")
  summaryTable$addColumnInfo(name = "nClusters", title = gettext("Clusters"),      type = "integer")
  summaryTable$addColumnInfo(name = "method",    title = gettext("Method"),         type = "string")

  jaspResults[["summaryTable"]] <- summaryTable

  if (!ready)
    return()

  cached <- jaspResults[["esaState"]]$object
  if (is.null(cached))
    return()

  # show footnotes when waiting for variance-covariance matrix inputs
  if (is.null(cached$result)) {
    for (msg in cached$vcovMessages) {
      summaryTable$addFootnote(msg)
    }
      
    if (!is.null(cached$error)) {
      summaryTable$setError(cached$error)
    }
      
    return()
  }

  aggResult <- cached$result

  if (options[["varianceCovarianceMatrixType"]] == "simple") {
    methodLabel <- switch(options[["varianceCovarianceMatrixSimpleStructure"]],
      "ID"     = gettext("Independent"),
      "CS"     = gettext("Compound symmetry"),
      "CAR"    = gettext("Autoregressive"),
      "CS+CAR" = gettext("Compound symmetry + autoregressive"),
      "CS*CAR" = gettext("Compound symmetry \u00d7 autoregressive")
    )
  } else {
    methodLabel <- switch(options[["varianceCovarianceMatrixType"]],
      precomputed           = gettext("Precomputed variance-covariance matrix"),
      correlationMatrix     = gettext("Correlation matrix"),
      constructsGroupsTimes = gettext("Constructs/groups/times")
    )
  }

  summaryTable$addRows(list(
    nInput    = nrow(dataset),
    nClusters = nrow(aggResult),
    method    = methodLabel
  ))

  summaryTable$addFootnote(gettextf(
    "%1$i effect sizes aggregated into %2$i cluster-level estimates.",
    nrow(dataset), nrow(aggResult)
  ))

  # add variance-covariance matrix readiness notes
  for (msg in cached$vcovMessages)
    summaryTable$addFootnote(msg)
}

.esaExportData <- function(jaspResults, dataset, options, ready) {

  if (!ready)
    return()

  cached <- jaspResults[["esaState"]]$object
  if (is.null(cached) || is.null(cached$result))
    return()

  aggResult     <- cached$result
  clusterValues <- cached$cluster
  clusterVar    <- options[["cluster"]]
  nRows         <- nrow(dataset)

  # map aggregated values back to first row of each cluster
  yiOut <- rep(NA_real_, nRows)
  viOut <- rep(NA_real_, nRows)

  aggCluster <- aggResult[[clusterVar]]
  for (i in seq_len(nrow(aggResult))) {
    firstRow       <- which(clusterValues == aggCluster[i])[1]
    yiOut[firstRow] <- as.numeric(aggResult$yi[i])
    viOut[firstRow] <- as.numeric(aggResult$vi[i])
  }

  # effect size column
  columnName <- options[["aggregatedColumnNamesEffectSize"]]
  .esaValidateColumnName(columnName)
  jaspResults[["esColYi"]] <- createJaspColumn(columnName = columnName, dependencies = .esaDependencies)
  jaspResults[["esColYi"]]$setScale(yiOut)

  # standard error or sampling variance column
  computeSamplingVariance <- .esaComputeSamplingVariance(options)
  columnName <- if (computeSamplingVariance)
    options[["aggregatedColumnNamesSamplingVariance"]]
  else
    options[["aggregatedColumnNamesStandardError"]]

  .esaValidateColumnName(columnName)
  jaspResults[["esColSe"]] <- createJaspColumn(columnName = columnName, dependencies = .esaDependencies)
  jaspResults[["esColSe"]]$setScale(if (computeSamplingVariance) viOut else sqrt(viOut))

  # cluster size column
  if (options[["addClusterSize"]]) {
    kiOut <- rep(NA_real_, nRows)
    for (i in seq_len(nrow(aggResult))) {
      firstRow      <- which(clusterValues == aggCluster[i])[1]
      kiOut[firstRow] <- aggResult$ki[i]
    }
    columnName <- options[["aggregatedColumnNamesClusterSize"]]
    .esaValidateColumnName(columnName)
    jaspResults[["esColKi"]] <- createJaspColumn(columnName = columnName, dependencies = .esaDependencies)
    jaspResults[["esColKi"]]$setScale(kiOut)
  }
}

.esaValidateColumnName <- function(columnName) {
  if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
    .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))
}

.esaComputeSamplingVariance <- function(options) {
  return(!is.null(options[["computeSamplingVariance"]]) && options[["computeSamplingVariance"]])
}

.esaShowMetaforRCode <- function(jaspResults, options, ready) {

  if (!ready || !is.null(jaspResults[["metaforRCode"]]))
    return()

  if (options[["varianceCovarianceMatrixType"]] == "simple") {
    simpleReady <- .esaSimpleReady(options)
    if (!simpleReady)
      return()
  } else if (!tryCatch(.mammVarianceCovarianceMatrixReady(options), error = function(e) FALSE)) {
    return()
  }

  metaforRCode <- createJaspHtml(title = gettext("Metafor R Code"))
  metaforRCode$dependOn(c(.esaDependencies, "showMetaforRCode"))
  metaforRCode$position <- 99

  metaforRCode$text <- .maTransformToHtml(.esaMakeMetaforCallText(options))

  jaspResults[["metaforRCode"]] <- metaforRCode

  return()
}

.esaMakeMetaforCallText <- function(options) {

  datInput <- list(
    measure = .esaQuoteRString("GEN"),
    yi      = .esaAsRName(options[["effectSize"]]),
    vi      = paste0(.esaAsRName(options[["effectSizeStandardError"]]), "^2"),
    data    = "dataset"
  )

  aggInput <- list(
    x        = "dat",
    cluster  = .esaAsRName(options[["cluster"]]),
    weighted = options[["weighted"]],
    na.rm    = TRUE,
    addk     = options[["addClusterSize"]]
  )

  vcalcText <- NULL
  if (options[["varianceCovarianceMatrixType"]] == "simple") {
    aggInput$struct <- .esaQuoteRString(options[["varianceCovarianceMatrixSimpleStructure"]])

    rho <- options[["varianceCovarianceMatrixSimpleWithinClusterCorrelation"]]
    if (length(rho) > 0)
      aggInput$rho <- rho

    phi <- options[["varianceCovarianceMatrixSimpleTimeLag1Correlation"]]
    if (length(phi) > 0)
      aggInput$phi <- phi

    timeVar <- options[["varianceCovarianceMatrixSimpleTimeVariable"]]
    if (length(timeVar) > 0 && !is.na(timeVar) && timeVar != "")
      aggInput$time <- .esaAsRName(timeVar)
  } else if (tryCatch(.mammVarianceCovarianceMatrixReady(options), error = function(e) FALSE)) {
    vcalcInput       <- .mammGetVarianceCovarianceMatrix(NULL, options, returnCall = TRUE)
    vcalcInput$data  <- as.name("dat")

    aggInput$V <- "effectSizeVarianceCovarianceMatrix"

    if (options[["varianceCovarianceMatrixType"]] == "precomputed") {
      vcalcText <- paste0(
        "effectSizeVarianceCovarianceMatrix <- ",
        .esaQuoteRString(vcalcInput[["file"]])
      )
    } else {
      vcalcText <- paste0(
        "dat$samplingVariance <- dat$vi\n\n",
        "effectSizeVarianceCovarianceMatrix <- vcalc(\n\t",
        paste(names(vcalcInput), "=", vcalcInput, collapse = ",\n\t"),
        "\n)"
      )
    }
  }

  datText <- paste0(
    "dat <- escalc(\n\t",
    paste(names(datInput), "=", datInput, collapse = ",\n\t"),
    "\n)"
  )

  aggText <- paste0(
    "aggResult <- aggregate.escalc(\n\t",
    paste(names(aggInput), "=", aggInput, collapse = ",\n\t"),
    "\n)"
  )

  return(paste(c(datText, vcalcText, aggText), collapse = "\n\n"))
}

.esaAsRName <- function(name) {
  return(deparse(as.name(name), width.cutoff = 500))
}

.esaQuoteRString <- function(value) {
  return(paste0("'", gsub("'", "\\\\'", value), "'"))
}
