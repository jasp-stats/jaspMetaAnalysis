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

  ready         <- .esaReady(options)
  exportDataset <- dataset
  if (ready) {
    dataset <- .esaCheckData(dataset, options)
    .esaCheckErrors(dataset, options)
  }

  .esaComputeAggregation(jaspResults, dataset, options, ready)
  .esaCreateSummaryTable(jaspResults, dataset, options, ready)
  .esaExportData(jaspResults, exportDataset, options, ready)

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

.esaReady                   <- function(options) {
  effectSizeReady <- options[["effectSize"]] != ""
  varianceReady   <- options[["effectSizeStandardError"]] != ""
  clusterReady    <- options[["cluster"]] != ""

  return(effectSizeReady && varianceReady && clusterReady)
}
.esaCheckData               <- function(dataset, options) {

  # omit NAs
  omitOnVariables <- .esaOmitVariables(options)
  anyNaByRows     <- !stats::complete.cases(dataset[, omitOnVariables, drop = FALSE])
  rowIds          <- seq_len(nrow(dataset))
  dataset         <- dataset[!anyNaByRows,, drop = FALSE]

  # drop empty factor levels
  dataset <- droplevels(dataset)

  attr(dataset, "NAs")    <- sum(anyNaByRows)
  attr(dataset, "NasIds") <- stats::setNames(anyNaByRows, rowIds)
  attr(dataset, "rowIds") <- rowIds[!anyNaByRows]

  return(dataset)
}
.esaOmitVariables          <- function(options) {

  omitOnVariables <- c(
    options[["effectSize"]],
    options[["effectSizeStandardError"]],
    options[["cluster"]]
  )

  if (isTRUE(options[["varianceCovarianceMatrixType"]] == "simple")) {
    structure <- options[["varianceCovarianceMatrixSimpleStructure"]]
    usesCAR   <- structure %in% c("CAR", "CS+CAR", "CS*CAR")
    timeVar   <- options[["varianceCovarianceMatrixSimpleTimeVariable"]]

    if (isTRUE(usesCAR) && length(timeVar) > 0 && !is.na(timeVar) && timeVar != "")
      omitOnVariables <- c(omitOnVariables, timeVar)
  } else {
    omitOnVariables <- c(omitOnVariables, .mammExtractVarianceCovarianceMatrixNames(options))
  }

  omitOnVariables <- omitOnVariables[!is.na(omitOnVariables) & omitOnVariables != ""]

  return(unique(omitOnVariables))
}
.esaCheckErrors            <- function(dataset, options) {

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations"),
    all.target           = c(
      options[["effectSize"]],
      options[["effectSizeStandardError"]]
    ),
    observations.amount  = "< 2",
    exitAnalysisIfErrors = TRUE)

  .hasErrors(
    dataset              = dataset,
    seCheck.target       = options[["effectSizeStandardError"]],
    custom               = .maCheckStandardErrors,
    exitAnalysisIfErrors = TRUE)
}
.esaComputeAggregation      <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["esaState"]]))
    return()

  esaState <- createJaspState()
  esaState$dependOn(.esaDependencies)
  jaspResults[["esaState"]] <- esaState

  if (!ready)
    return()

  rowIds <- attr(dataset, "rowIds")
  if (is.null(rowIds))
    rowIds <- seq_len(nrow(dataset))

  nOmitted <- attr(dataset, "NAs")
  if (is.null(nOmitted))
    nOmitted <- 0L

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
      esaState$object <- list(vcovMessages = vcovMessages, nOmitted = nOmitted)
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
      esaState$object <- list(vcovMessages = vcovMessages, nOmitted = nOmitted)
      return()
    }

    # add samplingVariance column for .mammGetVarianceCovarianceMatrix compatibility
    dat$samplingVariance <- vi
    attr(dat, "NAs")    <- attr(dataset, "NAs")
    attr(dat, "NasIds") <- attr(dataset, "NasIds")
    attr(dat, "rowIds") <- rowIds

    vMatrix <- tryCatch(
      .mammGetVarianceCovarianceMatrix(dat, options),
      error = function(e) e
    )

    if (inherits(vMatrix, "error")) {
      esaState$object <- list(vcovMessages = c(vcovMessages, vMatrix$message), nOmitted = nOmitted)
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
    esaState$object <- list(error = .esaCleanError(aggResult), nOmitted = nOmitted)
    return()
  }

  esaState$object <- list(
    result       = aggResult,
    cluster      = dataset[[options[["cluster"]]]],
    rowIds       = rowIds,
    nOmitted     = nOmitted,
    vcovMessages = vcovMessages
  )
}
.esaSimpleReady             <- function(options) {

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
.esaCleanError              <- function(err) {
  msg <- attr(err, "condition")$message
  if (is.null(msg)) {
    msg <- as.character(err)
  }
  msg <- gsub("\\n", " ", msg)
  return(msg)
}
.esaCreateSummaryTable      <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  summaryTable <- createJaspTable(title = gettext("Aggregation Summary"))
  summaryTable$dependOn(.esaDependencies)
  summaryTable$position <- 1

  summaryTable$addColumnInfo(name = "nInput",    title = gettext("Input Rows"),    type = "integer")
  summaryTable$addColumnInfo(name = "nClusters", title = gettext("Clusters"),      type = "integer")
  summaryTable$addColumnInfo(name = "method",    title = gettext("Method"),         type = "string")

  jaspResults[["summaryTable"]] <- summaryTable

  if (!ready) {
    summaryTable$addFootnote(gettextf(
      "Effect sizes were successfully aggregated and added to the dataset for 0 out of %1$i data entries.",
      nrow(dataset)
    ))
    return()
  }

  cached <- jaspResults[["esaState"]]$object
  if (is.null(cached))
    return()

  .esaAddMissingDataFootnote(summaryTable, cached$nOmitted)

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
    "%1$i effect sizes were successfully aggregated into %2$i cluster-level estimates and added to the dataset.",
    nrow(dataset), nrow(aggResult)
  ))

  # add variance-covariance matrix readiness notes
  for (msg in cached$vcovMessages)
    summaryTable$addFootnote(msg)
}
.esaAddMissingDataFootnote <- function(table, nOmitted) {

  if (is.null(nOmitted) || is.na(nOmitted) || nOmitted == 0)
    return()

  table$addFootnote(sprintf(
    ngettext(
      nOmitted,
      "%i observation was removed due to missing values.",
      "%i observations were removed due to missing values."
    ),
    nOmitted
  ))

  return()
}
.esaExportData              <- function(jaspResults, dataset, options, ready) {

  if (!ready)
    return()

  cached <- jaspResults[["esaState"]]$object
  if (is.null(cached) || is.null(cached$result))
    return()

  aggResult     <- cached$result
  clusterValues <- cached$cluster
  rowIds        <- cached$rowIds
  clusterVar    <- options[["cluster"]]
  nRows         <- nrow(dataset)

  if (is.null(rowIds))
    rowIds <- seq_along(clusterValues)

  # map aggregated values back to first row of each cluster
  yiOut      <- rep(NA_real_, nRows)
  viOut      <- rep(NA_real_, nRows)
  outputRows <- .esaOutputRows(aggResult, clusterVar, clusterValues, rowIds)
  validRows  <- !is.na(outputRows)

  yiOut[outputRows[validRows]] <- as.numeric(aggResult$yi[validRows])
  viOut[outputRows[validRows]] <- as.numeric(aggResult$vi[validRows])

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
    kiOut[outputRows[validRows]] <- aggResult$ki[validRows]
    columnName <- options[["aggregatedColumnNamesClusterSize"]]
    .esaValidateColumnName(columnName)
    jaspResults[["esColKi"]] <- createJaspColumn(columnName = columnName, dependencies = .esaDependencies)
    jaspResults[["esColKi"]]$setScale(kiOut)
  }
}
.esaOutputRows              <- function(aggResult, clusterVar, clusterValues, rowIds) {

  outputRows <- rep(NA_integer_, nrow(aggResult))
  aggCluster <- aggResult[[clusterVar]]

  for (i in seq_len(nrow(aggResult))) {
    firstRow <- which(clusterValues == aggCluster[i])[1]
    if (!is.na(firstRow))
      outputRows[i] <- rowIds[firstRow]
  }

  return(outputRows)
}
.esaValidateColumnName      <- function(columnName) {
  if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
    .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))
}
.esaComputeSamplingVariance <- function(options) {
  return(!is.null(options[["computeSamplingVariance"]]) && options[["computeSamplingVariance"]])
}
.esaShowMetaforRCode        <- function(jaspResults, options, ready) {

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
.esaMakeMetaforCallText     <- function(options) {

  dataPrepText <- .esaMakeDataPrepText(options)

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

  return(paste(c(dataPrepText, datText, vcalcText, aggText), collapse = "\n\n"))
}
.esaMakeDataPrepText        <- function(options) {

  omitOnVariables <- vapply(.esaOmitVariables(options), .esaQuoteRString, character(1))

  return(paste0(
    "dataset <- dataset[complete.cases(dataset[, c(",
    paste(omitOnVariables, collapse = ", "),
    "), drop = FALSE]), ]"
  ))
}
.esaAsRName                 <- function(name) {
  return(deparse(as.name(name), width.cutoff = 500))
}
.esaQuoteRString            <- function(value) {
  return(paste0("'", gsub("'", "\\\\'", value), "'"))
}
