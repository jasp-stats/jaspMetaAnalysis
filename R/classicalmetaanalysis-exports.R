# Classical meta-analysis computed-column exports.
#
# Builds residual, prediction, random-effect, weight, and diagnostic export columns.

# Export orchestration ----

.maCasewiseDiagnosticsExportColumns      <- function(jaspResults, dataset, options) {
  .maExportDiagnosticsColumns(jaspResults, dataset, options)
  return()
}

.maExportColumns                         <- function(jaspResults, dataset, options) {

  if (!.maAnyExportColumns(options) || is.null(dataset) || !.maReady(options))
    return()

  .maExportDiagnosticsColumns(jaspResults, dataset, options)
  .maExportResidualColumns(jaspResults, dataset, options)
  .maExportPredictedColumns(jaspResults, dataset, options)
  .maExportTrueEffectColumns(jaspResults, dataset, options)
  .maExportRandomEffectsColumns(jaspResults, dataset, options)
  .maExportWeightsColumns(jaspResults, dataset, options)

  return()
}

.maAnyExportColumns                      <- function(options) {

  exportOptions <- .maExportOptions
  return(any(vapply(exportOptions, function(option) isTRUE(options[[option]]), logical(1))))
}

# Diagnostic exports ----

.maExportDiagnosticsColumns              <- function(jaspResults, dataset, options) {

  if (!.maAnyDiagnosticsExportColumns(options))
    return()

  # the fit diagnostics work only for the non-clustered fit
  fit <- .maExportFitList(jaspResults, options, nonClustered = TRUE)

  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  diagnostics <- .maDiagnostics(jaspResults, options)

  if (options[["subgroup"]] != "" && isTRUE(options[["includeFullDatasetInSubgroupAnalysis"]]))
    diagnostics <- diagnostics[-1]

  exportOptions <- options
  exportOptions[["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"]] <- .maExportDiagnosticsCoefficientInfluence(options) && !.maIsMetaregressionHeterogeneity(options)

  diagnosticsTable <- .maSafeRbind(lapply(seq_along(fit), function(i) .maRowDiagnosticsTable(
    fit         = fit[[i]],
    diagnostics = diagnostics[[attr(fit[[i]], "subgroup")]],
    options     = exportOptions,
    forExport   = TRUE
  )))

  if (is.null(diagnosticsTable) || !"datasetOrder" %in% colnames(diagnosticsTable))
    return()

  diagnosticsTable <- .maExportCompleteRows(diagnosticsTable, dataset)

  if (.maIsMetaregressionHeterogeneity(options))
    .maExportUnavailableDiagnosticsColumns(jaspResults, dataset, options)

  if (.maExportDiagnosticsInfluentialCases(options) && "inf" %in% colnames(diagnosticsTable))
    .maExportNominalColumn(jaspResults, "Diagnostics: Influential", diagnosticsTable[["inf"]], c(.maExportDependencies(), "exportDiagnosticsInfluentialCases"))

  if (.maExportDiagnosticsCaseDiagnostics(options)) {
    for (diagnosticName in intersect(c("rstudent", "cook.d", "hat", "weight"), colnames(diagnosticsTable))) {
      columnName <- paste0("Diagnostics: ", .maCasewiseDiagnosticsExportColumnsNames(diagnosticName))
      .maExportScaleColumn(jaspResults, columnName, diagnosticsTable[[diagnosticName]], c(.maExportDependencies(), "exportDiagnosticsCaseDiagnostics"))
    }
  }

  if (.maExportDiagnosticsModelImpact(options)) {
    for (diagnosticName in intersect(c("dffits", "cov.r", "tau.del", "tau2.del", "QE.del"), colnames(diagnosticsTable))) {
      columnName <- paste0("Diagnostics: ", .maCasewiseDiagnosticsExportColumnsNames(diagnosticName))
      .maExportScaleColumn(jaspResults, columnName, diagnosticsTable[[diagnosticName]], c(.maExportDependencies(), "exportDiagnosticsModelImpact"))
    }
  }

  if (.maExportDiagnosticsCoefficientInfluence(options)) {
    coefficientNames <- setdiff(colnames(diagnosticsTable), c("subgroup", "label", "datasetOrder", .maCasewiseDiagnosticsNames()))
    variables        <- c(unlist(options[["effectSizeModelTerms"]]), unlist(options[["heterogeneityModelTerms"]]))

    for (diagnosticName in coefficientNames) {
      columnName <- decodeColNames(paste0("Difference in coefficients: ", .maVariableNames(diagnosticName, variables)))
      .maExportScaleColumn(jaspResults, columnName, diagnosticsTable[[diagnosticName]], c(.maExportDependencies(), "exportDiagnosticsCoefficientInfluence"))
    }
  }

  return()
}

.maAnyDiagnosticsExportColumns            <- function(options) {
  return(
    .maExportDiagnosticsInfluentialCases(options) ||
      .maExportDiagnosticsCaseDiagnostics(options) ||
      .maExportDiagnosticsModelImpact(options) ||
      .maExportDiagnosticsCoefficientInfluence(options)
  )
}

.maExportDiagnosticsInfluentialCases      <- function(options) {
  if (.maIsMultilevelMultivariate(options))
    return(FALSE)

  return(isTRUE(options[["exportDiagnosticsInfluentialCases"]]))
}

.maExportDiagnosticsCaseDiagnostics       <- function(options) {
  return(isTRUE(options[["exportDiagnosticsCaseDiagnostics"]]))
}

.maExportDiagnosticsModelImpact           <- function(options) {
  if (.maIsMultilevelMultivariate(options))
    return(FALSE)

  return(isTRUE(options[["exportDiagnosticsModelImpact"]]))
}

.maExportDiagnosticsCoefficientInfluence  <- function(options) {
  return(isTRUE(options[["exportDiagnosticsCoefficientInfluence"]]))
}

.maExportUnavailableDiagnosticsColumns    <- function(jaspResults, dataset, options) {

  nRows <- .maExportDatasetRows(dataset)

  if (.maExportDiagnosticsInfluentialCases(options))
    .maExportNominalColumn(jaspResults, "Diagnostics: Influential", rep(NA_character_, nRows), c(.maExportDependencies(), "exportDiagnosticsInfluentialCases"))

  if (.maExportDiagnosticsCaseDiagnostics(options))
    .maExportScaleColumn(jaspResults, "Diagnostics: Cook's Distance", rep(NA_real_, nRows), c(.maExportDependencies(), "exportDiagnosticsCaseDiagnostics"))

  if (.maExportDiagnosticsModelImpact(options)) {
    for (diagnosticName in c("dffits", "cov.r", "tau.del", "tau2.del", "QE.del")) {
      columnName <- paste0("Diagnostics: ", .maCasewiseDiagnosticsExportColumnsNames(diagnosticName))
      .maExportScaleColumn(jaspResults, columnName, rep(NA_real_, nRows), c(.maExportDependencies(), "exportDiagnosticsModelImpact"))
    }
  }

  if (.maExportDiagnosticsCoefficientInfluence(options)) {
    coefficientNames <- .maExportCoefficientInfluenceNames(dataset, options)
    variables        <- unlist(options[["effectSizeModelTerms"]])

    for (diagnosticName in coefficientNames) {
      columnName <- decodeColNames(paste0("Difference in coefficients: ", .maVariableNames(diagnosticName, variables)))
      .maExportScaleColumn(jaspResults, columnName, rep(NA_real_, nRows), c(.maExportDependencies(), "exportDiagnosticsCoefficientInfluence"))
    }
  }

  return()
}

.maExportCoefficientInfluenceNames        <- function(dataset, options) {

  coefficientNames <- character(0)

  if (isTRUE(options[["effectSizeModelIncludeIntercept"]]))
    coefficientNames <- c(coefficientNames, "intrcpt")

  formula <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  if (!is.null(formula)) {
    modelMatrix <- stats::model.matrix(formula, dataset)
    coefficientNames <- c(coefficientNames, colnames(modelMatrix))
  }

  coefficientNames[coefficientNames == "(Intercept)"] <- "intrcpt"
  coefficientNames <- unique(coefficientNames)

  return(coefficientNames)
}

# Fitted-value exports ----

.maExportResidualColumns                  <- function(jaspResults, dataset, options) {

  fit <- .maExportFitList(jaspResults, options)
  if (is.null(fit))
    return()

  if (isTRUE(options[["exportResidualsRaw"]])) {
    values <- .maExportVectorFromFitList(
      dataset        = dataset,
      fit            = fit,
      exportFunction = function(fit) stats::residuals(fit, type = "response"),
      isAvailable    = function(fit) .maExportFitSupportsResidual(fit, "response")
    )
    .maExportScaleColumn(jaspResults, "Residuals: Raw", values, c(.maExportDependencies(), "exportResidualsRaw"))
  }

  if (isTRUE(options[["exportResidualsPearson"]])) {
    values <- .maExportVectorFromFitList(
      dataset        = dataset,
      fit            = fit,
      exportFunction = function(fit) stats::residuals(fit, type = "pearson"),
      isAvailable    = function(fit) .maExportFitSupportsResidual(fit, "pearson")
    )
    .maExportScaleColumn(jaspResults, "Residuals: Pearson", values, c(.maExportDependencies(), "exportResidualsPearson"))
  }

  if (isTRUE(options[["exportResidualsStandardized"]])) {
    values <- .maExportVectorFromFitList(
      dataset        = dataset,
      fit            = fit,
      exportFunction = function(fit) stats::residuals(fit, type = "rstandard"),
      isAvailable    = function(fit) .maExportFitSupportsResidual(fit, "rstandard")
    )
    .maExportScaleColumn(jaspResults, "Residuals: Standardized", values, c(.maExportDependencies(), "exportResidualsStandardized"))
  }

  if (isTRUE(options[["exportResidualsStudentized"]])) {
    values <- .maExportVectorFromFitList(
      dataset        = dataset,
      fit            = fit,
      exportFunction = function(fit) stats::residuals(fit, type = "rstudent"),
      isAvailable    = function(fit) .maExportFitSupportsResidual(fit, "rstudent")
    )
    .maExportScaleColumn(jaspResults, "Residuals: Studentized", values, c(.maExportDependencies(), "exportResidualsStudentized"))
  }

  if (isTRUE(options[["exportResidualsConditional"]]) && !.maIsMultilevelMultivariate(options)) {
    values <- .maExportVectorFromFitList(
      dataset        = dataset,
      fit            = fit,
      exportFunction = function(fit) stats::rstandard(fit, type = "conditional")[["z"]],
      isAvailable    = function(fit) .maExportFitSupportsResidual(fit, "conditional")
    )
    .maExportScaleColumn(jaspResults, "Residuals: Conditional Standardized", values, c(.maExportDependencies(), "exportResidualsConditional"))
  }

  return()
}

.maExportPredictedColumns                 <- function(jaspResults, dataset, options) {

  if (!isTRUE(options[["exportPredictedValues"]]))
    return()

  fit     <- .maExportFitList(jaspResults, options)
  columns <- .maExportDataFrameFromFitList(dataset, fit, .maExportPredictedDataFrame, .maExportFitSupportsPredicted)

  .maExportScaleColumns(jaspResults, columns, "Predicted Values", c(.maExportDependencies(), "exportPredictedValues"))

  return()
}

.maExportTrueEffectColumns                <- function(jaspResults, dataset, options) {

  if (!isTRUE(options[["exportTrueEffectEstimates"]]) || .maIsMultilevelMultivariate(options))
    return()

  fit     <- .maExportFitList(jaspResults, options)
  columns <- .maExportDataFrameFromFitList(dataset, fit, .maExportTrueEffectDataFrame, .maExportFitSupportsTrueEffect)

  .maExportScaleColumns(jaspResults, columns, "True Effect Estimates (BLUPs)", c(.maExportDependencies(), "exportTrueEffectEstimates"))

  return()
}

.maExportRandomEffectsColumns             <- function(jaspResults, dataset, options) {

  if (!isTRUE(options[["exportRandomEffects"]]))
    return()

  fit <- .maExportFitList(jaspResults, options)

  if (.maIsMultilevelMultivariate(options)) {
    columns <- .maExportRandomEffectsMvColumns(dataset, fit)
  } else {
    columns <- .maExportDataFrameFromFitList(dataset, fit, .maExportRandomEffectsDataFrame, .maExportFitSupportsRandomEffects)
  }

  .maExportScaleColumns(jaspResults, columns, "Random Effects", c(.maExportDependencies(), "exportRandomEffects"))

  return()
}

.maExportWeightsColumns                   <- function(jaspResults, dataset, options) {

  if (!isTRUE(options[["exportWeights"]]))
    return()

  fit <- .maExportFitList(jaspResults, options)

  if (.maIsMultilevelMultivariate(options)) {
    rowSumValues <- .maExportVectorFromFitList(
      dataset        = dataset,
      fit            = fit,
      exportFunction = function(fit) {
        .maExportWeights(
          fit     = fit,
          options = options,
          type    = "rowsum"
        )
      },
      isAvailable    = .maExportFitSupportsWeights
    )
    .maExportScaleColumn(jaspResults, "Weights: Row Sum", rowSumValues, c(.maExportDependencies(), "exportWeights"))

    diagonalValues <- .maExportVectorFromFitList(
      dataset        = dataset,
      fit            = fit,
      exportFunction = function(fit) {
        .maExportWeights(
          fit     = fit,
          options = options,
          type    = "diagonal"
        )
      },
      isAvailable    = .maExportFitSupportsWeights
    )
    .maExportScaleColumn(jaspResults, "Weights: Diagonal", diagonalValues, c(.maExportDependencies(), "exportWeights"))

    return()
  }

  values <- .maExportVectorFromFitList(
    dataset        = dataset,
    fit            = fit,
    exportFunction = function(fit) {
      .maExportWeights(
        fit     = fit,
        options = options,
        type    = "diagonal"
      )
    },
    isAvailable    = .maExportFitSupportsWeights
  )
  .maExportScaleColumn(jaspResults, "Weights", values, c(.maExportDependencies(), "exportWeights"))

  return()
}

.maExportFitList                          <- function(jaspResults, options, nonClustered = FALSE) {

  fit <- .maExtractFit(jaspResults, options, nonClustered = nonClustered)

  if (is.null(fit))
    return(NULL)

  if (options[["subgroup"]] != "" && isTRUE(options[["includeFullDatasetInSubgroupAnalysis"]]))
    fit <- fit[-1]

  return(fit)
}

.maExportCompleteRows                     <- function(df, dataset) {

  df <- .maSafeRbind(list(
    df,
    data.frame("datasetOrder" = setdiff(seq_len(.maExportDatasetRows(dataset)), df[["datasetOrder"]]))
  ))

  df <- df[order(df[["datasetOrder"]]),, drop = FALSE]

  return(df)
}

.maExportDataFrameFromFitList             <- function(dataset, fit, exportFunction, isAvailable = NULL) {

  columns <- list()
  if (is.null(fit))
    return(columns)

  for (i in seq_along(fit)) {
    if (jaspBase::isTryError(fit[[i]]))
      next
    if (!is.null(isAvailable) && !isAvailable(fit[[i]]))
      next

    datasetOrder <- .maExportDatasetOrder(fit[[i]])
    if (length(datasetOrder) == 0)
      next

    values <- exportFunction(fit[[i]])
    if (is.null(values) || nrow(values) == 0)
      next

    values <- .maExportRecycleRows(values, length(datasetOrder))
    if (is.null(values))
      next

    for (columnName in colnames(values)) {
      if (is.null(columns[[columnName]]))
        columns[[columnName]] <- rep(NA_real_, .maExportDatasetRows(dataset))
      columns[[columnName]][datasetOrder] <- values[[columnName]]
    }
  }

  return(columns)
}

.maExportVectorFromFitList                <- function(dataset, fit, exportFunction, isAvailable = NULL) {

  output    <- rep(NA_real_, .maExportDatasetRows(dataset))
  hasValues <- FALSE
  if (is.null(fit))
    return(NULL)

  for (i in seq_along(fit)) {
    if (jaspBase::isTryError(fit[[i]]))
      next
    if (!is.null(isAvailable) && !isAvailable(fit[[i]]))
      next

    datasetOrder <- .maExportDatasetOrder(fit[[i]])
    if (length(datasetOrder) == 0)
      next

    values <- exportFunction(fit[[i]])
    if (is.null(values))
      next

    values <- as.numeric(values)
    if (length(values) == 1 && length(datasetOrder) > 1)
      values <- rep(values, length(datasetOrder))
    if (length(values) != length(datasetOrder))
      next

    output[datasetOrder] <- values
    hasValues <- TRUE
  }

  return(output)
}

# Multilevel random-effect exports ----

.maExportRandomEffectsMvColumns           <- function(dataset, fit) {

  columns <- list()
  if (is.null(fit))
    return(columns)

  for (i in seq_along(fit)) {
    if (jaspBase::isTryError(fit[[i]]))
      next
    if (!.maExportFitSupportsRandomEffects(fit[[i]]))
      next

    datasetOrder <- .maExportDatasetOrder(fit[[i]])
    randomEffects <- try(metafor::ranef(fit[[i]]), silent = TRUE)
    if (jaspBase::isTryError(randomEffects) || is.null(randomEffects))
      next

    if (is.matrix(randomEffects)) {
      columns <- .maExportRandomEffectsMvMatrixColumns(columns, dataset, fit[[i]], randomEffects, datasetOrder)
      next
    }

    for (componentName in names(randomEffects)) {
      component <- randomEffects[[componentName]]
      rowIndex  <- .maExportRandomEffectsMvRowIndex(fit[[i]], componentName, component)

      if (is.null(rowIndex))
        next

      matched <- component[rowIndex,, drop = FALSE]
      if (nrow(matched) != length(datasetOrder))
        next

      componentLabel <- .maExportCleanComponentName(componentName)
      for (columnName in colnames(matched)) {
        if (!is.numeric(matched[[columnName]]))
          next

        outputName <- paste0(componentLabel, " - ", .maExportStatisticName(columnName))
        if (is.null(columns[[outputName]]))
          columns[[outputName]] <- rep(NA_real_, .maExportDatasetRows(dataset))
        columns[[outputName]][datasetOrder] <- matched[[columnName]]
      }
    }
  }

  return(columns)
}

.maExportFitSupportsResidual              <- function(fit, type) {

  if (!inherits(fit, "rma"))
    return(FALSE)
  if (inherits(fit, "rma.glmm"))
    return(type == "response")
  if (type == "pearson")
    return(TRUE)
  if (type == "rstudent" && inherits(fit, "rma.ls"))
    return(FALSE)
  if (type == "conditional")
    return(inherits(fit, "rma.uni"))
  if (type %in% c("rstandard", "rstudent"))
    return(inherits(fit, c("rma.uni", "rma.mv", "rma.mh", "rma.peto")))

  return(TRUE)
}

.maExportFitSupportsPredicted             <- function(fit) {
  return(inherits(fit, "rma"))
}

.maExportFitSupportsTrueEffect            <- function(fit) {
  return(inherits(fit, "rma.uni") && !inherits(fit, c("rma.gen", "rma.uni.selmodel")))
}

.maExportFitSupportsRandomEffects         <- function(fit) {
  return(
    inherits(fit, c("rma.uni", "rma.mv")) &&
      !inherits(fit, c("rma.gen", "rma.uni.selmodel"))
  )
}

.maExportFitSupportsWeights               <- function(fit) {
  return(
    inherits(fit, c("rma.uni", "rma.mv", "rma.mh", "rma.peto")) &&
      !inherits(fit, c("rma.gen", "rma.uni.selmodel"))
  )
}

.maExportPredictedDataFrame               <- function(fit) {
  return(.maExportListRmaDataFrame(stats::predict(fit), c("pred", "se", "ci.lb", "ci.ub", "pi.lb", "pi.ub")))
}

.maExportTrueEffectDataFrame              <- function(fit) {
  return(.maExportListRmaDataFrame(metafor::blup(fit), c("pred", "se", "pi.lb", "pi.ub")))
}

.maExportRandomEffectsDataFrame           <- function(fit) {
  return(.maExportListRmaDataFrame(metafor::ranef(fit), c("pred", "se", "pi.lb", "pi.ub")))
}

.maExportWeights                          <- function(fit, options, type) {

  if (.maIsMultilevelMultivariate(options)) {
    if (type == "diagonal")
      return(stats::weights(fit, type = "diagonal"))
    if (!isTRUE(fit[["int.only"]]))
      return(NULL)
    return(stats::weights(fit, type = "rowsum"))
  }

  return(stats::weights(fit, type = "diagonal"))
}

.maExportListRmaDataFrame                 <- function(x, columns) {

  values <- list()
  lengths <- integer(0)

  for (columnName in columns) {
    if (!is.null(x[[columnName]]) && length(x[[columnName]]) > 0) {
      values[[.maExportStatisticName(columnName)]] <- as.numeric(x[[columnName]])
      lengths <- c(lengths, length(x[[columnName]]))
    }
  }

  if (length(values) == 0)
    return(NULL)

  nRows <- max(lengths)
  for (columnName in names(values)) {
    if (length(values[[columnName]]) == 1 && nRows > 1)
      values[[columnName]] <- rep(values[[columnName]], nRows)
    if (length(values[[columnName]]) != nRows)
      values[[columnName]] <- NULL
  }

  return(as.data.frame(values, check.names = FALSE))
}

.maExportRecycleRows                      <- function(values, nRows) {

  if (nrow(values) == nRows)
    return(values)
  if (nrow(values) == 1 && nRows > 1)
    return(values[rep(1, nRows),, drop = FALSE])

  return(NULL)
}

.maExportCleanComponentName               <- function(componentName) {
  componentName <- sub("^~", "", componentName)
  componentName <- gsub("\\s+", " ", componentName)
  return(trimws(componentName))
}

.maExportRandomEffectsMvMatrixColumns     <- function(columns, dataset, fit, randomEffects, datasetOrder) {

  randomPart <- .maExportRandomEffectsMvMatrixRandomPart(fit)
  if (is.null(randomPart))
    return(columns)

  rowIndex <- .maExportRandomEffectsMvMatrixRowIndex(fit, randomEffects)
  if (is.null(rowIndex))
    return(columns)

  componentLabel <- .maExportRandomEffectFormulaName(fit, randomPart)

  matched <- randomEffects[rowIndex,, drop = FALSE]
  if (nrow(matched) != length(datasetOrder))
    return(columns)

  for (columnName in colnames(matched)) {
    if (!is.numeric(matched[, columnName]))
      next

    outputName <- paste0(componentLabel, " - Estimate: ", .maExportRandomEffectCoefficientName(columnName))
    if (is.null(columns[[outputName]]))
      columns[[outputName]] <- rep(NA_real_, .maExportDatasetRows(dataset))
    columns[[outputName]][datasetOrder] <- matched[, columnName]
  }

  return(columns)
}

.maExportRandomEffectsMvRowIndex          <- function(fit, componentName, component) {

  sIndex <- match(componentName, fit[["s.names"]])
  if (!is.na(sIndex))
    return(.maExportRowsFromDesignMatrix(fit[["Z.S"]][[sIndex]], nrow(component)))

  if (identical(componentName, .maExportRandomEffectFormulaName(fit, "g")))
    return(.maExportInnerOuterRandomEffectRows(fit, component, "g"))

  if (identical(componentName, .maExportRandomEffectFormulaName(fit, "h")))
    return(.maExportInnerOuterRandomEffectRows(fit, component, "h"))

  return(NULL)
}

.maExportRandomEffectsMvMatrixRowIndex    <- function(fit, randomEffects) {

  randomPart <- .maExportRandomEffectsMvMatrixRandomPart(fit)
  if (is.null(randomPart))
    return(NULL)

  modelFrame <- fit[[paste0("mf.", randomPart)]]
  if (is.null(modelFrame) || !"outer" %in% colnames(modelFrame))
    return(NULL)

  rowIndex <- match(as.character(modelFrame[["outer"]]), rownames(randomEffects))
  if (anyNA(rowIndex))
    return(NULL)

  return(rowIndex)
}

.maExportRandomEffectsMvMatrixRandomPart  <- function(fit) {

  if (isTRUE(fit[["withG"]]) && fit[["struct"]][1] %in% c("GEN", "GDIAG"))
    return("g")

  if (isTRUE(fit[["withH"]]) && fit[["struct"]][2] %in% c("GEN", "GDIAG"))
    return("h")

  return(NULL)
}

.maExportRowsFromDesignMatrix             <- function(designMatrix, nComponentRows) {

  if (is.null(designMatrix))
    return(NULL)

  designMatrix <- as.matrix(designMatrix)
  if (nrow(designMatrix) == 0 || ncol(designMatrix) != nComponentRows)
    return(NULL)

  nonZeroRows <- rowSums(abs(designMatrix) > 0)
  if (any(nonZeroRows != 1))
    return(NULL)

  return(max.col(abs(designMatrix), ties.method = "first"))
}

.maExportInnerOuterRandomEffectRows       <- function(fit, component, randomPart) {

  modelFrame <- fit[[paste0("mf.", randomPart)]]
  if (is.null(modelFrame) || ncol(modelFrame) < 2)
    return(NULL)

  struct <- fit[["struct"]][switch(randomPart, "g" = 1, "h" = 2)]
  if (struct %in% c("GEN", "GDIAG"))
    return(NULL)

  nVars <- ncol(modelFrame)
  outer <- modelFrame[[nVars]]

  if (struct %in% .maExportSpatialRandomEffectStructures()) {
    key         <- paste(seq_len(nrow(modelFrame)), as.character(outer), sep = "\r")
    orderedRows <- which(!duplicated(key))
  } else {
    inner       <- modelFrame[[1]]
    key         <- paste(as.character(inner), as.character(outer), sep = "\r")
    uniqueRows  <- which(!duplicated(key))
    orderedRows <- uniqueRows[order(outer[uniqueRows], inner[uniqueRows])]
  }

  if (nrow(component) != length(orderedRows))
    return(NULL)

  rowIndex <- match(key, key[orderedRows])
  if (anyNA(rowIndex))
    return(NULL)

  return(rowIndex)
}

.maExportRandomEffectFormulaName          <- function(fit, randomPart) {

  formulaIndex <- switch(randomPart, "g" = 1, "h" = 2)
  formulas     <- fit[["formulas"]]

  if (is.null(formulas) || length(formulas) < formulaIndex)
    return("")

  formula <- formulas[[formulaIndex]]

  if (is.null(formula))
    return("")

  return(paste0(formula, collapse = ""))
}

.maExportRandomEffectCoefficientName      <- function(coefficientName) {

  if (coefficientName == "intrcpt")
    return("Intercept")

  return(coefficientName)
}

.maExportSpatialRandomEffectStructures    <- function() {
  return(c("SPEXP", "SPGAU", "SPLIN", "SPRAT", "SPSPH", "PHYBM", "PHYPL", "PHYPD"))
}

.maExportStatisticName                    <- function(columnName) {
  return(switch(
    columnName,
    "pred"  = "Estimate",
    "intrcpt" = "Estimate",
    "se"    = "SE",
    "ci.lb" = "CI Lower",
    "ci.ub" = "CI Upper",
    "pi.lb" = "PI Lower",
    "pi.ub" = "PI Upper",
    columnName
  ))
}

.maExportFitDataset                       <- function(fit) {

  fitData <- attr(fit, "dataset")
  if (is.null(fitData))
    fitData <- attr(fit, "data")

  return(fitData)
}

.maExportDatasetOrder                     <- function(fit) {

  fitData <- .maExportFitDataset(fit)
  if (is.null(fitData))
    return(integer(0))

  nasIds  <- attr(fitData, "NasIds")

  if (!is.null(nasIds))
    datasetOrder <- as.numeric(names(nasIds)[!nasIds])
  else
    datasetOrder <- seq_len(nrow(fitData))

  notNa <- fit[["not.na"]]
  if (!is.null(notNa) && length(notNa) == length(datasetOrder))
    datasetOrder <- datasetOrder[notNa]

  return(datasetOrder)
}

.maExportDatasetRows                      <- function(dataset) {

  nasIds <- attr(dataset, "NasIds")
  if (!is.null(nasIds))
    return(length(nasIds))

  return(nrow(dataset))
}

# JASP column helpers ----

.maExportDependencies                     <- function() {
  return(c(.maDependencies, "includeFullDatasetInSubgroupAnalysis"))
}

.maExportScaleColumns                     <- function(jaspResults, columns, prefix, dependencies) {

  for (columnName in names(columns))
    .maExportScaleColumn(jaspResults, paste0(prefix, ": ", columnName), columns[[columnName]], dependencies)

  return()
}

.maExportScaleColumn                      <- function(jaspResults, columnName, values, dependencies) {

  if (is.null(values))
    return()

  .metaValidateColumnName(columnName)
  jaspResults[[columnName]] <- createJaspColumn(columnName = columnName, dependencies = dependencies)
  jaspResults[[columnName]]$setScale(values)

  return()
}

.maExportNominalColumn                    <- function(jaspResults, columnName, values, dependencies) {

  .metaValidateColumnName(columnName)
  jaspResults[[columnName]] <- createJaspColumn(columnName = columnName, dependencies = dependencies)
  jaspResults[[columnName]]$setNominal(values)

  return()
}

.maCasewiseDiagnosticsExportColumnsNames  <- function(columnName) {

  return(switch(
    columnName,
    "rstudent"  = "Standardized Residual",
    "dffits"    = "DFFITS",
    "cook.d"    = "Cook's Distance",
    "cov.r"     = "Covariance Ratio",
    "tau.del"   = "Tau",
    "tau2.del"  = "Tau2 LOO",
    "QE.del"    = "QE LOO",
    "hat"       = "Hat",
    "weight"    = "Weight",
    "inf"       = "Influential"
  ))
}
