# Selection models.
#
# Prepares shared data and coordinates
# the classical meta-analysis output builders.

# Analysis and dependencies ----

SelectionModels                         <- function(jaspResults, dataset = NULL, options, ...) {

  options[["analysis"]] <- "selectionModels"
  specifications        <- .smSpecifications(options)

  if (.maReady(options)) {
    dataset <- .smCheckData(dataset, options, specifications)
    .maCheckErrors(dataset, options)
  }

  fits          <- .smFitModels(jaspResults, dataset, options, specifications)
  reportingFits <- .smSelectModels(fits, options)

  .smComparisonTable(jaspResults, fits, options, specifications)
  .smModelOutputs(jaspResults, reportingFits, options, specifications)
  .smDiagnostics(jaspResults, reportingFits, options, specifications)
  .smExportColumns(jaspResults, fits, options)
}

.smBaseDependencies <- c(
  "effectSize", "effectSizeStandardError",
  "predictors", "predictors.types", "effectSizeModelTerms", "effectSizeModelIncludeIntercept",
  "subgroup", "pValue", "noSelection", "noSelectionLevel",
  "method", "confidenceIntervalsLevel",
  "optimizerMethod", "optimizerMaximumIterations", "optimizerMaximumIterationsValue",
  "optimizerConvergenceRelativeTolerance", "optimizerConvergenceRelativeToleranceValue"
)

.smSpecificationDependencies <- c(
  "publicationBiasAdjustment", "selectionModels",
  "modelExpectedDirectionOfTheEffect", "selectionForceOrdinality"
)

.smDependencies <- c(.smBaseDependencies, .smSpecificationDependencies)

# Shared data and original row mapping ----

.smCheckData                            <- function(dataset, options, specifications) {

  originalRows   <- nrow(dataset)
  originalGroups <- if (options[["subgroup"]] != "") as.character(dataset[[options[["subgroup"]]]]) else NULL

  # Check the assigned factor before dropping observations. The chosen level can
  # legitimately disappear from a complete-case dataset or an individual subgroup.
  if (options[["noSelection"]] != "") {
    indicator       <- dataset[[options[["noSelection"]]]]
    availableLevels <- if (is.factor(indicator)) levels(indicator) else unique(as.character(indicator[!is.na(indicator)]))

    if (!options[["noSelectionLevel"]] %in% availableLevels)
      .quitAnalysis(gettext("The unaffected level is not present in the no-selection variable. Choose an available level."))
  }

  dataset  <- .maCheckData(dataset, options)
  retained <- which(!attr(dataset, "NasIds"))

  # All candidates use the same complete observations, including optional inputs.
  selectionVariables <- c(options[["pValue"]], options[["noSelection"]])
  selectionVariables <- selectionVariables[selectionVariables != ""]

  if (length(selectionVariables)) {
    keep     <- stats::complete.cases(dataset[, selectionVariables, drop = FALSE])
    dataset  <- dataset[keep, , drop = FALSE]
    retained <- retained[keep]
  }

  if (options[["pValue"]] != "" &&
      any(!is.finite(dataset[[options[["pValue"]]]]) |
          dataset[[options[["pValue"]]]] <= 0 |
          dataset[[options[["pValue"]]]] > 1))
    .quitAnalysis(gettext("P-values must be finite, greater than zero, and at most one."))

  sampleSizeVariables <- .smSampleSizeVariables(specifications)

  if (length(sampleSizeVariables)) {

    keep     <- stats::complete.cases(dataset[, sampleSizeVariables, drop = FALSE])
    dataset  <- dataset[keep, , drop = FALSE]
    retained <- retained[keep]

    for (variable in sampleSizeVariables) {
      if (any(!is.finite(dataset[[variable]]) | dataset[[variable]] <= 0))
        .quitAnalysis(gettext("Sample sizes must be finite and greater than zero."))
    }
  }

  # Finite standard errors can still overflow or underflow when squared.
  standardErrors <- dataset[[options[["effectSizeStandardError"]]]]
  variances      <- standardErrors^2

  if (any(is.finite(standardErrors) & standardErrors > 0 &
          (!is.finite(variances) | variances == 0)))
    .quitAnalysis(gettext("Standard errors are too small or too large to yield finite positive sampling variances. Rescale effect sizes and standard errors."))

  # Keep the original row numbers for code generation and exported columns.
  attr(dataset, "NasIds") <- !seq_len(originalRows) %in% retained
  attr(dataset, "NAs")    <- originalRows - length(retained)

  if (!is.null(originalGroups)) {
    attr(dataset, "selectionOmittedBySubgroup") <- table(factor(
      originalGroups[attr(dataset, "NasIds")],
      levels = unique(originalGroups[!is.na(originalGroups)])
    ))
  }

  attr(dataset, "selectionRows")      <- retained
  attr(dataset, "selectionTotalRows") <- originalRows

  return(dataset)
}
