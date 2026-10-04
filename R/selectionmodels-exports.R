# Selection-model computed-column exports.
#
# Restores original row positions and exports every candidate independently of
# the detailed-output filter.

# Candidate predictions and residuals ----

.smExportColumns                        <- function(jaspResults, fits, options) {

  if (is.null(fits) || (!options[["exportPredictedValues"]] && !options[["exportResidualsRaw"]]))
    return()

  if (is.null(jaspResults[["selectionExports"]])) {

    container <- createJaspContainer()
    container$dependOn(c(
      .smDependencies,
      "exportPredictedValues", "exportResidualsRaw",
      "exportColumnPrefix", "exportColumnPrefixValue",
      "includeFullDatasetInSubgroupAnalysis"
    ))
    jaspResults[["selectionExports"]] <- container

  } else {
    return()
  }

  .maValidateExportPrefix(options)

  container <- jaspResults[["selectionExports"]]

  # Display filters do not limit exports; only the subgroup/full-dataset switch does.
  for (scope in .smVisibleScopes(fits, options)) {
    for (i in seq_along(scope$models)) {

      fit <- scope$models[[i]]

      if (inherits(fit, "try-error"))
        next

      data   <- attr(fit, "dataset")
      rows   <- attr(data, "selectionRows")
      n      <- attr(data, "selectionTotalRows")
      values <- list()

      # Untranslated statistic names, as in the classical exports, keep
      # .maIsExportColumnName() ownership detection locale-independent.
      if (options[["exportResidualsRaw"]])
        values[["Residuals: Raw"]] <- as.numeric(stats::residuals(fit))

      if (options[["exportPredictedValues"]]) {
        prediction <- .smCapture(.maExportPredictedDataFrame(fit))

        if (!inherits(prediction, "try-error")) {
          for (column in names(prediction))
            values[[paste0("Predicted Values: ", column)]] <- prediction[[column]]
        }
      }

      for (name in names(values)) {

        # Restore omitted observations as missing values in their original positions.
        column       <- rep(NA_real_, n)
        column[rows] <- values[[name]]
        columnName   <- gettextf("Model %1$i: %2$s: %3$s", i, scope$label, name)

        .maExportScaleColumn(
          container,
          columnName,
          column,
          c(
            .smDependencies, "exportPredictedValues", "exportResidualsRaw",
            "includeFullDatasetInSubgroupAnalysis", .maExportNameOptions
          ),
          options
        )
      }
    }
  }
}
