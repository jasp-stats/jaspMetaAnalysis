# WAAP-WLS computed-column exports.
#
# Estimated power and WAAP inclusion of every estimate. Both are defined by the
# WLS model of all estimates, so they are exported also when WLS is reported.

.wwExportColumns                        <- function(jaspResults, dataset, options) {

  if (!(options[["exportWaapWlsEstimatedPower"]] || options[["exportWaapWlsIncludedInWaap"]]) || is.null(dataset) || !.maReady(options))
    return()

  .maValidateExportPrefix(options)

  power    <- rep(NA_real_,      .maExportDatasetRows(dataset))
  included <- rep(NA_character_, .maExportDatasetRows(dataset))

  for (fit in .maExportFitList(jaspResults, options, nonClustered = TRUE)) {

    selection <- attr(fit, "waapWlsDecision")[["selection"]]
    if (!is.list(selection))
      next

    # the WLS model holds all estimates of the subgroup (the reported WAAP model only the retained ones)
    datasetOrder <- .maExportDatasetOrder(.maReferenceFit(fit))
    if (length(datasetOrder) != length(selection[["retained"]]))
      next

    power[datasetOrder]    <- selection[["power"]]
    included[datasetOrder] <- ifelse(selection[["retained"]], "Yes", "No")
  }

  if (options[["exportWaapWlsEstimatedPower"]])
    .maExportScaleColumn(jaspResults, "Estimated Power", power, c(.maExportDependencies(options), "exportWaapWlsEstimatedPower"), options)

  if (options[["exportWaapWlsIncludedInWaap"]])
    .maExportNominalColumn(jaspResults, "Included in WAAP", included, c(.maExportDependencies(options), "exportWaapWlsIncludedInWaap"), options)

  return()
}
