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


ClassicalMantelHaenszelPeto <- function(jaspResults, dataset = NULL, options, ...) {

  options[["analysis"]] <- "mantelHaenszelPeto"
  options[["predictionIntervals"]] <- FALSE

  if (.maReady(options)) {
    dataset <- .mamhpCheckData(dataset, options)
    .mamhpCheckErrors(dataset, options)
  }

  # fit the model
  .maFitModel(jaspResults, dataset, options)
  .maUpdateFitModelDataset(jaspResults, dataset, options)

  # model summary
  .maOverallTestsTable(jaspResults, options)
  .maPooledEstimatesTable(jaspResults, options)

  if (options[["fitMeasures"]])
    .maFitMeasuresTable(jaspResults, options)

  # plots
  .maUltimateForestPlot(jaspResults, options)

  if (options[["diagnosticsCasewiseDiagnostics"]]) {
    .maCasewiseDiagnosticsTable(jaspResults, options)
    .maCasewiseDiagnosticsExportColumns(jaspResults, dataset, options)
  }

  if (options[["diagnosticsPlotsBaujat"]])
    .maBaujatPlot(jaspResults, options)
  if (options[["diagnosticsResidualFunnel"]])
    .maResidualFunnelPlot(jaspResults, options)

  # additional
  if (options[["showMetaforRCode"]])
    .mamhpShowMetaforRCode(jaspResults, options)

  return()
}

.mamhpCheckData                   <- function(dataset, options) {

  # switch between frequencies / events
  if (options[["method"]] %in% c("mantelHaenszelFrequencies", "peto")) {
    omitOnVariables <- c(
      options[["successesGroup1"]],
      options[["successesGroup2"]],
      options[["sampleSizeGroup1"]],
      options[["sampleSizeGroup2"]],
      if (options[["subgroup"]] != "") options[["subgroup"]]
    )
  } else if (options[["method"]] == "mantelHaenszelEvents") {
    omitOnVariables <- c(
      options[["eventsGroup1"]],
      options[["eventsGroup2"]],
      options[["personTimeGroup1"]],
      options[["personTimeGroup2"]],
      if (options[["subgroup"]] != "") options[["subgroup"]]
    )
  }

  # omit NAs
  anyNaByRows <- apply(dataset[,omitOnVariables], 1, function(x) anyNA(x))
  dataset     <- dataset[!anyNaByRows,]
  attr(dataset, "NAs")    <- sum(anyNaByRows)
  attr(dataset, "NasIds") <- anyNaByRows


  return(dataset)
}
.mamhpCheckErrors                 <- function(dataset, options) {

  # switch between frequencies / events
  if (options[["method"]] %in% c("mantelHaenszelFrequencies", "peto")) {
    variables <- c(
      options[["successesGroup1"]],
      options[["successesGroup2"]],
      options[["sampleSizeGroup1"]],
      options[["sampleSizeGroup2"]],
      if (options[["subgroup"]] != "") options[["subgroup"]]
    )
  } else if (options[["method"]] == "mantelHaenszelEvents") {
    variables <- c(
      options[["eventsGroup1"]],
      options[["eventsGroup2"]],
      options[["personTimeGroup1"]],
      options[["personTimeGroup2"]],
      if (options[["subgroup"]] != "") options[["subgroup"]]
    )
  }

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations"),
    all.target           = variables,
    observations.amount  = "< 2",
    custom               = list(mampCheck = function(dataset, target) {
      nonNegative <- !all(dataset[,target] >= 0, na.rm = TRUE)
      if (nonNegative) {
        return(gettext("All observations must be non-negative."))
      }
    }),
    exitAnalysisIfErrors = TRUE)

}

.mamhpFitModelFun                 <- function(dataset, options, subgroupName) {
  # --------------------------------------------------------------------------- #
  # when updating don't forget to update the '.mamhpMakeMetaforCallText' function! #
  # --------------------------------------------------------------------------- #

  # specify the effect size and outcome
  if (options[["method"]] %in% c("mantelHaenszelFrequencies", "peto")) {
    rmaInput <- list(
      ai   = as.name(options[["successesGroup1"]]),
      ci   = as.name(options[["successesGroup2"]]),
      n1i  = as.name(options[["sampleSizeGroup1"]]),
      n2i  = as.name(options[["sampleSizeGroup2"]]),
      data = dataset
    )
  } else if (options[["method"]] == "mantelHaenszelEvents") {
    rmaInput <- list(
      x1i  = as.name(options[["eventsGroup1"]]),
      x2i  = as.name(options[["eventsGroup2"]]),
      t1i  = as.name(options[["personTimeGroup1"]]),
      t2i  = as.name(options[["personTimeGroup2"]]),
      data = dataset
    )
  }

  rmaInput$measure <- options[["effectSizeMeasure"]]

  # additional input
  rmaInput$level <- 100 * options[["confidenceIntervalsLevel"]]

  # include corrections
  if (options[["method"]] %in% c("mantelHaenszelFrequencies", "peto")) {
    rmaInput$add    <- options[["advancedAdd"]]
    rmaInput$to     <- switch(
      options[["advancedTo"]],
      "all"       = "all",
      "onlyZero"  = "only0",
      "ifAnyZero" = "if0all",
      "none"      = "none"
    )
    rmaInput$drop00 <- switch(
      options[["advancedDropStudiesWithNoCasesOrEvents"]],
      "yes" = TRUE,
      "no"  = FALSE
    )
  }

  # continuity correction
  if (options[["method"]] == "mantelHaenszelFrequencies" && options[["effectSizeMeasure"]] == "OR") {
    rmaInput$correct  <- options[["advancedContinuityCorrection"]]
  }

  ### fit the model
  if (nrow(dataset) < 2) {
    fit <- try(stop("Fewer than two estimates."))
  } else if (options[["method"]] %in% c("mantelHaenszelFrequencies", "mantelHaenszelEvents")) {
    fit <- try(do.call(metafor::rma.mh, rmaInput))
  } else if (options[["method"]] == "peto") {
    fit <- try(do.call(metafor::rma.peto, rmaInput))
  }

  # add attributes
  attr(fit, "subgroup") <- paste0(subgroupName)
  attr(fit, "dataset")  <- dataset

  # return the results
  return(list(
    fit            = fit,
    fitClustered   = NULL
  ))
}
.mamhpRowMantelHaenszelTest       <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit) || (!is.null(fit[["MH"]]) && is.na(fit[["MHp"]]))) {
    return(data.frame(
      subgroup = attr(fit, "subgroup"),
      test     = if (options[["effectSizeMeasure"]] == "OR") gettext("Cochran-Mantel-Haenszel") else gettext("Mantel-Haenszel")
    ))
  }

  row <- data.frame(
    subgroup = attr(fit, "subgroup"),
    test     = if (options[["effectSizeMeasure"]] == "OR") gettext("Cochran-Mantel-Haenszel") else gettext("Mantel-Haenszel"),
    stat     = sprintf(paste0("CMH(1) = ", if (fit[["MH"]] < 1e5) "%1$.2f" else "%1$.3g"), fit[["MH"]]),
    pval     = fit[["MHp"]]
  )

  return(row)
}
.mamhpRowTaroneTest               <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit) || (!is.null(fit[["TA"]]) && is.na(fit[["TAp"]]))) {
    return(data.frame(
      subgroup = attr(fit, "subgroup"),
      test     = gettext("Tarone")
    ))
  }

  row <- data.frame(
    subgroup = attr(fit, "subgroup"),
    test     = gettext("Tarone"),
    stat     = sprintf(paste0("X\U2091(%1$i) = ", if (fit[["TA"]] < 1e5) "%2$.2f" else "%2$.3g"), fit[["k"]] - 1, fit[["TA"]]),
    pval     = fit[["TAp"]]
  )

  return(row)
}
.mamhpComputePooledHeterogeneity     <- function(fit, options) {

  heterogeneity <- data.frame(
    par = c("I\U00B2", "H\U00B2"),
    est = c(fit[["I2"]], fit[["H2"]]),
    lCi = NA,
    uCi = NA
  )
  # keep only the requested parameters
  heterogeneityShow <- c(
    if (options[["heterogeneityI2"]]) 1,
    if (options[["heterogeneityH2"]]) 2
  )

  heterogeneity <- heterogeneity[heterogeneityShow,,drop = FALSE]

  return(heterogeneity)
}
.mamhpComputePooledHeterogeneityPlot <- function(fit, options, parameter = "I2") {

  # don't use the confint on robust.rma objects (they are not implemented)
  # the clustering works only on the fixed effect estimates
  # -> we can drop the class and compute confint and get the heterogeneity from the original fit
  # (the fit is passed directly from from forest plot function so it is cleaner to dispatch it here)

  # dispatch options to the .mamhpComputePooledHeterogeneity function
  options[["heterogeneityTau"]]  <- parameter == FALSE
  options[["heterogeneityTau2"]] <- parameter == FALSE
  options[["heterogeneityI2"]]   <- parameter == "I2"
  options[["heterogeneityH2"]]   <- parameter == "H2"

  # compute the heterogeneity
  confIntHeterogeneity <- .mamhpComputePooledHeterogeneity(fit, options)

  return(confIntHeterogeneity)
}
.mamhpPrintHeterogeneityEstimate     <- function(fit, options, digits, parameter) {

  out <- .mamhpComputePooledHeterogeneityPlot(fit, options, parameter)

  return(sprintf(paste0(
    "%1$s  = ",
    "%2$.", digits, "f"
  ), out$par, out$est))
}
.mamhpShowMetaforRCode               <- function(jaspResults, options) {

  if (!.maReady(options) || !is.null(jaspResults[["metaforRCode"]]))
    return()

  metaforRCode <- createJaspHtml(title = gettext("Metafor R Code"))
  metaforRCode$dependOn(c(.maDependencies, "showMetaforRCode"))
  metaforRCode$position <- 99

  metaforRCode$text <- .maTransformToHtml(.mamhpMakeMetaforCallText(options))

  jaspResults[['metaforRCode']] <- metaforRCode

  return()
}
.mamhpMakeMetaforCallText            <- function(options) {

  # specify the effect size and outcome
  if (options[["method"]] %in% c("mantelHaenszelFrequencies", "peto")) {
    rmaInput <- list(
      ai   = as.name(options[["successesGroup1"]]),
      ci   = as.name(options[["successesGroup2"]]),
      n1i  = as.name(options[["sampleSizeGroup1"]]),
      n2i  = as.name(options[["sampleSizeGroup2"]]),
      data = as.name("dataset")
    )
  } else if (options[["method"]] == "mantelHaenszelEvents") {
    rmaInput <- c(
      x1i  = as.name(options[["eventsGroup1"]]),
      x2i  = as.name(options[["eventsGroup2"]]),
      t1i  = as.name(options[["personTimeGroup1"]]),
      t2i  = as.name(options[["personTimeGroup2"]]),
      data = as.name("dataset")
    )
  }

  rmaInput$measure <- paste0("'", options[["effectSizeMeasure"]], "'")

  # additional input
  rmaInput$level <- 100 * options[["confidenceIntervalsLevel"]]

  # include corrections
  if (options[["method"]] %in% c("mantelHaenszelFrequencies", "peto")) {
    rmaInput$add    <- options[["advancedAdd"]]
    rmaInput$to     <- paste0("'", switch(
      options[["advancedTo"]],
      "all"       = "all",
      "onlyZero"  = "only0",
      "ifAnyZero" = "if0all",
      "none"      = "none"
    ), "'")
    rmaInput$drop00 <- switch(
      options[["advancedDropStudiesWithNoCasesOrEvents"]],
      "yes" = TRUE,
      "no"  = FALSE
    )
  }

  # continuity correction
  if (options[["method"]] == "mantelHaenszelFrequencies" && options[["effectSizeMeasure"]] == "OR") {
    rmaInput$correct  <- options[["advancedContinuityCorrection"]]
  }

  if (options[["method"]] %in% c("mantelHaenszelFrequencies", "mantelHaenszelEvents")) {
    fit <- paste0("fit <- rma.mh(\n\t", paste(names(rmaInput), "=", rmaInput, collapse = ",\n\t"), "\n)\n")
  } else if (options[["method"]] == "peto") {
    fit <- paste0("fit <- rma.peto(\n\t", paste(names(rmaInput), "=", rmaInput, collapse = ",\n\t"), "\n)\n")
  }

  return(fit)
}
