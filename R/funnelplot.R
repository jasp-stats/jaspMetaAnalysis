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

# Funnel-plot entry point, validation, and cached model fits.
#
# Figure builders and table builders are implemented in funnelplot-figures.R and
# funnelplot-tables.R.

FunnelPlot <- function(jaspResults, dataset = NULL, options, ...) {

  options[["analysis"]] <- "metaAnalysis"

  if (.fpReady(options)) {
    # check data set
    dataset <- .fpCheckDataset(jaspResults, dataset, options)

    # pre-fit models if required
    .fpH1Fits(jaspResults, dataset, options)
    .fpH1TrimAndFillFits(jaspResults, dataset, options)
  }

  # make the funnel plots
  .fpPlot(jaspResults, dataset, options)
  if (options[["funnelUnderH1EstimatesTable"]])
    .fpPlotEstimatesTable(jaspResults, dataset, options)

  # add the funnel plot asymmetry table
  if (options[["funnelPlotAsymmetryTests"]])
    .fpTestFunnelPlotAsymmetryTests(jaspResults, dataset, options)

  # add trim and fill
  if (options[["trimAndFill"]]) {
    .fpTrimAndFillPlot(jaspResults, dataset, options)

    if (options[["trimAndFillEstimatesTable"]])
     .fpTrimAndFillEstimatesTable(jaspResults, dataset, options)
  }

  # add fail-safe n
  if (options[["failSafeN"]])
    .fpFailSafeNTable(jaspResults, dataset, options)


  return()
}

.fpDependencies <- c("effectSize", "effectSizeStandardError", "split")
.fpReady        <- function(options) {
  return(options[["effectSize"]] != "" && options[["effectSizeStandardError"]] != "")
}
.fpCheckDataset <- function(jaspResults, dataset, options) {

  # omit NAs
  dataset <- na.omit(dataset)

  # add a warning message
  if (!is.null(attr(dataset, "na.action")) && is.null(jaspResults[["missingDataInformation"]])) {
    missingDataInformation <- createJaspHtml(gettext("Missing Data Summary"))
    missingDataInformation$position <- 0.1
    missingDataInformation$dependOn(c(.fpDependencies, "estimatesMappingColor", "estimatesMappingShape", "studyLabel"))
    missingDataInformation$text <- gettextf("The dataset contains missing values: %1$i missing values were removed from the analysis.", length(attr(dataset, "na.action")))
    jaspResults[["missingDataInformation"]] <- missingDataInformation
  }

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations", "variance"),
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

  return(dataset)
}

.fpH1Fits                       <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["fitState"]]))
    return()

  # fit the models only if
  # - estimated funnel plot under H1 is requested
  # - meta-regression asymmetry test is requested
  # - trim and fill is requested

  if (!((options[["funnelUnderH1"]] && options[["funnelUnderH1Parameters"]] == "estimated") ||
         options[["funnelPlotAsymmetryTests"]] || options[["trimAndFill"]]))
    return()

  # store the fits into a state
  fitState <- createJaspState()
  fitState$dependOn(c(.fpDependencies, "method"))
  jaspResults[["fitState"]] <- fitState

  if (options[["split"]] == "") {

    fitState$object <- try(metafor::rma(
      yi     = dataset[[options[["effectSize"]]]],
      sei    = dataset[[options[["effectSizeStandardError"]]]],
      method = .maGetMethodOptions(options)
    ))

  } else {

    splitLevels <- unique(dataset[[options[["split"]]]])
    fits <- lapply(splitLevels, function(splitLevel) {
      try(metafor::rma(
        yi     = dataset[[options[["effectSize"]]]],
        sei    = dataset[[options[["effectSizeStandardError"]]]],
        subset = dataset[[options[["split"]]]] == splitLevel,
        method = .maGetMethodOptions(options)
      ))
    })
    names(fits) <- splitLevels
    fitState$object <- fits
  }

  return()
}
.fpH1TrimAndFillFits            <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["trimAndFillState"]]))
    return()

  if (!options[["trimAndFill"]])
    return()

  # store the fits into a state
  trimAndFillState <- createJaspState()
  trimAndFillState$dependOn(c(.fpDependencies, "method", "trimAndFillEstimator"))
  jaspResults[["trimAndFillState"]] <- trimAndFillState

  if (options[["split"]] == "") {

    if (jaspBase::isTryError(jaspResults[["fitState"]]$object)) {
      trimAndFillState$object <- jaspResults[["fitState"]]$object
    } else {
      trimAndFillState$object <- try(metafor::trimfill(
        jaspResults[["fitState"]]$object,
        estimator = options[["trimAndFillEstimator"]]
      ))
    }

  } else {

    splitLevels <- levels(dataset[[options[["split"]]]])
    fits <- lapply(splitLevels, function(splitLevel) {
      if (jaspBase::isTryError(jaspResults[["fitState"]]$object[[splitLevel]])) {
        return(jaspResults[["fitState"]]$object[[splitLevel]])
      } else {
        return(try(metafor::trimfill(
          jaspResults[["fitState"]]$object[[splitLevel]],
          estimator = options[["trimAndFillEstimator"]]
        )))
      }
    })

    names(fits) <- splitLevels
    trimAndFillState$object <- fits
  }

  return()
}
