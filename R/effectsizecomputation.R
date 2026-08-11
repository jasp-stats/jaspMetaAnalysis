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

# Effect-size computation entry point and computed-column export.
#
# Input mapping and reproducible R-code helpers are implemented in
# effectsizecomputation-mapping.R and effectsizecomputation-r-code.R.

EffectSizeComputation <- function(jaspResults, dataset, options, state = NULL) {

  # all input checking is done within the escalc function
  # - error messages are cleaned and forwarded to the user
  dataOutput  <- .escComputeEffectSizes(dataset, options)

  .escComputeSummaryTable(jaspResults, dataset, options, dataOutput)
  .escExportData(jaspResults, options, dataOutput)

  if (!is.null(options[["showMetaforRCode"]]) && options[["showMetaforRCode"]])
    .escShowMetaforRCode(jaspResults, dataset, options)

  return()
}

.escComputeEffectSizes  <- function(dataset, options) {

  # proceed with the escal in order
  dataOutput <- NULL
  errors     <- list()
  for (i in seq_along(options[["variables"]])) {

    # subset the relevant options (need to be passed separately as there are names overlap in "effectSize")
    effectSizeType <- options[["effectSizeType"]][[i]]
    variables      <- options[["variables"]][[i]]

    # skip on no input to the reported effect sizes (no error added)
    if (effectSizeType[["design"]] == "reportedEffectSizes" && !.escReportedEffectSizesReady(variables, all = FALSE))
      next

    # set escalc input (allows to check whether at least something was specified)
    tempDataOptions <- .escGetEscalcDataOptions(dataset, effectSizeType, variables)

    # skip on no input and don't set an error message
    if (length(tempDataOptions) == 0)
      next

    # set error message if reported effect sizes cannot be performed
    if (effectSizeType[["design"]] == "reportedEffectSizes" && !.escReportedEffectSizesReady(variables, all = TRUE)) {
      newDataOutput <- try(stop(gettextf("Cannot compute outcomes. Check that all of the required information is specified via the appropriate arguments (i.e. an Effect Size and either Standard Error, Sampling Variance, or 95%% Confidence Interval).")))
    } else {
    # set escalc input
      escalcInput <- c(
        tempDataOptions,
        .escGetEscalcAdjustFrequenciesOptions(effectSizeType, variables),
        .escGetEscalcVtypeOption(effectSizeType, variables),
        .escGetEscalcCorrectOption(effectSizeType, variables),
        measure     = if (effectSizeType[["design"]] == "reportedEffectSizes") "GEN" else effectSizeType[["effectSize"]],
        replace     = i == 1,
        add.measure = TRUE,
        data        = if (!is.null(dataOutput)) list(dataOutput)
      )

      newDataOutput <- try(do.call(metafor::escalc, escalcInput))
    }

    if (inherits(newDataOutput, "try-error")) {
      errors[[paste0("i",i)]] <- list(
        step  = i,
        error = .escCleanErrorMessage(attr(newDataOutput, "condition")$message, effectSizeType)
      )
    } else {

      # keep track of computation steps
      # (needs to be done manually as the same effect size can be specified multiple times...)
      newDataOutput$step <- NA
      newDataOutput[["step"]][!is.na(newDataOutput[["yi"]])] <- i

      if (is.null(dataOutput)) {
        dataOutput <- newDataOutput
      } else {
        dataOutput[is.na(dataOutput[["yi"]]),] <- newDataOutput[is.na(dataOutput[["yi"]]),]
      }
    }
  }

  # create an empty list if nothing was computed
  if (is.null(dataOutput))
    dataOutput <- list()

  attr(dataOutput, "errors") <- errors
  return(dataOutput)
}
.escComputeSummaryTable <- function(jaspResults, dataset, options, dataOutput) {

  # create summary table
  computeSummary <- createJaspTable(title = gettext("Summary"))
  computeSummary$dependOn(c("effectSizeType", "variables"))
  computeSummary$position <- 1

  jaspResults[["computeSummary"]] <- computeSummary

  computeSummary$addColumnInfo(name = "step",          title = gettext("Step"),           type = "integer")
  computeSummary$addColumnInfo(name = "effectSize",    title = gettext("Effect Size"),    type = "string")
  computeSummary$addColumnInfo(name = "computed",      title = gettext("Computed"),       type = "integer")
  computeSummary$addColumnInfo(name = "totalComputed", title = gettext("Total Computed"), type = "integer")

  # compute summary
  if (length(seq_along(options[["effectSizeType"]])) > 0) {

    computeSummaryData <- lapply(seq_along(options[["effectSizeType"]]), function(i) {
      list("step" = i, "effectSize" = options[["effectSizeType"]][[i]][["effectSize"]], "computed" = sum(dataOutput[["step"]] == i, na.rm = TRUE))
    })
    computeSummaryData <- do.call(rbind.data.frame, computeSummaryData)
    computeSummaryData$totalComputed <- cumsum(computeSummaryData$computed)

    # set the data
    computeSummary$setData(computeSummaryData)

    totalComputed <- sum(computeSummaryData[["computed"]])

    if (totalComputed == 0)
      computeSummary$addFootnote(gettext("Effect size calculation was not run yet."))
    else if (nrow(dataset) == totalComputed)
      computeSummary$addFootnote(gettext("Effect sizes were successfully computed and added to the dataset for each data entry."))
    else
      computeSummary$addFootnote(gettextf(
        "Effect sizes were successfully computed and added to the dataset for %1$i out of %2$i data entries.",
        totalComputed,
        nrow(dataset)))
  }

  computeErrors <- attr(dataOutput, "errors")
  for (i in seq_along(computeErrors)) {
    computeSummary$addFootnote(computeErrors[[i]]$error, symbol = gettextf("Error in Step %1$i:", computeErrors[[i]]$step))
  }

  return()
}
.escExportData          <- function(jaspResults, options, dataOutput) {

  if (length(dataOutput) == 0)
    return()

  # columns to add
  if (options[["computeSamplingVariance"]]) {
    columnOptions <- c("computedColumnsNamesEffectSize", "computedcolumnsNamesSamplingVariance", "computedColumnsNamesEffectSizeType")
  } else {
    columnOptions <- c("computedColumnsNamesEffectSize", "computedcolumnsNamesStandardError", "computedColumnsNamesEffectSizeType")
  }

  for (column in columnOptions) {

    columnName <- options[[column]]

    .metaValidateColumnName(columnName)

    jaspResults[[column]] <- createJaspColumn(columnName   = columnName, dependencies = c("effectSizeType", "variables", column))
    jaspResults[[column]]$setScale(switch(
      column,
      "computedColumnsNamesEffectSize"          = dataOutput[["yi"]],
      "computedcolumnsNamesStandardError"       = sqrt(dataOutput[["vi"]]),
      "computedcolumnsNamesSamplingVariance"    = dataOutput[["vi"]],
      "computedColumnsNamesEffectSizeType"      = dataOutput[["measure"]]
    ))

  }

  return()
}

# functions for transforming input into metafor::escalc settings
.escVariableInputs                    <- c(
  "group1OutcomePlus",
  "time1OutcomePlus",
  "outcomePlusPlus",
  "coefficientAlpha",
  "homozygousDominantAlleles",
  "group1OutcomeMinus",
  "time1OutcomeMinus",
  "outcomePlusMinus",
  "heterozygousAlleles",
  "group2OutcomePlus",
  "time2OutcomePlus",
  "outcomeMinusPlus",
  "homozygousRecessiveAlleles",
  "group2OutcomeMinus",
  "time2OutcomeMinus",
  "outcomeMinusMinus",
  "outcomePlusPlusAndPlusMinus",
  "outcomeMinusPlusAndMinusMinus",
  "eventsGroup1",
  "events",
  "nonEvents",
  "items",
  "predictors",
  "eventsGroup2",
  "personTimeGroup1",
  "personTime",
  "personTimeGroup2",
  "meanGroup1",
  "meanTime1",
  "meanGroup2",
  "meanTime2",
  "mean",
  "sdGroup1",
  "sdTime1",
  "sdGroup2",
  "sdTime2",
  "sd",
  "sampleSizeGroup1",
  "sampleSizeGroup2",
  "correlation",
  "proportionPlusPlus",
  "sampleSize",
  "cohensD",
  "rSquared",
  "tStatistic",
  "fStatistic",
  "semipartialCorrelation",
  "pValue",
  "effectSize",
  "standardError",
  "samplingVariance",
  "samplingVarianceTypeMixed",
  "smallSampleCorrection",
  "subset", "subsetLevel"
)
