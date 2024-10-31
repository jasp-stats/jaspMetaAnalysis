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

EffectSizeComputation <- function(jaspResults, dataset, options, state = NULL) {

  # all input checking is done within the escalc function
  # - error messages are cleaned and forwarded to the user
  dataOutput  <- .escComputeEffectSizes(dataset, options)
  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  .escComputeSummaryTable(jaspResults, dataset, options, dataOutput)
  .escExportData(jaspResults, options, dataOutput)

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
      newDataOutput <- try(stop(gettext("Cannot compute outcomes. Chech that all of the required information is specified via the appropriate arguments (i.e. an Effect Size and either Standard Error, Sampling Variance, or 95% Confidence Interval).")))
    } else {
    # set escalc input
      escalcInput <- c(
        tempDataOptions,
        .escGetEscalcAdjustFrequenciesOptions(effectSizeType, variables),
        .escGetEscalcVtypeOption(effectSizeType, variables),
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

    if (nrow(dataset) == sum(computeSummaryData[["computed"]]))
      computeSummary$addFootnote(gettext("Effect sizes were successfully computed for each data entry."))
    else
      computeSummary$addFootnote(gettextf(
        "Effect sizes were successfully computed for %1$i out of %2$i data entries.",
        sum(computeSummaryData[["computed"]]),
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
    columnOptions <- c("computedcolumnsNamesEffectSize", "computedcolumnsNamesSamplingVariance", "computedcolumnsNamesEffectSizeType")
  } else {
    columnOptions <- c("computedcolumnsNamesEffectSize", "computedcolumnsNamesStandardError", "computedcolumnsNamesEffectSizeType")
  }

  for (column in columnOptions) {

    columnName <- options[[column]]

    if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
      .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))

    jaspResults[[column]] <- createJaspColumn(columnName   = columnName, dependencies = c("effectSizeType", "variables", column))
    jaspResults[[column]]$setScale(switch(
      column,
      "computedcolumnsNamesEffectSize"          = dataOutput[["yi"]],
      "computedcolumnsNamesStandardError"       = sqrt(dataOutput[["vi"]]),
      "computedcolumnsNamesSamplingVariance"    = dataOutput[["vi"]],
      "computedcolumnsNamesEffectSizeType"      = dataOutput[["measure"]]
    ))

  }

  return()
}

# functions for transforming input into metafor::escalc settings
.escGetEscalcDataOptions              <- function(dataset, effectSizeType, variables) {

  design      <- effectSizeType[["design"]]
  measurement <- effectSizeType[["measurement"]]
  effectSize  <- effectSizeType[["effectSize"]]

  if (design == "independentGroups") {
    if (measurement == "quantitative") {
      if (effectSize == "SMD") {
        inputs <- list(
          m1i  = dataset[[variables[["meanGroup1"]]]],
          m2i  = dataset[[variables[["meanGroup2"]]]],
          sd1i = dataset[[variables[["sdGroup1"]]]],
          sd2i = dataset[[variables[["sdGroup2"]]]],
          n1i  = dataset[[variables[["sampleSizeGroup1"]]]],
          n2i  = dataset[[variables[["sampleSizeGroup2"]]]],
          ti   = dataset[[variables[["tStatistic"]]]],
          pi   = dataset[[variables[["pValue"]]]],
          di   = dataset[[variables[["cohensD"]]]]
        )
      } else if (effectSize %in% c("SMD1", "SMDH1")) {
        inputs <- list(
          m1i  = dataset[[variables[["meanGroup1"]]]],
          m2i  = dataset[[variables[["meanGroup2"]]]],
          sd2i = dataset[[variables[["sdGroup2"]]]],
          n1i  = dataset[[variables[["sampleSizeGroup1"]]]],
          n2i  = dataset[[variables[["sampleSizeGroup2"]]]]
        )
      } else if (effectSize %in% c("CVR", "VR")) {
        inputs <- list(
          sd1i = dataset[[variables[["sdGroup1"]]]],
          sd2i = dataset[[variables[["sdGroup2"]]]],
          n1i  = dataset[[variables[["sampleSizeGroup1"]]]],
          n2i  = dataset[[variables[["sampleSizeGroup2"]]]]
        )
      } else {
        inputs <- list(
          m1i  = dataset[[variables[["meanGroup1"]]]],
          m2i  = dataset[[variables[["meanGroup2"]]]],
          sd1i = dataset[[variables[["sdGroup1"]]]],
          sd2i = dataset[[variables[["sdGroup2"]]]],
          n1i  = dataset[[variables[["sampleSizeGroup1"]]]],
          n2i  = dataset[[variables[["sampleSizeGroup2"]]]]
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        ai  = dataset[[variables[["group1OutcomePlus"]]]],
        bi  = dataset[[variables[["group1OutcomeMinus"]]]],
        ci  = dataset[[variables[["group2OutcomePlus"]]]],
        di  = dataset[[variables[["group2OutcomeMinus"]]]],
        n1i = dataset[[variables[["sampleSizeGroup1"]]]],
        n2i = dataset[[variables[["sampleSizeGroup2"]]]]
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        t1i = dataset[[variables[["personTimeGroup1"]]]],
        t2i = dataset[[variables[["personTimeGroup2"]]]],
        x1i = dataset[[variables[["eventsGroup1"]]]],
        x2i = dataset[[variables[["eventsGroup2"]]]]
      )
    } else if (measurement == "mixed") {
      if (effectSize %in% c("D2ORN", "D2ORL")) {
        inputs <- list(
          m1i  = dataset[[variables[["meanGroup1"]]]],
          m2i  = dataset[[variables[["meanGroup2"]]]],
          sd1i = dataset[[variables[["sdGroup1"]]]],
          sd2i = dataset[[variables[["sdGroup2"]]]],
          n1i  = dataset[[variables[["sampleSizeGroup1"]]]],
          n2i  = dataset[[variables[["sampleSizeGroup2"]]]],
          ti   = dataset[[variables[["tStatistic"]]]],
          pi   = dataset[[variables[["pValue"]]]],
          di   = dataset[[variables[["cohensD"]]]]
        )
      } else if (effectSize %in% c("PBIT", "OR2DN", "OR2DL")) {
        inputs <- list(
          ai  = dataset[[variables[["group1OutcomePlus"]]]],
          bi  = dataset[[variables[["group1OutcomeMinus"]]]],
          ci  = dataset[[variables[["group2OutcomePlus"]]]],
          di  = dataset[[variables[["group2OutcomeMinus"]]]],
          n1i = dataset[[variables[["sampleSizeGroup1"]]]],
          n2i = dataset[[variables[["sampleSizeGroup2"]]]]
        )
      }
    }
  } else if (design == "variableAssociation") {
    if (measurement == "quantitative") {
      inputs <- list(
        ri = dataset[[variables[["correlation"]]]],
        ni = dataset[[variables[["sampleSize"]]]],
        ti = dataset[[variables[["tStatistic"]]]],
        pi = dataset[[variables[["pValue"]]]]
      )
    } else if (measurement == "binary") {
      if (effectSize %in% c("OR", "YUQ", "YUY", "RTET", "ZTET")) {
        inputs <- list(
          ai  = dataset[[variables[["outcomePlusPlus"]]]],
          bi  = dataset[[variables[["outcomePlusMinus"]]]],
          ci  = dataset[[variables[["outcomeMinusPlus"]]]],
          di  = dataset[[variables[["outcomeMinusMinus"]]]],
          n1i = dataset[[variables[["outcomePlusPlusAndPlusMinus"]]]],
          n2i = dataset[[variables[["outcomeMinusPlusAndMinusMinus"]]]]
        )
      } else if (effectSize %in% c("PHI", "ZPHI")) {
        inputs <- list(
          ai    = dataset[[variables[["outcomePlusPlus"]]]],
          bi    = dataset[[variables[["outcomePlusMinus"]]]],
          ci    = dataset[[variables[["outcomeMinusPlus"]]]],
          di    = dataset[[variables[["outcomeMinusMinus"]]]],
          n1i   = dataset[[variables[["outcomePlusPlusAndPlusMinus"]]]],
          n2i   = dataset[[variables[["outcomeMinusPlusAndMinusMinus"]]]]
        )
        if (variables[["samplingVarianceTypeMixed"]] != "")
          inputs$vtype <- dataset[[variables[["samplingVarianceTypeMixed"]]]]
      }
    } else if (measurement == "mixed") {
      if (effectSize %in% c("RBIS", "ZBIS")) {
        inputs <- list(
          m1i  = dataset[[variables[["meanGroup1"]]]],
          m2i  = dataset[[variables[["meanGroup2"]]]],
          sd1i = dataset[[variables[["sdGroup1"]]]],
          sd2i = dataset[[variables[["sdGroup2"]]]],
          n1i  = dataset[[variables[["sampleSizeGroup1"]]]],
          n2i  = dataset[[variables[["sampleSizeGroup2"]]]],
          ti   = dataset[[variables[["tStatistic"]]]],
          pi   = dataset[[variables[["pValue"]]]],
          di   = dataset[[variables[["cohensD"]]]]
        )
      } else if (effectSize %in% c("RPB", "ZPB")) {
        inputs <- list(
          m1i   = dataset[[variables[["meanGroup1"]]]],
          m2i   = dataset[[variables[["meanGroup2"]]]],
          sd1i  = dataset[[variables[["sdGroup1"]]]],
          sd2i  = dataset[[variables[["sdGroup2"]]]],
          n1i   = dataset[[variables[["sampleSizeGroup1"]]]],
          n2i   = dataset[[variables[["sampleSizeGroup2"]]]],
          ti    = dataset[[variables[["tStatistic"]]]],
          pi    = dataset[[variables[["pValue"]]]],
          di    = dataset[[variables[["cohensD"]]]]
        )
        if (variables[["samplingVarianceTypeMixed"]] != "")
          inputs$vtype <- dataset[[variables[["samplingVarianceTypeMixed"]]]]
      }
    }
  } else if (design == "singleGroup") {
    if (measurement == "quantitative") {
      if (effectSize %in% c("MN", "SMN", "MNLN", "CVLN")) {
        inputs <- list(
          mi  = dataset[[variables[["mean"]]]],
          sdi = dataset[[variables[["sd"]]]],
          ni  = dataset[[variables[["sampleSize"]]]]
        )
      } else if (effectSize == "SDLN") {
        inputs <- list(
          sdi = dataset[[variables[["sd"]]]],
          ni  = dataset[[variables[["sampleSize"]]]]
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        xi  = dataset[[variables[["events"]]]],
        mi  = dataset[[variables[["nonEvents"]]]],
        ni  = dataset[[variables[["sampleSize"]]]]
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        xi = dataset[[variables[["events"]]]],
        ti = dataset[[variables[["personTime"]]]],
        ni = dataset[[variables[["sampleSize"]]]]
      )
    }
  } else if (design == "repeatedMeasures") {
    if (measurement == "quantitative") {
      if (effectSize %in% c("MC", "SMCR", "SMCRH", "SMCRP", "SMCRPH", "ROMC")) {
        inputs <- list(
          m1i  = dataset[[variables[["meanTime1"]]]],
          m2i  = dataset[[variables[["meanTime2"]]]],
          sd1i = dataset[[variables[["sdTime1"]]]],
          sd2i = dataset[[variables[["sdTime2"]]]],
          ni   = dataset[[variables[["sampleSize"]]]],
          ri   = dataset[[variables[["correlation"]]]]
        )
      } else if (effectSize == "SMCC") {
        inputs <- list(
          m1i  = dataset[[variables[["meanTime1"]]]],
          m2i  = dataset[[variables[["meanTime2"]]]],
          sd1i = dataset[[variables[["sdTime1"]]]],
          sd2i = dataset[[variables[["sdTime2"]]]],
          ni   = dataset[[variables[["sampleSize"]]]],
          ri   = dataset[[variables[["correlation"]]]],
          ti   = dataset[[variables[["tStatistic"]]]],
          pi   = dataset[[variables[["pValue"]]]],
          di   = dataset[[variables[["cohensD"]]]]
        )
      } else if (effectSize %in% c("CVRC", "VRC")) {
        inputs <- list(
          sd1i = dataset[[variables[["sdTime1"]]]],
          sd2i = dataset[[variables[["sdTime2"]]]],
          ni   = dataset[[variables[["sampleSize"]]]],
          ri   = dataset[[variables[["correlation"]]]]
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        ai = dataset[[variables[["outcomePlusPlus"]]]],
        bi = dataset[[variables[["outcomePlusMinus"]]]],
        ci = dataset[[variables[["outcomeMinusPlus"]]]],
        di = dataset[[variables[["outcomeMinusMinus"]]]]
      )
    } else if (measurement == "binaryMarginal") {
      inputs <- list(
        ai = dataset[[variables[["time1OutcomePlus"]]]],
        bi = dataset[[variables[["time1OutcomeMinus"]]]],
        ci = dataset[[variables[["time2OutcomePlus"]]]],
        di = dataset[[variables[["time2OutcomeMinus"]]]],
        ri = dataset[[variables[["correlation"]]]],
        pi = dataset[[variables[["proportionPlusPlus"]]]]
      )
    }
  } else if (design == "other") {
    if (measurement == "reliability") {
      inputs <- list(
        ai = dataset[[variables[["coefficientAlpha"]]]],
        mi = dataset[[variables[["items"]]]],
        ni = dataset[[variables[["sampleSize"]]]]
      )
    } else if (measurement == "partialCorrelation") {
      if (effectSize %in% c("PCOR", "ZPCOR")) {
        inputs <- list(
          ti = dataset[[variables[["tStatistic"]]]],
          mi = dataset[[variables[["predictors"]]]],
          ni = dataset[[variables[["sampleSize"]]]],
          ti = dataset[[variables[["tStatistic"]]]],
          ri = dataset[[variables[["semipartialCorrelation"]]]],
          pi = dataset[[variables[["pValue"]]]]
        )
      } else if (effectSize %in% c("SPCOR", "ZSPCOR")) {
        inputs <- list(
          ti  = dataset[[variables[["tStatistic"]]]],
          mi  = dataset[[variables[["predictors"]]]],
          ni  = dataset[[variables[["sampleSize"]]]],
          r2i = dataset[[variables[["rSquared"]]]],
          ti  = dataset[[variables[["tStatistic"]]]],
          ri  = dataset[[variables[["semipartialCorrelation"]]]],
          pi  = dataset[[variables[["pValue"]]]]
        )
      }
    } else if (measurement == "modelFit") {
      inputs <- list(
        mi  = dataset[[variables[["predictors"]]]],
        ni  = dataset[[variables[["sampleSize"]]]],
        r2i = dataset[[variables[["rSquared"]]]],
        fi  = dataset[[variables[["fStatistic"]]]],
        pi  = dataset[[variables[["pValue"]]]]
      )
    } else if (measurement == "heterozygosity") {
      inputs <- list(
        ai = dataset[[variables[["homozygousDominantAlleles"]]]],
        bi = dataset[[variables[["heterozygousAlleles"]]]],
        ci = dataset[[variables[["homozygousRecessiveAlleles"]]]]
      )
    }
  } else if (design == "reportedEffectSizes") {
    inputs <- list(
      yi  = dataset[[variables[["effectSize"]]]],
      sei = dataset[[variables[["standardError"]]]],
      vi  = dataset[[variables[["samplingVariance"]]]],
      lci = if (length(variables[["confidenceInterval"]]) != 0) dataset[[variables[["confidenceInterval"]][[1]][1]]],
      uci = if (length(variables[["confidenceInterval"]]) != 0) dataset[[variables[["confidenceInterval"]][[1]][2]]]
    )
    inputs <- .escReportedEffectSizesInput(inputs)
  }

  if (variables[["subset"]] != "") {
    # subset should not be added to the dataset - escalc returns only the subset rows
    # we need the whole data set to facilitate merging across the steps
    # therefore, we set all non-subset columns to NAs
    for (i in seq_along(inputs)) {
      if (length(inputs[[i]]) != 0)
        inputs[[i]][dataset[[variables[["subset"]]]] != variables[["subsetLevel"]]] <- NA
    }
  }

  inputs <- inputs[!sapply(inputs, is.null)]

  return(inputs)
}
.escGetEscalcAdjustFrequenciesOptions <- function(effectSizeType, variables) {

  design      <- effectSizeType[["design"]]
  measurement <- effectSizeType[["measurement"]]
  effectSize  <- effectSizeType[["effectSize"]]

  # Conditions for when add is appropriate
  if ((design == "independentGroups" && measurement == "binary") ||
      (design == "independentGroups" && measurement == "countsPerTime") ||
      (design == "independentGroups" && measurement == "mixed" && effectSize %in% c("PBIT", "OR2DN", "OR2DL")) ||
      (design == "variableAssociation" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "countsPerTime")) {
    return(list(
      add    = variables[["add"]],
      to     = switch(
        variables[["to"]],
        "all"       = "all",
        "onlyZero"  = "only0",
        "ifAnyZero" = "if0all",
        "none"      = "none"
      ),
      drop00 = switch(
        variables[["dropStudiesWithNoCasesOrEvents"]],
        "yes" = TRUE,
        "no"  = FALSE
      )
    ))
  } else {
    return(NULL)
  }
}
.escGetEscalcVtypeOption              <- function(effectSizeType, variables) {

  design      <- effectSizeType[["design"]]
  measurement <- effectSizeType[["measurement"]]
  effectSize  <- effectSizeType[["effectSize"]]

  # Conditions for when vtype is appropriate
  if ((design == "independentGroups"   && measurement == "quantitative" && effectSize %in% c("MD", "SMD", "SMD1", "ROM")) ||
      (design == "variableAssociation" && measurement == "quantitative") ||
      (design == "variableAssociation" && measurement == "binary" && effectSize %in% c("PHI", "ZHI")) ||
      (design == "variableAssociation" && measurement == "mixed" && effectSize %in% c("RPB", "ZPB")) ||
      (design == "other" && measurement == "modelFit") &&
      variables[["samplingVarianceType"]] != "mixed") {
    return(list(vtype = variables[["samplingVarianceType"]]))
  } else {
    return(NULL)
  }
}
.escMapEscalcInput2Options            <- function(effectSizeType) {

  design      <- effectSizeType[["design"]]
  measurement <- effectSizeType[["measurement"]]
  effectSize  <- effectSizeType[["effectSize"]]

  if (design == "independentGroups") {
    if (measurement == "quantitative") {
      if (effectSize == "SMD") {
        inputs <- list(
          m1i  = "Mean Group 1",
          m2i  = "Mean Group 2",
          sd1i = "SD Group 1",
          sd2i = "SD Group 2",
          n1i  = "Sample Size Group 1",
          n2i  = "Sample Size Group 2",
          ti   = "T-Statistic",
          pi   = "P-Value",
          di   = "Cohen's d"
        )
      } else if (effectSize %in% c("SMD1", "SMDH1")) {
        inputs <- list(
          m1i  = "Mean Group 1",
          m2i  = "Mean Group 2",
          sd2i = "SD Group 2",
          n1i  = "Sample Size Group 1",
          n2i  = "Sample Size Group 2"
        )
      } else if (effectSize %in% c("CVR", "VR")) {
        inputs <- list(
          sd1i = "SD Group 1",
          sd2i = "SD Group 2",
          n1i  = "Sample Size Group 1",
          n2i  = "Sample Size Group 2"
        )
      } else {
        inputs <- list(
          m1i  = "Mean Group 1",
          m2i  = "Mean Group 2",
          sd1i = "SD Group 1",
          sd2i = "SD Group 2",
          n1i  = "Sample Size Group 1",
          n2i  = "Sample Size Group 2"
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        ai  = "Group 1/Outcome +",
        bi  = "Group 1/Outcome -",
        ci  = "Group 2/Outcome +",
        di  = "Group 2/Outcome -",
        n1i = "Sample Size Group 1",
        n2i = "Sample Size Group 2"
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        t1i = "Person-Time Group 1",
        t2i = "Person-Time Group 2",
        x1i = "Events Group 1",
        x2i = "Events Group 2"
      )
    } else if (measurement == "mixed") {
      if (effectSize %in% c("D2ORN", "D2ORL")) {
        inputs <- list(
          m1i  = "Mean Group 1",
          m2i  = "Mean Group 2",
          sd1i = "SD Group 1",
          sd2i = "SD Group 2",
          n1i  = "Sample Size Group 1",
          n2i  = "Sample Size Group 2",
          ti   = "T-Statistic",
          pi   = "P-Value",
          di   = "Cohen's d"
        )
      } else if (effectSize %in% c("PBIT", "OR2DN", "OR2DL")) {
        inputs <- list(
          ai  = "Group 1/Outcome +",
          bi  = "Group 1/Outcome -",
          ci  = "Group 2/Outcome +",
          di  = "Group 2/Outcome -",
          n1i = "Sample Size Group 1",
          n2i = "Sample Size Group 2"
        )
      }
    }
  } else if (design == "variableAssociation") {
    if (measurement == "quantitative") {
      inputs <- list(
        ri = "Correlation",
        ni = "Sample Size",
        ti = "T-Statistic",
        pi = "P-Value"
      )
    } else if (measurement == "binary") {
      if (effectSize %in% c("OR", "YUQ", "YUY", "RTET", "ZTET")) {
        inputs <- list(
          ai  = "Outcome +/+",
          bi  = "Outcome +/-",
          ci  = "Outcome -/+",
          di  = "Outcome -/-",
          n1i = "Outcome +/+ and +/-",
          n2i = "Outcome -/+ and -/-"
        )
      } else if (effectSize %in% c("PHI", "ZPHI")) {
        inputs <- list(
          ai    = "Outcome +/+",
          bi    = "Outcome +/-",
          ci    = "Outcome -/+",
          di    = "Outcome -/-",
          n1i   = "Outcome +/+ and +/-",
          n2i   = "Outcome -/+ and -/-",
          vtype = "Sampling Variance Type Mixed"
        )
      }
    } else if (measurement == "mixed") {
      if (effectSize %in% c("RBIS", "ZBIS")) {
        inputs <- list(
          m1i  = "Mean Group 1",
          m2i  = "Mean Group 2",
          sd1i = "SD Group 1",
          sd2i = "SD Group 2",
          n1i  = "Sample Size Group 1",
          n2i  = "Sample Size Group 2",
          ti   = "T-Statistic",
          pi   = "P-Value",
          di   = "Cohen's d"
        )
      } else if (effectSize %in% c("RPB", "ZPB")) {
        inputs <- list(
          m1i   = "Mean Group 1",
          m2i   = "Mean Group 2",
          sd1i  = "SD Group 1",
          sd2i  = "SD Group 2",
          n1i   = "Sample Size Group 1",
          n2i   = "Sample Size Group 2",
          ti    = "T-Statistic",
          pi    = "P-Value",
          di    = "Cohen's d",
          vtype = "Sampling Variance Type Mixed"
        )
      }
    }
  } else if (design == "singleGroup") {
    if (measurement == "quantitative") {
      if (effectSize %in% c("MN", "SMN", "MNLN", "CVLN")) {
        inputs <- list(
          mi  = "Mean",
          sdi = "SD",
          ni  = "Sample Size"
        )
      } else if (effectSize == "SDLN") {
        inputs <- list(
          sdi = "SD",
          ni  = "Sample Size"
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        xi  = "Events",
        mi  = "Non-Events",
        ni  = "Sample Size"
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        xi = "Events",
        ti = "Person-Time",
        ni = "Sample Size"
      )
    }
  } else if (design == "repeatedMeasures") {
    if (measurement == "quantitative") {
      if (effectSize %in% c("MC", "SMCR", "SMCRH", "SMCRP", "SMCRPH", "ROMC")) {
        inputs <- list(
          m1i  = "Mean Time 1 (or Group 1)",
          m2i  = "Mean Time 2 (or Group 2)",
          sd1i = "SD Time 1 (or Group 1)",
          sd2i = "SD Time 2 (or Group 2)",
          ni   = "Sample Size",
          ri   = "Correlation"
        )
      } else if (effectSize == "SMCC") {
        inputs <- list(
          m1i  = "Mean Time 1 (or Group 1)",
          m2i  = "Mean Time 2 (or Group 2)",
          sd1i = "SD Time 1 (or Group 1)",
          sd2i = "SD Time 2 (or Group 2)",
          ni   = "Sample Size",
          ri   = "Correlation",
          ti   = "T-Statistic",
          pi   = "P-Value",
          di   = "Cohen's d"
        )
      } else if (effectSize %in% c("CVRC", "VRC")) {
        inputs <- list(
          sd1i = "SD Time 1 (or Group 1)",
          sd2i = "SD Time 2 (or Group 2)",
          ni   = "Sample Size",
          ri   = "Correlation"
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        ai = "Outcome +/+",
        bi = "Outcome +/-",
        ci = "Outcome -/+",
        di = "Outcome -/-"
      )
    } else if (measurement == "binaryMarginal") {
      inputs <- list(
        ai = "Time 1/Outcome +",
        bi = "Time 1/Outcome -",
        ci = "Time 2/Outcome +",
        di = "Time 2/Outcome -",
        ri = "Correlation",
        pi = "Proportion +/+"
      )
    }
  } else if (design == "other") {
    if (measurement == "reliability") {
      inputs <- list(
        ai = "Cronbach's alpha",
        mi = "Items",
        ni = "Sample Size"
      )
    } else if (measurement == "partialCorrelation") {
      if (effectSize %in% c("PCOR", "ZPCOR")) {
        inputs <- list(
          ti = "T-Statistic",
          mi = "Predictors",
          ni = "Sample Size",
          ti = "T-Statistic",
          ri = "(Semi)Partial Correlation",
          pi = "P-Value"
        )
      } else if (effectSize %in% c("SPCOR", "ZSPCOR")) {
        inputs <- list(
          ti  = "T-Statistic",
          mi  = "Predictors",
          ni  = "Sample Size",
          r2i = "R-Squared",
          ti  = "T-Statistic",
          ri  = "(Semi)Partial Correlation",
          pi  = "P-Value"
        )
      }
    } else if (measurement == "modelFit") {
      inputs <- list(
        mi  = "Predictors",
        ni  = "Sample Size",
        r2i = "R-Squared",
        fi  = "F-Statistic",
        pi  = "P-Value"
      )
    } else if (measurement == "heterozygosity") {
      inputs <- list(
        ai = "Homozygous Dominant Alleles",
        bi = "Heterozygous Alleles",
        ci = "Homozygous Recessive Alleles"
      )
    }
  } else if (design == "reportedEffectSizes") {
    inputs <- list(
      yi  = "Effect Size",
      sei = "Standard Error",
      vi  = "Sampling Variance"
    )
  }

  return(inputs)
}
.escCleanErrorMessage                 <- function(errorMessage, effectSizeType) {

  # remove new lines
  errorMessage <- gsub("\\n ", "", errorMessage)

  if (grepl("via the appropriate arguments", errorMessage)) {

    # split the message at 'via the appropriate arguments'
    errorSplit        <- regexpr("via the appropriate arguments", errorMessage)
    errorMessageStart <- substr(errorMessage, 1, errorSplit + attr(errorSplit, "match.length") - 1)
    errorMessageEnd   <- substr(errorMessage, errorSplit + attr(errorSplit, "match.length"), nchar(errorMessage))

    inputMapping <- .escMapEscalcInput2Options(effectSizeType)
    for (input in names(inputMapping)) {
      errorMessageEnd <- gsub(input, inputMapping[[input]], errorMessageEnd)
    }

    # re-assemble the message
    errorMessage <- paste(errorMessageStart, errorMessageEnd, sep = "")

  } else if (grepl("'vtype'", errorMessage)) {
    errorMessage <- gsub("'vtype'", "'Sampling variance type'", errorMessage)
  }

  return(errorMessage)
}
.escReportedEffectSizesInput          <- function(inputs) {

  inputs <- inputs[!sapply(inputs, is.null)]
  inputs <- do.call(cbind.data.frame, inputs)

  if (is.null(inputs$sei))
    inputs$sei <- NA
  if (is.null(inputs$vi))
    inputs$vi <- NA
  if (is.null(inputs$uci))
    inputs$uci <- NA
  if (is.null(inputs$lci))
    inputs$lci <- NA

  # add standard error when missing and CI is available
  if (length((inputs$uci[is.na(inputs$sei)] - inputs$lci[is.na(inputs$sei)]) ) != 0)
    inputs$sei[is.na(inputs$sei)] <- (inputs$uci[is.na(inputs$sei)] - inputs$lci[is.na(inputs$sei)]) / (2 * stats::qnorm(0.975))

  # add variance when missing and standard error is available
  if (length(inputs$sei[is.na(inputs$vi)]) != 0)
    inputs$vi[is.na(inputs$vi)] <- inputs$sei[is.na(inputs$vi)]^2

  # remove sei and cis
  inputs$sei <- NULL
  inputs$uci <- NULL
  inputs$lci <- NULL

  return(inputs)
}
.escReportedEffectSizesReady          <- function(variables, all = TRUE){

  varianceMeasureReady <- !(length(variables[["confidenceInterval"]]) == 0 && variables[["standardError"]] == "" && variables[["samplingVariance"]] == "")
  effectSizeReady      <- variables[["effectSize"]] != ""

  if (all) {
    return(effectSizeReady && varianceMeasureReady)
  } else {
    return((effectSizeReady + varianceMeasureReady) >= 1)
  }
}
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
  "subset", "subsetLevel"
)
