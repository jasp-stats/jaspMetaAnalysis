
EffectSizeComputation <- function(jaspResults, dataset, options, state = NULL) {

#  saveRDS(options, file = "C:/JASP/options.RDS")
#  saveRDS(dataset, file = "C:/JASP/dataset.RDS")



  dataset     <- .escReadDataset(dataset, options)
  return(NULL)
  dataOutput  <- .escComputeEffectSizes(dataset, options)

  .escComputeSummaryTable(jaspResults, dataset, options, dataOutput)
  .escExportData(jaspResults, options, dataOutput)

  return()
}

.escCheckReady          <- function(options, type = "readData") {


  return(TRUE)
}
.escReadDataset         <- function(dataset, options) {

  if (!is.null(dataset)) {
    return(dataset)
  } else {

    # collect all selected variables
    selectedVariables <- lapply(seq_along(options[["variables"]]), function(i) {

      # all possible variables
      selectedVariables <- options[["variables"]][[i]]

      # remove auxiliary objects
      selectedVariables <- selectedVariables[!names(selectedVariables) %in% c("value")]

      # remove Frequency/event cell adjustment
      selectedVariables <- selectedVariables[!names(selectedVariables) %in% c("add", "to", "dropStudiesWithNoCasesOrEvents", "samplingVarianceType")]

      # remove empty variables
      selectedVariables <- selectedVariables[!(selectedVariables == "" | lengths(selectedVariables) == 0)]

      # unlist
      selectedVariables <- unname(unlist(selectedVariables))

      return(selectedVariables)
    })
    selectedVariables <- do.call(c, selectedVariables)
    selectedVariables <- unique(selectedVariables)

    return(.readDataSetToEnd(columns.as.numeric = selectedVariables))
  }
}
.escComputeEffectSizes  <- function(dataset, options) {

  # proceed with the escal in order
  dataOutput <- NULL
  errors     <- list()
  for (i in seq_along(options[["variables"]])) {

    # subset the relevant options (need to be passed separately as there are names overlap in "effectSize")
    effectSizeType <- options[["effectSizeType"]][[i]]
    variables      <- options[["variables"]][[i]]

    # set escalc input
    escalcInput <- c(
      .escGetEscalcDataOptions(dataset, effectSizeType, variables),
      .escGetEscalcAdjustFrequenciesOptions(effectSizeType, variables),
      .escGetEscalcVtypeOption(effectSizeType, variables),
      measure     = if (effectSizeType[["effectSize"]] == "reportedEffectSizes") "GEN" else effectSizeType[["effectSize"]],
      replace     = i == 1,
      add.measure = TRUE,
      data        = if (!is.null(dataOutput)) list(dataOutput)
    )

    newDataOutput <- try(do.call(metafor::escalc, escalcInput))

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

    if (nrow(dataset == sum(computeSummaryData[["computed"]])))
      addFootnote(computeSummary, gettext("Note: Effect sizes were successfully computed for each data entry."))
    else
      addFootnote(computeSummary, gettextf(
        "Note: Effect sizes were successfully computed for %1$i out of %2$i data entries.",
        sum(computeSummaryData[["computed"]]),
        nrow(dataset)))
  }

  computeErrors <- attr(dataOutput, "errors")
  for (i in seq_along(computeErrors)) {
    addFootnote(computeSummary, with(computeErrors[[i]], gettextf("Error in step %1$i: %2$s", step, error)))
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
      if (effectSize %in% c("OR", "YUQ", "YUY", "RTET")) {
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
          n2i   = dataset[[variables[["outcomeMinusPlusAndMinusMinus"]]]],
          vtype = if(length(variables[["samplingVarianceTypeMixed"]]) != 0) dataset[[variables[["samplingVarianceTypeMixed"]]]] else NULL
        )
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
          di    = dataset[[variables[["cohensD"]]]],
          vtype = if(length(variables[["samplingVarianceTypeMixed"]]) != 0) dataset[[variables[["samplingVarianceTypeMixed"]]]] else NULL
        )
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
        input <- list(
          m1i  = dataset[[variables[["meanTime1"]]]],
          m2i  = dataset[[variables[["meanTime2"]]]],
          sd1i = dataset[[variables[["sdTime1"]]]],
          sd2i = dataset[[variables[["sdTime2"]]]],
          ni   = dataset[[variables[["sampleSize"]]]],
          ri   = dataset[[variables[["correlation"]]]]
        )
      } else if (effectSize == "SMCC") {
        input <- list(
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
        input <- list(
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
        ai = dataset[[variables[["cronbachsAlpha"]]]],
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
      lci = dataset[[variables[["confidenceInterval"]][[1]][1]]],
      uci = dataset[[variables[["confidenceInterval"]][[1]][2]]]
    )
    inputs <- .escReportedEffectSizesInput(inputs)
  }

  if (length(variables[["subset"]]) == 1)
    inputs[["subset"]] <- dataset[[variables[["subset"]]]]

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
      (design == "variableAssociation" && measurement == "binary") ||
      (design == "variableAssociation" && measurement == "mixed") ||
      (design == "other" && measurement == "modelFit")) {
    return(list(
      vtype = variables[["samplingVarianceType"]]
    ))
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
      if (effectSize %in% c("OR", "YUQ", "YUY", "RTET")) {
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
          vtype = if(length(variables[["samplingVarianceTypeMixed"]]) != 0) "samplingVarianceTypeMixed" else NULL
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
          vtype = if(length(variables[["samplingVarianceTypeMixed"]]) != 0) "samplingVarianceTypeMixed" else NULL
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

  errorMessage <- attr(newDataOutput, "condition")$message

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

  # add standard error when missing and CI is available
  inputs$sei[[is.na(inputs$sei)]] <- (inputs$uci[[is.na(inputs$sei)]] - inputs$lci[[is.na(inputs$sei)]]) / (2 * stats::qnorm(0.975))

  # add variance when missing and standard error is available
  inputs$vi[[is.na(input$vi)]] <- inputs$sei[[is.na(inputs$vi)]]^2

  # remove sei and cis
  inputs$sei <- NULL
  inputs$uci <- NULL
  inputs$lci <- NULL

  return(inputs)
}
