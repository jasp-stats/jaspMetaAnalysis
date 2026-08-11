# Effect-size computation option mapping.
#
# Maps JASP effect-size inputs to metafor escalc data and adjustment options.

# Data arguments ----

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
      } else if (effectSize %in% c("SMD1", "SMD1H")) {
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
          ri = dataset[[variables[["semipartialCorrelation"]]]],
          pi = dataset[[variables[["pValue"]]]]
        )
      } else if (effectSize %in% c("SPCOR", "ZSPCOR")) {
        inputs <- list(
          ti  = dataset[[variables[["tStatistic"]]]],
          mi  = dataset[[variables[["predictors"]]]],
          ni  = dataset[[variables[["sampleSize"]]]],
          r2i = dataset[[variables[["rSquared"]]]],
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

# Corrections and adjustments ----

.escGetEscalcCorrectOption            <- function(effectSizeType, variables) {

  if (!.escSmallSampleCorrectionAvailable(effectSizeType))
    return(NULL)

  smallSampleCorrection <- variables[["smallSampleCorrection"]]
  if (is.null(smallSampleCorrection))
    smallSampleCorrection <- TRUE

  return(list(correct = smallSampleCorrection))
}

.escSmallSampleCorrectionAvailable    <- function(effectSizeType) {

  design      <- effectSizeType[["design"]]
  measurement <- effectSizeType[["measurement"]]
  effectSize  <- effectSizeType[["effectSize"]]

  return(
    (design == "independentGroups" && measurement == "quantitative" && effectSize %in% c("SMD", "SMDH", "SMD1", "SMD1H")) ||
      (design == "repeatedMeasures" && measurement == "quantitative" && effectSize %in% c("SMCC", "SMCR", "SMCRH", "SMCRP", "SMCRPH"))
  )
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
      (design == "variableAssociation" && measurement == "binary" && effectSize %in% c("PHI", "ZPHI")) ||
      (design == "variableAssociation" && measurement == "mixed" && effectSize %in% c("RPB", "ZPB")) ||
      (design == "other" && measurement == "modelFit") &&
      variables[["samplingVarianceType"]] != "mixed") {
    return(list(vtype = variables[["samplingVarianceType"]]))
  } else {
    return(NULL)
  }
}

# JASP option labels ----

# Parallel to .escMapEscalcInput2VariableInputs(); both mappings must expose
# identical metafor::escalc argument keys for each effect-size type.
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
      } else if (effectSize %in% c("SMD1", "SMD1H")) {
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

# Reported-effect helpers and error messages ----

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
