
EffectSizeComputation <- function(jaspResults, dataset, options, state = NULL) {


  return()
}

# functions for transforming input into metafor::escalc settings
getEscalcDataOptions              <- function(options, dataset) {

  design      <- options[["design"]]
  measurement <- options[["measurement"]]
  effectSize  <- options[["effectSize"]]

  if (design == "independentGroups") {
    if (measurement == "quantitative") {
      if (effectSize == "SMD") {
        inputs <- list(
          m1i  = dataset[[options[["meanGroup1"]]]],
          m2i  = dataset[[options[["meanGroup2"]]]],
          sd1i = dataset[[options[["sdGroup1"]]]],
          sd2i = dataset[[options[["sdGroup2"]]]],
          n1i  = dataset[[options[["sampleSizeGroup1"]]]],
          n2i  = dataset[[options[["sampleSizeGroup2"]]]],
          ti   = dataset[[options[["tStatistic"]]]],
          pi   = dataset[[options[["pValue"]]]],
          di   = dataset[[options[["cohensD"]]]]
        )
      } else if (effectSize %in% c("SMD1", "SMDH1")) {
        inputs <- list(
          m1i  = dataset[[options[["meanGroup1"]]]],
          m2i  = dataset[[options[["meanGroup2"]]]],
          sd2i = dataset[[options[["sdGroup2"]]]],
          n1i  = dataset[[options[["sampleSizeGroup1"]]]],
          n2i  = dataset[[options[["sampleSizeGroup2"]]]]
        )
      } else if (effectSize %in% c("CVR", "VR")) {
        inputs <- list(
          sd1i = dataset[[options[["sdGroup1"]]]],
          sd2i = dataset[[options[["sdGroup2"]]]],
          n1i  = dataset[[options[["sampleSizeGroup1"]]]],
          n2i  = dataset[[options[["sampleSizeGroup2"]]]]
        )
      } else {
        inputs <- list(
          m1i  = dataset[[options[["meanGroup1"]]]],
          m2i  = dataset[[options[["meanGroup2"]]]],
          sd1i = dataset[[options[["sdGroup1"]]]],
          sd2i = dataset[[options[["sdGroup2"]]]],
          n1i  = dataset[[options[["sampleSizeGroup1"]]]],
          n2i  = dataset[[options[["sampleSizeGroup2"]]]]
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        ai  = dataset[[options[["group1OutcomePlus"]]]],
        bi  = dataset[[options[["group1OutcomeMinus"]]]],
        ci  = dataset[[options[["group2OutcomePlus"]]]],
        di  = dataset[[options[["group2OutcomeMinus"]]]],
        n1i = dataset[[options[["sampleSizeGroup1"]]]],
        n2i = dataset[[options[["sampleSizeGroup2"]]]]
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        t1i = dataset[[options[["personTimeGroup1"]]]],
        t2i = dataset[[options[["personTimeGroup2"]]]],
        x1i = dataset[[options[["eventsGroup1"]]]],
        x2i = dataset[[options[["eventsGroup2"]]]]
      )
    } else if (measurement == "mixed") {
      if (effectSize %in% c("D2ORN", "D2ORL")) {
        inputs <- list(
          m1i  = dataset[[options[["meanGroup1"]]]],
          m2i  = dataset[[options[["meanGroup2"]]]],
          sd1i = dataset[[options[["sdGroup1"]]]],
          sd2i = dataset[[options[["sdGroup2"]]]],
          n1i  = dataset[[options[["sampleSizeGroup1"]]]],
          n2i  = dataset[[options[["sampleSizeGroup2"]]]],
          ti   = dataset[[options[["tStatistic"]]]],
          pi   = dataset[[options[["pValue"]]]],
          di   = dataset[[options[["cohensD"]]]]
        )
      } else if (effectSize %in% c("PBIT", "OR2DN", "OR2DL")) {
        inputs <- list(
          ai  = dataset[[options[["group1OutcomePlus"]]]],
          bi  = dataset[[options[["group1OutcomeMinus"]]]],
          ci  = dataset[[options[["group2OutcomePlus"]]]],
          di  = dataset[[options[["group2OutcomeMinus"]]]],
          n1i = dataset[[options[["sampleSizeGroup1"]]]],
          n2i = dataset[[options[["sampleSizeGroup2"]]]]
        )
      }
    }
  } else if (design == "variableAssociation") {
    if (measurement == "quantitative") {
      inputs <- list(
        ri = dataset[[options[["correlation"]]]],
        ni = dataset[[options[["sampleSize"]]]],
        ti = dataset[[options[["tStatistic"]]]],
        pi = dataset[[options[["pValue"]]]]
      )
    } else if (measurement == "binary") {
      if (effectSize %in% c("OR", "YUQ", "YUY", "RTET")) {
        inputs <- list(
          ai  = dataset[[options[["outcomePlusPlus"]]]],
          bi  = dataset[[options[["outcomePlusMinus"]]]],
          ci  = dataset[[options[["outcomeMinusPlus"]]]],
          di  = dataset[[options[["outcomeMinusMinus"]]]],
          n1i = dataset[[options[["outcomePlusPlusAndPlusMinus"]]]],
          n2i = dataset[[options[["outcomeMinusPlusAndMinusMinus"]]]]
        )
      } else if (effectSize %in% c("PHI", "ZPHI")) {
        inputs <- list(
          ai    = dataset[[options[["outcomePlusPlus"]]]],
          bi    = dataset[[options[["outcomePlusMinus"]]]],
          ci    = dataset[[options[["outcomeMinusPlus"]]]],
          di    = dataset[[options[["outcomeMinusMinus"]]]],
          n1i   = dataset[[options[["outcomePlusPlusAndPlusMinus"]]]],
          n2i   = dataset[[options[["outcomeMinusPlusAndMinusMinus"]]]],
          vtype = if(length(options[["samplingVarianceTypeMixed"]]) != 0) dataset[[options[["samplingVarianceTypeMixed"]]]] else NULL
        )
      }
    } else if (measurement == "mixed") {
      if (effectSize %in% c("RBIS", "ZBIS")) {
        inputs <- list(
          m1i  = dataset[[options[["meanGroup1"]]]],
          m2i  = dataset[[options[["meanGroup2"]]]],
          sd1i = dataset[[options[["sdGroup1"]]]],
          sd2i = dataset[[options[["sdGroup2"]]]],
          n1i  = dataset[[options[["sampleSizeGroup1"]]]],
          n2i  = dataset[[options[["sampleSizeGroup2"]]]],
          ti   = dataset[[options[["tStatistic"]]]],
          pi   = dataset[[options[["pValue"]]]],
          di   = dataset[[options[["cohensD"]]]]
        )
      } else if (effectSize %in% c("RPB", "ZPB")) {
        inputs <- list(
          m1i   = dataset[[options[["meanGroup1"]]]],
          m2i   = dataset[[options[["meanGroup2"]]]],
          sd1i  = dataset[[options[["sdGroup1"]]]],
          sd2i  = dataset[[options[["sdGroup2"]]]],
          n1i   = dataset[[options[["sampleSizeGroup1"]]]],
          n2i   = dataset[[options[["sampleSizeGroup2"]]]],
          ti    = dataset[[options[["tStatistic"]]]],
          pi    = dataset[[options[["pValue"]]]],
          di    = dataset[[options[["cohensD"]]]],
          vtype = if(length(options[["samplingVarianceTypeMixed"]]) != 0) dataset[[options[["samplingVarianceTypeMixed"]]]] else NULL
        )
      }
    }
  } else if (design == "singleGroup") {
    if (measurement == "quantitative") {
      if (effectSize %in% c("MN", "SMN", "MNLN", "CVLN")) {
        inputs <- list(
          mi  = dataset[[options[["mean"]]]],
          sdi = dataset[[options[["sd"]]]],
          ni  = dataset[[options[["sampleSize"]]]]
        )
      } else if (effectSize == "SDLN") {
        inputs <- list(
          sdi = dataset[[options[["sd"]]]],
          ni  = dataset[[options[["sampleSize"]]]]
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        xi  = dataset[[options[["events"]]]],
        mi  = dataset[[options[["nonEvents"]]]],
        ni  = dataset[[options[["sampleSize"]]]]
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        xi = dataset[[options[["events"]]]],
        ti = dataset[[options[["personTime"]]]],
        ni = dataset[[options[["sampleSize"]]]]
      )
    }
  } else if (design == "repeatedMeasures") {
    if (measurement == "quantitative") {
      if (effectSize %in% c("MC", "SMCR", "SMCRH", "SMCRP", "SMCRPH", "ROMC")) {
        input <- list(
          m1i  = dataset[[options[["meanTime1"]]]],
          m2i  = dataset[[options[["meanTime2"]]]],
          sd1i = dataset[[options[["sdTime1"]]]],
          sd2i = dataset[[options[["sdTime2"]]]],
          ni   = dataset[[options[["sampleSize"]]]],
          ri   = dataset[[options[["correlation"]]]]
        )
      } else if (effectSize == "SMCC") {
        input <- list(
          m1i  = dataset[[options[["meanTime1"]]]],
          m2i  = dataset[[options[["meanTime2"]]]],
          sd1i = dataset[[options[["sdTime1"]]]],
          sd2i = dataset[[options[["sdTime2"]]]],
          ni   = dataset[[options[["sampleSize"]]]],
          ri   = dataset[[options[["correlation"]]]],
          ti   = dataset[[options[["tStatistic"]]]],
          pi   = dataset[[options[["pValue"]]]],
          di   = dataset[[options[["cohensD"]]]]
        )
      } else if (effectSize %in% c("CVRC", "VRC")) {
        input <- list(
          sd1i = dataset[[options[["sdTime1"]]]],
          sd2i = dataset[[options[["sdTime2"]]]],
          ni   = dataset[[options[["sampleSize"]]]],
          ri   = dataset[[options[["correlation"]]]]
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        ai = dataset[[options[["outcomePlusPlus"]]]],
        bi = dataset[[options[["outcomePlusMinus"]]]],
        ci = dataset[[options[["outcomeMinusPlus"]]]],
        di = dataset[[options[["outcomeMinusMinus"]]]]
      )
    } else if (measurement == "binaryMarginal") {
      inputs <- list(
        ai = dataset[[options[["time1OutcomePlus"]]]],
        bi = dataset[[options[["time1OutcomeMinus"]]]],
        ci = dataset[[options[["time2OutcomePlus"]]]],
        di = dataset[[options[["time2OutcomeMinus"]]]],
        ri = dataset[[options[["correlation"]]]],
        pi = dataset[[options[["proportionPlusPlus"]]]]
      )
    }
  } else if (design == "other") {
    if (measurement == "reliability") {
      inputs <- list(
        ai = dataset[[options[["cronbachsAlpha"]]]],
        mi = dataset[[options[["items"]]]],
        ni = dataset[[options[["sampleSize"]]]]
      )
    } else if (measurement == "partialCorrelation") {
      if (effectSize %in% c("PCOR", "ZPCOR")) {
        inputs <- list(
          ti = dataset[[options[["tStatistic"]]]],
          mi = dataset[[options[["predictors"]]]],
          ni = dataset[[options[["sampleSize"]]]],
          ti = dataset[[options[["tStatistic"]]]],
          ri = dataset[[options[["semipartialCorrelation"]]]],
          pi = dataset[[options[["pValue"]]]]
        )
      } else if (effectSize %in% c("SPCOR", "ZSPCOR")) {
        inputs <- list(
          ti  = dataset[[options[["tStatistic"]]]],
          mi  = dataset[[options[["predictors"]]]],
          ni  = dataset[[options[["sampleSize"]]]],
          r2i = dataset[[options[["rSquared"]]]],
          ti  = dataset[[options[["tStatistic"]]]],
          ri  = dataset[[options[["semipartialCorrelation"]]]],
          pi  = dataset[[options[["pValue"]]]]
        )
      }
    } else if (measurement == "modelFit") {
      inputs <- list(
        mi  = dataset[[options[["predictors"]]]],
        ni  = dataset[[options[["sampleSize"]]]],
        r2i = dataset[[options[["rSquared"]]]],
        fi  = dataset[[options[["fStatistic"]]]],
        pi  = dataset[[options[["pValue"]]]]
      )
    } else if (measurement == "heterozygosity") {
      inputs <- list(
        ai = dataset[[options[["homozygousDominantAlleles"]]]],
        bi = dataset[[options[["heterozygousAlleles"]]]],
        ci = dataset[[options[["homozygousRecessiveAlleles"]]]]
      )
    }
  } else if (design == "reportedEffectSizes") {
    inputs <- list(
      yi  = dataset[[options[["effectSize"]]]],
      sei = dataset[[options[["standardError"]]]],
      vi  = dataset[[options[["samplingVariance"]]]]
    )
  }

  if (length(options[["subset"]]) == 1)
    inputs[["subset"]] <- dataset[[options[["subset"]]]]

  return(inputs)
}
getEscalcAdjustFrequenciesOptions <- function(design, measurement, effectSize, options) {

  # Conditions for when add is appropriate
  if ((design == "independentGroups" && measurement == "binary") ||
      (design == "independentGroups" && measurement == "countsPerTime") ||
      (design == "independentGroups" && measurement == "mixed" && effectSize %in% c("PBIT", "OR2DN", "OR2DL")) ||
      (design == "variableAssociation" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "countsPerTime")) {
    return(list(
      add    = options[["add"]],
      to     = options[["to"]],
      drop00 = switch(
        options[["dropStudiesWithNoCasesOrEvents"]],
        "yes" = TRUE,
        "no"  = FALSE
      )
    ))
  } else {
    return(NULL)
  }
}
getEscalcVtypeOption              <- function(design, measurement, effectSize, options) {

  # Conditions for when vtype is appropriate
  if ((design == "independentGroups"   && measurement == "quantitative" && effectSize %in% c("MD", "SMD", "SMD1", "ROM")) ||
      (design == "variableAssociation" && measurement == "quantitative") ||
      (design == "variableAssociation" && measurement == "binary") ||
      (design == "variableAssociation" && measurement == "mixed") ||
      (design == "other" && measurement == "modelFit")) {
    return(options[["samplingVarianceType"]])
  } else {
    return(NULL)
  }
}

