
EffectSizeComputation <- function(jaspResults, dataset, options, state = NULL) {


  return()
}


getEscalcDataOptions  <- function(design, measurement, effectSizeValue, options, dataset) {

  if (design == "independentGroups") {
    if (measurement == "quantitative") {
      inputs <- list(
        n1i  = dataset[[options[["nGroup1"]]]],
        n2i  = dataset[[options[["nGroup2"]]]],
        m1i  = dataset[[options[["meanGroup1"]]]],
        m2i  = dataset[[options[["meanGroup2"]]]],
        sd1i = dataset[[options[["sdGroup1"]]]],
        sd2i = dataset[[options[["sdGroup2"]]]]
      )
    } else if (measurement == "binary") {
      inputs <- list(
        ai  = dataset[[options[["nGroup1Outcome1"]]]],
        bi  = dataset[[options[["nGroup1Outcome2"]]]],
        ci  = dataset[[options[["nGroup2Outcome1"]]]],
        di  = dataset[[options[["nGroup2Outcome2"]]]],
        n1i = dataset[[options[["nGroup1"]]]],
        n2i = dataset[[options[["nGroup2"]]]],
        x1i = dataset[[options[["eventsGroup1"]]]],
        x2i = dataset[[options[["eventsGroup2"]]]]
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        t1i = dataset[[options[["personTimeGroup1"]]]],
        t2i = dataset[[options[["personTimeGroup2"]]]],
        x1i = dataset[[options[["eventsGroup1"]]]],
        x2i = dataset[[options[["eventsGroup2"]]]]
      )
    } else if (measurement == "mixed" && (effectSizeValue == "D2ORN" || effectSizeValue == "D2ORL")) {
      inputs <- list(
        ai  = dataset[[options[["nGroup1Outcome1"]]]],
        bi  = dataset[[options[["nGroup1Outcome2"]]]],
        ci  = dataset[[options[["nGroup2Outcome1"]]]],
        di  = dataset[[options[["nGroup2Outcome2"]]]],
        n1i = dataset[[options[["nGroup1"]]]],
        n2i = dataset[[options[["nGroup2"]]]]
      )
    } else if (measurement == "mixed" && (effectSizeValue == "PBIT" || effectSizeValue == "OR2DN" || effectSizeValue == "OR2DL")) {
      inputs <- list(
        n1i  = dataset[[options[["nGroup1"]]]],
        n2i  = dataset[[options[["nGroup2"]]]],
        m1i  = dataset[[options[["meanGroup1"]]]],
        m2i  = dataset[[options[["meanGroup2"]]]],
        sd1i = dataset[[options[["sdGroup1"]]]],
        sd2i = dataset[[options[["sdGroup2"]]]]
      )
    }
  } else if (design == "variableAssociation") {
    if (measurement == "quantitative") {
      inputs <- list(
        ri = dataset[[options[["rawCorrelations"]]]],
        ni = dataset[[options[["sampleSizes"]]]],
        ti = dataset[[options[["tStatistics"]]]],
        pi = dataset[[options[["pValues"]]]],
        fi = dataset[[options[["fStatistics"]]]]
      )
    } else if (measurement == "binary") {
      inputs <- list(
        ai  = dataset[[options[["nVariable1Outcome+"]]]],
        bi  = dataset[[options[["nVariable1Outcome-"]]]],
        ci  = dataset[[options[["nVariable2Outcome+"]]]],
        di  = dataset[[options[["nVariable2Outcome-"]]]],
        n1i = dataset[[options[["nRow1"]]]],
        n2i = dataset[[options[["nRow2"]]]]
      )
    } else if (measurement == "mixed") {
      inputs <- list(
        ri = dataset[[options[["rawCorrelations"]]]],
        ni = dataset[[options[["sampleSizes"]]]],
        ti = dataset[[options[["tStatistics"]]]],
        pi = dataset[[options[["pValues"]]]],
        fi = dataset[[options[["fStatistics"]]]],
        ai = dataset[[options[["nVariable1Outcome+"]]]],
        bi = dataset[[options[["nVariable1Outcome-"]]]],
        ci = dataset[[options[["nVariable2Outcome+"]]]],
        di = dataset[[options[["nVariable2Outcome-"]]]]
      )
    }
  } else if (design == "singleGroup") {
    if (measurement == "quantitative") {
      inputs <- list(
        mi  = dataset[[options[["complementFrequencies"]]]],
        sdi = dataset[[options[["standardDeviations"]]]],
        ni  = dataset[[options[["sampleSizes"]]]],
        yi  = dataset[[options[["observedEffectSizes"]]]],
        vi  = dataset[[options[["samplingVariances"]]]],
        sei = dataset[[options[["standardErrors"]]]],
        m1i = dataset[[options[["meanSample1"]]]]
      )
    } else if (measurement == "binary") {
      inputs <- list(
        xi  = dataset[[options[["eventFrequencies"]]]],
        mi  = dataset[[options[["complementFrequencies"]]]],
        ni  = dataset[[options[["sampleSizes"]]]],
        yi  = dataset[[options[["observedEffectSizes"]]]],
        vi  = dataset[[options[["samplingVariances"]]]],
        sei = dataset[[options[["standardErrors"]]]]
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        x1i = dataset[[options[["eventsSample1"]]]],
        t1i = dataset[[options[["totalPersonTimes"]]]],
        ni  = dataset[[options[["sampleSizes"]]]],
        yi  = dataset[[options[["observedEffectSizes"]]]],
        vi  = dataset[[options[["samplingVariances"]]]],
        sei = dataset[[options[["standardErrors"]]]]
      )
    }
  } else if (design == "repeatedMeasuresOrMatchedGroups") {
    if (measurement == "quantitative") {
      inputs <- list(
        n1i  = dataset[[options[["nTimePoint1"]]]],
        n2i  = dataset[[options[["nTimePoint2"]]]],
        m1i  = dataset[[options[["meanTimePoint1"]]]],
        m2i  = dataset[[options[["meanTimePoint2"]]]],
        sd1i = dataset[[options[["sdTimePoint1"]]]],
        sd2i = dataset[[options[["sdTimePoint2"]]]]
      )
    } else if (measurement == "binary") {
      inputs <- list(
        ai  = dataset[[options[["nTreatment1Outcome1Treatment2Outcome2+"]]]],
        bi  = dataset[[options[["nTreatment1Outcome2Treatment2Outcome1+"]]]],
        ci  = dataset[[options[["nTreatment2Outcome1Treatment1Outcome2+"]]]],
        di  = dataset[[options[["nTreatment2Outcome2Treatment1Outcome1+"]]]],
        n1i = dataset[[options[["nTimePoint1"]]]],
        n2i = dataset[[options[["nTimePoint2"]]]],
        x1i = dataset[[options[["eventsTimePoint1"]]]],
        x2i = dataset[[options[["eventsTimePoint2"]]]]
      )
    }
  } else if (design == "other") {
    if (effectSizeValue == "ARAW" || effectSizeValue == "AHW" || effectSizeValue == "ABT") {
      inputs <- list(
        ai = dataset[[options[["alpha"]]]],
        bi = dataset[[options[["beta"]]]],
        ci = dataset[[options[["gamma"]]]],
        di = dataset[[options[["delta"]]]]
      )
    } else if (effectSizeValue == "REH") {
      inputs <- list(
        ai = dataset[[options[["nHomozygousDominantAlleles"]]]],
        bi = dataset[[options[["nHomozygousRecessiveAlleles"]]]],
        ci = dataset[[options[["nHeterozygousAlleles"]]]]
      )
    } else if (effectSizeValue == "R2" || effectSizeValue == "ZR2") {
      inputs <- list(
        r2i = dataset[[options[["rSquared"]]]],
        ni  = dataset[[options[["sampleSizes"]]]]
      )
    }
  }

  return(inputs)
}
getEscalcAddOption    <- function(design, measurement, effectSizeValue, options) {

  # Conditions for when add is appropriate
  if ((design == "independentGroups" && measurement == "binary" && (effectSize %in% c("RR", "OR", "RD", "AS", "PETO"))) ||
      (design == "independentGroups" && measurement == "countsPerTime") ||
      (design == "repeatedMeasuresOrMatchedGroups" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "countsPerTime") ||
      (design == "variableAssociation" && measurement == "binary") ||
      (design == "independentGroups" && measurement == "mixed" && effectSize %in% c("D2ORN", "D2ORL"))) {
    return(options[["add"]])
  } else {
    return(NULL)
  }
}
getEscalcToOption     <- function(design, measurement, effectSizeValue, options) {

  # Conditions for when to is appropriate
  if ((design == "independentGroups" && measurement == "binary") ||
      (design == "independentGroups" && measurement == "countsPerTime") ||
      (design == "repeatedMeasuresOrMatchedGroups" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "countsPerTime") ||
      (design == "variableAssociation" && measurement == "binary") ||
      (design == "independentGroups" && measurement == "mixed" && effectSize %in% c("D2ORN", "D2ORL"))) {
    return(options[["to"]])
  } else {
    return(NULL) # Return NULL if 'to' is not appropriate for the given conditions
  }
}
getEscalcDrop00Option <- function(design, measurement, effectSizeValue, options) {

  # Conditions for when drop00 is appropriate
  if ((design == "independentGroups" && measurement == "binary") ||
      (design == "independentGroups" && measurement == "countsPerTime") ||
      (design == "repeatedMeasuresOrMatchedGroups" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "countsPerTime") ||
      (design == "variableAssociation" && measurement == "binary") ||
      (design == "independentGroups" && measurement == "mixed" && effectSize %in% c("D2ORN", "D2ORL"))) {
    return(options[["dropStudiesWithNoCasesOrEvents"]])
  } else {
    return(NULL)
  }
}
getEscalcVtypeOption  <- function(design, measurement, effectSizeValue, options) {

  # Conditions for when vtype is appropriate
  if ((design == "independentGroups" && measurement == "quantitative") ||
      (design == "independentGroups" && measurement == "binary") ||
      (design == "independentGroups" && measurement == "countsPerTime") ||
      (design == "independentGroups" && measurement == "mixed" && (effectSize %in% c("PBIT", "OR2DN", "OR2DL"))) ||
      (design == "variableAssociation" && measurement == "quantitative") ||
      (design == "variableAssociation" && measurement == "binary") ||
      (design == "variableAssociation" && measurement == "mixed") ||
      (design == "singleGroup" && measurement == "quantitative") ||
      (design == "singleGroup" && measurement == "binary") ||
      (design == "singleGroup" && measurement == "countsPerTime") ||
      (design == "repeatedMeasuresOrMatchedGroups" && measurement == "quantitative") ||
      (design == "repeatedMeasuresOrMatchedGroups" && measurement == "binary")) {
    return(options[["samplingVarianceType"]])
  } else {
    return(NULL)
  }
}

inputs <- c(
  "nGroup1Outcome1",
  "nVariable1Outcome+",
  "nTreatment1Outcome1Treatment2Outcome2+",
  "alpha",
  "nHomozygousDominantAlleles",
  "nGroup1Outcome2",
  "nVariable1Outcome-",
  "nTreatment1Outcome2Treatment2Outcome1+",
  "beta",
  "nHomozygousRecessiveAlleles",
  "nGroup2Outcome1",
  "nVariable2Outcome+",
  "nTreatment2Outcome1Treatment1Outcome2+",
  "gamma",
  "nHeterozygousAlleles",
  "nGroup2Outcome2",
  "nVariable2Outcome-",
  "nTreatment2Outcome2Treatment1Outcome1+",
  "delta",
  "nNonEvents",
  "nGroup1",
  "nRow1",
  "nSample1",
  "nTimePoint1",
  "nGroup2",
  "nRow2",
  "nSample2",
  "nTimePoint2",
  "eventsGroup1",
  "eventsSample1",
  "eventsTimePoint1",
  "eventsGroup2",
  "eventsSample2",
  "eventsTimePoint2",
  "personTimeGroup1",
  "personTimeSample1",
  "personTimeGroup2",
  "personTimeSample2",
  "meanGroup1",
  "meanSample1",
  "meanTimePoint1",
  "meanGroup2",
  "meanSample2",
  "meanTimePoint2",
  "sdGroup1",
  "sdSample1",
  "sdTimePoint1",
  "sdGroup2",
  "sdSample2",
  "sdTimePoint2",
  "eventFrequencies",
  "complementFrequencies",
  "rawCorrelations",
  "tStatistics",
  "totalPersonTimes",
  "fStatistics",
  "pValues",
  "standardDeviations",
  "rSquared",
  "sampleSizes",
  "observedEffectSizes",
  "samplingVariances",
  "standardErrors"
)
