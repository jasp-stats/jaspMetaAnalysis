# Effect-size computation R-code output.
#
# Constructs reproducible metafor escalc calls and R-safe names.

# Output ----

.escShowMetaforRCode                  <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["metaforRCode"]]))
    return()

  callText <- .escMakeMetaforCallText(dataset, options)
  if (callText == "")
    return()

  metaforRCode <- createJaspHtml(title = gettext("Metafor R Code"))
  metaforRCode$dependOn(c("effectSizeType", "variables", "showMetaforRCode"))
  metaforRCode$position <- 99
  metaforRCode$text <- .maTransformToHtml(callText)

  jaspResults[["metaforRCode"]] <- metaforRCode

  return()
}

.escMakeMetaforCallText               <- function(dataset, options) {

  callText <- c()
  dataName <- "dataset"

  for (i in seq_along(options[["variables"]])) {

    effectSizeType <- options[["effectSizeType"]][[i]]
    variables      <- options[["variables"]][[i]]

    if (effectSizeType[["design"]] == "reportedEffectSizes" && !.escReportedEffectSizesReady(variables, all = FALSE))
      next

    callDataOptions <- .escGetEscalcCallDataOptions(effectSizeType, variables)
    if (length(callDataOptions) == 0)
      next

    escalcInput <- c(
      callDataOptions,
      .escGetEscalcCallAdjustFrequenciesOptions(effectSizeType, variables),
      .escGetEscalcCallVtypeOption(effectSizeType, variables),
      .escGetEscalcCorrectOption(effectSizeType, variables),
      measure     = .metaQuoteRString(if (effectSizeType[["design"]] == "reportedEffectSizes") "GEN" else effectSizeType[["effectSize"]]),
      replace     = i == 1,
      add.measure = TRUE,
      data        = dataName
    )

    callText <- c(
      callText,
      paste0(
        "dataset <- escalc(\n\t",
        paste(names(escalcInput), "=", escalcInput, collapse = ",\n\t"),
        "\n)"
      )
    )

    dataName <- "dataset"
  }

  return(paste(callText, collapse = "\n\n"))
}

# Call arguments ----

.escGetEscalcCallDataOptions          <- function(effectSizeType, variables) {

  inputMapping <- .escMapEscalcInput2VariableInputs(effectSizeType)

  if (length(inputMapping) == 0)
    return(NULL)

  inputs <- list()
  for (inputName in names(inputMapping)) {
    variableName  <- inputMapping[[inputName]]
    variableValue <- variables[[variableName]]
    if (!is.null(variableValue) && length(variableValue) == 1 && variableValue != "")
      inputs[[inputName]] <- .metaAsRName(variableValue)
  }

  if (effectSizeType[["design"]] == "reportedEffectSizes" && length(variables[["confidenceInterval"]]) != 0) {
    inputs[["lci"]] <- .metaAsRName(variables[["confidenceInterval"]][[1]][1])
    inputs[["uci"]] <- .metaAsRName(variables[["confidenceInterval"]][[1]][2])
  }

  return(inputs)
}

.escGetEscalcCallAdjustFrequenciesOptions <- function(effectSizeType, variables) {

  adjustOptions <- .escGetEscalcAdjustFrequenciesOptions(effectSizeType, variables)
  if (!is.null(adjustOptions))
    adjustOptions[["to"]] <- .metaQuoteRString(adjustOptions[["to"]])

  return(adjustOptions)
}

.escGetEscalcCallVtypeOption          <- function(effectSizeType, variables) {

  vtypeOption <- .escGetEscalcVtypeOption(effectSizeType, variables)
  if (!is.null(vtypeOption))
    vtypeOption[["vtype"]] <- .metaQuoteRString(vtypeOption[["vtype"]])

  return(vtypeOption)
}

# Variable-option mapping ----

# Parallel to .escMapEscalcInput2Options(); both mappings must expose
# identical metafor::escalc argument keys for each effect-size type.
.escMapEscalcInput2VariableInputs     <- function(effectSizeType) {

  design      <- effectSizeType[["design"]]
  measurement <- effectSizeType[["measurement"]]
  effectSize  <- effectSizeType[["effectSize"]]

  if (design == "independentGroups") {
    if (measurement == "quantitative") {
      if (effectSize == "SMD") {
        inputs <- list(
          m1i  = "meanGroup1",
          m2i  = "meanGroup2",
          sd1i = "sdGroup1",
          sd2i = "sdGroup2",
          n1i  = "sampleSizeGroup1",
          n2i  = "sampleSizeGroup2",
          ti   = "tStatistic",
          pi   = "pValue",
          di   = "cohensD"
        )
      } else if (effectSize %in% c("SMD1", "SMD1H")) {
        inputs <- list(
          m1i  = "meanGroup1",
          m2i  = "meanGroup2",
          sd2i = "sdGroup2",
          n1i  = "sampleSizeGroup1",
          n2i  = "sampleSizeGroup2"
        )
      } else if (effectSize %in% c("CVR", "VR")) {
        inputs <- list(
          sd1i = "sdGroup1",
          sd2i = "sdGroup2",
          n1i  = "sampleSizeGroup1",
          n2i  = "sampleSizeGroup2"
        )
      } else {
        inputs <- list(
          m1i  = "meanGroup1",
          m2i  = "meanGroup2",
          sd1i = "sdGroup1",
          sd2i = "sdGroup2",
          n1i  = "sampleSizeGroup1",
          n2i  = "sampleSizeGroup2"
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        ai  = "group1OutcomePlus",
        bi  = "group1OutcomeMinus",
        ci  = "group2OutcomePlus",
        di  = "group2OutcomeMinus",
        n1i = "sampleSizeGroup1",
        n2i = "sampleSizeGroup2"
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        t1i = "personTimeGroup1",
        t2i = "personTimeGroup2",
        x1i = "eventsGroup1",
        x2i = "eventsGroup2"
      )
    } else if (measurement == "mixed") {
      if (effectSize %in% c("D2ORN", "D2ORL")) {
        inputs <- list(
          m1i  = "meanGroup1",
          m2i  = "meanGroup2",
          sd1i = "sdGroup1",
          sd2i = "sdGroup2",
          n1i  = "sampleSizeGroup1",
          n2i  = "sampleSizeGroup2",
          ti   = "tStatistic",
          pi   = "pValue",
          di   = "cohensD"
        )
      } else if (effectSize %in% c("PBIT", "OR2DN", "OR2DL")) {
        inputs <- list(
          ai  = "group1OutcomePlus",
          bi  = "group1OutcomeMinus",
          ci  = "group2OutcomePlus",
          di  = "group2OutcomeMinus",
          n1i = "sampleSizeGroup1",
          n2i = "sampleSizeGroup2"
        )
      }
    }
  } else if (design == "variableAssociation") {
    if (measurement == "quantitative") {
      inputs <- list(
        ri = "correlation",
        ni = "sampleSize",
        ti = "tStatistic",
        pi = "pValue"
      )
    } else if (measurement == "binary") {
      if (effectSize %in% c("OR", "YUQ", "YUY", "RTET", "ZTET")) {
        inputs <- list(
          ai  = "outcomePlusPlus",
          bi  = "outcomePlusMinus",
          ci  = "outcomeMinusPlus",
          di  = "outcomeMinusMinus",
          n1i = "outcomePlusPlusAndPlusMinus",
          n2i = "outcomeMinusPlusAndMinusMinus"
        )
      } else if (effectSize %in% c("PHI", "ZPHI")) {
        inputs <- list(
          ai    = "outcomePlusPlus",
          bi    = "outcomePlusMinus",
          ci    = "outcomeMinusPlus",
          di    = "outcomeMinusMinus",
          n1i   = "outcomePlusPlusAndPlusMinus",
          n2i   = "outcomeMinusPlusAndMinusMinus",
          vtype = "samplingVarianceTypeMixed"
        )
      }
    } else if (measurement == "mixed") {
      if (effectSize %in% c("RBIS", "ZBIS")) {
        inputs <- list(
          m1i  = "meanGroup1",
          m2i  = "meanGroup2",
          sd1i = "sdGroup1",
          sd2i = "sdGroup2",
          n1i  = "sampleSizeGroup1",
          n2i  = "sampleSizeGroup2",
          ti   = "tStatistic",
          pi   = "pValue",
          di   = "cohensD"
        )
      } else if (effectSize %in% c("RPB", "ZPB")) {
        inputs <- list(
          m1i   = "meanGroup1",
          m2i   = "meanGroup2",
          sd1i  = "sdGroup1",
          sd2i  = "sdGroup2",
          n1i   = "sampleSizeGroup1",
          n2i   = "sampleSizeGroup2",
          ti    = "tStatistic",
          pi    = "pValue",
          di    = "cohensD",
          vtype = "samplingVarianceTypeMixed"
        )
      }
    }
  } else if (design == "singleGroup") {
    if (measurement == "quantitative") {
      if (effectSize %in% c("MN", "SMN", "MNLN", "CVLN")) {
        inputs <- list(
          mi  = "mean",
          sdi = "sd",
          ni  = "sampleSize"
        )
      } else if (effectSize == "SDLN") {
        inputs <- list(
          sdi = "sd",
          ni  = "sampleSize"
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        xi = "events",
        mi = "nonEvents",
        ni = "sampleSize"
      )
    } else if (measurement == "countsPerTime") {
      inputs <- list(
        xi = "events",
        ti = "personTime",
        ni = "sampleSize"
      )
    }
  } else if (design == "repeatedMeasures") {
    if (measurement == "quantitative") {
      if (effectSize %in% c("MC", "SMCR", "SMCRH", "SMCRP", "SMCRPH", "ROMC")) {
        inputs <- list(
          m1i  = "meanTime1",
          m2i  = "meanTime2",
          sd1i = "sdTime1",
          sd2i = "sdTime2",
          ni   = "sampleSize",
          ri   = "correlation"
        )
      } else if (effectSize == "SMCC") {
        inputs <- list(
          m1i  = "meanTime1",
          m2i  = "meanTime2",
          sd1i = "sdTime1",
          sd2i = "sdTime2",
          ni   = "sampleSize",
          ri   = "correlation",
          ti   = "tStatistic",
          pi   = "pValue",
          di   = "cohensD"
        )
      } else if (effectSize %in% c("CVRC", "VRC")) {
        inputs <- list(
          sd1i = "sdTime1",
          sd2i = "sdTime2",
          ni   = "sampleSize",
          ri   = "correlation"
        )
      }
    } else if (measurement == "binary") {
      inputs <- list(
        ai = "outcomePlusPlus",
        bi = "outcomePlusMinus",
        ci = "outcomeMinusPlus",
        di = "outcomeMinusMinus"
      )
    } else if (measurement == "binaryMarginal") {
      inputs <- list(
        ai = "time1OutcomePlus",
        bi = "time1OutcomeMinus",
        ci = "time2OutcomePlus",
        di = "time2OutcomeMinus",
        ri = "correlation",
        pi = "proportionPlusPlus"
      )
    }
  } else if (design == "other") {
    if (measurement == "reliability") {
      inputs <- list(
        ai = "coefficientAlpha",
        mi = "items",
        ni = "sampleSize"
      )
    } else if (measurement == "partialCorrelation") {
      if (effectSize %in% c("PCOR", "ZPCOR")) {
        inputs <- list(
          ti = "tStatistic",
          mi = "predictors",
          ni = "sampleSize",
          ri = "semipartialCorrelation",
          pi = "pValue"
        )
      } else if (effectSize %in% c("SPCOR", "ZSPCOR")) {
        inputs <- list(
          ti  = "tStatistic",
          mi  = "predictors",
          ni  = "sampleSize",
          r2i = "rSquared",
          ri  = "semipartialCorrelation",
          pi  = "pValue"
        )
      }
    } else if (measurement == "modelFit") {
      inputs <- list(
        mi  = "predictors",
        ni  = "sampleSize",
        r2i = "rSquared",
        fi  = "fStatistic",
        pi  = "pValue"
      )
    } else if (measurement == "heterozygosity") {
      inputs <- list(
        ai = "homozygousDominantAlleles",
        bi = "heterozygousAlleles",
        ci = "homozygousRecessiveAlleles"
      )
    }
  } else if (design == "reportedEffectSizes") {
    inputs <- list(
      yi  = "effectSize",
      sei = "standardError",
      vi  = "samplingVariance"
    )
  }

  if (!exists("inputs"))
    inputs <- list()

  return(inputs)
}

