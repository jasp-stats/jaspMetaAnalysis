# Classical network meta-analysis.
#
# Entry point of the frequentist network meta-analysis fitted with netmeta: readiness,
# dependency vectors, and the option helpers shared by the data, fit, table, and R code files.

# Analysis ----

ClassicalNetworkMetaAnalysis <- function(jaspResults, dataset = NULL, options, ...) {

  options[["analysis"]] <- "networkMetaAnalysis"

  if (.nmaReady(options)) {
    dataset <- .nmaCheckData(dataset, options)
    .nmaCheckErrors(dataset, options)
  }

  .nmaFitModels(jaspResults, dataset, options)

  .nmaModelSummaryTables(jaspResults, options)
  .nmaTreatmentEffectsTables(jaspResults, options)
  .nmaExtensionOutputs(jaspResults, options)

  .nmaRankingOutputs(jaspResults, options)

  # the inconsistency, evidence structure, and small-study effects analyses need the standard model
  if (!.nmaIsComponentModel(options)) {
    .nmaInconsistencyOutputs(jaspResults, options)
    .nmaEvidenceOutputs(jaspResults, options)
  }

  if (options[["networkPlot"]])
    .nmaNetworkPlot(jaspResults, options)

  if (options[["forestPlot"]])
    .nmaForestPlot(jaspResults, options)

  if (.nmaIsComponentModel(options) && options[["componentForestPlot"]])
    .nmaComponentForestPlot(jaspResults, options)

  if (!.nmaIsComponentModel(options))
    .nmaSmallStudyEffectsOutputs(jaspResults, options)

  if (options[["showNetmetaRCode"]])
    .nmaShowRCode(jaspResults, options)

  return()
}

# Dependencies ----

# Options entering the data preparation or any argument of the fitting calls
# (display options such as the ranking, tables, and plots are not model dependencies).
.nmaModelDependencies <- c(
  # variables
  "studyId", "effectSize", "effectSizeStandardError", "treatment1", "treatment2",
  "treatment", "armEvents", "armSampleSize", "armMean", "armStandardDeviation", "armPersonTime",
  "armEffectSize", "armEffectSizeStandardError", "predictor", "subgroup",
  # data format, outcome, and pooling
  "dataFormat", "outcomeType", "effectSizeMeasure", "poolingMethod",
  # model (standard or component; the components, the inactive treatment, and the interactions of the component model)
  "model", "componentSpecification", "componentSeparator", "componentVariables", "componentInactiveTreatment", "componentInteractions",
  "method", "fixedEffectTest", "referenceTreatment", "confidenceIntervalsLevel",
  "advancedMultiArmTolerance", "fixParametersTau2", "fixParametersTau2Value",
  # continuity correction
  "advancedAdd", "advancedTo", "advancedDropStudiesWithNoCasesOrEvents"
)

# Readiness and option helpers ----

.nmaReady               <- function(options) {

  if (options[["studyId"]] == "")
    return(FALSE)

  # the component model with indicators needs the indicator variables (the tests table notes them)
  if (!is.null(.nmaMissingVariablesMessage(options)))
    return(FALSE)

  if (options[["dataFormat"]] == "contrastBased") {
    variables <- c(options[["effectSize"]], options[["effectSizeStandardError"]], options[["treatment1"]], options[["treatment2"]])
  } else {
    variables <- c(options[["treatment"]], .nmaArmOutcomeVariables(options))
  }

  return(all(variables != ""))
}

.nmaIsComponentModel    <- function(options) {

  return(options[["model"]] == "component")
}

# A note on the variables that the selected model still needs: the indicator variables of the component model with indicators.
.nmaMissingVariablesMessage <- function(options) {

  if (.nmaIsComponentModel(options) && .nmaComponentMode(options) == "indicatorVariables" && length(options[["componentVariables"]]) == 0)
    return(gettext("The component model with indicators requires the component indicator variables."))

  return(NULL)
}

.nmaIsArmBased          <- function(options) {

  return(options[["dataFormat"]] == "armBased")
}

.nmaArmOutcomeVariables <- function(options) {

  # the data columns of the selected arm-based outcome type (in the order of the pairwise() call)
  return(switch(
    options[["outcomeType"]],
    binary        = c(options[["armEvents"]], options[["armSampleSize"]]),
    continuous    = c(options[["armMean"]], options[["armStandardDeviation"]], options[["armSampleSize"]]),
    incidenceRate = c(options[["armEvents"]], options[["armPersonTime"]]),
    general       = c(options[["armEffectSize"]], options[["armEffectSizeStandardError"]])
  ))
}

.nmaPoolingMethod       <- function(options) {

  # the pooling method is available for arm-based binary data with the odds ratio only
  # (the GUI keeps the value of the hidden option)
  if (.nmaIsArmBased(options) && options[["outcomeType"]] == "binary" && options[["effectSizeMeasure"]] == "OR")
    return(options[["poolingMethod"]])

  return("inverseVariance")
}

# The count-based pooling methods (Mantel-Haenszel, non-central hypergeometric, penalized logistic regression) lack the model structures of
# the inverse variance network meta-analysis. The form disables the outputs that need them and keeps the values of the disabled options
# (so they stay checked): R must not build these outputs (the conditions mirror the 'enabled' properties of the QML components).
.nmaIsCountBased        <- function(options) {

  return(.nmaPoolingMethod(options) != "inverseVariance")
}

.nmaIsTwoTreatmentFit   <- function(fit) {

  return(length(fit[["trts"]]) < 3)
}

.nmaIsRandomEffects     <- function(options) {

  # the Mantel-Haenszel and the non-central hypergeometric methods provide only the common-effect model
  return(options[["method"]] != "equalEffects" && !.nmaPoolingMethod(options) %in% c("mantelHaenszel", "nonCentralHypergeometric"))
}

.nmaMethodTau           <- function(options) {

  return(switch(
    options[["method"]],
    restrictedML      = "REML",
    maximumLikelihood = "ML",
    derSimonianLaird  = "DL"
  ))
}

# netmeta's 'small.values': negative beneficial effects make small treatment effects desirable
.nmaSmallValues         <- function(options) {

  return(if (options[["beneficialEffect"]] == "negative") "desirable" else "undesirable")
}

.nmaFixedTau2           <- function(options) {

  tau2 <- try(.parseRCodeInOptions(options[["fixParametersTau2Value"]]), silent = TRUE)

  if (jaspBase::isTryError(tau2) || !is.numeric(tau2) || length(tau2) != 1 || is.na(tau2) || tau2 < 0)
    .quitAnalysis(gettext("The fixed value for tau2 must be a positive number."))

  return(tau2)
}
