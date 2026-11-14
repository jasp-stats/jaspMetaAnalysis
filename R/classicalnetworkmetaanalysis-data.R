# Classical network meta-analysis data.
#
# Selects the complete cases of the variables used by the analysis and validates
# the data before they are passed to the pairwise() and netmeta() calls.

# Data preparation ----

# The predictor of the network meta-regression is a data variable of the standard model with the inverse variance pooling only (the
# component model and the count-based pooling methods ignore it, while the form keeps the assigned variable)
.nmaUsesPredictor       <- function(options) {

  return(options[["predictor"]] != "" && !.nmaIsComponentModel(options) && !.nmaIsCountBased(options))
}

.nmaDataVariables       <- function(options) {

  # all columns used by the analysis (the treatments and outcomes depend on the data format)
  variables <- c(
    options[["studyId"]],
    .nmaTreatmentVariables(options),
    .nmaNumericVariables(options),
    # (the indicator variables of the components of the component model with indicators)
    if (.nmaIsComponentModel(options) && .nmaComponentMode(options) == "indicatorVariables") options[["componentVariables"]],
    # (the network meta-regression belongs to the standard model with the inverse variance pooling)
    if (.nmaUsesPredictor(options)) options[["predictor"]],
    if (options[["subgroup"]]  != "") options[["subgroup"]]
  )

  return(unique(variables))
}

.nmaNumericVariables    <- function(options) {

  if (.nmaIsArmBased(options))
    return(.nmaArmOutcomeVariables(options))

  return(c(options[["effectSize"]], options[["effectSizeStandardError"]]))
}

.nmaTreatmentVariables  <- function(options) {

  if (.nmaIsArmBased(options))
    return(options[["treatment"]])

  return(c(options[["treatment1"]], options[["treatment2"]]))
}

.nmaCheckData           <- function(dataset, options) {

  # omit incomplete rows (a study with an omitted arm remains in the analysis with its other arms)
  anyNaByRows <- apply(dataset[, .nmaDataVariables(options), drop = FALSE], 1, function(x) anyNA(x))
  omitted     <- dataset[anyNaByRows, , drop = FALSE]
  dataset     <- dataset[!anyNaByRows, , drop = FALSE]

  # drop empty factor levels (e.g., treatments that appear only in the omitted rows)
  dataset     <- droplevels(dataset)

  attr(dataset, "NAs")             <- sum(anyNaByRows)
  attr(dataset, "NasIds")          <- anyNaByRows
  attr(dataset, "omittedSubgroup") <- if (options[["subgroup"]] != "") as.character(omitted[[options[["subgroup"]]]])
  attr(dataset, "omittedStudies")  <- as.character(omitted[[options[["studyId"]]]])

  return(dataset)
}

# Data validation ----

.nmaVariableName        <- function(variable) {

  return(jaspBase::decodeColNames(variable))
}

.nmaCheckErrors         <- function(dataset, options) {

  # the random effects (dispersion) model is the random-effects model of penalized logistic regression
  if (options[["method"]] == "randomEffects" && .nmaPoolingMethod(options) != "penalizedLogisticRegression")
    .quitAnalysis(gettext("The selected method is only available for the penalized logistic regression pooling method."))

  # fixed between-study variance (FormulaField)
  if (options[["fixParametersTau2"]] && .nmaIsRandomEffects(options) && .nmaPoolingMethod(options) == "inverseVariance")
    .nmaFixedTau2(options)

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations"),
    all.target           = .nmaNumericVariables(options),
    observations.amount  = "< 2",
    exitAnalysisIfErrors = TRUE
  )

  .nmaCheckValueRanges(dataset, options)
  .nmaCheckTreatments(dataset, options)
  .nmaCheckStudyLevelVariables(dataset, options)

  return()
}

.nmaCheckValueRanges    <- function(dataset, options) {

  studies <- as.character(dataset[[options[["studyId"]]]])

  # the first offending value is reported with its study
  checkRange <- function(variable, positive) {

    values  <- dataset[[variable]]
    invalid <- which(if (positive) values <= 0 else values < 0)

    if (length(invalid) > 0)
      .quitAnalysis(gettextf(
        if (positive) "The variable '%1$s' must contain positive values, but it contains %2$s in the study '%3$s'."
        else          "The variable '%1$s' must contain non-negative values, but it contains %2$s in the study '%3$s'.",
        .nmaVariableName(variable), format(values[invalid[1]]), studies[invalid[1]]
      ))

    return()
  }

  # only the variables of the selected data format and outcome type are checked
  if (!.nmaIsArmBased(options)) {
    checkRange(options[["effectSizeStandardError"]], positive = TRUE)
  } else {
    switch(
      options[["outcomeType"]],
      binary        = {
        checkRange(options[["armEvents"]],     positive = FALSE)
        checkRange(options[["armSampleSize"]], positive = TRUE)
      },
      continuous    = {
        # the standardized mean difference and the ratio of means divide by the standard deviations; the ratio of means is the logarithm of
        # the ratio of the means, which are positive
        checkRange(options[["armStandardDeviation"]], positive = options[["effectSizeMeasure"]] %in% c("SMD", "ROM"))
        if (options[["effectSizeMeasure"]] == "ROM")
          checkRange(options[["armMean"]],            positive = TRUE)
        checkRange(options[["armSampleSize"]],        positive = TRUE)
      },
      incidenceRate = {
        checkRange(options[["armEvents"]],     positive = FALSE)
        checkRange(options[["armPersonTime"]], positive = TRUE)
      },
      general       = checkRange(options[["armEffectSizeStandardError"]], positive = TRUE)
    )
  }

  # binary outcomes cannot have more events than participants
  if (.nmaIsArmBased(options) && options[["outcomeType"]] == "binary") {
    tooManyEvents <- which(dataset[[options[["armEvents"]]]] > dataset[[options[["armSampleSize"]]]])
    if (length(tooManyEvents) > 0)
      .quitAnalysis(gettextf(
        "The number of events exceeds the sample size in the study '%1$s' (treatment '%2$s').",
        studies[tooManyEvents[1]],
        as.character(dataset[[options[["treatment"]]]][tooManyEvents[1]])
      ))
  }

  return()
}

.nmaCheckTreatments     <- function(dataset, options) {

  studies    <- as.character(dataset[[options[["studyId"]]]])
  treatments <- lapply(.nmaTreatmentVariables(options), function(variable) as.character(dataset[[variable]]))

  if (length(unique(unlist(treatments))) < 2)
    .quitAnalysis(gettext("The network meta-analysis requires at least two different treatments."))

  if (.nmaIsArmBased(options)) {

    # each treatment can appear only once in a study
    repeatedArm <- which(duplicated(data.frame(study = studies, treatment = treatments[[1]])))
    if (length(repeatedArm) > 0)
      .quitAnalysis(gettextf(
        "The study '%1$s' contains the treatment '%2$s' in more than one arm. Each treatment can appear in a study only once.",
        studies[repeatedArm[1]], treatments[[1]][repeatedArm[1]]
      ))

  } else {

    # a comparison needs two different treatments
    sameTreatment <- which(treatments[[1]] == treatments[[2]])
    if (length(sameTreatment) > 0)
      .quitAnalysis(gettextf(
        "The study '%1$s' contains a comparison of the treatment '%2$s' with itself. The two treatments of a comparison must differ.",
        studies[sameTreatment[1]], treatments[[1]][sameTreatment[1]]
      ))

    # a comparison can appear only once in a study (in either direction)
    comparisons <- data.frame(study = studies, first = pmin(treatments[[1]], treatments[[2]]), second = pmax(treatments[[1]], treatments[[2]]))
    repeated    <- which(duplicated(comparisons))[1]
    if (!is.na(repeated)) {
      # (the comparison as its first occurrence in the data)
      first <- which(comparisons$study == comparisons$study[repeated] & comparisons$first == comparisons$first[repeated] & comparisons$second == comparisons$second[repeated])[1]
      .quitAnalysis(gettextf(
        "The study '%1$s' contains the comparison '%2$s' vs. '%3$s' more than once. Each comparison can appear in a study only once.",
        studies[first], treatments[[1]][first], treatments[[2]][first]
      ))
    }
  }

  return()
}

.nmaCheckStudyLevelVariables <- function(dataset, options) {

  # the predictor and the subgroup describe studies; they must not vary within a study
  studies <- as.character(dataset[[options[["studyId"]]]])

  # (the predictor of the network meta-regression is used by the standard model with the inverse variance pooling only)
  for (variable in c(if (.nmaUsesPredictor(options)) options[["predictor"]], options[["subgroup"]])) {

    if (variable == "")
      next

    values        <- as.character(dataset[[variable]])
    isVarying     <- tapply(values, studies, function(x) length(unique(x)) > 1)
    varyingStudy  <- names(isVarying)[isVarying]

    if (length(varyingStudy) > 0)
      .quitAnalysis(gettextf(
        "The variable '%1$s' is not constant within the study '%2$s'. The predictor and the subgroup must be study-level variables.",
        .nmaVariableName(variable), varyingStudy[1]
      ))
  }

  return()
}
