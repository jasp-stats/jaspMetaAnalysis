# Forest plot data builders.
#
# This file contains the content-building pipeline: study-level data extraction
# (base data, CIs, transforms, ordering, aggregation), additional-section state
# management (row-by-row accumulation of text/diamond/interval rows), and the
# concrete section builders for estimated marginal means and model information.
# Panel header helpers and study-variable aggregation utilities live here too.

# ── Study data extraction ─────────────────────────────────────────────────────

.forestPlotStudyExtractBaseData        <- function(fit, dataset, options) {

  if (.maIsClassical(options)) {

    if (options[["analysis"]] %in% c("generalizedMetaAnalysis", "mantelHaenszelPeto")) {
      studyWeights <- 1 / fit[["vi"]]
    } else if (.maIsMultilevelMultivariate(options)) {
      studyWeights <- stats::weights(fit, type = "diagonal")
    } else {
      studyWeights <- stats::weights(fit)
    }

    return(data.frame(
      effectSize     = fit[["yi"]],
      standardError  = sqrt(fit[["vi"]]),
      weights        = studyWeights,
      id             = seq_along(fit[["yi"]])
    ))
  }

  if (options[["analysis"]] == "BiBMA") {
    tempDf <- metafor::escalc(
      measure = "OR",
      ai      = dataset[[options[["successesGroup1"]]]],
      n1i     = dataset[[options[["sampleSizeGroup1"]]]],
      ci      = dataset[[options[["successesGroup2"]]]],
      n2i     = dataset[[options[["sampleSizeGroup2"]]]]
    )

    return(data.frame(
      effectSize     = tempDf[["yi"]],
      standardError  = sqrt(tempDf[["vi"]]),
      weights        = dataset[[options[["sampleSizeGroup1"]]]] + dataset[[options[["sampleSizeGroup2"]]]],
      id             = seq_len(nrow(dataset))
    ))
  }

  return(data.frame(
    effectSize     = dataset[[options[["effectSize"]]]],
    standardError  = dataset[[options[["effectSizeStandardError"]]]],
    weights        = 1 / dataset[[options[["effectSizeStandardError"]]]]^2,
    id             = seq_len(nrow(dataset))
  ))
}

# Confidence intervals are derived on the current effect-size scale and only
# transformed afterwards if the plot itself is transformed.
.forestPlotStudyAddConfidenceIntervals <- function(dfForest, options) {

  ciZValue <- qnorm((1 - options[["confidenceIntervalsLevel"]]) / 2, lower.tail = FALSE)
  dfForest$lCi <- dfForest$effectSize - ciZValue * dfForest$standardError
  dfForest$uCi <- dfForest$effectSize + ciZValue * dfForest$standardError

  if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
    secondaryCiZValue <- qnorm((1 - options[["forestPlotStudyInformationSecondaryConfidenceIntervalLevel"]]) / 2, lower.tail = FALSE)
    dfForest$lCi2 <- dfForest$effectSize - secondaryCiZValue * dfForest$standardError
    dfForest$uCi2 <- dfForest$effectSize + secondaryCiZValue * dfForest$standardError
  }

  return(dfForest)
}
.forestPlotStudyTransformationColumns  <- function(options) {

  transformColumns <- c("effectSize", "lCi", "uCi")
  if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
    transformColumns <- c(transformColumns, "lCi2", "uCi2")
  }

  return(transformColumns)
}
.forestPlotStudyTransformEffectSizes   <- function(dfForest, options) {

  if (options[["transformEffectSize"]] == "none") {
    return(dfForest)
  }

  transformColumns <- .forestPlotStudyTransformationColumns(options)
  dfForest[, transformColumns] <- do.call(
    .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
    list(dfForest[, transformColumns])
  )

  return(dfForest)
}

# Collect every dataset column that later steps may need so the study data can
# be ordered, aggregated, or used for color/shape mappings.
.forestPlotStudyAdditionalVariables    <- function(options) {

  return(c(
    if (length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) {
      unlist(options[["forestPlotStudyInformationSelectedVariables"]])
    },
    if (options[["forestPlotStudyInformationOrderBy"]] != "") {
      options[["forestPlotStudyInformationOrderBy"]]
    },
    if (options[["forestPlotStudyInformationAggregateBy"]] != "") {
      options[["forestPlotStudyInformationAggregateBy"]]
    },
    if (options[["forestPlotMappingColor"]] != "") {
      options[["forestPlotMappingColor"]]
    },
    if (options[["forestPlotMappingShape"]] != "") {
      options[["forestPlotMappingShape"]]
    }
  ))
}
.forestPlotStudyBindAdditionalVariables <- function(dfForest, dataset, additionalVariables) {

  if (length(additionalVariables) == 0) {
    return(dfForest)
  }

  return(cbind(dfForest, dataset[, additionalVariables, drop = FALSE]))
}
.forestPlotStudyAggregateData          <- function(dfForest, options, additionalVariables) {

  if (options[["forestPlotStudyInformationAggregateBy"]] == "") {
    return(list(
      forest = dfForest,
      geoms  = NULL
    ))
  }

  return(.forestStudyInformationAggregate(dfForest, options, additionalVariables))
}
.forestPlotStudyOrderData              <- function(dfForest, options) {

  if (options[["forestPlotStudyInformationOrderBy"]] == "") {
    return(dfForest)
  }

  # "Ascending" means values increase top-to-bottom (forest plot convention),
  # which corresponds to decreasing = TRUE in R's order() (row 1 = top of plot).
  dfForest <- dfForest[order(
    dfForest[[options[["forestPlotStudyInformationOrderBy"]]]],
    decreasing = options[["forestPlotStudyInformationOrderAscending"]]
  ), ]

  return(dfForest)
}

# Coordinates are assigned only after ordering/aggregation so every downstream
# panel can assume y already encodes the final row order.
.forestPlotStudyAssignCoordinates      <- function(dfForest, dfGeoms = NULL) {

  dfForest$y <- seq(nrow(dfForest))
  if (!is.null(dfGeoms)) {
    dfGeoms <- merge(dfGeoms, dfForest[, colnames(dfForest) %in% c("id", "y")], by = "id")
  }

  return(list(
    forest = dfForest,
    geoms  = dfGeoms
  ))
}

# Merge back the non-geometry study columns so predicted-effect diamonds retain
# the same auxiliary fields available on the original study rows.
.forestPlotStudyPredictionMergeData    <- function(dfForest) {

  return(dfForest[, !colnames(dfForest) %in% c("effectSize", "standardError", "weights", "lCi", "uCi", "y"), drop = FALSE])
}
.forestPlotStudyBuildPredictions       <- function(fit, dfForest, options) {

  if (!options[["forestPlotStudyInformationPredictedEffects"]]) {
    return(NULL)
  }

  dfForestPrediction <- data.frame(predict(fit))

  if (nrow(dfForestPrediction) == 1) {
    dfForestPrediction <- do.call(rbind, replicate(nrow(dfForest), dfForestPrediction, simplify = FALSE))
  }

  dfForestPrediction$id <- dfForest$id
  dfForestPrediction$y  <- dfForest$y

  dfForestPrediction <- do.call(rbind, lapply(seq_len(nrow(dfForestPrediction)), function(i) {
    with(dfForestPrediction[i, ], .maMakeDiamondDataFrame(est = pred, lCi = pi.lb, uCi = pi.ub, row = y, id = id))
  }))
  dfForestPrediction <- merge(dfForestPrediction, .forestPlotStudyPredictionMergeData(dfForest), by = "id")

  if (options[["transformEffectSize"]] != "none") {
    dfForestPrediction[, "x"] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(dfForestPrediction[, "x"])
    )
  }

  return(dfForestPrediction)
}

# Build the study section once and return the normalized forest/object payload
# expected by the later layout stage.
.forestPlotBuildStudyInformation       <- function(fit, options){

  if (is.null(fit) || jaspBase::isTryError(fit)) {
    return(NULL)
  }

  # The fit carries the filtered dataset that actually entered the analysis, so
  # the forest plot stays aligned with model-side NA handling and subgroup fits.
  dataset <- attr(fit, "dataset")

  additionalVariables <- .forestPlotStudyAdditionalVariables(options)
  dfForest            <- .forestPlotStudyExtractBaseData(fit, dataset, options)
  dfForest            <- .forestPlotStudyAddConfidenceIntervals(dfForest, options)
  dfForest            <- .forestPlotStudyTransformEffectSizes(dfForest, options)
  dfForest            <- .forestPlotStudyBindAdditionalVariables(dfForest, dataset, additionalVariables)

  dfAggregate         <- .forestPlotStudyAggregateData(dfForest, options, additionalVariables)
  dfForest            <- .forestPlotStudyOrderData(dfAggregate[["forest"]], options)
  dfCoordinates       <- .forestPlotStudyAssignCoordinates(dfForest, dfAggregate[["geoms"]])
  dfForest            <- dfCoordinates[["forest"]]
  dfGeoms             <- dfCoordinates[["geoms"]]
  dfForestPrediction  <- .forestPlotStudyBuildPredictions(fit, dfForest, options)

  return(list(
    forest     = dfForest,
    prediction = dfForestPrediction,
    geoms      = dfGeoms
  ))
}

# ── Additional section state management ───────────────────────────────────────

.forestPlotCreateAdditionalSectionState <- function() {

  return(list(
    row         = 1,
    information = list(),
    objects     = list()
  ))
}

# Common text-row record used by all additional sections.
.forestPlotCreateAdditionalRow          <- function(label, row, est = NA, lCi = NA, uCi = NA, test = "", face = NA) {

  return(data.frame(
    "label" = label,
    "y"     = row,
    "est"   = est,
    "lCi"   = lCi,
    "uCi"   = uCi,
    "test"  = test,
    "face"  = face
  ))
}
.forestPlotAppendAdditionalRow         <- function(state, label, est = NA, lCi = NA, uCi = NA, test = "", face = NA) {

  row <- state[["row"]]
  state[["information"]][[row]] <- .forestPlotCreateAdditionalRow(
    label = label,
    row   = row,
    est   = est,
    lCi   = lCi,
    uCi   = uCi,
    test  = test,
    face  = face
  )
  state[["row"]] <- row + 1

  return(state)
}

# Additional-section geometry helpers append either a diamond estimate or a
# rectangle-only interval row at the current state$row position.
.forestPlotAppendAdditionalDiamond     <- function(state, label, est, lCi, uCi, test = "", face = NA, mapColor = NA) {

  row <- state[["row"]]
  state[["information"]][[row]] <- .forestPlotCreateAdditionalRow(
    label = label,
    row   = row,
    est   = est,
    lCi   = lCi,
    uCi   = uCi,
    test  = test,
    face  = face
  )
  state[["objects"]][[row]] <- .maMakeDiamondDataFrame(
    est = est,
    lCi = lCi,
    uCi = uCi,
    row = row,
    id  = row
  )
  state[["objects"]][[row]]$mapColor <- mapColor
  state[["row"]] <- row + 1

  return(state)
}
.forestPlotAppendAdditionalInterval    <- function(state, label = NA, lCi = NA, uCi = NA, test = "", face = NA, drawRectangle = TRUE, mapColor = NA) {

  row <- state[["row"]]
  state[["information"]][[row]] <- .forestPlotCreateAdditionalRow(
    label = label,
    row   = row,
    est   = NA,
    lCi   = lCi,
    uCi   = uCi,
    test  = test,
    face  = face
  )

  if (drawRectangle) {
    state[["objects"]][[row]] <- .maMakeRectangleDataFrame(
      lCi = lCi,
      uCi = uCi,
      row = row,
      id  = row
    )
    state[["objects"]][[row]]$mapColor <- mapColor
  }

  state[["row"]] <- row + 1

  return(state)
}
.forestPlotAppendAdditionalSpacer      <- function(state) {

  state[["row"]] <- state[["row"]] + 1

  return(state)
}
.forestPlotResolveTestPlacement       <- function(showTest, options, allowBelow = TRUE) {

  # Forest-plot tests can be rendered inline with the label, in the right panel,
  # or on a separate row below when prediction intervals already occupy a second row.
  placeRight <- showTest && options[["forestPlotTestsInRightPanel"]]
  placeBelow <- showTest && allowBelow && !options[["forestPlotTestsInRightPanel"]] && options[["forestPlotPredictionIntervals"]]

  return(list(
    right = placeRight,
    below = placeBelow,
    left  = showTest && !placeRight && !placeBelow
  ))
}
.forestPlotApplyLeftTestLabel         <- function(label, testText, testPlacement) {
  return(if (isTRUE(testPlacement[["left"]])) paste0(label, ": ", testText) else label)
}
.forestPlotApplyRightTestLabel        <- function(testText, testPlacement) {
  return(if (isTRUE(testPlacement[["right"]])) testText else "")
}
.forestPlotAppendBelowTestRow         <- function(state, testText, testPlacement) {

  if (!isTRUE(testPlacement[["below"]])) {
    return(state)
  }

  return(.forestPlotAppendAdditionalRow(
    state = state,
    label = testText
  ))
}
.forestPlotAppendEstimateWithInterval <- function(state, label, estimate, testText, testPlacement,
                                                  showPredictionIntervals, drawRectangle = showPredictionIntervals,
                                                  face = NA, mapColor = NA) {

  # Additional sections reuse the same visual pattern: one estimate row plus an
  # optional second row for prediction intervals and/or below-label test text.
  state <- .forestPlotAppendAdditionalDiamond(
    state    = state,
    label    = .forestPlotApplyLeftTestLabel(label, testText, testPlacement),
    est      = estimate[["est"]],
    lCi      = estimate[["lCi"]],
    uCi      = estimate[["uCi"]],
    test     = .forestPlotApplyRightTestLabel(testText, testPlacement),
    face     = face,
    mapColor = mapColor
  )

  if (isTRUE(testPlacement[["below"]]) || showPredictionIntervals) {
    state <- .forestPlotAppendAdditionalInterval(
      state         = state,
      label         = if (isTRUE(testPlacement[["below"]])) testText else NA,
      lCi           = if (showPredictionIntervals) estimate[["lPi"]] else NA,
      uCi           = if (showPredictionIntervals) estimate[["uPi"]] else NA,
      drawRectangle = drawRectangle,
      mapColor      = mapColor
    )
  }

  return(state)
}

# Collapse the row-wise builder state to plain data frames for the layout stage.
.forestPlotFinalizeAdditionalSectionState <- function(state) {

  return(list(
    information = .forestPlotBindDataFrames(state[["information"]]),
    objects     = .forestPlotBindDataFrames(state[["objects"]])
  ))
}

# ── Estimated marginal means builder ──────────────────────────────────────────

# Shared guard: skip on NULL/error fit and disable prediction intervals for
# complex or Bayesian models that don't support them.
.forestPlotAdditionalSectionPreamble <- function(fit, options) {

  if (is.null(fit) || jaspBase::isTryError(fit)) {
    return(NULL)
  }

  if (.mammHasMultipleHeterogeneities(options)) {
    options[["predictionIntervals"]]           <- FALSE
    options[["forestPlotPredictionIntervals"]] <- FALSE
  }

  return(options)
}

.forestPlotBuildEstimatedMarginalMeans <- function(fit, options){

  options <- .forestPlotAdditionalSectionPreamble(fit, options)
  if (is.null(options)) {
    return(NULL)
  }

  if (!.maIsClassical(options)) {
    options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] <- options[["forestPlotEstimatedMarginalMeansCoefficientTestsAgainst0"]]
    options[["predictionIntervals"]]           <- FALSE
    options[["forestPlotPredictionIntervals"]] <- FALSE

    # disable tests when no averaging is performed
    if (!(options[["bayesianModelAveragingModerations"]] || options[["bayesianModelAveragingEffectSize"]])) {
      options[["forestPlotEstimatedMarginalMeansTermTests"]]        <- FALSE
      options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] <- FALSE
    }
  }


  state <- .forestPlotCreateAdditionalSectionState()

  classical                            <- .maIsClassical(options)
  fitWrapper                           <- list(fit = fit)
  estimatedMarginalMeansTestsStatistics <- options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue"
  estimatedMarginalMeansVariables      <- unlist(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]])
  termTestPlacement                    <- .forestPlotResolveTestPlacement(
    options[["forestPlotEstimatedMarginalMeansTermTests"]],
    options,
    allowBelow = FALSE
  )
  coefficientTestPlacement             <- .forestPlotResolveTestPlacement(
    options[["forestPlotEstimatedMarginalMeansCoefficientTests"]],
    options
  )

  # add marginal estimates
  for (variable in estimatedMarginalMeansVariables) {

    if (classical) {
      tempTermTest               <- .maTermTests(fit, options, variable)
      tempEstimatedMarginalMeans <- .maComputeMarginalMeansVariable(
        fit,
        options,
        variable,
        options[["forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"]],
        "effectSize"
      )
      tempTestText               <- .maPrintTermTest(tempTermTest, estimatedMarginalMeansTestsStatistics)
    } else {
      tempTermTest               <- .robmaTermTests(fit, options, variable)
      tempEstimatedMarginalMeans <- .robmaComputeMarginalMeansVariable(
        fitWrapper,
        options,
        variable,
        conditional = options[["forestPlotConditionalEstimates"]]
      )
      tempTestText               <- .robmaPrintBfTest(tempTermTest, options)
    }


    # add term information
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .forestPlotApplyLeftTestLabel(variable, tempTestText, termTestPlacement),
      test  = .forestPlotApplyRightTestLabel(tempTestText, termTestPlacement)
    )

    # add levels information
    for (j in seq_len(nrow(tempEstimatedMarginalMeans))) {

      if (classical) {
        tempCoefficientTest <- .maPrintCoefficientTest(tempEstimatedMarginalMeans[j,], estimatedMarginalMeansTestsStatistics)
      } else {
        tempCoefficientTest <- .robmaPrintBfTest(tempEstimatedMarginalMeans[j,], options)
      }

      state <- .forestPlotAppendEstimateWithInterval(
        state                   = state,
        label                   = tempEstimatedMarginalMeans$value[j],
        estimate                = tempEstimatedMarginalMeans[j, ],
        testText                = tempCoefficientTest,
        testPlacement           = coefficientTestPlacement,
        showPredictionIntervals = options[["forestPlotPredictionIntervals"]],
        face                    = "italic",
        mapColor                = if (options[["forestPlotMappingColor"]] == variable) tempEstimatedMarginalMeans$value[j] else NA
      )
    }

    # add empty row
    state <- .forestPlotAppendAdditionalSpacer(state)
  }

  # add adjusted effect size estimate
  if (options[["forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"]]) {

    if (classical) {
      tempEstimatedMarginalMeans <- .maComputeMarginalMeansVariable(
        fit,
        options,
        "",
        options[["forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"]],
        "effectSize"
      )
      tempCoefficientTest <- .maPrintCoefficientTest(tempEstimatedMarginalMeans, estimatedMarginalMeansTestsStatistics)
    } else {
      tempEstimatedMarginalMeans <- .robmaComputeMarginalMeansVariable(
        fitWrapper,
        options,
        "intercept",
        conditional = options[["forestPlotConditionalEstimates"]]
      )
      tempCoefficientTest <- .robmaPrintBfTest(tempEstimatedMarginalMeans[1,], options)
    }

    state <- .forestPlotAppendEstimateWithInterval(
      state                   = state,
      label                   = gettext("Adjusted estimate"),
      estimate                = tempEstimatedMarginalMeans,
      testText                = tempCoefficientTest,
      testPlacement           = coefficientTestPlacement,
      showPredictionIntervals = options[["forestPlotPredictionIntervals"]]
    )
  }

  return(.forestPlotFinalizeAdditionalSectionState(state))
}

# ── Model information builder ─────────────────────────────────────────────────

.forestPlotBuildModelInformation       <- function(fit, options){

  options <- .forestPlotAdditionalSectionPreamble(fit, options)
  if (is.null(options)) {
    return(NULL)
  }

  if (!any(unlist(options[c(
    "forestPlotEffectSizeFixedEffectEstimate",
    "forestPlotEffectSizeFixedEffectTest",
    "forestPlotEffectSizePooledEstimate",
    "forestPlotEffectSizePooledEstimateTest",
    "forestPlotEffectSizeModerationTest",
    "forestPlotHeterogeneityTest",
    "forestPlotHeterogeneityTestWald",
    "forestPlotHeterogeneityTestLRT",
    "forestPlotHeterogeneityEstimateTau",
    "forestPlotHeterogeneityEstimateTau2",
    "forestPlotHeterogeneityEstimateI2",
    "forestPlotHeterogeneityEstimateH2",
    "forestPlotHeterogeneityModerationTest"
  )])))
    return(NULL)

  classical                   <- .maIsClassical(options)
  standardClassical           <- .maIsClassical(options, notMHP = TRUE)
  method                      <- .maGetMethodOptions(options)
  randomEffectsMethod         <- !method %in% c("FE", "EE", "MH", "PETO")
  mantelHaenszelMethod        <- method %in% c("MH", "PETO")
  heterogeneityMetaregression <- .maIsMetaregressionHeterogeneity(options)
  scaleRegression             <- options[["analysis"]] == "metaAnalysis" && heterogeneityMetaregression
  testsStatistics             <- options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue"

  state <- .forestPlotCreateAdditionalSectionState()

  if (isTRUE(options[["forestPlotHeterogeneityTest"]]) && ((classical && !.maIsGLMM(options)) || isTRUE(options[["bayesianModelAveragingHeterogeneity"]]))) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (classical) .maPrintQTest(fit) else .robmaPrintTest(fit, options, "heterogeneity")
    )
  }

  if (isTRUE(options[["forestPlotHeterogeneityTestWald"]]) && .maIsGLMM(options)) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .maPrintQTest(fit, type = "Wald")
    )
  }

  if (isTRUE(options[["forestPlotHeterogeneityTestLRT"]]) && .maIsGLMM(options)) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .maPrintQTest(fit, type = "LRT")
    )
  }

  if (randomEffectsMethod && options[["forestPlotHeterogeneityEstimateTau"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (classical) .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau")
        else .robmaPrintPooledEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau", conditional = options[["forestPlotConditionalEstimates"]])
    )
  }

  if (randomEffectsMethod && options[["forestPlotHeterogeneityEstimateTau2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (classical) .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau2")
        else .robmaPrintPooledEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau2", conditional = options[["forestPlotConditionalEstimates"]])
    )
  }

  if (randomEffectsMethod && !heterogeneityMetaregression && options[["forestPlotHeterogeneityEstimateI2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (classical) .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "I2")
        else .robmaPrintPooledEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "I2", conditional = options[["forestPlotConditionalEstimates"]])
    )
  }

  if (mantelHaenszelMethod && options[["forestPlotHeterogeneityEstimateI2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .mamhpPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "I2")
    )
  }

  if (randomEffectsMethod && !heterogeneityMetaregression && options[["forestPlotHeterogeneityEstimateH2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (classical) .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "H2")
        else .robmaPrintPooledEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "H2", conditional = options[["forestPlotConditionalEstimates"]])
    )
  }

  if (mantelHaenszelMethod && options[["forestPlotHeterogeneityEstimateH2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .mamhpPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "H2")
    )
  }

  if (.maIsMetaregressionEffectSize(options) && options[["forestPlotEffectSizeModerationTest"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .maPrintModerationTest(fit, options, par = "effectSize")
    )
  }

  if (.maIsMetaregressionHeterogeneity(options) && options[["forestPlotHeterogeneityModerationTest"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .maPrintModerationTest(fit, options, par = "heterogeneity")
    )
  }

  if (options[["forestPlotPublicationBiasTest"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = gettextf("Publication bias: %1$s", .robmaPrintTest(fit, options, "bias", includeName = FALSE))
    )
  }

  if (
    standardClassical &&
    randomEffectsMethod &&
    !scaleRegression &&
    options[["forestPlotEffectSizeFixedEffectEstimate"]]
  ) {

    fixedEffectTestPlacement <- .forestPlotResolveTestPlacement(
      options[["forestPlotEffectSizeFixedEffectTest"]],
      options
    )

    tempPooledEstimate <- try(.maComputePooledEffectPlot(fit, options, forceFixed = TRUE))
    if (jaspBase::isTryError(tempPooledEstimate)) {
      stop(gettext("The fixed effect effect size could not be calculated."))
    }
    tempTestText       <- .maPrintCoefficientTest(tempPooledEstimate, testsStatistics)

    state <- .forestPlotAppendAdditionalDiamond(
      state = state,
      label = .forestPlotApplyLeftTestLabel(gettext("Fixed effect estimate"), tempTestText, fixedEffectTestPlacement),
      est   = tempPooledEstimate$est,
      lCi   = tempPooledEstimate$lCi,
      uCi   = tempPooledEstimate$uCi,
      test  = .forestPlotApplyRightTestLabel(tempTestText, fixedEffectTestPlacement)
    )
    state <- .forestPlotAppendBelowTestRow(state, tempTestText, fixedEffectTestPlacement)
  }

  if (classical && options[["forestPlotEffectSizePooledEstimate"]]) {

    pooledEffectTestPlacement <- .forestPlotResolveTestPlacement(
      options[["forestPlotEffectSizePooledEstimateTest"]],
      options
    )

    effectSizeName     <- gettext("Pooled effect")
    tempPooledEstimate <- try(.maComputePooledEffectPlot(fit, options))
    if (jaspBase::isTryError(tempPooledEstimate)) {
      stop(gettext("The pooled effect size could not be calculated."))
    }
    tempTestText       <- .maPrintCoefficientTest(tempPooledEstimate, testsStatistics)

    state <- .forestPlotAppendEstimateWithInterval(
      state                   = state,
      label                   = effectSizeName,
      estimate                = tempPooledEstimate,
      testText                = tempTestText,
      testPlacement           = pooledEffectTestPlacement,
      showPredictionIntervals = options[["forestPlotPredictionIntervals"]]
    )
  }

  if (!classical && options[["forestPlotEffectSizePooledEstimate"]]) {

    pooledEffectTestPlacement <- .forestPlotResolveTestPlacement(
      options[["bayesianModelAveragingEffectSize"]] && options[["forestPlotEffectSizePooledEstimateTest"]],
      options
    )

    effectSizeName         <- gettext("Pooled effect")
    tempPooledEstimate     <- .robmaComputePooledEffect(fit, options, conditional = options[["forestPlotConditionalEstimates"]])
    tempPooledEstimate$est <- tempPooledEstimate$mean
    tempTestText           <- .robmaPrintTest(fit, options, "effect", includeName = FALSE)

    if (!.maIsMetaregression(options)) {

      # only in nonmeta-regression models the pooled effect size matches the overall test
      state <- .forestPlotAppendEstimateWithInterval(
        state                   = state,
        label                   = effectSizeName,
        estimate                = tempPooledEstimate,
        testText                = tempTestText,
        testPlacement           = pooledEffectTestPlacement,
        showPredictionIntervals = options[["forestPlotPredictionIntervals"]]
      )

    } else {


      # only in nonmeta-regression models the pooled effect size matches the overall test
      state <- .forestPlotAppendEstimateWithInterval(
        state                   = state,
        label                   = effectSizeName,
        estimate                = tempPooledEstimate,
        testText                = "",
        testPlacement           = list(right = FALSE, below = FALSE, left = FALSE),
        showPredictionIntervals = options[["forestPlotPredictionIntervals"]]
      )

      if (options[["forestPlotEffectSizePooledEstimateTest"]]) {

        # add adjusted effect size for meta-regression since they match the meta-analytic test
        if (.robmaIsMetaregressionCentered(options)) {
          tempTestEstimate     <- .robmaComputeAdjustedEffect(fit, options, conditional = options[["forestPlotConditionalEstimates"]])
          tempTestEstimate$est <- tempPooledEstimate$mean
          effectSizeName       <- gettext("Adjusted estimate")
        } else {
          tempTestEstimate     <- .robmaComputeInterceptEffect(fit, options, conditional = options[["forestPlotConditionalEstimates"]])
          tempTestEstimate$est <- tempPooledEstimate$mean
          effectSizeName       <- gettext("Intercept estimate")
        }

        state <- .forestPlotAppendEstimateWithInterval(
          state                   = state,
          label                   = effectSizeName,
          estimate                = tempTestEstimate,
          testText                = tempTestText,
          testPlacement           = pooledEffectTestPlacement,
          showPredictionIntervals = options[["forestPlotPredictionIntervals"]],
          drawRectangle           = options[["forestPlotPredictionIntervals"]] && .robmaIsMetaregressionCentered(options)
        )
      }

    }
  }


  return(.forestPlotFinalizeAdditionalSectionState(state))
}

# ── Left panel header helpers ─────────────────────────────────────────────────

.forestPlotInformationSettings         <- function(options, key) {

  settings <- options[[key]]
  if (length(settings) == 0) {
    return(data.frame(
      title     = character(0),
      value     = character(0),
      width     = numeric(0),
      alignment = character(0)
    ))
  }

  return(do.call(rbind.data.frame, settings))
}
.forestPlotStudyInformationSettings    <- function(options) {
  return(.forestPlotInformationSettings(options, "forestPlotStudyInformationSelectedVariablesSettings"))
}
.forestPlotEstimateInformationSettings <- function(options) {
  return(.forestPlotInformationSettings(options, "forestPlotEstimateInformationSelectedVariablesSettings"))
}
.forestPlotStudyInformationColumnWidths <- function(studyInformation, forestInformation, options) {

  if (nrow(studyInformation) == 0) {
    return(numeric(0))
  }

  valueWidths <- vapply(studyInformation$value, function(variable) {
    variableValues <- forestInformation[[variable]]
    variableValues <- ifelse(is.na(variableValues), "", as.character(variableValues))
    widths <- .forestPlotMeasureTextWidthMm(variableValues, options)
    if (length(widths) == 0) {
      return(0)
    }

    return(max(widths, na.rm = TRUE))
  }, numeric(1))

  titleWidths <- .forestPlotMeasureTextWidthMm(studyInformation$title, options, fontface = "bold")
  return(pmax(titleWidths, valueWidths) + .forestPlotInformationColumnGutterMm(options))
}
.forestPlotInformationColumnGutterMm  <- function(options) {
  return(6 * options[["forestPlotSizeText"]])
}
.forestPlotStudyInformationRelativeWidths <- function(studyInformation, studyInformationWidths) {

  if (nrow(studyInformation) == 0) {
    return(numeric(0))
  }

  relativeWidths <- studyInformationWidths * studyInformation$width
  relativeWidths[!is.finite(relativeWidths) | relativeWidths <= 0] <- 0
  if (sum(relativeWidths) <= 0) {
    relativeWidths <- studyInformation$width
    relativeWidths[!is.finite(relativeWidths) | relativeWidths <= 0] <- 1
  }

  return(relativeWidths / sum(relativeWidths))
}
.forestPlotBuildStudyInformationHeader <- function(options, forestInformation, additionalInformation) {

  leftPanelStudyInformation <- .forestPlotStudyInformationSettings(options)

  if (options[["forestPlotStudyInformation"]] && nrow(leftPanelStudyInformation) > 0) {
    leftPanelStudyInformationWidths <- .forestPlotStudyInformationColumnWidths(
      leftPanelStudyInformation,
      forestInformation,
      options
    )
    maxPanelWidth <- sum(leftPanelStudyInformationWidths)
  } else {
    leftPanelStudyInformationWidths <- 0
    maxPanelWidth <- 0
  }
  if (.forestPlotHasDataFrame(additionalInformation)) {
    additionalInformationWidths <- .forestPlotMeasureTextWidthMm(additionalInformation$label, options)
    if (length(additionalInformationWidths) > 0) {
      maxPanelWidth <- max(c(maxPanelWidth, additionalInformationWidths), na.rm = TRUE)
    }
  }

  if (nrow(leftPanelStudyInformation) > 0) {
    # Use the same no-device text estimate for internal columns and outer panel
    # sizing, so adjacent columns have real space between rendered labels.
    leftPanelRelativeWidths <- .forestPlotStudyInformationRelativeWidths(
      leftPanelStudyInformation,
      leftPanelStudyInformationWidths
    )
    leftPanelStudyInformation$xStart <- c(0, cumsum(leftPanelRelativeWidths[-length(leftPanelRelativeWidths)]))
    leftPanelStudyInformation$xEnd   <- cumsum(leftPanelRelativeWidths)
  }

  attr(leftPanelStudyInformation, "widthMm") <- maxPanelWidth
  return(leftPanelStudyInformation)
}
.forestPlotHasStudyInformationHeader   <- function(options) {

  leftPanelStudyInformation <- .forestPlotStudyInformationSettings(options)

  return(nrow(leftPanelStudyInformation) > 0 && any(leftPanelStudyInformation[["title"]] != ""))
}
.forestPlotHasEstimateInformationHeader <- function(options) {

  estimateSettings <- .forestPlotEstimateInformationSettings(options)

  return(nrow(estimateSettings) > 0 && any(estimateSettings[["title"]] != ""))
}

# Decide whether the right panel is needed at all before building/laying it out.
.forestPlotHasRightPanel               <- function(options, additionalInformation) {

  hasAdditionalInformation <- .forestPlotHasDataFrame(additionalInformation)

  if (!options[["forestPlotStudyInformation"]] && !hasAdditionalInformation) {
    return(FALSE)
  }
  if (options[["forestPlotEstimatesAndConfidenceIntervals"]]) {
    return(TRUE)
  }
  if (options[["forestPlotStudyInformation"]] && options[["forestPlotStudyInformationStudyWeights"]]) {
    return(TRUE)
  }
  if (hasAdditionalInformation &&
      (options[["forestPlotEstimatedMarginalMeansTermTests"]] || options[["forestPlotEstimatedMarginalMeansCoefficientTests"]]) &&
      options[["forestPlotTestsInRightPanel"]]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Heading-row constructors keep subgroup and section-title rows in the same
# schema as other left-panel text rows.
.forestPlotSubgroupHeading             <- function(options, subgroup, tempRow) {

  return(data.frame(
    "label"  = if (subgroup == gettext("Full dataset")) gettext("Full dataset") else gettextf("Subgroup: %1$s", subgroup),
    "y"      = tempRow,
    "est"    = NA,
    "lCi"    = NA,
    "uCi"    = NA,
    "test"   = "",
    "face"   = "bold"
  ))
}
.forestPlotPanelHeading                <- function(panel, tempRow) {

  return(data.frame(
    "label"  = panel,
    "y"      = tempRow,
    "est"    = NA,
    "lCi"    = NA,
    "uCi"    = NA,
    "test"   = "",
    "face"   = "bold"
  ))
}

# Left-panel alignment is shared between x placement and hjust.
.forestPlotLeftPanelAlign              <- function(options) {
  return(switch(
    options[["forestPlotAlignLeftPanel"]],
    "left"   = 0,
    "middle" = 0.5,
    "right"  = 1
  ))
}

# ── Aggregation helpers ───────────────────────────────────────────────────────

.forestPlotAggregateVariable           <- function(x) {
  if (length(unique(x)) == 1) {
    return(unique(x))
  } else {
    x <- table(x)
    x <- x[x > 0]
    xNames <- names(x)
    xFreqs <- paste0(" (", x, ")")
    xFreqs[xFreqs == " (1)"] <- ""
    return(paste0(xNames, xFreqs, collapse = ", "))
  }
}
.forestPlotColorKeyColumn             <- function() {
  return(".forestPlotColorKey")
}
.forestPlotAggregateColorValue        <- function(x) {

  x <- as.character(x)
  if (any(is.na(x) | x == "")) {
    return(NA_character_)
  }

  x <- unique(x)
  if (length(x) != 1) {
    return(NA_character_)
  }

  return(x)
}
.forestPlotAddAggregateColorKey       <- function(dfOut, df, options, method) {

  colorVar <- options[["forestPlotMappingColor"]]
  if (colorVar == "" || !colorVar %in% colnames(df)) {
    return(dfOut)
  }

  if (method == "boxplot") {
    dfOut[[.forestPlotColorKeyColumn()]] <- .forestPlotAggregateColorValue(df[[colorVar]])
  } else if (method == "bubbles") {
    dfOut[[.forestPlotColorKeyColumn()]] <- as.character(df[[colorVar]])
  }

  return(dfOut)
}
.forestPlotAggregateEffectSizeSummary  <- function(effectSize) {

  effectSize <- effectSize[is.finite(effectSize)]
  if (length(effectSize) == 0) {
    return(list(
      min    = NA_real_,
      lower  = NA_real_,
      middle = NA_real_,
      upper  = NA_real_,
      max    = NA_real_
    ))
  }

  return(list(
    min    = min(effectSize),
    lower  = stats::quantile(effectSize, 0.25, names = FALSE),
    middle = stats::median(effectSize),
    upper  = stats::quantile(effectSize, 0.75, names = FALSE),
    max    = max(effectSize)
  ))
}

# Aggregation re-expresses study rows either as one boxplot summary per group or
# as a group label plus one bubble per contributing study.
.forestStudyInformationAggregate       <- function(dfForest, options, additionalVariables) {

  # split the data set by the grouping variable
  datasetSplit <- split(dfForest, dfForest[[options[["forestPlotStudyInformationAggregateBy"]]]])

  # add id to each split index
  for (i in seq_along(datasetSplit)) {
    datasetSplit[[i]]$id <- i
  }

  if (options[["forestPlotStudyInformationAggregateMethod"]] == "boxplot") {

    datasetAggregated <- do.call(rbind, lapply(datasetSplit, function(df) {

      effectSizeSummary <- .forestPlotAggregateEffectSizeSummary(df$effectSize)

      # create a base of the geom
      tempDf <- data.frame(
        id      = df$id[1],
        weights = .forestPlotAggregateWeights(df$weights[is.finite(df$effectSize)]),
        min     = effectSizeSummary[["min"]],
        lower   = effectSizeSummary[["lower"]],
        middle  = effectSizeSummary[["middle"]],
        upper   = effectSizeSummary[["upper"]],
        max     = effectSizeSummary[["max"]],
        geom    = "boxplot"
      )

      # add the additional variables
      for (var in additionalVariables) {
        tempDf[[var]] <- .forestPlotAggregateVariable(df[[var]])
      }
      tempDf <- .forestPlotAddAggregateColorKey(tempDf, df, options, "boxplot")

      return(tempDf)
    }))

    # split into study information and geoms
    dfGeoms  <- datasetAggregated
    dfForest <- datasetAggregated[
      , !colnames(datasetAggregated) %in% c(
        "min", "lower", "middle", "upper", "max", "geom", .forestPlotColorKeyColumn()
      ),
      drop = FALSE
    ]


  } else if (options[["forestPlotStudyInformationAggregateMethod"]] == "bubbles") {

    dfForest <- do.call(rbind, lapply(datasetSplit, function(df) {

      tempDf <- data.frame(
        id      = df$id[1],
        weights = .forestPlotAggregateWeights(df$weights)
      )

      # add the additional variables
      for (var in additionalVariables) {
        tempDf[[var]] <- .forestPlotAggregateVariable(df[[var]])
      }

      return(tempDf)
    }))
    dfGeoms  <- do.call(rbind, lapply(datasetSplit, function(df) {

      # create a base of the geom
      tempDf <- data.frame(
        id      = df$id[1],
        x       = df$effectSize,
        weight  = df$weights,
        geom    = "bubbles"
      )

      # add the additional variables
      for (var in additionalVariables) {
        tempDf[[var]] <- .forestPlotAggregateVariable(df[[var]])
      }
      tempDf <- .forestPlotAddAggregateColorKey(tempDf, df, options, "bubbles")

      return(tempDf)
    }))

  }

  return(list(
    forest = dfForest,
    geoms  = dfGeoms
  ))
}
.forestPlotAggregateWeights           <- function(weights) {

  weights <- weights[!is.na(weights)]
  if (length(weights) == 0)
    return(NA_real_)

  return(sum(weights))
}
