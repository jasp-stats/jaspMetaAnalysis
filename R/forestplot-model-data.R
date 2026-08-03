# Forest plot model-summary data.
#
# Builds additional-section state, estimated marginal means, and model information.

.forestPlotCreateAdditionalSectionState <- function() {

  return(list(
    row         = 1,
    information = list(),
    objects     = list()
  ))
}

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

.forestPlotFinalizeAdditionalSectionState <- function(state) {

  return(list(
    information = .forestPlotBindDataFrames(state[["information"]]),
    objects     = .forestPlotBindDataFrames(state[["objects"]])
  ))
}

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

# Convert fitted-model summaries into normalized rows for the additional
# model-information section of the forest plot.
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
