# Standalone Forest Plot analysis.
#
# Renders a forest plot from user-supplied effect sizes, CIs, and section types
# without fitting any model. Reuses the forest plot rendering pipeline from
# forestplotcommon.R / forestplotdata.R / forestplotscaling.R.

ForestPlot <- function(jaspResults, dataset, options) {

  options[["analysis"]] <- "standaloneForestPlot"
  options              <- .fpStandaloneDefaults(options)

  ready <- .fpStandaloneReady(options)

  if (ready)
    dataset <- .fpStandaloneCheckData(dataset, options)

  .fpStandaloneForestPlot(jaspResults, dataset, options, ready)
}


# ── readiness & data ─────────────────────────────────────────────────────────

.fpStandaloneReady <- function(options) {
  return(options[["effectSize"]] != "")
}

.fpStandaloneCheckData <- function(dataset, options) {

  # record NA counts but keep all rows (users may have text-only rows)
  naRows <- is.na(dataset[[options[["effectSize"]]]])
  attr(dataset, "NAs") <- sum(naRows)

  return(dataset)
}


# ── option defaults (for jaspTools testing; JASP Desktop always provides) ────

.fpStandaloneDefaults <- function(options) {

  # jaspTools::analysisOptions() cannot parse imported QML components, so
  # shared-component options (study info, estimate info, settings) must be
  # provided here for headless testing.  JASP Desktop always supplies them.
  defaults <- list(
    # shared component: study information
    forestPlotStudyInformationSelectedVariables                     = list(),
    forestPlotStudyInformationSelectedVariablesSettings             = list(),
    forestPlotStudyInformationOrderBy                               = "",
    forestPlotStudyInformationOrderAscending                        = FALSE,
    forestPlotStudyInformationAggregateBy                           = "",
    forestPlotStudyInformationAggregateMethod                       = "boxplot",
    forestPlotStudyInformationAggregateMethodBubbleRelativeSize     = 1,
    # shared component: estimate information
    forestPlotEstimateInformationSelectedVariables                  = list(),
    forestPlotEstimateInformationSelectedVariablesSettings          = list(),
    # shared component: settings
    forestPlotEstimatesAndConfidenceIntervals                       = TRUE,
    forestPlotAllignLeftPanel                                       = "right",
    forestPlotTestsInRightPanel                                     = FALSE,
    forestPlotSubgroupShowTitles                                    = TRUE,
    forestPlotAuxiliaryDigits                                       = 2,
    forestPlotAuxiliaryPlotColor                                    = "black",
    forestPlotAuxiliaryAddVerticalLine                              = FALSE,
    forestPlotAuxiliaryAddVerticalLineValue                         = 0,
    forestPlotAuxiliaryAddVerticalLine2                             = FALSE,
    forestPlotAuxiliaryAddVerticalLineValue2                        = 0,
    forestPlotAuxiliaryEffectLabel                                  = "Effect Size",
    forestPlotAuxiliarySetXAxisLimit                                = FALSE,
    forestPlotAuxiliarySetXAxisLimitLower                           = -1,
    forestPlotAuxiliarySetXAxisLimitUpper                           = 1,
    forestPlotAuxiliarySetXAxisTicks                                = FALSE,
    forestPlotAuxiliarySetXAxisTicksValues                          = "-1, -0.5, 0, 0.5, 1",
    forestPlotAuxiliaryXAxisTransformLabelsOnly                     = TRUE,
    forestPlotAuxiliaryAdjustWidthBasedOnText                       = TRUE,
    forestPlotRelativeSizeEstimates                                 = 1,
    forestPlotRelativeSizeText                                      = 1,
    forestPlotRelativeSizeAxisLabels                                = 1,
    forestPlotRelativeSizeRow                                       = 1,
    forestPlotRelativeSizeLeftPanel                                 = 0.5,
    forestPlotRelativeSizeMiddlePanel                               = 1,
    forestPlotRelativeSizeRightPanel                                = 0.5,
    forestPlotMappingColor                                          = "",
    forestPlotMappingShape                                          = "",
    # pipeline options not present in standalone QML
    subgroup                                                       = "",
    weights                                                        = "",
    confidenceIntervals                                            = TRUE,
    confidenceIntervalsLevel                                       = 0.95,
    predictionIntervals                                            = FALSE,
    forestPlotStudyInformation                                     = TRUE,
    forestPlotEstimatedMarginalMeans                                = FALSE,
    forestPlotModelInformation                                     = FALSE,
    forestPlotPredictionIntervals                                  = FALSE,
    forestPlotStudyInformationPredictedEffects                     = FALSE,
    forestPlotStudyInformationStudyWeights                         = FALSE,
    forestPlotStudyInformationSecondaryConfidenceInterval          = FALSE,
    forestPlotStudyInformationSecondaryConfidenceIntervalLevel     = 0.89,
    forestPlotEstimatedMarginalMeansSelectedVariables              = list(),
    forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate     = FALSE,
    forestPlotEstimatedMarginalMeansTermTests                      = FALSE,
    forestPlotEstimatedMarginalMeansCoefficientTests               = FALSE,
    forestPlotEstimatedMarginalMeansCoefficientTestsAgainst        = 0,
    forestPlotEstimatedMarginalMeansCoefficientTestsAgainst0       = FALSE,
    forestPlotSubgroupPanelsWithinSubgroup                         = FALSE,
    forestPlotSubgroupFullDatasetEstimatedMarginalMeans            = FALSE,
    forestPlotSubgroupFullDatasetModelInformation                  = FALSE,
    forestPlotConditionalEstimates                                 = FALSE,
    forestPlotPublicationBiasTest                                  = FALSE,
    forestPlotEffectSizeFixedEffectEstimate                        = FALSE,
    forestPlotEffectSizeFixedEffectTest                            = FALSE,
    forestPlotEffectSizePooledEstimate                             = FALSE,
    forestPlotEffectSizePooledEstimateTest                         = FALSE,
    forestPlotEffectSizeModerationTest                             = FALSE,
    forestPlotHeterogeneityTest                                    = FALSE,
    forestPlotHeterogeneityTestWald                                = FALSE,
    forestPlotHeterogeneityTestLRT                                 = FALSE,
    forestPlotHeterogeneityEstimateTau                             = FALSE,
    forestPlotHeterogeneityEstimateTau2                            = FALSE,
    forestPlotHeterogeneityEstimateI2                              = FALSE,
    forestPlotHeterogeneityEstimateH2                              = FALSE,
    forestPlotHeterogeneityModerationTest                          = FALSE,
    includeFullDatasetInSubgroupAnalysis                           = FALSE,
    bayesianModelAveragingEffectSize                               = FALSE,
    bayesianModelAveragingModerations                              = FALSE,
    bayesianModelAveragingHeterogeneity                            = FALSE
  )

  for (key in names(defaults)) {
    if (is.null(options[[key]]))
      options[[key]] <- defaults[[key]]
  }

  # DropDown with addEmptyValue produces NA; pipeline expects ""
  emptyValueOptions <- c(
    "forestPlotStudyInformationOrderBy",
    "forestPlotStudyInformationAggregateBy",
    "weights",
    "forestPlotMappingColor",
    "forestPlotMappingShape"
  )
  for (key in emptyValueOptions) {
    if (!is.null(options[[key]]) && length(options[[key]]) == 1 && is.na(options[[key]]))
      options[[key]] <- ""
  }

  return(options)
}


# ── forest plot builder ──────────────────────────────────────────────────────

.fpStandaloneDependencies <- c(
  .maForestPlotDependencies,
  "effectSize", "effectSizeStandardError",
  "confidenceInterval", "predictionInterval", "weights",
  "row", "visualizationType",
  "visualizationTypeStudy", "visualizationTypeEstimate",
  "subgroup",
  "forestPlotEstimateInformationSelectedVariables",
  "forestPlotEstimateInformationSelectedVariablesSettings"
)

.fpStandaloneForestPlot <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["forestPlot"]]))
    return()

  if (!ready) {
    forestPlot          <- createJaspPlot(title = gettext("Forest Plot"), width = 500, height = 400)
    forestPlot$position <- 1
    forestPlot$dependOn(.fpStandaloneDependencies)
    jaspResults[["forestPlot"]] <- forestPlot
    return()
  }

  # build plot data from user columns
  plotOut <- try(.fpStandaloneBuildAndRender(dataset, options))

  if (inherits(plotOut, "try-error")) {
    forestPlot            <- createJaspPlot(title = gettext("Forest Plot"))
    forestPlot$position   <- 1
    forestPlot$dependOn(.fpStandaloneDependencies)
    forestPlot$setError(plotOut)
    jaspResults[["forestPlot"]] <- forestPlot
    return()
  }

  height <- 200 + attr(plotOut, "rows") * 10
  if (!attr(plotOut, "isPanel"))
    width <- 500
  else
    width <- 500 + 500 * attr(plotOut, "panelRatio")

  forestPlot            <- createJaspPlot(title = gettext("Forest Plot"), width = width, height = height)
  forestPlot$position   <- 1
  forestPlot$dependOn(.fpStandaloneDependencies)

  if (!attr(plotOut, "isPanel")) {
    forestPlot$plotObject <- plotOut
  } else {
    plotOut <- jaspGraphs:::jaspGraphsPlot$new(
      subplots = plotOut,
      layout   = attr(plotOut, "layout"),
      heights  = 1,
      widths   = attr(plotOut, "widths")
    )
    forestPlot$plotObject <- plotOut
  }

  jaspResults[["forestPlot"]] <- forestPlot
}


# ── data pipeline ────────────────────────────────────────────────────────────

.fpStandaloneBuildAndRender <- function(dataset, options) {

  # disable right panel when no intervals are available
  hasCi <- length(.fpStandaloneGetPairColumns(options, "confidenceInterval")) == 2
  hasSe <- options[["effectSizeStandardError"]] != ""
  if (!hasCi && !hasSe)
    options[["forestPlotEstimatesAndConfidenceIntervals"]] <- FALSE

  if (options[["weights"]] == "")
    options[["forestPlotStudyInformationStudyWeights"]] <- FALSE

  options  <- .forestPlotPrepareOptions(options)
  dataOpts <- .forestPlotPrepareDataOptions(options)

  plotInput <- .fpStandaloneBuildInput(dataset, dataOpts)
  plotData  <- .forestPlotBuildPlotData(plotInput, dataOpts)

  return(.forestPlotRenderPlot(plotData, options))
}

.fpStandaloneBuildInput <- function(dataset, options) {

  if (options[["subgroup"]] == "") {
    sections <- .fpStandaloneCollectSections(dataset, options)
    if (!.forestPlotSectionsHaveContent(sections)) {
      stop(gettext("No rows to display. Check that the visualization type mapping assigns at least one level to 'Study' or 'Estimate'."))
    }

    return(.forestPlotCreateInput(
      items = list(.forestPlotCreateInputItem(
        key           = "standalone",
        index         = 1,
        subgroup      = NULL,
        isFullDataset = FALSE,
        sections      = sections
      )),
      rowMode         = if (options[["row"]] != "") "absolute" else "stacked",
      sectionGrouping = "byItem"
    ))
  }

  sgCol    <- dataset[[options[["subgroup"]]]]
  sgLevels <- .fpStandaloneSubgroupLevels(sgCol)
  items    <- list()

  for (sg in sgLevels) {
    sgRows <- !is.na(sgCol) & sgCol == sg
    sgData <- dataset[sgRows, , drop = FALSE]
    if (nrow(sgData) == 0)
      next

    sections <- .fpStandaloneCollectSections(sgData, options)
    if (!.forestPlotSectionsHaveContent(sections)) {
      next
    }

    items[[length(items) + 1]] <- .forestPlotCreateInputItem(
      key           = paste0("subgroup", sg),
      index         = length(items) + 1,
      subgroup      = sg,
      isFullDataset = FALSE,
      sections      = sections
    )
  }

  if (length(items) == 0)
    stop(gettext("No rows to display. Check that the visualization type mapping assigns at least one level to 'Study' or 'Estimate'."))

  return(.forestPlotCreateInput(items, sectionGrouping = "byItem"))
}

.fpStandaloneCollectSections <- function(dataset, options) {

  split <- .fpStandaloneSplitByVisualizationType(dataset, options)

  return(list(
    study      = .fpStandaloneBuildStudySection(split[["study"]], options),
    additional = list(
      estimates = .fpStandaloneBuildEstimateSection(split[["estimate"]], options)
    )
  ))
}

.fpStandaloneSubgroupLevels <- function(sgCol) {

  if (is.factor(sgCol))
    return(levels(droplevels(sgCol[!is.na(sgCol)])))

  return(unique(as.character(sgCol[!is.na(sgCol)])))
}

# Split Study and Estimate rows from the user-selected visualization type.
.fpStandaloneSplitByVisualizationType <- function(dataset, options) {

  studyRows    <- rep(TRUE, nrow(dataset))
  estimateRows <- rep(FALSE, nrow(dataset))

  if (options[["visualizationType"]] != "") {
    vizCol        <- as.character(dataset[[options[["visualizationType"]]]])
    studyLevel    <- options[["visualizationTypeStudy"]]
    estimateLevel <- options[["visualizationTypeEstimate"]]

    if (studyLevel != "" || estimateLevel != "") {
      studyRows    <- !is.na(vizCol) & vizCol == studyLevel
      estimateRows <- !is.na(vizCol) & vizCol == estimateLevel
    }
  }

  return(list(
    study    = if (any(studyRows, na.rm = TRUE))    dataset[studyRows,    , drop = FALSE] else NULL,
    estimate = if (any(estimateRows, na.rm = TRUE)) dataset[estimateRows, , drop = FALSE] else NULL
  ))
}

# Extract the two column names from an AssignedPairsVariablesList option.
.fpStandaloneGetPairColumns <- function(options, optionName) {

  value <- options[[optionName]]
  if (is.null(value) || length(value) == 0)
    return(character(0))

  cols <- unlist(value)
  cols <- cols[cols != ""]
  return(cols)
}


# ── study section builder ────────────────────────────────────────────────────

.fpStandaloneBuildStudySection <- function(studyData, options) {

  if (is.null(studyData) || nrow(studyData) == 0)
    return(NULL)

  # build forestInformation from user columns
  dfForest <- .fpStandaloneExtractForestInfo(studyData, options)

  # transform
  dfForest <- .forestPlotStudyTransformEffectSizes(dfForest, options)

  # bind additional variables
  additionalVariables <- .forestPlotStudyAdditionalVariables(options)
  dfForest <- .forestPlotStudyBindAdditionalVariables(dfForest, studyData, additionalVariables)

  # aggregate (boxplot / bubbles)
  dfAggregate <- .forestPlotStudyAggregateData(dfForest, options, additionalVariables)
  dfForest    <- dfAggregate[["forest"]]
  dfGeoms     <- dfAggregate[["geoms"]]

  # order
  dfForest <- .forestPlotStudyOrderData(dfForest, options)

  # assign coordinates
  dfCoordinates <- .forestPlotStudyAssignCoordinates(dfForest, dfGeoms)
  dfForest      <- dfCoordinates[["forest"]]
  dfGeoms       <- dfCoordinates[["geoms"]]

  # override y with user row values (preserving gaps)
  if (options[["row"]] != "" && ".userRow" %in% colnames(dfForest)) {
    dfForest$y <- dfForest$.userRow
    dfForest$.userRow <- NULL
    if (!is.null(dfGeoms) && ".userRow" %in% colnames(dfGeoms)) {
      dfGeoms$y <- dfGeoms$.userRow
      dfGeoms$.userRow <- NULL
    }
  }

  return(.forestPlotCreateStudySection(
    forest     = dfForest,
    prediction = NULL,
    geoms      = dfGeoms
  ))
}

.fpStandaloneExtractForestInfo <- function(studyData, options) {

  n  <- nrow(studyData)
  es <- studyData[[options[["effectSize"]]]]

  # standard error and weights
  hasSe <- options[["effectSizeStandardError"]] != ""
  if (hasSe) {
    se <- studyData[[options[["effectSizeStandardError"]]]]
  } else {
    se <- rep(NA_real_, n)
  }
  weights <- .fpStandaloneStudyWeights(studyData, se, options)

  dfForest <- data.frame(
    effectSize    = es,
    standardError = se,
    weights       = weights,
    id            = seq_len(n)
  )

  # confidence interval: from columns or computed from SE
  ciCols <- .fpStandaloneGetPairColumns(options, "confidenceInterval")
  hasCi  <- length(ciCols) == 2
  if (hasCi) {
    dfForest$lCi <- studyData[[ciCols[1]]]
    dfForest$uCi <- studyData[[ciCols[2]]]
  } else if (hasSe) {
    dfForest <- .forestPlotStudyAddConfidenceIntervals(dfForest, options)
  } else {
    dfForest$lCi <- rep(NA_real_, n)
    dfForest$uCi <- rep(NA_real_, n)
  }

  # row ordering: store user values for later gap support
  if (options[["row"]] != "") {
    dfForest$.userRow <- studyData[[options[["row"]]]]
    dfForest          <- dfForest[order(dfForest$.userRow), ]
  }

  return(dfForest)
}


# ── estimate section builder (vectorized) ────────────────────────────────────

.fpStandaloneBuildEstimateSection <- function(estimateData, options) {

  if (is.null(estimateData) || nrow(estimateData) == 0)
    return(NULL)

  if (options[["row"]] != "")
    estimateData <- estimateData[order(estimateData[[options[["row"]]]]), ]

  intervals <- .fpStandaloneExtractEstimateIntervals(estimateData, options)
  positions <- .fpStandaloneEstimatePositions(estimateData, intervals, options)

  information <- .fpStandaloneEstimateInformation(estimateData, intervals, positions, options)
  objects     <- .fpStandaloneEstimateObjects(intervals, positions)

  return(.forestPlotCreateAdditionalSection(
    heading     = gettext("Estimates"),
    information = information,
    objects     = objects,
    showHeading = FALSE
  ))
}

# study weight builder
.fpStandaloneStudyWeights <- function(studyData, standardError, options) {

  if (options[["weights"]] != "") {
    weights   <- suppressWarnings(as.numeric(studyData[[options[["weights"]]]]))
    weightSum <- sum(weights)
    if (any(!is.finite(weights) | weights < 0) || !is.finite(weightSum) || weightSum <= 0)
      stop(gettext("The weights variable must contain finite, non-negative values with at least one positive value."))

    return(weights)
  }

  if (all(is.na(standardError)))
    return(rep(1, length(standardError)))

  return(ifelse(is.na(standardError) | standardError == 0, 1, 1 / standardError^2))
}

# Extract effect sizes, CIs, and PIs from estimate rows; apply transformation.
.fpStandaloneExtractEstimateIntervals <- function(estimateData, options) {

  n  <- nrow(estimateData)
  es <- estimateData[[options[["effectSize"]]]]

  # confidence intervals: from paired columns or computed from SE
  ciCols <- .fpStandaloneGetPairColumns(options, "confidenceInterval")
  hasCi  <- length(ciCols) == 2
  hasSe  <- options[["effectSizeStandardError"]] != ""

  if (hasCi) {
    lCi <- estimateData[[ciCols[1]]]
    uCi <- estimateData[[ciCols[2]]]
  } else if (hasSe) {
    se  <- estimateData[[options[["effectSizeStandardError"]]]]
    ciZ <- qnorm((1 - options[["confidenceIntervalsLevel"]]) / 2, lower.tail = FALSE)
    lCi <- es - ciZ * se
    uCi <- es + ciZ * se
  } else {
    lCi <- rep(NA_real_, n)
    uCi <- rep(NA_real_, n)
  }

  # prediction intervals: from paired columns only
  piCols <- .fpStandaloneGetPairColumns(options, "predictionInterval")
  hasPi  <- length(piCols) == 2
  if (hasPi) {
    lPi      <- estimateData[[piCols[1]]]
    uPi      <- estimateData[[piCols[2]]]
    hasPiRow <- !is.na(lPi) & !is.na(uPi)
  } else {
    lPi      <- rep(NA_real_, n)
    uPi      <- rep(NA_real_, n)
    hasPiRow <- rep(FALSE, n)
  }

  # apply effect size transformation
  if (options[["transformEffectSize"]] != "none") {
    transformFn <- .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]])
    es  <- transformFn(es)
    lCi <- transformFn(lCi)
    uCi <- transformFn(uCi)
    if (hasPi) {
      lPi <- transformFn(lPi)
      uPi <- transformFn(uPi)
    }
  }

  return(list(n = n, es = es, lCi = lCi, uCi = uCi, lPi = lPi, uPi = uPi, hasPiRow = hasPiRow))
}

# Compute row positions for diamonds and PI rectangles, handling user-specified
# row values and the extra rows needed for prediction interval display.
.fpStandaloneEstimatePositions <- function(estimateData, intervals, options) {

  hasPiRow <- intervals[["hasPiRow"]]

  if (options[["row"]] != "") {
    diamondRow <- estimateData[[options[["row"]]]]
    if (any(hasPiRow)) {
      piShift    <- c(0L, head(cumsum(as.integer(hasPiRow)), -1))
      diamondRow <- diamondRow + piShift
    }
  } else {
    diamondRow <- cumsum(c(1L, head(1L + as.integer(hasPiRow), -1L)))
  }

  return(list(diamondRow = diamondRow, piRow = diamondRow + 1L))
}

# Build the information data frame (text rows for left/right panels).
.fpStandaloneEstimateInformation <- function(estimateData, intervals, positions, options) {

  n          <- intervals[["n"]]
  hasPiRow   <- intervals[["hasPiRow"]]
  diamondRow <- positions[["diamondRow"]]
  piRow      <- positions[["piRow"]]

  diamondInfo <- data.frame(
    label = rep(NA_character_, n),
    y     = diamondRow,
    est   = intervals[["es"]],
    lCi   = intervals[["lCi"]],
    uCi   = intervals[["uCi"]],
    test  = rep("", n),
    face  = rep(NA, n)
  )

  # attach estimate info variable columns for multi-column left panel
  estimateVars    <- unlist(options[["forestPlotEstimateInformationSelectedVariables"]])
  hasEstimateVars <- length(estimateVars) > 0

  if (hasEstimateVars) {
    for (v in estimateVars)
      diamondInfo[[v]] <- as.character(estimateData[[v]])
  }

  if (!any(hasPiRow))
    return(diamondInfo)

  piInfo <- data.frame(
    label = rep(NA_character_, sum(hasPiRow)),
    y     = piRow[hasPiRow],
    est   = rep(NA_real_, sum(hasPiRow)),
    lCi   = intervals[["lPi"]][hasPiRow],
    uCi   = intervals[["uPi"]][hasPiRow],
    test  = rep("", sum(hasPiRow)),
    face  = rep(NA, sum(hasPiRow))
  )
  if (hasEstimateVars) {
    for (v in estimateVars)
      piInfo[[v]] <- NA_character_
  }

  information <- rbind(diamondInfo, piInfo)
  return(information[order(information$y), ])
}

# Build the polygon objects (diamonds for estimates, rectangles for PIs).
.fpStandaloneEstimateObjects <- function(intervals, positions) {

  es         <- intervals[["es"]]
  lCi        <- intervals[["lCi"]]
  uCi        <- intervals[["uCi"]]
  hasPiRow   <- intervals[["hasPiRow"]]
  diamondRow <- positions[["diamondRow"]]
  piRow      <- positions[["piRow"]]

  adj     <- 1 / 3
  objects <- data.frame(
    id       = rep(diamondRow, each = 4),
    x        = as.vector(rbind(lCi, es, uCi, es)),
    y        = as.vector(rbind(diamondRow, diamondRow - adj, diamondRow, diamondRow + adj)),
    type     = "diamond",
    mapColor = NA
  )

  if (any(hasPiRow)) {
    radj   <- 1 / 5
    piIdx  <- which(hasPiRow)
    piRect <- data.frame(
      id       = rep(piRow[piIdx], each = 4),
      x        = as.vector(rbind(intervals[["lPi"]][piIdx], intervals[["uPi"]][piIdx],
                                 intervals[["uPi"]][piIdx], intervals[["lPi"]][piIdx])),
      y        = as.vector(rbind(piRow[piIdx] - radj, piRow[piIdx] - radj,
                                 piRow[piIdx] + radj, piRow[piIdx] + radj)),
      type     = "rectangle",
      mapColor = NA
    )
    objects <- rbind(objects, piRect)
  }

  return(objects)
}
