# Forest plot pipeline: orchestration, layout, rendering, and composition.
#
# This is the main entry point for building forest plots.  The pipeline is:
#   fit -> fitItems -> sections -> layout -> plotData -> render
#
# The code is split across three files:
#   forestplotcommon.R  -- this file: pipeline, section collection, layout,
#                          rendering (middle/left/right panels), axis spec,
#                          clipped-interval rendering, and final composition
#   forestplotscaling.R -- transformation specs, axis math, tick planning,
#                          bounded axis spec, label formatting
#   forestplotdata.R    -- study data extraction, additional-section state
#                          management, EMM/model-info builders, panel helpers,
#                          aggregation


# ── Entry point & options preparation ──────────────────────────────────────────

.maMakeTheUltimateForestPlot           <- function(fit, options) {

  fitItems    <- .forestPlotPrepareFitItems(fit, options)
  options     <- .forestPlotPrepareOptions(options)
  dataOptions <- .forestPlotPrepareDataOptions(options)
  plotData    <- .forestPlotBuildPlotData(fitItems, dataOptions)

  return(.forestPlotRenderPlot(plotData, options))
}

# Adapt the incoming fit list into a stable shape before the later section and
# layout builders start combining subgroup/full-dataset outputs.
.forestPlotPrepareFitItems             <- function(fit, options) {

  if (options[["subgroup"]] != "" && length(fit) > 1) {
    fit <- fit[c(2:length(fit), 1)]
  }

  if (options[["subgroup"]] != "" && length(fit) > 0) {
    fullDatasetName <- names(fit)[length(fit)]
  } else {
    fullDatasetName <- NULL
  }
  fit <- fit[!vapply(fit, jaspBase::isTryError, logical(1))]

  fitItems <- lapply(seq_along(fit), function(i) {
    list(
      index         = i,
      key           = names(fit)[i],
      fit           = fit[[i]],
      subgroup      = attr(fit[[i]], "subgroup"),
      isFullDataset = !is.null(fullDatasetName) && identical(names(fit)[i], fullDatasetName)
    )
  })
  names(fitItems) <- names(fit)

  return(fitItems)
}

# Normalize plot-only options so the downstream study/EMM/model builders can
# rely on one internal option contract.
.forestPlotPrepareOptions              <- function(options) {

  # forest plot has separate confidence and prediction interval toggles,
  # but the underlying builders use the general settings
  # (overridden here to simplify dispatch in general functions)
  options[["confidenceIntervals"]] <- TRUE
  options[["predictionIntervals"]] <- options[["forestPlotPredictionIntervals"]]

  if (is.null(options[["forestPlotAuxiliaryXAxisTransformLabelsOnly"]])) {
    options[["forestPlotAuxiliaryXAxisTransformLabelsOnly"]] <- options[["transformEffectSize"]] != "none"
  }

  # cannot plot predicted effects alongside aggregate study information
  if (options[["forestPlotStudyInformationAggregateBy"]] != "") {
    options[["forestPlotStudyInformationPredictedEffects"]] <- FALSE
  }

  return(options)
}
.forestPlotPrepareDataOptions          <- function(options) {

  dataOptions <- options

  # Keep study/object coordinates on the original scale when only the tick labels
  # should be transformed; the displayed axis labels are handled later.
  if (.forestPlotTransformXAxisLabelsOnly(options)) {
    dataOptions[["transformEffectSize"]] <- "none"
  }

  return(dataOptions)
}

# ── Section collection ────────────────────────────────────────────────────────
# Build the three logical forest-plot sections (study, EMM, model info) so
# the layout stage can stay agnostic to whether a row came from studies,
# estimated marginal means, or model summaries.
.forestPlotCollectSections             <- function(fitItems, options) {

  fitSections <- lapply(fitItems, function(fitItem) {
    sections <- list(
      study                  = .forestPlotCollectStudySection(fitItem[["fit"]], options),
      estimatedMarginalMeans = .forestPlotCollectEstimatedMarginalMeansSection(fitItem[["fit"]], options),
      modelInformation       = .forestPlotCollectModelInformationSection(fitItem[["fit"]], options)
    )

    c(fitItem, list(sections = sections))
  })
  names(fitSections) <- names(fitItems)

  return(fitSections)
}
.forestPlotCollectStudySection         <- function(fit, options) {

  if (!options[["forestPlotStudyInformation"]]) {
    return(NULL)
  }

  studySection <- .forestPlotBuildStudyInformation(fit, options)
  if (is.null(studySection)) {
    return(NULL)
  }

  return(.forestPlotCreateStudySection(
    forest     = studySection[["forest"]],
    prediction = studySection[["prediction"]],
    geoms      = studySection[["geoms"]]
  ))
}
.forestPlotCollectEstimatedMarginalMeansSection <- function(fit, options) {

  if (!(options[["forestPlotEstimatedMarginalMeans"]] &&
        (length(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]]) > 0 ||
         options[["forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"]]))) {
    return(NULL)
  }

  estimatedMarginalMeansSection <- .forestPlotBuildEstimatedMarginalMeans(fit, options)
  if (is.null(estimatedMarginalMeansSection)) {
    return(NULL)
  }

  return(.forestPlotCreateAdditionalSection(
    heading     = gettext("Estimated Marginal Means"),
    information = estimatedMarginalMeansSection[["information"]],
    objects     = estimatedMarginalMeansSection[["objects"]]
  ))
}
.forestPlotCollectModelInformationSection <- function(fit, options) {

  if (!options[["forestPlotModelInformation"]]) {
    return(NULL)
  }

  modelInformationSection <- .forestPlotBuildModelInformation(fit, options)
  if (is.null(modelInformationSection)) {
    return(NULL)
  }

  return(.forestPlotCreateAdditionalSection(
    heading     = gettext("Model Information"),
    information = modelInformationSection[["information"]],
    objects     = modelInformationSection[["objects"]]
  ))
}

# Study/additional sections share one normalized structure: a text data frame
# plus optional object layers to draw in the middle panel.
.forestPlotCreateStudySection         <- function(forest, prediction = NULL, geoms = NULL) {

  objects <- .forestPlotBindDataFrames(list(
    .forestPlotNormalizeObjectData(prediction),
    .forestPlotNormalizeObjectData(geoms)
  ))

  if (is.null(forest) && is.null(objects)) {
    return(NULL)
  }

  return(list(
    kind        = "study",
    information = forest,
    objects     = objects
  ))
}
.forestPlotCreateAdditionalSection    <- function(heading, information, objects = NULL) {

  objects <- .forestPlotNormalizeObjectData(objects)

  if (is.null(information) && is.null(objects)) {
    return(NULL)
  }

  return(list(
    kind        = "additional",
    heading     = heading,
    information = information,
    objects     = objects
  ))
}
.forestPlotNormalizeObjectData        <- function(objects) {

  if (is.null(objects)) {
    return(NULL)
  }

  if (!"type" %in% colnames(objects) && "geom" %in% colnames(objects)) {
    objects$type <- objects$geom
  }

  if (!"mapColor" %in% colnames(objects)) {
    objects$mapColor <- NA
  }

  return(objects)
}
.forestPlotBindDataFrames             <- function(dataFrames) {

  dataFrames <- dataFrames[!vapply(dataFrames, is.null, logical(1))]
  if (length(dataFrames) == 0) {
    return(NULL)
  }

  return(do.call(rbind, dataFrames))
}
.forestPlotHasDataFrame               <- function(dataFrame) {
  return(!is.null(dataFrame) && nrow(dataFrame) > 0)
}
.forestPlotSectionHasContent          <- function(section) {
  return(!is.null(section) && (.forestPlotHasDataFrame(section[["information"]]) || .forestPlotHasDataFrame(section[["objects"]])))
}


# ── Layout accumulation ───────────────────────────────────────────────────────
# The layout accumulator tracks row numbers and postpones coordinate scaling
# until all study and additional blocks have been appended.
.forestPlotCreateLayout               <- function() {
  return(list(
    forestHeaderIndex     = NULL,
    estimateHeaderIndex   = NULL,
    forestInformation     = list(),
    forestObjects         = list(),
    additionalInformation = list(),
    additionalObjects     = list(),
    subgroupHeadings      = list(),
    row                   = 1
  ))
}
.forestPlotBuildPlotData              <- function(fitItems, options) {

  fitSections <- .forestPlotCollectSections(fitItems, options)
  layout      <- .forestPlotCreateLayout()

  if (options[["subgroup"]] == "" || options[["forestPlotSubgroupPanelsWithinSubgroup"]]) {
    layout <- .forestPlotAppendGroupedFitSections(layout, fitSections, options)
  } else {
    layout <- .forestPlotAppendGroupedSectionPanels(layout, fitSections, options)
  }

  return(.forestPlotFinalizeLayout(layout, options))
}

# Layout can be grouped by fit (study + extras together) or by section type
# (all study blocks first, then shared EMM/model panels across subgroups).
.forestPlotAppendGroupedFitSections   <- function(layout, fitSections, options) {

  for (fitSection in fitSections) {
    sectionVisibility <- .forestPlotFitSectionVisibility(fitSection, options)

    if (options[["subgroup"]] != "") {
      layout <- .forestPlotAppendSubgroupHeading(layout, options, fitSection[["subgroup"]])
    }

    if (sectionVisibility[["study"]]) {
      layout <- .forestPlotAppendStudySection(layout, fitSection[["sections"]][["study"]], fitSection[["index"]], options)
    }

    if (sectionVisibility[["estimatedMarginalMeans"]]) {
      layout <- .forestPlotAppendAdditionalSection(
        layout   = layout,
        section  = fitSection[["sections"]][["estimatedMarginalMeans"]],
        blockId  = paste("emm", fitSection[["index"]], sep = "_"),
        addTitle = TRUE
      )
    }

    if (sectionVisibility[["modelInformation"]]) {
      layout <- .forestPlotAppendAdditionalSection(
        layout   = layout,
        section  = fitSection[["sections"]][["modelInformation"]],
        blockId  = paste("mi", fitSection[["index"]], sep = "_"),
        addTitle = TRUE
      )
    }
  }

  return(layout)
}
.forestPlotAppendGroupedSectionPanels <- function(layout, fitSections, options) {

  layout <- .forestPlotAppendStudyBlocks(layout, fitSections, options)
  layout <- .forestPlotAppendAdditionalBlocks(layout, fitSections, options, "estimatedMarginalMeans")
  layout <- .forestPlotAppendAdditionalBlocks(layout, fitSections, options, "modelInformation")

  return(layout)
}
.forestPlotAppendStudyBlocks          <- function(layout, fitSections, options) {

  for (fitSection in fitSections) {
    if (options[["subgroup"]] != "" && isTRUE(fitSection[["isFullDataset"]])) {
      next
    }

    if (options[["subgroup"]] != "") {
      layout <- .forestPlotAppendSubgroupHeading(layout, options, fitSection[["subgroup"]])
    }

    layout <- .forestPlotAppendStudySection(layout, fitSection[["sections"]][["study"]], fitSection[["index"]], options)
  }

  return(layout)
}
.forestPlotAppendAdditionalBlocks     <- function(layout, fitSections, options, sectionName) {

  includedSections <- Filter(function(fitSection) {
    .forestPlotShouldIncludeGroupedSection(fitSection, sectionName, options) &&
      .forestPlotSectionHasContent(fitSection[["sections"]][[sectionName]])
  }, fitSections)

  if (length(includedSections) == 0) {
    return(layout)
  }

  layout <- .forestPlotAppendAdditionalHeading(layout, includedSections[[1]][["sections"]][[sectionName]][["heading"]])

  for (fitSection in fitSections) {
    if (!.forestPlotShouldIncludeGroupedSection(fitSection, sectionName, options)) {
      next
    }

    if (options[["subgroup"]] != "") {
      layout <- .forestPlotAppendSubgroupHeading(layout, options, fitSection[["subgroup"]])
    }

    layout <- .forestPlotAppendAdditionalSection(
      layout   = layout,
      section  = fitSection[["sections"]][[sectionName]],
      blockId  = paste(sectionName, fitSection[["index"]], sep = "_"),
      addTitle = FALSE
    )
  }

  return(layout)
}
.forestPlotFitSectionVisibility       <- function(fitSection, options) {

  if (options[["subgroup"]] != "" && isTRUE(fitSection[["isFullDataset"]])) {
    return(list(
      study                  = FALSE,
      estimatedMarginalMeans = options[["forestPlotSubgroupFullDatasetEstimatedMarginalMeans"]],
      modelInformation       = options[["forestPlotSubgroupFullDatasetModelInformation"]]
    ))
  }

  return(list(
    study                  = TRUE,
    estimatedMarginalMeans = TRUE,
    modelInformation       = TRUE
  ))
}
.forestPlotShouldIncludeGroupedSection <- function(fitSection, sectionName, options) {

  if (options[["subgroup"]] == "" || !isTRUE(fitSection[["isFullDataset"]])) {
    return(TRUE)
  }

  if (sectionName == "estimatedMarginalMeans") {
    return(options[["forestPlotSubgroupFullDatasetEstimatedMarginalMeans"]])
  }

  if (sectionName == "modelInformation") {
    return(options[["forestPlotSubgroupFullDatasetModelInformation"]])
  }

  return(FALSE)
}
.forestPlotAppendSubgroupHeading      <- function(layout, options, subgroup) {

  if (isFALSE(options[["forestPlotSubgroupShowTitles"]])) {
    return(layout)
  }

  layout[["subgroupHeadings"]][[length(layout[["subgroupHeadings"]]) + 1]] <- .forestPlotSubgroupHeading(options, subgroup, layout[["row"]])
  layout[["row"]] <- layout[["row"]] + 1

  return(layout)
}
.forestPlotAppendAdditionalHeading    <- function(layout, heading) {

  layout[["additionalInformation"]][[length(layout[["additionalInformation"]]) + 1]] <- .forestPlotPanelHeading(heading, layout[["row"]])
  layout[["row"]] <- layout[["row"]] + 1

  return(layout)
}
.forestPlotAppendStudySection         <- function(layout, section, blockId, options) {

  if (!.forestPlotSectionHasContent(section)) {
    return(layout)
  }

  if (.forestPlotHasStudyInformationHeader(options)) {
    layout[["forestHeaderIndex"]] <- c(layout[["forestHeaderIndex"]], layout[["row"]])
    layout[["row"]]               <- layout[["row"]] + 1
  }

  return(.forestPlotAppendSectionData(layout, section, blockId, infoSlot = "forestInformation", objectSlot = "forestObjects"))
}
.forestPlotAppendAdditionalSection    <- function(layout, section, blockId, addTitle = TRUE) {

  if (!.forestPlotSectionHasContent(section)) {
    return(layout)
  }

  if (addTitle) {
    layout <- .forestPlotAppendAdditionalHeading(layout, section[["heading"]])
  }

  return(.forestPlotAppendSectionData(layout, section, blockId, infoSlot = "additionalInformation", objectSlot = "additionalObjects"))
}
# Shared append logic: offset y by current row, add info/objects to named slots.
.forestPlotAppendSectionData         <- function(layout, section, blockId, infoSlot, objectSlot) {

  info   <- section[["information"]]
  info$y <- info$y + (layout[["row"]] - 1)
  layout[[infoSlot]][[length(layout[[infoSlot]]) + 1]] <- info

  if (.forestPlotHasDataFrame(section[["objects"]])) {
    objects    <- section[["objects"]]
    objects$y  <- objects$y + (layout[["row"]] - 1)
    objects$id <- paste(objects$id, blockId, sep = "_")
    layout[[objectSlot]][[length(layout[[objectSlot]]) + 1]] <- objects
  }

  layout[["row"]] <- max(info$y, na.rm = TRUE) + 2

  return(layout)
}

# Convert row indices into plot coordinates only once, after the full block
# stack is known.
.forestPlotFinalizeLayout             <- function(layout, options) {

  plotData <- list(
    forestHeaderIndex     = layout[["forestHeaderIndex"]],
    estimateHeaderIndex   = layout[["estimateHeaderIndex"]],
    forestInformation     = .forestPlotBindDataFrames(layout[["forestInformation"]]),
    forestObjects         = .forestPlotBindDataFrames(layout[["forestObjects"]]),
    additionalInformation = .forestPlotBindDataFrames(layout[["additionalInformation"]]),
    additionalObjects     = .forestPlotBindDataFrames(layout[["additionalObjects"]]),
    subgroupHeadings      = .forestPlotBindDataFrames(layout[["subgroupHeadings"]]),
    nextRow               = layout[["row"]]
  )

  # Convert row indices to plot coordinates (negative = top-to-bottom, scaled)
  rowSize <- options[["forestPlotRelativeSizeRow"]]
  plotData[["forestHeaderIndex"]]     <- .forestPlotScaleY(plotData[["forestHeaderIndex"]], rowSize)
  plotData[["estimateHeaderIndex"]]   <- .forestPlotScaleY(plotData[["estimateHeaderIndex"]], rowSize)
  plotData[["forestInformation"]]     <- .forestPlotScaleYColumn(plotData[["forestInformation"]],     rowSize)
  plotData[["forestObjects"]]         <- .forestPlotScaleYColumn(plotData[["forestObjects"]],         rowSize)
  plotData[["additionalInformation"]] <- .forestPlotScaleYColumn(plotData[["additionalInformation"]], rowSize)
  plotData[["additionalObjects"]]     <- .forestPlotScaleYColumn(plotData[["additionalObjects"]],     rowSize)
  plotData[["subgroupHeadings"]]      <- .forestPlotScaleYColumn(plotData[["subgroupHeadings"]],      rowSize)

  return(plotData)
}
.forestPlotScaleY                    <- function(values, rowSize) {
  if (is.null(values)) return(NULL)
  return(-values * rowSize)
}
.forestPlotScaleYColumn              <- function(df, rowSize) {
  if (is.null(df)) return(NULL)
  df$y <- -df$y * rowSize
  return(df)
}

# ── Rendering & panel construction ─────────────────────────────────────────────
# Split into three panel builders so left/right text columns can be composed
# independently from the forest geometry in the middle panel.
.forestPlotRenderPlot                 <- function(plotData, options) {

  axisSpec   <- .forestPlotBuildAxisSpec(plotData, options)
  plotForest <- .forestPlotBuildMiddlePanel(plotData, axisSpec, options)
  plotLeft   <- .forestPlotBuildLeftPanel(plotData, options)
  plotRight  <- .forestPlotBuildRightPanel(plotData, options)

  plotForest <- .forestPlotApplyForestTheme(plotForest, axisSpec, options)

  if (!is.null(plotLeft)) {
    plotLeft <- .forestPlotApplySidePanelTheme(plotLeft, axisSpec[["yRange"]], options)
  }

  if (!is.null(plotRight)) {
    plotRight <- .forestPlotApplySidePanelTheme(plotRight, axisSpec[["yRange"]], options)
  }

  return(.forestPlotComposeOutput(plotForest, plotLeft, plotRight, plotData, options))
}
.forestPlotBuildMiddlePanel           <- function(plotData, axisSpec, options) {

  plotForest <- ggplot2::ggplot()
  plotForest <- .forestPlotAddStudyObjects(plotForest, plotData, axisSpec, options)
  plotForest <- .forestPlotAddAdditionalObjects(plotForest, plotData, axisSpec)
  plotForest <- .forestPlotAddVerticalLines(plotForest, options)

  return(plotForest)
}

# Middle-panel study objects include study-level estimates plus aggregate
# geoms/predictions when those options are enabled.
.forestPlotAddStudyObjects            <- function(plotForest, plotData, axisSpec, options) {

  if (!options[["forestPlotStudyInformation"]]) {
    return(plotForest)
  }

  if (options[["forestPlotStudyInformationAggregateBy"]] != "") {
    return(.forestPlotAddAggregateObjects(plotForest, plotData[["forestObjects"]], axisSpec, options))
  }

  plotForest <- .forestPlotAddPredictionDiamonds(plotForest, plotData[["forestObjects"]], axisSpec, options)
  plotForest <- .forestPlotAddStudyPoints(plotForest, plotData[["forestInformation"]], axisSpec, options)

  return(plotForest)
}
.forestPlotAddAggregateObjects       <- function(plotForest, forestObjects, axisSpec, options) {

  if (!.forestPlotHasDataFrame(forestObjects)) {
    return(plotForest)
  }

  colorVar <- options[["forestPlotMappingColor"]]

  if (options[["forestPlotStudyInformationAggregateMethod"]] == "boxplot") {
    forestObjects <- forestObjects[forestObjects$type == "boxplot", , drop = FALSE]
    forestObjects <- .forestPlotClipXAxisColumns(forestObjects, c("min", "lower", "middle", "upper", "max"), axisSpec[["xRange"]])

    plotForest <- plotForest + .forestPlotGeomLayer(
      ggplot2::geom_boxplot, forestObjects,
      aes = list(
        y = as.name("y"), group = as.name("id"),
        xmin = as.name("min"), xlower = as.name("lower"), xmiddle = as.name("middle"),
        xupper = as.name("upper"), xmax = as.name("max"),
        fill = if (colorVar != "") as.name(colorVar)
      ),
      fill = if (colorVar == "") "grey20", alpha = 0.8, orientation = "y", stat = "identity"
    )
  }

  if (options[["forestPlotStudyInformationAggregateMethod"]] == "bubbles") {
    forestObjects <- forestObjects[forestObjects$type == "bubbles", , drop = FALSE]
    forestObjects <- .forestPlotClipXAxisColumns(forestObjects, c("x"), axisSpec[["xRange"]])

    plotForest <- plotForest + .forestPlotGeomLayer(
      jaspGraphs::geom_point, forestObjects,
      aes = list(
        y = as.name("y"), x = as.name("x"), size = as.name("weight"),
        fill  = if (colorVar != "") as.name(colorVar),
        color = if (colorVar != "") as.name(colorVar)
      ),
      fill = if (colorVar == "") "grey20", alpha = 0.8, na.rm = TRUE,
      position = ggplot2::position_jitter(width = 0, height = 0.10)
    ) + ggplot2::scale_size(range = c(1.5, 10) * options[["forestPlotStudyInformationAggregateMethodBubbleRelativeSize"]])
  }

  return(plotForest)
}
.forestPlotAddPredictionDiamonds     <- function(plotForest, forestObjects, axisSpec, options) {

  if (!options[["forestPlotStudyInformationPredictedEffects"]] || !.forestPlotHasDataFrame(forestObjects)) {
    return(plotForest)
  }

  forestPrediction <- forestObjects[forestObjects$type == "diamond", , drop = FALSE]
  forestPrediction <- .forestPlotClipXAxisColumns(forestPrediction, c("x"), axisSpec[["xRange"]])
  colorVar         <- options[["forestPlotMappingColor"]]

  plotForest <- plotForest + .forestPlotGeomLayer(
    ggplot2::geom_polygon, forestPrediction,
    aes = list(
      x = as.name("x"), y = as.name("y"), group = as.name("id"),
      fill = if (colorVar != "") as.name(colorVar)
    ),
    fill = if (colorVar == "") "grey20", alpha = 0.8
  )

  return(plotForest)
}
.forestPlotAddStudyPoints            <- function(plotForest, forestInformation, axisSpec, options) {

  if (!.forestPlotHasDataFrame(forestInformation)) {
    return(plotForest)
  }

  colorVar <- options[["forestPlotMappingColor"]]
  shapeVar <- options[["forestPlotMappingShape"]]

  forestInformationPoints <- .forestPlotMaskOutsideXAxis(forestInformation, "effectSize", axisSpec[["xRange"]])

  plotForest <- plotForest + .forestPlotGeomLayer(
    ggplot2::geom_point, forestInformationPoints,
    aes = list(
      x     = as.name("effectSize"), y = as.name("y"), size = as.name("weights"),
      color = if (colorVar != "") as.name(colorVar),
      shape = if (shapeVar != "") as.name(shapeVar)
    ),
    color = if (colorVar == "") options[["forestPlotAuxiliaryPlotColor"]],
    shape = if (shapeVar == "") 15,
    na.rm = TRUE
  ) + ggplot2::scale_size(range = c(1, 6) * options[["forestPlotRelativeSizeEstimates"]])

  if (shapeVar != "") {
    plotForest <- plotForest + ggplot2::scale_shape_manual(
      values = rep(c(15:18, 21:25), length.out = length(unique(forestInformation[[shapeVar]])))
    )
  }

  # Primary confidence intervals
  plotForest <- .forestPlotAddClippedIntervalLayers(
    plotForest        = plotForest,
    clippedIntervals  = .forestPlotPrepareClippedIntervalData(
      intervalData = data.frame(xmin = forestInformation$lCi, xmax = forestInformation$uCi,
                                y = forestInformation$y, est = forestInformation$effectSize),
      axisSpec = axisSpec, capHeight = 0, style = "interval"
    ),
    color = "black", lineWidth = 0.5, overflowLineWidth = 1
  )

  # Secondary confidence intervals
  if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
    plotForest <- .forestPlotAddClippedIntervalLayers(
      plotForest        = plotForest,
      clippedIntervals  = .forestPlotPrepareClippedIntervalData(
        intervalData = data.frame(xmin = forestInformation$lCi2, xmax = forestInformation$uCi2,
                                  y = forestInformation$y, est = forestInformation$effectSize),
        axisSpec = axisSpec, capHeight = 0.3, style = "interval"
      ),
      color = "darkblue", lineWidth = 0.5, overflowLineWidth = 1
    )
  }

  return(plotForest)
}
.forestPlotAddAdditionalObjects       <- function(plotForest, plotData, axisSpec) {

  additionalObjectsRaw <- plotData[["additionalObjects"]]
  forestObjects        <- plotData[["forestObjects"]]
  additionalObjects    <- additionalObjectsRaw

  if (!.forestPlotHasDataFrame(additionalObjects)) {
    additionalObjects <- NULL
  } else {
    additionalObjects <- .forestPlotClipXAxisColumns(
      data    = additionalObjects,
      columns = c("x"),
      xRange  = axisSpec[["xRange"]]
    )
  }

  if (.forestPlotHasDataFrame(additionalObjects) && any(!is.na(additionalObjects$mapColor))) {
    plotForest <- plotForest + ggplot2::geom_polygon(
      data    = additionalObjects[!is.na(additionalObjects$mapColor), , drop = FALSE],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        group = id,
        fill  = mapColor
      )
    )
  }

  if (.forestPlotHasDataFrame(additionalObjects) && any(is.na(additionalObjects$mapColor))) {
    plotForest <- plotForest + ggplot2::geom_polygon(
      data    = additionalObjects[is.na(additionalObjects$mapColor), , drop = FALSE],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        group = id
      )
    )
  }

  objectIndicatorData <- .forestPlotPrepareObjectIndicatorData(
    objects   = .forestPlotBindDataFrames(list(forestObjects, additionalObjectsRaw)),
    axisSpec  = axisSpec,
    skipTypes = c("boxplot", "bubbles")
  )
  plotForest <- .forestPlotAddClippedIntervalLayers(
    plotForest        = plotForest,
    clippedIntervals  = objectIndicatorData,
    color             = "grey20",
    lineWidth         = 0.6,
    overflowLineWidth = 1.2
  )

  return(plotForest)
}
.forestPlotAddVerticalLines           <- function(plotForest, options) {

  if (options[["forestPlotAuxiliaryAddVerticalLine"]]) {
    plotForest <- plotForest + ggplot2::geom_vline(
      xintercept = .forestPlotResolveVerticalLineValue(options[["forestPlotAuxiliaryAddVerticalLineValue"]], options),
      linetype   = "dashed"
    )
  }

  if (options[["forestPlotAuxiliaryAddVerticalLine2"]]) {
    plotForest <- plotForest + ggplot2::geom_vline(
      xintercept = .forestPlotResolveVerticalLineValue(options[["forestPlotAuxiliaryAddVerticalLineValue2"]], options),
      linetype   = "dotted"
    )
  }

  return(plotForest)
}

# ── Side panel data & rendering ────────────────────────────────────────────────
# Side panels are assembled as text-layer payloads first so the ggplot code
# stays mostly declarative.
.forestPlotPrepareLeftPanelData       <- function(plotData, options) {

  forestInformation     <- plotData[["forestInformation"]]
  additionalInformation <- plotData[["additionalInformation"]]
  subgroupHeadings      <- plotData[["subgroupHeadings"]]

  hasStudyVars    <- options[["forestPlotStudyInformation"]] && length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0
  hasEstimateVars <- length(options[["forestPlotEstimateInformationSelectedVariables"]]) > 0
  if (!hasStudyVars && !hasEstimateVars && !.forestPlotHasDataFrame(additionalInformation)) {
    return(NULL)
  }

  leftPanelData <- list(
    titles = NULL, studyDataColored = NULL, studyData = NULL,
    estimateTitles = NULL, additionalData = NULL, additionalInformation = NULL,
    subgroupHeadings = NULL, maxCharsLeft = NULL
  )

  if (hasStudyVars) {
    leftPanelData <- .forestPlotPrepareLeftPanelStudyData(leftPanelData, plotData, forestInformation, additionalInformation, options)
  }
  if (hasEstimateVars) {
    leftPanelData <- .forestPlotPrepareLeftPanelEstimateData(leftPanelData, plotData, additionalInformation, options)
  }
  leftPanelData <- .forestPlotPrepareLeftPanelAdditional(leftPanelData, additionalInformation, options)
  leftPanelData <- .forestPlotPrepareLeftPanelSubgroups(leftPanelData, subgroupHeadings, options)

  return(leftPanelData)
}
.forestPlotPrepareLeftPanelStudyData <- function(leftPanelData, plotData, forestInformation, additionalInformation, options) {

  studyInfo <- .forestPlotBuildStudyInformationHeader(options, forestInformation, additionalInformation)
  leftPanelData[["maxCharsLeft"]] <- attr(studyInfo, "maxChars")

  studyInfo$x <- ifelse(
    studyInfo$alignment == "left",   studyInfo$xStart,
    ifelse(studyInfo$alignment == "middle",
           (studyInfo$xStart + studyInfo$xEnd) / 2,
           studyInfo$xEnd)
  )

  # Title row(s) repeated at each forest header position
  if (any(studyInfo$title != "") && length(plotData[["forestHeaderIndex"]]) > 0) {
    leftPanelData[["titles"]] <- do.call(rbind, lapply(plotData[["forestHeaderIndex"]], function(y) {
      studyInfo$y <- y
      return(studyInfo)
    }))
  }

  colorVar <- options[["forestPlotMappingColor"]]

  # Color-mapped variable column gets its own text layer (so ggplot can color it)
  if (any(studyInfo$value == colorVar)) {
    leftPanelData[["studyDataColored"]] <- data.frame(
      x         = studyInfo$x[studyInfo$value == colorVar],
      y         = forestInformation$y,
      label     = as.character(forestInformation[[colorVar]]),
      alignment = studyInfo$alignment[studyInfo$value == colorVar]
    )
  }

  # Remaining (non-color-mapped) study variable columns
  otherVars <- unique(studyInfo$value[studyInfo$value != colorVar])
  if (length(otherVars) > 0) {
    leftPanelData[["studyData"]] <- do.call(rbind.data.frame, lapply(otherVars, function(variable) {
      data.frame(
        x         = studyInfo$x[studyInfo$value == variable],
        y         = forestInformation$y,
        label     = as.character(forestInformation[[variable]]),
        alignment = studyInfo$alignment[studyInfo$value == variable]
      )
    }))
  }

  return(leftPanelData)
}
.forestPlotPrepareLeftPanelEstimateData <- function(leftPanelData, plotData, additionalInformation, options) {

  estimateSettings <- .forestPlotEstimateInformationSettings(options)
  if (nrow(estimateSettings) == 0 || !.forestPlotHasDataFrame(additionalInformation)) {
    return(leftPanelData)
  }

  availableVars <- intersect(estimateSettings$value, colnames(additionalInformation))
  if (length(availableVars) == 0) {
    return(leftPanelData)
  }

  estimateSettings <- estimateSettings[estimateSettings$value %in% availableVars, , drop = FALSE]

  hasMultiColumn <- rowSums(!is.na(additionalInformation[, availableVars, drop = FALSE])) > 0
  if (!any(hasMultiColumn)) {
    return(leftPanelData)
  }

  estimateInfo <- additionalInformation[hasMultiColumn, , drop = FALSE]

  charWidths       <- .forestPlotStudyInformationCharWidths(estimateSettings, estimateInfo)
  maxCharsEstimate <- sum(charWidths)

  leftPanelData[["maxCharsLeft"]] <- max(c(leftPanelData[["maxCharsLeft"]], maxCharsEstimate), na.rm = TRUE)

  relativeWidths <- .forestPlotStudyInformationRelativeWidths(
    estimateSettings, leftPanelData[["maxCharsLeft"]], charWidths, options
  )

  if (options[["forestPlotAuxiliaryAdjustWidthBasedOnText"]]) {
    estimateSettings$xStart <- cumsum(relativeWidths[-length(relativeWidths)])
    estimateSettings$xEnd   <- cumsum(relativeWidths)[-1]
  } else {
    estimateSettings$xStart <- c(0, cumsum(relativeWidths[-length(relativeWidths)]))
    estimateSettings$xEnd   <- cumsum(relativeWidths)
  }

  estimateSettings$x <- ifelse(
    estimateSettings$alignment == "left",   estimateSettings$xStart,
    ifelse(estimateSettings$alignment == "middle",
           (estimateSettings$xStart + estimateSettings$xEnd) / 2,
           estimateSettings$xEnd)
  )

  leftPanelData[["additionalData"]] <- do.call(rbind.data.frame, lapply(availableVars, function(variable) {
    data.frame(
      x         = estimateSettings$x[estimateSettings$value == variable],
      y         = estimateInfo$y,
      label     = as.character(estimateInfo[[variable]]),
      alignment = estimateSettings$alignment[estimateSettings$value == variable]
    )
  }))

  # title rows at estimateHeaderIndex positions
  estimateHeaderIndex <- plotData[["estimateHeaderIndex"]]
  if (!is.null(estimateHeaderIndex) && any(estimateSettings$title != "")) {
    leftPanelData[["estimateTitles"]] <- do.call(rbind, lapply(estimateHeaderIndex, function(y) {
      data.frame(
        x         = estimateSettings$x,
        y         = y,
        label     = estimateSettings$title,
        alignment = estimateSettings$alignment
      )
    }))
  }

  return(leftPanelData)
}
.forestPlotPrepareLeftPanelAdditional <- function(leftPanelData, additionalInformation, options) {

  if (!.forestPlotHasDataFrame(additionalInformation)) {
    return(leftPanelData)
  }

  info <- additionalInformation[!is.na(additionalInformation$label), , drop = FALSE]
  if (!.forestPlotHasDataFrame(info)) {
    return(leftPanelData)
  }

  info$x     <- .forestPlotLeftPanelAlign(options)
  info$face[is.na(info$face)] <- "plain"

  leftPanelData[["additionalInformation"]] <- info
  leftPanelData[["maxCharsLeft"]] <- max(c(
    leftPanelData[["maxCharsLeft"]],
    max(nchar(info$label))
  ), na.rm = TRUE)

  return(leftPanelData)
}
.forestPlotPrepareLeftPanelSubgroups <- function(leftPanelData, subgroupHeadings, options) {

  if (!.forestPlotHasDataFrame(subgroupHeadings)) {
    return(leftPanelData)
  }

  subgroupHeadings$x <- .forestPlotLeftPanelAlign(options)
  leftPanelData[["subgroupHeadings"]] <- subgroupHeadings

  return(leftPanelData)
}

# Right-panel intervals are formatted once into fixed-width strings so the
# panel can render them in a monospace column.
.forestPlotFormatRightPanelIntervals  <- function(intervalData, options) {

  if (!.forestPlotHasDataFrame(intervalData)) {
    return(NULL)
  }

  intervalData <- intervalData[
    !apply(intervalData[, c("est", "lCi", "uCi")], 1, function(x) all(is.na(x))),
    ,
    drop = FALSE
  ]
  if (!.forestPlotHasDataFrame(intervalData)) {
    return(NULL)
  }

  if (.forestPlotTransformXAxisLabelsOnly(options)) {
    for (colName in c("est", "lCi", "uCi")) {
      intervalData[[colName]] <- .forestPlotTransformAxisValues(intervalData[[colName]], options)
    }
  }

  for (colName in c("est", "lCi", "uCi")) {
    nonMissing <- !is.na(intervalData[[colName]])
    intervalData[nonMissing, colName] <- .maFormatDigits(
      intervalData[nonMissing, colName],
      options[["forestPlotAuxiliaryDigits"]]
    )
  }

  intervalData$label <- ifelse(
    is.na(intervalData$est),
    paste0("PI [", intervalData$lCi, ", ", intervalData$uCi, "]"),
    paste0(intervalData$est, " [", intervalData$lCi, ", ", intervalData$uCi, "]")
  )

  return(intervalData)
}
.forestPlotBuildStudyWeightLabels     <- function(forestInformation, options) {

  if (!.forestPlotHasDataFrame(forestInformation)) {
    return(NULL)
  }

  studyWeights       <- forestInformation[, c("y", "weights")]
  formatString       <- paste0("%1$.", options[["forestPlotAuxiliaryDigits"]], "f")
  studyWeights$label <- paste0(sprintf(formatString, studyWeights$weights), " %")

  return(studyWeights[, c("y", "label")])
}

# Character counts drive the relative spacing between the interval column and
# the auxiliary tests/weights column on the right.
.forestPlotPanelMaxChars              <- function(data) {
  return(if (.forestPlotHasDataFrame(data)) max(nchar(data$label), na.rm = TRUE) else 0)
}
.forestPlotPrepareRightPanelData      <- function(plotData, options) {

  forestInformation     <- plotData[["forestInformation"]]
  additionalInformation <- plotData[["additionalInformation"]]

  if (!.forestPlotHasRightPanel(options, additionalInformation)) {
    return(NULL)
  }

  if (options[["forestPlotEstimatesAndConfidenceIntervals"]] && options[["forestPlotStudyInformationAggregateBy"]] == "") {
    if (.forestPlotHasDataFrame(additionalInformation)) {
      rightPanelAdditionalCis <- additionalInformation[, c("y", "est", "lCi", "uCi")]
    } else {
      rightPanelAdditionalCis <- NULL
    }

    if (.forestPlotHasDataFrame(forestInformation)) {
      studyCis            <- forestInformation[, c("y", "effectSize", "lCi", "uCi")]
      colnames(studyCis)  <- c("y", "est", "lCi", "uCi")
    } else {
      studyCis <- NULL
    }

    rightPanelCis <- .forestPlotFormatRightPanelIntervals(
      .forestPlotBindDataFrames(list(studyCis, rightPanelAdditionalCis)),
      options
    )
  } else {
    rightPanelCis <- NULL
  }

  if (.forestPlotHasDataFrame(additionalInformation)) {
    rightPanelAdditionalTests <- additionalInformation[, c("y", "test")]
    colnames(rightPanelAdditionalTests) <- c("y", "label")
  } else {
    rightPanelAdditionalTests <- NULL
  }

  if (isTRUE(options[["forestPlotStudyInformation"]]) &&
      options[["forestPlotStudyInformationStudyWeights"]] &&
      options[["forestPlotStudyInformationAggregateBy"]] == "" &&
      .forestPlotHasDataFrame(forestInformation)) {
    studyWeights <- .forestPlotBuildStudyWeightLabels(forestInformation, options)
  } else {
    studyWeights <- NULL
  }

  rightPanelTestsAndWeights <- .forestPlotBindDataFrames(list(studyWeights, rightPanelAdditionalTests))
  if (.forestPlotHasDataFrame(rightPanelTestsAndWeights)) {
    rightPanelTestsAndWeights <- rightPanelTestsAndWeights[rightPanelTestsAndWeights$label != "", , drop = FALSE]
  }
  if (!.forestPlotHasDataFrame(rightPanelTestsAndWeights)) {
    rightPanelTestsAndWeights <- NULL
  }

  maxCharsRightCis <- .forestPlotPanelMaxChars(rightPanelCis)
  maxCharsRightAdd <- .forestPlotPanelMaxChars(rightPanelTestsAndWeights)
  maxCharsRight    <- maxCharsRightCis + maxCharsRightAdd + 2

  if (.forestPlotHasDataFrame(rightPanelCis)) {
    rightPanelCis$x <- maxCharsRightCis / maxCharsRight
  }
  if (.forestPlotHasDataFrame(rightPanelTestsAndWeights)) {
    rightPanelTestsAndWeights$x <- (maxCharsRightCis + 2) / maxCharsRight
  }

  return(list(
    cis             = rightPanelCis,
    testsAndWeights = rightPanelTestsAndWeights,
    maxCharsRight   = maxCharsRight
  ))
}
# Add a text layer to a side panel plot, skipping when data is NULL.
.forestPlotAddTextLayer              <- function(plot, data, mapping, options, ...) {

  if (is.null(data))
    return(plot)

  return(plot + ggplot2::geom_text(
    data    = data,
.forestPlotBuildLeftPanel             <- function(plotData, options) {

  leftPanelData <- .forestPlotPrepareLeftPanelData(plotData, options)
  if (is.null(leftPanelData)) {
    return(NULL)
  }

  align    <- .forestPlotLeftPanelAlign(options)
  plotLeft <- ggplot2::ggplot()

  # column headers (bold)
  plotLeft <- .forestPlotAddTextLayer(plotLeft, leftPanelData[["titles"]], ggplot2::aes(
    x = x, y = y, label = title, hjust = alignment
  ), options, fontface = "bold")

  # color-mapped study column (color driven by label value)
  plotLeft <- .forestPlotAddTextLayer(plotLeft, leftPanelData[["studyDataColored"]], ggplot2::aes(
    x = x, y = y, label = label, hjust = alignment, color = label
  ), options)

  # remaining study columns
  plotLeft <- .forestPlotAddTextLayer(plotLeft, leftPanelData[["studyData"]], ggplot2::aes(
    x = x, y = y, label = label, hjust = alignment
  ), options)

  # estimate column headers (bold)
  plotLeft <- .forestPlotAddTextLayer(plotLeft, leftPanelData[["estimateTitles"]], ggplot2::aes(
    x = x, y = y, label = label, hjust = alignment
  ), options, fontface = "bold")

  # estimate-level multi-column data
  plotLeft <- .forestPlotAddTextLayer(plotLeft, leftPanelData[["additionalData"]], ggplot2::aes(
    x = x, y = y, label = label, hjust = alignment
  ), options)

  # additional section labels (EMM / model info, face mapped per row)
  plotLeft <- .forestPlotAddTextLayer(plotLeft, leftPanelData[["additionalInformation"]], ggplot2::aes(
    x = x, y = y, label = label, fontface = face
  ), options, hjust = align)

  # subgroup headings (bold face mapped per row)
  plotLeft <- .forestPlotAddTextLayer(plotLeft, leftPanelData[["subgroupHeadings"]], ggplot2::aes(
    x = x, y = y, label = label, fontface = face
  ), options, hjust = align)

  attr(plotLeft, "maxCharsLeft") <- leftPanelData[["maxCharsLeft"]]

  return(plotLeft)
}
.forestPlotBuildRightPanel            <- function(plotData, options) {

  rightPanelData <- .forestPlotPrepareRightPanelData(plotData, options)
  if (is.null(rightPanelData)) {
    return(NULL)
  }

  plotRight <- ggplot2::ggplot()

  if (!is.null(rightPanelData[["cis"]])) {
    plotRight <- plotRight + ggplot2::geom_text(
      data    = rightPanelData[["cis"]],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        label = label
      ),
      na.rm  = TRUE,
      hjust  = "right",
      family = "mono",
      size   = 4 * options[["forestPlotRelativeSizeText"]]
    )
  }

  if (!is.null(rightPanelData[["testsAndWeights"]])) {
    plotRight <- plotRight + ggplot2::geom_text(
      data    = rightPanelData[["testsAndWeights"]],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        label = label
      ),
      na.rm  = TRUE,
      hjust  = "left",
      family = "mono",
      size   = 4 * options[["forestPlotRelativeSizeText"]]
    )
  }

  attr(plotRight, "maxCharsRight") <- rightPanelData[["maxCharsRight"]]

  return(plotRight)
}

# ── Axis construction & theme ──────────────────────────────────────────────────
# Axis spec is based on every x/y value that can affect the rendered output,
# including hidden overflow objects that need padding for arrows.
.forestPlotBuildAxisSpec              <- function(plotData, options) {

  xValuesRaw <- c(
    .forestPlotCollectNumericValues(plotData[["forestInformation"]], c("lCi", "uCi")),
    .forestPlotCollectNumericValues(plotData[["forestObjects"]], c("x", "min", "max")),
    .forestPlotCollectNumericValues(plotData[["additionalInformation"]], c("lCi", "uCi")),
    .forestPlotCollectNumericValues(plotData[["additionalObjects"]], c("x"))
  )
  yValues <- c(
    .forestPlotCollectNumericValues(plotData[["forestInformation"]], c("y")),
    .forestPlotCollectNumericValues(plotData[["forestObjects"]], c("y")),
    .forestPlotCollectNumericValues(plotData[["additionalInformation"]], c("y")),
    .forestPlotCollectNumericValues(plotData[["additionalObjects"]], c("y"))
  )

  transformationSpec <- .forestPlotTransformationSpec(options)
  labelsOnly         <- .forestPlotTransformXAxisLabelsOnly(options)

  # Bounded transforms in labels-only mode: work entirely on the original
  # (plotted) scale to avoid inverse-transform inflation near boundaries
  if (labelsOnly && transformationSpec[["tickStyle"]] == "bounded") {
    bounded <- .forestPlotBoundedAxisSpec(xValuesRaw, transformationSpec, options)
    xRange  <- bounded[["range"]]
    xBreaks <- bounded[["breaks"]]
    xLabels <- bounded[["labels"]]
  } else {
    # When data is already transformed (not labels-only), use standard pretty
    # ticks for uniform spacing on the plotted scale
    tickSpec <- if (labelsOnly) {
      transformationSpec
    } else {
      modSpec <- transformationSpec
      modSpec[["tickStyle"]] <- "pretty"
      modSpec
    }

    xValuesDisplay <- if (labelsOnly) {
      .forestPlotTransformAxisValues(xValuesRaw, options)
    } else {
      xValuesRaw
    }

    xAxis <- .forestPlotResolveDisplayXAxis(xValuesDisplay, options, tickSpec)

    xLabels <- if (!is.null(xAxis[["labels"]])) {
      xAxis[["labels"]]
    } else {
      .forestPlotResolveXAxisLabels(xAxis[["breaks"]], options)
    }

    xRange  <- if (labelsOnly) .forestPlotInverseAxisValues(xAxis[["range"]],  transformationSpec) else xAxis[["range"]]
    xBreaks <- if (labelsOnly) .forestPlotInverseAxisValues(xAxis[["breaks"]], transformationSpec) else xAxis[["breaks"]]
  }

  xPadding <- .forestPlotResolveXAxisPadding(plotData, xRange)
  yRange   <- c(min(yValues, na.rm = TRUE) - options[["forestPlotRelativeSizeRow"]], 0)

  return(list(
    xBreaks  = xBreaks,
    xLabels  = xLabels,
    xPadding = xPadding,
    xRange   = sort(xRange),
    yRange   = yRange
  ))
}
.forestPlotApplyForestTheme           <- function(plotForest, axisSpec, options) {

  if (options[["forestPlotAuxiliaryEffectLabel"]] != "Effect Size") {
    xLabel <- options[["forestPlotAuxiliaryEffectLabel"]]
  } else if (options[["transformEffectSize"]] == "none") {
    xLabel <- gettext("Effect Size")
  } else {
    xLabel <- .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])
  }

  plotForest <- plotForest + jaspGraphs::scale_x_continuous(
    breaks = axisSpec[["xBreaks"]],
    labels = axisSpec[["xLabels"]]
  ) +
    ggplot2::coord_cartesian(
      xlim   = axisSpec[["xRange"]],
      ylim   = axisSpec[["yRange"]],
      expand = FALSE,
      clip   = "off"
    ) +
    ggplot2::xlab(xLabel) +
    ggplot2::theme(
      axis.line.y      = ggplot2::element_blank(),
      axis.line.x      = ggplot2::element_line(color = "black"),
      axis.text.y      = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(color = "black", size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
      axis.ticks.y     = ggplot2::element_blank(),
      axis.title.y     = ggplot2::element_blank(),
      axis.title.x     = ggplot2::element_text(color = "black", size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
      legend.position  = "none",
      panel.background = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background  = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(
        t = 5.5,
        r = 5.5 + if (axisSpec[["xPadding"]][["right"]] > 0) 24 else 0,
        b = 5.5,
        l = 5.5 + if (axisSpec[["xPadding"]][["left"]] > 0) 24 else 0
      )
    )

  return(plotForest)
}

# Shared clipping helpers keep points/intervals inside coord_cartesian() while
# leaving enough information to draw overflow markers at the panel edge.
.forestPlotClipXAxisValues            <- function(values, xRange) {
  return(pmin(pmax(values, xRange[1]), xRange[2]))
}
.forestPlotClipXAxisColumns           <- function(data, columns, xRange) {

  if (!.forestPlotHasDataFrame(data)) {
    return(data)
  }

  for (column in intersect(columns, colnames(data))) {
    data[[column]] <- .forestPlotClipXAxisValues(data[[column]], xRange)
  }

  return(data)
}
.forestPlotMaskOutsideXAxis           <- function(data, column, xRange) {

  if (!.forestPlotHasDataFrame(data) || !column %in% colnames(data)) {
    return(data)
  }

  data[[column]][data[[column]] < xRange[1] | data[[column]] > xRange[2]] <- NA_real_

  return(data)
}
.forestPlotResolveXAxisLabels         <- function(xBreaks, options) {

  if (options[["transformEffectSize"]] == "none") {
    return(ggplot2::waiver())
  }

  return(.forestPlotFormatAxisValues(
    values             = xBreaks,
    transformationSpec = .forestPlotTransformationSpec(options)
  ))
}
.forestPlotResolveVerticalLineValue   <- function(value, options) {

  if (!.forestPlotTransformXAxisLabelsOnly(options)) {
    return(value)
  }

  return(.forestPlotInverseAxisValues(
    value,
    .forestPlotTransformationSpec(options)
  ))
}

# Extra x-padding is only reserved when intervals or polygon objects extend past
# the visible range and therefore need arrow indicators outside the panel.
.forestPlotResolveXAxisPadding        <- function(plotData, xRange) {

  lower <- xRange[1]
  upper <- xRange[2]
  xSpan <- max(diff(xRange), .Machine$double.eps)

  estimateValues <- c(
    .forestPlotCollectNumericValues(plotData[["forestInformation"]], c("effectSize")),
    .forestPlotCollectNumericValues(plotData[["additionalInformation"]], c("est"))
  )
  lowerIntervalValues <- c(
    .forestPlotCollectNumericValues(plotData[["forestInformation"]], c("lCi")),
    .forestPlotCollectNumericValues(plotData[["additionalInformation"]], c("lCi"))
  )
  upperIntervalValues <- c(
    .forestPlotCollectNumericValues(plotData[["forestInformation"]], c("uCi")),
    .forestPlotCollectNumericValues(plotData[["additionalInformation"]], c("uCi"))
  )

  objectData <- .forestPlotBindDataFrames(list(
    plotData[["forestObjects"]],
    plotData[["additionalObjects"]]
  ))
  if (.forestPlotHasDataFrame(objectData)) {
    objectData <- objectData[objectData$type %in% c("diamond", "rectangle"), , drop = FALSE]
  }

  return(c(
    left  = if (
      any(lowerIntervalValues < lower, na.rm = TRUE) ||
      any(estimateValues < lower, na.rm = TRUE) ||
      (.forestPlotHasDataFrame(objectData) && any(objectData$x < lower, na.rm = TRUE))
    ) {
      xSpan * 0.05
    } else {
      0
    },
    right = if (
      any(upperIntervalValues > upper, na.rm = TRUE) ||
      any(estimateValues > upper, na.rm = TRUE) ||
      (.forestPlotHasDataFrame(objectData) && any(objectData$x > upper, na.rm = TRUE))
    ) {
      xSpan * 0.05
    } else {
      0
    }
  ))
}

# ── Clipping, padding & overflow indicators ────────────────────────────────────
# Keep points/intervals inside coord_cartesian() while leaving enough
# information to draw overflow markers (arrows, closed-arrow polygons).

# Clipped interval rendering works in two stages: first classify each interval
# against the visible range, then build the concrete segment/cap/arrow layers.
.forestPlotFilterIntervalData         <- function(intervalData) {

  if (!.forestPlotHasDataFrame(intervalData)) {
    return(NULL)
  }

  intervalData <- intervalData[
    !is.na(intervalData$xmin) &
    !is.na(intervalData$xmax) &
    !is.na(intervalData$y),
    ,
    drop = FALSE
  ]
  if (nrow(intervalData) == 0) {
    return(NULL)
  }
  if (!"est" %in% colnames(intervalData)) {
    intervalData$est <- NA_real_
  }

  return(intervalData)
}
.forestPlotIntervalRowHeight         <- function(yValues) {

  ySteps <- diff(sort(unique(yValues)))
  ySteps <- abs(ySteps[ySteps != 0])

  return(if (length(ySteps) > 0) min(ySteps) else 1)
}
.forestPlotAnnotateClippedIntervals   <- function(intervalData, lower, upper, style) {

  intervalData$clippedXmin <- pmax(intervalData$xmin, lower)
  intervalData$clippedXmax <- pmin(intervalData$xmax, upper)
  intervalData$overlaps    <- intervalData$xmax >= lower & intervalData$xmin <= upper
  intervalData$entirelyLeft  <- intervalData$xmax < lower
  intervalData$entirelyRight <- intervalData$xmin > upper
  intervalData$leftOverflow  <- intervalData$xmin < lower
  intervalData$rightOverflow <- intervalData$xmax > upper
  intervalData$leftClosedArrow <- if (style == "object") {
    intervalData$leftOverflow
  } else {
    intervalData$entirelyLeft
  }
  intervalData$rightClosedArrow <- if (style == "object") {
    intervalData$rightOverflow
  } else {
    intervalData$entirelyRight
  }
  intervalData$leftOpenArrow  <- style == "interval" & intervalData$leftOverflow  & !intervalData$leftClosedArrow  & intervalData$overlaps
  intervalData$rightOpenArrow <- style == "interval" & intervalData$rightOverflow & !intervalData$rightClosedArrow & intervalData$overlaps
  intervalData$leftCap        <- intervalData$overlaps & !intervalData$leftOverflow  & intervalData$xmin >= lower
  intervalData$rightCap       <- intervalData$overlaps & !intervalData$rightOverflow & intervalData$xmax <= upper

  return(intervalData)
}
.forestPlotBuildClippedSegments       <- function(intervalData, style) {

  if (style != "interval" || !any(intervalData$overlaps)) {
    return(NULL)
  }

  return(data.frame(
    x    = intervalData$clippedXmin[intervalData$overlaps],
    xend = intervalData$clippedXmax[intervalData$overlaps],
    y    = intervalData$y[intervalData$overlaps],
    yend = intervalData$y[intervalData$overlaps]
  ))
}
.forestPlotBuildClippedCaps           <- function(intervalData, capHeight) {

  if (capHeight <= 0 || !any(intervalData$leftCap | intervalData$rightCap)) {
    return(NULL)
  }

  return(.forestPlotBindDataFrames(list(
    if (any(intervalData$leftCap)) {
      data.frame(
        x    = intervalData$xmin[intervalData$leftCap],
        xend = intervalData$xmin[intervalData$leftCap],
        y    = intervalData$y[intervalData$leftCap] - capHeight,
        yend = intervalData$y[intervalData$leftCap] + capHeight
      )
    },
    if (any(intervalData$rightCap)) {
      data.frame(
        x    = intervalData$xmax[intervalData$rightCap],
        xend = intervalData$xmax[intervalData$rightCap],
        y    = intervalData$y[intervalData$rightCap] - capHeight,
        yend = intervalData$y[intervalData$rightCap] + capHeight
      )
    }
  )))
}
.forestPlotClosedArrowBoundaryGap     <- function(style, padding, xSpan) {
  return(if (style == "object") xSpan * 0.01 else padding * 0.05)
}
.forestPlotPrepareClippedIntervalData <- function(intervalData, axisSpec, capHeight = 0, style = "interval") {

  intervalData <- .forestPlotFilterIntervalData(intervalData)
  if (is.null(intervalData)) {
    return(NULL)
  }

  lower        <- axisSpec[["xRange"]][1]
  upper        <- axisSpec[["xRange"]][2]
  xSpan        <- max(diff(axisSpec[["xRange"]]), .Machine$double.eps)
  rowHeight    <- .forestPlotIntervalRowHeight(intervalData$y)
  xPadding     <- axisSpec[["xPadding"]]

  # Mark each interval as overlapping, fully outside, or overflowing the visible
  # x-range so the renderer can swap between normal segments and arrow indicators.
  intervalData <- .forestPlotAnnotateClippedIntervals(intervalData, lower, upper, style)

  segments <- .forestPlotBuildClippedSegments(intervalData, style)
  caps     <- .forestPlotBuildClippedCaps(intervalData, capHeight)

  openArrows <- .forestPlotBindDataFrames(list(
    .forestPlotArrowSegments(
      yValues   = intervalData$y[intervalData$leftOpenArrow],
      xBoundary = lower,
      padding   = xPadding[["left"]],
      rowHeight = rowHeight,
      xSpan     = xSpan,
      direction = "left"
    ),
    .forestPlotArrowSegments(
      yValues   = intervalData$y[intervalData$rightOpenArrow],
      xBoundary = upper,
      padding   = xPadding[["right"]],
      rowHeight = rowHeight,
      xSpan     = xSpan,
      direction = "right"
    )
  ))

  closedArrows <- .forestPlotBindDataFrames(list(
    .forestPlotClosedArrowData(
      yValues    = intervalData$y[intervalData$leftClosedArrow],
      xBoundary  = lower,
      padding    = xPadding[["left"]],
      rowHeight  = rowHeight,
      boundaryGap = .forestPlotClosedArrowBoundaryGap(style, xPadding[["left"]], xSpan),
      direction  = "left"
    ),
    .forestPlotClosedArrowData(
      yValues    = intervalData$y[intervalData$rightClosedArrow],
      xBoundary  = upper,
      padding    = xPadding[["right"]],
      rowHeight  = rowHeight,
      boundaryGap = .forestPlotClosedArrowBoundaryGap(style, xPadding[["right"]], xSpan),
      direction  = "right"
    )
  ))

  return(list(
    segments     = segments,
    caps         = caps,
    openArrows   = openArrows,
    closedArrows = closedArrows
  ))
}

# Open arrows are just short line segments; closed arrows are polygons used for
# fully clipped objects that should still suggest off-panel mass.
.forestPlotArrowSegments             <- function(yValues, xBoundary, padding, rowHeight, xSpan, direction) {

  if (length(yValues) == 0) {
    return(NULL)
  }

  arrowHeight <- rowHeight * 0.18
  arrowLength <- if (padding > 0) {
    min(padding * 0.55, max(xSpan * 0.02, padding * 0.30))
  } else {
    max(xSpan * 0.025, .Machine$double.eps)
  }
  xBase <- xBoundary
  xTip <- if (direction == "left") {
    max(xBase - arrowLength, xBoundary - if (padding > 0) padding * 0.90 else arrowLength)
  } else {
    min(xBase + arrowLength, xBoundary + if (padding > 0) padding * 0.90 else arrowLength)
  }

  return(rbind(
    data.frame(
      x    = xTip,
      xend = xBase,
      y    = yValues,
      yend = yValues + arrowHeight
    ),
    data.frame(
      x    = xTip,
      xend = xBase,
      y    = yValues,
      yend = yValues - arrowHeight
    )
  ))
}
.forestPlotClosedArrowData           <- function(yValues, xBoundary, padding, rowHeight, boundaryGap, direction) {

  if (length(yValues) == 0 || padding <= 0) {
    return(NULL)
  }

  boundaryGap <- min(max(boundaryGap, 0), padding * 0.50)
  bodyHeight <- rowHeight * 0.07
  headHeight <- rowHeight * 0.22
  xNear      <- if (direction == "left") {
    xBoundary - boundaryGap
  } else {
    xBoundary + boundaryGap
  }
  xFar       <- if (direction == "left") {
    xBoundary - padding * 0.48
  } else {
    xBoundary + padding * 0.48
  }
  xTip       <- if (direction == "left") {
    xBoundary - padding * 0.95
  } else {
    xBoundary + padding * 0.95
  }

  return(do.call(rbind, lapply(seq_along(yValues), function(i) {
    yValue <- yValues[i]

    data.frame(
      id = paste(direction, i, sep = "_"),
      x  = c(xNear, xFar, xFar, xTip, xFar, xFar, xNear),
      y  = c(
        yValue - bodyHeight,
        yValue - bodyHeight,
        yValue - headHeight,
        yValue,
        yValue + headHeight,
        yValue + bodyHeight,
        yValue + bodyHeight
      )
    )
  })))
}

# Render the normalized clipped-interval data returned by
# .forestPlotPrepareClippedIntervalData().
.forestPlotAddClippedIntervalLayers  <- function(plotForest, clippedIntervals, color, lineWidth, overflowLineWidth) {

  if (is.null(clippedIntervals)) {
    return(plotForest)
  }

  if (.forestPlotHasDataFrame(clippedIntervals[["segments"]])) {
    plotForest <- plotForest + ggplot2::geom_segment(
      data        = clippedIntervals[["segments"]],
      mapping     = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color       = color,
      linewidth   = lineWidth
    )
  }

  if (.forestPlotHasDataFrame(clippedIntervals[["caps"]])) {
    plotForest <- plotForest + ggplot2::geom_segment(
      data        = clippedIntervals[["caps"]],
      mapping     = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color       = color,
      linewidth   = lineWidth
    )
  }

  if (.forestPlotHasDataFrame(clippedIntervals[["openArrows"]])) {
    plotForest <- plotForest + ggplot2::geom_segment(
      data        = clippedIntervals[["openArrows"]],
      mapping     = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color       = color,
      linewidth   = lineWidth
    )
  }

  if (.forestPlotHasDataFrame(clippedIntervals[["closedArrows"]])) {
    plotForest <- plotForest + ggplot2::geom_polygon(
      data        = clippedIntervals[["closedArrows"]],
      mapping     = ggplot2::aes(x = x, y = y, group = id),
      inherit.aes = FALSE,
      color       = color,
      fill        = color,
      linewidth   = overflowLineWidth
    )
  }

  return(plotForest)
}

# Diamonds/rectangles can overflow too, so collapse each polygon to a single
# xmin/xmax indicator before feeding it into the generic clipping pipeline.
.forestPlotPrepareObjectIndicatorData <- function(objects, axisSpec, skipTypes = character(0)) {

  if (!.forestPlotHasDataFrame(objects)) {
    return(NULL)
  }

  objects <- objects[objects$type %in% c("diamond", "rectangle"), , drop = FALSE]
  objects <- objects[!(objects$type %in% skipTypes), , drop = FALSE]
  objects <- objects[!is.na(objects$x) & !is.na(objects$y), , drop = FALSE]
  if (nrow(objects) == 0) {
    return(NULL)
  }

  objects <- do.call(rbind, lapply(split(objects, objects$id), function(objectData) {
    data.frame(
      xmin = min(objectData$x, na.rm = TRUE),
      xmax = max(objectData$x, na.rm = TRUE),
      y    = stats::median(objectData$y, na.rm = TRUE)
    )
  }))
  objects <- objects[
    objects$xmin < axisSpec[["xRange"]][1] |
    objects$xmax > axisSpec[["xRange"]][2],
    ,
    drop = FALSE
  ]
  if (nrow(objects) == 0) {
    return(NULL)
  }

  return(.forestPlotPrepareClippedIntervalData(
    intervalData = objects,
    axisSpec     = axisSpec,
    capHeight    = 0,
    style        = "object"
  ))
}
.forestPlotApplySidePanelTheme        <- function(plotPanel, yRange, options) {

  plotPanel <- plotPanel +
    ggplot2::coord_cartesian(
      xlim   = c(0, 1),
      ylim   = yRange,
      expand = FALSE
    ) +
    ggplot2::xlab("") +
    ggplot2::theme(
      axis.line        = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(color = NA, size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
      axis.ticks       = ggplot2::element_blank(),
      axis.title.y     = ggplot2::element_blank(),
      axis.title.x     = ggplot2::element_text(color = NA, size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
      legend.position  = "none",
      panel.background = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background  = ggplot2::element_blank()
    )

  return(plotPanel)
}

# ── Final composition ─────────────────────────────────────────────────────────
# The final output is either a single forest ggplot or a list of panel plots
# plus layout metadata for jaspGraphsPlot.
.forestPlotComposeOutput              <- function(plotForest, plotLeft, plotRight, plotData, options) {

  if (!is.null(plotLeft)) {
    maxCharsLeft <- attr(plotLeft, "maxCharsLeft")
  } else {
    maxCharsLeft <- NULL
  }
  if (!is.null(plotRight)) {
    maxCharsRight <- attr(plotRight, "maxCharsRight")
  } else {
    maxCharsRight <- NULL
  }

  plotsWidths <- c(
    if (!is.null(plotLeft))  options[["forestPlotRelativeSizeLeftPanel"]],
    options[["forestPlotRelativeSizeMiddlePanel"]],
    if (!is.null(plotRight)) options[["forestPlotRelativeSizeRightPanel"]]
  )

  if (options[["forestPlotAuxiliaryAdjustWidthBasedOnText"]] && length(plotsWidths) == 3) {
    plotsWidths[1] <- plotsWidths[1] * 2 * maxCharsLeft  / (maxCharsRight + maxCharsLeft)
    plotsWidths[3] <- plotsWidths[3] * 2 * maxCharsRight / (maxCharsRight + maxCharsLeft)
  }

  if (length(plotsWidths) != 1) {
    panelRatio <- 0

    if (!is.null(plotLeft)) {
      panelRatio <- panelRatio + options[["forestPlotRelativeSizeLeftPanel"]]
    }
    if (!is.null(plotRight)) {
      panelRatio <- panelRatio + options[["forestPlotRelativeSizeRightPanel"]]
    }

    panelRatio <- panelRatio / options[["forestPlotRelativeSizeMiddlePanel"]]
  }

  if (length(plotsWidths) == 1) {
    plotOut <- plotForest
    attr(plotOut, "isPanel") <- FALSE
    if (!is.null(plotData[["forestInformation"]])) {
      forestRowsOffset <- max(plotData[["forestInformation"]]$y)
    } else {
      forestRowsOffset <- 0
    }
    attr(plotOut, "rows")    <- plotData[["nextRow"]] + forestRowsOffset

    return(plotOut)
  }

  plotOut <- list()
  if (!is.null(plotLeft)) {
    plotOut <- c(plotOut, list(plotLeft))
  }
  plotOut <- c(plotOut, list(plotForest))
  if (!is.null(plotRight)) {
    plotOut <- c(plotOut, list(plotRight))
  }

  attr(plotOut, "isPanel")    <- TRUE
  attr(plotOut, "panelRatio") <- panelRatio
  attr(plotOut, "rows")       <- plotData[["nextRow"]] + if (!is.null(plotData[["forestInformation"]])) max(plotData[["forestInformation"]]$y) else 0
  attr(plotOut, "widths")     <- plotsWidths
  attr(plotOut, "layout")     <- matrix(1:length(plotOut), nrow = 1, ncol = length(plotOut), byrow = TRUE)

  return(plotOut)
}

# Safe numeric collector used by the axis and padding builders.
.forestPlotCollectNumericValues        <- function(dataFrame, columns) {

  if (!.forestPlotHasDataFrame(dataFrame)) {
    return(numeric(0))
  }

  availableColumns <- intersect(columns, colnames(dataFrame))
  if (length(availableColumns) == 0) {
    return(numeric(0))
  }

  values <- unlist(dataFrame[, availableColumns, drop = FALSE], use.names = FALSE)

  return(values[!is.na(values)])
}
# Build a ggplot layer from named aes/geom args, dropping NULLs so that
# conditional mappings (color, shape, fill) can be expressed inline.
.forestPlotGeomLayer                  <- function(geomFun, data, aes, ...) {
  aes     <- aes[!vapply(aes, is.null, logical(1))]
  extras  <- list(...)
  extras  <- extras[!vapply(extras, is.null, logical(1))]
  args    <- c(list(data = data, mapping = do.call(ggplot2::aes, aes)), extras)
  return(do.call(geomFun, args))
}
