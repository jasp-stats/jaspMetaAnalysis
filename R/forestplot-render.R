# Forest plot canvas and middle-panel rendering.
#
# Builds the canvas, renders study/aggregate objects, and composes the final plot.

# Orchestration and colors ----

.forestPlotRenderPlot                 <- function(plotData, options) {

  plotData       <- .forestPlotBuildPlotModel(plotData, options)
  axisSpec       <- .forestPlotBuildAxisSpec(plotData, options)
  canvasSpec     <- .forestPlotBuildCanvasSpec(plotData, axisSpec, options)
  canvasPlotData <- .forestPlotTransformPlotDataToCanvas(plotData, axisSpec, canvasSpec)
  canvasClipSpec <- .forestPlotBuildCanvasClipSpec(canvasSpec)

  plotCanvas <- .forestPlotBuildCanvas(canvasPlotData, canvasClipSpec, axisSpec, canvasSpec, options)

  return(.forestPlotRenderResult(plotCanvas, canvasSpec, options))
}

.forestPlotRenderResult               <- function(plot, canvasSpec, options) {
  return(list(
    plot   = plot,
    width  = canvasSpec[["plotWidth"]],
    height = .forestPlotPlotHeight(canvasSpec, options)
  ))
}

.forestPlotBuildPlotModel             <- function(plotData, options) {

  plotData[["forestInformation"]] <- .forestPlotAddColorKey(plotData[["forestInformation"]], options)
  plotData[["forestObjects"]]     <- .forestPlotAddColorKey(plotData[["forestObjects"]], options)
  plotData[["additionalObjects"]] <- .forestPlotAddObjectColorKey(plotData[["additionalObjects"]], options)

  return(plotData)
}

.forestPlotAddColorKey                <- function(data, options) {

  if (!.forestPlotHasDataFrame(data)) {
    return(data)
  }

  colorKey <- .forestPlotColorKeyColumn()
  if (colorKey %in% colnames(data)) {
    return(data)
  }

  colorVar <- options[["forestPlotMappingColor"]]
  if (colorVar != "" && colorVar %in% colnames(data)) {
    data[[colorKey]] <- as.character(data[[colorVar]])
  }

  return(data)
}

.forestPlotAddObjectColorKey          <- function(data, options) {

  data <- .forestPlotAddColorKey(data, options)
  if (!.forestPlotHasDataFrame(data) || !("mapColor" %in% colnames(data))) {
    return(data)
  }

  colorKey <- .forestPlotColorKeyColumn()
  if (!colorKey %in% colnames(data)) {
    data[[colorKey]] <- NA_character_
  }

  useMapColor <- !is.na(data[["mapColor"]]) & data[["mapColor"]] != ""
  data[[colorKey]][useMapColor] <- as.character(data[["mapColor"]][useMapColor])

  return(data)
}

.forestPlotColorRenderColumn          <- function(data, options) {

  if (options[["forestPlotMappingColor"]] == "" || !.forestPlotHasDataFrame(data)) {
    return("")
  }

  colorKey <- .forestPlotColorKeyColumn()
  if (colorKey %in% colnames(data)) {
    return(colorKey)
  }

  colorVar <- options[["forestPlotMappingColor"]]
  if (colorVar %in% colnames(data)) {
    return(colorVar)
  }

  return("")
}

.forestPlotColorRows                  <- function(data, colorColumn) {

  if (!.forestPlotHasDataFrame(data)) {
    return(logical(0))
  }

  if (colorColumn == "" || !colorColumn %in% colnames(data)) {
    return(rep(FALSE, nrow(data)))
  }

  return(!is.na(data[[colorColumn]]) & data[[colorColumn]] != "")
}

# Canvas specification and transforms ----

.forestPlotBuildCanvasSpec           <- function(plotData, axisSpec, options) {

  leftPanelData     <- .forestPlotPrepareLeftPanelData(plotData, options)
  rightPanelData    <- .forestPlotPrepareRightPanelData(plotData, options)
  leftPanelWidth    <- .forestPlotLeftPanelWidthMm(leftPanelData, options)
  middlePanelWidth  <- .forestPlotMiddlePanelWidthMm(options)
  rightPanelWidth   <- .forestPlotRightPanelWidthMm(rightPanelData, options)
  xSpan             <- max(diff(axisSpec[["xRange"]]), .Machine$double.eps)
  leftOverflowWidth  <- axisSpec[["xPadding"]][["left"]]  / xSpan * middlePanelWidth
  rightOverflowWidth <- axisSpec[["xPadding"]][["right"]] / xSpan * middlePanelWidth

  plotStart  <- leftPanelWidth + leftOverflowWidth
  plotEnd    <- plotStart + middlePanelWidth
  rightStart <- plotEnd + rightOverflowWidth
  totalWidth <- rightStart + rightPanelWidth
  rowSize  <- .forestPlotRowSize(options)
  axisBand <- .forestPlotAxisBand(axisSpec[["yRange"]], options)
  yRange   <- c(axisBand[["bottomY"]], axisSpec[["yRange"]][2])

  return(list(
    leftPanelData      = leftPanelData,
    rightPanelData     = rightPanelData,
    leftPanelWidth     = leftPanelWidth,
    leftOverflowWidth  = leftOverflowWidth,
    plotStart          = plotStart,
    plotEnd            = plotEnd,
    plotWidthMm        = middlePanelWidth,
    rightOverflowWidth = rightOverflowWidth,
    rightPanelStart    = rightStart,
    rightPanelWidth    = rightPanelWidth,
    totalWidth         = totalWidth,
    plotWidth          = .forestPlotWidthToPixels(totalWidth),
    rowSize            = rowSize,
    axisY              = axisBand[["axisY"]],
    axisTickY          = axisBand[["tickY"]],
    axisLabelY         = axisBand[["labelY"]],
    axisTitleY         = axisBand[["titleY"]],
    axisHeight         = axisBand[["heightPx"]],
    yRange             = yRange,
    rows               = plotData[["rowCount"]]
  ))
}

.forestPlotLeftPanelWidthMm          <- function(leftPanelData, options) {

  if (is.null(leftPanelData)) {
    return(0)
  }

  align <- .forestPlotLeftPanelAlign(options)
  measuredWidth <- .forestPlotMeasurePanelWidthMm(
    .forestPlotLeftPanelTextData(leftPanelData, align),
    options,
    side = "left"
  )
  columnWidth <- leftPanelData[["widthMm"]]
  if (is.null(columnWidth) || !is.finite(columnWidth)) {
    columnWidth <- 0
  }

  requiredWidth <- max(
    measuredWidth,
    columnWidth + .forestPlotPanelPaddingMm(options, "left")
  )

  return(requiredWidth * .forestPlotPositiveOption(options, "forestPlotSizeLeftPanel"))
}

.forestPlotRightPanelWidthMm         <- function(rightPanelData, options) {

  if (is.null(rightPanelData)) {
    return(0)
  }

  requiredWidth <- .forestPlotMeasurePanelWidthMm(
    .forestPlotRightPanelTextData(rightPanelData),
    options,
    family = "mono",
    side   = "right"
  )

  return(requiredWidth * .forestPlotPositiveOption(options, "forestPlotSizeRightPanel"))
}

.forestPlotTransformPlotDataToCanvas <- function(plotData, axisSpec, canvasSpec) {

  canvasPlotData <- plotData
  canvasPlotData[["forestInformation"]] <- .forestPlotTransformDataFrameXToCanvas(
    canvasPlotData[["forestInformation"]],
    axisSpec, canvasSpec,
    c("effectSize", "lCi", "uCi", "lCi2", "uCi2")
  )
  canvasPlotData[["forestObjects"]] <- .forestPlotTransformDataFrameXToCanvas(
    canvasPlotData[["forestObjects"]],
    axisSpec, canvasSpec,
    c("x", "min", "lower", "middle", "upper", "max")
  )
  canvasPlotData[["additionalInformation"]] <- .forestPlotTransformDataFrameXToCanvas(
    canvasPlotData[["additionalInformation"]],
    axisSpec, canvasSpec,
    c("est", "lCi", "uCi")
  )
  canvasPlotData[["additionalObjects"]] <- .forestPlotTransformDataFrameXToCanvas(
    canvasPlotData[["additionalObjects"]],
    axisSpec, canvasSpec,
    c("x")
  )

  return(canvasPlotData)
}

.forestPlotTransformDataFrameXToCanvas <- function(data, axisSpec, canvasSpec, columns) {

  if (!.forestPlotHasDataFrame(data)) {
    return(data)
  }

  for (column in intersect(columns, colnames(data))) {
    data[[column]] <- .forestPlotTransformXToCanvas(data[[column]], axisSpec, canvasSpec)
  }

  return(data)
}

.forestPlotBuildCanvasClipSpec       <- function(canvasSpec) {

  return(list(
    xRange   = c(canvasSpec[["plotStart"]], canvasSpec[["plotEnd"]]),
    xPadding = c(
      left  = canvasSpec[["leftOverflowWidth"]],
      right = canvasSpec[["rightOverflowWidth"]]
    )
  ))
}

.forestPlotTransformXToCanvas        <- function(x, axisSpec, canvasSpec) {

  xSpan <- max(diff(axisSpec[["xRange"]]), .Machine$double.eps)
  return(
    canvasSpec[["plotStart"]] +
      (x - axisSpec[["xRange"]][1]) / xSpan * canvasSpec[["plotWidthMm"]]
  )
}

# Canvas layers ----

.forestPlotBuildCanvas               <- function(plotData, canvasClipSpec, axisSpec, canvasSpec, options) {

  plotCanvas <- ggplot2::ggplot()
  plotCanvas <- .forestPlotAddStudyObjects(plotCanvas, plotData, canvasClipSpec, options)
  plotCanvas <- .forestPlotAddAdditionalObjects(plotCanvas, plotData, canvasClipSpec, options)
  plotCanvas <- .forestPlotAddCanvasVerticalLines(plotCanvas, axisSpec, canvasSpec, options)
  plotCanvas <- .forestPlotAddCanvasSidePanels(plotCanvas, canvasSpec, options)
  plotCanvas <- .forestPlotAddCanvasAxis(plotCanvas, axisSpec, canvasSpec, options)
  plotCanvas <- .forestPlotApplyCanvasTheme(plotCanvas, canvasSpec)

  return(plotCanvas)
}

.forestPlotAddCanvasVerticalLines    <- function(plotCanvas, axisSpec, canvasSpec, options) {

  bodyYRange <- axisSpec[["yRange"]]

  if (options[["forestPlotAuxiliaryAddVerticalLine"]]) {
    x <- .forestPlotTransformXToCanvas(
      .forestPlotResolveVerticalLineValue(options[["forestPlotAuxiliaryAddVerticalLineValue"]], options),
      axisSpec,
      canvasSpec
    )
    plotCanvas <- plotCanvas + ggplot2::geom_segment(
      data        = data.frame(x = x, xend = x, y = canvasSpec[["axisY"]], yend = bodyYRange[2]),
      mapping     = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      linetype    = "dashed"
    )
  }

  if (options[["forestPlotAuxiliaryAddVerticalLine2"]]) {
    x <- .forestPlotTransformXToCanvas(
      .forestPlotResolveVerticalLineValue(options[["forestPlotAuxiliaryAddVerticalLineValue2"]], options),
      axisSpec,
      canvasSpec
    )
    plotCanvas <- plotCanvas + ggplot2::geom_segment(
      data        = data.frame(x = x, xend = x, y = canvasSpec[["axisY"]], yend = bodyYRange[2]),
      mapping     = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      linetype    = "dotted"
    )
  }

  return(plotCanvas)
}

.forestPlotAddCanvasSidePanels       <- function(plotCanvas, canvasSpec, options) {

  plotCanvas <- .forestPlotAddCanvasLeftPanel(plotCanvas, canvasSpec, options)
  plotCanvas <- .forestPlotAddCanvasRightPanel(plotCanvas, canvasSpec, options)

  return(plotCanvas)
}

.forestPlotAddCanvasLeftPanel        <- function(plotCanvas, canvasSpec, options) {

  leftPanelData <- canvasSpec[["leftPanelData"]]
  if (is.null(leftPanelData) || canvasSpec[["leftPanelWidth"]] <= 0) {
    return(plotCanvas)
  }

  start <- 0
  width <- canvasSpec[["leftPanelWidth"]]
  align <- .forestPlotLeftPanelAlign(options)

  plotCanvas <- .forestPlotAddCanvasTextLayer(
    plotCanvas,
    .forestPlotCanvasTextData(leftPanelData[["titles"]], start, width, "title", "alignment", fontface = "bold"),
    options
  )
  plotCanvas <- .forestPlotAddCanvasTextLayer(
    plotCanvas,
    .forestPlotCanvasTextData(leftPanelData[["studyDataColored"]], start, width, "label", "alignment"),
    options,
    colorColumn = "label"
  )
  plotCanvas <- .forestPlotAddCanvasTextLayer(
    plotCanvas,
    .forestPlotCanvasTextData(leftPanelData[["studyData"]], start, width, "label", "alignment"),
    options
  )
  plotCanvas <- .forestPlotAddCanvasTextLayer(
    plotCanvas,
    .forestPlotCanvasTextData(
      leftPanelData[["estimateTitles"]],
      start, width, "label", "alignment",
      fontface = "bold"
    ),
    options
  )
  plotCanvas <- .forestPlotAddCanvasTextLayer(
    plotCanvas,
    .forestPlotCanvasTextData(leftPanelData[["additionalData"]], start, width, "label", "alignment"),
    options
  )
  plotCanvas <- .forestPlotAddCanvasTextLayer(
    plotCanvas,
    .forestPlotCanvasTextData(
      leftPanelData[["additionalInformation"]],
      start, width, "label",
      hjust = align, fontfaceColumn = "face"
    ),
    options
  )
  plotCanvas <- .forestPlotAddCanvasTextLayer(
    plotCanvas,
    .forestPlotCanvasTextData(
      leftPanelData[["subgroupHeadings"]],
      start, width, "label",
      hjust = align, fontfaceColumn = "face"
    ),
    options
  )

  return(plotCanvas)
}

.forestPlotAddCanvasRightPanel       <- function(plotCanvas, canvasSpec, options) {

  rightPanelData <- canvasSpec[["rightPanelData"]]
  if (is.null(rightPanelData) || canvasSpec[["rightPanelWidth"]] <= 0) {
    return(plotCanvas)
  }

  start <- canvasSpec[["rightPanelStart"]]
  width <- canvasSpec[["rightPanelWidth"]]

  plotCanvas <- .forestPlotAddCanvasTextLayer(
    plotCanvas,
    .forestPlotCanvasTextData(rightPanelData[["cis"]], start, width, "label", hjust = 1),
    options,
    family = "mono"
  )
  plotCanvas <- .forestPlotAddCanvasTextLayer(
    plotCanvas,
    .forestPlotCanvasTextData(rightPanelData[["testsAndWeights"]], start, width, "label", "hjust"),
    options,
    family = "mono"
  )

  return(plotCanvas)
}

.forestPlotCanvasTextData            <- function(data, start, width, labelColumn, hjustColumn = "hjust",
                                                 hjust = NULL, fontface = "plain", fontfaceColumn = NULL) {

  textData <- .forestPlotTextData(
    data           = data,
    labelColumn    = labelColumn,
    hjustColumn    = hjustColumn,
    hjust          = hjust,
    fontface       = fontface,
    fontfaceColumn = fontfaceColumn,
    includeY       = TRUE
  )
  if (is.null(textData)) {
    return(NULL)
  }

  textData$x <- start + textData$x * width

  return(textData)
}

.forestPlotAddCanvasTextLayer        <- function(plotCanvas, data, options, colorColumn = NULL, family = NULL) {

  if (!.forestPlotHasDataFrame(data)) {
    return(plotCanvas)
  }

  return(plotCanvas + .forestPlotGeomLayer(
    ggplot2::geom_text,
    data,
    aes = list(
      x        = as.name("x"),
      y        = as.name("y"),
      label    = as.name("label"),
      hjust    = as.name("hjust"),
      fontface = as.name("fontface"),
      color    = if (!is.null(colorColumn)) as.name(colorColumn)
    ),
    na.rm = TRUE,
    size  = 4 * options[["forestPlotSizeText"]],
    vjust = "middle",
    family = family
  ))
}

.forestPlotAddCanvasAxis             <- function(plotCanvas, axisSpec, canvasSpec, options) {

  axisY  <- canvasSpec[["axisY"]]
  breaks <- .forestPlotTransformXToCanvas(axisSpec[["xBreaks"]], axisSpec, canvasSpec)
  labels <- .forestPlotCanvasAxisLabels(axisSpec)
  axisData <- data.frame(x = breaks, label = labels)
  axisData$y <- canvasSpec[["axisLabelY"]]

  plotCanvas <- plotCanvas + ggplot2::geom_segment(
    data        = data.frame(
      x    = canvasSpec[["plotStart"]],
      xend = canvasSpec[["plotEnd"]],
      y    = axisY,
      yend = axisY
    ),
    mapping     = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color       = "black"
  )
  plotCanvas <- plotCanvas + ggplot2::geom_segment(
    data        = data.frame(x = breaks, xend = breaks, y = axisY, yend = canvasSpec[["axisTickY"]]),
    mapping     = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color       = "black"
  )
  plotCanvas <- plotCanvas + ggplot2::geom_text(
    data        = axisData,
    mapping     = ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size        = 4 * .forestPlotAxisLabelSize(options),
    vjust       = 1,
    na.rm       = TRUE
  )
  plotCanvas <- plotCanvas + ggplot2::geom_text(
    data        = data.frame(
      x     = (canvasSpec[["plotStart"]] + canvasSpec[["plotEnd"]]) / 2,
      y     = canvasSpec[["axisTitleY"]],
      label = .forestPlotCanvasAxisTitle(options)
    ),
    mapping     = ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size        = 4 * .forestPlotAxisLabelSize(options),
    vjust       = 1,
    na.rm       = TRUE
  )

  return(plotCanvas)
}

.forestPlotCanvasAxisLabels          <- function(axisSpec) {

  labels <- axisSpec[["xLabels"]]
  if (inherits(labels, "waiver")) {
    return(format(axisSpec[["xBreaks"]], trim = TRUE, scientific = FALSE))
  }
  if (is.function(labels)) {
    return(as.character(labels(axisSpec[["xBreaks"]])))
  }

  return(as.character(labels))
}

.forestPlotCanvasAxisTitle           <- function(options) {

  if (options[["forestPlotAuxiliaryEffectLabel"]] != "Effect Size") {
    return(options[["forestPlotAuxiliaryEffectLabel"]])
  }
  if (options[["transformEffectSize"]] == "none") {
    return(gettext("Effect Size"))
  }

  return(.maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]]))
}

.forestPlotApplyCanvasTheme          <- function(plotCanvas, canvasSpec) {

  return(plotCanvas +
    ggplot2::coord_cartesian(
      xlim   = c(0, canvasSpec[["totalWidth"]]),
      ylim   = canvasSpec[["yRange"]],
      expand = FALSE,
      clip   = "off"
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(
      axis.line        = ggplot2::element_blank(),
      axis.text        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      axis.title       = ggplot2::element_blank(),
      legend.position  = "none",
      panel.background = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background  = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
    ))
}

# Study and additional objects ----

.forestPlotAddStudyObjects            <- function(plotForest, plotData, clipSpec, options) {

  if (!options[["forestPlotStudyInformation"]]) {
    return(plotForest)
  }

  if (options[["forestPlotStudyInformationAggregateBy"]] != "") {
    return(.forestPlotAddAggregateObjects(plotForest, plotData[["forestObjects"]], clipSpec, options))
  }

  plotForest <- .forestPlotAddPredictionDiamonds(plotForest, plotData[["forestObjects"]], clipSpec, options)
  plotForest <- .forestPlotAddStudyPoints(plotForest, plotData[["forestInformation"]], clipSpec, options)

  return(plotForest)
}

.forestPlotAddAggregateObjects       <- function(plotForest, forestObjects, clipSpec, options) {

  if (!.forestPlotHasDataFrame(forestObjects)) {
    return(plotForest)
  }

  estimateSize <- .forestPlotEstimateSize(options)

  if (options[["forestPlotStudyInformationAggregateMethod"]] == "boxplot") {
    forestObjects <- forestObjects[forestObjects$type == "boxplot", , drop = FALSE]
    forestObjects <- .forestPlotClipXAxisColumns(
      forestObjects,
      c("min", "lower", "middle", "upper", "max"),
      clipSpec[["xRange"]]
    )
    colorColumn   <- .forestPlotColorRenderColumn(forestObjects, options)

    if (colorColumn == "") {
      plotForest <- .forestPlotAddAggregateBoxplotLayer(
        plotForest, forestObjects, colorColumn = "", fill = "grey20", estimateSize = estimateSize
      )
    } else {
      coloredRows <- .forestPlotColorRows(forestObjects, colorColumn)
      plotForest <- .forestPlotAddAggregateBoxplotLayer(
        plotForest, forestObjects[!coloredRows, , drop = FALSE],
        colorColumn = "", fill = NA, estimateSize = estimateSize
      )
      plotForest <- .forestPlotAddAggregateBoxplotLayer(
        plotForest, forestObjects[coloredRows, , drop = FALSE],
        colorColumn = colorColumn, fill = NULL, estimateSize = estimateSize
      )
    }
  }

  if (options[["forestPlotStudyInformationAggregateMethod"]] == "bubbles") {
    forestObjects <- forestObjects[forestObjects$type == "bubbles", , drop = FALSE]
    forestObjects <- .forestPlotClipXAxisColumns(forestObjects, c("x"), clipSpec[["xRange"]])
    colorColumn   <- .forestPlotColorRenderColumn(forestObjects, options)

    plotForest <- plotForest + .forestPlotGeomLayer(
      jaspGraphs::geom_point, forestObjects,
      aes = list(
        y = as.name("y"), x = as.name("x"), size = as.name("weight"),
        fill  = if (colorColumn != "") as.name(colorColumn),
        color = if (colorColumn != "") as.name(colorColumn)
      ),
      fill = if (colorColumn == "") "grey20", alpha = 0.8, na.rm = TRUE,
      position = ggplot2::position_jitter(width = 0, height = 0.10)
    ) + ggplot2::scale_size(range = c(1.5, 10) *
      options[["forestPlotStudyInformationAggregateMethodBubbleRelativeSize"]] *
      estimateSize)
  }

  return(plotForest)
}

.forestPlotAddAggregateBoxplotLayer  <- function(plotForest, forestObjects, colorColumn, fill, estimateSize) {

  if (!.forestPlotHasDataFrame(forestObjects)) {
    return(plotForest)
  }

  return(plotForest + .forestPlotGeomLayer(
    ggplot2::geom_boxplot, forestObjects,
    aes = list(
      y = as.name("y"), group = as.name("id"),
      xmin = as.name("min"), xlower = as.name("lower"), xmiddle = as.name("middle"),
      xupper = as.name("upper"), xmax = as.name("max"),
      fill = if (colorColumn != "") as.name(colorColumn)
    ),
    fill        = fill,
    alpha       = 0.8,
    orientation = "y",
    stat        = "identity",
    width       = 0.6 * estimateSize,
    na.rm       = TRUE
  ))
}

.forestPlotAddPredictionDiamonds     <- function(plotForest, forestObjects, clipSpec, options) {

  if (!options[["forestPlotStudyInformationPredictedEffects"]] || !.forestPlotHasDataFrame(forestObjects)) {
    return(plotForest)
  }

  forestPrediction <- forestObjects[forestObjects$type == "diamond", , drop = FALSE]
  forestPrediction <- .forestPlotClipXAxisColumns(forestPrediction, c("x"), clipSpec[["xRange"]])
  forestPrediction <- .forestPlotScaleObjectHeight(forestPrediction, options)
  colorColumn      <- .forestPlotColorRenderColumn(forestPrediction, options)

  plotForest <- plotForest + .forestPlotGeomLayer(
    ggplot2::geom_polygon, forestPrediction,
    aes = list(
      x = as.name("x"), y = as.name("y"), group = as.name("id"),
      fill = if (colorColumn != "") as.name(colorColumn)
    ),
    fill = if (colorColumn == "") "grey20", alpha = 0.8
  )

  return(plotForest)
}

.forestPlotAddStudyPoints            <- function(plotForest, forestInformation, clipSpec, options) {

  if (!.forestPlotHasDataFrame(forestInformation)) {
    return(plotForest)
  }

  colorColumn  <- .forestPlotColorRenderColumn(forestInformation, options)
  shapeVar     <- options[["forestPlotMappingShape"]]
  estimateSize <- .forestPlotEstimateSize(options)

  forestInformationPoints <- .forestPlotMaskOutsideXAxis(forestInformation, "effectSize", clipSpec[["xRange"]])

  plotForest <- plotForest + .forestPlotGeomLayer(
    ggplot2::geom_point, forestInformationPoints,
    aes = list(
      x     = as.name("effectSize"), y = as.name("y"), size = as.name("weights"),
      color = if (colorColumn != "") as.name(colorColumn),
      shape = if (shapeVar != "") as.name(shapeVar)
    ),
    color = if (colorColumn == "") options[["forestPlotAuxiliaryPlotColor"]],
    shape = if (shapeVar == "") 15,
    na.rm = TRUE
  ) + ggplot2::scale_size(range = c(1, 6) * estimateSize)

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
      clipSpec = clipSpec, capHeight = 0, style = "interval", heightScale = estimateSize
    ),
    color = "black", lineWidth = 0.5 * estimateSize, overflowLineWidth = estimateSize
  )

  # Secondary confidence intervals
  if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
    plotForest <- .forestPlotAddClippedIntervalLayers(
      plotForest        = plotForest,
      clippedIntervals  = .forestPlotPrepareClippedIntervalData(
        intervalData = data.frame(xmin = forestInformation$lCi2, xmax = forestInformation$uCi2,
                                   y = forestInformation$y, est = forestInformation$effectSize),
        clipSpec = clipSpec, capHeight = 0.3 * estimateSize, style = "interval", heightScale = estimateSize
      ),
      color = "darkblue", lineWidth = 0.5 * estimateSize, overflowLineWidth = estimateSize
    )
  }

  return(plotForest)
}

.forestPlotAddAdditionalObjects       <- function(plotForest, plotData, clipSpec, options) {

  additionalObjectsRaw <- plotData[["additionalObjects"]]
  forestObjects        <- plotData[["forestObjects"]]
  additionalObjects    <- additionalObjectsRaw
  estimateSize         <- .forestPlotEstimateSize(options)

  if (!.forestPlotHasDataFrame(additionalObjects)) {
    additionalObjects <- NULL
  } else {
    additionalObjects <- .forestPlotClipXAxisColumns(
      data    = additionalObjects,
      columns = c("x"),
      xRange  = clipSpec[["xRange"]]
    )
    additionalObjects <- .forestPlotScaleObjectHeight(additionalObjects, options)
  }

  colorColumn <- .forestPlotColorRenderColumn(additionalObjects, options)
  coloredRows <- .forestPlotColorRows(additionalObjects, colorColumn)

  if (.forestPlotHasDataFrame(additionalObjects) && any(coloredRows)) {
    plotForest <- plotForest + .forestPlotGeomLayer(
      ggplot2::geom_polygon,
      additionalObjects[coloredRows, , drop = FALSE],
      aes = list(
        x     = as.name("x"),
        y     = as.name("y"),
        group = as.name("id"),
        fill  = as.name(colorColumn)
      )
    )
  }

  if (.forestPlotHasDataFrame(additionalObjects) && any(!coloredRows)) {
    plotForest <- plotForest + ggplot2::geom_polygon(
      data    = additionalObjects[!coloredRows, , drop = FALSE],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        group = id
      ),
      fill = "grey20"
    )
  }

  objectIndicatorData <- .forestPlotPrepareObjectIndicatorData(
    objects   = .forestPlotBindDataFrames(list(
      .forestPlotObjectIndicatorInput(forestObjects),
      .forestPlotObjectIndicatorInput(additionalObjectsRaw)
    )),
    clipSpec    = clipSpec,
    skipTypes   = c("boxplot", "bubbles"),
    heightScale = estimateSize
  )
  plotForest <- .forestPlotAddClippedIntervalLayers(
    plotForest        = plotForest,
    clippedIntervals  = objectIndicatorData,
    color             = "grey20",
    lineWidth         = 0.6 * estimateSize,
    overflowLineWidth = 1.2 * estimateSize
  )

  return(plotForest)
}

# Geometry helpers ----

.forestPlotObjectIndicatorInput       <- function(objects) {

  requiredColumns <- c("id", "x", "y", "type")
  if (!.forestPlotHasDataFrame(objects) || !all(requiredColumns %in% colnames(objects))) {
    return(NULL)
  }

  objects <- objects[objects$type %in% c("diamond", "rectangle"), requiredColumns, drop = FALSE]
  if (!.forestPlotHasDataFrame(objects)) {
    return(NULL)
  }

  return(objects)
}

.forestPlotScaleObjectHeight         <- function(objects, options) {

  if (!.forestPlotHasDataFrame(objects) || !"id" %in% colnames(objects) || !"y" %in% colnames(objects)) {
    return(objects)
  }

  estimateSize <- .forestPlotEstimateSize(options)
  if (identical(estimateSize, 1)) {
    return(objects)
  }

  yCenter   <- ave(objects$y, objects$id, FUN = function(y) stats::median(y, na.rm = TRUE))
  objects$y <- yCenter + (objects$y - yCenter) * estimateSize

  return(objects)
}

.forestPlotMiddlePanelWidthMm        <- function(options) {
  return(.forestPlotMiddlePanelBaseWidthMm() * .forestPlotPositiveOption(options, "forestPlotSizePlotArea"))
}

.forestPlotMiddlePanelBaseWidthMm    <- function() {
  return(130)
}

.forestPlotWidthToPixels             <- function(widthMm) {
  return(500 * widthMm / .forestPlotMiddlePanelBaseWidthMm())
}

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

.forestPlotGeomLayer                  <- function(geomFun, data, aes, ...) {
  aes     <- aes[!vapply(aes, is.null, logical(1))]
  extras  <- list(...)
  extras  <- extras[!vapply(extras, is.null, logical(1))]
  args    <- c(list(data = data, mapping = do.call(ggplot2::aes, aes)), extras)
  return(do.call(geomFun, args))
}
