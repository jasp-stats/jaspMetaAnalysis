# Forest plot clipping and overflow indicators.
#
# Classifies clipped intervals and renders caps, arrows, and boundary indicators.

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

.forestPlotPrepareClippedIntervalData <- function(intervalData, clipSpec, capHeight = 0,
                                                  style = "interval", heightScale = 1) {

  intervalData <- .forestPlotFilterIntervalData(intervalData)
  if (is.null(intervalData)) {
    return(NULL)
  }

  lower        <- clipSpec[["xRange"]][1]
  upper        <- clipSpec[["xRange"]][2]
  xSpan        <- max(diff(clipSpec[["xRange"]]), .Machine$double.eps)
  rowHeight    <- .forestPlotIntervalRowHeight(intervalData$y) * heightScale
  xPadding     <- clipSpec[["xPadding"]]

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

.forestPlotPrepareObjectIndicatorData <- function(objects, clipSpec, skipTypes = character(0), heightScale = 1) {

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
    objects$xmin < clipSpec[["xRange"]][1] |
    objects$xmax > clipSpec[["xRange"]][2],
    ,
    drop = FALSE
  ]
  if (nrow(objects) == 0) {
    return(NULL)
  }

  return(.forestPlotPrepareClippedIntervalData(
    intervalData = objects,
    clipSpec     = clipSpec,
    capHeight    = 0,
    style        = "object",
    heightScale  = heightScale
  ))
}
