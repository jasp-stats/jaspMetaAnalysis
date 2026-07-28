# Forest plot axis construction.
#
# Builds axis specifications, labels, vertical lines, and overflow padding.

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
  yRange   <- c(min(yValues, na.rm = TRUE) - .forestPlotRowSize(options), 0)

  return(list(
    xBreaks  = xBreaks,
    xLabels  = xLabels,
    xPadding = xPadding,
    xRange   = sort(xRange),
    yRange   = yRange
  ))
}

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
    .forestPlotObjectIndicatorInput(plotData[["forestObjects"]]),
    .forestPlotObjectIndicatorInput(plotData[["additionalObjects"]])
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
