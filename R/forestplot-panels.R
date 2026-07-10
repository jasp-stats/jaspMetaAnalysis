# Forest plot side panels.
#
# Prepares and measures left/right text-panel content.

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
    subgroupHeadings = NULL, widthMm = NULL
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
  leftPanelData[["widthMm"]] <- attr(studyInfo, "widthMm")

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

  columnWidths  <- .forestPlotStudyInformationColumnWidths(estimateSettings, estimateInfo, options)
  relativeWidths <- .forestPlotStudyInformationRelativeWidths(estimateSettings, columnWidths)

  estimateSettings$xStart <- c(0, cumsum(relativeWidths[-length(relativeWidths)]))
  estimateSettings$xEnd   <- cumsum(relativeWidths)

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
  labelWidths <- .forestPlotMeasureTextWidthMm(info$label, options)
  if (length(labelWidths) > 0) {
    leftPanelData[["widthMm"]] <- max(c(leftPanelData[["widthMm"]], labelWidths), na.rm = TRUE)
  }

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
  if (!"weights" %in% colnames(forestInformation)) {
    return(NULL)
  }

  studyWeights         <- forestInformation[, c("y", "weights")]
  studyWeights$weights <- .forestPlotDisplayStudyWeights(studyWeights$weights, options)
  formatString         <- paste0("%1$.", .forestPlotStudyWeightDigits(options), "f")
  suffix               <- .forestPlotStudyWeightSuffix(options)
  studyWeights$label   <- ""
  nonMissingWeights    <- is.finite(studyWeights$weights)
  studyWeights$label[nonMissingWeights] <- paste0(
    sprintf(formatString, studyWeights$weights[nonMissingWeights]),
    suffix
  )
  studyWeights$type <- "weight"

  return(studyWeights[, c("y", "label", "type")])
}

.forestPlotUsesRawStudyWeights       <- function(options) {

  return(
    isTRUE(options[["analysis"]] == "standaloneForestPlot") &&
      !is.null(options[["weight"]]) &&
      options[["weight"]] != ""
  )
}

.forestPlotDisplayStudyWeights       <- function(weights, options) {

  if (!.forestPlotUsesRawStudyWeights(options) ||
      !isTRUE(options[["forestPlotStudyInformationBoxplotWeightsNormalized"]])) {
    return(weights)
  }

  weightSum <- sum(weights, na.rm = TRUE)
  if (!is.finite(weightSum) || weightSum <= 0) {
    return(weights)
  }

  return(weights / weightSum * 100)
}

.forestPlotStudyWeightSuffix         <- function(options) {

  if (.forestPlotUsesRawStudyWeights(options)) {
    return(if (isTRUE(options[["forestPlotStudyInformationBoxplotWeightsPercentage"]])) " %" else "")
  }

  return(" %")
}

.forestPlotStudyWeightDigits         <- function(options) {

  if (.forestPlotUsesRawStudyWeights(options)) {
    if (isTRUE(options[["forestPlotStudyInformationBoxplotWeightsNormalized"]]) ||
        isTRUE(options[["forestPlotStudyInformationBoxplotWeightsPercentage"]])) {
      return(1)
    }

    return(options[["forestPlotAuxiliaryDigits"]])
  }

  return(1)
}

.forestPlotPanelMaxChars              <- function(data) {
  if (!.forestPlotHasDataFrame(data) || !"label" %in% colnames(data)) {
    return(0)
  }

  chars <- nchar(data$label, type = "width")
  chars <- chars[!is.na(chars)]
  if (length(chars) == 0) {
    return(0)
  }

  return(max(chars))
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
    rightPanelAdditionalTests$type <- "test"
  } else {
    rightPanelAdditionalTests <- NULL
  }

  if (isTRUE(options[["forestPlotStudyInformation"]]) &&
      options[["forestPlotStudyInformationStudyWeights"]] &&
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
    rightPanelTestsAndWeights$x <- ifelse(
      rightPanelTestsAndWeights$type == "weight",
      1,
      (maxCharsRightCis + 2) / maxCharsRight
    )
    rightPanelTestsAndWeights$hjust <- ifelse(
      rightPanelTestsAndWeights$type == "weight",
      1,
      0
    )
  }

  return(list(
    cis             = rightPanelCis,
    testsAndWeights = rightPanelTestsAndWeights,
    maxCharsRight   = maxCharsRight
  ))
}

.forestPlotMeasureTextWidthMm        <- function(labels, options, family = "", fontface = "plain") {

  labels <- as.character(labels)
  if (length(labels) == 0) {
    return(numeric(0))
  }

  keep  <- !is.na(labels) & labels != ""
  widths <- rep(0, length(labels))
  if (!any(keep)) {
    return(widths)
  }

  if (length(fontface) == 1) {
    fontface <- rep(fontface, length(labels))
  } else {
    fontface <- rep_len(as.character(fontface), length(labels))
  }
  fontface <- fontface[keep]
  fontface[is.na(fontface) | fontface == ""] <- "plain"

  labelWidths <- vapply(labels[keep], .forestPlotEstimateTextWidthMm, numeric(1), family = family)
  labelWidths[fontface == "bold"] <- labelWidths[fontface == "bold"] * 1.06
  widths[keep] <- labelWidths * options[["forestPlotSizeText"]]

  return(widths)
}

.forestPlotEstimateTextWidthMm       <- function(label, family = "") {

  if (identical(family, "mono")) {
    return(nchar(label, type = "width") * 1.65)
  }

  characters <- strsplit(label, "", fixed = TRUE)[[1]]
  if (length(characters) == 0) {
    return(0)
  }

  widths <- rep(1.00, length(characters))
  widths[grepl("[ilI1\\.,:;!'\\|\\[\\]\\(\\)]", characters)] <- 0.45
  widths[grepl("[fjrt\\-]", characters)] <- 0.70
  widths[grepl("[MWmw@#%&]", characters)] <- 1.35
  widths[characters == " "] <- 0.55

  return(sum(widths) * 1.65)
}

.forestPlotNormalizeHjust            <- function(hjust) {

  if (is.null(hjust)) {
    return(NULL)
  }

  out <- suppressWarnings(as.numeric(hjust))
  missing <- is.na(out)
  if (any(missing)) {
    hjustText <- as.character(hjust[missing])
    hjustValues <- rep(0, length(hjustText))
    hjustValues[hjustText %in% c("middle", "center")] <- 0.5
    hjustValues[hjustText == "right"] <- 1
    out[missing] <- hjustValues
  }

  return(pmin(pmax(out, 0), 1))
}

.forestPlotMeasurePanelWidthMm       <- function(textData, options, family = "", side = "left") {

  if (!.forestPlotHasDataFrame(textData)) {
    return(0)
  }

  textData <- textData[!is.na(textData$label) & textData$label != "", , drop = FALSE]
  if (!.forestPlotHasDataFrame(textData)) {
    return(0)
  }

  if (!"fontface" %in% colnames(textData)) {
    textData$fontface <- "plain"
  }

  labelWidth <- .forestPlotMeasureTextWidthMm(textData$label, options, family = family, fontface = textData$fontface)
  x          <- pmin(pmax(as.numeric(textData$x), 0), 1)
  hjust      <- .forestPlotNormalizeHjust(textData$hjust)
  epsilon    <- 0.02

  leftWidth  <- ifelse(hjust > 0, hjust * labelWidth / pmax(x, epsilon), 0)
  rightWidth <- ifelse(hjust < 1, (1 - hjust) * labelWidth / pmax(1 - x, epsilon), 0)
  required   <- max(pmax(leftWidth, rightWidth), na.rm = TRUE)

  return(required + .forestPlotPanelPaddingMm(options, side))
}

.forestPlotPanelPaddingMm            <- function(options, side) {

  padding <- switch(
    side,
    "left"  = 12,
    "right" = 4,
    8
  )

  return(padding * options[["forestPlotSizeText"]])
}

.forestPlotTextData                  <- function(data, labelColumn = "label", hjustColumn = "hjust",
                                                 hjust = NULL, fontface = "plain", fontfaceColumn = NULL,
                                                 includeY = FALSE) {

  if (!.forestPlotHasDataFrame(data) || !labelColumn %in% colnames(data) || !"x" %in% colnames(data)) {
    return(NULL)
  }

  if (!is.null(hjust)) {
    hjustValues <- hjust
  } else if (hjustColumn %in% colnames(data)) {
    hjustValues <- data[[hjustColumn]]
  } else {
    hjustValues <- 0
  }

  if (!is.null(fontfaceColumn) && fontfaceColumn %in% colnames(data)) {
    fontfaceValues <- data[[fontfaceColumn]]
  } else if ("fontface" %in% colnames(data)) {
    fontfaceValues <- data[["fontface"]]
  } else if ("face" %in% colnames(data)) {
    fontfaceValues <- data[["face"]]
  } else {
    fontfaceValues <- fontface
  }

  textData <- data.frame(
    label    = as.character(data[[labelColumn]]),
    x        = data[["x"]],
    hjust    = .forestPlotNormalizeHjust(hjustValues),
    fontface = fontfaceValues
  )

  if (includeY) {
    if (!"y" %in% colnames(data)) {
      return(NULL)
    }
    textData$y <- data[["y"]]
  }

  return(textData)
}

.forestPlotLeftPanelTextData         <- function(leftPanelData, align) {

  return(.forestPlotBindDataFrames(list(
    .forestPlotTextData(leftPanelData[["titles"]], "title", "alignment", fontface = "bold"),
    .forestPlotTextData(leftPanelData[["studyDataColored"]], "label", "alignment"),
    .forestPlotTextData(leftPanelData[["studyData"]], "label", "alignment"),
    .forestPlotTextData(leftPanelData[["estimateTitles"]], "label", "alignment", fontface = "bold"),
    .forestPlotTextData(leftPanelData[["additionalData"]], "label", "alignment"),
    .forestPlotTextData(leftPanelData[["additionalInformation"]], "label", hjust = align),
    .forestPlotTextData(leftPanelData[["subgroupHeadings"]], "label", hjust = align)
  )))
}

.forestPlotRightPanelTextData        <- function(rightPanelData) {

  return(.forestPlotBindDataFrames(list(
    .forestPlotTextData(rightPanelData[["cis"]], "label", hjust = 1),
    .forestPlotTextData(rightPanelData[["testsAndWeights"]], "label", "hjust")
  )))
}
