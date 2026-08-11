# Forest plot panel data.
#
# Builds headers, settings, alignment, and panel-presence metadata.

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

.forestPlotLeftPanelAlign              <- function(options) {
  return(switch(
    options[["forestPlotAlignLeftPanel"]],
    "left"   = 0,
    "middle" = 0.5,
    "right"  = 1
  ))
}
