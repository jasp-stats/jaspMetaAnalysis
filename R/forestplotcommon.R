# Forest plot orchestration and section collection.
#
# This is the main entry point for building forest plots.  The pipeline is:
#   fit -> forestPlotInput -> layout -> plotData -> render
#
# Focused implementations live in forestplot-study-data.R,
# forestplot-model-data.R, forestplot-panel-data.R, forestplot-layout.R,
# forestplot-render.R, forestplot-panels.R, forestplot-axis.R,
# forestplot-clipping.R, and forestplotscaling.R.


# ── Entry point & options preparation ──────────────────────────────────────────

.maMakeTheUltimateForestPlot           <- function(fit, options) {

  options     <- .forestPlotPrepareOptions(options)
  dataOptions <- .forestPlotPrepareDataOptions(options)
  plotInput   <- .forestPlotInputFromFit(fit, dataOptions)
  plotData    <- .forestPlotBuildPlotData(plotInput, dataOptions)

  return(.forestPlotRenderPlot(plotData, options))
}

.forestPlotInputFromFit                <- function(fit, options) {

  fitItems <- .forestPlotPrepareFitItems(fit, options)
  return(.forestPlotCreateInput(.forestPlotCollectInputItems(fitItems, options)))
}

.forestPlotCreateInput                 <- function(items, rowMode = "stacked", sectionGrouping = NULL) {

  return(structure(
    list(
      items           = items,
      rowMode         = rowMode,
      sectionGrouping = sectionGrouping
    ),
    class = "forestPlotInput"
  ))
}

.forestPlotCreateInputItem             <- function(key, index, subgroup, isFullDataset, sections) {
  return(list(
    key           = key,
    index         = index,
    subgroup      = subgroup,
    isFullDataset = isFullDataset,
    sections      = sections
  ))
}

# forestPlotInput items are the renderer boundary. Each item contains only plot
# metadata and already-built sections; raw fit objects must stay upstream.
.forestPlotValidateInput               <- function(plotInput) {

  if (!inherits(plotInput, "forestPlotInput"))
    stop(gettext("Invalid forest plot input."))

  validRowMode         <- plotInput[["rowMode"]] %in% c("stacked", "absolute")
  validSectionGrouping <- is.null(plotInput[["sectionGrouping"]]) ||
    plotInput[["sectionGrouping"]] %in% c("byItem", "bySection")

  if (!is.list(plotInput[["items"]]) || !validRowMode || !validSectionGrouping)
    stop(gettext("Invalid forest plot input."))

  requiredItemFields <- c("key", "index", "subgroup", "isFullDataset", "sections")

  for (i in seq_along(plotInput[["items"]])) {
    item <- plotInput[["items"]][[i]]

    if (!is.list(item) ||
        !all(requiredItemFields %in% names(item)) ||
        !is.list(item[["sections"]]) ||
        !("study" %in% names(item[["sections"]])) ||
        !is.list(item[["sections"]][["additional"]]))
      stop(gettext("Invalid forest plot input."))
  }

  return(plotInput)
}

.forestPlotAdditionalSectionNames      <- function(items) {

  sectionNames <- unlist(lapply(items, function(item) {
    names(item[["sections"]][["additional"]])
  }), use.names = FALSE)

  return(unique(sectionNames[sectionNames != ""]))
}

.forestPlotSectionsHaveContent         <- function(sections) {
  return(
    .forestPlotSectionHasContent(sections[["study"]]) ||
      any(vapply(sections[["additional"]], .forestPlotSectionHasContent, logical(1)))
  )
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

  sizeDefaults <- c(
    forestPlotSizeEstimates  = 1,
    forestPlotSizeText       = 1,
    forestPlotSizeAxisLabels = 1,
    forestPlotSizeRow        = 1,
    forestPlotSizeLeftPanel  = 1,
    forestPlotSizePlotArea   = 1,
    forestPlotSizeRightPanel = 1
  )
  for (key in names(sizeDefaults)) {
    if (is.null(options[[key]])) {
      options[[key]] <- sizeDefaults[[key]]
    }
  }

  # cannot plot predicted effects alongside aggregate study information
  if (options[["forestPlotStudyInformationAggregateBy"]] != "") {
    options[["forestPlotStudyInformationPredictedEffects"]] <- FALSE
  }

  return(options)
}
.forestPlotPositiveOption             <- function(options, optionName) {

  value <- suppressWarnings(as.numeric(options[[optionName]]))
  if (length(value) != 1 || !is.finite(value) || value <= 0) {
    return(1)
  }

  return(value)
}
.forestPlotEstimateSize               <- function(options) {
  return(.forestPlotPositiveOption(options, "forestPlotSizeEstimates"))
}
.forestPlotRowSize                    <- function(options) {
  return(.forestPlotPositiveOption(options, "forestPlotSizeRow"))
}
.forestPlotAxisLabelSize              <- function(options) {
  return(.forestPlotPositiveOption(options, "forestPlotSizeAxisLabels"))
}
.forestPlotAxisBand                   <- function(bodyYRange, options) {

  axisSpacing <- max(.forestPlotRowSize(options), .forestPlotAxisLabelSize(options))
  axisY       <- bodyYRange[1] - 0.35 * axisSpacing

  return(list(
    spacing  = axisSpacing,
    axisY    = axisY,
    tickY    = axisY - 0.12 * axisSpacing,
    labelY   = axisY - 0.32 * axisSpacing,
    titleY   = axisY - 0.95 * axisSpacing,
    bottomY  = axisY - 1.25 * axisSpacing,
    heightPx = 45 * .forestPlotAxisLabelSize(options)
  ))
}
.forestPlotPlotHeight                 <- function(canvasSpec, options) {
  return(
    155 +
      canvasSpec[["rows"]] * 10 * .forestPlotRowSize(options) +
      canvasSpec[["axisHeight"]]
  )
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
.forestPlotCollectInputItems           <- function(fitItems, options) {

  inputItems <- lapply(fitItems, function(fitItem) {
    return(.forestPlotCreateInputItem(
      key           = fitItem[["key"]],
      index         = fitItem[["index"]],
      subgroup      = fitItem[["subgroup"]],
      isFullDataset = fitItem[["isFullDataset"]],
      sections      = .forestPlotCollectItemSections(fitItem[["fit"]], options)
    ))
  })
  names(inputItems) <- names(fitItems)

  return(inputItems)
}
.forestPlotCollectItemSections         <- function(fit, options) {

  additionalSections <- list(
    estimatedMarginalMeans = .forestPlotCollectEstimatedMarginalMeansSection(fit, options),
    modelInformation       = .forestPlotCollectModelInformationSection(fit, options)
  )

  return(list(
    study      = .forestPlotCollectStudySection(fit, options),
    additional = additionalSections
  ))
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
    heading                = gettext("Estimated Marginal Means"),
    information            = estimatedMarginalMeansSection[["information"]],
    objects                = estimatedMarginalMeansSection[["objects"]],
    includeWithFullDataset = options[["forestPlotSubgroupFullDatasetEstimatedMarginalMeans"]]
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
    heading                = gettext("Model Information"),
    information            = modelInformationSection[["information"]],
    objects                = modelInformationSection[["objects"]],
    includeWithFullDataset = options[["forestPlotSubgroupFullDatasetModelInformation"]]
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
.forestPlotCreateAdditionalSection    <- function(heading, information, objects = NULL, showHeading = TRUE, includeWithFullDataset = FALSE) {

  objects <- .forestPlotNormalizeObjectData(objects)

  if (is.null(information) && is.null(objects)) {
    return(NULL)
  }

  return(list(
    kind                   = "additional",
    heading                = heading,
    showHeading            = showHeading,
    includeWithFullDataset = includeWithFullDataset,
    information            = information,
    objects                = objects
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
