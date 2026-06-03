# Forest plot pipeline: orchestration, layout, rendering, and composition.
#
# This is the main entry point for building forest plots.  The pipeline is:
#   fit -> forestPlotInput -> layout -> plotData -> render
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
.forestPlotPlotHeight                 <- function(plotOut, options) {
  return(200 + attr(plotOut, "rows") * 10 * .forestPlotRowSize(options))
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
.forestPlotBuildPlotData              <- function(plotInput, options) {

  plotInput <- .forestPlotValidateInput(plotInput)
  items     <- plotInput[["items"]]
  layout    <- .forestPlotCreateLayout()

  if (identical(plotInput[["sectionGrouping"]], "byItem") ||
      options[["subgroup"]] == "" || options[["forestPlotSubgroupPanelsWithinSubgroup"]]) {
    layout <- .forestPlotAppendGroupedItems(layout, items, options, rowMode = plotInput[["rowMode"]])
  } else {
    layout <- .forestPlotAppendGroupedSectionPanels(layout, items, options)
  }

  return(.forestPlotFinalizeLayout(layout, options))
}

# Layout can be grouped by item (study + extras together) or by section type
# (all study blocks first, then shared EMM/model panels across subgroups).
.forestPlotAppendGroupedItems         <- function(layout, items, options, rowMode = "stacked") {

  for (item in items) {

    if (options[["subgroup"]] != "") {
      layout <- .forestPlotAppendSubgroupHeading(layout, options, item[["subgroup"]])
    }

    if (.forestPlotShouldIncludeStudySection(item, options)) {
      layout <- .forestPlotAppendStudySection(layout, item[["sections"]][["study"]], item[["index"]], options)
    }

    if (identical(rowMode, "absolute") && options[["subgroup"]] == "") {
      layout[["row"]] <- 1
    }

    for (sectionName in .forestPlotAdditionalSectionNames(items)) {
      if (!.forestPlotShouldIncludeAdditionalSection(item, sectionName))
        next

      layout <- .forestPlotAppendAdditionalSection(
        layout   = layout,
        section  = item[["sections"]][["additional"]][[sectionName]],
        blockId  = paste(sectionName, item[["index"]], sep = "_"),
        addTitle = TRUE,
        options  = options
      )
    }
  }

  return(layout)
}
.forestPlotAppendGroupedSectionPanels <- function(layout, items, options) {

  layout <- .forestPlotAppendStudyBlocks(layout, items, options)

  for (sectionName in .forestPlotAdditionalSectionNames(items))
    layout <- .forestPlotAppendAdditionalBlocks(layout, items, options, sectionName)

  return(layout)
}
.forestPlotAppendStudyBlocks          <- function(layout, items, options) {

  for (item in items) {
    if (!.forestPlotShouldIncludeStudySection(item, options)) {
      next
    }

    if (options[["subgroup"]] != "") {
      layout <- .forestPlotAppendSubgroupHeading(layout, options, item[["subgroup"]])
    }

    layout <- .forestPlotAppendStudySection(layout, item[["sections"]][["study"]], item[["index"]], options)
  }

  return(layout)
}
.forestPlotAppendAdditionalBlocks     <- function(layout, items, options, sectionName) {

  includedItems <- Filter(function(item) {
    .forestPlotShouldIncludeAdditionalSection(item, sectionName) &&
      .forestPlotSectionHasContent(item[["sections"]][["additional"]][[sectionName]])
  }, items)

  if (length(includedItems) == 0) {
    return(layout)
  }

  layout <- .forestPlotAppendAdditionalHeading(layout, includedItems[[1]][["sections"]][["additional"]][[sectionName]][["heading"]])

  for (item in items) {
    if (!.forestPlotShouldIncludeAdditionalSection(item, sectionName)) {
      next
    }

    if (options[["subgroup"]] != "") {
      layout <- .forestPlotAppendSubgroupHeading(layout, options, item[["subgroup"]])
    }

    layout <- .forestPlotAppendAdditionalSection(
      layout   = layout,
      section  = item[["sections"]][["additional"]][[sectionName]],
      blockId  = paste(sectionName, item[["index"]], sep = "_"),
      addTitle = FALSE,
      options  = options
    )
  }

  return(layout)
}
.forestPlotShouldIncludeStudySection  <- function(item, options) {

  return(!(options[["subgroup"]] != "" && isTRUE(item[["isFullDataset"]])))
}
.forestPlotShouldIncludeAdditionalSection <- function(item, sectionName) {

  if (!isTRUE(item[["isFullDataset"]])) {
    return(TRUE)
  }

  section <- item[["sections"]][["additional"]][[sectionName]]
  return(!is.null(section) && isTRUE(section[["includeWithFullDataset"]]))
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
.forestPlotAppendAdditionalSection    <- function(layout, section, blockId, addTitle = TRUE, options = NULL) {

  if (!.forestPlotSectionHasContent(section)) {
    return(layout)
  }

  if (addTitle && !isFALSE(section[["showHeading"]])) {
    layout <- .forestPlotAppendAdditionalHeading(layout, section[["heading"]])
  }

  if (.forestPlotSectionNeedsEstimateHeader(section, options)) {
    layout[["estimateHeaderIndex"]] <- c(layout[["estimateHeaderIndex"]], layout[["row"]])
    layout[["row"]]                 <- layout[["row"]] + 1
  }

  return(.forestPlotAppendSectionData(layout, section, blockId, infoSlot = "additionalInformation", objectSlot = "additionalObjects"))
}
.forestPlotSectionNeedsEstimateHeader <- function(section, options) {

  if (is.null(options) || !.forestPlotHasDataFrame(section[["information"]])) {
    return(FALSE)
  }

  rowVariable <- options[["row"]]
  if (!is.null(rowVariable) && length(rowVariable) == 1 && !is.na(rowVariable) && rowVariable != "") {
    return(FALSE)
  }

  estimateSettings <- .forestPlotEstimateInformationSettings(options)
  if (nrow(estimateSettings) == 0) {
    return(FALSE)
  }

  availableVars <- intersect(estimateSettings$value, colnames(section[["information"]]))
  if (length(availableVars) == 0) {
    return(FALSE)
  }

  return(any(rowSums(!is.na(section[["information"]][, availableVars, drop = FALSE])) > 0))
}
# Shared append logic: offset y by current row, add info/objects to named slots.
.forestPlotAppendSectionData         <- function(layout, section, blockId, infoSlot, objectSlot) {

  infoRowValues   <- numeric(0)
  objectRowValues <- numeric(0)

  if (.forestPlotHasDataFrame(section[["information"]])) {
    info          <- section[["information"]]
    info$y        <- info$y + (layout[["row"]] - 1)
    infoRowValues <- c(infoRowValues, info$y)
    layout[[infoSlot]][[length(layout[[infoSlot]]) + 1]] <- info
  }

  if (.forestPlotHasDataFrame(section[["objects"]])) {
    objects    <- section[["objects"]]
    objects$y  <- objects$y + (layout[["row"]] - 1)
    objects$id <- paste(objects$id, blockId, sep = "_")
    objectRowValues <- c(objectRowValues, objects$y)
    layout[[objectSlot]][[length(layout[[objectSlot]]) + 1]] <- objects
  }

  rowValues <- if (length(infoRowValues) > 0) infoRowValues else objectRowValues
  rowValues <- rowValues[!is.na(rowValues)]
  if (length(rowValues) > 0) {
    layout[["row"]] <- max(rowValues) + 2
  }

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
  plotData[["rowCount"]] <- .forestPlotPlotDataRowCount(plotData)

  # Convert row indices to plot coordinates (negative = top-to-bottom, scaled)
  rowSize <- .forestPlotRowSize(options)
  plotData[["forestHeaderIndex"]]     <- .forestPlotScaleY(plotData[["forestHeaderIndex"]], rowSize)
  plotData[["estimateHeaderIndex"]]   <- .forestPlotScaleY(plotData[["estimateHeaderIndex"]], rowSize)
  plotData[["forestInformation"]]     <- .forestPlotScaleYColumn(plotData[["forestInformation"]],     rowSize)
  plotData[["forestObjects"]]         <- .forestPlotScaleYColumn(plotData[["forestObjects"]],         rowSize)
  plotData[["additionalInformation"]] <- .forestPlotScaleYColumn(plotData[["additionalInformation"]], rowSize)
  plotData[["additionalObjects"]]     <- .forestPlotScaleYColumn(plotData[["additionalObjects"]],     rowSize)
  plotData[["subgroupHeadings"]]      <- .forestPlotScaleYColumn(plotData[["subgroupHeadings"]],      rowSize)

  return(plotData)
}
.forestPlotPlotDataRowCount          <- function(plotData) {

  rowValues <- c(
    plotData[["forestHeaderIndex"]],
    plotData[["estimateHeaderIndex"]],
    .forestPlotCollectNumericValues(plotData[["forestInformation"]],     "y"),
    .forestPlotCollectNumericValues(plotData[["forestObjects"]],         "y"),
    .forestPlotCollectNumericValues(plotData[["additionalInformation"]], "y"),
    .forestPlotCollectNumericValues(plotData[["additionalObjects"]],     "y"),
    .forestPlotCollectNumericValues(plotData[["subgroupHeadings"]],      "y"),
    plotData[["nextRow"]] - 1
  )
  rowValues <- rowValues[is.finite(rowValues)]
  if (length(rowValues) == 0) {
    return(1)
  }

  return(ceiling(max(rowValues)))
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

  plotData   <- .forestPlotBuildPlotModel(plotData, options)
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
.forestPlotBuildMiddlePanel           <- function(plotData, axisSpec, options) {

  plotForest <- ggplot2::ggplot()
  plotForest <- .forestPlotAddStudyObjects(plotForest, plotData, axisSpec, options)
  plotForest <- .forestPlotAddAdditionalObjects(plotForest, plotData, axisSpec, options)
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

  estimateSize <- .forestPlotEstimateSize(options)

  if (options[["forestPlotStudyInformationAggregateMethod"]] == "boxplot") {
    forestObjects <- forestObjects[forestObjects$type == "boxplot", , drop = FALSE]
    forestObjects <- .forestPlotClipXAxisColumns(forestObjects, c("min", "lower", "middle", "upper", "max"), axisSpec[["xRange"]])
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
    forestObjects <- .forestPlotClipXAxisColumns(forestObjects, c("x"), axisSpec[["xRange"]])
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
.forestPlotAddPredictionDiamonds     <- function(plotForest, forestObjects, axisSpec, options) {

  if (!options[["forestPlotStudyInformationPredictedEffects"]] || !.forestPlotHasDataFrame(forestObjects)) {
    return(plotForest)
  }

  forestPrediction <- forestObjects[forestObjects$type == "diamond", , drop = FALSE]
  forestPrediction <- .forestPlotClipXAxisColumns(forestPrediction, c("x"), axisSpec[["xRange"]])
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
.forestPlotAddStudyPoints            <- function(plotForest, forestInformation, axisSpec, options) {

  if (!.forestPlotHasDataFrame(forestInformation)) {
    return(plotForest)
  }

  colorColumn  <- .forestPlotColorRenderColumn(forestInformation, options)
  shapeVar     <- options[["forestPlotMappingShape"]]
  estimateSize <- .forestPlotEstimateSize(options)

  forestInformationPoints <- .forestPlotMaskOutsideXAxis(forestInformation, "effectSize", axisSpec[["xRange"]])

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
      axisSpec = axisSpec, capHeight = 0, style = "interval", heightScale = estimateSize
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
        axisSpec = axisSpec, capHeight = 0.3 * estimateSize, style = "interval", heightScale = estimateSize
      ),
      color = "darkblue", lineWidth = 0.5 * estimateSize, overflowLineWidth = estimateSize
    )
  }

  return(plotForest)
}
.forestPlotAddAdditionalObjects       <- function(plotForest, plotData, axisSpec, options) {

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
      xRange  = axisSpec[["xRange"]]
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
    axisSpec    = axisSpec,
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

# Character counts drive the relative spacing between the interval column and
# the auxiliary tests/weights column on the right.
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
# Add a text layer to a side panel plot, skipping when data is NULL.
.forestPlotAddTextLayer              <- function(plot, data, mapping, options, ...) {

  if (is.null(data))
    return(plot)

  return(plot + ggplot2::geom_text(
    data    = data,
    mapping = mapping,
    na.rm   = TRUE,
    size    = 4 * options[["forestPlotSizeText"]],
    vjust   = "middle",
    ...
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
.forestPlotTextData                  <- function(data, labelColumn = "label", hjustColumn = "hjust", hjust = NULL, fontface = "plain") {

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

  if ("fontface" %in% colnames(data)) {
    fontfaceValues <- data[["fontface"]]
  } else if ("face" %in% colnames(data)) {
    fontfaceValues <- data[["face"]]
  } else {
    fontfaceValues <- fontface
  }

  return(data.frame(
    label    = data[[labelColumn]],
    x        = data[["x"]],
    hjust    = hjustValues,
    fontface = fontfaceValues
  ))
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

  measuredWidth <- .forestPlotMeasurePanelWidthMm(
    .forestPlotLeftPanelTextData(leftPanelData, align),
    options,
    side = "left"
  )
  columnWidth <- leftPanelData[["widthMm"]]
  if (is.null(columnWidth) || !is.finite(columnWidth)) {
    columnWidth <- 0
  }

  attr(plotLeft, "requiredWidthMm") <- max(
    measuredWidth,
    columnWidth + .forestPlotPanelPaddingMm(options, "left")
  )

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
      size   = 4 * options[["forestPlotSizeText"]]
    )
  }

  if (!is.null(rightPanelData[["testsAndWeights"]])) {
    plotRight <- plotRight + ggplot2::geom_text(
      data    = rightPanelData[["testsAndWeights"]],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        label = label,
        hjust = hjust
      ),
      na.rm  = TRUE,
      family = "mono",
      size   = 4 * options[["forestPlotSizeText"]]
    )
  }

  attr(plotRight, "maxCharsRight") <- rightPanelData[["maxCharsRight"]]
  attr(plotRight, "requiredWidthMm") <- .forestPlotMeasurePanelWidthMm(
    .forestPlotRightPanelTextData(rightPanelData),
    options,
    family = "mono",
    side   = "right"
  )

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
  yRange   <- c(min(yValues, na.rm = TRUE) - .forestPlotRowSize(options), 0)

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
      axis.text.x      = ggplot2::element_text(color = "black", size = 12 * options[["forestPlotSizeAxisLabels"]]),
      axis.ticks.y     = ggplot2::element_blank(),
      axis.title.y     = ggplot2::element_blank(),
      axis.title.x     = ggplot2::element_text(color = "black", size = 12 * options[["forestPlotSizeAxisLabels"]]),
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
.forestPlotPrepareClippedIntervalData <- function(intervalData, axisSpec, capHeight = 0, style = "interval", heightScale = 1) {

  intervalData <- .forestPlotFilterIntervalData(intervalData)
  if (is.null(intervalData)) {
    return(NULL)
  }

  lower        <- axisSpec[["xRange"]][1]
  upper        <- axisSpec[["xRange"]][2]
  xSpan        <- max(diff(axisSpec[["xRange"]]), .Machine$double.eps)
  rowHeight    <- .forestPlotIntervalRowHeight(intervalData$y) * heightScale
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
.forestPlotPrepareObjectIndicatorData <- function(objects, axisSpec, skipTypes = character(0), heightScale = 1) {

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
    style        = "object",
    heightScale  = heightScale
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
      axis.text.x      = ggplot2::element_text(color = NA, size = 12 * options[["forestPlotSizeAxisLabels"]]),
      axis.ticks       = ggplot2::element_blank(),
      axis.title.y     = ggplot2::element_blank(),
      axis.title.x     = ggplot2::element_text(color = NA, size = 12 * options[["forestPlotSizeAxisLabels"]]),
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

  layoutSpec <- .forestPlotBuildLayoutSpec(plotLeft, plotRight, plotData, options)

  if (!layoutSpec[["isPanel"]]) {
    plotOut <- plotForest
    attr(plotOut, "isPanel")    <- FALSE
    attr(plotOut, "plotWidth")  <- layoutSpec[["plotWidth"]]
    attr(plotOut, "rows")       <- layoutSpec[["rows"]]
    attr(plotOut, "layoutSpec") <- layoutSpec

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
  attr(plotOut, "panelRatio") <- layoutSpec[["panelRatio"]]
  attr(plotOut, "plotWidth")  <- layoutSpec[["plotWidth"]]
  attr(plotOut, "rows")       <- layoutSpec[["rows"]]
  attr(plotOut, "widths")     <- layoutSpec[["widths"]]
  attr(plotOut, "layout")     <- matrix(1:length(plotOut), nrow = 1, ncol = length(plotOut), byrow = TRUE)
  attr(plotOut, "layoutSpec") <- layoutSpec

  return(plotOut)
}
.forestPlotBuildLayoutSpec           <- function(plotLeft, plotRight, plotData, options) {

  middlePanelWidth <- .forestPlotMiddlePanelWidthMm(options)
  widths <- c(
    if (!is.null(plotLeft)) .forestPlotSidePanelWidthMm(plotLeft, options, "left"),
    middlePanelWidth,
    if (!is.null(plotRight)) .forestPlotSidePanelWidthMm(plotRight, options, "right")
  )
  isPanel <- length(widths) != 1

  if (!isPanel) {
    return(list(
      isPanel    = FALSE,
      widths     = widths,
      panelRatio = 0,
      plotWidth  = .forestPlotWidthToPixels(middlePanelWidth),
      rows       = plotData[["rowCount"]]
    ))
  }

  middlePanelIndex <- if (!is.null(plotLeft)) 2 else 1
  panelRatio       <- (sum(widths) - widths[middlePanelIndex]) / widths[middlePanelIndex]
  plotWidth        <- .forestPlotWidthToPixels(sum(widths))

  return(list(
    isPanel    = TRUE,
    widths     = widths,
    panelRatio = panelRatio,
    plotWidth  = plotWidth,
    rows       = plotData[["rowCount"]]
  ))
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
.forestPlotSidePanelWidthMm          <- function(plotPanel, options, side) {

  requiredWidth <- attr(plotPanel, "requiredWidthMm")
  if (is.null(requiredWidth) || !is.finite(requiredWidth)) {
    requiredWidth <- 0
  }

  multiplier <- switch(
    side,
    "left"  = .forestPlotPositiveOption(options, "forestPlotSizeLeftPanel"),
    "right" = .forestPlotPositiveOption(options, "forestPlotSizeRightPanel"),
    1
  )

  return(requiredWidth * multiplier)
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
