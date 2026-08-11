# Forest plot layout accumulation.
#
# Stacks study and additional sections and finalizes shared row coordinates.

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
