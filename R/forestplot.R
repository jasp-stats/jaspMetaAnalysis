# Active implementation: fit/user input adapters -> section data -> layout -> render.
.maMakeTheUltimateForestPlot           <- function(fit, options) {

  fitItems <- .forestPlotPrepareFitItems(fit, options)
  options  <- .forestPlotPrepareOptions(options)
  plotData <- .forestPlotBuildPlotData(fitItems, options)

  return(.forestPlotRenderPlot(plotData, options))
}
.forestPlotPrepareFitItems             <- function(fit, options) {

  if (options[["subgroup"]] != "" && length(fit) > 1) {
    fit <- fit[c(2:length(fit), 1)]
  }

  if (options[["subgroup"]] != "" && length(fit) > 0) {
    fullDatasetName <- names(fit)[length(fit)]
  } else {
    fullDatasetName <- NULL
  }
  fit             <- fit[!vapply(fit, jaspBase::isTryError, logical(1))]

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
.forestPlotPrepareOptions              <- function(options) {

  options[["confidenceIntervals"]] <- TRUE
  options[["predictionIntervals"]] <- options[["forestPlotPredictionIntervals"]]

  if (options[["forestPlotStudyInformationAggregateBy"]] != "") {
    options[["forestPlotStudyInformationPredictedEffects"]] <- FALSE
  }

  return(options)
}
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
.forestPlotCreateLayout               <- function() {
  return(list(
    forestHeaderIndex     = NULL,
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

  tempForestInformation   <- section[["information"]]
  tempForestInformation$y <- tempForestInformation$y + (layout[["row"]] - 1)
  layout[["forestInformation"]][[length(layout[["forestInformation"]]) + 1]] <- tempForestInformation

  if (.forestPlotHasDataFrame(section[["objects"]])) {
    tempForestObjects    <- section[["objects"]]
    tempForestObjects$y  <- tempForestObjects$y + (layout[["row"]] - 1)
    tempForestObjects$id <- paste(tempForestObjects$id, blockId, sep = "_")
    layout[["forestObjects"]][[length(layout[["forestObjects"]]) + 1]] <- tempForestObjects
  }

  layout[["row"]] <- max(tempForestInformation$y, na.rm = TRUE) + 2

  return(layout)
}
.forestPlotAppendAdditionalSection    <- function(layout, section, blockId, addTitle = TRUE) {

  if (!.forestPlotSectionHasContent(section)) {
    return(layout)
  }

  if (addTitle) {
    layout <- .forestPlotAppendAdditionalHeading(layout, section[["heading"]])
  }

  tempAdditionalInformation   <- section[["information"]]
  tempAdditionalInformation$y <- tempAdditionalInformation$y + (layout[["row"]] - 1)
  layout[["additionalInformation"]][[length(layout[["additionalInformation"]]) + 1]] <- tempAdditionalInformation

  if (.forestPlotHasDataFrame(section[["objects"]])) {
    tempAdditionalObjects    <- section[["objects"]]
    tempAdditionalObjects$y  <- tempAdditionalObjects$y + (layout[["row"]] - 1)
    tempAdditionalObjects$id <- paste(tempAdditionalObjects$id, blockId, sep = "_")
    layout[["additionalObjects"]][[length(layout[["additionalObjects"]]) + 1]] <- tempAdditionalObjects
  }

  layout[["row"]] <- max(tempAdditionalInformation$y, na.rm = TRUE) + 2

  return(layout)
}
.forestPlotFinalizeLayout             <- function(layout, options) {

  plotData <- list(
    forestHeaderIndex     = layout[["forestHeaderIndex"]],
    forestInformation     = .forestPlotBindDataFrames(layout[["forestInformation"]]),
    forestObjects         = .forestPlotBindDataFrames(layout[["forestObjects"]]),
    additionalInformation = .forestPlotBindDataFrames(layout[["additionalInformation"]]),
    additionalObjects     = .forestPlotBindDataFrames(layout[["additionalObjects"]]),
    subgroupHeadings      = .forestPlotBindDataFrames(layout[["subgroupHeadings"]]),
    nextRow               = layout[["row"]]
  )

  if (!is.null(plotData[["forestHeaderIndex"]])) {
    plotData[["forestHeaderIndex"]]       <- - plotData[["forestHeaderIndex"]]       * options[["forestPlotRelativeSizeRow"]]
  }
  if (!is.null(plotData[["forestInformation"]])) {
    plotData[["forestInformation"]]$y     <- - plotData[["forestInformation"]]$y     * options[["forestPlotRelativeSizeRow"]]
  }
  if (!is.null(plotData[["forestObjects"]])) {
    plotData[["forestObjects"]]$y         <- - plotData[["forestObjects"]]$y         * options[["forestPlotRelativeSizeRow"]]
  }
  if (!is.null(plotData[["additionalInformation"]])) {
    plotData[["additionalInformation"]]$y <- - plotData[["additionalInformation"]]$y * options[["forestPlotRelativeSizeRow"]]
  }
  if (!is.null(plotData[["additionalObjects"]])) {
    plotData[["additionalObjects"]]$y     <- - plotData[["additionalObjects"]]$y     * options[["forestPlotRelativeSizeRow"]]
  }
  if (!is.null(plotData[["subgroupHeadings"]])) {
    plotData[["subgroupHeadings"]]$y      <- - plotData[["subgroupHeadings"]]$y      * options[["forestPlotRelativeSizeRow"]]
  }

  return(plotData)
}
.forestPlotRenderPlot                 <- function(plotData, options) {

  plotForest <- .forestPlotBuildMiddlePanel(plotData, options)
  plotLeft   <- .forestPlotBuildLeftPanel(plotData, options)
  plotRight  <- .forestPlotBuildRightPanel(plotData, options)
  axisSpec   <- .forestPlotBuildAxisSpec(plotData, options)

  plotForest <- .forestPlotApplyForestTheme(plotForest, axisSpec, options)

  if (!is.null(plotLeft)) {
    plotLeft <- .forestPlotApplySidePanelTheme(plotLeft, axisSpec[["yRange"]], options)
  }

  if (!is.null(plotRight)) {
    plotRight <- .forestPlotApplySidePanelTheme(plotRight, axisSpec[["yRange"]], options)
  }

  return(.forestPlotComposeOutput(plotForest, plotLeft, plotRight, plotData, options))
}
.forestPlotBuildMiddlePanel           <- function(plotData, options) {

  plotForest <- ggplot2::ggplot()
  plotForest <- .forestPlotAddStudyObjects(plotForest, plotData, options)
  plotForest <- .forestPlotAddAdditionalObjects(plotForest, plotData)
  plotForest <- .forestPlotAddVerticalLines(plotForest, options)

  return(plotForest)
}
.forestPlotAddStudyObjects            <- function(plotForest, plotData, options) {

  forestInformation <- plotData[["forestInformation"]]
  forestObjects     <- plotData[["forestObjects"]]

  if (!options[["forestPlotStudyInformation"]]) {
    return(plotForest)
  }

  if (options[["forestPlotStudyInformationAggregateBy"]] != "") {

    if (options[["forestPlotStudyInformationAggregateMethod"]] == "boxplot" && .forestPlotHasDataFrame(forestObjects)) {
      forestObjects <- forestObjects[forestObjects$type == "boxplot", , drop = FALSE]

      aesCall <- list(
        y       = as.name("y"),
        group   = as.name("id"),
        xmin    = as.name("min"),
        xlower  = as.name("lower"),
        xmiddle = as.name("middle"),
        xupper  = as.name("upper"),
        xmax    = as.name("max"),
        fill    = if (options[["forestPlotMappingColor"]] != "") as.name(options[["forestPlotMappingColor"]])
      )
      geomCall <- list(
        data        = forestObjects,
        mapping     = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
        fill        = if (options[["forestPlotMappingColor"]] == "") "grey20",
        alpha       = 0.8,
        orientation = "y",
        stat        = "identity"
      )
      plotForest <- plotForest + do.call(ggplot2::geom_boxplot, geomCall[!sapply(geomCall, is.null)])
    }

    if (options[["forestPlotStudyInformationAggregateMethod"]] == "bubbles" && .forestPlotHasDataFrame(forestObjects)) {
      forestObjects <- forestObjects[forestObjects$type == "bubbles", , drop = FALSE]

      aesCall <- list(
        y     = as.name("y"),
        x     = as.name("x"),
        size  = as.name("weight"),
        fill  = if (options[["forestPlotMappingColor"]] != "") as.name(options[["forestPlotMappingColor"]]),
        color = if (options[["forestPlotMappingColor"]] != "") as.name(options[["forestPlotMappingColor"]])
      )
      geomCall <- list(
        data     = forestObjects,
        mapping  = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
        fill     = if (options[["forestPlotMappingColor"]] == "") "grey20",
        alpha    = 0.8,
        position = ggplot2::position_jitter(width = 0, height = 0.10)
      )
      plotForest <- plotForest + do.call(jaspGraphs::geom_point, geomCall[!sapply(geomCall, is.null)]) +
        ggplot2::scale_size(range = c(1.5, 10) * options[["forestPlotStudyInformationAggregateMethodBubbleRelativeSize"]])
    }

    return(plotForest)
  }

  if (options[["forestPlotStudyInformationPredictedEffects"]] && .forestPlotHasDataFrame(forestObjects)) {
    forestPrediction <- forestObjects[forestObjects$type == "diamond", , drop = FALSE]

    aesCall <- list(
      x     = as.name("x"),
      y     = as.name("y"),
      group = as.name("id"),
      fill  = if (options[["forestPlotMappingColor"]] != "") as.name(options[["forestPlotMappingColor"]])
    )
    geomCall <- list(
      data    = forestPrediction,
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
      fill    = if (options[["forestPlotMappingColor"]] == "") "grey20",
      alpha   = 0.8
    )
    plotForest <- plotForest + do.call(ggplot2::geom_polygon, geomCall[!sapply(geomCall, is.null)])
  }

  if (.forestPlotHasDataFrame(forestInformation)) {
    aesCall <- list(
      x     = as.name("effectSize"),
      y     = as.name("y"),
      color = if (options[["forestPlotMappingColor"]] != "") as.name(options[["forestPlotMappingColor"]]),
      shape = if (options[["forestPlotMappingShape"]] != "") as.name(options[["forestPlotMappingShape"]]),
      size  = as.name("weights")
    )
    geomCall <- list(
      data    = forestInformation,
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
      color   = if (options[["forestPlotMappingColor"]] == "") options[["forestPlotAuxiliaryPlotColor"]],
      shape   = if (options[["forestPlotMappingShape"]] == "") 15
    )
    plotForest <- plotForest + do.call(ggplot2::geom_point, geomCall[!sapply(geomCall, is.null)]) +
      ggplot2::scale_size(range = c(1, 6) * options[["forestPlotRelativeSizeEstimates"]])

    if (options[["forestPlotMappingShape"]] != "") {
      plotForest <- plotForest + ggplot2::scale_shape_manual(
        values = rep(c(15:18, 21:25), length.out = length(unique(forestInformation[[options[["forestPlotMappingShape"]]]])))
      )
    }

    plotForest <- plotForest + ggplot2::geom_errorbarh(
      data    = forestInformation,
      mapping = ggplot2::aes(
        xmin = lCi,
        xmax = uCi,
        y    = y
      ),
      height = 0
    )

    if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
      plotForest <- plotForest + ggplot2::geom_errorbarh(
        data    = forestInformation,
        mapping = ggplot2::aes(
          xmin = lCi2,
          xmax = uCi2,
          y    = y
        ),
        color  = "darkblue",
        height = 0.3
      )
    }
  }

  return(plotForest)
}
.forestPlotAddAdditionalObjects       <- function(plotForest, plotData) {

  additionalObjects <- plotData[["additionalObjects"]]

  if (!.forestPlotHasDataFrame(additionalObjects)) {
    return(plotForest)
  }

  if (any(!is.na(additionalObjects$mapColor))) {
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

  if (any(is.na(additionalObjects$mapColor))) {
    plotForest <- plotForest + ggplot2::geom_polygon(
      data    = additionalObjects[is.na(additionalObjects$mapColor), , drop = FALSE],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        group = id
      )
    )
  }

  return(plotForest)
}
.forestPlotAddVerticalLines           <- function(plotForest, options) {

  if (options[["forestPlotAuxiliaryAddVerticalLine"]]) {
    plotForest <- plotForest + ggplot2::geom_vline(
      xintercept = options[["forestPlotAuxiliaryAddVerticalLineValue"]],
      linetype   = "dashed"
    )
  }

  if (options[["forestPlotAuxiliaryAddVerticalLine2"]]) {
    plotForest <- plotForest + ggplot2::geom_vline(
      xintercept = options[["forestPlotAuxiliaryAddVerticalLineValue2"]],
      linetype   = "dotted"
    )
  }

  return(plotForest)
}
.forestPlotPrepareLeftPanelData       <- function(plotData, options) {

  forestInformation     <- plotData[["forestInformation"]]
  additionalInformation <- plotData[["additionalInformation"]]
  subgroupHeadings      <- plotData[["subgroupHeadings"]]

  if (!((options[["forestPlotStudyInformation"]] && length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) ||
        .forestPlotHasDataFrame(additionalInformation))) {
    return(NULL)
  }

  leftPanelData <- list(
    titles                 = NULL,
    studyDataColored       = NULL,
    studyData              = NULL,
    additionalInformation  = NULL,
    subgroupHeadings       = NULL,
    maxCharsLeft           = NULL
  )

  if (options[["forestPlotStudyInformation"]] && length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) {
    leftPanelStudyInformation <- .forestPlotBuildStudyInformationHeader(options, forestInformation, additionalInformation)
    leftPanelData[["maxCharsLeft"]] <- attr(leftPanelStudyInformation, "maxChars")

    leftPanelStudyInformation$x <- ifelse(
      leftPanelStudyInformation$alignment == "left",
      leftPanelStudyInformation$xStart,
      ifelse(
        leftPanelStudyInformation$alignment == "middle",
        (leftPanelStudyInformation$xStart + leftPanelStudyInformation$xEnd) / 2,
        leftPanelStudyInformation$xEnd
      )
    )

    if (any(leftPanelStudyInformation$title != "") && length(plotData[["forestHeaderIndex"]]) > 0) {
      leftPanelTitleInformation <- leftPanelStudyInformation
      leftPanelData[["titles"]] <- do.call(rbind, lapply(plotData[["forestHeaderIndex"]], function(y) {
        leftPanelTitleInformation$y <- y
        return(leftPanelTitleInformation)
      }))
    }

    if (any(leftPanelStudyInformation$value == options[["forestPlotMappingColor"]])) {
      leftPanelData[["studyDataColored"]] <- data.frame(
        x         = leftPanelStudyInformation$x[leftPanelStudyInformation$value == options[["forestPlotMappingColor"]]],
        y         = forestInformation$y,
        label     = as.character(forestInformation[[options[["forestPlotMappingColor"]]]]),
        alignment = leftPanelStudyInformation$alignment[leftPanelStudyInformation$value == options[["forestPlotMappingColor"]]]
      )
    }

    if (any(leftPanelStudyInformation$value != options[["forestPlotMappingColor"]])) {
      tempVariables <- unique(leftPanelStudyInformation$value[leftPanelStudyInformation$value != options[["forestPlotMappingColor"]]])
      leftPanelData[["studyData"]] <- do.call(rbind.data.frame, lapply(tempVariables, function(variable) {
        data.frame(
          x         = leftPanelStudyInformation$x[leftPanelStudyInformation$value == variable],
          y         = forestInformation$y,
          label     = as.character(forestInformation[[variable]]),
          alignment = leftPanelStudyInformation$alignment[leftPanelStudyInformation$value == variable]
        )
      }))
    }
  }

  if (.forestPlotHasDataFrame(additionalInformation)) {
    leftPanelAdditionalInformation       <- additionalInformation[!is.na(additionalInformation$label), , drop = FALSE]
    leftPanelAdditionalInformation$x     <- .forestPlotLeftPanelAlign(options)
    leftPanelAdditionalInformation$face[is.na(leftPanelAdditionalInformation$face)] <- "plain"
    leftPanelData[["additionalInformation"]] <- leftPanelAdditionalInformation
    leftPanelData[["maxCharsLeft"]] <- max(c(
      leftPanelData[["maxCharsLeft"]],
      max(nchar(leftPanelAdditionalInformation$label))
    ), na.rm = TRUE)
  }

  if (.forestPlotHasDataFrame(subgroupHeadings)) {
    subgroupHeadings$x <- .forestPlotLeftPanelAlign(options)
    leftPanelData[["subgroupHeadings"]] <- subgroupHeadings
  }

  return(leftPanelData)
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

    rightPanelCis <- .forestPlotBindDataFrames(list(studyCis, rightPanelAdditionalCis))
    if (.forestPlotHasDataFrame(rightPanelCis)) {
      rightPanelCis <- rightPanelCis[!apply(rightPanelCis[, 2:4], 1, function(x) all(is.na(x))), , drop = FALSE]
    }

    if (.forestPlotHasDataFrame(rightPanelCis)) {
      for (colName in c("est", "lCi", "uCi")) {
        rightPanelCis[!is.na(rightPanelCis[, colName]), colName] <- .maFormatDigits(
          rightPanelCis[!is.na(rightPanelCis[, colName]), colName],
          options[["forestPlotAuxiliaryDigits"]]
        )
      }

      rightPanelCis$label <- NA
      rightPanelCis$label[ is.na(rightPanelCis$est)] <- with(rightPanelCis[ is.na(rightPanelCis$est), ], paste0("PI [", lCi, ", ", uCi, "]"))
      rightPanelCis$label[!is.na(rightPanelCis$est)] <- with(rightPanelCis[!is.na(rightPanelCis$est), ], paste0(est, " [", lCi, ", ", uCi, "]"))
    } else {
      rightPanelCis <- NULL
    }
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
    studyWeights       <- forestInformation[, c("y", "weights")]
    studyWeights$label <- paste0(sprintf(paste0("%1$.", options[["forestPlotAuxiliaryDigits"]], "f"), studyWeights$weights), " %")
    studyWeights       <- studyWeights[, c("y", "label")]
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

  if (!is.null(rightPanelCis)) {
    maxCharsRightCis <- max(nchar(rightPanelCis$label))
  } else {
    maxCharsRightCis <- 0
  }
  if (!is.null(rightPanelTestsAndWeights)) {
    maxCharsRightAdd <- max(nchar(rightPanelTestsAndWeights$label))
  } else {
    maxCharsRightAdd <- 0
  }
  maxCharsRight <- maxCharsRightCis + maxCharsRightAdd + 2

  if (!is.null(rightPanelCis)) {
    rightPanelCis$x <- maxCharsRightCis / maxCharsRight
  }
  if (!is.null(rightPanelTestsAndWeights)) {
    rightPanelTestsAndWeights$x <- (maxCharsRightCis + 2) / maxCharsRight
  }

  return(list(
    cis             = rightPanelCis,
    testsAndWeights = rightPanelTestsAndWeights,
    maxCharsRight   = maxCharsRight
  ))
}
.forestPlotBuildLeftPanel             <- function(plotData, options) {

  leftPanelData <- .forestPlotPrepareLeftPanelData(plotData, options)
  if (is.null(leftPanelData)) {
    return(NULL)
  }

  plotLeft <- ggplot2::ggplot()

  if (!is.null(leftPanelData[["titles"]])) {
    plotLeft <- plotLeft + ggplot2::geom_text(
      data    = leftPanelData[["titles"]],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        label = title,
        hjust = alignment
      ),
      size     = 4 * options[["forestPlotRelativeSizeText"]],
      vjust    = "middle",
      fontface = "bold"
    )
  }

  if (!is.null(leftPanelData[["studyDataColored"]])) {
    plotLeft <- plotLeft + ggplot2::geom_text(
      data    = leftPanelData[["studyDataColored"]],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        label = label,
        hjust = alignment,
        color = label
      ),
      size  = 4 * options[["forestPlotRelativeSizeText"]],
      vjust = "middle"
    )
  }

  if (!is.null(leftPanelData[["studyData"]])) {
    plotLeft <- plotLeft + ggplot2::geom_text(
      data    = leftPanelData[["studyData"]],
      mapping = ggplot2::aes(
        x     = x,
        y     = y,
        label = label,
        hjust = alignment
      ),
      size  = 4 * options[["forestPlotRelativeSizeText"]],
      vjust = "middle"
    )
  }

  if (!is.null(leftPanelData[["additionalInformation"]])) {
    plotLeft <- plotLeft + ggplot2::geom_text(
      data    = leftPanelData[["additionalInformation"]],
      mapping = ggplot2::aes(
        x        = x,
        y        = y,
        label    = label,
        fontface = face
      ),
      size  = 4 * options[["forestPlotRelativeSizeText"]],
      hjust = .forestPlotLeftPanelHjust(options),
      vjust = "middle"
    )
  }

  if (!is.null(leftPanelData[["subgroupHeadings"]])) {
    plotLeft <- plotLeft + ggplot2::geom_text(
      data    = leftPanelData[["subgroupHeadings"]],
      mapping = ggplot2::aes(
        x        = x,
        y        = y,
        label    = label,
        fontface = face
      ),
      size  = 4 * options[["forestPlotRelativeSizeText"]],
      hjust = .forestPlotLeftPanelHjust(options),
      vjust = "middle"
    )
  }

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
      hjust  = "left",
      family = "mono",
      size   = 4 * options[["forestPlotRelativeSizeText"]]
    )
  }

  attr(plotRight, "maxCharsRight") <- rightPanelData[["maxCharsRight"]]

  return(plotRight)
}
.forestPlotBuildAxisSpec              <- function(plotData, options) {

  xValues <- c(
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

  if (options[["forestPlotAuxiliarySetXAxisLimit"]]) {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(
      options[["forestPlotAuxiliarySetXAxisLimitLower"]],
      options[["forestPlotAuxiliarySetXAxisLimitUpper"]]
    ))
  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(xValues, na.rm = TRUE))
  }
  xRange <- range(xBreaks)

  yRange <- c(min(yValues, na.rm = TRUE), 0)
  yRange[1] <- yRange[1] - options[["forestPlotRelativeSizeRow"]]

  return(list(
    xBreaks = xBreaks,
    xRange  = xRange,
    yRange  = yRange
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

  plotForest <- plotForest +
    jaspGraphs::scale_x_continuous(
      limits = axisSpec[["xRange"]],
      breaks = axisSpec[["xBreaks"]]
    ) +
    ggplot2::coord_cartesian(
      xlim   = axisSpec[["xRange"]],
      ylim   = axisSpec[["yRange"]],
      expand = FALSE
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
      plot.background  = ggplot2::element_blank()
    )

  return(plotForest)
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

  plotsWidths <- c()
  if (!is.null(plotLeft)) {
    plotsWidths <- c(plotsWidths, options[["forestPlotRelativeSizeLeftPanel"]])
  }
  plotsWidths <- c(plotsWidths, options[["forestPlotRelativeSizeMiddlePanel"]])
  if (!is.null(plotRight)) {
    plotsWidths <- c(plotsWidths, options[["forestPlotRelativeSizeRightPanel"]])
  }

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
.forestPlotStudyExtractBaseData        <- function(fit, dataset, options) {

  if (.maIsClassical(options)) {
    return(data.frame(
      effectSize     = fit[["yi"]],
      standardError  = sqrt(fit[["vi"]]),
      weights        = weights(fit),
      id             = seq_along(fit[["yi"]])
    ))
  }

  if (options[["analysis"]] == "BiBMA") {
    tempDf <- metafor::escalc(
      measure = "OR",
      ai      = dataset[[options[["successesGroup1"]]]],
      n1i     = dataset[[options[["sampleSizeGroup1"]]]],
      ci      = dataset[[options[["successesGroup2"]]]],
      n2i     = dataset[[options[["sampleSizeGroup2"]]]]
    )

    return(data.frame(
      effectSize     = tempDf[["yi"]],
      standardError  = sqrt(tempDf[["vi"]]),
      weights        = dataset[[options[["sampleSizeGroup1"]]]] + dataset[[options[["sampleSizeGroup2"]]]],
      id             = seq_len(nrow(dataset))
    ))
  }

  return(data.frame(
    effectSize     = dataset[[options[["effectSize"]]]],
    standardError  = dataset[[options[["effectSizeStandardError"]]]],
    weights        = 1 / dataset[[options[["effectSizeStandardError"]]]]^2,
    id             = seq_len(nrow(dataset))
  ))
}
.forestPlotStudyAddConfidenceIntervals <- function(dfForest, options) {

  ciZValue <- qnorm((1 - options[["confidenceIntervalsLevel"]]) / 2, lower.tail = FALSE)
  dfForest$lCi <- dfForest$effectSize - ciZValue * dfForest$standardError
  dfForest$uCi <- dfForest$effectSize + ciZValue * dfForest$standardError

  if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
    secondaryCiZValue <- qnorm((1 - options[["forestPlotStudyInformationSecondaryConfidenceIntervalLevel"]]) / 2, lower.tail = FALSE)
    dfForest$lCi2 <- dfForest$effectSize - secondaryCiZValue * dfForest$standardError
    dfForest$uCi2 <- dfForest$effectSize + secondaryCiZValue * dfForest$standardError
  }

  return(dfForest)
}
.forestPlotStudyTransformationColumns  <- function(options) {

  transformColumns <- c("effectSize", "lCi", "uCi")
  if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
    transformColumns <- c(transformColumns, "lCi2", "uCi2")
  }

  return(transformColumns)
}
.forestPlotStudyTransformEffectSizes   <- function(dfForest, options) {

  if (options[["transformEffectSize"]] == "none") {
    return(dfForest)
  }

  transformColumns <- .forestPlotStudyTransformationColumns(options)
  dfForest[, transformColumns] <- do.call(
    .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
    list(dfForest[, transformColumns])
  )

  return(dfForest)
}
.forestPlotStudyAdditionalVariables    <- function(options) {

  return(c(
    if (length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) {
      unlist(options[["forestPlotStudyInformationSelectedVariables"]])
    },
    if (options[["forestPlotStudyInformationOrderBy"]] != "") {
      options[["forestPlotStudyInformationOrderBy"]]
    },
    if (options[["forestPlotStudyInformationAggregateBy"]] != "") {
      options[["forestPlotStudyInformationAggregateBy"]]
    },
    if (options[["forestPlotMappingColor"]] != "") {
      options[["forestPlotMappingColor"]]
    },
    if (options[["forestPlotMappingShape"]] != "") {
      options[["forestPlotMappingShape"]]
    }
  ))
}
.forestPlotStudyBindAdditionalVariables <- function(dfForest, dataset, additionalVariables) {

  if (length(additionalVariables) == 0) {
    return(dfForest)
  }

  return(cbind(dfForest, dataset[, additionalVariables, drop = FALSE]))
}
.forestPlotStudyAggregateData          <- function(dfForest, options, additionalVariables) {

  if (options[["forestPlotStudyInformationAggregateBy"]] == "") {
    return(list(
      forest = dfForest,
      geoms  = NULL
    ))
  }

  return(.forestStudyInformationAggregate(dfForest, options, additionalVariables))
}
.forestPlotStudyOrderData              <- function(dfForest, options) {

  if (options[["forestPlotStudyInformationOrderBy"]] == "") {
    return(dfForest)
  }

  dfForest <- dfForest[order(
    dfForest[[options[["forestPlotStudyInformationOrderBy"]]]],
    decreasing = options[["forestPlotStudyInformationOrderAscending"]]
  ), ]

  return(dfForest)
}
.forestPlotStudyAssignCoordinates      <- function(dfForest, dfGeoms = NULL) {

  dfForest$y <- seq(nrow(dfForest))
  if (!is.null(dfGeoms)) {
    dfGeoms <- merge(dfGeoms, dfForest[, colnames(dfForest) %in% c("id", "y")], by = "id")
  }

  return(list(
    forest = dfForest,
    geoms  = dfGeoms
  ))
}
.forestPlotStudyPredictionMergeData    <- function(dfForest) {

  return(dfForest[, !colnames(dfForest) %in% c("effectSize", "standardError", "weights", "lCi", "uCi", "y"), drop = FALSE])
}
.forestPlotStudyBuildPredictions       <- function(fit, dfForest, options) {

  if (!options[["forestPlotStudyInformationPredictedEffects"]]) {
    return(NULL)
  }

  dfForestPrediction <- data.frame(predict(fit))

  if (nrow(dfForestPrediction) == 1) {
    dfForestPrediction <- do.call(rbind, replicate(nrow(dfForest), dfForestPrediction, simplify = FALSE))
  }

  dfForestPrediction$id <- dfForest$id
  dfForestPrediction$y  <- dfForest$y

  dfForestPrediction <- do.call(rbind, lapply(seq_len(nrow(dfForestPrediction)), function(i) {
    with(dfForestPrediction[i, ], .maMakeDiamondDataFrame(est = pred, lCi = pi.lb, uCi = pi.ub, row = y, id = id))
  }))
  dfForestPrediction <- merge(dfForestPrediction, .forestPlotStudyPredictionMergeData(dfForest), by = "id")

  if (options[["transformEffectSize"]] != "none") {
    dfForestPrediction[, "x"] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(dfForestPrediction[, "x"])
    )
  }

  return(dfForestPrediction)
}
.forestPlotBuildStudyInformation       <- function(fit, options){

  ### builds the study information section
  # returns a
  # - data frame with the study information
  # - data frame with the predicted effects

  # return null on error
  if (is.null(fit) || jaspBase::isTryError(fit)) {
    return(NULL)
  }

  # extract data set forwarded via the fit object
  # allows dispatching from subfits, NAs removal matching the analysis etc
  dataset <- attr(fit, "dataset")

  additionalVariables <- .forestPlotStudyAdditionalVariables(options)
  dfForest            <- .forestPlotStudyExtractBaseData(fit, dataset, options)
  dfForest            <- .forestPlotStudyAddConfidenceIntervals(dfForest, options)
  dfForest            <- .forestPlotStudyTransformEffectSizes(dfForest, options)
  dfForest            <- .forestPlotStudyBindAdditionalVariables(dfForest, dataset, additionalVariables)

  dfAggregate         <- .forestPlotStudyAggregateData(dfForest, options, additionalVariables)
  dfForest            <- .forestPlotStudyOrderData(dfAggregate[["forest"]], options)
  dfCoordinates       <- .forestPlotStudyAssignCoordinates(dfForest, dfAggregate[["geoms"]])
  dfForest            <- dfCoordinates[["forest"]]
  dfGeoms             <- dfCoordinates[["geoms"]]
  dfForestPrediction  <- .forestPlotStudyBuildPredictions(fit, dfForest, options)

  # return
  return(list(
    forest     = dfForest,
    prediction = dfForestPrediction,
    geoms      = dfGeoms
  ))
}
.forestPlotCreateAdditionalSectionState <- function() {

  return(list(
    row         = 1,
    information = list(),
    objects     = list()
  ))
}
.forestPlotCreateAdditionalRow          <- function(label, row, est = NA, lCi = NA, uCi = NA, test = "", face = NA) {

  return(data.frame(
    "label" = label,
    "y"     = row,
    "est"   = est,
    "lCi"   = lCi,
    "uCi"   = uCi,
    "test"  = test,
    "face"  = face
  ))
}
.forestPlotAppendAdditionalRow         <- function(state, label, est = NA, lCi = NA, uCi = NA, test = "", face = NA) {

  row <- state[["row"]]
  state[["information"]][[row]] <- .forestPlotCreateAdditionalRow(
    label = label,
    row   = row,
    est   = est,
    lCi   = lCi,
    uCi   = uCi,
    test  = test,
    face  = face
  )
  state[["row"]] <- row + 1

  return(state)
}
.forestPlotAppendAdditionalDiamond     <- function(state, label, est, lCi, uCi, test = "", face = NA, mapColor = NA) {

  row <- state[["row"]]
  state[["information"]][[row]] <- .forestPlotCreateAdditionalRow(
    label = label,
    row   = row,
    est   = est,
    lCi   = lCi,
    uCi   = uCi,
    test  = test,
    face  = face
  )
  state[["objects"]][[row]] <- .maMakeDiamondDataFrame(
    est = est,
    lCi = lCi,
    uCi = uCi,
    row = row,
    id  = row
  )
  state[["objects"]][[row]]$mapColor <- mapColor
  state[["row"]] <- row + 1

  return(state)
}
.forestPlotAppendAdditionalInterval    <- function(state, label = NA, lCi = NA, uCi = NA, test = "", face = NA, drawRectangle = TRUE, mapColor = NA) {

  row <- state[["row"]]
  state[["information"]][[row]] <- .forestPlotCreateAdditionalRow(
    label = label,
    row   = row,
    est   = NA,
    lCi   = lCi,
    uCi   = uCi,
    test  = test,
    face  = face
  )

  if (drawRectangle) {
    state[["objects"]][[row]] <- .maMakeRectangleDataFrame(
      lCi = lCi,
      uCi = uCi,
      row = row,
      id  = row
    )
    state[["objects"]][[row]]$mapColor <- mapColor
  }

  state[["row"]] <- row + 1

  return(state)
}
.forestPlotAppendAdditionalSpacer      <- function(state) {

  state[["row"]] <- state[["row"]] + 1

  return(state)
}
.forestPlotFinalizeAdditionalSectionState <- function(state) {

  return(list(
    information = .forestPlotBindDataFrames(state[["information"]]),
    objects     = .forestPlotBindDataFrames(state[["objects"]])
  ))
}
.forestPlotBuildEstimatedMarginalMeans <- function(fit, options){

  ### builds the estimated marginal means section
  # returns a
  # - data frame with the estimated marginal means information
  # - a list with the estimated marginal means prediction intervals

  # return null on error
  if (is.null(fit) || jaspBase::isTryError(fit)) {
    return(NULL)
  }

  dataset <- attr(fit, "dataset")

  # Make sure no multiple prediction intervals are drawn for complex models or Bayesian models
  if (.mammHasMultipleHeterogeneities(options)) {
    options[["predictionIntervals"]]           <- FALSE
    options[["forestPlotPredictionIntervals"]] <- FALSE
  }
  if (!.maIsClassical(options)) {
    options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] <- options[["forestPlotEstimatedMarginalMeansCoefficientTestsAgainst0"]]
    options[["predictionIntervals"]]           <- FALSE
    options[["forestPlotPredictionIntervals"]] <- FALSE

    # disable tests when no averaging is performed
    if (!(options[["bayesianModelAveragingModerations"]] || options[["bayesianModelAveragingEffectSize"]])) {
      options[["forestPlotEstimatedMarginalMeansTermTests"]]        <- FALSE
      options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] <- FALSE
    }
  }


  state <- .forestPlotCreateAdditionalSectionState()

  # terms and levels information
  estimatedMarginalMeansTestsStatistics  <- options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue"
  estimatedMarginalMeansVariables        <- unlist(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]])

  # statistics position adjustment
  estimatedMarginalMeansTermsTestsRight  <- options[["forestPlotEstimatedMarginalMeansTermTests"]] && options[["forestPlotTestsInRightPanel"]]
  estimatedMarginalMeansTermsTestsLeft   <- options[["forestPlotEstimatedMarginalMeansTermTests"]] && !options[["forestPlotTestsInRightPanel"]]

  estimatedMarginalMeansCoefficientTestsRight <- options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] && options[["forestPlotTestsInRightPanel"]]
  estimatedMarginalMeansCoefficientTestsBelow <- options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] && !options[["forestPlotTestsInRightPanel"]] && options[["forestPlotPredictionIntervals"]]
  estimatedMarginalMeansCoefficientTestsLeft  <- options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] && !options[["forestPlotTestsInRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

  # add marginal estimates
  for (i in seq_along(estimatedMarginalMeansVariables)) {

    if (.maIsClassical(options)) {
      tempTermTest               <- .maTermTests(fit, options, estimatedMarginalMeansVariables[i])
      tempEstimatedMarginalMeans <- .maComputeMarginalMeansVariable(fit, options, estimatedMarginalMeansVariables[i], options[["forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"]], "effectSize")
      tempTestText               <- .maPrintTermTest(tempTermTest, estimatedMarginalMeansTestsStatistics)
    } else {
      tempTermTest               <- .robmaTermTests(fit, options, estimatedMarginalMeansVariables[i])
      tempEstimatedMarginalMeans <- .robmaComputeMarginalMeansVariable(list("fit" = fit), options, estimatedMarginalMeansVariables[i], conditional = options[["forestPlotConditionalEstimates"]])
      tempTestText               <- .robmaPrintBfTest(tempTermTest, options)
    }


    # add term information
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (estimatedMarginalMeansTermsTestsLeft) paste0(estimatedMarginalMeansVariables[i], ": ", tempTestText) else estimatedMarginalMeansVariables[i],
      test  = if (estimatedMarginalMeansTermsTestsRight) tempTestText else ""
    )

    # add levels information
    for (j in 1:nrow(tempEstimatedMarginalMeans)) {

      if (.maIsClassical(options)) {
        tempCoefficientTest <- .maPrintCoefficientTest(tempEstimatedMarginalMeans[j,], estimatedMarginalMeansTestsStatistics)
      } else {
        tempCoefficientTest <- .robmaPrintBfTest(tempEstimatedMarginalMeans[j,], options)
      }

      state <- .forestPlotAppendAdditionalDiamond(
        state    = state,
        label    = if (estimatedMarginalMeansCoefficientTestsLeft) paste0(tempEstimatedMarginalMeans$value[j], ": ", tempCoefficientTest) else tempEstimatedMarginalMeans$value[j],
        est      = tempEstimatedMarginalMeans$est[j],
        lCi      = tempEstimatedMarginalMeans$lCi[j],
        uCi      = tempEstimatedMarginalMeans$uCi[j],
        test     = if (estimatedMarginalMeansCoefficientTestsRight) tempCoefficientTest else "",
        face     = "italic",
        mapColor = if (options[["forestPlotMappingColor"]] == estimatedMarginalMeansVariables[i]) tempEstimatedMarginalMeans$value[j] else NA
      )


      if (options[["forestPlotPredictionIntervals"]] || estimatedMarginalMeansCoefficientTestsBelow) {

        state <- .forestPlotAppendAdditionalInterval(
          state         = state,
          label         = if (estimatedMarginalMeansCoefficientTestsBelow) tempCoefficientTest else NA,
          lCi           = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$lPi[j] else NA,
          uCi           = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$uPi[j] else NA,
          drawRectangle = options[["forestPlotPredictionIntervals"]],
          mapColor      = if (options[["forestPlotMappingColor"]] == estimatedMarginalMeansVariables[i]) tempEstimatedMarginalMeans$value[j] else NA
        )
      }
    }

    # add empty row
    state <- .forestPlotAppendAdditionalSpacer(state)
  }

  # add adjusted effect size estimate
  if (options[["forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"]]) {

    if (.maIsClassical(options)) {
      tempEstimatedMarginalMeans <- .maComputeMarginalMeansVariable(fit, options, "", options[["forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"]] , "effectSize")
      tempCoefficientTest <- .maPrintCoefficientTest(tempEstimatedMarginalMeans, options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue")
    } else {
      tempEstimatedMarginalMeans <- .robmaComputeMarginalMeansVariable(list("fit" = fit), options, "intercept", conditional = options[["forestPlotConditionalEstimates"]])
      tempCoefficientTest <- .robmaPrintBfTest(tempEstimatedMarginalMeans[1,], options)
    }

    state <- .forestPlotAppendAdditionalDiamond(
      state = state,
      label = if (estimatedMarginalMeansCoefficientTestsLeft) paste0(gettext("Adjusted Estimate"), ": ", tempCoefficientTest) else gettext("Adjusted Estimate"),
      est   = tempEstimatedMarginalMeans$est,
      lCi   = tempEstimatedMarginalMeans$lCi,
      uCi   = tempEstimatedMarginalMeans$uCi,
      test  = if (estimatedMarginalMeansCoefficientTestsRight) tempCoefficientTest else ""
    )

    if (options[["forestPlotPredictionIntervals"]] || estimatedMarginalMeansCoefficientTestsBelow) {

      state <- .forestPlotAppendAdditionalInterval(
        state         = state,
        label         = if(estimatedMarginalMeansCoefficientTestsBelow) tempCoefficientTest else NA,
        lCi           = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$lPi else NA,
        uCi           = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$uPi else NA,
        drawRectangle = options[["forestPlotPredictionIntervals"]]
      )
    }
  }

  # return
  return(.forestPlotFinalizeAdditionalSectionState(state))
}
.forestPlotBuildModelInformation       <- function(fit, options){

  ### builds the model information section
  # returns a
  # - data frame with the model information information
  # - a list with the model information prediction intervals

  # return null on error
  if (is.null(fit) || jaspBase::isTryError(fit)) {
    return(NULL)
  }

  # return null if no model information is requested
  if (!any(unlist(options[c(
    "forestPlotEffectSizeFixedEffectEstimate",
    "forestPlotEffectSizeFixedEffectTest",
    "forestPlotEffectSizePooledEstimate",
    "forestPlotEffectSizePooledEstimateTest",
    "forestPlotEffectSizeModerationTest",
    "forestPlotHeterogeneityTest",
    "forestPlotHeterogeneityEstimateTau",
    "forestPlotHeterogeneityEstimateTau2",
    "forestPlotHeterogeneityEstimateI2",
    "forestPlotHeterogeneityEstimateH2",
    "forestPlotHeterogeneityModerationTest"
  )])))
    return(NULL)

  # Make sure no multiple prediction intervals are drawn for complex models
  if (.mammHasMultipleHeterogeneities(options)) {
    options[["predictionIntervals"]]           <- FALSE
    options[["forestPlotPredictionIntervals"]] <- FALSE
  }

  state <- .forestPlotCreateAdditionalSectionState()

  if (options[["forestPlotHeterogeneityTest"]] && (.maIsClassical(options) || options[["bayesianModelAveragingHeterogeneity"]])) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (.maIsClassical(options)) .maPrintQTest(fit) else .robmaPrintTest(fit, options, "heterogeneity")
    )
  }

  if (!.maGetMethodOptions(options) %in% c("FE", "EE", "MH", "PETO") && options[["forestPlotHeterogeneityEstimateTau"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (.maIsClassical(options)) .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau")
        else .robmaPrintPooledEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau", conditional = options[["forestPlotConditionalEstimates"]])
    )
  }

  if ((!.maGetMethodOptions(options) %in% c("FE", "EE", "MH", "PETO")) && options[["forestPlotHeterogeneityEstimateTau2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (.maIsClassical(options)) .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau2")
        else .robmaPrintPooledEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau2", conditional = options[["forestPlotConditionalEstimates"]])
    )
  }

  if (!.maGetMethodOptions(options) %in% c("FE", "EE", "MH", "PETO") && !.maIsMetaregressionHeterogeneity(options) && options[["forestPlotHeterogeneityEstimateI2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (.maIsClassical(options)) .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "I2")
        else .robmaPrintPooledEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "I2", conditional = options[["forestPlotConditionalEstimates"]])
    )
  }

  if (.maGetMethodOptions(options) %in% c("MH", "PETO") && options[["forestPlotHeterogeneityEstimateI2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .mamhpPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "I2")
    )
  }

  if (!.maGetMethodOptions(options) %in% c("FE", "EE", "MH", "PETO") && !.maIsMetaregressionHeterogeneity(options) && options[["forestPlotHeterogeneityEstimateH2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = if (.maIsClassical(options)) .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "H2")
        else .robmaPrintPooledEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "H2", conditional = options[["forestPlotConditionalEstimates"]])
    )
  }

  if (.maGetMethodOptions(options) %in% c("MH", "PETO") && options[["forestPlotHeterogeneityEstimateH2"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .mamhpPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "H2")
    )
  }

  if (.maIsMetaregressionEffectSize(options) && options[["forestPlotEffectSizeModerationTest"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .maPrintModerationTest(fit, options, par = "effectSize")
    )
  }

  if (.maIsMetaregressionHeterogeneity(options) && options[["forestPlotHeterogeneityModerationTest"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = .maPrintModerationTest(fit, options, par = "heterogeneity")
    )
  }

  if (options[["forestPlotPublicationBiasTest"]]) {
    state <- .forestPlotAppendAdditionalRow(
      state = state,
      label = gettextf("Publication Bias: %1$s", .robmaPrintTest(fit, options, "bias", includeName = FALSE))
    )
  }

  if (options[["forestPlotEffectSizeFixedEffectEstimate"]]) {

    pooledEffectSizeTestsRight <- options[["forestPlotEffectSizeFixedEffectTest"]] && options[["forestPlotTestsInRightPanel"]]
    pooledEffectSizeTestsBelow <- options[["forestPlotEffectSizeFixedEffectTest"]] && !options[["forestPlotTestsInRightPanel"]] && options[["forestPlotPredictionIntervals"]]
    pooledEffectSizeTestsLeft  <- options[["forestPlotEffectSizeFixedEffectTest"]] && !options[["forestPlotTestsInRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

    tempPooledEstimate <- try(.maComputePooledEffectPlot(fit, options, forceFixed = TRUE))
    if (jaspBase::isTryError(tempPooledEstimate)) {
      stop(gettext("The fixed effect effect size could not be calculated."))
    }
    tempTestText       <- .maPrintCoefficientTest(tempPooledEstimate, options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue")

    state <- .forestPlotAppendAdditionalDiamond(
      state = state,
      label = if (pooledEffectSizeTestsLeft) paste0(gettext("Fixed Effect Estimate"), ": ", tempTestText) else gettext("Fixed Effect Estimate"),
      est   = tempPooledEstimate$est,
      lCi   = tempPooledEstimate$lCi,
      uCi   = tempPooledEstimate$uCi,
      test  = if (pooledEffectSizeTestsRight) tempTestText else ""
    )

    if (pooledEffectSizeTestsBelow) {
      state <- .forestPlotAppendAdditionalRow(
        state = state,
        label = if (pooledEffectSizeTestsBelow) tempTestText else NA
      )
    }
  }

  if (.maIsClassical(options) && options[["forestPlotEffectSizePooledEstimate"]]) {

    pooledEffectSizeTestsRight <- options[["forestPlotEffectSizePooledEstimateTest"]] && options[["forestPlotTestsInRightPanel"]]
    pooledEffectSizeTestsBelow <- options[["forestPlotEffectSizePooledEstimateTest"]] && !options[["forestPlotTestsInRightPanel"]] && options[["forestPlotPredictionIntervals"]]
    pooledEffectSizeTestsLeft  <- options[["forestPlotEffectSizePooledEstimateTest"]] && !options[["forestPlotTestsInRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

    effectSizeName     <- gettext("Pooled Effect")
    tempPooledEstimate <- try(.maComputePooledEffectPlot(fit, options))
    if (jaspBase::isTryError(tempPooledEstimate)) {
      stop(gettext("The pooled effect size could not be calculated."))
    }
    tempTestText       <- .maPrintCoefficientTest(tempPooledEstimate, options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue")


    state <- .forestPlotAppendAdditionalDiamond(
      state = state,
      label = if (pooledEffectSizeTestsLeft) paste0(effectSizeName, ": ", tempTestText) else effectSizeName,
      est   = tempPooledEstimate$est,
      lCi   = tempPooledEstimate$lCi,
      uCi   = tempPooledEstimate$uCi,
      test  = if (pooledEffectSizeTestsRight) tempTestText else ""
    )

    if (pooledEffectSizeTestsBelow || options[["forestPlotPredictionIntervals"]]) {
      state <- .forestPlotAppendAdditionalInterval(
        state         = state,
        label         = if (pooledEffectSizeTestsBelow) tempTestText else NA,
        lCi           = if (options[["forestPlotPredictionIntervals"]]) tempPooledEstimate$lPi else NA,
        uCi           = if (options[["forestPlotPredictionIntervals"]]) tempPooledEstimate$uPi else NA,
        drawRectangle = options[["forestPlotPredictionIntervals"]]
      )
    }
  }

  if (!.maIsClassical(options) && options[["forestPlotEffectSizePooledEstimate"]]) {

    pooledEffectSizeTestsRight <- options[["bayesianModelAveragingEffectSize"]] && options[["forestPlotEffectSizePooledEstimateTest"]] && options[["forestPlotTestsInRightPanel"]]
    pooledEffectSizeTestsBelow <- options[["bayesianModelAveragingEffectSize"]] && options[["forestPlotEffectSizePooledEstimateTest"]] && !options[["forestPlotTestsInRightPanel"]] && options[["forestPlotPredictionIntervals"]]
    pooledEffectSizeTestsLeft  <- options[["bayesianModelAveragingEffectSize"]] && options[["forestPlotEffectSizePooledEstimateTest"]] && !options[["forestPlotTestsInRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

    effectSizeName         <- gettext("Pooled Effect")
    tempPooledEstimate     <- .robmaComputePooledEffect(fit, options, conditional = options[["forestPlotConditionalEstimates"]])
    tempPooledEstimate$est <- tempPooledEstimate$mean
    tempTestText           <- .robmaPrintTest(fit, options, "effect", includeName = FALSE)

    if (!.maIsMetaregression(options)) {

      # only in nonmeta-regression models the pooled effect size matches the overall test
      state <- .forestPlotAppendAdditionalDiamond(
        state = state,
        label = if (pooledEffectSizeTestsLeft) paste0(effectSizeName, ": ", tempTestText) else effectSizeName,
        est   = tempPooledEstimate$est,
        lCi   = tempPooledEstimate$lCi,
        uCi   = tempPooledEstimate$uCi,
        test  = if (pooledEffectSizeTestsRight) tempTestText else ""
      )

      if (pooledEffectSizeTestsBelow || options[["forestPlotPredictionIntervals"]]) {
        state <- .forestPlotAppendAdditionalInterval(
          state         = state,
          label         = if (pooledEffectSizeTestsBelow) tempTestText else NA,
          lCi           = if (options[["forestPlotPredictionIntervals"]]) tempPooledEstimate$lPi else NA,
          uCi           = if (options[["forestPlotPredictionIntervals"]]) tempPooledEstimate$uPi else NA,
          drawRectangle = options[["forestPlotPredictionIntervals"]]
        )
      }

    } else {


      # only in nonmeta-regression models the pooled effect size matches the overall test
      state <- .forestPlotAppendAdditionalDiamond(
        state = state,
        label = effectSizeName,
        est   = tempPooledEstimate$est,
        lCi   = tempPooledEstimate$lCi,
        uCi   = tempPooledEstimate$uCi
      )

      if (options[["forestPlotPredictionIntervals"]]) {
        state <- .forestPlotAppendAdditionalInterval(
          state         = state,
          lCi           = tempPooledEstimate$lPi,
          uCi           = tempPooledEstimate$uPi,
          drawRectangle = TRUE
        )
      }

      if (options[["forestPlotEffectSizePooledEstimateTest"]]) {

        # add adjusted effect size for meta-regression since they match the meta-analytic test
        if (.robmaIsMetaregressionCentered(options)) {
          tempTestEstimate     <- .robmaComputeAdjustedEffect(fit, options, conditional = options[["forestPlotConditionalEstimates"]])
          tempTestEstimate$est <- tempPooledEstimate$mean
          effectSizeName       <- gettext("Adjusted Estimate")
        } else {
          tempTestEstimate     <- .robmaComputeInterceptEffect(fit, options, conditional = options[["forestPlotConditionalEstimates"]])
          tempTestEstimate$est <- tempPooledEstimate$mean
          effectSizeName       <- gettext("Intercept Estimate")
        }

        state <- .forestPlotAppendAdditionalDiamond(
          state = state,
          label = if (pooledEffectSizeTestsLeft) paste0(effectSizeName, ": ", tempTestText) else effectSizeName,
          est   = tempTestEstimate$est,
          lCi   = tempTestEstimate$lCi,
          uCi   = tempTestEstimate$uCi,
          test  = if (pooledEffectSizeTestsRight) tempTestText else ""
        )

        if (pooledEffectSizeTestsBelow || options[["forestPlotPredictionIntervals"]]) {
          state <- .forestPlotAppendAdditionalInterval(
            state         = state,
            label         = if (pooledEffectSizeTestsBelow) tempTestText else NA,
            lCi           = if (options[["forestPlotPredictionIntervals"]]) tempTestEstimate$lPi else NA,
            uCi           = if (options[["forestPlotPredictionIntervals"]]) tempTestEstimate$uPi else NA,
            drawRectangle = options[["forestPlotPredictionIntervals"]] && .robmaIsMetaregressionCentered(options)
          )
        }
      }

    }
  }


  # return
  return(.forestPlotFinalizeAdditionalSectionState(state))
}
.forestPlotBuildStudyInformationHeader <- function(options, forestInformation, additionalInformation) {

  # get study information panel header
  leftPanelStudyInformation <- do.call(rbind.data.frame, options[["forestPlotStudyInformationSelectedVariablesSettings"]])

  ### compute the total character width
  if (options[["forestPlotStudyInformation"]] && length(leftPanelStudyInformation) != 0) {
    leftPanelStudyInformationChars <- rbind(
      nchar(leftPanelStudyInformation$title),
      apply(forestInformation[,leftPanelStudyInformation$value, drop = FALSE], 2, function(x) max(nchar(x), na.rm = TRUE)))
    leftPanelStudyInformationChars <- apply(leftPanelStudyInformationChars, 2, max) + 2
    maxCharsLeft <- sum(leftPanelStudyInformationChars)
  } else {
    leftPanelStudyInformationChars <- 0
    maxCharsLeft <- 0
  }
  if (length(additionalInformation) != 0) {
    additionalInformationChars <- max(nchar(additionalInformation$label), na.rm = TRUE)
    maxCharsLeft <- max(c(maxCharsLeft, additionalInformationChars))
  } else {
    additionalInformationChars <- 0
  }

  # split the columns
  if (options[["forestPlotAuxiliaryAdjustWidthBasedOnText"]]) {
    leftPanelRelativeWidths <- c(maxCharsLeft - sum(leftPanelStudyInformationChars), leftPanelStudyInformationChars)
    leftPanelRelativeWidths[2:length(leftPanelRelativeWidths)] <- leftPanelRelativeWidths[2:length(leftPanelRelativeWidths)] * leftPanelStudyInformation$width
    leftPanelRelativeWidths <- leftPanelRelativeWidths / sum(leftPanelRelativeWidths)
    leftPanelStudyInformation$xStart    <- cumsum(leftPanelRelativeWidths[-length(leftPanelRelativeWidths)])
    leftPanelStudyInformation$xEnd      <- cumsum(leftPanelRelativeWidths)[-1]
  } else {
    leftPanelRelativeWidths <- leftPanelStudyInformation$width / sum(leftPanelStudyInformation$width)
    leftPanelStudyInformation$xStart <- c(0, cumsum(leftPanelRelativeWidths[-length(leftPanelRelativeWidths)]))
    leftPanelStudyInformation$xEnd   <- cumsum(leftPanelRelativeWidths)
  }

  attr(leftPanelStudyInformation, "maxChars") <- maxCharsLeft
  return(leftPanelStudyInformation)
}
.forestPlotHasStudyInformationHeader   <- function(options) {

  # get study information panel header
  leftPanelStudyInformation <- do.call(rbind.data.frame, options[["forestPlotStudyInformationSelectedVariablesSettings"]])

  # check if any titles specified
  return(any(leftPanelStudyInformation[["title"]] != ""))
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
    "label"  = if (subgroup == gettext("Full dataset")) gettext("Full Dataset") else gettextf("Subgroup: %1$s", subgroup),
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
    options[["forestPlotAllignLeftPanel"]],
    "left"   = 0,
    "middle" = 0.5,
    "right"  = 1
  ))
}
.forestPlotLeftPanelHjust              <- function(options) {
  return(switch(
    options[["forestPlotAllignLeftPanel"]],
    "left"   = 0,
    "middle" = 0.5,
    "right"  = 1
  ))
}
.forestPlotAggregateVariable           <- function(x) {
  if (length(unique(x)) == 1) {
    return(unique(x))
  } else {
    x <- table(x)
    x <- x[x > 0]
    xNames <- names(x)
    xFreqs <- paste0(" (", x, ")")
    xFreqs[xFreqs == " (1)"] <- ""
    return(paste0(xNames, xFreqs, collapse = ", "))
  }
}
.forestStudyInformationAggregate       <- function(dfForest, options, additionalVariables) {

  # split the data set by the grouping variable
  datasetSplit <- split(dfForest, dfForest[[options[["forestPlotStudyInformationAggregateBy"]]]])

  # add id to each split index
  for (i in seq_along(datasetSplit)) {
    datasetSplit[[i]]$id <- i
  }

  if (options[["forestPlotStudyInformationAggregateMethod"]] == "boxplot") {

    datasetAggregated <- do.call(rbind, lapply(datasetSplit, function(df) {

      # create a base of the geom
      tempDf <- data.frame(
        id     = df$id[1],
        min    = min(df$effectSize),
        lower  = quantile(df$effectSize, 0.25),
        middle = median(df$effectSize),
        upper  = quantile(df$effectSize, 0.75),
        max    = max(df$effectSize),
        geom   = "boxplot"
      )

      # add the additional variables
      for (var in additionalVariables) {
        tempDf[[var]] <- .forestPlotAggregateVariable(df[[var]])
      }

      return(tempDf)
    }))

    # split into study information and geoms
    dfGeoms  <- datasetAggregated
    dfForest <- datasetAggregated[,!colnames(datasetAggregated) %in% c("min", "lower", "middle", "upper", "max", "geom"),drop=FALSE]


  } else if (options[["forestPlotStudyInformationAggregateMethod"]] == "bubbles") {

    dfForest <- do.call(rbind, lapply(datasetSplit, function(df) {

      tempDf <- data.frame(
        id     = df$id[1]
      )

      # add the additional variables
      for (var in additionalVariables) {
        tempDf[[var]] <- .forestPlotAggregateVariable(df[[var]])
      }

      return(tempDf)
    }))
    dfGeoms  <- do.call(rbind, lapply(datasetSplit, function(df) {

      # create a base of the geom
      tempDf <- data.frame(
        id      = df$id[1],
        x       = df$effectSize,
        weight  = 1/df$standardError^2,
        geom    = "bubbles"
      )

      # add the additional variables
      for (var in additionalVariables) {
        tempDf[[var]] <- .forestPlotAggregateVariable(df[[var]])
      }

      return(tempDf)
    }))

  }

  return(list(
    forest = dfForest,
    geoms  = dfGeoms
  ))
}
