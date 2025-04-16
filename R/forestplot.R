.maMakeTheUltimateForestPlot <- function(fit, options) {

  # in case of subgroups, reorder the fit objects so the subgroup output is first
  if (options[["subgroup"]] != "") {
    fit <- fit[c(2:length(fit), 1)]
  }

  # remove failed model
  fit <- fit[!sapply(fit, jaspBase::isTryError)]

  # overwrite basic options used in the pooled effect/marginal means
  options[["confidenceIntervals"]] <- TRUE
  options[["predictionIntervals"]] <- options[["forestPlotPredictionIntervals"]]

  ### initiate objects for generating the forest plot ----
  forestInput                 <- list() # this list carries the study information
  estimatedMarginalMeansInput <- list() # this list carries estimated marginal means information
  modelInformationInput       <- list() # this list carries model information

  ### create the inputs ----
  for (i in seq_along(fit)) {

    ### Study Information panel
    if (options[["forestPlotStudyInformation"]]) {
      forestInput[[names(fit)[i]]] <- .forestPlotBuildStudyInformation(fit[[i]], options)
    }

    ### Estimated marginal means panel
    if (options[["forestPlotEstimatedMarginalMeans"]] &&
        (length(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]]) > 0 || options[["forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"]])) {
      estimatedMarginalMeansInput[[names(fit)[i]]] <- .forestPlotBuildEstimatedMarginalMeans(fit[[i]], options)
    }

    ### Model information panel
    if (options[["forestPlotModelInformation"]]) {
      modelInformationInput[[names(fit)[i]]] <- .forestPlotBuildModelInformation(fit[[i]], options)
    }

  }

  ### reorder the inputs ----
  forestHeaderIndex     <- NULL
  forestInformation     <- NULL
  forestObjects         <- NULL
  additionalInformation <- NULL
  additionalObjects     <- NULL
  tempRow <- 1

  if (options[["subgroup"]] == "" || (options[["subgroup"]] != "" && options[["forestPlotSubgroupPanelsWithinSubgroup"]])) {

    for (i in seq_along(fit)) {

      # skip individual sections in case a subgroup analysis is performed and we are at the full fit object
      if (i == length(fit) && options[["subgroup"]] != "") {
        doForest                 <- FALSE
        doEstimatedMarginalMeans <- options[["forestPlotSubgroupFullDatasetEstimatedMarginalMeans"]]
        doModelInformation       <- options[["forestPlotSubgroupFullDatasetModelInformation"]]
      } else {
        doForest                 <- TRUE
        doEstimatedMarginalMeans <- TRUE
        doModelInformation       <- TRUE
      }

      # extract forest input
      if (doForest && length(forestInput) > 0) {

        # add space for forest header if specified
        if (.forestPlotHasStudyInformationHeader(options)) {
          forestHeaderIndex <- c(forestHeaderIndex, tempRow)
          tempRow           <- tempRow + 1
        }

        tempForestInformation   <- forestInput[[i]][["forest"]]
        tempForestInformation$y <- tempForestInformation$y + (tempRow - 1)
        forestInformation[[length(forestInformation) + 1]]  <- tempForestInformation

        if (!is.null(forestInput[[i]][["prediction"]])) {
          tempForestObjects    <- forestInput[[i]][["prediction"]]
          tempForestObjects$y  <- tempForestInformation$y + (tempRow - 1)
          tempForestObjects$id <- paste(tempForestObjects$id, i, sep = "_")
          forestObjects[[length(forestObjects) + 1]]   <- tempForestObjects
        }

        tempRow <- tempRow + nrow(tempForestInformation) + 1
      }

      # extract estimated marginal means input
      if (doEstimatedMarginalMeans && length(estimatedMarginalMeansInput) > 0){

        tempAdditionalInformation   <- estimatedMarginalMeansInput[[i]][["information"]]
        tempAdditionalInformation$y <- tempAdditionalInformation$y + (tempRow - 1)
        additionalInformation[[length(additionalInformation) + 1]]  <- tempAdditionalInformation

        if (!is.null(estimatedMarginalMeansInput[[i]][["objects"]])) {
          tempAdditionalObjects    <- estimatedMarginalMeansInput[[i]][["objects"]]
          tempAdditionalObjects$y  <- tempAdditionalObjects$y + (tempRow - 1)
          tempAdditionalObjects$id <- paste(tempAdditionalObjects$id, "emm", i, sep = "_")
          additionalObjects[[length(additionalObjects) + 1]]   <- tempAdditionalObjects
        }

        tempRow <- tempRow + nrow(tempAdditionalInformation) + 1
      }

      # extract model information input
      if (doModelInformation && length(modelInformationInput) > 0) {

        tempAdditionalInformation   <- modelInformationInput[[i]][["information"]]
        tempAdditionalInformation$y <- tempAdditionalInformation$y + (tempRow - 1)
        additionalInformation[[length(additionalInformation) + 1]]  <- tempAdditionalInformation

        if (!is.null(modelInformationInput[[i]][["objects"]])) {
          tempAdditionalObjects    <- modelInformationInput[[i]][["objects"]]
          tempAdditionalObjects$y  <- tempAdditionalObjects$y + (tempRow - 1)
          tempAdditionalObjects$id <- paste(tempAdditionalObjects$id, "mi", i, sep = "_")
          additionalObjects[[length(additionalObjects) + 1]]   <- tempAdditionalObjects
        }

        tempRow <- tempRow + nrow(tempAdditionalInformation) + 1
      }
    }
  } else {

    # extract forest input
    if (length(forestInput) > 0) {

      for (i in seq_along(fit)) {

        # add space for forest header if specified
        if (.forestPlotHasStudyInformationHeader(options)) {
          forestHeaderIndex <- c(forestHeaderIndex, tempRow)
          tempRow           <- tempRow + 1
        }

        # skip individual sections in case a subgroup analysis is performed and we are at the full fit object
        # (which is in the end of the order)
        if (i == length(fit) && options[["subgroup"]] != "")
          next

        tempForestInformation   <- forestInput[[i]][["forest"]]
        tempForestInformation$y <- tempForestInformation$y + (tempRow - 1)
        forestInformation[[length(forestInformation) + 1]]  <- tempForestInformation

        if (!is.null(forestInput[[i]][["prediction"]])) {
          tempForestObjects    <- forestInput[[i]][["prediction"]]
          tempForestObjects$y  <- tempForestInformation$y + (tempRow - 1)
          tempForestObjects$id <- paste(tempForestObjects$id, i, sep = "_")
          forestObjects[[length(forestObjects) + 1]]   <- tempForestObjects
        }

        tempRow <- tempRow + nrow(tempForestInformation) + 1
      }
    }

    # extract estimated marginal means input
    if (length(estimatedMarginalMeansInput) > 0){

      for (i in seq_along(fit)) {

        # skip individual sections in case a subgroup analysis is performed and we are at the full fit object
        if (i == length(fit) && options[["subgroup"]] != "" && !options[["forestPlotSubgroupFullDatasetEstimatedMarginalMeans"]])
          next

        tempAdditionalInformation   <- estimatedMarginalMeansInput[[i]][["information"]]
        tempAdditionalInformation$y <- tempAdditionalInformation$y + (tempRow - 1)
        additionalInformation[[length(additionalInformation) + 1]]  <- tempAdditionalInformation

        if (!is.null(estimatedMarginalMeansInput[[i]][["objects"]])) {
          tempAdditionalObjects    <- estimatedMarginalMeansInput[[i]][["objects"]]
          tempAdditionalObjects$y  <- tempAdditionalObjects$y + (tempRow - 1)
          tempAdditionalObjects$id <- paste(tempAdditionalObjects$id, "emm", i, sep = "_")
          additionalObjects[[length(additionalObjects) + 1]]   <- tempAdditionalObjects
        }

        tempRow <- tempRow + nrow(tempAdditionalInformation) + 1
      }
    }

    # extract model information input
    if (length(modelInformationInput) > 0) {

      for (i in seq_along(fit)) {

        # skip individual sections in case a subgroup analysis is performed and we are at the full fit object
        if (i == length(fit) && options[["subgroup"]] != "" && !options[["forestPlotSubgroupFullDatasetModelInformation"]])
          next

        tempAdditionalInformation   <- modelInformationInput[[i]][["information"]]
        tempAdditionalInformation$y <- tempAdditionalInformation$y + (tempRow - 1)
        additionalInformation[[length(additionalInformation) + 1]]  <- tempAdditionalInformation

        if (!is.null(modelInformationInput[[i]][["objects"]])) {
          tempAdditionalObjects    <- modelInformationInput[[i]][["objects"]]
          tempAdditionalObjects$y  <- tempAdditionalObjects$y + (tempRow - 1)
          tempAdditionalObjects$id <- paste(tempAdditionalObjects$id, "mi", i, sep = "_")
          additionalObjects[[length(additionalObjects) + 1]]   <- tempAdditionalObjects
        }

        tempRow <- tempRow + nrow(tempAdditionalInformation) + 1
      }
    }
  }

  ### adjust the inputs ----
  # merge the parts
  if (!is.null(forestInformation))
    forestInformation     <- do.call(rbind, forestInformation[!sapply(forestInformation, is.null)])
  if (!is.null(forestObjects))
    forestObjects         <- do.call(rbind, forestObjects[!sapply(forestObjects, is.null)])
  if (!is.null(additionalInformation))
    additionalInformation <- do.call(rbind, additionalInformation[!sapply(additionalInformation, is.null)])
  if (!is.null(additionalObjects))
    additionalObjects     <- do.call(rbind, additionalObjects[!sapply(additionalObjects, is.null)])

  # adjust y-coordinates
  if (!is.null(forestHeaderIndex))
    forestHeaderIndex       <- - forestHeaderIndex       * options[["forestPlotRelativeSizeRow"]]
  if (!is.null(forestInformation))
    forestInformation$y     <- - forestInformation$y     * options[["forestPlotRelativeSizeRow"]]
  if (!is.null(forestObjects))
    forestObjects$y         <- - forestObjects$y         * options[["forestPlotRelativeSizeRow"]]
  if (!is.null(additionalInformation))
    additionalInformation$y <- - additionalInformation$y * options[["forestPlotRelativeSizeRow"]]
  if (!is.null(additionalObjects))
    additionalObjects$y     <- - additionalObjects$y     * options[["forestPlotRelativeSizeRow"]]


  ### make the forest plot ----
  plotForest <- ggplot2::ggplot()

  # study information panel estimates
  if (options[["forestPlotStudyInformation"]]) {

    # add prediction intervals
    if (options[["forestPlotStudyInformationPredictedEffects"]]) {
      # dispatch the aes call based on mapping
      aesCall <- list(
        x     = as.name("x"),
        y     = as.name("y"),
        group = as.name("id"),
        fill  = if (options[["forestPlotMappingColor"]] != "") as.name(options[["forestPlotMappingColor"]])
      )
      geomCall <- list(
        data    = forestObjects,
        mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
        fill    = if (options[["forestPlotMappingColor"]] == "") "grey20",
        alpha   = 0.8
      )
      plotForest <- plotForest + do.call(ggplot2::geom_polygon, geomCall[!sapply(geomCall, is.null)])
    }

    ### add estimates
    # dispatch the aes call based on mapping:
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


    # change scale for shapes to full shapes if used
    if (options[["forestPlotMappingShape"]] != "")
      plotForest <- plotForest + ggplot2::scale_shape_manual(values = rep(c(15:18, 21:25), length.out = length(unique(forestInformation[[options[["forestPlotMappingShape"]]]]))))


    ### add CIs
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

  # add additional information
  if (length(additionalInformation) > 0) {

    # dispatch the aes call based on color mapping
    if (any(!is.na(additionalObjects$mapColor))) {
      plotForest <- plotForest + ggplot2::geom_polygon(
        data    = additionalObjects[!is.na(additionalObjects$mapColor),],
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
        data    = additionalObjects[is.na(additionalObjects$mapColor),],
        mapping = ggplot2::aes(
          x     = x,
          y     = y,
          group = id,
        )
      )
    }
  }

  # add vertical line
  if (options[["forestPlotAuxiliaryAddVerticalLine"]])
    plotForest <- plotForest + ggplot2::geom_vline(xintercept = options[["forestPlotAuxiliaryAddVerticalLineValue"]], linetype = "dashed")
  if (options[["forestPlotAuxiliaryAddVerticalLine2"]])
    plotForest <- plotForest + ggplot2::geom_vline(xintercept = options[["forestPlotAuxiliaryAddVerticalLineValue2"]], linetype = "dotted")


  ### make the left information panel ----
  if ((options[["forestPlotStudyInformation"]] && length(options[["forestPlotStudyInformationSelectedVariablesSettings"]]) > 0) || length(additionalInformation) > 0) {

    plotLeft     <- ggplot2::ggplot()
    maxCharsLeft <- NULL

    # add forest information
    if (options[["forestPlotStudyInformation"]] && length(options[["forestPlotStudyInformationSelectedVariablesSettings"]]) > 0) {

      # build the study information header
      leftPanelStudyInformation <- .forestPlotBuildStudyInformationHeader(options, forestInformation, additionalInformation)
      maxCharsLeft              <- attr(leftPanelStudyInformation, "maxChars")

      # compute study information coordinates
      leftPanelStudyInformation$x <- ifelse(
        leftPanelStudyInformation$alignment == "left", leftPanelStudyInformation$xStart, ifelse(
          leftPanelStudyInformation$alignment == "middle", (leftPanelStudyInformation$xStart + leftPanelStudyInformation$xEnd) / 2, leftPanelStudyInformation$xEnd
        ))
      leftPanelStudyInformation <- do.call(rbind, lapply(forestHeaderIndex, function(y) {
        leftPanelStudyInformation$y <- y
        return(leftPanelStudyInformation)
      }))

      # add titles
      if (length(leftPanelStudyInformation) > 0) {
        plotLeft <- plotLeft + ggplot2::geom_text(
          data    = leftPanelStudyInformation,
          mapping = ggplot2::aes(
            x     = x,
            y     = y,
            label = title,
            hjust = alignment
          ),
          size     = 4 * options[["forestPlotRelativeSizeText"]],
          vjust    = "midle",
          fontface = "bold"
        )
      }

      # add information
      if (any(leftPanelStudyInformation$value == options[["forestPlotMappingColor"]])) {
        leftPanelStudyDataColored <- data.frame(
          x         = leftPanelStudyInformation$x[leftPanelStudyInformation$value == options[["forestPlotMappingColor"]]],
          y         = forestInformation$y,
          label     = as.character(forestInformation[[options[["forestPlotMappingColor"]]]]),
          alignment = leftPanelStudyInformation$alignment[leftPanelStudyInformation$value == options[["forestPlotMappingColor"]]]
        )
        plotLeft <- plotLeft + ggplot2::geom_text(
          data    = leftPanelStudyDataColored,
          mapping = ggplot2::aes(
            x     = x,
            y     = y,
            label = label,
            hjust = alignment,
            color = label
          ),
          size     = 4 * options[["forestPlotRelativeSizeText"]],
          vjust    = "midle",
        )
      }
      if (any(leftPanelStudyInformation$value != options[["forestPlotMappingColor"]])) {
        tempVariables <- unique(leftPanelStudyInformation$value[leftPanelStudyInformation$value != options[["forestPlotMappingColor"]]])
        leftPanelStudyData <- do.call(rbind.data.frame, lapply(tempVariables, function(variable) {
          data.frame(
            x         = leftPanelStudyInformation$x[leftPanelStudyInformation$value == variable],
            y         = forestInformation$y,
            label     = as.character(forestInformation[[variable]]),
            alignment = leftPanelStudyInformation$alignment[leftPanelStudyInformation$value == variable]
          )
        }))
        plotLeft <- plotLeft + ggplot2::geom_text(
          data    = leftPanelStudyData,
          mapping = ggplot2::aes(
            x     = x,
            y     = y,
            label = label,
            hjust = alignment
          ),
          size     = 4 * options[["forestPlotRelativeSizeText"]],
          vjust    = "midle",
        )
      }
    }

    # add additional information
    if (length(additionalInformation) > 0) {

      # subset left panel information only
      leftPanelAdditionalInformation   <- additionalInformation[!is.na(additionalInformation$label),]
      leftPanelAdditionalInformation$x <- 1
      leftPanelAdditionalInformation$face[is.na(leftPanelAdditionalInformation$face)] <- "plain"
      maxCharsLeft <- max(c(maxCharsLeft, max(nchar(leftPanelAdditionalInformation$label))), na.rm = TRUE)

      # add titles
      plotLeft <- plotLeft + ggplot2::geom_text(
        data    = leftPanelAdditionalInformation,
        mapping     = ggplot2::aes(
          x         = x,
          y         = y,
          label     = label,
          fontface  = face
        ),
        size     = 4 * options[["forestPlotRelativeSizeText"]],
        hjust    = "right",
        vjust    = "midle",
      )
    }

  } else {
    plotLeft <- NULL
  }

  ### make the right information panel ----
  if (.forestPlotHasrightPanel(options, additionalInformation)) {

    # estimates and confidence intervals
    if (options[["forestPlotEstimatesAndConfidenceIntervals"]]) {

      ### join the est and Cis for the right panel
      rightPanelCis <- rbind(
        if (options[["forestPlotStudyInformation"]]) {
          tempDf <- forestInformation[,c("y", "effectSize", "lCi", "uCi")]
          colnames(tempDf) <- c("y", "est", "lCi", "uCi")
          tempDf
        },
        if (length(additionalInformation) > 0) additionalInformation[,c("y", "est", "lCi", "uCi")]
      )

      # remove all NAs
      rightPanelCis <- rightPanelCis[!apply(rightPanelCis[,2:4], 1, function(x) all(is.na(x))),]

      # adjust the number formatings
      for (colName in c("est", "lCi", "uCi")) {
        rightPanelCis[!is.na(rightPanelCis[,colName]),colName] <- .maFormatDigits(
          rightPanelCis[!is.na(rightPanelCis[,colName]),colName],
          options[["forestPlotAuxiliaryDigits"]])
      }

      # deal with PIs and CIs separately
      rightPanelCis$label <- NA
      rightPanelCis$label[ is.na(rightPanelCis$est)] <- with(rightPanelCis[ is.na(rightPanelCis$est), ], paste0("PI [", lCi, ", ", uCi, "]"))
      rightPanelCis$label[!is.na(rightPanelCis$est)] <- with(rightPanelCis[!is.na(rightPanelCis$est), ], paste0(est, " [", lCi, ", ", uCi, "]"))

    } else {
      rightPanelCis <- NULL
    }

    ### tests and weights right panel
    rightPanelTestsAndWeights <- rbind(
      if (options[["forestPlotStudyInformation"]] > 0 && options[["forestPlotStudyInformationStudyWeights"]]) {
        tempDf <- forestInformation[,c("y", "weights")]
        tempDf$label <- paste0(sprintf(paste0("%1$.", options[["forestPlotAuxiliaryDigits"]], "f"), tempDf$weights), " %")
        tempDf[,c("y", "label")]
      },
      if (length(additionalInformation) > 0) {
        tempDf <- additionalInformation[,c("y", "test")]
        colnames(tempDf) <- c("y", "label")
        tempDf
      }
    )
    rightPanelTestsAndWeights <- rightPanelTestsAndWeights[rightPanelTestsAndWeights$label != "",]
    if (length(rightPanelTestsAndWeights) == 0 || nrow(rightPanelTestsAndWeights) == 0)
      rightPanelTestsAndWeights <- NULL

    ### compute the total character width
    if (!is.null(rightPanelCis)) {
      maxCharsRightCis <- max(nchar(rightPanelCis$label))
    } else {
      maxCharsRightCis <- 0
    }
    if (length(rightPanelTestsAndWeights) != 0) {
      maxCharsRightAdd <- max(nchar(rightPanelTestsAndWeights$label))
    } else {
      maxCharsRightAdd <- 0
    }
    maxCharsRight <- maxCharsRightCis + maxCharsRightAdd + 2


    ### start plotting
    plotRight <- ggplot2::ggplot()

    ### add the subplots
    if (!is.null(rightPanelCis)) {

      rightPanelCis$x <- maxCharsRightCis / maxCharsRight

      # add titles
      plotRight <- plotRight + ggplot2::geom_text(
        data    = rightPanelCis,
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

    if (length(rightPanelTestsAndWeights) > 0) {

      rightPanelTestsAndWeights$x <- (maxCharsRightCis + 2) / maxCharsRight

      # add titles
      plotRight <- plotRight + ggplot2::geom_text(
        data    = rightPanelTestsAndWeights,
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
  } else {
    plotRight <- NULL
  }

  ### adjust axis, themes, and labels ----

  # specify axis limits & breaks
  if (options[["forestPlotAuxiliarySetXAxisLimit"]]) {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(options[["forestPlotAuxiliarySetXAxisLimitLower"]], options[["forestPlotAuxiliarySetXAxisLimitUpper"]]))
  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(
      forestInformation$lCi, forestInformation$uCi,
      forestObjects$x,
      additionalInformation$lCi, additionalInformation$uCi,
      additionalObjects$x
    ), na.rm = TRUE))
  }
  xRange <- range(xBreaks)

  yRange <- c(
    min(c(
      forestInformation$y, forestObjects$y, additionalInformation$y, additionalObjects$y
    ), na.rm = TRUE), 0)
  yRange[1] <- yRange[1] - options[["forestPlotRelativeSizeRow"]]

  # fix plotting range
  plotForest <- plotForest + ggplot2::coord_cartesian(
    xlim   = xRange,
    ylim   = yRange,
    expand = FALSE
  ) + ggplot2::xlab(
    if (options[["forestPlotAuxiliaryEffectLabel"]] != "Effect Size")  options[["forestPlotAuxiliaryEffectLabel"]]
    else if (options[["transformEffectSize"]] == "none")               gettext("Effect Size")
    else                                                               .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])
  ) + ggplot2::theme(
    axis.line.y       = ggplot2::element_blank(),
    axis.line.x       = ggplot2::element_line(color = "black"),
    axis.text.y       = ggplot2::element_blank(),
    axis.text.x       = ggplot2::element_text(color = "black", size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
    axis.ticks.y      = ggplot2::element_blank(),
    axis.title.y      = ggplot2::element_blank(),
    axis.title.x      = ggplot2::element_text(color = "black", size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
    legend.position   = "none",
    panel.background  = ggplot2::element_blank(),
    panel.border      = ggplot2::element_blank(),
    panel.grid.major  = ggplot2::element_blank(),
    panel.grid.minor  = ggplot2::element_blank(),
    plot.background   = ggplot2::element_blank()
  )

  if (!is.null(plotLeft)) {
    plotLeft <- plotLeft + ggplot2::coord_cartesian(
      xlim   = c(0,1),
      ylim   = yRange,
      expand = FALSE
    ) + ggplot2::xlab("") + ggplot2::theme(
      axis.line         = ggplot2::element_blank(),
      axis.text.y       = ggplot2::element_blank(),
      axis.text.x       = ggplot2::element_text(color = NA, size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
      axis.ticks        = ggplot2::element_blank(),
      axis.title.y      = ggplot2::element_blank(),
      axis.title.x      = ggplot2::element_text(color = NA, size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
      legend.position   = "none",
      panel.background  = ggplot2::element_blank(),
      panel.border      = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      plot.background   = ggplot2::element_blank()
    )
  }

  if (!is.null(plotRight)) {
    plotRight <- plotRight + ggplot2::coord_cartesian(
      xlim   = c(0,1),
      ylim   = yRange,
      expand = FALSE
    ) + ggplot2::xlab("") + ggplot2::theme(
      axis.line         = ggplot2::element_blank(),
      axis.text.y       = ggplot2::element_blank(),
      axis.text.x       = ggplot2::element_text(color = NA, size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
      axis.ticks        = ggplot2::element_blank(),
      axis.title.y      = ggplot2::element_blank(),
      axis.title.x      = ggplot2::element_text(color = NA, size = 12 * options[["forestPlotRelativeSizeAxisLabels"]]),
      legend.position   = "none",
      panel.background  = ggplot2::element_blank(),
      panel.border      = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      plot.background   = ggplot2::element_blank()
    )
  }

  ### adjust panel plot widths
  plotsWidths <- c(
    if (!is.null(plotLeft)) options[["forestPlotRelativeSizeLeftPanel"]],
    options[["forestPlotRelativeSizeMiddlePanel"]],
    if (!is.null(plotRight)) options[["forestPlotRelativeSizeRightPanel"]]
  )
  if (options[["forestPlotAuxiliaryAdjustWidthBasedOnText"]] && length(plotsWidths) == 3) {
    plotsWidths[1] <- plotsWidths[1] * 2 * maxCharsLeft  / (maxCharsRight + maxCharsLeft)
    plotsWidths[3] <- plotsWidths[3] * 2 * maxCharsRight / (maxCharsRight + maxCharsLeft)
  }
  # compute ratio of main panel to side panels
  if (length(plotsWidths) != 1) {
    panelRatio <- sum(c(
      if (!is.null(plotLeft))  options[["forestPlotRelativeSizeLeftPanel"]] else 0,
      if (!is.null(plotRight)) options[["forestPlotRelativeSizeRightPanel"]] else 0
    )) / options[["forestPlotRelativeSizeMiddlePanel"]]
  }

  if (length(plotsWidths) == 1) {

    plotOut <- plotForest
    attr(plotOut, "isPanel") <- FALSE
    attr(plotOut, "rows")    <- tempRow + max(forestInformation$y)

  } else {

    plotOut <- list()
    if (!is.null(plotLeft))
      plotOut <-  c(plotOut, list(plotLeft))
    plotOut <-  c(plotOut, list(plotForest))
    if (!is.null(plotRight))
      plotOut <- c(plotOut, list(plotRight))

    attr(plotOut, "isPanel")     <- TRUE
    attr(plotOut, "panelRatio")  <- panelRatio
    attr(plotOut, "rows")        <- tempRow + if(!is.null(forestInformation)) max(forestInformation$y) else 0
    attr(plotOut, "widths")      <- plotsWidths
    attr(plotOut, "layout")      <- matrix(1:length(plotOut), nrow = 1, ncol = length(plotOut), byrow = TRUE)

  }

  return(plotOut)
}

.forestPlotBuildStudyInformation       <- function(fit, options){

  ### builds the study information section
  # returns a
  # - data frame with the study information
  # - data frame with the predicted effects

  # return null on error
  if (is.null(fit) || jaspBase::isTryError(fit))
    return(NULL)

  # extract data set forwarded via the fit object
  # allows dispatching from subfits, NAs removal matching the analysis etc
  dataset <- attr(fit, "dataset")

  ### extract effect sizes and variances from the fitted object
  dfForest <- data.frame(
    effectSize     = fit[["yi"]],
    standardError  = sqrt(fit[["vi"]]),
    weights        = weights(fit),
    id             = seq_along(fit[["yi"]])
  )

  # add CI using normal approximation
  dfForest$lCi <- dfForest$effectSize - qnorm((1 - options[["confidenceIntervalsLevel"]]) / 2, lower.tail = F) * dfForest$standardError
  dfForest$uCi <- dfForest$effectSize + qnorm((1 - options[["confidenceIntervalsLevel"]]) / 2, lower.tail = F) * dfForest$standardError

  # add secondary CI using normal approximation
  if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
    dfForest$lCi2 <- dfForest$effectSize - qnorm((1 - options[["forestPlotStudyInformationSecondaryConfidenceIntervalLevel"]]) / 2, lower.tail = F) * dfForest$standardError
    dfForest$uCi2 <- dfForest$effectSize + qnorm((1 - options[["forestPlotStudyInformationSecondaryConfidenceIntervalLevel"]]) / 2, lower.tail = F) * dfForest$standardError
  }

  # transform effect size when requested
  if (options[["transformEffectSize"]] != "none") {
    dfForest[,c(
      "effectSize", "lCi", "uCi",
      if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) c("lCi2", "uCi2"))] <- do.call(
        .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
        list(dfForest[,c(
          "effectSize", "lCi", "uCi",
          if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) c("lCi2", "uCi2"))]))
  }

  # add variables used for either color, shape, order or Left panel information
  additionalVariables <- c(
    if (length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) unlist(options[["forestPlotStudyInformationSelectedVariables"]]),
    if (options[["forestPlotStudyInformationOrderBy"]] != "")                 options[["forestPlotStudyInformationOrderBy"]],
    if (options[["forestPlotMappingColor"]] != "")                            options[["forestPlotMappingColor"]],
    if (options[["forestPlotMappingShape"]] != "")                            options[["forestPlotMappingShape"]]
  )
  if (length(additionalVariables) > 0)
    dfForest <- cbind(dfForest, dataset[,additionalVariables,drop=FALSE])

  # TODO: temporal fix for the variable names in the Component list not being properly translated
  # for (i in seq_along(options[["forestPlotStudyInformationSelectedVariables"]])) {
  #   options[["forestPlotStudyInformationSelectedVariablesSettings"]][[i]][["value"]] <- options[["forestPlotStudyInformationSelectedVariables"]][[i]]
  # }

  # re-order
  if (options[["forestPlotStudyInformationOrderBy"]] != "") {
    dfForest <- dfForest[order(
      dfForest[,options[["forestPlotStudyInformationOrderBy"]]],
      decreasing = options[["forestPlotStudyInformationOrderAscending"]]),]
  }

  # add y-axis coordinates for plotting
  dfForest$y <- seq(nrow(dfForest))


  ### add predicted effects
  if (options[["forestPlotStudyInformationPredictedEffects"]]) {

    dfForestPrediction <- data.frame(predict(fit))

    # replicate the prediction for each estimate if the predictions are the same (no moderators)
    if (nrow(dfForestPrediction) == 1)
      dfForestPrediction <- do.call(rbind, replicate(nrow(dfForest), dfForestPrediction, simplify = FALSE))

    dfForestPrediction$id <- dfForest$id
    dfForestPrediction$y  <- dfForest$y

    # create prediction diamond coordinates for each estimate
    dfForestPrediction <- do.call(rbind, lapply(1:nrow(dfForestPrediction), function(i) {
      with(dfForestPrediction[i,], .maMakeDiamondDataFrame(est = pred, lCi = pi.lb, uCi = pi.ub, row = y, id = id))
    }))

    dfForestPrediction <- merge(dfForestPrediction, dfForest[,!colnames(dfForest) %in% c("effectSize", "standardError", "weights", "lCi", "uCi", "y")], by = "id")

    # transform effect size when requested
    if (options[["transformEffectSize"]] != "none")
      dfForestPrediction[,"xPrediction"] <- do.call(
        .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
        list(dfForestPrediction[,"xPrediction"]))

  } else {
    dfForestPrediction <- NULL
  }

  # return
  return(list(
    forest     = dfForest,
    prediction = dfForestPrediction
  ))
}
.forestPlotBuildEstimatedMarginalMeans <- function(fit, options){

  ### builds the estimated marginal means section
  # returns a
  # - data frame with the estimated marginal means information
  # - a list with the estimated marginal means prediction intervals

  # return null on error
  if (is.null(fit) || jaspBase::isTryError(fit))
    return(NULL)

  dataset <- attr(fit, "dataset")

  # Make sure no multiple prediction intervals are drawn for complex models
  if (.mammHasMultipleHeterogeneities(options)) {
    options[["predictionIntervals"]]           <- FALSE
    options[["forestPlotPredictionIntervals"]] <- FALSE
  }

  # initiate the local objects
  tempRow <- 1
  additionalInformation <- list()
  additionalObjects     <- list()

  # terms and levels information
  estimatedMarginalMeansTestsStaistics   <- options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue"
  estimatedMarginalMeansVariables        <- unlist(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]])

  # statistics position adjustment
  estimatedMarginalMeansTermsTestsRight  <- options[["forestPlotEstimatedMarginalMeansTermTests"]] && options[["forestPlotTestsInRightPanel"]]
  estimatedMarginalMeansTermsTestsLeft   <- options[["forestPlotEstimatedMarginalMeansTermTests"]] && !options[["forestPlotTestsInRightPanel"]]

  estimatedMarginalMeansCoefficientTestsRight <- options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] && options[["forestPlotTestsInRightPanel"]]
  estimatedMarginalMeansCoefficientTestsBelow <- options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] && !options[["forestPlotTestsInRightPanel"]] && options[["forestPlotPredictionIntervals"]]
  estimatedMarginalMeansCoefficientTestsLeft  <- options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] && !options[["forestPlotTestsInRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

  # add header
  additionalInformation[[tempRow]] <- data.frame(
    "label"  = gettextf(
      "Estimated Marginal Means%1$s",
      if (options[["subgroup"]] != "") gettextf(" (Subgroup: %1$s)", attr(fit, "subgroup")) else ""
    ),
    "y"      = tempRow,
    "est"    = NA,
    "lCi"    = NA,
    "uCi"    = NA,
    "test"   = "",
    "face"   = "bold"
  )
  tempRow <- tempRow + 1

  # add marginal estimates
  for (i in seq_along(estimatedMarginalMeansVariables)) {

    tempTermTest               <- .maTermTests(fit, options, estimatedMarginalMeansVariables[i])
    tempEstimatedMarginalMeans <- .maComputeMarginalMeansVariable(fit, options, dataset, estimatedMarginalMeansVariables[i], options[["forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"]] , "effectSize")
    tempTestText               <- .maPrintTermTest(tempTermTest, estimatedMarginalMeansTestsStaistics)

    # add term information
    additionalInformation[[tempRow]] <- data.frame(
      "label"  = if (estimatedMarginalMeansTermsTestsLeft) paste0(estimatedMarginalMeansVariables[i], ": ", tempTestText) else estimatedMarginalMeansVariables[i],
      "y"      = tempRow,
      "est"    = NA,
      "lCi"    = NA,
      "uCi"    = NA,
      "test"   = if (estimatedMarginalMeansTermsTestsRight) tempTestText else "",
      "face"   = NA
    )
    tempRow <- tempRow + 1

    # add levels information
    for (j in 1:nrow(tempEstimatedMarginalMeans)) {

      tempCoefficientTest <- .maPrintCoefficientTest(tempEstimatedMarginalMeans[j,], estimatedMarginalMeansTestsStaistics)

      additionalInformation[[tempRow]] <- data.frame(
        "label"  = if (estimatedMarginalMeansCoefficientTestsLeft) paste0(tempEstimatedMarginalMeans$value[j], ": ", tempCoefficientTest) else tempEstimatedMarginalMeans$value[j],
        "y"      = tempRow,
        "est"    = tempEstimatedMarginalMeans$est[j],
        "lCi"    = tempEstimatedMarginalMeans$lCi[j],
        "uCi"    = tempEstimatedMarginalMeans$uCi[j],
        "test"   = if (estimatedMarginalMeansCoefficientTestsRight) tempCoefficientTest else "",
        "face"   = "italic"
      )
      additionalObjects[[tempRow]] <- with(tempEstimatedMarginalMeans[j,], .maMakeDiamondDataFrame(est = est, lCi = lCi, uCi = uCi, row = tempRow, id = tempRow))
      additionalObjects[[tempRow]]$mapColor <- if(options[["forestPlotMappingColor"]] == estimatedMarginalMeansVariables[i]) tempEstimatedMarginalMeans$value[j] else NA
      tempRow <- tempRow + 1


      if (options[["forestPlotPredictionIntervals"]] || estimatedMarginalMeansCoefficientTestsBelow) {

        additionalInformation[[tempRow]] <- data.frame(
          "label"  = if (estimatedMarginalMeansCoefficientTestsBelow) tempCoefficientTest else NA,
          "y"      = tempRow,
          "est"    = NA,
          "lCi"    = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$lPi[j] else NA,
          "uCi"    = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$uPi[j] else NA,
          "test"   = "",
          "face"   = NA
        )
        if (options[["forestPlotPredictionIntervals"]]) {
          additionalObjects[[tempRow]] <- with(tempEstimatedMarginalMeans[j,], .maMakeRectangleDataFrame(lCi = lPi, uCi = uPi, row = tempRow, id = tempRow))
          additionalObjects[[tempRow]]$mapColor <- if(options[["forestPlotMappingColor"]] == estimatedMarginalMeansVariables[i]) tempEstimatedMarginalMeans$value[j] else NA
        }


        tempRow <- tempRow + 1
      }
    }

    # add empty row
    tempRow <- tempRow + 1
  }

  # add adjusted effect size estimate
  if (options[["forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"]]) {

    tempEstimatedMarginalMeans <- .maComputeMarginalMeansVariable(fit, options, dataset, "", options[["forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"]] , "effectSize")
    tempCoefficientTest <- .maPrintCoefficientTest(tempEstimatedMarginalMeans, options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue")

    additionalInformation[[tempRow]] <- data.frame(
      "label" = if (estimatedMarginalMeansCoefficientTestsLeft) paste0(gettext("Adjusted Estimate"), ": ", tempCoefficientTest) else gettext("Adjusted Estimate"),
      "y"     = tempRow,
      "est"   = tempEstimatedMarginalMeans$est,
      "lCi"   = tempEstimatedMarginalMeans$lCi,
      "uCi"   = tempEstimatedMarginalMeans$uCi,
      "test"  = if (estimatedMarginalMeansCoefficientTestsRight) tempCoefficientTest else "",
      "face"  = NA
    )
    additionalObjects[[tempRow]] <- with(tempEstimatedMarginalMeans, .maMakeDiamondDataFrame(est = est, lCi = lCi, uCi = uCi, row = tempRow, id = tempRow))
    tempRow <- tempRow + 1

    if (options[["forestPlotPredictionIntervals"]] || estimatedMarginalMeansCoefficientTestsBelow) {

      additionalInformation[[tempRow]] <- data.frame(
        "label" = if(estimatedMarginalMeansCoefficientTestsBelow) tempCoefficientTest else NA,
        "y"     = tempRow,
        "est"   = NA,
        "lCi"   = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$lPi else NA,
        "uCi"   = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$uPi else NA,
        "test"  = "",
        "face"  = NA
      )

      if (options[["forestPlotPredictionIntervals"]])
        additionalObjects[[tempRow]] <- with(tempEstimatedMarginalMeans, .maMakeRectangleDataFrame(lCi = lPi, uCi = uPi, row = tempRow, id = tempRow))

      tempRow <- tempRow + 1
    }
  }

  # return
  return(list(
    information = do.call(rbind, additionalInformation),
    objects     = do.call(rbind,additionalObjects[!sapply(additionalObjects, is.null)])
  ))
}
.forestPlotBuildModelInformation       <- function(fit, options){

  ### builds the model information section
  # returns a
  # - data frame with the model information information
  # - a list with the model information prediction intervals

  # return null on error
  if (is.null(fit) || jaspBase::isTryError(fit))
    return(NULL)

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

  # initiate the local objects
  tempRow <- 1
  additionalInformation <- list()
  additionalObjects     <- list()

  # add Header
  additionalInformation[[tempRow]] <- data.frame(
    "label"  = gettextf(
      "Model Fit%1$s",
      if (options[["subgroup"]] != "") gettextf(" (Subgroup: %1$s)", attr(fit, "subgroup")) else ""
    ),
    "y"     = tempRow,
    "est"   = NA,
    "lCi"   = NA,
    "uCi"   = NA,
    "test"  = "",
    "face"  = "bold"
  )
  tempRow <- tempRow + 1

  if (options[["forestPlotResidualHeterogeneityTest"]]) {
    additionalInformation[[tempRow]] <- data.frame(
      "label" = .maPrintQTest(fit),
      "y"     = tempRow,
      "est"   = NA,
      "lCi"   = NA,
      "uCi"   = NA,
      "test"  = "",
      "face"  = NA
    )
    tempRow <- tempRow + 1
  }

  if (!.maGetMethodOptions(options) %in% c("FE", "EE") && options[["forestPlotHeterogeneityEstimateTau"]]) {
    additionalInformation[[tempRow]] <- data.frame(
      "label" = .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau"),
      "y"     = tempRow,
      "est"   = NA,
      "lCi"   = NA,
      "uCi"   = NA,
      "test"  = "",
      "face"  = NA
    )
    tempRow <- tempRow + 1
  }

  if (!.maGetMethodOptions(options) %in% c("FE", "EE") && options[["forestPlotHeterogeneityEstimateTau2"]]) {
    additionalInformation[[tempRow]] <- data.frame(
      "label" = .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "tau2"),
      "y"     = tempRow,
      "est"   = NA,
      "lCi"   = NA,
      "uCi"   = NA,
      "test"  = "",
      "face"  = NA
    )
    tempRow <- tempRow + 1
  }

  if (!.maGetMethodOptions(options) %in% c("FE", "EE") && !.maIsMetaregressionHeterogeneity(options) && options[["forestPlotHeterogeneityEstimateI2"]]) {
    additionalInformation[[tempRow]] <- data.frame(
      "label" = .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "I2"),
      "y"     = tempRow,
      "est"   = NA,
      "lCi"   = NA,
      "uCi"   = NA,
      "test"  = "",
      "face"  = NA
    )
    tempRow <- tempRow + 1
  }

  if (!.maGetMethodOptions(options) %in% c("FE", "EE") && !.maIsMetaregressionHeterogeneity(options) && options[["forestPlotHeterogeneityEstimateH2"]]) {
    additionalInformation[[tempRow]] <- data.frame(
      "label" = .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], parameter = "H2"),
      "y"     = tempRow,
      "est"   = NA,
      "lCi"   = NA,
      "uCi"   = NA,
      "test"  = "",
      "face"  = NA
    )
    tempRow <- tempRow + 1
  }

  if (.maIsMetaregressionEffectSize(options) && options[["forestPlotEffectSizeModerationTest"]]) {
    additionalInformation[[tempRow]] <- data.frame(
      "label" = .maPrintModerationTest(fit, options, par = "effectSize"),
      "y"     = tempRow,
      "est"   = NA,
      "lCi"   = NA,
      "uCi"   = NA,
      "test"  = "",
      "face"  = NA
    )
    tempRow <- tempRow + 1
  }

  if (.maIsMetaregressionHeterogeneity(options) && options[["forestPlotHeterogeneityModerationTest"]]) {
    additionalInformation[[tempRow]] <- data.frame(
      "label" = .maPrintModerationTest(fit, options, par = "heterogeneity"),
      "y"     = tempRow,
      "est"   = NA,
      "lCi"   = NA,
      "uCi"   = NA,
      "test"  = "",
      "face"  = NA
    )
    tempRow <- tempRow + 1
  }

  if (options[["forestPlotEffectSizeFixedEffectEstimate"]]) {

    pooledEffectSizeTestsRight <- options[["forestPlotEffectSizeFixedEffectTest"]] && options[["forestPlotTestsInRightPanel"]]
    pooledEffectSizeTestsBelow <- options[["forestPlotEffectSizeFixedEffectTest"]] && !options[["forestPlotTestsInRightPanel"]] && options[["forestPlotPredictionIntervals"]]
    pooledEffectSizeTestsLeft  <- options[["forestPlotEffectSizeFixedEffectTest"]] && !options[["forestPlotTestsInRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

    tempPooledEstimate <- .maComputePooledEffectPlot(fit, options, forceFixed = TRUE)
    tempTestText       <- .maPrintCoefficientTest(tempPooledEstimate, options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue")

    additionalInformation[[tempRow]] <- data.frame(
      "label" = if (pooledEffectSizeTestsLeft) paste0(gettext("Fixed Effect Estimate"), ": ", tempTestText) else gettext("Fixed Effect Estimate"),
      "y"     = tempRow,
      "est"   = tempPooledEstimate$est,
      "lCi"   = tempPooledEstimate$lCi,
      "uCi"   = tempPooledEstimate$uCi,
      "test"  = if (pooledEffectSizeTestsRight) tempTestText else "",
      "face"  = NA
    )
    additionalObjects[[tempRow]] <- with(tempPooledEstimate, .maMakeDiamondDataFrame(est = est, lCi = lCi, uCi = uCi, row = tempRow, id = tempRow))
    tempRow <- tempRow + 1

    if (pooledEffectSizeTestsBelow) {
      additionalInformation[[tempRow]] <- data.frame(
        "label" = if (pooledEffectSizeTestsBelow) tempTestText else NA,
        "y"     = tempRow,
        "est"   = NA,
        "lCi"   = NA,
        "uCi"   = NA,
        "test"  = "",
        "face"  = NA
      )

      tempRow <- tempRow + 1
    }
  }

  if (options[["forestPlotEffectSizePooledEstimate"]]) {

    pooledEffectSizeTestsRight <- options[["forestPlotEffectSizePooledEstimateTest"]] && options[["forestPlotTestsInRightPanel"]]
    pooledEffectSizeTestsBelow <- options[["forestPlotEffectSizePooledEstimateTest"]] && !options[["forestPlotTestsInRightPanel"]] && options[["forestPlotPredictionIntervals"]]
    pooledEffectSizeTestsLeft  <- options[["forestPlotEffectSizePooledEstimateTest"]] && !options[["forestPlotTestsInRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

    tempPooledEstimate <- .maComputePooledEffectPlot(fit, options)
    tempTestText      <- .maPrintCoefficientTest(tempPooledEstimate, options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue")

    additionalInformation[[tempRow]] <- data.frame(
      "label" = if (pooledEffectSizeTestsLeft) paste0(gettext("Pooled Estimate"), ": ", tempTestText) else gettext("Pooled Estimate"),
      "y"     = tempRow,
      "est"   = tempPooledEstimate$est,
      "lCi"   = tempPooledEstimate$lCi,
      "uCi"   = tempPooledEstimate$uCi,
      "test"  = if (pooledEffectSizeTestsRight) tempTestText else "",
      "face"  = NA
    )
    additionalObjects[[tempRow]] <- with(tempPooledEstimate, .maMakeDiamondDataFrame(est = est, lCi = lCi, uCi = uCi, row = tempRow, id = tempRow))
    tempRow <- tempRow + 1

    if (pooledEffectSizeTestsBelow || options[["forestPlotPredictionIntervals"]]) {
      additionalInformation[[tempRow]] <- data.frame(
        "label" = if (pooledEffectSizeTestsBelow) tempTestText else NA,
        "y"     = tempRow,
        "est"   = NA,
        "lCi"   = if (options[["forestPlotPredictionIntervals"]]) tempPooledEstimate$lPi else NA,
        "uCi"   = if (options[["forestPlotPredictionIntervals"]]) tempPooledEstimate$uPi else NA,
        "test"  = "",
        "face"  = NA
      )

      if (options[["forestPlotPredictionIntervals"]])
        additionalObjects[[tempRow]] <- with(tempPooledEstimate, .maMakeRectangleDataFrame(lCi = lPi, uCi = uPi, row = tempRow, id = tempRow))

      tempRow <- tempRow + 1
    }
  }

  # return
  return(list(
    information = do.call(rbind, additionalInformation),
    objects     = do.call(rbind, additionalObjects[!sapply(additionalObjects, is.null)])
  ))
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
.forestPlotHasrightPanel               <- function(options, additionalInformation) {

  if (!options[["forestPlotStudyInformation"]] && length(additionalInformation) == 0)
    return(FALSE)
  if (options[["forestPlotEstimatesAndConfidenceIntervals"]])
    return(TRUE)
  if (options[["forestPlotStudyInformation"]] && options[["forestPlotStudyInformationStudyWeights"]])
    return(TRUE)
  if (length(additionalInformation) != 0 &&
      (options[["forestPlotEstimatedMarginalMeansTermTests"]] || options[["forestPlotEstimatedMarginalMeansCoefficientTests"]]) &&
      options[["forestPlotTestsInRightPanel"]])
    return(TRUE)
  else
    return(FALSE)
}
