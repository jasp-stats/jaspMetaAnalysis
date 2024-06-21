if(FALSE){

  # extract common options
  relativeRowSize <- options[["forestPlotRelativeSizeRow"]]

  # keep track of added rows across marginal means and model estimates:
  tempRow <- 1
  additionalInformation <- list()
  additionalObjects     <- list()

  ### Study information panel ----
  if (options[["forestPlotStudyInformation"]]) {

    ### extract effect sizes and variances from the fitted object
    dfForrest <- data.frame(
      effectSize     = fit[["yi"]],
      standardError  = sqrt(fit[["vi"]]),
      weights        = weights(fit),
      id             = seq_along(fit[["yi"]])
    )

    # add CI using normal approximation
    dfForrest$lCi <- dfForrest$effectSize - 1.96 * dfForrest$standardError
    dfForrest$uCi <- dfForrest$effectSize + 1.96 * dfForrest$standardError

    # transform effect size when requested
    if (options[["transformEffectSize"]] != "none")
      dfForrest[,c("effectSize", "lCi", "uCi")] <- do.call(
        .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
        list(dfForrest[,c("effectSize", "lCi", "uCi")]))

    xRangeStudyInformationPanel <- range(c(dfForrest$lCi, dfForrest$uCi))

    # add y-axis coordinates for plotting
    dfForrest$row <- seq(nrow(dfForrest))

    # add variables used for either color, shape, order or Left panel information
    additionalVariables <- c(
      if (length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) unlist(options[["forestPlotStudyInformationSelectedVariables"]]),
      if (options[["forestPlotMappingColor"]] != "") options[["forestPlotMappingColor"]],
      if (options[["forestPlotMappingShape"]] != "") options[["forestPlotMappingShape"]]
    )
    if (length(additionalVariables) > 0)
      dfForrest <- cbind(dfForrest, dataset[,additionalVariables,drop=FALSE])

    # combine left panel information
    leftPanelStudyInformation <- do.call(rbind.data.frame, options[["forestPlotStudyInformationSelectedVariablesSettings"]])

    ### add predicted effects
    if (options[["forestPlotStudyInformationPredictedEffects"]]) {

      fitPrediction <- data.frame(predict(fit))

      # replicate the prediction for each estimate if the predictions are the same (no moderators)
      if (nrow(fitPrediction) == 1)
        fitPrediction <- do.call(rbind, replicate(nrow(dfForrest), fitPrediction, simplify = FALSE))

      fitPrediction$id   <- dfForrest$id
      fitPrediction$row  <- dfForrest$row

      # create prediction diamond coordinates for each estimate
      fitPrediction <- do.call(rbind, lapply(1:nrow(fitPrediction), function(i) {
        with(fitPrediction[i,], .maMakeDiamondDataFrame(est = pred,lCi = pi.lb, uCi = pi.ub, row = row, id = id))
      }))

      fitPrediction <- merge(fitPrediction, dfForrest[,!colnames(dfForrest) %in% c("effectSize", "standardError", "weights", "lCi", "uCi")], by = "id")

      # transform effect size when requested
      if (options[["transformEffectSize"]] != "none")
        fitPrediction[,"xPrediction"] <- do.call(
          .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
          list(fitPrediction[,"xPrediction"]))

      xRangeStudyInformationPanel <- range(c(xRangeStudyInformationPanel, range(fitPrediction$x)))

      # adjust y-coordinates
      fitPrediction$y <- fitPrediction$y * relativeRowSize
    }

    # adjust y-coordinates
    dfForrest$y <- dfForrest$row * relativeRowSize

  } else {
    xRangeStudyInformationPanel <- NA
  }


  ### Estimated marginal means panel ----

  ### compute and add marginal estimates
  if (options[["forestPlotEstimatedMarginalMeans"]]) {

    # terms and levels information
    estimatedMarginalMeansTestsStaistics   <- options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue"
    estimatedMarginalMeansVariables        <- unlist(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]])

    # statistics position adjustment
    estimatedMarginalMeansTermsTestsRight  <- options[["forestPlotEstimatedMarginalMeansTermTests"]] && options[["forestPlotRightPanel"]]
    estimatedMarginalMeansTermsTestsLeft   <- options[["forestPlotEstimatedMarginalMeansTermTests"]] && !options[["forestPlotRightPanel"]]

    estimatedMarginalMeansCoefficientTestsRight <- options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] && options[["forestPlotRightPanel"]]
    estimatedMarginalMeansCoefficientTestsBelow <- options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] && !options[["forestPlotRightPanel"]] && options[["forestPlotPredictionIntervals"]]
    estimatedMarginalMeansCoefficientTestsLeft  <- options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] && !options[["forestPlotRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

    # add header
    if (length(estimatedMarginalMeansVariables) > 0) {

      additionalInformation[[tempRow]] <- data.frame(
        "label"  = gettext("Estimated Marginal Means"),
        "row"    = tempRow,
        "est"    = NA,
        "lCi"    = NA,
        "uCi"    = NA,
        "test"   = "",
        "print"  = "bold"
      )
      tempRow <- tempRow + 1
    }

    # add marginal estimates
    for (i in seq_along(estimatedMarginalMeansVariables)) {

      tempTermTest               <- .maTermTests(fit, options, estimatedMarginalMeansVariables[i])
      tempEstimatedMarginalMeans <- .maComputeMarginalMeansVariable(fit, options, dataset, estimatedMarginalMeansVariables[i], options[["forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"]] , "effectSize")
      tempTestText               <- .maPrintTermTest(tempTermTest, estimatedMarginalMeansTestsStaistics)

      # add term information
      additionalInformation[[tempRow]] <- data.frame(
        "label"  = if (estimatedMarginalMeansTermsTestsLeft) paste0(estimatedMarginalMeansVariables[i], ": ", tempTestText) else estimatedMarginalMeansVariables[i],
        "row"    = tempRow,
        "est"    = NA,
        "lCi"    = NA,
        "uCi"    = NA,
        "test"   = if (estimatedMarginalMeansTermsTestsRight) tempTestText else "",
        "print"  = NA
      )
      tempRow <- tempRow + 1

      # add levels information
      for (j in 1:nrow(tempEstimatedMarginalMeans)) {

        tempCoefficientTest <- .maPrintCoefficientTest(tempEstimatedMarginalMeans[j,], estimatedMarginalMeansTestsStaistics)

        additionalInformation[[tempRow]] <- data.frame(
          "label"  = if (estimatedMarginalMeansCoefficientTestsLeft) paste0(tempEstimatedMarginalMeans$value[j], ": ", tempCoefficientTest) else tempEstimatedMarginalMeans$value[j],
          "row"    = tempRow,
          "est"    = tempEstimatedMarginalMeans$est[j],
          "lCi"    = tempEstimatedMarginalMeans$lCi[j],
          "uCi"    = tempEstimatedMarginalMeans$uCi[j],
          "test"   = if (estimatedMarginalMeansCoefficientTestsRight) tempCoefficientTest else "",
          "print"  = "italic"
        )
        additionalObjects[[tempRow]] <- with(tempEstimatedMarginalMeans[j,], .maMakeDiamondDataFrame(est = est, lCi = lCi, uCi = uCi, row = tempRow, id = tempRow))
        additionalObjects[[tempRow]]$mapColor <- if(options[["forestPlotMappingColor"]] == estimatedMarginalMeansVariables[i]) tempEstimatedMarginalMeans$value[j] else NA
        tempRow <- tempRow + 1


        if (options[["forestPlotPredictionIntervals"]] || estimatedMarginalMeansCoefficientTestsBelow) {

          additionalInformation[[tempRow]] <- data.frame(
            "label"  = if (estimatedMarginalMeansCoefficientTestsBelow) tempCoefficientTest else "",
            "row"    = tempRow,
            "est"    = NA,
            "lCi"    = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$lPi[j] else NA,
            "uCi"    = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$uPi[j] else NA,
            "test"   = "",
            "print"  = NA
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
        "row"   = tempRow,
        "est"   = tempEstimatedMarginalMeans$est,
        "lCi"   = tempEstimatedMarginalMeans$lCi,
        "uCi"   = tempEstimatedMarginalMeans$uCi,
        "test"  = if (estimatedMarginalMeansCoefficientTestsRight) tempCoefficientTest else "",
        "print" = NA
      )
      additionalObjects[[tempRow]] <- with(tempEstimatedMarginalMeans, .maMakeDiamondDataFrame(est = est, lCi = lCi, uCi = uCi, row = tempRow, id = tempRow))
      tempRow <- tempRow + 1

      if (options[["forestPlotPredictionIntervals"]] || estimatedMarginalMeansCoefficientTestsBelow) {

        additionalInformation[[tempRow]] <- data.frame(
          "label" = if(estimatedMarginalMeansCoefficientTestsBelow) tempCoefficientTest else "",
          "row"   = tempRow,
          "est"   = NA,
          "lCi"   = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$lPi else NA,
          "uCi"   = if (options[["forestPlotPredictionIntervals"]]) tempEstimatedMarginalMeans$uPi else NA,
          "test"  = "",
          "print" = NA
        )

        if (options[["forestPlotPredictionIntervals"]])
          additionalObjects[[tempRow]] <- with(tempEstimatedMarginalMeans, .maMakeRectangleDataFrame(lCi = lPi, uCi = uPi, row = tempRow, id = tempRow))

        tempRow <- tempRow + 1
      }
    }
  }


  ### Model information panel ----
  # - residual heterogeneity test
  # - moderation tests
  # - pooled estimate
  if (options[["forestPlotModelInformation"]]) {

    if (any(unlist(options[c(
      "forestPlotResidualHeterogeneityTest", "forestPlotResidualHeterogeneityEstimate",
      "forestPlotEffectSizeModerationTest",
      "forestPlotHeterogeneityModerationTest",
      "forestPlotPooledEffectSizeEstimate"
    )]))) {
      # add Header
      additionalInformation[[tempRow]] <- data.frame(
        "label" = gettext("Model Information"),
        "row"   = tempRow,
        "est"   = NA,
        "lCi"   = NA,
        "uCi"   = NA,
        "test"  = "",
        "print" = "bold"
      )
      tempRow <- tempRow + 1
    }

    if (options[["forestPlotResidualHeterogeneityTest"]]) {
      additionalInformation[[tempRow]] <- data.frame(
        "label" = .maPrintQTest(fit),
        "row"   = tempRow,
        "est"   = NA,
        "lCi"   = NA,
        "uCi"   = NA,
        "test"  = "",
        "print" = NA
      )
      tempRow <- tempRow + 1
    }

    if (!.maGetMethodOptions(options) %in% c("FE", "EE") && options[["forestPlotResidualHeterogeneityEstimate"]]) {
      additionalInformation[[tempRow]] <- data.frame(
        "label" = .maPrintHeterogeneityEstimate(fit, options, digits = options[["forestPlotAuxiliaryDigits"]], keepText = !options[["forestPlotResidualHeterogeneityTest"]]),
        "row"   = tempRow,
        "est"   = NA,
        "lCi"   = NA,
        "uCi"   = NA,
        "test"  = "",
        "print" = NA
      )
      tempRow <- tempRow + 1
    }

    if (.maIsMetaregressionEffectSize(options) && options[["forestPlotEffectSizeModerationTest"]]) {
      additionalInformation[[tempRow]] <- data.frame(
        "label" = .maPrintModerationTest(fit, options, par = "effectSize"),
        "row"   = tempRow,
        "est"   = NA,
        "lCi"   = NA,
        "uCi"   = NA,
        "test"  = "",
        "print" = NA
      )
      tempRow <- tempRow + 1
    }

    if (.maIsMetaregressionHeterogeneity(options) && options[["forestPlotHeterogeneityModerationTest"]]) {
      additionalInformation[[tempRow]] <- data.frame(
        "label" = .maPrintModerationTest(fit, options, par = "heterogeneity"),
        "row"   = tempRow,
        "est"   = NA,
        "lCi"   = NA,
        "uCi"   = NA,
        "test"  = "",
        "print" = NA
      )
      tempRow <- tempRow + 1
    }

    if (options[["forestPlotPooledEffectSizeEstimate"]]) {

      pooledEffectSizeTestsRight <- options[["forestPlotPooledEffectSizeTest"]] && options[["forestPlotRightPanel"]]
      pooledEffectSizeTestsBelow <- options[["forestPlotPooledEffectSizeTest"]] && !options[["forestPlotRightPanel"]] && options[["forestPlotPredictionIntervals"]]
      pooledEffectSizeTestsLeft  <- options[["forestPlotPooledEffectSizeTest"]] && !options[["forestPlotRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

      tempPooledEstimate <- .maComputePooledEffectPlot(fit, options)
      tempTestText <- .maPrintCoefficientTest(tempPooledEstimate, options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue")

      additionalInformation[[tempRow]] <- data.frame(
        "label" = if (pooledEffectSizeTestsLeft) paste0(gettext("Pooled Estimate"), ": ", tempTestText) else gettext("Pooled Estimate"),
        "row"   = tempRow,
        "est"   = tempPooledEstimate$est,
        "lCi"   = tempPooledEstimate$lCi,
        "uCi"   = tempPooledEstimate$uCi,
        "test"  = if (pooledEffectSizeTestsRight) tempTestText else "",
        "print" = NA
      )
      additionalObjects[[tempRow]] <- with(tempEstimatedMarginalMeans, .maMakeDiamondDataFrame(est = est, lCi = lCi, uCi = uCi, row = tempRow, id = tempRow))
      tempRow <- tempRow + 1

      if (pooledEffectSizeTestsBelow || options[["forestPlotPredictionIntervals"]]) {
        additionalInformation[[tempRow]] <- data.frame(
          "label" = if (pooledEffectSizeTestsBelow) tempTestText else "",
          "row"   = tempRow,
          "est"   = NA,
          "lCi"   = if (options[["forestPlotPredictionIntervals"]]) tempPooledEstimate$lPi else NA,
          "uCi"   = if (options[["forestPlotPredictionIntervals"]]) tempPooledEstimate$uPi else NA,
          "test"  = "",
          "print" = NA
        )

        if (options[["forestPlotPredictionIntervals"]])
          additionalObjects[[tempRow]] <- with(tempEstimatedMarginalMeans, .maMakeRectangleDataFrame(lCi = lPi, uCi = uPi, row = tempRow, id = tempRow))

        tempRow <- tempRow + 1
      }
    }


  }


  ### Merge results from estimated marginal means and information panel ----
  if (length(additionalInformation) > 0) {

    # merge additional information
    additionalInformation <- do.call(rbind, additionalInformation)
    additionalObjects     <- do.call(rbind, additionalObjects[!sapply(additionalObjects, is.null)])

    # adjust y-coordinates
    additionalInformation$y <- -additionalInformation$row * relativeRowSize
    additionalObjects$y     <- -additionalObjects$y * relativeRowSize

    xRangeAddedPanels <- range(c(additionalInformation$lCi, additionalInformation$uCi, additionalObjects$x), na.rm = TRUE)
  } else {
    xRangeAddedPanels <- NA
  }

  # specify x-axis limits
  if (options[["forestPlotAuxiliarySetXAxisLimit"]]) {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(options[["forestPlotAuxiliarySetXAxisLimitLower"]], options[["forestPlotAuxiliarySetXAxisLimitUpper"]]))
  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(xRangeStudyInformationPanel, xRangeAddedPanels), na.rm = TRUE))
  }
  xRange <- range(xBreaks)

  # specify y-axis limits
  if (options[["forestPlotStudyInformation"]]) {
    yRange <- c(0, max(dfForrest$y) + relativeRowSize + any(leftPanelStudyInformation$title != ""))
  } else {
    yRange <- c(0, 0)
  }
  if (length(additionalInformation) > 0) {
    yRange[1] <- min(additionalInformation$y) - relativeRowSize
  }
  if (length(additionalInformation) > 0) {
    yRange[1] <- min(c(additionalObjects$y, yRange))
  }

  ### Make the forest plot ----
  plotForrest <- ggplot2::ggplot()

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
        data    = fitPrediction,
        mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
        fill    = if (options[["forestPlotMappingColor"]] == "") "grey20",
        alpha   = 0.8
      )
      plotForrest <- plotForrest + do.call(ggplot2::geom_polygon, geomCall[!sapply(geomCall, is.null)])
    }

    ### add estimates
    # dispatch the aes call based on mapping:
    aesCall <- list(
      x     = as.name("effectSize"),
      y     = as.name("y"),
      color = if (options[["forestPlotMappingColor"]] != "") as.name(options[["forestPlotMappingColor"]]),
      shape = if (options[["forestPlotMappingShape"]] != "") as.name(options[["forestPlotMappingShape"]])
    )
    geomCall <- list(
      data    = dfForrest,
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
      color   = if (options[["forestPlotMappingColor"]] == "") options[["forestPlotAuxiliaryPlotColor"]],
      shape   = if (options[["forestPlotMappingShape"]] == "") 15,
      size    = dfForrest[["weights"]] * options[["forestPlotRelativeSizeEstimates"]]
    )
    plotForrest <- plotForrest + do.call(ggplot2::geom_point, geomCall[!sapply(geomCall, is.null)])

    # change scale for shapes to full shapes if used
    if (options[["forestPlotMappingShape"]] != "")
      plotForrest <- plotForrest + ggplot2::scale_shape_manual(values = rep(c(15:18, 21:25), length.out = length(unique(dfForrest[[options[["forestPlotMappingShape"]]]]))))


    ### add CIs
    plotForrest <- plotForrest + ggplot2::geom_errorbarh(
      data    = dfForrest,
      mapping = ggplot2::aes(
        xmin = lCi,
        xmax = uCi,
        y    = y
      ),
      height = 0
    )

  }

  # add additional information
  if (length(additionalInformation) > 0) {

    # dispatch the aes call based on color mapping
    if (any(!is.na(additionalObjects$mapColor))) {
      plotForrest <- plotForrest + ggplot2::geom_polygon(
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
      plotForrest <- plotForrest + ggplot2::geom_polygon(
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
    plotForrest <- plotForrest + ggplot2::geom_vline(xintercept = options[["forestPlotAuxiliaryAddVerticalLineValue"]], linetype = "dashed")

  # fix plotting range
  plotForrest <- plotForrest + jaspGraphs::scale_x_continuous(
    name   = options[["forestPlotAuxiliaryEffectLabel"]],
    breaks = xBreaks,
    limits = xRange
  ) + ggplot2::ylim(yRange)

  # plotForrest <- plotForrest + jaspGraphs::geom_rangeframe(sides = "b") + jaspGraphs::themeJaspRaw()

  ### Make the left information panel ----
  if (length(options[["forestPlotStudyInformationSelectedVariablesSettings"]]) > 0 || length(additionalInformation) > 0) {

    # determine number of columns and study information
    leftPanelStudyInformation   <- do.call(rbind.data.frame, options[["forestPlotStudyInformationSelectedVariablesSettings"]])

    columnsAdd <- max(c(
      if (length(additionalInformation) > 0)   1,
      if (!is.null(leftPanelStudyInformation)) sum(leftPanelStudyInformation$width)
    ))

    xRangeLeftPanel <- c(0, columnsAdd)


    plotLeft <- ggplot2::ggplot() + ggplot2::ylim(yRange) + ggplot2::xlim(xRangeLeftPanel)

    if (!is.null(leftPanelStudyInformation)) {

      # compute study information coordinates
      leftPanelStudyInformation$y         <- (max(dfForrest$row) + 1) * relativeRowSize
      leftPanelStudyInformation$xStart    <- c(0, cumsum(leftPanelStudyInformation$width[-length(leftPanelStudyInformation$width)]))
      leftPanelStudyInformation$xEnd      <- cumsum(leftPanelStudyInformation$width)
      leftPanelStudyInformation$x         <- ifelse(
        leftPanelStudyInformation$alignment == "left", leftPanelStudyInformation$xStart, ifelse(
          leftPanelStudyInformation$alignment == "middle", (leftPanelStudyInformation$xStart + leftPanelStudyInformation$xEnd) / 2, leftPanelStudyInformation$xEnd
      ))

      # add titles
      plotLeft <- plotLeft + ggplot2::geom_text(
        data    = leftPanelStudyInformation,
        mapping = ggplot2::aes(
          x     = 0,
          y     = y,
          label = title,
          hjust = alignment
        ),
        vjust    = "midle",
        fontface = "bold"
      )

      # add information
      if (any(leftPanelStudyInformation$value == options[["forestPlotMappingColor"]])) {
        leftPanelStudyDataColored <- data.frame(
          x         = leftPanelStudyInformation$x[leftPanelStudyInformation$value == options[["forestPlotMappingColor"]]],
          y         = dfForrest$y,
          label     = dfForrest[[options[["forestPlotMappingColor"]]]],
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
          vjust    = "midle",
        )
      }
      if (any(leftPanelStudyInformation$value != options[["forestPlotMappingColor"]])) {
        tempVariables <- leftPanelStudyInformation$value[leftPanelStudyInformation$value != options[["forestPlotMappingColor"]]]
        leftPanelStudyData <- do.call(rbind.data.frame, lapply(tempVariables, function(variable) {
          data.frame(
            x         = leftPanelStudyInformation$x[leftPanelStudyInformation$value == tempVariables],
            y         = dfForrest$y,
            label     = dfForrest[[tempVariables]],
            alignment = leftPanelStudyInformation$alignment[leftPanelStudyInformation$value == tempVariables]
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
          vjust    = "midle",
        )
      }

    }


  }





}
