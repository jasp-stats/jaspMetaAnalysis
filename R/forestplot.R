.maMakeTheUltimateForestPlot <- function(fit, dataset, options) {

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
    dfForrest$lCi <- dfForrest$effectSize - qnorm((1 - options[["confidenceIntervalsLevel"]]) / 2, lower.tail = F) * dfForrest$standardError
    dfForrest$uCi <- dfForrest$effectSize + qnorm((1 - options[["confidenceIntervalsLevel"]]) / 2, lower.tail = F) * dfForrest$standardError

    if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
      dfForrest$lCi2 <- dfForrest$effectSize - qnorm((1 - options[["forestPlotStudyInformationSecondaryConfidenceIntervalLevel"]]) / 2, lower.tail = F) * dfForrest$standardError
      dfForrest$uCi2 <- dfForrest$effectSize + qnorm((1 - options[["forestPlotStudyInformationSecondaryConfidenceIntervalLevel"]]) / 2, lower.tail = F) * dfForrest$standardError
    }


    # transform effect size when requested
    if (options[["transformEffectSize"]] != "none") {

      dfForrest[,c(
        "effectSize", "lCi", "uCi",
        if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) c("lCi2", "uCi2"))] <- do.call(
          .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
          list(dfForrest[,c(
              "effectSize", "lCi", "uCi",
              if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) c("lCi2", "uCi2"))]))
    }

    xRangeStudyInformationPanel <- range(c(dfForrest$lCi, dfForrest$uCi))

    # add variables used for either color, shape, order or Left panel information
    additionalVariables <- c(
      if (length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) unlist(options[["forestPlotStudyInformationSelectedVariables"]]),
      if (options[["forestPlotStudyInformationOrderBy"]] != "") options[["forestPlotStudyInformationOrderBy"]],
      if (options[["forestPlotMappingColor"]] != "")            options[["forestPlotMappingColor"]],
      if (options[["forestPlotMappingShape"]] != "")            options[["forestPlotMappingShape"]]
    )
    if (length(additionalVariables) > 0)
      dfForrest <- cbind(dfForrest, dataset[,additionalVariables,drop=FALSE])

    # TODO: temporal fix for the variable names in the Component list not being properly translated
    for (i in seq_along(options[["forestPlotStudyInformationSelectedVariables"]])) {
      options[["forestPlotStudyInformationSelectedVariablesSettings"]][[i]][["value"]] <- options[["forestPlotStudyInformationSelectedVariables"]][[i]]
    }

    # combine left panel information
    leftPanelStudyInformation <- do.call(rbind.data.frame, options[["forestPlotStudyInformationSelectedVariablesSettings"]])

    # re-order
    if (options[["forestPlotStudyInformationOrderBy"]] != "") {
      dfForrest <- dfForrest[order(
        dfForrest[,options[["forestPlotStudyInformationOrderBy"]]],
        decreasing = options[["forestPlotStudyInformationOrderAscending"]]),]
    }

    # add y-axis coordinates for plotting
    dfForrest$row <- seq(nrow(dfForrest))

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
        with(fitPrediction[i,], .maMakeDiamondDataFrame(est = pred, lCi = pi.lb, uCi = pi.ub, row = row, id = id))
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
    dfForrest <- NULL
    xRangeStudyInformationPanel <- NA
  }


  ### Estimated marginal means panel ----

  ### compute and add marginal estimates
  if (options[["forestPlotEstimatedMarginalMeans"]] && (
    length(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]]) > 0 || options[["forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"]]
  )) {

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
      "label"  = gettext("Estimated Marginal Means"),
      "row"    = tempRow,
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
        "row"    = tempRow,
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
          "row"    = tempRow,
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
            "row"    = tempRow,
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
        "row"   = tempRow,
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
          "row"   = tempRow,
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

    tempRow <- tempRow + 1
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
        "face"  = "bold"
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
        "face"  = NA
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
        "face"  = NA
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
        "face"  = NA
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
        "face"  = NA
      )
      tempRow <- tempRow + 1
    }

    if (options[["forestPlotPooledEffectSizeEstimate"]]) {

      pooledEffectSizeTestsRight <- options[["forestPlotPooledEffectSizeTest"]] && options[["forestPlotTestsInRightPanel"]]
      pooledEffectSizeTestsBelow <- options[["forestPlotPooledEffectSizeTest"]] && !options[["forestPlotTestsInRightPanel"]] && options[["forestPlotPredictionIntervals"]]
      pooledEffectSizeTestsLeft  <- options[["forestPlotPooledEffectSizeTest"]] && !options[["forestPlotTestsInRightPanel"]] && !options[["forestPlotPredictionIntervals"]]

      tempPooledEstimate <- .maComputePooledEffectPlot(fit, options)
      tempTestText <- .maPrintCoefficientTest(tempPooledEstimate, options[["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue")

      additionalInformation[[tempRow]] <- data.frame(
        "label" = if (pooledEffectSizeTestsLeft) paste0(gettext("Pooled Estimate"), ": ", tempTestText) else gettext("Pooled Estimate"),
        "row"   = tempRow,
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
          "row"   = tempRow,
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
        data    = fitPrediction,
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
      data    = dfForrest,
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
      color   = if (options[["forestPlotMappingColor"]] == "") options[["forestPlotAuxiliaryPlotColor"]],
      shape   = if (options[["forestPlotMappingShape"]] == "") 15
    )
    plotForest <- plotForest + do.call(ggplot2::geom_point, geomCall[!sapply(geomCall, is.null)]) +
      ggplot2::scale_size(range = c(1, 6) * options[["forestPlotRelativeSizeEstimates"]])


    # change scale for shapes to full shapes if used
    if (options[["forestPlotMappingShape"]] != "")
      plotForest <- plotForest + ggplot2::scale_shape_manual(values = rep(c(15:18, 21:25), length.out = length(unique(dfForrest[[options[["forestPlotMappingShape"]]]]))))


    ### add CIs
    plotForest <- plotForest + ggplot2::geom_errorbarh(
      data    = dfForrest,
      mapping = ggplot2::aes(
        xmin = lCi,
        xmax = uCi,
        y    = y
      ),
      height = 0
    )

    if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
      plotForest <- plotForest + ggplot2::geom_errorbarh(
        data    = dfForrest,
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


  ### Make the left information panel ----
  if (length(options[["forestPlotStudyInformationSelectedVariablesSettings"]]) > 0 || length(additionalInformation) > 0) {

    ### determine number of columns and study information
    leftPanelStudyInformation   <- do.call(rbind.data.frame, options[["forestPlotStudyInformationSelectedVariablesSettings"]])

    ### compute the total character width
    if (options[["forestPlotStudyInformation"]] && length(leftPanelStudyInformation) != 0) {
      leftPanelStudyInformationChars <- rbind(
        nchar(leftPanelStudyInformation$title),
        apply(dfForrest[,leftPanelStudyInformation$value, drop = FALSE], 2, function(x) max(nchar(x), na.rm = TRUE)))
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

    ### start plotting
    plotLeft <- ggplot2::ggplot()

    ### add the subplots
    if (options[["forestPlotStudyInformation"]] && length(leftPanelStudyInformation) > 0) {

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

      # compute study information coordinates
      leftPanelStudyInformation$y         <- (max(dfForrest$row) + 1) * relativeRowSize
      leftPanelStudyInformation$x         <- ifelse(
        leftPanelStudyInformation$alignment == "left", leftPanelStudyInformation$xStart, ifelse(
          leftPanelStudyInformation$alignment == "middle", (leftPanelStudyInformation$xStart + leftPanelStudyInformation$xEnd) / 2, leftPanelStudyInformation$xEnd
      ))

      # add titles
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

      # add information
      if (any(leftPanelStudyInformation$value == options[["forestPlotMappingColor"]])) {
        leftPanelStudyDataColored <- data.frame(
          x         = leftPanelStudyInformation$x[leftPanelStudyInformation$value == options[["forestPlotMappingColor"]]],
          y         = dfForrest$y,
          label     = as.character(dfForrest[[options[["forestPlotMappingColor"]]]]),
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
        tempVariables <- leftPanelStudyInformation$value[leftPanelStudyInformation$value != options[["forestPlotMappingColor"]]]
        leftPanelStudyData <- do.call(rbind.data.frame, lapply(tempVariables, function(variable) {
          data.frame(
            x         = leftPanelStudyInformation$x[leftPanelStudyInformation$value == variable],
            y         = dfForrest$y,
            label     = as.character(dfForrest[[variable]]),
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

    if (length(additionalInformation) > 0) {

      # subset left panel information only
      leftPanelAdditionalInformation   <- additionalInformation[!is.na(additionalInformation$label),]
      leftPanelAdditionalInformation$x <- 1
      leftPanelAdditionalInformation$face[is.na(leftPanelAdditionalInformation$face)] <- "plain"

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
  ### Make the right information panel ----
  if (.maForestPlotMakeRightPannel(options, additionalInformation)) {

    # estimates and confidence intervales
    if (options[["forestPlotEstimatesAndConfidenceIntervals"]]) {

      ### join the est and Cis for the right panel
      rightPanelCis <- rbind(
        if (options[["forestPlotStudyInformation"]]) {
          tempDf <- dfForrest[,c("y", "effectSize", "lCi", "uCi")]
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
        tempDf <- dfForrest[,c("y", "weights")]
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
    attr(plotOut, "rows")    <- tempRow + max(dfForrest$row)

  } else {

    plotOut <- list()
    if (!is.null(plotLeft))
      plotOut <-  c(plotOut, list(plotLeft))
    plotOut <-  c(plotOut, list(plotForest))
    if (!is.null(plotRight))
      plotOut <- c(plotOut, list(plotRight))

    attr(plotOut, "isPanel")     <- TRUE
    attr(plotOut, "panelRatio")  <- panelRatio
    attr(plotOut, "rows")        <- tempRow + if(!is.null(dfForrest)) max(dfForrest$row) else 0
    attr(plotOut, "widths")      <- plotsWidths
    attr(plotOut, "layout")      <- matrix(1:length(plotOut), nrow = 1, ncol = length(plotOut), byrow = TRUE)

  }

  return(plotOut)
}
.maForestPlotMakeRightPannel <- function(options, additionalInformation) {

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
