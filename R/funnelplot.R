#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

FunnelPlot <- function(jaspResults, dataset = NULL, options, ...) {

  if (.fpReady(options)) {
    # check data set
    dataset <- .fpCheckDataset(jaspResults, dataset, options)

    # pre-fit models if required
    .fpH1Fits(jaspResults, dataset, options)
    .fpH1TrimAndFillFits(jaspResults, dataset, options)
  }

  # make the funnel plots
  .fpPlot(jaspResults, dataset, options)
  if (options[["funnelUnderH1EstimatesTable"]])
    .fpPlotEstimatesTable(jaspResults, dataset, options)

  # add the funnel plot asymmetry table
  if (options[["funnelPlotAsymmetryTests"]])
    .fpTestFunnelPlotAsymmetryTests(jaspResults, dataset, options)

  # add trim and fill
  if (options[["trimAndFill"]]) {
    .fpTrimAndFillPlot(jaspResults, dataset, options)

    if (options[["trimAndFillEstimatesTable"]])
     .fpTrimAndFillEstimatesTable(jaspResults, dataset, options)
  }


  return()
}

.fpDependencies <- c("effectSize", "effectSizeStandardError", "split")
.fpReady        <- function(options) {
  return(options[["effectSize"]] != "" && options[["effectSizeStandardError"]] != "")
}
.fpCheckDataset <- function(jaspResults, dataset, options) {

  # omit NAs
  dataset <- na.omit(dataset)

  # add a warning message
  if (!is.null(attr(dataset, "na.action")) && is.null(jaspResults[["missingDataInformation"]])) {
    missingDataInformation <- createJaspHtml(gettext("Missing Data Summary"))
    missingDataInformation$position <- 0.1
    missingDataInformation$dependOn(c(.fpDependencies, "estimatesMappingColor", "estimatesMappingShape", "studyLabel"))
    missingDataInformation$text <- gettextf("The dataset contains missing values: %1$i missing values were removed from the analysis.", length(attr(dataset, "na.action")))
    jaspResults[["missingDataInformation"]] <- missingDataInformation
  }

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations", "variance"),
    all.target           = c(
      options[["effectSize"]],
      options[["effectSizeStandardError"]]
    ),
    observations.amount  = "< 2",
    exitAnalysisIfErrors = TRUE)

  .hasErrors(
    dataset              = dataset,
    seCheck.target       = options[["effectSizeStandardError"]],
    custom               = .maCheckStandardErrors,
    exitAnalysisIfErrors = TRUE)

  return(dataset)
}

.fpH1Fits                       <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["fitState"]]))
    return()

  # fit the models only if
  # - estimated funnel plot under H1 is requested
  # - meta-regression asymmetry test is requested
  # - trim and fill is requested

  if (!((options[["funnelUnderH1"]] && options[["funnelUnderH1Parameters"]] == "estimated") ||
         options[["funnelPlotAsymmetryTests"]] || options[["trimAndFill"]]))
    return()

  # store the fits into a state
  fitState <- createJaspState()
  fitState$dependOn(c(.fpDependencies, "method"))
  jaspResults[["fitState"]] <- fitState

  if (options[["split"]] == "") {

    fitState$object <- try(metafor::rma(
      yi     = dataset[[options[["effectSize"]]]],
      sei    = dataset[[options[["effectSizeStandardError"]]]],
      method = .maGetMethodOptions(options)
    ))

  } else {

    splitLevels <- unique(dataset[[options[["split"]]]])
    fits <- lapply(splitLevels, function(splitLevel) {
      try(metafor::rma(
        yi     = dataset[[options[["effectSize"]]]],
        sei    = dataset[[options[["effectSizeStandardError"]]]],
        subset = dataset[[options[["split"]]]] == splitLevel,
        method = .maGetMethodOptions(options)
      ))
    })
    names(fits) <- splitLevels
    fitState$object <- fits
  }

  return()
}
.fpH1TrimAndFillFits            <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["trimAndFillState"]]))
    return()

  if (!options[["trimAndFill"]])
    return()

  # store the fits into a state
  trimAndFillState <- createJaspState()
  trimAndFillState$dependOn(c(.fpDependencies, "method", "trimAndFillEstimator"))
  jaspResults[["trimAndFillState"]] <- trimAndFillState

  if (options[["split"]] == "") {

    trimAndFillState$object <- try(metafor::trimfill(
      jaspResults[["fitState"]]$object,
      estimator = options[["trimAndFillEstimator"]]
    ))

  } else {

    splitLevels <- unique(dataset[[options[["split"]]]])
    fits <- lapply(splitLevels, function(splitLevel) {
      try(metafor::trimfill(
        jaspResults[["fitState"]]$object[[splitLevel]],
        estimator = options[["trimAndFillEstimator"]]
      ))
    })

    names(fits) <- splitLevels
    trimAndFillState$object <- fits
  }

  return()
}
.fpPlot                         <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["funnelPlotContainer"]])) {
    funnelPlotContainer <- createJaspContainer(title = gettext("Funnel Plot"))
    funnelPlotContainer$dependOn(c(
      .fpDependencies, "studyLabel",
      "funnelUnderH0", "funnelUnderH0ParametersFixedMu", "funnelUnderH0ParametersFixedTau",
      "funnelUnderH1", "funnelUnderH1Parameters", "funnelUnderH1ParametersFixedMu", "funnelUnderH1ParametersFixedTau", "funnelUnderH1IncludeHeterogeneity",
      "funnelUnderH1PowerEnhancement", "funnelUnderH1PowerEnhancementBreaks",
      "funnelUnderH0LineType", "funnelUnderH0FillColors", "funnelUnderH1LineType", "funnelUnderH1FillColors",
      "invertColors", "funnelPredictionInterval", "method",
      "estimatesMappingLabel", "estimatesMappingColor", "estimatesMappingShape", "estimatesLegendPosition", "estimatesMappingLabelOffset", "colorPalette"
    ))
    funnelPlotContainer$position <- 1
    jaspResults[["funnelPlotContainer"]] <- funnelPlotContainer
  } else {
    funnelPlotContainer <- jaspResults[["funnelPlotContainer"]]
  }

  # create a waitting plot
  if (!.fpReady(options)) {
    tempPlot <- createJaspPlot(width = 550, height = 480)
    funnelPlotContainer[["tempPlot"]] <- tempPlot
    return()
  }

  # create funnel plots
  if (options[["split"]] == "") {

    funnelPlot <- createJaspPlot(width = 550, height = 480)
    funnelPlotContainer[["funnelPlot"]] <- funnelPlot

    if (options[["funnelUnderH1"]] && options[["funnelUnderH1Parameters"]] == "estimated" && jaspBase::isTryError(jaspResults[["fitState"]]$object))
      funnelPlot$setError(.fpMetaforTranslateErrorMessage(jaspResults[["fitState"]]$object))
    else
      funnelPlot$plotObject <- .fpMakeFunnelPlot(jaspResults, dataset, options)

  } else {

    splitLevels <- unique(dataset[[options[["split"]]]])
    for (splitLevel in splitLevels) {

      funnelPlot <- createJaspPlot(title = paste0(options[["split"]], " = ", splitLevel), width = 550, height = 480)
      funnelPlotContainer[[splitLevel]] <- funnelPlot

      if (options[["funnelUnderH1"]] && options[["funnelUnderH1Parameters"]] == "estimated" && jaspBase::isTryError(jaspResults[["fitState"]]$object[[splitLevel]]))
        funnelPlot$setError(.fpMetaforTranslateErrorMessage(jaspResults[["fitState"]]$object[[splitLevel]]))
      else
        funnelPlot$plotObject <- .fpMakeFunnelPlot(jaspResults, dataset, options, splitLevel = splitLevel)

    }

  }

  return()
}
.fpTrimAndFillPlot              <- function(jaspResults, dataset, options) {

  trimAndFillContainer <- .fpGetTrimAndFillContainer(jaspResults)

  # create a waiting plot
  if (!.fpReady(options)) {
    tempPlot <- createJaspPlot(width = 550, height = 480)
    trimAndFillContainer[["tempPlot"]] <- tempPlot
    return()
  }

  # dependencies for the trim and fill plot
  .fpTrimAndFillPlotDependencies <- c(
    "studyLabel",
    "trimAndFillIncludeHeterogeneity", "trimAndFillFillColors", "trimAndFillLineType",
    "funnelPredictionInterval",  "invertColors",
    "estimatesMappingLabel", "estimatesMappingColor", "estimatesMappingShape", "estimatesLegendPosition", "estimatesMappingLabelOffset", "colorPalette"
  )

  # create funnel plots
  if (options[["split"]] == "") {

    if (!is.null(trimAndFillContainer[["funnelPlot"]]))
      return()

    funnelPlot <- createJaspPlot(width = 550, height = 480)
    funnelPlot$dependOn(.fpTrimAndFillPlotDependencies)
    trimAndFillContainer[["funnelPlot"]] <- funnelPlot

    fit <- jaspResults[["trimAndFillState"]]$object
    if (jaspBase::isTryError(fit))
      funnelPlot$setError(.fpMetaforTranslateErrorMessage(fit))
    else
      funnelPlot$plotObject <- .fpMakeFunnelPlot(jaspResults, dataset, options, isTrimAndFill = TRUE)

  } else {

    splitLevels <- unique(dataset[[options[["split"]]]])
    for (splitLevel in splitLevels) {

      if (!is.null(trimAndFillContainer[[splitLevel]]))
        next

      funnelPlot <- createJaspPlot(title = paste0(options[["split"]], " = ", splitLevel), width = 550, height = 480)
      funnelPlot$dependOn(.fpTrimAndFillPlotDependencies)
      trimAndFillContainer[[splitLevel]] <- funnelPlot

      fit <- jaspResults[["trimAndFillState"]]$object[[splitLevel]]
      if (jaspBase::isTryError(fit))
        funnelPlot$setError(.fpMetaforTranslateErrorMessage(fit))
      else
        funnelPlot$plotObject <- .fpMakeFunnelPlot(jaspResults, dataset, options, splitLevel = splitLevel, isTrimAndFill = TRUE)
    }

  }

  return()
}
.fpMakeFunnelPlot               <- function(jaspResults, dataset, options, splitLevel = NULL, isTrimAndFill = FALSE) {

  ### extract the funnel levels
  if (options[["funnelUnderH0"]] || options[["funnelUnderH1"]] || isTrimAndFill) {
    funnelLevels <- .robmaCleanOptionsToPriors(options[["funnelPredictionInterval"]], message = gettext("Funnel plot prediction interval was specified in an incorrect format. Try '(0.90, 0.95, 0.99)'."))
    if (any(is.na(funnelLevels)) || any(funnelLevels <= 0 | funnelLevels >= 1))
      .quitAnalysis(gettext("Funnel plot prediction intervals must be between 0 and 1."))
    if (length(funnelLevels) < 1)
      .quitAnalysis(gettext("Funnel plot prediction intervals must be specified."))
    funnelLevels <- (1 - funnelLevels) / 2
    funnelLevels <- sort(funnelLevels)

    # funnel colors
    funnelColorsSteps <- 2 * length(funnelLevels) + 1
    funnelColorsSteps <- seq(0, 1, length.out = funnelColorsSteps)
    funnelColorsSteps <- funnelColorsSteps[-c(1, length(funnelColorsSteps))]
    funnelColors      <- paste0("grey", round(funnelColorsSteps * 100))

    if (options[["invertColors"]])
      funnelColors <- rev(funnelColors)
  }

  ### data-points
  dfPlot <- data.frame(
    x  = dataset[[options[["effectSize"]]]],
    y  = dataset[[options[["effectSizeStandardError"]]]]
  )
  if (options[["estimatesMappingShape"]] != "") dfPlot$shape <- dataset[[options[["estimatesMappingShape"]]]]
  if (options[["estimatesMappingColor"]] != "") dfPlot$color <- dataset[[options[["estimatesMappingColor"]]]]
  if (options[["studyLabel"]] != "")            dfPlot$label <- dataset[[options[["studyLabel"]]]]

  if (!is.null(splitLevel))
    dfPlot <- dfPlot[dataset[[options[["split"]]]] == splitLevel,]

  # additional data points from trim and fill
  if (isTrimAndFill) {
    if (is.null(splitLevel)) {
      tempFit <- jaspResults[["trimAndFillState"]]$object
    } else {
      tempFit <- jaspResults[["trimAndFillState"]]$object[[splitLevel]]
    }

    if (any(tempFit$fill)) {
      dfPlotTrimAndFill <- data.frame(
        x = tempFit$yi[tempFit$fill],
        y = sqrt(tempFit$vi[tempFit$fill])
      )
    } else {
      dfPlotTrimAndFill <- NULL
    }
  } else {
    dfPlotTrimAndFill <- NULL
  }

  ### y-axis plotting range (based on the common data set to make them common across figures)
  yTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(0, dataset[[options[["effectSizeStandardError"]]]], dfPlotTrimAndFill[["y"]])))
  # a sequence of points must be used if tau is included in the confidence bands (PI is a nonlinear function of se)
  ySeqH0 <- if (options[["funnelUnderH0ParametersFixedTau"]] == 0) range(yTicks) else seq(from = min(yTicks), to = max(yTicks), length.out = 100)
  ySeqH1 <- if ((options[["funnelUnderH1Parameters"]] == "estimated" && !options[["funnelUnderH1IncludeHeterogeneity"]])
                || (options[["funnelUnderH1Parameters"]] == "fixed"  && options[["funnelUnderH1ParametersFixedTau"]] == 0))
    range(yTicks) else seq(from = min(yTicks), to = max(yTicks), length.out = 100)

  ### specify zero-centered funnels
  if (options[["funnelUnderH0"]] && !isTrimAndFill) {
    adjustFunnel0Mean          <- options[["funnelUnderH0ParametersFixedMu"]]
    adjustFunnel0Heterogeneity <- options[["funnelUnderH0ParametersFixedTau"]]
    dfsFunnel0 <- .fpComputeFunnelDf(ySeqH0, adjustFunnel0Mean, adjustFunnel0Heterogeneity, funnelLevels)
  }

  ### specify meta-analysis centered funnels
  # allow user imputed vs meta-analytic estimated values
  if (options[["funnelUnderH1"]] || isTrimAndFill) {

    if (options[["funnelUnderH1Parameters"]] == "estimated" || isTrimAndFill){

      if (options[["split"]] == "") {
        fit <- if (isTrimAndFill) jaspResults[["trimAndFillState"]]$object else jaspResults[["fitState"]]$object
      } else {
        fit <- if (isTrimAndFill) jaspResults[["trimAndFillState"]]$object[[splitLevel]] else jaspResults[["fitState"]]$object[[splitLevel]]
      }

      adjustFunnel1Mean          <- fit$b[1]
      adjustFunnel1Heterogeneity <- if ((isTrimAndFill && options[["trimAndFillIncludeHeterogeneity"]]) || (!isTrimAndFill && options[["funnelUnderH1IncludeHeterogeneity"]])) sqrt(fit$tau2) else 0
    } else if (options[["funnelUnderH1Parameters"]] == "fixed") {
      adjustFunnel1Mean          <- options[["funnelUnderH1ParametersFixedMu"]]
      adjustFunnel1Heterogeneity <- options[["funnelUnderH1ParametersFixedTau"]]
    }

    dfsFunnel1 <- .fpComputeFunnelDf(ySeqH1, adjustFunnel1Mean, adjustFunnel1Heterogeneity, funnelLevels)

    # get maximum x value across all funnels in case of a split
    if (options[["split"]] == "" || (!isTrimAndFill && options[["funnelUnderH1Parameters"]] == "fixed")) {
      dfsFunnel1XRange <- range(sapply(dfsFunnel1, function(x) x$x))
    } else {
      dfsFunnel1XMax <- list()
      tempFits <- if (isTrimAndFill) jaspResults[["trimAndFillState"]]$object else jaspResults[["fitState"]]$object
      for (i in seq_along(tempFits)) {
        # extract each fit
        tempFit <- tempFits[[i]]
        if (jaspBase::isTryError(tempFit))
          next
        tempAdjustFunnel1Mean          <- tempFit$b[1]
        tempAdjustFunnel1Heterogeneity <- if ((isTrimAndFill && options[["trimAndFillIncludeHeterogeneity"]]) || (!isTrimAndFill && options[["funnelUnderH1IncludeHeterogeneity"]])) sqrt(tempFit$tau2) else 0

        # compute the maximum funnel width
        tempFitX <- .fpComputeFunnelDf(max(ySeqH1), tempAdjustFunnel1Mean, tempAdjustFunnel1Heterogeneity, min(funnelLevels))
        dfsFunnel1XMax[[i]] <- range(tempFitX[[1]])
      }
      dfsFunnel1XRange <- range(unlist(dfsFunnel1XMax))
    }
  }


  ### get x-axis ticks
  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(
    range(dataset[[options[["effectSize"]]]]),
    if (options[["funnelUnderH0"]] && !isTrimAndFill) range(sapply(dfsFunnel0, function(x) x$x)),
    if (options[["funnelUnderH1"]] || isTrimAndFill)  dfsFunnel1XRange
  )))


  ### compute power enhancement
  if (!isTrimAndFill && options[["funnelUnderH1"]] && options[["funnelUnderH1PowerEnhancement"]]) {
    powerEnhancementBreaks <- .robmaCleanOptionsToPriors(options[["funnelUnderH1PowerEnhancementBreaks"]], message = gettext("Power enhancement breaks were specified in an incorrect format. Try '(0.30, 0.50, 0.80)'."))
    if (any(is.na(powerEnhancementBreaks)) || any(powerEnhancementBreaks <= 0.05 | powerEnhancementBreaks >= 1))
      .quitAnalysis(gettext("Power enhancement breaks must be between 0.05 and 1."))
    powerEnhancementBreaks   <- sort(powerEnhancementBreaks)
    powerEnhancementBreaksZ  <- .power_to_z(powerEnhancementBreaks, two.sided = TRUE)

    # add the first and last breaks
    powerEnhancementBreaks  <- c(0.05, powerEnhancementBreaks, 1)
    powerEnhancementBreaksZ <- c(0,    powerEnhancementBreaksZ, Inf)

    # compute the se ranges and restrict to the plotting range
    powerEnhancementBreaksSe     <- abs(adjustFunnel1Mean) / powerEnhancementBreaksZ
    powerEnhancementBreaks       <- powerEnhancementBreaks[(which.max(powerEnhancementBreaksSe < max(yTicks)) - 1):length(powerEnhancementBreaksSe)]
    powerEnhancementBreaksSe     <- powerEnhancementBreaksSe[(which.max(powerEnhancementBreaksSe < max(yTicks)) - 1):length(powerEnhancementBreaksSe)]
    powerEnhancementBreaksSe[1]  <- max(yTicks)
    powerEnhancementBreaksLabels <- paste0(powerEnhancementBreaks[-length(powerEnhancementBreaks)] * 100, "% - ", round(powerEnhancementBreaks[-1] * 100, 2), "%")

    # get the colors
    powerEnhancementColors   <- .getPowerEnhancementColors(length(powerEnhancementBreaksLabels))

    # create segments
    dfsPowerEnhancement <- lapply(seq_along(powerEnhancementBreaksLabels), function(i) {
      data.frame(
        x = c(min(xTicks), max(xTicks), max(xTicks), min(xTicks)),
        y = c(powerEnhancementBreaksSe[i], powerEnhancementBreaksSe[i], powerEnhancementBreaksSe[i+1], powerEnhancementBreaksSe[i+1]),
        label = powerEnhancementBreaksLabels[i],
        color = powerEnhancementColors[i]
      )
    })
  }


  ### prepare lables
  if (options[["studyLabel"]] != "" && options[["estimatesMappingLabel"]] != "none") {

    dfLabels <- dfPlot

    # exclusion of data points outside the funnel (if requested) and alignment with the appropriate funnel
    if (options[["estimatesMappingLabel"]] %in% c("outsideH0", "outsideH1")) {
      # get the appropriate funnel parameters
      tempAdjustMean          <- if (options[["estimatesMappingLabel"]] == "outsideH0") adjustFunnel0Mean          else adjustFunnel1Mean
      tempAdjustHeterogeneity <- if (options[["estimatesMappingLabel"]] == "outsideH0") adjustFunnel0Heterogeneity else adjustFunnel1Heterogeneity
      # exclusion of data points outside the funnel
      tempDiff <- abs(dfLabels$x - tempAdjustMean)
      tempDiff[tempDiff < 1.96 * tempAdjustHeterogeneity] <- 0
      tempZ    <- tempDiff / dfLabels$y
      dfLabels <- dfLabels[tempZ > max(qnorm(funnelLevels, lower.tail = FALSE)),]
    } else {
      # use H1 -> H0 -> mean to align the if the funnels are present
      tempAdjustMean <- if (options[["funnelUnderH1"]]) adjustFunnel1Mean else if (options[["funnelUnderH0"]]) adjustFunnel0Mean else 0
    }
    # specify the position of the labels
    dfLabels$position <- ifelse(dfLabels$x < tempAdjustMean, "right", "left")
    dfLabels$nudge_x  <- ifelse(dfLabels$x < tempAdjustMean, -1, 1) * options[["estimatesMappingLabelOffset"]]
  }

  ### specify "background" for the funnel plot
  dfBackground <- data.frame(
    x = c(min(xTicks), max(xTicks), max(xTicks), min(xTicks)),
    y = c(min(yTicks), min(yTicks), max(yTicks), max(yTicks))
  )

  ### plot
  out <- ggplot2::ggplot()

  if (options[["invertColors"]])
    out <- out + ggplot2::geom_polygon(
      data    = dfBackground,
      mapping = ggplot2::aes(x = x, y = y),
      fill    = "black"
    )

  if (!isTrimAndFill && options[["funnelUnderH1"]] && options[["funnelUnderH1PowerEnhancement"]]) {
    for (i in seq_along(dfsPowerEnhancement)) {
      out <- out + ggplot2::geom_polygon(
        data    = dfsPowerEnhancement[[i]],
        mapping = ggplot2::aes(x = x, y = y),
        fill    = dfsPowerEnhancement[[i]]$color[1]
      )
    }
  }

  # add H0 funnel
  if (!isTrimAndFill && options[["funnelUnderH0"]]) {

    if (options[["funnelUnderH0FillColors"]]) {
      for (i in rev(seq_along(dfsFunnel0))) {
        out <- out + ggplot2::geom_polygon(
          data     = dfsFunnel0[[i]],
          mapping  = ggplot2::aes(x = x, y = y),
          fill     = scales::alpha(funnelColors[i], .25)
        )
      }
    }

    if (options[["funnelUnderH0LineType"]]!= "none") {
      for (i in rev(seq_along(dfsFunnel0))) {
        out <- out + ggplot2::geom_line(
          data     = dfsFunnel0[[i]],
          mapping  = ggplot2::aes(x = x, y = y),
          linetype = options[["funnelUnderH0LineType"]]
        )
      }
    }
  }

  # add H1 funnel
  if (isTrimAndFill || options[["funnelUnderH1"]]) {

    if ((isTrimAndFill && options[["trimAndFillFillColors"]]) || (!isTrimAndFill && options[["funnelUnderH1FillColors"]])) {
      for (i in rev(seq_along(dfsFunnel1))) {
        out <- out + ggplot2::geom_polygon(
          data     = dfsFunnel1[[i]],
          mapping  = ggplot2::aes(x = x, y = y),
          fill     = scales::alpha(funnelColors[i], .25)
        )
      }
    }

    if ((isTrimAndFill && options[["trimAndFillLineType"]]!= "none") || (!isTrimAndFill && options[["funnelUnderH1LineType"]]!= "none")) {
      for (i in rev(seq_along(dfsFunnel1))) {
        out <- out + ggplot2::geom_line(
          data     = dfsFunnel1[[i]],
          mapping  = ggplot2::aes(x = x, y = y),
          linetype = if (isTrimAndFill) options[["trimAndFillLineType"]] else options[["funnelUnderH1LineType"]]
        )
      }
    }
  }

  # add estimates
  pointAes <- list(
    x = as.name("x"),
    y = as.name("y")
  )
  if (options[["estimatesMappingShape"]] != "") pointAes$shape <- as.name("shape")
  if (options[["estimatesMappingColor"]] != "") pointAes$fill  <- as.name("color")

  out <- out + jaspGraphs::geom_point(
    data    = dfPlot,
    mapping = do.call(ggplot2::aes, pointAes)
  )

  # add imputed estimates
  if (isTrimAndFill && !is.null(dfPlotTrimAndFill)) {
    out <- out + jaspGraphs::geom_point(
      data    = dfPlotTrimAndFill,
      mapping = ggplot2::aes(x = x, y = y),
      shape   = 21,
      color   = "black",
      fill    = "white"
    )
  }

  if (options[["estimatesMappingShape"]] != "")
    out <- out + ggplot2::labs(shape = options[["estimatesMappingShape"]])
  if (options[["estimatesMappingColor"]] != "")
    out <- out + ggplot2::labs(color = options[["estimatesMappingColor"]], fill = options[["estimatesMappingColor"]])


  # add labels
  if (options[["studyLabel"]] != "" && options[["estimatesMappingLabel"]] != "none") {
    out <- out +
      ggplot2::geom_text(
        data    = dfLabels,
        mapping = ggplot2::aes(x = x, y = y, label = label, hjust = position),
        nudge_x = dfLabels$nudge_x,

      )
  }

  out <- out + jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks), name = gettext("Effect Size"), oob = scales::oob_keep)

  # add secondary axis whenever needed
  if (!isTrimAndFill && options[["funnelUnderH1"]] && options[["funnelUnderH1PowerEnhancement"]]) {
    out <- out + ggplot2::scale_y_reverse(
      breaks = rev(yTicks), limits = rev(range(yTicks)), name = gettext("Standard Error"), oob = scales::oob_keep,
      sec.axis = ggplot2::dup_axis(
        breaks = rev(powerEnhancementBreaksSe),
        labels = rev(paste0(round(c(.z_to_power(abs(adjustFunnel1Mean) / powerEnhancementBreaksSe[1]), powerEnhancementBreaks[-1]) * 100), "% ")), name = gettext("Power"))
    )
  } else {
    out <- out + ggplot2::scale_y_reverse(breaks = rev(yTicks), limits = rev(range(yTicks)), name = gettext("Standard Error"), oob = scales::oob_keep)
  }

  if (options[["estimatesMappingColor"]] != "")
    out <- out +
    jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])

  out <- out +
    jaspGraphs::geom_rangeframe(sides = if (options[["funnelUnderH1"]] && options[["funnelUnderH1PowerEnhancement"]]) "blr" else "bl") +
    jaspGraphs::themeJaspRaw(legend.position = options[["estimatesLegendPosition"]])

  return(out)
}
.fpPlotEstimatesTable           <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["funnelParametersTable"]]) || options[["funnelUnderH1Parameters"]] != "estimated")
    return()

  # estimates table
  funnelParametersTable          <- createJaspTable(gettext("H₁ Funnel Parameter Estimates"))
  funnelParametersTable$position <- 2
  funnelParametersTable$dependOn(c(.fpDependencies, "funnelUnderH1Parameters", "method", "funnelUnderH1EstimatesTable"))
  jaspResults[["funnelParametersTable"]] <- funnelParametersTable

  if (options[["split"]] != "")
    funnelParametersTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
  funnelParametersTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")

  overtitleMu <- gettext("Estimate \U03BC")
  funnelParametersTable$addColumnInfo(name = "muEst", title = gettext("Estimate"),        type = "number", overtitle = overtitleMu)
  funnelParametersTable$addColumnInfo(name = "muLCI", title = gettextf("Lower 95%% CI"),  type = "number", overtitle = overtitleMu)
  funnelParametersTable$addColumnInfo(name = "muUCI", title = gettextf("Upper 95%% CI"),  type = "number", overtitle = overtitleMu)
  funnelParametersTable$addColumnInfo(name = "muP",   title = gettext("p"),               type = "pvalue", overtitle = overtitleMu)

  if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
    overtitleTau <- gettext("Estimate \U1D70F")
    funnelParametersTable$addColumnInfo(name = "tauEst", title = gettext("Estimate"),         type = "number", overtitle = overtitleTau)
    funnelParametersTable$addColumnInfo(name = "tauLCI", title = gettextf("Lower 95%% CI"),   type = "number", overtitle = overtitleTau)
    funnelParametersTable$addColumnInfo(name = "tauUCI", title = gettextf("Upper 95%% CI"),   type = "number", overtitle = overtitleTau)
    funnelParametersTable$addColumnInfo(name = "tauP",   title = gettext("p"),                type = "pvalue", overtitle = overtitleTau)
  }

  if (!.fpReady(options))
    return()

  if (options[["split"]] == "") {

    fit        <- jaspResults[["fitState"]]$object
    fitSummary <- .fpExtractFitEstimates(fit, options)
    if (jaspBase::isTryError(fit))
      funnelParametersTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = gettext("The funnel plot parameter estimation failed with the following error: "))

  } else {

    fits       <- jaspResults[["fitState"]]$object
    fitSummary <- do.call(rbind, lapply(fits, function(fit) {

      tempFitSummary <- .fpExtractFitEstimates(fit, options)
      if (jaspBase::isTryError(fit))
        funnelParametersTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = gettext("The funnel plot parameter estimation failed with the following error: "))

      return(tempFitSummary)
    }))
    fitSummary <- data.frame(split = names(fits), fitSummary)

  }

  funnelParametersTable$setData(fitSummary)

  return()
}
.fpTrimAndFillEstimatesTable    <- function(jaspResults, dataset, options) {

  trimAndFillContainer <- .fpGetTrimAndFillContainer(jaspResults)

  if (!is.null(trimAndFillContainer[["trimAndFillTable"]]))
    return()

  # Trim and Fill Estimates Table
  trimAndFillTable          <- createJaspTable(gettext("Trim and Fill Parameter Estimates"))
  trimAndFillTable$dependOn(c(.fpDependencies, "trimAndFillEstimatesTable"))
  trimAndFillContainer[["trimAndFillTable"]] <- trimAndFillTable

  if (options[["split"]] != "")
    trimAndFillTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
  trimAndFillTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")

  trimAndFillTable$addColumnInfo(name = "missingK", title = gettext("Missing Estimates"), type = "integer", overtitle = gettext("Trim and Fill"))
  if (options[["trimAndFillEstimator"]] == "R0") {
    trimAndFillTable$addColumnInfo(name = "missingP", title = gettext("p"), type = "pvalue",  overtitle = gettext("Trim and Fill"))
  }

  overtitleMu <- gettext("Adjusted Estimate \U03BC")
  trimAndFillTable$addColumnInfo(name = "muEst", title = gettext("Estimate"),        type = "number", overtitle = overtitleMu)
  trimAndFillTable$addColumnInfo(name = "muLCI", title = gettextf("Lower 95%% CI"),  type = "number", overtitle = overtitleMu)
  trimAndFillTable$addColumnInfo(name = "muUCI", title = gettextf("Upper 95%% CI"),  type = "number", overtitle = overtitleMu)
  trimAndFillTable$addColumnInfo(name = "muP",   title = gettext("p"),               type = "pvalue", overtitle = overtitleMu)

  if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
    overtitleTau <- gettext("Adjusted Estimate \U1D70F")
    trimAndFillTable$addColumnInfo(name = "tauEst", title = gettext("Estimate"),         type = "number", overtitle = overtitleTau)
    trimAndFillTable$addColumnInfo(name = "tauLCI", title = gettextf("Lower 95%% CI"),   type = "number", overtitle = overtitleTau)
    trimAndFillTable$addColumnInfo(name = "tauUCI", title = gettextf("Upper 95%% CI"),   type = "number", overtitle = overtitleTau)
    trimAndFillTable$addColumnInfo(name = "tauP",   title = gettext("p"),                type = "pvalue", overtitle = overtitleTau)
  }


  if (!.fpReady(options))
    return()

  if (options[["split"]] == "") {

    fit        <- jaspResults[["trimAndFillState"]]$object
    fitSummary <- .fpExtractTrimAndFillEstimates(fit, options)
    if (jaspBase::isTryError(fit))
      trimAndFillTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = gettext("The funnel plot parameter estimation failed with the following error: "))

  } else {

    fits       <- jaspResults[["trimAndFillState"]]$object
    fitSummary <- do.call(rbind, lapply(fits, function(fit) {

      tempFitSummary <- .fpExtractTrimAndFillEstimates(fit, options)
      if (jaspBase::isTryError(fit))
        trimAndFillTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = gettext("The funnel plot parameter estimation failed with the following error: "))

      return(tempFitSummary)
    }))
    fitSummary <- data.frame(split = names(fits), fitSummary)

  }

  trimAndFillTable$setData(fitSummary)

  return()
}
.fpTestFunnelPlotAsymmetryTests <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["funnelPlotAsymmetryTests"]])) {
    funnelAsymetryTests <- createJaspContainer(title = gettext("Funnel Plot Asymmetry Tests"))
    funnelAsymetryTests$dependOn(c(.fpDependencies, "funnelPlotAsymmetryTests"))
    funnelAsymetryTests$position <- 3
    jaspResults[["funnelAsymetryTests"]] <- funnelAsymetryTests
  } else {
    funnelAsymetryTests <- jaspResults[["funnelAsymetryTests"]]
  }

  ### create table for each test

  # meta-regression
  if (options[["funnelPlotAsymmetryTests"]] && is.null(funnelAsymetryTests[["metaRegressionTable"]])) {

    metaRegressionTable <- createJaspTable(gettext("Meta-Regression Test for Funnel Plot Asymmetry"))
    metaRegressionTable$position <- 1
    metaRegressionTable$dependOn("funnelPlotAsymmetryTestsMetaRegression")
    funnelAsymetryTests[["metaRegressionTable"]] <- metaRegressionTable

    if (options[["split"]] != "")
      metaRegressionTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
    metaRegressionTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")
    metaRegressionTable$addColumnInfo(name = "z", title = gettext("z"), type = "number", overtitle = gettext("Asymmetry Test"))
    metaRegressionTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue", overtitle = gettext("Asymmetry Test"))
    metaRegressionTable$addColumnInfo(name = "est", title = gettext("Estimate"),       type = "number", overtitle = gettext("Limit Estimate \U03BC"))
    metaRegressionTable$addColumnInfo(name = "lCI", title = gettextf("Lower 95%% CI"), type = "number", overtitle = gettext("Limit Estimate \U03BC"))
    metaRegressionTable$addColumnInfo(name = "uCI", title = gettextf("Upper 95%% CI"), type = "number", overtitle = gettext("Limit Estimate \U03BC"))

    if (.fpReady(options)) {
      if (options[["split"]] == "") {

        fit        <- jaspResults[["fitState"]]$object
        fitTest    <- try(metafor::regtest(fit))
        fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "metaRegression")

        if (jaspBase::isTryError(fit))
          metaRegressionTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = .fpAsymmetryTestErrorMessage())
        else if (jaspBase::isTryError(fitTest))
          metaRegressionTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage())

        metaRegressionTable$setData(fitSummary)

      } else {

        fits         <- jaspResults[["fitState"]]$object
        fitSummaries <- do.call(rbind, lapply(seq_along(fits), function(i) {

          fitTest    <- try(metafor::regtest(fits[[i]]))
          fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "metaRegression")
          fitSummary$split <- names(fits)[i]

          if (jaspBase::isTryError(fits[[i]]))
            metaRegressionTable$addFootnote(.fpMetaforTranslateErrorMessage(fits[[i]]), symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))
          else if (jaspBase::isTryError(fitTest))
            metaRegressionTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))

          return(fitSummary)
        }))

        metaRegressionTable$setData(fitSummaries)

      }
    }
  }

  # weighted regression
  if (options[["funnelPlotAsymmetryTestsWeightedRegression"]] && is.null(funnelAsymetryTests[["weightedRegressionTable"]])) {

    weightedRegressionTable <- createJaspTable(gettext("Weighted Regression Test for Funnel Plot Asymmetry"))
    weightedRegressionTable$position <- 2
    weightedRegressionTable$dependOn("funnelPlotAsymmetryTestsWeightedRegression")
    funnelAsymetryTests[["weightedRegressionTable"]] <- weightedRegressionTable

    if (options[["split"]] != "")
      weightedRegressionTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
    weightedRegressionTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")
    weightedRegressionTable$addColumnInfo(name = "t",  title = gettext("t"),  type = "number",  overtitle = gettext("Asymmetry Test"))
    weightedRegressionTable$addColumnInfo(name = "df", title = gettext("df"), type = "integer", overtitle = gettext("Asymmetry Test"))
    weightedRegressionTable$addColumnInfo(name = "p",  title = gettext("p"),  type = "pvalue",  overtitle = gettext("Asymmetry Test"))
    weightedRegressionTable$addColumnInfo(name = "est", title = gettext("Estimate"),       type = "number", overtitle = gettext("Limit Estimate \U03BC"))
    weightedRegressionTable$addColumnInfo(name = "lCI", title = gettextf("Lower 95%% CI"), type = "number", overtitle = gettext("Limit Estimate \U03BC"))
    weightedRegressionTable$addColumnInfo(name = "uCI", title = gettextf("Upper 95%% CI"), type = "number", overtitle = gettext("Limit Estimate \U03BC"))

    if (.fpReady(options)) {
      if (options[["split"]] == "") {

        fit        <- jaspResults[["fitState"]]$object
        fitTest    <- try(metafor::regtest(fit, model = "lm"))
        fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "weightedRegression")

        if (jaspBase::isTryError(fit))
          weightedRegressionTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = .fpAsymmetryTestErrorMessage())
        else if (jaspBase::isTryError(fitTest))
          weightedRegressionTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage())

        weightedRegressionTable$setData(fitSummary)

      } else {

        fits         <- jaspResults[["fitState"]]$object
        fitSummaries <- do.call(rbind, lapply(seq_along(fits), function(i) {

          fitTest    <- try(metafor::regtest(fits[[i]], model = "lm"))
          fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "weightedRegression")
          fitSummary$split <- names(fits)[i]

          if (jaspBase::isTryError(fits[[i]]))
            weightedRegressionTable$addFootnote(.fpMetaforTranslateErrorMessage(fits[[i]]), symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))
          else if (jaspBase::isTryError(fitTest))
            weightedRegressionTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))

          return(fitSummary)
        }))

        weightedRegressionTable$setData(fitSummaries)

      }
    }
  }

  # rank correlation
  if (options[["funnelPlotAsymmetryTestsRankCorrelation"]] && is.null(funnelAsymetryTests[["rankCorrelationTable"]])) {

    rankCorrelationTable <- createJaspTable(gettext("Rank Correlation Test for Funnel Plot Asymmetry"))
    rankCorrelationTable$position <- 3
    rankCorrelationTable$dependOn("funnelPlotAsymmetryTestsRankCorrelation")
    funnelAsymetryTests[["rankCorrelationTable"]] <- rankCorrelationTable

    if (options[["split"]] != "")
      rankCorrelationTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
    rankCorrelationTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")
    rankCorrelationTable$addColumnInfo(name = "tau",   title = gettext("\U1D70F"),   type = "number")
    rankCorrelationTable$addColumnInfo(name = "p",     title = gettext("p"),         type = "pvalue")

    if (.fpReady(options)) {

      if (options[["split"]] == "") {

        fit        <- jaspResults[["fitState"]]$object
        fitTest    <- try(metafor::ranktest(fit))
        fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "rankCorrelation")

        if (jaspBase::isTryError(fit))
          rankCorrelationTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = .fpAsymmetryTestErrorMessage())
        else if (jaspBase::isTryError(fitTest))
          rankCorrelationTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage())
        else
          fitSummary$k <- fit$k

        rankCorrelationTable$setData(fitSummary)

      } else {

        fits         <- jaspResults[["fitState"]]$object
        fitSummaries <- do.call(rbind, lapply(seq_along(fits), function(i) {

          fitTest    <- try(metafor::ranktest(fits[[i]]))
          fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "rankCorrelation")
          fitSummary$split <- names(fits)[i]

          if (jaspBase::isTryError(fits[[i]])) {
            fitSummary$k <- NA
            rankCorrelationTable$addFootnote(.fpMetaforTranslateErrorMessage(fits[[i]]), symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))
          } else if (jaspBase::isTryError(fitTest)) {
            fitSummary$k <- fits[[i]]$k
            rankCorrelationTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))
          } else {
            fitSummary$k <- fits[[i]]$k
          }

          return(fitSummary)
        }))

        rankCorrelationTable$setData(fitSummaries)

      }
    }
  }

  return()
}

.fpComputeFunnelDf              <- function(seSeq, mean, heterogeneity, funnelLevels) {
  dfs <- list()
  for (i in seq_along(funnelLevels)) {
    tempZ <- qnorm(funnelLevels[i], lower.tail = FALSE)
    dfs[[i]] <- data.frame(
      x = c(rev(mean - tempZ * sqrt(heterogeneity^2 + seSeq^2)), mean + tempZ * sqrt(heterogeneity^2 + seSeq^2)),
      y = c(rev(seSeq), seSeq),
      p = 2 * funnelLevels[i],
      lvl = 1 - 2 * funnelLevels[i]
    )
  }
  return(dfs)
}
.fpAsymmetryTestErrorMessage    <- function(level = NULL) {
  if (is.null(level))
    return(gettext("The funnel plot asymmetry test failed with the following error: "))
  else
    return(gettextf("The funnel plot asymmetry test at level %1$s failed with the following error: ", level))
}
.fpMetaforTranslateErrorMessage <- function(fit) {
  if (grepl("did not converge", fit))
    return(gettext("The meta-analytic model did not converge. Try modifying the 'Method' option."))
  else
    return(fit)
}
.fpExtractAsymmetryTest         <- function(fitTest, testType) {
  if (testType == "metaRegression") {
    return(data.frame(
      k   = if (jaspBase::isTryError(fitTest)) NA else fitTest$fit$k, # nobs will be fixed in the next release
      z   = if (jaspBase::isTryError(fitTest)) NA else fitTest$zval,
      p   = if (jaspBase::isTryError(fitTest)) NA else fitTest$pval,
      est = if (jaspBase::isTryError(fitTest)) NA else fitTest$est,
      lCI = if (jaspBase::isTryError(fitTest)) NA else fitTest$ci.lb,
      uCI = if (jaspBase::isTryError(fitTest)) NA else fitTest$ci.ub
    ))
  } else if (testType == "weightedRegression") {
    return(data.frame(
      k   = if (jaspBase::isTryError(fitTest)) NA else nobs(fitTest$fit),
      t   = if (jaspBase::isTryError(fitTest)) NA else fitTest$zval,
      df  = if (jaspBase::isTryError(fitTest)) NA else fitTest$dfs,
      p   = if (jaspBase::isTryError(fitTest)) NA else fitTest$pval,
      est = if (jaspBase::isTryError(fitTest)) NA else fitTest$est,
      lCI = if (jaspBase::isTryError(fitTest)) NA else fitTest$ci.lb,
      uCI = if (jaspBase::isTryError(fitTest)) NA else fitTest$ci.ub
    ))
  } else if (testType == "rankCorrelation") {
    return(data.frame(
      tau = if (jaspBase::isTryError(fitTest)) NA else fitTest$tau,
      p   = if (jaspBase::isTryError(fitTest)) NA else fitTest$pval
    ))
  }
}
.fpExtractFitEstimates          <- function(fit, options) {

  if (jaspBase::isTryError(fit)) {
    fitSummary <- data.frame(k = NA, muEst = NA, muLCI = NA, muUCI = NA, muP = NA)

    if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
      fitSummary$tauEst <- NA
      fitSummary$tauLCI <- NA
      fitSummary$tauUCI <- NA
      fitSummary$tauP   <- NA
    }

    return(fitSummary)
  }

  fitSummary <- data.frame(
    k        = fit$k,
    muEst    = fit$b[1],
    muLCI    = fit$ci.lb,
    muUCI    = fit$ci.ub,
    muP      = fit$pval
  )

  if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
    tempTau <- data.frame(confint(fit)$random)[2,]
    fitSummary$tauEst <- tempTau$estimate
    fitSummary$tauLCI <- tempTau$ci.lb
    fitSummary$tauUCI <- tempTau$ci.ub
    fitSummary$tauP   <- fit$QEp
  }

  return(fitSummary)
}
.fpExtractTrimAndFillEstimates  <- function(fit, options) {

  if (jaspBase::isTryError(fit)) {
    fitSummary <- data.frame(k = NA, missingK = NA, muEst = NA, muLCI = NA, muUCI = NA, muP = NA)

    if (options[["trimAndFillEstimator"]] == "R0")
      fitSummary$missingP <- NA

    if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
      fitSummary$tauEst <- NA
      fitSummary$tauLCI <- NA
      fitSummary$tauUCI <- NA
      fitSummary$tauP   <- NA
    }

    return(fitSummary)
  }

  fitSummary <- data.frame(
    k        = fit$k,
    missingK = fit$k0,
    muEst    = fit$b[1],
    muLCI    = fit$ci.lb,
    muUCI    = fit$ci.ub,
    muP      = fit$pval
  )

  if (options[["trimAndFillEstimator"]] == "R0") {
    fitSummary$missingP <- fit$p.k0
  }

  if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
    tempTau <- data.frame(confint(fit)$random)[2,]
    fitSummary$tauEst <- tempTau$estimate
    fitSummary$tauLCI <- tempTau$ci.lb
    fitSummary$tauUCI <- tempTau$ci.ub
    fitSummary$tauP   <- fit$QEp
  }

  return(fitSummary)
}
.fpGetTrimAndFillContainer      <- function(jaspResults) {

  if (is.null(jaspResults[["trimAndFillContainer"]])) {
    trimAndFillContainer <- createJaspContainer(title = gettext("Trim and Fill"))
    trimAndFillContainer$dependOn(c(
      .fpDependencies, "method", "trimAndFillEstimator", "trimAndFill"
    ))
    trimAndFillContainer$position <- 4
    jaspResults[["trimAndFillContainer"]] <- trimAndFillContainer
  } else {
    trimAndFillContainer <- jaspResults[["trimAndFillContainer"]]
  }

  return(trimAndFillContainer)
}

# compute power enhancement contours (lifted from zcurve)
.power_to_z       <- function(power, alpha = .05, a = stats::qnorm(alpha/2,lower.tail = FALSE), two.sided = TRUE, nleqslv_control = list(xtol = 1e-15, maxit = 300, stepmax = .5)){
  if(a  < 0)stop("a must be >= 0")
  if(is.null(a) & is.null(alpha))stop("Either 'alpha' or 'a' must be provided")
  if(is.null(alpha) & !is.null(a))alpha <- stats::pnorm(a, lower.tail = FALSE)*2
  if(alpha < 0 | alpha > 1)stop("alpha must be >= 0 & <= 1")
  if(!all(sapply(power, function(x)x >= alpha & x <= 1)))stop("power must be >= alpha & <= 1")
  sapply(power, function(pow)nleqslv::nleqslv(.5, .solve_power_to_z, power = pow, a = a, two.sided = two.sided, control = nleqslv_control)$x)
}
.solve_power_to_z <- function(x, power, a, two.sided){
  y = numeric(1)
  y = .z_to_power(z = x, a = a, two.sided = two.sided) - power
  y
}
.z_to_power       <- function(z, alpha = .05, a = stats::qnorm(alpha/2,lower.tail = FALSE), two.sided = TRUE){
  if(!all(sapply(z, function(x)x >= 0)))stop("z must be >= 0")
  if(a  < 0)stop("a must be >= 0")
  if(is.null(a) & is.null(alpha))stop("Either 'alpha' or 'a' must be provided")
  if(is.null(alpha) & !is.null(a))alpha <- stats::pnorm(a, lower.tail = FALSE)*2
  if(alpha < 0 | alpha > 1)stop("alpha must be >= 0 & <= 1")
  if(two.sided){
    return(1 - stats::pnorm(a, z, 1) + stats::pnorm(-a, z, 1))
  }else{
    return(1 - stats::pnorm(a, z, 1))
  }
}

# get the color scale
.getPowerEnhancementColors <- function(n) scales::gradient_n_pal(RColorBrewer::brewer.pal(n = 11, name = "RdYlGn"))(seq(0, 1, length.out = n))
