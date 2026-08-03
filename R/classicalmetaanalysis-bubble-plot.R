# Classical meta-analysis bubble plot.
#
# Builds bubble-plot data, confidence bands, geometry, and presentation.

# Output orchestration ----

.maBubblePlot                            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["bubblePlot"]]))
    return()

  if (length(options[["bubblePlotSelectedVariable"]]) == 0)
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    bubblePlot       <- .maBubblePlotFun(fit[[1]], options)
    bubblePlot$title <- gettext("Bubble Plots")
    bubblePlot$dependOn(c(.maBubblePlotDependencies, "includeFullDatasetInSubgroupAnalysis", if (.maIsClassical(options)) .maDependencies else .robmaDependencies))
    bubblePlot$position <- if (.maIsClassical(options)) 5 else 6
    jaspResults[["bubblePlot"]] <- bubblePlot
    return()

  } else {

    # create the output container
    bubblePlot       <- createJaspContainer()
    bubblePlot$title <- gettext("Bubble Plots")
    bubblePlot$dependOn(c(.maBubblePlotDependencies, "includeFullDatasetInSubgroupAnalysis", if (.maIsClassical(options)) .maDependencies else .robmaDependencies))
    bubblePlot$position <- if (.maIsClassical(options)) 5 else 6
    jaspResults[["bubblePlot"]] <- bubblePlot

    for (i in seq_along(fit)) {
      bubblePlot[[names(fit)[i]]]          <- .maBubblePlotFun(fit[[i]], options)
      bubblePlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      bubblePlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}

.maBubblePlotFun                         <- function(fit, options) {

  if (jaspBase::isTryError(fit)) {
    return()
  }

  # set dimensions
  width  <- if (length(options[["bubblePlotSeparateLines"]]) == 0 || options[["bubblePlotLegendPosition"]] == "none") 450 else 550
  height <- 350

  # create containers / figure
  if (length(options[["bubblePlotSeparatePlots"]]) > 0) {
    bubblePlot <- createJaspContainer()
  } else {
    bubblePlot <- createJaspPlot(width = width, height = height)
  }

  if (.maIsClassical(options)) {
    dfPlot <- .maMakeBubblePlotDataset(fit, options)
  } else {
    dfPlot <- .robmaMakeBubblePlotDataset(fit, options)
  }


  if (attr(dfPlot, "separatePlots") == "") {
    tempPlots <- list(.maMakeBubblePlot(fit, options, dfPlot))
  } else {
    tempPlots <- lapply(unique(dfPlot[["separatePlots"]]), function(lvl) {
      .maMakeBubblePlot(fit, options, dfPlot[dfPlot[["separatePlots"]] == lvl,], separatePlotsLvl = lvl)
    })
  }

  # modify all generated plots simultaneously
  yRange <- do.call(rbind, lapply(tempPlots, attr, which = "yRange"))
  yRange <- c(min(yRange[, 1]), max(yRange[, 2]))
  yRange <- range(jaspGraphs::getPrettyAxisBreaks(yRange))

  tempPlots <- lapply(tempPlots, function(plot) {
    .maAddBubblePlotTheme(plot, options, dfPlot, yRange)
  })

  if (length(options[["bubblePlotSeparatePlots"]]) > 0) {
    for (i in seq_along(tempPlots)) {
      tempBubblePlot <- createJaspPlot(title = gettextf("%1$s (%2$s)", attr(dfPlot, "separatePlots"), unique(dfPlot[["separatePlots"]])[i]), width = width, height = height)
      tempBubblePlot$position      <- i
      tempBubblePlot$plotObject    <- tempPlots[[i]]
      bubblePlot[[paste0("plot", i)]] <- tempBubblePlot
    }
  } else {
    bubblePlot$plotObject <- tempPlots[[1]]
  }


  return(bubblePlot)
}

# Plot data ----

.maMakeBubblePlotDataset           <- function(fit, options) {

  # extract options
  separateLines        <- unlist(options[["bubblePlotSeparateLines"]])
  separatePlots        <- unlist(options[["bubblePlotSeparatePlots"]])
  selectedVariable     <- options[["bubblePlotSelectedVariable"]][[1]][["variable"]]
  selectedVariableType <- options[["predictors.types"]][options[["predictors"]] == selectedVariable]
  dataset              <- attr(fit, "dataset")

  # create a range of values for continuous predictors to plot the trend but use lvls for factors
  if (selectedVariableType == "scale") {

    xRange <- range(jaspGraphs::getPrettyAxisBreaks(range(dataset[[selectedVariable]])))
    trendSequence <- seq(xRange[1], xRange[2], length.out =  101)

    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      selectedVariables = c(separateLines, separatePlots),
      sdFactor          = options[["bubblePlotSdFactorCovariates"]],
      trendVarible      = selectedVariable,
      trendSequence     = trendSequence,
      parameter         = "effectSize"
    )

  } else if (selectedVariableType == "nominal") {

    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      selectedVariables = c(selectedVariable, separateLines, separatePlots),
      sdFactor          = options[["bubblePlotSdFactorCovariates"]],
      parameter         = "effectSize"
    )

  }


  if (.maIsMetaregressionHeterogeneity(options)) {

    if (selectedVariableType == "scale") {

      xRange <- range(jaspGraphs::getPrettyAxisBreaks(range(dataset[[selectedVariable]])))
      trendSequence <- seq(xRange[1], xRange[2], length.out =  101)

      predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
        fit               = fit,
        options           = options,
        selectedVariables = c(separateLines, separatePlots),
        sdFactor          = options[["bubblePlotSdFactorCovariates"]],
        trendVarible      = selectedVariable,
        trendSequence     = trendSequence,
        parameter         = "heterogeneity"
      )

    } else if (selectedVariableType == "nominal") {

      predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
        fit               = fit,
        options           = options,
        selectedVariables = c(selectedVariable, separateLines, separatePlots),
        sdFactor          = options[["bubblePlotSdFactorCovariates"]],
        parameter         = "heterogeneity"
      )

    }

    computedMarginalMeans <- predict(
      fit,
      newmods  = predictorMatrixEffectSize,
      newscale = predictorMatrixHeterogeneity,
      level    = 100 * options[["confidenceIntervalsLevel"]]
    )
  } else {

    computedMarginalMeans <- predict(
      fit,
      newmods = predictorMatrixEffectSize,
      level   = 100 * options[["confidenceIntervalsLevel"]]
    )
  }

  ### modify and rename selectedGrid
  selectedGrid <- attr(predictorMatrixEffectSize, "selectedGrid")
  selectedGrid$selectedVariable <- selectedGrid[,selectedVariable]
  # deal with continuous variables dichotomization
  selectedGrid     <- .maDichotomizeVariablesLevels(selectedGrid, c(separateLines, separatePlots), options)
  continuousLevels <- attr(selectedGrid, "continuousLevels")
  # collapse factor levels if multiple selected
  selectedGrid <- .maMergeVariablesLevels(selectedGrid, separateLines, "separateLines")
  selectedGrid <- .maMergeVariablesLevels(selectedGrid, separatePlots, "separatePlots")
  # remove original names
  selectedGrid <- selectedGrid[,setdiff(names(selectedGrid), c(selectedVariable, separateLines, separatePlots)),drop = FALSE]

  ### modify marginal means
  computedMarginalMeans <- .maExtractAndFormatPrediction(computedMarginalMeans)

  ### merge and add attributes
  dfPlot <- cbind.data.frame(selectedGrid, computedMarginalMeans)

  attr(dfPlot, "selectedVariable")     <- selectedVariable
  attr(dfPlot, "selectedVariableType") <- selectedVariableType
  attr(dfPlot, "separateLines")    <- paste(separateLines, collapse = " | ")
  attr(dfPlot, "separatePlots")    <- paste(separatePlots, collapse = " | ")
  attr(dfPlot, "variablesLines")   <- separateLines
  attr(dfPlot, "variablesPlots")   <- separatePlots
  attr(dfPlot, "continuousLevels") <- continuousLevels[!sapply(continuousLevels, is.null)]
  attr(dfPlot, "xRange")           <- if (selectedVariableType == "scale") xRange

  return(dfPlot)
}

# Plot rendering ----

.maMakeBubblePlot                  <- function(fit, options, dfPlot, separatePlotsLvl = NULL) {

  bubblePlot <- ggplot2::ggplot()
  yRange     <- NULL

  hasSeparateLines <- attr(dfPlot, "separateLines") != ""
  hasSeparatePlots <- attr(dfPlot, "separatePlots") != ""

  ### add prediction bands
  if (options[["bubblePlotPredictionIntervals"]]) {

    geomPi <- .maBubblePlotMakeCiGeom(dfPlot, options, ci = FALSE)

    if (!is.null(geomPi)) {
      bubblePlot <- bubblePlot + do.call(geomPi$what, geomPi$args)
      yRange     <- attr(geomPi, "yRange")
    } else {
      yRange     <- NA
    }

  }

  ### add confidence bands
  if (options[["bubblePlotConfidenceIntervals"]]) {

    geomCi <- .maBubblePlotMakeCiGeom(dfPlot, options, ci = TRUE)

    if (!is.null(geomCi)) {
      bubblePlot <- bubblePlot + do.call(geomCi$what, geomCi$args)
      yRange     <- range(c(yRange, attr(geomCi, "yRange")), na.rm = TRUE)
    }

  }

  ### add prediction line
  if (attr(dfPlot, "selectedVariableType") == "scale") {
    aesCall <- list(
      x     = as.name("selectedVariable"),
      y     = as.name("est"),
      color = if (hasSeparateLines) as.name("separateLines")
    )
    dfPlot[["est"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfPlot[["est"]]))
    geomCall <- list(
      data    = dfPlot,
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)])
    )
    bubblePlot <- bubblePlot + do.call(jaspGraphs::geom_line, geomCall)
    yRange <- range(c(yRange, dfPlot$pred), na.rm = TRUE)
  }

  ### add studies as bubbles
  dataset <- attr(fit, "dataset")
  if (.maIsGLMM(options)) {
    tempDf    <- .maglmmEscalc(dataset, options)
    dfStudies <- data.frame(
      effectSize        = tempDf[["yi"]],
      inverseVariance   = 1/tempDf[["vi"]],
      sampleSize        = .maglmmGetSampleSize(dataset, options),
      constant          = rep(options[["bubblePlotBubblesRelativeSize"]], nrow(dataset)),
      selectedVariable  = dataset[[attr(dfPlot, "selectedVariable")]]
    )
  } else if (options[["analysis"]] == "BiBMA") {
    tempDf <- metafor::escalc(
      measure = "OR",
      ai      = dataset[[options[["successesGroup1"]]]],
      n1i     = dataset[[options[["sampleSizeGroup1"]]]],
      ci      = dataset[[options[["successesGroup2"]]]],
      n2i     = dataset[[options[["sampleSizeGroup2"]]]]
    )
    dfStudies <- data.frame(
      effectSize        = tempDf[["yi"]],
      inverseVariance   = 1/tempDf[["vi"]],
      sampleSize        = (dataset[[options[["sampleSizeGroup1"]]]] + dataset[[options[["sampleSizeGroup2"]]]]),
      weight            = if (.maIsClassical(options)) weights(fit) else NA,
      constant          = rep(options[["bubblePlotBubblesRelativeSize"]], nrow(dataset)),
      selectedVariable  = dataset[[attr(dfPlot, "selectedVariable")]]
    )
  } else {
    dfStudies <- data.frame(
      effectSize        = dataset[[options[["effectSize"]]]],
      inverseVariance   = 1/dataset[[options[["effectSizeStandardError"]]]]^2,
      weight            = if (.maIsClassical(options)) weights(fit) else NA,
      constant          = rep(options[["bubblePlotBubblesRelativeSize"]], nrow(dataset)),
      selectedVariable  = dataset[[attr(dfPlot, "selectedVariable")]]
    )
  }


  # add separate lines and plots
  if (hasSeparateLines)
    dfStudies[attr(dfPlot, "variablesLines")] <- dataset[attr(dfPlot, "variablesLines")]
  if (hasSeparatePlots)
    dfStudies[attr(dfPlot, "variablesPlots")] <- dataset[attr(dfPlot, "variablesPlots")]

  # make same encoding
  dfStudies <- .maDichotomizeVariablesDataset(dfStudies, c(attr(dfPlot, "variablesLines"), attr(dfPlot, "variablesPlots")), attr(dfPlot, "continuousLevels"), options)
  dfStudies <- .maMergeVariablesLevels(dfStudies, attr(dfPlot, "variablesLines"), "separateLines")
  dfStudies <- .maMergeVariablesLevels(dfStudies, attr(dfPlot, "variablesPlots"), "separatePlots")
  if (hasSeparateLines)
    levels(dfStudies[,"separateLines"]) <- levels(dfPlot[,"separateLines"])

  # subset original data across plots
  if (!is.null(separatePlotsLvl))
    dfStudies <- dfStudies[dfStudies$separatePlots == separatePlotsLvl,]

  aesCall <- list(
    x     = as.name("selectedVariable"),
    y     = as.name("effectSize"),
    size  = switch(
      options[["bubblePlotBubblesSize"]],
      "weight"            = as.name("weight"),
      "inverseVariance"   = as.name("inverseVariance"),
      "sampleSize"        = as.name("sampleSize"),
      "equal"             = as.name("constant")
    ),
    color = if (hasSeparateLines) as.name("separateLines"),
    fill  = if (hasSeparateLines) as.name("separateLines"),
    alpha = options[["bubblePlotBubblesTransparency"]]
  )

  dfStudies[["effectSize"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfStudies[["effectSize"]]))

  geomCall <- list(
    data    = dfStudies,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
    show.legend = FALSE
  )
  if (attr(dfPlot, "selectedVariableType") == "nominal" && hasSeparateLines) {
    geomCall$position <- ggplot2::position_jitterdodge(
      jitter.width  = 0.35 * options[["bubblePlotBubblesJitter"]],
      jitter.height = 0,
      dodge.width   = 0.9
    )
  }else if (attr(dfPlot, "selectedVariableType") == "nominal") {
    geomCall$position <- ggplot2::position_jitter(
      width       = 0.35 * options[["bubblePlotBubblesJitter"]],
      height      = 0
    )
  }

  if (nrow(dfStudies) > 0) {
    bubblePlot <- bubblePlot + do.call(jaspGraphs::geom_point, geomCall) +
      ggplot2::scale_size(range = c(1.5, 10) * options[["bubblePlotBubblesRelativeSize"]])
    yRange     <- range(c(yRange, dfStudies[["effectSize"]]))
  }

  # add color palette
  bubblePlot <- bubblePlot +
    jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]]) +
    jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])

  attr(bubblePlot, "yRange") <- yRange
  return(bubblePlot)
}

.maAddBubblePlotTheme              <- function(plot, options, dfPlot, yRange) {


  selectedVariableType <- attr(dfPlot, "selectedVariableType")

  if (selectedVariableType == "scale") {
    plot <- plot +
      jaspGraphs::scale_x_continuous(
        name   = attr(dfPlot, "selectedVariable"),
        breaks = jaspGraphs::getPrettyAxisBreaks(attr(dfPlot, "xRange")),
        limits = attr(dfPlot, "xRange")
      )
  } else if (selectedVariableType == "nominal") {
    plot <- plot +
      ggplot2::scale_x_discrete(
        name   = attr(dfPlot, "selectedVariable")
      )
  }

  plot <- plot +
    jaspGraphs::scale_y_continuous(
      name   = if (options[["transformEffectSize"]] == "none") gettext("Effect Size") else .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]]),
      breaks = jaspGraphs::getPrettyAxisBreaks(yRange),
      limits = yRange
    )

  if (attr(dfPlot, "separateLines") != "")
    plot <- plot + ggplot2::labs(fill = attr(dfPlot, "separateLines"), color = attr(dfPlot, "separateLines"))

  if (options[["bubblePlotTheme"]] == "jasp") {

    plot <- plot +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = if (attr(dfPlot, "separateLines") == "") "none" else options[["bubblePlotLegendPosition"]])

  } else {

    plot <- plot +
      switch(
        options[["bubblePlotTheme"]],
        "whiteBackground" = ggplot2::theme_bw()       + ggplot2::theme(legend.position = "bottom"),
        "light"           = ggplot2::theme_light()    + ggplot2::theme(legend.position = "bottom"),
        "minimal"         = ggplot2::theme_minimal()  + ggplot2::theme(legend.position = "bottom"),
        "pubr"            = jaspGraphs::themePubrRaw(legend = options[["bubblePlotLegendPosition"]]),
        "apa"             = jaspGraphs::themeApaRaw(legend.pos = switch(
          options[["bubblePlotLegendPosition"]],
          "none"   = "none",
          "bottom" = "bottommiddle",
          "right"  = "bottomright",
          "top"    = "topmiddle",
          "left"   = "bottomleft"
        ))
      )

    plot <- plot + ggplot2::theme(
      legend.text  = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      legend.title = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      axis.text    = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      axis.title   = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      legend.position = if (attr(dfPlot, "separateLines") == "") "none" else options[["bubblePlotLegendPosition"]])
  }

  return(plot)
}

# Confidence geometry ----

.maBubblePlotMakeCiGeom            <- function(dfPlot, options, ci = TRUE) {

  hasSeparateLines     <- attr(dfPlot, "separateLines") != ""
  hasSeparatePlots     <- attr(dfPlot, "separatePlots") != ""
  selectedVariableType <- attr(dfPlot, "selectedVariableType")

  aesCall <- list(
    x      = as.name("selectedVariable"),
    fill   = if (hasSeparateLines) as.name("separateLines"),
    group  = if (hasSeparateLines && selectedVariableType == "scale") as.name("separateLines")
  )

  if (selectedVariableType == "scale") {
    aesCall$y      <- as.name("y")
  } else if (selectedVariableType == "nominal") {
    aesCall$lower   <- as.name("lower")
    aesCall$upper   <- as.name("upper")
    aesCall$ymin    <- as.name("lower")
    aesCall$ymax    <- as.name("upper")
    aesCall$middle  <- as.name("middle")
  }

  dfBands <-  .maBubblePlotMakeConfidenceBands(
    dfPlot,
    lCi = if (ci) "lCi" else "lPi",
    uCi = if (ci) "uCi" else "uPi"
  )

  if (selectedVariableType == "scale") {
    dfBands[["y"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfBands[["y"]]))
  } else if (selectedVariableType == "nominal") {
    dfBands[,c("lower","middle","upper")]  <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(dfBands[,c("lower","middle","upper")]))
  }

  geomCall <- list(
    data    = dfBands,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
    alpha   = options[["bubblePlotPredictionIntervalsTransparency"]]
  )

  if (selectedVariableType == "nominal") {
    geomCall$stat     <- "identity"
    geomCall$position <- ggplot2::position_dodge2(width = 0.9)
    if (!hasSeparateLines)
      geomCall$fill <- "grey"
  }


  if (selectedVariableType == "scale" && any(!is.na(dfBands[["y"]]))) {
    geom <- list(
      what = ggplot2::geom_polygon,
      args = geomCall
    )
    attr(geom, "yRange") <- range(c(dfBands$y))
  } else if (selectedVariableType == "nominal" && any(!is.na(dfBands[["lower"]]))) {
    geom <- list(
      what = ggplot2::geom_boxplot,
      args = geomCall
    )
    attr(geom, "yRange") <- range(c(dfBands$lower, dfBands$upper))
  } else {
    geom <- NULL
  }

  return(geom)
}

.maBubblePlotMakeConfidenceBands      <- function(dfPlot, lCi = "lCi", uCi = "uCi") {

  if (attr(dfPlot, "selectedVariableType") == "scale") {

    if (!is.null(dfPlot[["separateLines"]])) {

      dfBands <- do.call(rbind, lapply(unique(dfPlot[["separateLines"]]), function(lvl) {
        dfSubset  <- dfPlot[dfPlot[["separateLines"]] == lvl,]
        dfPolygon <- data.frame(
          selectedVariable  = c(dfSubset$selectedVariable, rev(dfSubset$selectedVariable)),
          y                 = c(dfSubset[[lCi]],           rev(dfSubset[[uCi]]))
        )
        dfPolygon$separateLines <- lvl
        return(dfPolygon)
      }))

    } else {

      dfBands <- data.frame(
        selectedVariable = c(dfPlot$selectedVariable, rev(dfPlot$selectedVariable)),
        y                = c(dfPlot[[lCi]],           rev(dfPlot[[uCi]]))
      )

    }

  } else {

    dfBands <- data.frame(
      lower            = dfPlot[[lCi]],
      upper            = dfPlot[[uCi]],
      middle           = dfPlot[["est"]],
      selectedVariable = dfPlot[["selectedVariable"]]
    )

    if (!is.null(dfPlot[["separateLines"]]))
      dfBands$separateLines <- dfPlot[["separateLines"]]

  }

  return(dfBands)
}
