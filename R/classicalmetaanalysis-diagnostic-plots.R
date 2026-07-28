# Classical meta-analysis diagnostic figures.
#
# Builds profile-likelihood, Baujat, and residual-funnel plots.

# Profile-likelihood plot ----

.maProfileLikelihoodPlot                 <- function(jaspResults, options) {

  if (!is.null(jaspResults[["profileLikelihoodPlot"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # extract precomputed profile likelihoods if done before:
  dfProfile <- .maProfile(jaspResults, options)

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    profileLikelihoodPlot       <- .maProfileLikelihoodPlotFun(fit[[1]], dfProfile[[1]], options)
    profileLikelihoodPlot$title <- gettext("Profile Likelihood Plot")
    profileLikelihoodPlot$dependOn(c(.maDependencies, "diagnosticsPlotsProfileLikelihood", "includeFullDatasetInSubgroupAnalysis"))
    profileLikelihoodPlot$position <- 8
    jaspResults[["profileLikelihoodPlot"]] <- profileLikelihoodPlot
    return()

  } else {

    # create the output container
    profileLikelihoodPlot       <- createJaspContainer()
    profileLikelihoodPlot$title <- gettext("Profile Likelihood Plot")
    profileLikelihoodPlot$dependOn(c(.maDependencies, "diagnosticsPlotsProfileLikelihood", "includeFullDatasetInSubgroupAnalysis"))
    profileLikelihoodPlot$position <- 8
    jaspResults[["profileLikelihoodPlot"]] <- profileLikelihoodPlot

    for (i in seq_along(fit)) {
      profileLikelihoodPlot[[names(fit)[i]]]          <- .maProfileLikelihoodPlotFun(fit[[i]], dfProfile[[i]], options)
      profileLikelihoodPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      profileLikelihoodPlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}

.maProfileLikelihoodPlotFun              <- function(fit, dfProfile, options) {

  # create profile likelihood plot / container
  if (.maIsMultilevelMultivariate(options)) {

    # container for multivariate
    profileLikelihoodPlot <- createJaspContainer()

    # error plot
    if (jaspBase::isTryError(dfProfile)) {
      errorPlot <- createJaspPlot(title = gettext("Profile Likelihood Plot"))
      errorPlot$setError(dfProfile)
      profileLikelihoodPlot[["errorPlot"]] <- errorPlot
      return(profileLikelihoodPlot)
    }
    if (length(dfProfile) == 0) {
      return()
    }

    # component specific plots
    for (i in 1:dfProfile[["comps"]]) {
      tempProfilePlot <- createJaspPlot(title = paste0(dfProfile[[i]][["title"]][-1], collapse = " "), width = 400, height = 320)
      tempProfilePlot$position <- i
      profileLikelihoodPlot[[paste0("plot", i)]] <- tempProfilePlot
      tempProfilePlot$plotObject <- .maMakeProfileLikelihoodPlot(dfProfile[[i]])
    }

  } else {

    # plot for univariate
    profileLikelihoodPlot <- createJaspPlot(width = 400, height = 320)

    if (.maIsMetaregressionHeterogeneity(options)) {
      profileLikelihoodPlot$setError(gettext("Profile likelihood is not available for models that contain meta-regression on heterogeneity."))
      return(profileLikelihoodPlot)
    }
    if (jaspBase::isTryError(dfProfile)) {
      profileLikelihoodPlot$setError(dfProfile)
      return(profileLikelihoodPlot)
    }
    if (length(dfProfile) == 0) {
      return()
    }

    profileLikelihoodPlot$plotObject <- .maMakeProfileLikelihoodPlot(dfProfile)
  }

  return(profileLikelihoodPlot)
}

# Baujat plot ----

.maBaujatPlot                            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["baujatPlot"]]))
    return()

  fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # extract precomputed baujat plot if done before:
  dfBaujat <- .maBaujat(jaspResults, options)

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    baujatPlot       <- .maBaujatPlotFun(dfBaujat[[names(fit)[1]]], options, fit[[1]])
    baujatPlot$title <- gettext("Baujat Plot")
    baujatPlot$dependOn(c(.maDependencies, "diagnosticsPlotsBaujat", "includeFullDatasetInSubgroupAnalysis", "studyLabels"))
    baujatPlot$position <- 9
    jaspResults[["baujatPlot"]] <- baujatPlot
    return()

  } else {

    # create the output container
    baujatPlot       <- createJaspContainer()
    baujatPlot$title <- gettext("Baujat Plot")
    baujatPlot$dependOn(c(.maDependencies, "diagnosticsPlotsBaujat", "includeFullDatasetInSubgroupAnalysis", "studyLabels"))
    baujatPlot$position <- 9
    jaspResults[["baujatPlot"]] <- baujatPlot

    for (i in seq_along(fit)) {
      baujatPlot[[names(fit)[i]]]          <- .maBaujatPlotFun(dfBaujat[[names(fit)[i]]], options, fit[[i]])
      baujatPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      baujatPlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}

.maBaujatPlotFun                         <- function(dfBaujat, options, fit) {

  baujatPlot <- createJaspPlot(width = 400, height = 320)

  # error handling
  if (.maIsMetaregressionHeterogeneity(options)) {
    baujatPlot$setError(gettext("Baujat plot is not available for models that contain meta-regression on heterogeneity."))
    return(baujatPlot)
  }
  if (jaspBase::isTryError(dfBaujat)) {
    baujatPlot$setError(dfBaujat)
    return(baujatPlot)
  }
  if (length(dfBaujat) == 0) {
    return()
  }

  if (options[["studyLabels"]] != "") {
    fitData <- attr(fit, "dataset")
    if (!is.null(fitData) && nrow(fitData) == nrow(dfBaujat)) {
      dfBaujat$label <- as.character(fitData[[options[["studyLabels"]]]])
      dfBaujat$label[is.na(dfBaujat$label)] <- ""
    }
  }
  hasStudyLabels <- "label" %in% colnames(dfBaujat)

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(dfBaujat$x, na.rm = TRUE))
  yTicks <- jaspGraphs::getPrettyAxisBreaks(range(dfBaujat$y, na.rm = TRUE))

  aesCall <- list(
    x     = as.name("x"),
    y     = as.name("y")
  )
  geomCall <- list(
    data    = dfBaujat,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)])
  )

  # create plot
  plotOut <- do.call(ggplot2::ggplot, geomCall) +
    jaspGraphs::geom_point(
      size = if (hasStudyLabels) 2 else 3
    )

  if (hasStudyLabels)
    plotOut <- plotOut + ggplot2::geom_text(
      data        = dfBaujat,
      mapping     = ggplot2::aes(x = x, y = y, label = label),
      hjust       = 0,
      vjust       = 0,
      inherit.aes = FALSE
    )

  plotOut <- plotOut +
    ggplot2::labs(x = gettext("Squared Pearson Residual"), y = gettext("Influence on Fitted Value")) +
    jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks)) +
    jaspGraphs::scale_y_continuous(breaks = yTicks, limits = range(yTicks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  baujatPlot$plotObject <- plotOut

  return(baujatPlot)
}

# Residual-funnel plot ----

.maResidualFunnelPlot                    <- function(jaspResults, options) {

  if (!is.null(jaspResults[["residualFunnelPlot"]]))
    return()

  # rstandard() is not available for rma.glmm
  if (.maIsGLMM(options))
    return()

  fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    residualFunnelPlot       <- .maResidualFunnelPlotFun(fit[[1]], options)
    residualFunnelPlot$title <- gettext("Residual Funnel Plot")
    residualFunnelPlot$dependOn(c(.maDependencies, "diagnosticsResidualFunnel", "studyLabels", "includeFullDatasetInSubgroupAnalysis"))
    residualFunnelPlot$position <- 10
    jaspResults[["residualFunnelPlot"]] <- residualFunnelPlot
    return()

  } else {

    # create the output container
    residualFunnelPlot       <- createJaspContainer()
    residualFunnelPlot$title <- gettext("Residual Funnel Plot")
    residualFunnelPlot$dependOn(c(.maDependencies, "diagnosticsResidualFunnel", "studyLabels", "includeFullDatasetInSubgroupAnalysis"))
    residualFunnelPlot$position <- 10
    jaspResults[["residualFunnelPlot"]] <- residualFunnelPlot

    for (i in seq_along(fit)) {
      residualFunnelPlot[[names(fit)[i]]]          <- .maResidualFunnelPlotFun(fit[[i]], options)
      residualFunnelPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      residualFunnelPlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}

.maResidualFunnelPlotFun                 <- function(fit, options) {

  # create plot
  residualFunnelPlot <- createJaspPlot(width = 400, height = 320)

  if (jaspBase::isTryError(fit)) {
    return()
  }

  # obtain residual funnel plot
  residualFunnelPlot$plotObject <- .maMakeResidualFunnelPlot(fit, options)

  return(residualFunnelPlot)
}

# Plot constructors ----

.maMakeResidualFunnelPlot          <- function(fit, options) {

  residuals <- rstandard(fit)
  dataset   <- attr(fit, "dataset")
  dfPlot    <- data.frame(
    x  = residuals[["resid"]],
    y  = residuals[["se"]]
  )

  yTicks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(dfPlot$y)))

  dfFunnel <- data.frame(
    x = c(-max(yTicks), 0, max(yTicks)) * 1.96,
    y = c(max(yTicks),  0, max(yTicks))
  )
  dfFunnelEdge1 <- dfFunnel[1:2,]
  dfFunnelEdge2 <- dfFunnel[2:3,]

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(dfPlot$x, dfFunnel$x)))

  dfBackground <- data.frame(
    x = c(min(xTicks), max(xTicks), max(xTicks), min(xTicks)),
    y = c(min(yTicks), min(yTicks), max(yTicks), max(yTicks))
  )

  out <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data    = dfBackground,
      mapping = ggplot2::aes(x = x, y = y),
      fill    = "grey",
    ) +
    ggplot2::geom_polygon(
      data    = dfFunnel,
      mapping = ggplot2::aes(x = x, y = y),
      fill    = "white",
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = c(0, 0),
        y = range(yTicks)
      ), linetype = "dotted"
    ) +
    ggplot2::geom_line(
      data    = dfFunnelEdge1,
      mapping = ggplot2::aes(x = x, y = y), linetype = "dotted"
    ) +
    ggplot2::geom_line(
      data    = dfFunnelEdge2,
      mapping = ggplot2::aes(x = x, y = y), linetype = "dotted"
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = c(0, 0),
        y = range(yTicks)
      ), linetype = "dotted"
    ) +
    jaspGraphs::geom_point(
      data    = dfPlot,
      mapping = ggplot2::aes(x = x, y = y),
      fill    = "black"
    )

  # add labels if specified
  if (options[["studyLabels"]] != "") {

    dfLabels <- cbind(
      dfPlot,
      label = dataset[[options[["studyLabels"]]]]
    )
    dfLabels <- dfLabels[abs(dfLabels$y * 1.96) < abs(dfLabels$x),]
    dfLabels$position <- ifelse(dfLabels$x < 0, "right", "left")
    dfLabels$nudge_x  <- ifelse(dfLabels$x < 0, -0.1, 0.1)

    out <- out +
      ggplot2::geom_text(
        data    = dfLabels,
        mapping = ggplot2::aes(x = x, y = y, label = label, hjust = position), nudge_x = dfLabels$nudge_x
      )
  }

  out <- out +
    jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks), name = gettext("Residual Value")) +
    ggplot2::scale_y_reverse(breaks = rev(yTicks), limits = rev(range(yTicks)), name = gettext("Standard Error")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(out)
}

.maMakeProfileLikelihoodPlot       <- function(dfPlot) {

  yTicks <- jaspGraphs::getPrettyAxisBreaks(c(min(dfPlot$ll), max(dfPlot$ll)))

  # xTicks and other attributes only passed for rma.uni
  # (there are way too many options to deal with for rma.mv --- using the metafor package defaults)
  if (!is.null(attr(dfPlot, "xTicks"))) {
    xTicks <- attr(dfPlot, "xTicks")
  } else {
    xTicks <- jaspGraphs::getPrettyAxisBreaks(c(min(dfPlot[[1]]), max(dfPlot[[1]])))
  }

  # create plot
  plotOut <- ggplot2::ggplot(
    data    = data.frame(x = dfPlot[[1]], y = dfPlot[["ll"]]),
    mapping = ggplot2::aes(x = x, y = y)
  ) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point()

  plotOut <- plotOut +
    ggplot2::geom_line(
      data = data.frame(
        x = rep(dfPlot[["vc"]], 2),
        y = range(yTicks)),
      linetype = "dotted") +
    ggplot2::geom_line(
      data = data.frame(
        x = range(xTicks),
        y = rep(max(dfPlot[["maxll"]]), 2)),
      linetype = "dotted")

  plotOut <- plotOut +
    ggplot2::labs(x = dfPlot[["xlab"]], y = gettext("Profile Likelihood")) +
    jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks)) +
    jaspGraphs::scale_y_continuous(breaks = yTicks, limits = range(yTicks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotOut)
}
