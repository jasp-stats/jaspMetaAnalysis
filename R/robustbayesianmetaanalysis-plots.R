# Robust Bayesian meta-analysis figures.
#
# Builds prior and posterior figures.

.robmaPriorAndPosteriorPlot              <- function(jaspResults, options, parameter) {

  # section container
  priorAndPosteriorPlotContainer <- .robmaExtractPriorAndPosteriorPlotContainer(jaspResults)

  # plot container
  if (!is.null(priorAndPosteriorPlotContainer[[parameter]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  # dispatch options
  if (parameter == "pooledEffect") {
    tempTitle        <- if (.maIsMetaregression(options)) gettext("Adjusted Effect") else gettext("Pooled Effect")
    tempPosition     <- 1
    tempFunction     <- .robmaPriorAndPosteriorPlotEstimateFun
    tempDependencies <- "priorAndPosteriorPlotEffectSize"
  } else if (parameter == "heterogeneity") {
    tempTitle        <- gettext("Heterogeneity")
    tempPosition     <- 2
    tempFunction     <- .robmaPriorAndPosteriorPlotEstimateFun
    tempDependencies <- "priorAndPosteriorPlotHeterogeneity"
  } else if (parameter == "moderation") {
    tempTitle        <- if (options[["priorAndPosteriorPlotModerationEstimatedMarginalMeans"]]) gettext("Moderation: Estimated Marginal Means") else gettext("Moderation")
    tempPosition     <- 3
    tempFunction     <- .robmaPriorAndPosteriorPlotModerationFun
    tempDependencies <- c("priorAndPosteriorPlotModeration", "priorAndPosteriorPlotModerationEstimatedMarginalMeans")
    fit              <- list(fit) # trick to forward the fit to a futher dispatch later
  } else if (parameter == "weightFunction") {
    tempTitle        <- gettext("Weight Function")
    tempPosition     <- 4
    tempFunction     <- .robmaPriorAndPosteriorPlotEstimateFun
    tempDependencies <- c("priorAndPosteriorPlotWeightFunction", "priorAndPosteriorPlotWeightFunctionRescaleXAxis")
  } else if (parameter == "petPeese") {
    tempTitle        <- gettext("PET-PEESE")
    tempPosition     <- 5
    tempFunction     <- .robmaPriorAndPosteriorPlotEstimateFun
    tempDependencies <- "priorAndPosteriorPlotPetPeese"
  }

  if (options[["priorAndPosteriorPlotType"]] == "conditional")
    tempTitle <- gettextf("Conditional %1$s", tempTitle)


  # create individual plots for each subgroup
  if (options[["subgroup"]] == "" || parameter == "moderation") {

    tempPlot       <- do.call(tempFunction, list(fit = fit[[1]], options = options, parameter = parameter))
    tempPlot$title <- tempTitle
    tempPlot$dependOn(c(.robmaDependencies, tempDependencies))
    tempPlot$position <- tempPosition
    priorAndPosteriorPlotContainer[[parameter]] <- tempPlot

  } else {

    # create the output container
    tempPlot       <- createJaspContainer()
    tempPlot$title <- tempTitle
    tempPlot$dependOn(c(.robmaDependencies, tempDependencies))
    tempPlot$position <- tempPosition
    priorAndPosteriorPlotContainer[[parameter]] <- tempPlot

    for (i in seq_along(fit)) {
      tempPlot[[names(fit)[i]]]          <- do.call(tempFunction, list(fit = fit[[i]], options = options, parameter = parameter))
      tempPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      tempPlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}

.robmaPriorAndPosteriorPlotEstimateFun   <- function(fit, options, parameter) {

  # create plot
  tempPlot <- createJaspPlot(
    width  = if (!parameter %in% c("pooledEffect", "heterogeneity")) 500 else 400,
    height = 320)

  if (jaspBase::isTryError(fit)) {
    return()
  }

  if (!parameter %in% c("pooledEffect", "heterogeneity", "weightFunction", "petPeese") &&
      options[["priorAndPosteriorPlotModerationEstimatedMarginalMeans"]]) {
    p <- try(RoBMA::marginal_plot(
      fit,
      parameter    = parameter,
      prior        = options[["priorAndPosteriorPlotIncludePriorDistribution"]],
      conditional  = options[["priorAndPosteriorPlotType"]] == "conditional",
      plot_type    = "ggplot",
      xlab         = parameter
    ))
  } else {
    p <- try(plot(
      fit,
      parameter    = switch(
        parameter,
        "pooledEffect"   = "mu",
        "heterogeneity"  = "tau",
        "weightFunction" = "weightfunction",
        "petPeese"       = "petpeese",
        parameter
      ),
      prior        = options[["priorAndPosteriorPlotIncludePriorDistribution"]],
      conditional  = options[["priorAndPosteriorPlotType"]] == "conditional",
      rescale_x    = options[["priorAndPosteriorPlotWeightFunctionRescaleXAxis"]],
      plot_type    = "ggplot",
      xlab         = switch(
        parameter,
        "pooledEffect"   = if (.maIsMetaregression(options)) gettext("Adjusted Effect") else gettext("Pooled Effect"),
        "heterogeneity"  = gettext("Heterogeneity"),
        "weightFunction" = gettext("P-Value"),
        "petPeese"       = gettext("Standard Error"),
        parameter
      ),
      ylab         = if (parameter == "petPeese") gettext("Effect Size")
    ))
  }



  if (jaspBase::isTryError(p)) {
    tempPlot$setError(p)
    return(tempPlot)
  }

  if (attr(p, "sec_axis"))
    p <- p + jaspGraphs::geom_rangeframe(sides = "blr") + jaspGraphs::themeJaspRaw(legend.position = "right", legend.title = ggplot2::element_blank()) + ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(vjust = 3.25),
      plot.margin        = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))
  else
    p <- p + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw(legend.position = "right", legend.title = ggplot2::element_blank())

  # obtain residual funnel plot
  tempPlot$plotObject <- p

  return(tempPlot)
}

.robmaPriorAndPosteriorPlotModerationFun <- function(fit, options, parameter) {

  moderators          <- unlist(options[["effectSizeModelTerms"]])
  moderationContainer <- createJaspContainer()

  for (j in seq_along(moderators)) {

    parameter <- moderators[j]

    # create individual plots for each subgroup
    if (options[["subgroup"]] == "") {

      tempPlot       <- do.call(.robmaPriorAndPosteriorPlotEstimateFun, list(fit = fit[[1]], options = options, parameter = parameter))
      tempPlot$title <- parameter
      tempPlot$dependOn(.robmaDependencies)
      tempPlot$position <- j
      moderationContainer[[parameter]] <- tempPlot

    } else {

      # create the output container
      tempPlot       <- createJaspContainer()
      tempPlot$title <- parameter
      tempPlot$dependOn(.robmaDependencies)
      tempPlot$position <- j
      moderationContainer[[parameter]] <- tempPlot

      for (i in seq_along(fit)) {
        tempPlot[[names(fit)[i]]]          <- do.call(.robmaPriorAndPosteriorPlotEstimateFun, list(fit = fit[[i]], options = options, parameter = parameter))
        tempPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
        tempPlot[[names(fit)[i]]]$position <- i
      }

    }
  }

  return(moderationContainer)
}
