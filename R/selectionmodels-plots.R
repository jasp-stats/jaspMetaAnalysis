# Selection-model figures.
#
# Builds weight-function and estimate plots.

.smWeightsPlot             <- function(jaspResults, dataset, options, type = "FE") {

  if (!is.null(jaspResults[[paste0(type, "_weights")]])) {
    return()
  } else {
    plotWeights <- createJaspPlot(
      title  = gettextf(
        "Weight Function (%s)",
        ifelse(type == "FE", gettext("Fixed Effects"), gettext("Random Effects"))
      ),
      width  = 500,
      height = 400)
    plotWeights$dependOn(c(.smDependencies, "plotsWeightFunctionRescaleXAxis", ifelse(type == "FE", "plotsWeightFunctionFixedEffectsPlot", "plotsWeightFunctionRandomEffectsPlot")))
    plotWeights$position <- ifelse(type == "FE", 5, 6)
    jaspResults[[paste0(type, "_weights")]] <- plotWeights
  }

  if (!.smCheckReady(options))
    return()


  # handle errors
  fit <- jaspResults[["models"]]$object[[type]]
  if (jaspBase::isTryError(fit)) {
    errorMessage <- .smSetErrorMessage(fit)
    if (!is.null(errorMessage))
      plotWeights$setError(errorMessage)
    return()
  }

  # get the weights and steps
  steps       <- c(0, fit[["steps"]])
  weightsMean <- c(1, fit[["adj_est"]][  ifelse(type == "FE", 2, 3):nrow(fit[["adj_est"]]),  1])
  weightsLowerCI  <- c(1, fit[["ci.lb_adj"]][ifelse(type == "FE", 2, 3):nrow(fit[["ci.lb_adj"]]), 1])
  weightsupperCI  <- c(1, fit[["ci.ub_adj"]][ifelse(type == "FE", 2, 3):nrow(fit[["ci.ub_adj"]]), 1])

  # handle NaN in the estimates
  if (any(c(is.nan(weightsMean), is.nan(weightsLowerCI), is.nan(weightsupperCI)))) {
    plotWeights$setError(gettext("The figure could not be created since one of the estimates is not a number."))
    return()
  }

  # correct the lower bound
  weightsLowerCI[weightsLowerCI < 0] <- 0

  # get the ordering for plotting
  coordOrder <- sort(rep(1:(length(steps)-1),2), decreasing = FALSE)
  stepsOrder <- c(1, sort(rep(2:(length(steps)-1), 2)), length(steps))

  # axis ticks
  xTicks    <- trimws(steps, which = "both", whitespace = "0")
  xTicks[1] <- 0
  yTicks    <- jaspGraphs::getPrettyAxisBreaks(range(c(weightsMean, weightsLowerCI, weightsupperCI)))
  xSteps    <- if (options[["plotsWeightFunctionRescaleXAxis"]]) seq(0, 1, length.out = length(steps)) else steps

  # make the plot happen
  plot <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      ggplot2::aes(
        x = c(xSteps[stepsOrder], rev(xSteps[stepsOrder])),
        y = c(weightsLowerCI[coordOrder], rev(weightsupperCI[coordOrder]))
      ),
      fill = "grey80") +
    ggplot2::geom_path(
      ggplot2::aes(
        x = xSteps[stepsOrder],
        y = weightsMean[coordOrder]
      ),
      size = 1.25) +
    ggplot2::scale_x_continuous(
      gettext("P-value (One-sided)"),
      breaks = xSteps,
      labels = xTicks,
      limits = c(0, 1)) +
    ggplot2::scale_y_continuous(
      gettext("Publication Probability"),
      breaks = yTicks,
      limits = range(yTicks))+
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plotWeights$plotObject <- plot

  return()
}

.smEstimatesPlot           <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["plotEstimates"]])) {
    return()
  } else {
    plotEstimates <- createJaspPlot(
      title  = gettextf(
        "Mean Model Estimates (%s)",
        if (options[["measures"]] == "correlation") "\u03C1" else "\u03BC"
      ),
      width  = 500,
      height = 200)
    plotEstimates$dependOn(c(.smDependencies, "plotsMeanModelEstimatesPlot"))
    plotEstimates$position <- 7
    jaspResults[["plotEstimates"]] <- plotEstimates
  }

  if (!.smCheckReady(options))
    return()


  # handle errors
  FE <- jaspResults[["models"]]$object[["FE"]]
  RE <- jaspResults[["models"]]$object[["RE"]]

  if (jaspBase::isTryError(FE)) {
    errorMessage <- .smSetErrorMessage(FE)
    if (!is.null(errorMessage))
      plotEstimates$setError(errorMessage)
    return()
  }
  if (jaspBase::isTryError(RE)) {
    errorMessage <- .smSetErrorMessage(RE)
    if (!is.null(errorMessage))
      plotEstimates$setError(errorMessage)
    return()
  }

  # get the estimates
  estimates <- data.frame(
    model = c(gettext("Fixed effects"),    gettext("Fixed effects (adjusted)"),    gettext("Random effects"),   gettext("Random effects (adjusted)")),
    mean  = c(FE[["unadj_est"]][1, 1],     FE[["adj_est"]][1, 1],                  RE[["unadj_est"]][2, 1],     RE[["adj_est"]][2, 1]),
    lowerCI   = c(FE[["ci.lb_unadj"]][1, 1],   FE[["ci.lb_adj"]][1, 1],                RE[["ci.lb_unadj"]][2, 1],   RE[["ci.lb_adj"]][2, 1]),
    upperCI   = c(FE[["ci.ub_unadj"]][1, 1],   FE[["ci.ub_adj"]][1, 1],                RE[["ci.ub_unadj"]][2, 1],   RE[["ci.ub_adj"]][2, 1])
  )
  estimates <- estimates[4:1,]

  # handle NaN in the estimates
  if (any(c(is.nan(estimates[,"mean"]), is.nan(estimates[,"lowerCI"]), is.nan(estimates[,"upperCI"]))))
    plotEstimates$setError(gettext("The figure could not be created since one of the estimates is not a number."))

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(0, estimates[,"lowerCI"], estimates[,"upperCI"])))

  # make the plot happen
  plot <- ggplot2::ggplot() +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = estimates[,"lowerCI"],
        xmax = estimates[,"upperCI"],
        y    = 1:4
      ),
      height = 0.3) +
    jaspGraphs::geom_point(
      ggplot2::aes(
        x = estimates[,"mean"],
        y = 1:4)) +
    ggplot2::geom_line(ggplot2::aes(x = c(0,0), y = c(.5, 4.5)), linetype = "dotted") +
    ggplot2::scale_x_continuous(
      bquote("Mean Estimate"~.(if (options[["measures"]] == "correlation") bquote(rho) else bquote(mu))),
      breaks = xTicks,
      limits = range(xTicks)) +
    ggplot2::scale_y_continuous(
      "",
      breaks = 1:4,
      labels = estimates[,"model"],
      limits = c(0.5, 4.5)) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
    jaspGraphs::geom_rangeframe(sides = "b") + jaspGraphs::themeJaspRaw()

  plotEstimates$plotObject <- plot

  return()
}
