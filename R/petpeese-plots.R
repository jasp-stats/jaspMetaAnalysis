# PET-PEESE plots.
#
# The regression plot shows the fitted PET line or PEESE curve over the observed
# standard errors; its intercept at a zero standard error is the adjusted
# estimate. Study-level forest and bubble plots use the classical pipeline.

# Regression plot ----

.ppRegressionPlot                       <- function(jaspResults, options) {

  if (!options[["petPeeseRegressionPlot"]])
    return()

  summary <- .ppExtractSummaryContainer(jaspResults, options)

  if (!is.null(summary[["regressionPlot"]]))
    return()

  fits <- .maExtractFit(jaspResults, options)

  if (options[["subgroup"]] == "") {

    plot <- createJaspPlot(title = gettext("Regression Plot"), width = 500, height = 350)
    plot$position <- 2
    plot$dependOn(.ppRegressionPlotDependencies)
    summary[["regressionPlot"]] <- plot

    if (!is.null(fits))
      .ppFillRegressionPlot(plot, fits[[1]], options)

  } else {

    container <- createJaspContainer(gettext("Regression Plot"))
    container$position <- 2
    container$dependOn(.ppRegressionPlotDependencies)
    summary[["regressionPlot"]] <- container

    for (scope in names(fits)) {
      plot <- createJaspPlot(title = gettextf("Subgroup: %1$s", attr(fits[[scope]], "subgroup")), width = 500, height = 350)
      plot$position     <- which(names(fits) == scope)
      container[[scope]] <- plot
      .ppFillRegressionPlot(plot, fits[[scope]], options)
    }
  }

  return()
}

# the summary container holds the model and subgroup dependencies
.ppRegressionPlotDependencies <- c("petPeeseRegressionPlot", "transformEffectSize")

.ppFillRegressionPlot                   <- function(plot, fit, options) {

  if (jaspBase::isTryError(fit)) {
    plot$setError(.maTryCleanErrorMessages(fit))
    return()
  }

  plotData <- try(.ppRegressionPlotData(fit, options))

  if (jaspBase::isTryError(plotData)) {
    plot$setError(gettext("The regression plot could not be created."))
    return()
  }

  plot$plotObject <- .ppMakeRegressionPlot(plotData, fit, options)

  return()
}

.ppRegressionPlotData                   <- function(fit, options) {

  candidate <- attr(fit, "petPeeseCandidate")
  dataset   <- attr(fit, "dataset")

  # observed estimates, sized by their inverse-variance weight; the standard errors are the
  # assigned ones used for the bias term (a precomputed V can have a different diagonal)
  standardErrors <- dataset[[options[["effectSizeStandardError"]]]]
  studies <- data.frame(
    x        = standardErrors,
    y        = fit[["yi"]],
    weight   = 1 / standardErrors^2,
    selected = .ppSelected(dataset, options)
  )

  # fitted relationship at the mean of the user moderators
  seSequence <- seq(0, max(studies[["x"]]) * 1.05, length.out = 101)
  newmods    <- .maZeroBiasTerm(t(colMeans(stats::model.matrix(fit))), fit)
  newmods    <- newmods[rep(1, length(seSequence)), , drop = FALSE]
  newmods[, .maBiasTerm(fit)] <- switch(candidate, pet = seSequence, peese = seSequence^2)

  if (fit[["int.incl"]])
    newmods <- newmods[, -1, drop = FALSE]

  prediction <- data.frame(stats::predict(fit, newmods = newmods))
  regression <- data.frame(
    x     = seSequence,
    y     = prediction[["pred"]],
    lower = prediction[["ci.lb"]],
    upper = prediction[["ci.ub"]]
  )

  # transform the effect size scale if requested
  if (options[["transformEffectSize"]] != "none") {
    transformation <- .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]])
    studies[["y"]] <- do.call(transformation, list(studies[["y"]]))
    regression[, c("y", "lower", "upper")] <- do.call(transformation, list(regression[, c("y", "lower", "upper")]))
  }

  return(list(studies = studies, regression = regression))
}

.ppMakeRegressionPlot                   <- function(plotData, fit, options) {

  studies    <- plotData[["studies"]]
  regression <- plotData[["regression"]]
  noSelection <- options[["noSelection"]] != ""

  studies[["group"]] <- factor(
    ifelse(studies[["selected"]], gettext("Subject to selection"), gettext("Unaffected")),
    levels = c(gettext("Subject to selection"), gettext("Unaffected"))
  )

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(0, regression[["x"]])))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(range(c(studies[["y"]], regression[["lower"]], regression[["upper"]]), na.rm = TRUE))

  plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data    = regression,
      mapping = ggplot2::aes(x = x, ymin = lower, ymax = upper),
      alpha   = .15,
      colour  = NA
    ) +
    ggplot2::geom_line(
      data    = regression,
      mapping = ggplot2::aes(x = x, y = y)
    )

  # estimates at the unaffected level are not adjusted
  if (noSelection) {
    plot <- plot + ggplot2::geom_hline(yintercept = regression[["y"]][1], linetype = "dashed")
  }

  # unaffected estimates are shown as white-filled points
  studyPoints <- if (noSelection) {
    jaspGraphs::geom_point(
      data    = studies,
      mapping = ggplot2::aes(x = x, y = y, size = weight, fill = group),
      alpha   = .7
    )
  } else {
    jaspGraphs::geom_point(
      data    = studies,
      mapping = ggplot2::aes(x = x, y = y, size = weight),
      fill    = "grey",
      alpha   = .7
    )
  }

  plot <- plot +
    studyPoints +
    ggplot2::geom_errorbar(
      data    = regression[1, , drop = FALSE],
      mapping = ggplot2::aes(x = x, ymin = lower, ymax = upper),
      width   = diff(range(xBreaks)) / 50
    ) +
    jaspGraphs::geom_point(
      data    = regression[1, , drop = FALSE],
      mapping = ggplot2::aes(x = x, y = y),
      shape   = 23,
      size    = 4,
      fill    = "black"
    ) +
    ggplot2::scale_size(range = c(1.5, 6), guide = "none") +
    jaspGraphs::scale_x_continuous(
      name   = gettext("Standard Error"),
      breaks = xBreaks,
      limits = range(xBreaks)
    ) +
    jaspGraphs::scale_y_continuous(
      name   = if (options[["transformEffectSize"]] == "none") gettext("Effect Size") else .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]]),
      breaks = yBreaks,
      limits = range(yBreaks)
    ) +
    ggplot2::labs(fill = NULL) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw(legend.position = if (noSelection) "bottom" else "none")

  if (noSelection)
    plot <- plot + ggplot2::scale_fill_manual(
      values = c("grey", "white"),
      drop   = FALSE,
      guide  = ggplot2::guide_legend(override.aes = list(size = 4))
    )

  return(plot)
}
