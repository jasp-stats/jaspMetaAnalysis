# Prediction-performance funnel-asymmetry workflow.
#
# Fits funnel-asymmetry models and builds their tables and figures.

.metamiscFitFunnelAsymmetryTest  <- function(jaspResults, options, dataset) {

  if (jaspResults[["summaryTable"]]$getError())
    return()

  if (is.null(jaspResults[["modelsFat"]])) {
    modelsFat <- createJaspState()
    modelsFat$dependOn(c(.metamiscFunnelTests, .metamiscDependencies, if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
    jaspResults[["modelsFat"]] <- modelsFat
  } else
    return()

  fit     <- jaspResults[["model"]]$object
  fatFits <- modelsFat[["object"]]

  # switch the theta / theta.se location according to the link (poisson/log derives and stores the values at different place)
  if (options[["withinStudyVariation"]] == "poisson/log" && options$method != "BAYES" && .maGetMethodOptions(options) != "FE") {
    theta    <- "theta.blup"
    theta.se <- "theta.se.blup"
  } else {
    theta    <- "theta"
    theta.se <- "theta.se"
  }

  if (is.null(fatFits[["E-UW"]]) && options[["funnelPlotAsymmetryTestEggerUnweighted"]])
    fatFits[["E-UW"]] <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "E-UW")

  if (is.null(fatFits[["E-FIV"]]) && options[["funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion"]])
    fatFits[["E-FIV"]] <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "E-FIV")

  if (is.null(fatFits[["M-FIV"]]) && options[["funnelPlotAsymmetryTestMacaskill"]])
    fatFits[["M-FIV"]] <- try({
      if (options[["numberOfParticipants"]] == "")
        stop("The number of participants must be specified.", call. = FALSE)
      fitFat <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "M-FIV",
                              n.total = dataset[, options[["numberOfParticipants"]]])
      if (is.na(fitFat$pval))
        stop("The regression model could not be estimated.", call. = FALSE)
      else
        fitFat
    })

  if (is.null(fatFits[["M-FPV"]]) && options[["funnelPlotAsymmetryTestMacaskillPooled"]])
    fatFits[["M-FPV"]] <- try({
      if (options[["numberOfParticipants"]] == "")
        stop("The number of participants must be specified.", call. = FALSE)
      else if (options[["numberOfObservedEvents"]] == "")
        stop("The number of observed events must be specified.", call. = FALSE)
      fitFat <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "M-FPV",
                              n.total = dataset[, options[["numberOfParticipants"]]], d.total = dataset[, options[["numberOfObservedEvents"]]])
      if (is.na(fitFat$pval))
        stop("The regression model could not be estimated.", call. = FALSE)
      else
        fitFat
    })

  if (is.null(fatFits[["P-FPV"]]) && options[["funnelPlotAsymmetryTestPeters"]])
    fatFits[["P-FPV"]] <- try({
      if (options[["numberOfParticipants"]] == "")
        stop("The number of participants must be specified.", call. = FALSE)
      else if (options[["numberOfObservedEvents"]] == "")
        stop("The number of observed events must be specified.", call. = FALSE)
      fitFat <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "P-FPV",
                              n.total = dataset[, options[["numberOfParticipants"]]], d.total = dataset[, options[["numberOfObservedEvents"]]])
      if (is.na(fitFat$pval))
        stop("The regression model could not be estimated.", call. = FALSE)
      else
        fitFat
    })

  if (is.null(fatFits[["D-FIV"]]) && options[["funnelPlotAsymmetryTestDebray"]])
    fatFits[["D-FIV"]] <- try({
      if (options[["numberOfObservedEvents"]] == "")
        stop("The number of observed events must be specified.")
      fitFat <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "D-FIV",
                              d.total = dataset[, options[["numberOfObservedEvents"]]])
      if (is.na(fitFat$pval))
        stop("The regression model could not be estimated.", call. = FALSE)
      else
        fitFat
    })

  # if (is.null(fatFits[["D-FAV"]]) && options[["funnelPlotAsymmetryTestDebrayFAV"]])
  #   fatFits[["D-FAV"]] <- try(
  #     metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "D-FAV",
  #                   d1 = dataset[, options[["inputO1"]]], d2 = dataset[, options[["inputO2"]]])
  #   )


  modelsFat[["object"]] <- fatFits

  return()
}

.metamiscFitFunnelAsymmetryTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["funnelTestTable"]]))
    return()

  fatFits <- jaspResults[["modelsFat"]]$object


  funnelTestTable <- createJaspTable(title = gettext("Funnel Plot Asymmetry Tests"))
  funnelTestTable$dependOn(c(.metamiscDependencies, .metamiscFunnelTests, "funnelPlotAsymmetryTest"))
  funnelTestTable$position <- 3

  # add columns
  funnelTestTable$addColumnInfo(name = "method", title = gettext("Method"),       type = "string")
  funnelTestTable$addColumnInfo(name = "t",      title = gettext("t-statistic"),  type = "number")
  funnelTestTable$addColumnInfo(name = "df",     title = gettext("df"),           type = "integer")
  funnelTestTable$addColumnInfo(name = "p",      title = "p",                     type = "pvalue")
  jaspResults[["funnelTestTable"]] <- funnelTestTable

  if (is.null(fatFits) || jaspResults[["summaryTable"]]$getError())
    return()

  for(i in seq_along(fatFits)) {
    if (jaspBase::isTryError(fatFits[[i]])) {
      funnelTestTable$addRows(list(
        method  = .metamiscFitFunnelAsymmetryNames(names(fatFits)[i])
      ))
      funnelTestTable$addFootnote(gettextf("The %1$s test failed with the following error: %2$s",
                                           .metamiscFitFunnelAsymmetryNames(names(fatFits)[i]),
                                           .extractErrorMessage(fatFits[[i]])))
    } else {
      funnelTestTable$addRows(list(
        method  = .metamiscFitFunnelAsymmetryNames(fatFits[[i]]$method),
        t       = fatFits[[i]]$tval,
        df      = fatFits[[i]]$df,
        p       = fatFits[[i]]$pval
      ))
    }

  }


  return()
}

.metamiscFitFunnelAsymmetryPlot  <- function(jaspResults, options) {

  if (!is.null(jaspResults[["funnelTestPlots"]])) {
    funnelTestPlots <- jaspResults[["funnelTestPlots"]]
  } else{
    funnelTestPlots <- createJaspContainer(title = gettext("Funnel Plot Asymmetry Plots"))
    funnelTestPlots$dependOn(c(.metamiscDependencies, "funnelPlotAsymmetryTestPlot", "funnelPlotAsymmetryTest", if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
    funnelTestPlots$position <- 4
    jaspResults[["funnelTestPlots"]] <- funnelTestPlots
  }

  fatFits <- jaspResults[["modelsFat"]]$object

  for(i in seq_along(fatFits)) {
    if (!jaspBase::isTryError(fatFits[[i]]) && is.null(funnelTestPlots[[fatFits[[i]]$method]])) {

      tempFunnelPlot   <- createJaspPlot(
        title  = .metamiscFitFunnelAsymmetryNames(fatFits[[i]]$method),
        width  = 340,
        height = 300)
      tempFunnelPlot$position <- i
      tempFunnelPlot$dependOn(.metamiscFitFunnelAsymmetryOptions(fatFits[[i]]$method))
      funnelTestPlots[[fatFits[[i]]$method]] <- tempFunnelPlot

      tempPlot <- try(.metamiscFitFunnelAsymmetryggPlot(fatFits[[i]]))

      if (any(jaspBase::isTryError(tempPlot))) {
        tempFunnelPlot$setError(tempPlot)
      } else{
        tempPlot <- tempPlot + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
        tempFunnelPlot$plotObject <- tempPlot
      }

    }
  }

  return()
}

.metamiscFitFunnelAsymmetryNames <- function(shortcut) {
  switch(
    shortcut,
    "E-UW"  = gettext("Egger (unweighted)"),
    "E-FIV" = gettext("Egger (multiplicative overdispersion)"),
    "M-FIV" = gettext("Macaskill"),
    "M-FPV" = gettext("Macaskill (pooled)"),
    "P-FPV" = gettext("Peters"),
    "D-FIV" = gettext("Debray")
  )
}

.metamiscFitFunnelAsymmetryOptions <- function(shortcut) {
  switch(
    shortcut,
    "E-UW"  = "funnelPlotAsymmetryTestEggerUnweighted",
    "E-FIV" = "funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion",
    "M-FIV" = "funnelPlotAsymmetryTestMacaskill",
    "M-FPV" = "funnelPlotAsymmetryTestMacaskillPooled",
    "P-FPV" = "funnelPlotAsymmetryTestPeters",
    "D-FIV" = "funnelPlotAsymmetryTestDebray"
  )
}

.metamiscFitFunnelAsymmetryggPlot  <- function(x, ref, xlab = gettext("Effect size"),
                                               confint = TRUE, confint.level = 0.1, confint.alpha = .50, confint.col = "skyblue") {

  if (!inherits(x, "fat"))
    stop("Argument 'x' must be an object of class \"fat\".", domain = NA)
  if (confint.level < 0 | confint.level > 1) {
    stop("Argument 'confint.level' must be between 0 and 1.", domain = NA)
  }

  xval <- x$model$data[, "y"]
  if (x$method %in% c("E-UW", "E-FIV")) {
    ylab <- gettext("Standard error")
    yval <- (x$model$data[, "x"])
    ylim <- rev(c(0, max(yval, na.rm = TRUE)))
    xlim <- c(min(c(0, xval)), max(xval))
  } else if (x$method %in% c("M-FIV")) {
    ylab <- gettext("Sample size")
    xlim <- c(min(c(0, xval)), max(xval))
    yval <- (x$model$data[, "x"])
    yax  <- jaspGraphs::getPrettyAxisBreaks(range(yval, na.rm = TRUE))
    ylim <- range(c(yval, yax))
  } else if (x$method == "P-FPV") {
    ylab <- gettext("Sample size")
    xlim <- c(min(c(0, xval)), max(xval))
    yval <- (x$model$data[, "x"])
    yax  <- unique(round(1/jaspGraphs::getPrettyAxisBreaks(range(yval, na.rm = TRUE))))
    ylim <- range(c(yval, 1/yax))
  } else if (x$method == "D-FIV") {
    ylab <- gettext("Total events")
    xlim <- c(min(c(0, xval)), max(xval))
    yval <- (x$model$data[, "x"])
    yax  <- unique(round(1/jaspGraphs::getPrettyAxisBreaks(range(yval, na.rm = TRUE))))
    ylim <- range(c(yval, 1/yax))
  } else if (x$method == "D-FAV") {
    ylab <- gettext("Total events")
    xlim <- c(min(c(0, xval)), max(xval))
    yval <- (x$model$data[, "x"])
    yax  <- unique(round(1/jaspGraphs::getPrettyAxisBreaks(range(yval, na.rm = TRUE))))
    ylim <- range(c(yval, 1/yax))
  } else {
    stop("Plot is not supported.", call. = FALSE)
  }

  newdata <- sort(c(-max(x$model$data[, "x"]), x$model$data[,"x"], 2 * max(x$model$data[, "x"])))
  newdata <- as.data.frame(cbind(seq(min(newdata), max(newdata), length.out = 500), NA))
  colnames(newdata) <- c("x", "y")
  predy <- predict(x$model, newdata = newdata, se.fit = T)
  predy.mean <- predy$fit
  predy.lowerInt <- as.vector(predy$fit + qt(confint.level/2,  df = x$df) * predy$se.fit)
  predy.upperInt <- as.vector(predy$fit + qt((1 - confint.level/2),  df = x$df) * predy$se.fit)

  p <- ggplot2::ggplot(data = data.frame(x = xval, y = yval))

  if (confint) {


    p <- p + ggplot2::geom_polygon(
      mapping = ggplot2::aes(
        x = x,
        y = y
      ),
      data = data.frame(
        x = c(
          predy.upperInt,
          rev(predy.lowerInt)),
        y = c(
          newdata[, "x"],
          rev(newdata[, "x"]))
      ),
      fill  = confint.col,
      alpha = confint.alpha
    )
  }

  p <- p +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = x, y = y),
      shape   = 19
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = x, y = y),
      data    = data.frame(
        x = predy.mean[newdata[, "x"] > min(pretty(range(ylim))) & newdata[, "x"] < max(pretty(range(ylim)))],
        y = newdata[, "x"][newdata[, "x"] > min(pretty(range(ylim))) & newdata[, "x"] < max(pretty(range(ylim)))]
      ),
      linetype = 2)



  if (missing(ref)) {
    p <- p + ggplot2::geom_vline(xintercept = x$fema$b)
  } else {
    p <- p + ggplot2::geom_vline(xintercept = ref)
  }

  p <- p + ggplot2::scale_x_continuous(
    name   = xlab,
    limits = range(pretty(range(xlim))),
    breaks = pretty(range(xlim)),
    oob    = scales::oob_keep)
  if (x$method %in% c("P-FPV", "D-FAV", "D-FIV")) {
    p <- p + ggplot2::scale_y_reverse(name = ylab, breaks = 1/yax, labels = yax, limits = rev(ylim), oob = scales::oob_keep)
  } else if (x$method %in% c("E-UW", "E-FIV")) {
    p <- p + ggplot2::scale_y_reverse(name = ylab, limits = rev(range(pretty(ylim))), breaks = pretty(range(ylim)), oob = scales::oob_keep)
  } else {
    p <- p + ggplot2::scale_y_continuous(name = ylab, breaks = yax, labels = yax, limits = ylim, oob = scales::oob_keep)
  }

  return(p)
}
