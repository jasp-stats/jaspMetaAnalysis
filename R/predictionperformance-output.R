# Prediction-performance summary output and exports.
#
# Builds summary/forest output and computed effect-size columns.

.metamiscSummaryTable        <- function(jaspResults, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  fit <- jaspResults[["model"]]$object

  overtitleCI <- gettextf("%s%% CI", 95)
  overtitlePI <- gettextf("%s%% PI", 95)

  summaryTable <- createJaspTable(title = gettextf(
    "%s Summary",
    switch(options[["measure"]],
           "oeRatio"    = gettext("Observed-Expected Ratio Meta-Analysis"),
           "cStatistic" = gettext("Concordance Statistic Meta-Analysis"))))
  summaryTable$dependOn(c(.metamiscDependencies, if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
  summaryTable$position <- 1

  # add columns
  summaryTable$addColumnInfo(name = "estimate",    title = gettext("Mean"),  type = "number")
  summaryTable$addColumnInfo(name = "lowerCI",     title = gettext("Lower"), type = "number", overtitle = overtitleCI)
  summaryTable$addColumnInfo(name = "upperCI",     title = gettext("Upper"), type = "number", overtitle = overtitleCI)
  summaryTable$addColumnInfo(name = "lowerPI",     title = gettext("Lower"), type = "number", overtitle = overtitlePI)
  summaryTable$addColumnInfo(name = "upperPI",     title = gettext("Upper"), type = "number", overtitle = overtitlePI)
  jaspResults[["summaryTable"]] <- summaryTable


  if (jaspBase::isTryError(fit))
    jaspResults[["summaryTable"]]$setError(gettextf("metamisc package failed with the following error: '%s'", .extractErrorMessage(fit)))

  if (is.null(fit) || jaspResults[["summaryTable"]]$getError())
    return()

  summaryTable$addRows(list(
    estimate = fit[["est"]],
    lowerCI  = fit[["ci.lb"]],
    upperCI  = fit[["ci.ub"]],
    lowerPI  = fit[["pi.lb"]],
    upperPI  = fit[["pi.ub"]]
  ))

  summaryTable$addFootnote(gettextf("Based on %i studies.", fit[["numstudies"]]))
  if (options[["method"]] != "BAYES") {
    summaryTable$addFootnote(gettextf(
      "The model was estimated using %1$s method with %2$s link function.",
      options[["method"]],
      options[["withinStudyVariation"]]
    ))
  } else{
    summaryTable$addFootnote(gettextf(
      "The model was estimated using MCMC with %1$s link function.",
      options[["withinStudyVariation"]]
    ))
  }

  return()
}

.metamiscForestPlot          <- function(jaspResults, options, dataset, ready) {

  if (!is.null(jaspResults[["forestPlot"]]))
    return()

  imgHeight  <- 400
  imgWidth   <- 650
  if (ready && !jaspResults[["summaryTable"]]$getError()) {
    imgHeight <- jaspResults[["model"]][["object"]][["numstudies"]] * 25 + 50
    if (!is.null(options[["studyLabels"]]))
      imgWidth <- max(nchar(as.character(dataset[,options[["studyLabels"]]]))) * 5 + 1200
  }

  forestPlot   <- createJaspPlot(title = gettext("Forest plot"), width = imgWidth, height = imgHeight)
  forestPlot$position <- 2
  forestPlot$dependOn(c("forestPlot", "forestPlotLabels", "forestPlotEstimates", .metamiscDependencies, if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
  jaspResults[["forestPlot"]] <- forestPlot

  if (!ready || jaspResults[["summaryTable"]]$getError())
    return()

  p <- plot(jaspResults[["model"]][["object"]])

  if (isTryError(p)) {
    forestPlot$setError(.extractErrorMessage(p))
    return()
  }

  p <- p + jaspGraphs::geom_rangeframe(sides = "b") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::ylab(switch(options[["measure"]],
                         "oeRatio"    = gettext("Observed-Expected Ratio"),
                         "cStatistic" = gettext("Concordance Statistic"))) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank()
    )


  if (!options[["forestPlotLabels"]]) {
    p <- p + ggplot2::theme(
      axis.text.y.left = ggplot2::element_blank(),
    )
  }
  if (!options[["forestPlotEstimates"]]) {
    p <- p + ggplot2::theme(
      axis.text.y.right = ggplot2::element_blank(),
    )
  }

  forestPlot$plotObject <- p
  return()
}

.metamiscAddColumn           <- function(jaspResults, options, dataset) {

  if (jaspResults[["summaryTable"]]$getError())
    return()

  # # computing the effect sizes based on input
  # if (options[["measure"]] == "cStatistic") {
  #   computedMeasure <- metamisc::ccalc(
  #     cstat      = if (options[["effectSize"]] != "")              dataset[, options[["effectSize"]]],
  #     cstat.se   = if (options[["effectSizeSe"]] != "")                   dataset[, options[["effectSizeSe"]]],
  #     cstat.cilb = if (sum(unlist(options[["effectSizeCi"]]) != "") == 2) dataset[, options[["effectSizeCi"]][[1]][1]],
  #     cstat.ciub = if (sum(unlist(options[["effectSizeCi"]]) != "") == 2) dataset[, options[["effectSizeCi"]][[1]][2]],
  #     N          = if (options[["numberOfParticipants"]] != "")      dataset[, options[["numberOfParticipants"]]],
  #     O          = if (options[["numberOfObservedEvents"]] != "")      dataset[, options[["numberOfObservedEvents"]]],
  #     E          = if (options[["numberOfExpectedEvents"]] != "")      dataset[, options[["numberOfExpectedEvents"]]]
  #   )
  # } else if (options[["measure"]] == "oeRatio") {
  #   computedMeasure <- metamisc::oecalc(
  #     OE         = if (options[["effectSize"]] != "")                 dataset[, options[["effectSize"]]],
  #     OE.se      = if (options[["effectSizeSe"]] != "")                      dataset[, options[["effectSizeSe"]]],
  #     OE.cilb    = if (sum(unlist(options[["effectSizeCi"]]) != "") == 2)    dataset[, options[["effectSizeCi"]][[1]][1]],
  #     OE.ciub    = if (sum(unlist(options[["effectSizeCi"]]) != "") == 2)    dataset[, options[["effectSizeCi"]][[1]][2]],
  #     N          = if (options[["numberOfParticipants"]] != "")      dataset[, options[["numberOfParticipants"]]],
  #     O          = if (options[["numberOfObservedEvents"]] != "")      dataset[, options[["numberOfObservedEvents"]]],
  #     E          = if (options[["numberOfExpectedEvents"]] != "")      dataset[, options[["numberOfExpectedEvents"]]]
  #   )
  # }

  # extracting the effect sizes from model
  fit     <- jaspResults[["model"]]$object
  fitData <- .metamiscExportData(fit)

  .metamiscAddColumnVariable(jaspResults, options, fitData, "estimate")
  .metamiscAddColumnVariable(jaspResults, options, fitData, "lCI")
  .metamiscAddColumnVariable(jaspResults, options, fitData, "uCI")

  return()
}

.metamiscAddColumnVariable   <- function(jaspResults, options, data, variable) {

  if (options[["measure"]] == "oeRatio")
    optionsVariable <- switch(
      variable,
      "estimate" = "exportComputedEffectSizeOeRatioColumnName",
      "lCI"      = "exportComputedEffectSizeOeRatioLCiColumnName",
      "uCI"      = "exportComputedEffectSizeOeRatioUCiColumnName"
    )
  else if (options[["measure"]] == "cStatistic")
    optionsVariable <- switch(
      variable,
      "estimate" = "exportComputedEffectSizeCStatisticColumnName",
      "lCI"      = "exportComputedEffectSizeCStatisticLCiColumnName",
      "uCI"      = "exportComputedEffectSizeCStatisticUCiColumnName"
    )

  dataVariable <- switch(
    variable,
    "estimate" = "yi",
    "lCI"      = "yi.lci",
    "uCI"      = "yi.uci"
  )


  if (options[[optionsVariable]] != "") {
    jaspResults[[optionsVariable]] <- createJaspColumn(
      columnName   = options[[optionsVariable]],
      dependencies = c(optionsVariable, .metamiscDependencies, if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
    jaspResults[[optionsVariable]]$setScale(data[[dataVariable]])
  }

  return()
}

.metamiscExportData <- function(fit) {

  yi     <- c(fit$data[,"theta"])
  yi.lci <- c(fit$data[,"theta.cilb"])
  yi.uci <- c(fit$data[,"theta.ciub"])

  # Back-transform the raw data
  if (fit$model == "normal/logit") {
    yi     <- sapply(yi,      metamisc:::inv.logit)
    yi.lci <- sapply(yi.lci,  metamisc:::inv.logit)
    yi.uci <- sapply(yi.uci,  metamisc:::inv.logit)
  } else if (fit$model == "normal/log" | fit$model == "poisson/log") {
    yi     <- sapply(yi,   exp)
    yi.lci <- sapply(yi.lci, exp)
    yi.uci <- sapply(yi.uci, exp)
  }

  return(list(
    yi     = yi,
    yi.lci = yi.lci,
    yi.uci = yi.uci
  ))
}
