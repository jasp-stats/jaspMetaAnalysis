#
# Copyright (C) 2019 University of Amsterdam
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


ClassicalPredictionPerformance   <- function(jaspResults, dataset, options, state = NULL) {

  ready <- .metamiscReady(options)

  if (ready) {
    .metamiscCheckData(options, dataset)
    .metamiscFitModel(jaspResults, options, dataset)
  }

  .metamiscSummaryTable(jaspResults, options)

  if (options[["forestPlot"]])
    .metamiscForestPlot(jaspResults, options, dataset, ready)

  if (ready && options[["exportComputedEffectSize"]])
    .metamiscAddColumn(jaspResults, options, dataset)

  if (ready && options[["funnelPlotAsymmetryTest"]])
    .metamiscFitFunnelAsymmetryTest(jaspResults, options, dataset)

  if (options[["funnelPlotAsymmetryTest"]])
    .metamiscFitFunnelAsymmetryTable(jaspResults, options)

  if (options[["funnelPlotAsymmetryTest"]] && options[["funnelPlotAsymmetryTestPlot"]])
    .metamiscFitFunnelAsymmetryPlot(jaspResults, options)

  return()
}

.metamiscDependencies        <- c("measure", "effectSize", "effectSizeSe", "effectSizeCi", "numberOfParticipants", "numberOfObservedEvents", "numberOfExpectedEvents", "studyLabel",
                                  "method", "withinStudyVariation")
.metamiscDependenciesBayesian<- c("adapt", "burnin", "sample", "chains",
                                  "muNormalPriorMean","muNormalPriorSd",
                                  "tauPrior",
                                  "uniformPrior", "tauUniformPriorMin", "tauUniformPriorMax",
                                  "tPrior", "tauTPriorLocation", "tauTPriorScale", "tauTPriorDf", "tauTPriorMin", "tauTPriorMax")
.metamiscFunnelTests         <- c(
  "funnelPlotAsymmetryTest",
  "funnelPlotAsymmetryTestEggerUnweighted",
  "funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion",
  "funnelPlotAsymmetryTestMacaskill",
  "funnelPlotAsymmetryTestMacaskillPooled",
  "funnelPlotAsymmetryTestPeters",
  "funnelPlotAsymmetryTestDebray")
.metamiscReady               <- function(options) {

  if (options[["effectSize"]] != "" && options[["effectSizeSe"]] != "")
    return(TRUE)

  if (options[["effectSize"]] != "" && sum(unlist(options[["effectSizeCi"]]) != "") == 2)
    return(TRUE)

  if (options[["measure"]] == "cStatistic" && options[["effectSize"]] != "" && options[["numberOfParticipants"]] != "" && options[["numberOfObservedEvents"]] != "")
    return(TRUE)

  if (options[["measure"]] == "oeRatio" && options[["numberOfExpectedEvents"]] != "" && options[["numberOfObservedEvents"]] != "")
    return(TRUE)

  return(FALSE)
}
.metamiscCheckData           <- function(options, dataset) {

  varNames <- c(options[["effectSize"]], options[["effectSizeSe"]], unlist(options[["effectSizeCi"]]),
                options[["numberOfParticipants"]], options[["numberOfObservedEvents"]], options[["numberOfExpectedEvents"]])
  varNames <- varNames[varNames != ""]

  .hasErrors(dataset               = dataset[,varNames],
             type                  = c("infinity", "observations", "negativeValues"),
             observations.amount   = "< 2",
             exitAnalysisIfErrors  = TRUE)

  .hasErrors(dataset              = dataset,
             seCheck.target       = varNames[varNames %in% c(options[["effectSizeSe"]],options[["numberOfParticipants"]])],
             custom               = .maCheckStandardErrors,
             exitAnalysisIfErrors = TRUE)

  return()
}
.metamiscFitModel            <- function(jaspResults, options, dataset) {

  if (is.null(jaspResults[["model"]])) {
    model <- createJaspState()
    model$dependOn(.metamiscDependencies)
    jaspResults[["model"]] <- model
  } else
    return()

  fit <- try(metamisc::valmeta(
    measure    = .metamiscGetMeasureOption(options),
    cstat      = if (options[["measure"]] == "cStatistic" && options[["effectSize"]] != "")              dataset[, options[["effectSize"]]],
    cstat.se   = if (options[["measure"]] == "cStatistic" && options[["effectSizeSe"]] != "")                   dataset[, options[["effectSizeSe"]]],
    cstat.cilb = if (options[["measure"]] == "cStatistic" && sum(unlist(options[["effectSizeCi"]]) != "") == 2) dataset[, options[["effectSizeCi"]][[1]][1]],
    cstat.ciub = if (options[["measure"]] == "cStatistic" && sum(unlist(options[["effectSizeCi"]]) != "") == 2) dataset[, options[["effectSizeCi"]][[1]][2]],
    OE         = if (options[["measure"]] == "oeRatio" && options[["effectSize"]] != "")                 dataset[, options[["effectSize"]]],
    OE.se      = if (options[["measure"]] == "oeRatio" && options[["effectSizeSe"]] != "")                      dataset[, options[["effectSizeSe"]]],
    OE.cilb    = if (options[["measure"]] == "oeRatio" && sum(unlist(options[["effectSizeCi"]]) != "") == 2)    dataset[, options[["effectSizeCi"]][[1]][1]],
    OE.ciub    = if (options[["measure"]] == "oeRatio" && sum(unlist(options[["effectSizeCi"]]) != "") == 2)    dataset[, options[["effectSizeCi"]][[1]][2]],
    N          = if (options[["numberOfParticipants"]] != "")      dataset[, options[["numberOfParticipants"]]],
    O          = if (options[["numberOfObservedEvents"]] != "")      dataset[, options[["numberOfObservedEvents"]]],
    E          = if (options[["numberOfExpectedEvents"]] != "")      dataset[, options[["numberOfExpectedEvents"]]],
    slab       = if (options[["studyLabel"]] != "") dataset[, options[["studyLabel"]]],
    method     = .maGetMethodOptions(options),
    pars       = list(
      model.oe    = if (options[["measure"]] == "oeRatio")    options[["withinStudyVariation"]],
      model.cstat = if (options[["measure"]] == "cStatistic") options[["withinStudyVariation"]])
  ))

  model[["object"]] <- fit

  return()
}
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
.metamiscGetMeasureOption <- function(options){
  return(switch(
    options[["measure"]],
    "oeRatio"    = "OE",
    "cStatistic" = "cstat"
  ))
}




