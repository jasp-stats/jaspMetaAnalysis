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
    dataset <- .metamiscGetData(options, dataset)
    .metamiscFitModel(jaspResults, options, dataset)
  }

  .metamiscSummaryTable(jaspResults, options)

  if (options[["forestPlot"]])
    .metamiscForestPlot(jaspResults, options, dataset, ready)

  if (ready && options[["exportColumns"]])
    .metamiscAddColumn(jaspResults, options, dataset)

  if (ready && options[["funnelAsymmetryTest"]])
    .metamiscFitFunnelAsymmetryTest(jaspResults, options, dataset)

  if (options[["funnelAsymmetryTest"]])
    .metamiscFitFunnelAsymmetryTable(jaspResults, options)

  if (options[["funnelAsymmetryTest"]] && options[["funnelAsymmetryTestPlot"]])
    .metamiscFitFunnelAsymmetryPlot(jaspResults, options)

  return()
}

.metamiscDependencies        <- c("measure", "inputMeasure", "inputSE", "inputCI", "inputN", "inputO", "inputE", "inputLabels",
                                  "method", "linkOE", "linkCstat")
.metamiscDependenciesBayesian<- c("adapt", "burnin", "sample", "chains",
                                  "priorMuNMeam","priorMuNSD",
                                  "priorTau",
                                  "priorTauU", "priorTauUMin", "priorTauUMax",
                                  "priorTauT", "priorTauTLocation", "priorTauTScale", "priorTauTDf", "priorTauTMin", "priorTauTMax")
.metamiscFunnelTests         <- c(
  "funnelAsymmetryTest",
  "funnelAsymmetryTestEggerUW",
  "funnelAsymmetryTestEggerFIV",
  "funnelAsymmetryTestMacaskillFIV",
  "funnelAsymmetryTestMacaskillFPV",
  "funnelAsymmetryTestPeters",
  "funnelAsymmetryTestDebrayFIV")
.metamiscReady               <- function(options) {

  if (options[["inputMeasure"]] != "" && options[["inputSE"]] != "")
    return(TRUE)

  if (options[["inputMeasure"]] != "" && sum(unlist(options[["inputCI"]]) != "") == 2)
    return(TRUE)

  if (options[["measure"]] == "cstat" && options[["inputMeasure"]] != "" && options[["inputN"]] != "" && options[["inputO"]] != "")
    return(TRUE)

  if (options[["measure"]] == "OE" && options[["inputE"]] != "" && options[["inputO"]] != "")
    return(TRUE)

  return(FALSE)
}
.metamiscGetData             <- function(options, dataset) {

  if (!is.null(dataset))
    return(dataset)


  varNames <- c(options[["inputMeasure"]], options[["inputSE"]], unlist(options[["inputCI"]]),
                options[["inputN"]], options[["inputO"]], options[["inputE"]])
  varNames <- varNames[varNames != ""]

  dataset <- readDataSetToEnd(
    columns.as.numeric = varNames,
    columns            = if (options[["inputLabels"]] != "") options[["inputLabels"]]
  )

  if (options[["inputLabels"]] != "") {
    dataset[[options[["inputLabels"]]]] <- as.character(dataset[[options[["inputLabels"]]]])
    if (!validUTF8(dataset[[options[["inputLabels"]]]]))
      .quitAnalysis(gettext("The study labels contain invalid characters. Please, remove them before running the analysis."))
  }

  .hasErrors(dataset               = dataset[,!grepl(colnames(dataset), options[["inputLabels"]])],
             type                  = c("infinity", "observations", "negativeValues"),
             observations.amount   = "< 2",
             exitAnalysisIfErrors  = TRUE)


  return(dataset)
}
.metamiscFitModel            <- function(jaspResults, options, dataset) {

  if (is.null(jaspResults[["model"]])) {
    model <- createJaspState()
    model$dependOn(.metamiscDependencies)
    jaspResults[["model"]] <- model
  } else
    return()

  fit <- tryCatch(metamisc::valmeta(
    measure    = options[["measure"]],
    cstat      = if (options[["measure"]] == "cstat" && options[["inputMeasure"]] != "")              dataset[, options[["inputMeasure"]]],
    cstat.se   = if (options[["measure"]] == "cstat" && options[["inputSE"]] != "")                   dataset[, options[["inputSE"]]],
    cstat.cilb = if (options[["measure"]] == "cstat" && sum(unlist(options[["inputCI"]]) != "") == 2) dataset[, options[["inputCI"]][[1]][1]],
    cstat.ciub = if (options[["measure"]] == "cstat" && sum(unlist(options[["inputCI"]]) != "") == 2) dataset[, options[["inputCI"]][[1]][2]],
    OE         = if (options[["measure"]] == "OE" && options[["inputMeasure"]] != "")                 dataset[, options[["inputMeasure"]]],
    OE.se      = if (options[["measure"]] == "OE" && options[["inputSE"]] != "")                      dataset[, options[["inputSE"]]],
    OE.cilb    = if (options[["measure"]] == "OE" && sum(unlist(options[["inputCI"]]) != "") == 2)    dataset[, options[["inputCI"]][[1]][1]],
    OE.ciub    = if (options[["measure"]] == "OE" && sum(unlist(options[["inputCI"]]) != "") == 2)    dataset[, options[["inputCI"]][[1]][2]],
    N          = if (options[["inputN"]] != "")      dataset[, options[["inputN"]]],
    O          = if (options[["inputO"]] != "")      dataset[, options[["inputO"]]],
    E          = if (options[["inputE"]] != "")      dataset[, options[["inputE"]]],
    slab       = if (options[["inputLabels"]] != "") dataset[, options[["inputLabels"]]],
    method     = .metaAnalysisGetMethod(options),
    pars       = list(
      model.oe    = if (options[["measure"]] == "OE")    options[["linkOE"]],
      model.cstat = if (options[["measure"]] == "cstat") options[["linkCstat"]])
  ), error = function(e) e )

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
           "OE"    = gettext("Observed-Expected Ratio Meta-Analysis"),
           "cstat" = gettext("Concordance Statistic Meta-Analysis"))))
  summaryTable$dependOn(c(.metamiscDependencies, if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
  summaryTable$position <- 1

  # add columns
  summaryTable$addColumnInfo(name = "estimate",    title = gettext("Mean"),  type = "number")
  summaryTable$addColumnInfo(name = "lowerCI",     title = gettext("Lower"), type = "number", overtitle = overtitleCI)
  summaryTable$addColumnInfo(name = "upperCI",     title = gettext("Upper"), type = "number", overtitle = overtitleCI)
  summaryTable$addColumnInfo(name = "lowerPI",     title = gettext("Lower"), type = "number", overtitle = overtitlePI)
  summaryTable$addColumnInfo(name = "upperPI",     title = gettext("Upper"), type = "number", overtitle = overtitlePI)
  jaspResults[["summaryTable"]] <- summaryTable


  if (inherits(fit, c("simpleError", "error")))
    jaspResults[["summaryTable"]]$setError(gettextf("metamisc package failed with the following error: '%s'", fit[["message"]]))

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
      if (options[["measure"]] == "OE") { options[["linkOE"]] } else if (options[["measure"]] == "cstat") {options[["linkCstat"]]}
    ))
  } else{
    summaryTable$addFootnote(gettextf(
      "The model was estimated using MCMC with %1$s link function.",
      if (options[["measure"]] == "OE") { options[["linkOE"]] } else if (options[["measure"]] == "cstat") {options[["linkCstat"]]}
    ))
  }

  return()
}
.metamiscForestPlot          <- function(jaspResults, options, dataset, ready) {

  if (!is.null(jaspResults[["forestPlot"]]))
    return()

  imgHeight  <- 400
  imgWidth   <- 520
  if (ready && !jaspResults[["summaryTable"]]$getError()) {
    imgHeight <- jaspResults[["model"]][["object"]][["numstudies"]] * 25 + 50
    if (!is.null(options[["studyLabels"]]))
      imgWidth <- max(nchar(as.character(dataset[,options[["studyLabels"]]]))) * 5 + 1100
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
                         "OE"    = gettext("Observed-Expected Ratio"),
                         "cstat" = gettext("Concordance Statistic"))) +
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

  if (options[["measure"]] == "OE")
    if ((options[["exportOE"]] == "" || !is.null(jaspResults[["exportOE"]])) || (options[["exportOElCI"]] == "" || !is.null(jaspResults[["exportOElCI"]])) || (options[["exportOEuCI"]] == "" || !is.null(jaspResults[["exportOEuCI"]])))
      return()

  if (options[["measure"]] == "cstat")
    if ((options[["exportCstat"]] == "" || !is.null(jaspResults[["exportCstat"]])) || (options[["exportCstatlCI"]] == "" || !is.null(jaspResults[["exportCstatlCI"]])) || (options[["exportCstatuCI"]] == "" || !is.null(jaspResults[["exportCstatuCI"]])))
      return()

  # # computing the effect sizes based on input
  # if (options[["measure"]] == "cstat") {
  #   computedMeasure <- metamisc::ccalc(
  #     cstat      = if (options[["inputMeasure"]] != "")              dataset[, options[["inputMeasure"]]],
  #     cstat.se   = if (options[["inputSE"]] != "")                   dataset[, options[["inputSE"]]],
  #     cstat.cilb = if (sum(unlist(options[["inputCI"]]) != "") == 2) dataset[, options[["inputCI"]][[1]][1]],
  #     cstat.ciub = if (sum(unlist(options[["inputCI"]]) != "") == 2) dataset[, options[["inputCI"]][[1]][2]],
  #     N          = if (options[["inputN"]] != "")      dataset[, options[["inputN"]]],
  #     O          = if (options[["inputO"]] != "")      dataset[, options[["inputO"]]],
  #     E          = if (options[["inputE"]] != "")      dataset[, options[["inputE"]]]
  #   )
  # } else if (options[["measure"]] == "OE") {
  #   computedMeasure <- metamisc::oecalc(
  #     OE         = if (options[["inputMeasure"]] != "")                 dataset[, options[["inputMeasure"]]],
  #     OE.se      = if (options[["inputSE"]] != "")                      dataset[, options[["inputSE"]]],
  #     OE.cilb    = if (sum(unlist(options[["inputCI"]]) != "") == 2)    dataset[, options[["inputCI"]][[1]][1]],
  #     OE.ciub    = if (sum(unlist(options[["inputCI"]]) != "") == 2)    dataset[, options[["inputCI"]][[1]][2]],
  #     N          = if (options[["inputN"]] != "")      dataset[, options[["inputN"]]],
  #     O          = if (options[["inputO"]] != "")      dataset[, options[["inputO"]]],
  #     E          = if (options[["inputE"]] != "")      dataset[, options[["inputE"]]]
  #   )
  # }

  # extracting the effect sizes from model
  fit     <- jaspResults[["model"]]$object
  fitData <- .metamiscExportData(fit)

  jaspResults[[if (options[["measure"]] == "OE") "exportOE" else "exportCstat"]] <- createJaspColumn(
    columnName   = if (options[["measure"]] == "OE") options[["exportOE"]] else options[["exportCstat"]],
    dependencies = c(if (options[["measure"]] == "OE") "exportOE" else "exportCstat", .metamiscDependencies, if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
  jaspResults[[if (options[["measure"]] == "OE") "exportOE" else "exportCstat"]]$setScale(fitData[["yi"]])

  jaspResults[[if (options[["measure"]] == "OE") "exportOElCI" else "exportCstatlCI"]] <- createJaspColumn(
    columnName   = if (options[["measure"]] == "OE") options[["exportOElCI"]] else options[["exportCstatlCI"]],
    dependencies = c(if (options[["measure"]] == "OE") "exportOElCI" else "exportCstatlCI", .metamiscDependencies, if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
  jaspResults[[if (options[["measure"]] == "OE") "exportOElCI" else "exportCstatlCI"]]$setScale(fitData[["yi.lci"]])

  jaspResults[[if (options[["measure"]] == "OE") "exportOEuCI" else "exportCstatuCI"]] <- createJaspColumn(
    columnName   = if (options[["measure"]] == "OE") options[["exportOEuCI"]] else options[["exportCstatuCI"]],
    dependencies = c(if (options[["measure"]] == "OE") "exportOEuCI" else "exportCstatuCI", .metamiscDependencies, if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
  jaspResults[[if (options[["measure"]] == "OE") "exportOEuCI" else "exportCstatuCI"]]$setScale(fitData[["yi.uci"]])

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
  if (options[[switch(options[["measure"]], "OE" = "linkOE", "cstat" = "linkCstat")]] == "poisson/log" && options$method != "BAYES" && .metaAnalysisGetMethod(options) != "FE") {
    theta    <- "theta.blup"
    theta.se <- "theta.se.blup"
  } else {
    theta    <- "theta"
    theta.se <- "theta.se"
  }

  if (is.null(fatFits[["E-UW"]]) && options[["funnelAsymmetryTestEggerUW"]])
    fatFits[["E-UW"]] <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "E-UW")

  if (is.null(fatFits[["E-FIV"]]) && options[["funnelAsymmetryTestEggerFIV"]])
    fatFits[["E-FIV"]] <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "E-FIV")

  if (is.null(fatFits[["M-FIV"]]) && options[["funnelAsymmetryTestMacaskillFIV"]])
    fatFits[["M-FIV"]] <- tryCatch({
      if (options[["inputN"]] == "")
        stop("The number of participants must be specified.")
      fitFat <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "M-FIV",
                              n.total = dataset[, options[["inputN"]]])
      if (is.na(fitFat$pval))
        stop("The regression model could not be estimated.")
      else
        fitFat
    }, error = function(e)e)

  if (is.null(fatFits[["M-FPV"]]) && options[["funnelAsymmetryTestMacaskillFPV"]])
    fatFits[["M-FPV"]] <- tryCatch({
      if (options[["inputN"]] == "")
        stop("The number of participants must be specified.")
      else if (options[["inputO"]] == "")
        stop("The number of observed events must be specified.")
      fitFat <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "M-FPV",
                              n.total = dataset[, options[["inputN"]]], d.total = dataset[, options[["inputO"]]])
      if (is.na(fitFat$pval))
        stop("The regression model could not be estimated.")
      else
        fitFat
    }, error = function(e)e)

  if (is.null(fatFits[["P-FPV"]]) && options[["funnelAsymmetryTestPeters"]])
    fatFits[["P-FPV"]] <- tryCatch({
      if (options[["inputN"]] == "")
        stop("The number of participants must be specified.")
      else if (options[["inputO"]] == "")
        stop("The number of observed events must be specified.")
      fitFat <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "P-FPV",
                              n.total = dataset[, options[["inputN"]]], d.total = dataset[, options[["inputO"]]])
      if (is.na(fitFat$pval))
        stop("The regression model could not be estimated.")
      else
        fitFat
    }, error = function(e)e)

  if (is.null(fatFits[["D-FIV"]]) && options[["funnelAsymmetryTestDebrayFIV"]])
    fatFits[["D-FIV"]] <- tryCatch({
      if (options[["inputO"]] == "")
        stop("The number of observed events must be specified.")
      fitFat <- metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "D-FIV",
                              d.total = dataset[, options[["inputO"]]])
      if (is.na(fitFat$pval))
        stop("The regression model could not be estimated.")
      else
        fitFat
    }, error = function(e)e)

  # if (is.null(fatFits[["D-FAV"]]) && options[["funnelAsymmetryTestDebrayFAV"]])
  #   fatFits[["D-FAV"]] <- tryCatch(
  #     metamisc::fat(b = fit$data[,theta], b.se = fit$data[,theta.se], method = "D-FAV",
  #                   d1 = dataset[, options[["inputO1"]]], d2 = dataset[, options[["inputO2"]]]),
  #     error = function(e)e
  #   )


  modelsFat[["object"]] <- fatFits

  return()
}
.metamiscFitFunnelAsymmetryTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["funnelTestTable"]]))
    return()

  fatFits <- jaspResults[["modelsFat"]]$object


  funnelTestTable <- createJaspTable(title = gettext("Funnel Plot Asymmetry Tests"))
  funnelTestTable$dependOn(c(.metamiscDependencies, .metamiscFunnelTests, "funnelAsymmetryTest"))
  funnelTestTable$position <- 3

  # add columns
  funnelTestTable$addColumnInfo(name = "method", title = gettext("Method"),       type = "string")
  funnelTestTable$addColumnInfo(name = "t",      title = gettext("t-statistic"),  type = "number")
  funnelTestTable$addColumnInfo(name = "df",     title = gettext("df"),           type = "integer")
  funnelTestTable$addColumnInfo(name = "p",      title = gettext("p"),            type = "pvalue")
  jaspResults[["funnelTestTable"]] <- funnelTestTable

  if (is.null(fatFits) || jaspResults[["summaryTable"]]$getError())
    return()

  for(i in seq_along(fatFits)) {
    if (inherits(fatFits[[i]], c("simpleError", "error"))) {
      funnelTestTable$addRows(list(
        method  = .metamiscFitFunnelAsymmetryNames(names(fatFits)[i])
      ))
      funnelTestTable$addFootnote(gettextf("The %1$s test failed with the following error: %2$s",
                                           .metamiscFitFunnelAsymmetryNames(names(fatFits)[i]),
                                           fatFits[[i]]$message))
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
    funnelTestPlots$dependOn(c(.metamiscDependencies, "funnelAsymmetryTestPlot", "funnelAsymmetryTest", if (options[["method"]] == "BAYES") .metamiscDependenciesBayesian))
    funnelTestPlots$position <- 4
    jaspResults[["funnelTestPlots"]] <- funnelTestPlots
  }

  fatFits <- jaspResults[["modelsFat"]]$object

  for(i in seq_along(fatFits)) {
    if (!inherits(fatFits[[i]], c("simpleError", "error")) && is.null(funnelTestPlots[[.metamiscFitFunnelAsymmetryNames(fatFits[[i]]$method)]])) {

      tempFunnelPlot   <- createJaspPlot(
        title  = .metamiscFitFunnelAsymmetryNames(fatFits[[i]]$method),
        width  = 340,
        height = 300)
      tempFunnelPlot$position <- i
      tempFunnelPlot$dependOn(.metamiscFitFunnelAsymmetryOptions(fatFits[[i]]$method))
      funnelTestPlots[[.metamiscFitFunnelAsymmetryNames(fatFits[[i]]$method)]] <- tempFunnelPlot

      tempPlot <- tryCatch(.metamiscFitFunnelAsymmetryggPlot(fatFits[[i]]), error = function(e)e)

      if (any(class(tempPlot) %in% c("simpleError", "error"))) {
        tempFunnelPlot$setError(tempPlot$message)
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
    "E-UW"  = "Egger (unweighted)",
    "E-FIV" = "Egger (multiplicative overdispersion)",
    "M-FIV" = "Macaskill",
    "M-FPV" = "Macaskill (pooled)",
    "P-FPV" = "Peters",
    "D-FIV" = "Debray"
  )
}
.metamiscFitFunnelAsymmetryOptions <- function(shortcut) {
  switch(
    shortcut,
    "E-UW"  = "funnelAsymmetryTestEggerUW",
    "E-FIV" = "funnelAsymmetryTestEggerFIV",
    "M-FIV" = "funnelAsymmetryTestMacaskillFIV",
    "M-FPV" = "funnelAsymmetryTestMacaskillFPV",
    "P-FPV" = "funnelAsymmetryTestPeters",
    "D-FIV" = "funnelAsymmetryTestDebrayFIV"
  )
}
.metamiscFitFunnelAsymmetryggPlot  <- function(x, ref, xlab = "Effect size",
                                               confint = TRUE, confint.level = 0.1, confint.alpha = .50, confint.col = "skyblue") {

  if (!inherits(x, "fat"))
    stop("Argument 'x' must be an object of class \"fat\".")
  if (confint.level < 0 | confint.level > 1) {
    stop("Argument 'confint.level' must be between 0 and 1.")
  }

  xval <- x$model$data[, "y"]
  if (x$method %in% c("E-UW", "E-FIV")) {
    ylab <- "Standard error"
    yval <- (x$model$data[, "x"])
    ylim <- rev(c(0, max(yval, na.rm = TRUE)))
    xlim <- c(min(c(0, xval)), max(xval))
  } else if (x$method %in% c("M-FIV")) {
    ylab <- "Sample size"
    yval <- (x$model$data[, "x"])
    ylim <- (c(0, max(yval, na.rm = TRUE)))
    xlim <- c(min(c(0, xval)), max(xval))
  } else if (x$method == "P-FPV") {
    ylab <- "Sample size"
    yval <- (x$model$data[, "x"])
    ylim <- rev(c(0, max(yval, na.rm = TRUE)))
    xlim <- c(min(c(0, xval)), max(xval))
    step <- ((max(yval) - min(yval))/5)
    yax  <- c(plyr::round_any(1/min(yval), 10^(sapply(round(1/min(yval)), nchar) - 1)),
              plyr::round_any(1/seq(step, 4 * step, by = step), 10), plyr::round_any(1/max(yval), 10))
  } else if (x$method == "D-FIV") {
    ylab <- "Total events"
    yval <- (x$model$data[, "x"])
    ylim <- rev(c(0, max(yval, na.rm = TRUE)))
    xlim <- c(min(c(0, xval)), max(xval))
    step <- ((max(yval) - min(yval))/4)
    yax  <- c(plyr::round_any(1/min(yval), 10^(sapply(round(1/min(yval)), nchar) - 1)),
              plyr::round_any(1/seq(step, 4 * step, by = step), 10), plyr::round_any(1/max(yval), 10))
  } else if (x$method == "D-FAV") {
    ylab <- "Total events"
    yval <- (x$model$data[, "x"])
    ylim <- rev(c(0, max(yval, na.rm = TRUE)))
    xlim <- c(min(c(0, xval)), max(xval))
    step <- ((max(yval) - min(yval))/4)
    yax  <- c(plyr::round_any(1/min(yval),10^(sapply(round(1/min(yval)), nchar) - 1)),
              plyr::round_any(1/seq(step, 4 * step, by = step), 10), plyr::round_any(1/max(yval), 10))
  } else {
    stop("Plot not supported!")
  }

  newdata <- sort(c(-max(x$model$data[, "x"]), x$model$data[,"x"], 2 * max(x$model$data[, "x"])))
  newdata <- as.data.frame(cbind(seq(min(newdata), max(newdata), length.out = 500), NA))
  colnames(newdata) <- c("x", "y")
  predy <- predict(x$model, newdata = newdata, se.fit = T)
  predy.mean <- predy$fit
  predy.lowerInt <- as.vector(predy$fit + qt(confint.level/2,  df = x$df) * predy$se.fit)
  predy.upperInt <- as.vector(predy$fit + qt((1 - confint.level/2),  df = x$df) * predy$se.fit)

  # restricting plotting range to the selection
  predy.upperInt[predy.upperInt < min(pretty(range(xlim)))] <- min(pretty(range(xlim)))
  predy.upperInt[predy.upperInt > max(pretty(range(xlim)))] <- max(pretty(range(xlim)))
  predy.lowerInt[predy.lowerInt < min(pretty(range(xlim)))] <- min(pretty(range(xlim)))
  predy.lowerInt[predy.lowerInt > max(pretty(range(xlim)))] <- max(pretty(range(xlim)))
  newdata[, "x"][newdata[, "x"] < min(pretty(range(ylim)))] <- min(pretty(range(ylim)))
  newdata[, "x"][newdata[, "x"] > max(pretty(range(ylim)))] <- max(pretty(range(ylim)))

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
    breaks = pretty(range(xlim)))
  if (x$method %in% c("P-FPV", "D-FAV", "D-FIV")) {
    p <- p + ggplot2::scale_y_reverse(name = ylab, breaks = 1/yax, labels = yax, limits = rev(range(pretty(ylim))))
  } else if (x$method %in% c("E-UW", "E-FIV")) {
    p <- p + ggplot2::scale_y_reverse(name = ylab, limits = rev(range(pretty(ylim))), breaks = pretty(range(ylim)))
  } else {
    p <- p + ggplot2::scale_y_continuous(name = ylab, limits = range(pretty(ylim)), breaks = pretty(range(ylim)))
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





