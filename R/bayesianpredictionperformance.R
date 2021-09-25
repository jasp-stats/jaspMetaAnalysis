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

BayesianPredictionPerformance  <- function(jaspResults, dataset, options, state = NULL) {

  options[["method"]] <- "BAYES"
  ready <- .metamiscReady(options)

  if (ready) {
    dataset <- .metamiscGetData(options, dataset)
    .metamiscFitModelBayesian(jaspResults, options, dataset)
  }

  .metamiscSummaryTable(jaspResults, options)

  if (options[["forestPlot"]])
    .metamiscForestPlot(jaspResults, options, dataset, ready)

  if (options[["priorAndPosteriorPlot"]])
    .metamiscPriorAndPosteriorPlot(jaspResults, options, dataset, ready)

  if (ready && options[["exportColumns"]])
    .metamiscAddColumn(jaspResults, options, dataset)

  if (ready)
    .metamiscFitFunnelAsymmetryTest(jaspResults, options, dataset)

  if (options[["funnelAsymmetryTest"]])
    .metamiscFitFunnelAsymmetryTable(jaspResults, options)

  if (options[["funnelAsymmetryTest"]] && options[["funnelAsymmetryTestPlot"]])
    .metamiscFitFunnelAsymmetryPlot(jaspResults, options)

  if (options[["diagnosticsRmPlot"]])
    .metamiscRmPlot(jaspResults, options, dataset, ready)

  if (options[["diagnosticsAcPlot"]])
    .metamiscAcPlot(jaspResults, options, dataset, ready)

  if (options[["diagnosticsGelmanRubinPlot"]])
    .metamiscGelmanRubinPlot(jaspResults, options, dataset, ready)

  return()
}

# common functions for the classical and Bayesian analysis are in the metamiscPredictionPerformance.R file
.metamiscFitModelBayesian      <- function(jaspResults, options, dataset) {

  if (is.null(jaspResults[["model"]])) {
    model <- createJaspState()
    model$dependOn(c(.metamiscDependencies, .metamiscDependenciesBayesian))
    jaspResults[["model"]] <- model
  } else
    return()


  pars <- list(
    model.oe    = if (options[["measure"]] == "OE")    options[["linkOE"]],
    model.cstat = if (options[["measure"]] == "cstat") options[["linkCstat"]]
  )

  pars$hp.mu.mean <- options[["priorMuNMeam"]]
  pars$hp.mu.var  <- options[["priorMuNSD"]]^2

  if (options[["priorTau"]] == "priorTauU") {
    pars$hp.tau.min <- options[["priorTauUMin"]]
    pars$hp.tau.max <- options[["priorTauUMax"]]
    pars$hp.tau.dist="dunif"
  } else if (options[["priorTau"]] == "priorTauT") {
    pars$hp.tau.dist  <- "dhalft"
    pars$hp.tau.mean  <- options[["priorTauTLocation"]]
    pars$hp.tau.sigma <- options[["priorTauTScale"]]
    pars$hp.tau.df    <- options[["priorTauTDf"]]
    pars$hp.tau.min   <- options[["priorTauTMin"]]
    pars$hp.tau.max   <- options[["priorTauTMax"]]
  }

  dataset <- .metamiscOmitNAs(dataset, options)

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
    method     = "BAYES",
    pars       = pars,
    adapt      = options[["adapt"]],
    burnin     = options[["burnin"]],
    sample     = options[["sample"]],
    n.chains   = options[["chains"]]
  ),error = function(e)e)

  # error handling
  if (any(class(fit) %in% c("simpleError", "error"))) {
    .quitAnalysis(paste0("metamisc package failed with the following error: '", fit[["message"]], "'"))
  }

  model[["object"]] <- fit

  return()
}
.metamiscPriorAndPosteriorPlot <- function(jaspResults, options, dataset, ready) {

  if (!is.null(jaspResults[["priorAndPosteriorPlot"]]))
    return()

  imgHeight  <- 1000
  imgWidth   <- 520

  priorAndPosteriorPlot   <- createJaspPlot(title = gettext("Prior and posterior plot"), width = imgWidth, height = imgHeight)
  priorAndPosteriorPlot$position <- 2.1
  priorAndPosteriorPlot$dependOn(c("priorAndPosteriorPlot", c(.metamiscDependencies, .metamiscDependenciesBayesian)))
  jaspResults[["priorAndPosteriorPlot"]] <- priorAndPosteriorPlot

  if (!ready)
    return()

  p <- try(metamisc::dplot(jaspResults[["model"]][["object"]]))

  if (isTryError(p)) {
    priorAndPosteriorPlot$setError(.extractErrorMessage(p))
    return()
  }

  p <- jaspGraphs::themeJasp(p, sides = "bl") + ggplot2::xlab(gettext("Value")) + ggplot2::ylab(gettext("Density"))

  priorAndPosteriorPlot$plotObject <- p
  return()
}
.metamiscRmPlot                <- function(jaspResults, options, dataset, ready) {

  if (!is.null(jaspResults[["rmPlot"]]))
    return()

  imgHeight  <- 500
  imgWidth   <- 300 * options[["chains"]]

  rmPlot   <- createJaspPlot(title = gettext("Running means plot"), width = imgWidth, height = imgHeight)
  rmPlot$position <- 5
  rmPlot$dependOn(c("diagnosticsRmPlot", c(.metamiscDependencies, .metamiscDependenciesBayesian)))
  jaspResults[["rmPlot"]] <- rmPlot

  if (!ready)
    return()

  p <- try(metamisc::rmplot(jaspResults[["model"]][["object"]]))

  if (isTryError(p)) {
    rmPlot$setError(.extractErrorMessage(p))
    return()
  }

  p <- p + 
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() + 
    ggplot2::xlab(gettext("Iteration")) + ggplot2::ylab(gettext("Running mean"))

  rmPlot$plotObject <- p

  return()
}
.metamiscAcPlot                <- function(jaspResults, options, dataset, ready) {

  if (!is.null(jaspResults[["acPlot"]]))
    return()

  imgHeight  <- 500
  imgWidth   <- 250 * options[["chains"]]

  acPlot   <- createJaspPlot(title = gettext("Autocorrelations plot"), width = imgWidth, height = imgHeight)
  acPlot$position <- 6
  acPlot$dependOn(c("diagnosticsAcPlot", c(.metamiscDependencies, .metamiscDependenciesBayesian)))
  jaspResults[["acPlot"]] <- acPlot

  if (!ready)
    return()

  p <- try(metamisc::acplot(jaspResults[["model"]][["object"]]))

  if (isTryError(p)) {
    acPlot$setError(.extractErrorMessage(p))
    return()
  }

  p <- jaspGraphs::themeJasp(p, sides = "bl") + ggplot2::xlab(gettext("Lag")) + ggplot2::ylab(gettext("Autocorrelation"))

  acPlot$plotObject <- p

  return()
}
.metamiscGelmanRubinPlot       <- function(jaspResults, options, dataset, ready) {

  if (!is.null(jaspResults[["gRPlot"]]))
    return()

  imgHeight  <- 250
  imgWidth   <- 600

  gRPlot   <- createJaspPlot(title = gettext("Autocorrelations plot"), width = imgWidth, height = imgHeight)
  gRPlot$position <- 7
  gRPlot$dependOn(c("gelmanRubinPlot", c(.metamiscDependencies, .metamiscDependenciesBayesian)))
  jaspResults[["gRPlot"]] <- gRPlot

  if (!ready)
    return()

  p <- try(metamisc:::gelmanplot(jaspResults[["model"]][["object"]]))

  if (isTryError(p)) {
    gRPlot$setError(.extractErrorMessage(p))
    return()
  }

  p <- jaspGraphs::themeJasp(p, sides = "bl") + ggplot2::xlab(gettext("Last iteration in chain")) + ggplot2::ylab(gettext("Shrink factor"))

  gRPlot$plotObject <- p
  return()
}
.metamiscOmitNAs               <- function(dataset, options) {

  
  computable <- rep(FALSE, nrow(dataset))

  if (options[["inputMeasure"]] != "" && options[["inputSE"]] != "")
    computable <- computable | (!is.na(dataset[, options[["inputMeasure"]]]) & !is.na(dataset[, options[["inputSE"]]])

  if (options[["inputMeasure"]] != "" && sum(unlist(options[["inputCI"]]) != "") == 2)
    computable <- computable | (!is.na(dataset[,options[["inputMeasure"]]]) & !is.na(dataset[, options[["inputCI"]][[1]][1]]) & !is.na(dataset[, options[["inputCI"]][[1]][2]])

  if (options[["measure"]] == "cstat" && options[["inputMeasure"]] != "" && options[["inputN"]] != "" && options[["inputO"]] != "")
    computable <- computable | (!is.na(dataset[, options[["inputMeasure"]]]) & !is.na(dataset[, options[["inputN"]]]) & !is.na(dataset[, options[["inputO"]]]))

  if (options[["measure"]] == "OE" && options[["inputE"]] != "" && options[["inputO"]] != "")
    computable <- computable | (!is.na(dataset[, options[["inputE"]]]) & !is.na(dataset[, options[["inputO"]]]))

  dataset <- dataset[computable, ]

  return(dataset)
}
