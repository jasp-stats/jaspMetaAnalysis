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
    .metamiscCheckData(options, dataset)
    .metamiscFitModelBayesian(jaspResults, options, dataset)
  }

  .metamiscSummaryTable(jaspResults, options)

  if (options[["forestPlot"]])
    .metamiscForestPlot(jaspResults, options, dataset, ready)

  if (options[["priorAndPosteriorPlot"]])
    .metamiscPriorAndPosteriorPlot(jaspResults, options, dataset, ready)

  if (ready && options[["exportComputedEffectSize"]])
    .metamiscAddColumn(jaspResults, options, dataset)

  if (ready)
    .metamiscFitFunnelAsymmetryTest(jaspResults, options, dataset)

  if (options[["funnelPlotAsymmetryTest"]])
    .metamiscFitFunnelAsymmetryTable(jaspResults, options)

  if (options[["funnelPlotAsymmetryTest"]] && options[["funnelPlotAsymmetryTestPlot"]])
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
    model.oe    = if (options[["measure"]] == "oeRatio")    options[["withinStudyVariation"]],
    model.cstat = if (options[["measure"]] == "cStatistic") options[["withinStudyVariation"]]
  )

  pars$hp.mu.mean <- options[["muNormalPriorMean"]]
  pars$hp.mu.var  <- options[["muNormalPriorSd"]]^2

  if (options[["tauPrior"]] == "uniformPrior") {
    pars$hp.tau.min <- options[["tauUniformPriorMin"]]
    pars$hp.tau.max <- options[["tauUniformPriorMax"]]
    pars$hp.tau.dist="dunif"
  } else if (options[["tauPrior"]] == "tPrior") {
    pars$hp.tau.dist  <- "dhalft"
    pars$hp.tau.mean  <- options[["tauTPriorLocation"]]
    pars$hp.tau.sigma <- options[["tauTPriorScale"]]
    pars$hp.tau.df    <- options[["tauTPriorDf"]]
    pars$hp.tau.min   <- options[["tauTPriorMin"]]
    pars$hp.tau.max   <- options[["tauTPriorMax"]]
  }

  dataset <- .metamiscOmitNAs(dataset, options)

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
    method     = "BAYES",
    pars       = pars,
    adapt      = options[["adapt"]],
    burnin     = options[["burnin"]],
    sample     = options[["sample"]],
    n.chains   = options[["chains"]]
  ))

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

  if (!ready || jaspResults[["summaryTable"]]$getError())
    return()

  p <- try(metamisc::dplot(jaspResults[["model"]][["object"]]))

  if (isTryError(p)) {
    priorAndPosteriorPlot$setError(.extractErrorMessage(p))
    return()
  }

  p <- p + jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::xlab(gettext("Value")) + ggplot2::ylab(gettext("Density"))

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

  if (!ready || jaspResults[["summaryTable"]]$getError())
    return()

  p <- try(metamisc::rmplot(jaspResults[["model"]][["object"]]))

  if (isTryError(p)) {
    rmPlot$setError(.extractErrorMessage(p))
    return()
  }

  p <- p + jaspGraphs::geom_rangeframe() +
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

  if (!ready || jaspResults[["summaryTable"]]$getError())
    return()

  p <- try(metamisc::acplot(jaspResults[["model"]][["object"]]))

  if (isTryError(p)) {
    acPlot$setError(.extractErrorMessage(p))
    return()
  }

  p <- p + jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::xlab(gettext("Lag")) + ggplot2::ylab(gettext("Autocorrelation"))

  acPlot$plotObject <- p

  return()
}
.metamiscGelmanRubinPlot       <- function(jaspResults, options, dataset, ready) {

  if (!is.null(jaspResults[["gRPlot"]]))
    return()

  imgHeight  <- 250
  imgWidth   <- 750

  gRPlot   <- createJaspPlot(title = gettext("Gelman-Rubin plot"), width = imgWidth, height = imgHeight)
  gRPlot$position <- 7
  gRPlot$dependOn(c("gelmanRubinPlot", c(.metamiscDependencies, .metamiscDependenciesBayesian)))
  jaspResults[["gRPlot"]] <- gRPlot

  if (!ready || jaspResults[["summaryTable"]]$getError())
    return()

  p <- try(metamisc:::gelmanplot(jaspResults[["model"]][["object"]]))

  if (isTryError(p)) {
    gRPlot$setError(.extractErrorMessage(p))
    return()
  }

  p <- p + jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    ggplot2::xlab(gettext("Last iteration in chain")) + ggplot2::ylab(gettext("Shrink factor"))

  gRPlot$plotObject <- p
  return()
}
.metamiscOmitNAs               <- function(dataset, options) {

  computable <- rep(FALSE, nrow(dataset))

  if (options[["effectSize"]] != "" && options[["effectSizeSe"]] != "")
    computable <- computable | (!is.na(dataset[, options[["effectSize"]]]) & !is.na(dataset[, options[["effectSizeSe"]]]))

  if (options[["effectSize"]] != "" && sum(unlist(options[["effectSizeCi"]]) != "") == 2)
    computable <- computable | (!is.na(dataset[,options[["effectSize"]]]) & !is.na(dataset[, options[["effectSizeCi"]][[1]][1]]) & !is.na(dataset[, options[["effectSizeCi"]][[1]][2]]))

  if (options[["measure"]] == "cStatistic" && options[["effectSize"]] != "" && options[["numberOfParticipants"]] != "" && options[["numberOfObservedEvents"]] != "")
    computable <- computable | (!is.na(dataset[, options[["effectSize"]]]) & !is.na(dataset[, options[["numberOfParticipants"]]]) & !is.na(dataset[, options[["numberOfObservedEvents"]]]))

  if (options[["measure"]] == "oeRatio" && options[["numberOfExpectedEvents"]] != "" && options[["numberOfObservedEvents"]] != "")
    computable <- computable | (!is.na(dataset[, options[["numberOfExpectedEvents"]]]) & !is.na(dataset[, options[["numberOfObservedEvents"]]]))

  dataset <- dataset[computable, ]

  return(dataset)
}
