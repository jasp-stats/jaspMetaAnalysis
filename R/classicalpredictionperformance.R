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

# Classical prediction-performance entry point and model state.
#
# Shared validation, output, and funnel-plot helpers are implemented in
# predictionperformance-validation.R, predictionperformance-output.R, and
# predictionperformance-funnel.R.

#' @export
ClassicalPredictionPerformance   <- function(jaspResults, dataset, options, state = NULL) {

  # required for dispatching
  options[["analysis"]] <- "metaAnalysis"

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
