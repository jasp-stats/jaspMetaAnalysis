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

# Robust Bayesian meta-analysis entry point and option readiness checks.
#
# Fitting, priors, tables, figures, diagnostics, computations, R code, and
# helpers are grouped in the robustbayesianmetaanalysis-*.R files.

RobustBayesianMetaAnalysisCommon <- function(jaspResults, dataset, options, state = NULL) {

  # this is needed to register the contrasts till BayesTools is updated
  contr.meandif     <<- BayesTools::contr.meandif
  contr.orthonormal <<- BayesTools::contr.orthonormal
  contr.independent <<- BayesTools::contr.independent

  # check for interactions
  .robmaCheckOptions(options)

  # devel settings
  # options[["advancedMcmcChains"]]     <- 2
  # options[["advancedMcmcAdaptation"]] <- 500
  # options[["advancedMcmcBurnin"]]     <- 1000
  # options[["advancedMcmcSamples"]]    <- 2000

  # attach priors to options
  options <- .robmaAttachPriors(options)

  # get priors and show model specification table
  if (options[["showModelSpecification"]])
    .robmaModelSpecificationTables(jaspResults, options)

  # fit the model
  .maFitModel(jaspResults, dataset, options)
  .maUpdateFitModelDataset(jaspResults, dataset, options)
  .robmaAddMarginalSummary(jaspResults, options)

  # model summary
  .robmaOverallTestsTable(jaspResults, options)
  .robmaPooledEstimatesTable(jaspResults, options)
  if (options[["conditionalEstimates"]])
    .robmaPooledEstimatesTable(jaspResults, options, conditional = TRUE)

  # meta-regression tables
  if (.maIsMetaregression(options)) {
    if (options[["metaregressionTermTests"]] && options[["bayesianModelAveragingModerations"]])
      .robmaTermsTable(jaspResults, options)
    if (options[["metaregressionCoefficientEstimates"]])
      .robmaCoefficientEstimatesTable(jaspResults, options)
    if (options[["metaregressionCoefficientEstimates"]] && options[["conditionalEstimates"]])
      .robmaCoefficientEstimatesTable(jaspResults, options, conditional = TRUE)
    if (options[["metaregressionStandardizedCoefficientEstimates"]])
      .robmaCoefficientEstimatesTable(jaspResults, options, standardized = TRUE)
    if (options[["metaregressionStandardizedCoefficientEstimates"]] && options[["conditionalEstimates"]])
      .robmaCoefficientEstimatesTable(jaspResults, options, standardized = TRUE, conditional = TRUE)
  }

  # publication bias adjustment tables
  if (options[["publicationBiasAdjustmentWeightfunctionEstimates"]] && .robmaHasWeightfunction(options))
    .robmaPublicationBiasWeightfunctionEstimatesTable(jaspResults, options)
  if (options[["publicationBiasAdjustmentWeightfunctionEstimates"]] && options[["conditionalEstimates"]] && .robmaHasWeightfunction(options))
    .robmaPublicationBiasWeightfunctionEstimatesTable(jaspResults, options, conditional = TRUE)
  if (options[["publicationBiasAdjustmentPetPeeseEstimates"]] && .robmaHasPetPeese(options))
    .robmaPublicationBiasPetPeeseEstimatesTable(jaspResults, options)
  if (options[["publicationBiasAdjustmentPetPeeseEstimates"]] && options[["conditionalEstimates"]] && .robmaHasPetPeese(options))
    .robmaPublicationBiasPetPeeseEstimatesTable(jaspResults, options, conditional = TRUE)

  # estimated marginal means and contrasts (the whole section is created within the dispatch)
  .robmaEstimatedMarginalMeans(jaspResults, options)

  # pooled estimates plots
  if (options[["priorAndPosteriorPlotEffectSize"]])
    .robmaPriorAndPosteriorPlot(jaspResults, options, "pooledEffect")
  if (options[["priorAndPosteriorPlotHeterogeneity"]])
    .robmaPriorAndPosteriorPlot(jaspResults, options, "heterogeneity")
  if (options[["priorAndPosteriorPlotModeration"]])
    .robmaPriorAndPosteriorPlot(jaspResults, options, "moderation")
  if (options[["priorAndPosteriorPlotWeightFunction"]] && .robmaHasWeightfunction(options))
    .robmaPriorAndPosteriorPlot(jaspResults, options, "weightFunction")
  if (options[["priorAndPosteriorPlotPetPeese"]] && .robmaHasPetPeese(options))
    .robmaPriorAndPosteriorPlot(jaspResults, options, "petPeese")

  # plots
  .maUltimateForestPlot(jaspResults, options)
  .maBubblePlot(jaspResults, options)

  # diagnostics
  if (options[["mcmcDiagnosticsOverviewTable"]])
    .robmaDiagnosticsTable(jaspResults, options)
  if (options[["mcmcDiagnosticsPlotEffectSize"]])
    .robmaDiagnosticsPlot(jaspResults, options, "pooledEffect")
  if (options[["mcmcDiagnosticsPlotHeterogeneity"]])
    .robmaDiagnosticsPlot(jaspResults, options, "heterogeneity")
  if (options[["mcmcDiagnosticsPlotModeration"]])
    .robmaDiagnosticsPlot(jaspResults, options, "moderation")
  if (options[["mcmcDiagnosticsPlotWeights"]] && .robmaHasWeightfunction(options))
    .robmaDiagnosticsPlot(jaspResults, options, "weights")
  if (options[["mcmcDiagnosticsPlotPet"]]     && .robmaHasPet(options))
    .robmaDiagnosticsPlot(jaspResults, options, "pet")
  if (options[["mcmcDiagnosticsPlotPeese"]]   && .robmaHasPeese(options))
    .robmaDiagnosticsPlot(jaspResults, options, "peese")


  # additional
  if (options[["showRoBMARCode"]])
    .robmaShowRobmaRCode(jaspResults, options)

  return()
}

.robmaDependencies <- c(
  "effectSize", "effectSizeStandardError", "effectSizeMeasure",                   # RoBMA / NoBMA
  "successesGroup1", "successesGroup2", "sampleSizeGroup1", "sampleSizeGroup2",   # BiBMA
  "predictors", "predictors.types", "studyLevelMultilevel", "subgroup",
  "effectSizeModelTerms", "effectSizeModelIncludeIntercept",
  "bayesianModelAveragingEffectSize", "bayesianModelAveragingHeterogeneity", "bayesianModelAveragingModerations", "bayesianModelAveragingPublicationBias",
  "priorDistributionsEffectSizeAndHeterogeneity", "priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield", "priorDistributionsScale", "publicationBiasAdjustment", "modelExpectedDirectionOfTheEffect",
  # prior distributions
  "priorsEffect", "priorsEffectNull", "priorsHeterogeneity", "priorsHeterogeneityNull",
  "priorsModeratorsFactor", "priorsModeratorsFactorNull", "priorsModeratorsContinuous", "priorsModeratorsContinuousNull",
  "priorsBiasSelectionModels", "priorsBiasSelectionModelsNull", "priorsBiasPet", "priorsBiasPetNull", "priorsBiasPeese", "priorsBiasPeeseNull",
  "priorsBaseline", "priorsBaselineNull",

  # MCMC settings
  "advancedMcmcAdaptation", "advancedMcmcBurnin", "advancedMcmcSamples", "advancedMcmcChains", "advancedMcmcThin",
  "autofit", "advancedAutofitRHat", "advancedAutofitRHatTarget", "advancedAutofitEss", "advancedAutofitEssTarget", "advancedAutofitMcmcError",
  "advancedAutofitMcmcErrorTarget", "advancedAutofitMcmcErrorSd", "advancedAutofitMcmcErrorSdTarget", "advancedAutofitMaximumFittingTime",
  "advancedAutofitMaximumFittingTimeTarget", "advancedAutofitMaximumFittingTimeTargetUnit", "advancedAutofitExtendSamples",
  "seed", "setSeed"
)

.robmaCheckOptions <- function(options) {

  if (length(options[["effectSizeModelTerms"]]) == 0)
    return()

  sapply(options[["effectSizeModelTerms"]], function(x) {
    if (length(x[["components"]]) > 1)
      .quitAnalysis(gettext("Interaction terms are currently not possible in Bayesian meta-analyses."))
  })

  return()
}
