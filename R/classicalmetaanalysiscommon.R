#
# Copyright (C) 2013-2018 University of Amsterdam
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

# This shared pipeline runs
# - classical meta-analysis (metafor::rma.uni; .ma helpers)
# - multilevel/multivariate meta-analysis (metafor::rma.mv; .mamm helpers)
# - generalized meta-analysis (metafor::rma.glmm; .maglmm helpers)
#
# This file keeps the shared entry point. Focused implementations are grouped in
# the classicalmetaanalysis-*.R files.


# TODO:
# AIC/BIC Model-averaging
# Diagnostics
# - model re-run on presence of influential cases
# - residual
#   - vs predicted
#   - vs outcome
#   - vs covariates
# Generic
# - allow different covariates factoring across all settings

ClassicalMetaAnalysisCommon <- function(jaspResults, dataset, options, ...) {

  # fit the model
  .maFitModel(jaspResults, dataset, options)
  .maUpdateFitModelDataset(jaspResults, dataset, options)

  # # remove influential observations and refit the model if requested
  # if (options[["diagnosticsCasewiseDiagnostics"]] && options[["diagnosticsCasewiseDiagnosticsRerunWithoutInfluentialCases"]]) {
  #   dataset <- .maRemoveInfluentialObservations(jaspResults, dataset, options)
  #   .maFitModel(jaspResults, dataset, options, objectName = "fitNoInfluence")
  # }

  # model summary
  .maOverallTestsTable(jaspResults, options)
  .maPooledEstimatesTable(jaspResults, options)

  # random effects
  if (.maIsMultilevelMultivariate(options))
    .mammRandomEstimatesTable(jaspResults, options)

  if (options[["fitMeasures"]])
    .maFitMeasuresTable(jaspResults, options)

  # meta-regression tables
  if (.maIsMetaregression(options)) {
    if (options[["metaregressionTermTests"]]) {
      .maTermsTable(jaspResults, options, "effectSize")
      .maTermsTable(jaspResults, options, "heterogeneity")
    }
    if (options[["metaregressionCoefficientEstimates"]]) {
      .maCoefficientEstimatesTable(jaspResults, options, "effectSize")
      .maCoefficientEstimatesTable(jaspResults, options, "heterogeneity")
    }
    if (options[["metaregressionCoefficientCorrelationMatrix"]]) {
      .maCoefficientCorrelationMatrixTable(jaspResults, options, "effectSize")
      .maCoefficientCorrelationMatrixTable(jaspResults, options, "heterogeneity")
    }
  }

  # estimated marginal means and contrasts (the whole section is created within the dispatch)
  .maEstimatedMarginalMeansAndContrasts(jaspResults, options)

  # plots
  .maUltimateForestPlot(jaspResults, options)
  .maBubblePlot(jaspResults, options)

  # diagnostics
  if (.maIsMetaregression(options) && options[["diagnosticsVarianceInflationFactor"]]) {
    .maVarianceInflationTable(jaspResults, options, "effectSize")
    .maVarianceInflationTable(jaspResults, options, "heterogeneity")
  }
  if (!.maIsGLMM(options) && options[["diagnosticsCasewiseDiagnostics"]]) {
    .maCasewiseDiagnosticsTable(jaspResults, options)
  }
  if (!.maIsGLMM(options) && options[["diagnosticsPlotsProfileLikelihood"]])
    .maProfileLikelihoodPlot(jaspResults, options)
  if (!.maIsGLMM(options) && options[["diagnosticsPlotsBaujat"]])
    .maBaujatPlot(jaspResults, options)
  if (!.maIsGLMM(options) && options[["diagnosticsResidualFunnel"]])
    .maResidualFunnelPlot(jaspResults, options)

  # export
  .maExportColumns(jaspResults, dataset, options)

    # additional
  if (options[["showMetaforRCode"]]) {
    if (.maIsGLMM(options)) {
      .maShowMetaforRCode(jaspResults, options, .maglmmMakeMetaforCallText)
    } else {
      .maShowMetaforRCode(jaspResults, options)
    }
  }

  # export the variance-covariance matrix if requested
  if (.maIsMultilevelMultivariate(options) && options[["varianceCovarianceMatrixSaveComputedVarianceCovarianceMatrix"]] != "") {
    .mammExportVarianceCovarianceMatrix(dataset, options)
  }


  return()
}
