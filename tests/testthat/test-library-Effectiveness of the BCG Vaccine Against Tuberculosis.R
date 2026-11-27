context("Library: Effectiveness of the BCG Vaccine Against Tuberculosis")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("EffectSizeComputation (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeComputation", dataset, opts, view = FALSE)

  table <- results[["results"]][["computeSummary"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-compute-summary")

})

test_that("FunnelPlot (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("FunnelPlot", dataset, opts, view = FALSE)

  table <- results[["results"]][["funnelAsymetryTests"]][["collection"]][["funnelAsymetryTests_metaRegressionTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-funnel-asymetry-tests-meta-regression")

  plotName <- results[["results"]][["funnelPlotContainer"]][["collection"]][["funnelPlotContainer_funnelPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_")

})

test_that("ClassicalMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_bubble-plots")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_adjustedEstimate"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_adjustedEstimate_estimatedMarginalMeansTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-adjusted-estimate-estimated-marginal-means")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc_contrastsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-alloc-contrasts")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc_estimatedMarginalMeansTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-alloc-estimated-marginal-means")

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-tests")

})

