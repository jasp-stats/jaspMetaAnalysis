jaspTools::preserveSnapshotReferences()
context("Other: GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("EffectSizeComputation (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeComputation", dataset, opts, view = FALSE)

  table <- results[["results"]][["computeSummary"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-compute-summary")

})

test_that("ClassicalMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", dataset, opts, view = FALSE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("ClassicalMantelHaenszelPeto (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMantelHaenszelPeto", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-tests")

})

test_that("ClassicalGeneralizedMetaAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalGeneralizedMetaAnalysis", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-tests")

})

test_that("ClassicalMetaAnalysis (analysis 5) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[5]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", dataset, opts, view = FALSE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("ClassicalGeneralizedMetaAnalysis (analysis 6) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[6]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalGeneralizedMetaAnalysis", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-6_figure-1_bubble-plots")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_adjustedEstimate"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_adjustedEstimate_estimatedMarginalMeansTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-6-effect-size-adjusted-estimate-estimated-marginal-means")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc_contrastsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-6-effect-size-alloc-contrasts")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc_estimatedMarginalMeansTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-6-effect-size-alloc-estimated-marginal-means")

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-6_figure-2_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-6-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-6-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-6-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-6-tests")

})

