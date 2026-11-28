context("Other: Dependent Effect Sizes - dat.assink2016")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("WaapWls (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Dependent Effect Sizes - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-tests")

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-random-estimates-s")

})

test_that("PetPeese (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Dependent Effect Sizes - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-tests")

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-coefficient-estimates")

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_regression-plot")

})

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Dependent Effect Sizes - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", dataset, opts, view = FALSE)

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_deltype"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_deltype_estimatedMarginalMeansTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-deltype-estimated-marginal-means")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-tests")

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerG"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerG_tempTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-confidence-container-g-temp")

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerG"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerG_tempTable2"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-confidence-container-g-temp-table2")

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerG"]][["collection"]][["randomEstimatesContainer_containerG_table1"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-random-estimates-g-1")

})

