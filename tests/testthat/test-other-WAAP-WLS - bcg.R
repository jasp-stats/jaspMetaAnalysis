jaspTools::preserveSnapshotReferences()
context("Other: WAAP-WLS - bcg")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("WaapWls (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "WAAP-WLS - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-tests")

})

test_that("WaapWls (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "WAAP-WLS - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["baujatPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_baujat-plot")

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_bubble-plots")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-tests")

  plotName <- results[["results"]][["residualFunnelPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_residual-funnel-plot")

})

test_that("WaapWls (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "WAAP-WLS - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-tests")

})

