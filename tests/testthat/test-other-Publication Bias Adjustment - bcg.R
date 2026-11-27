jaspTools::preserveSnapshotReferences()
context("Other: Publication Bias Adjustment - bcg")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("WaapWls (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Publication Bias Adjustment - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["baujatPlot"]][["collection"]][["baujatPlot_subgroupalternate"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_subgroup-alternate")

  plotName <- results[["results"]][["baujatPlot"]][["collection"]][["baujatPlot_subgrouprandom"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_subgroup-random")

  plotName <- results[["results"]][["baujatPlot"]][["collection"]][["baujatPlot_subgroupsystematic"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_subgroup-systematic")

  table <- results[["results"]][["casewiseDiagnosticsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-casewise-diagnostics")

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-tests")

})

test_that("WaapWls (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Publication Bias Adjustment - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_bubble-plots")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-tests")

})

test_that("PetPeese (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Publication Bias Adjustment - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-tests")

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-coefficient-estimates")

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["collection"]][["petPeeseSummary_regressionPlot_subgrouprandom"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_subgroup-random")

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["collection"]][["petPeeseSummary_regressionPlot_subgroupsystematic"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_subgroup-systematic")

  plotName <- results[["results"]][["profileLikelihoodPlot"]][["collection"]][["profileLikelihoodPlot_subgrouprandom"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_subgroup-random")

  plotName <- results[["results"]][["profileLikelihoodPlot"]][["collection"]][["profileLikelihoodPlot_subgroupsystematic"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-5_subgroup-systematic")

})

test_that("PetPeese (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Publication Bias Adjustment - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", dataset, opts, view = FALSE)

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_year"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_year_estimatedMarginalMeansTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-effect-size-year-estimated-marginal-means")

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCorrelationTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-effect-size-correlation")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-tests")

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-coefficient-estimates")

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_regression-plot")

  table <- results[["results"]][["varianceInflationContainer"]][["collection"]][["varianceInflationContainer_effectSize"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-effect-size")

})

