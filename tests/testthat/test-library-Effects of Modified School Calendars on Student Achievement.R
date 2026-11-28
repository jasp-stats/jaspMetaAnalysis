context("Library: Effects of Modified School Calendars on Student Achievement")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effects of Modified School Calendars on Student Achievement.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-tests")

  plotName <- results[["results"]][["profileLikelihoodPlot"]][["collection"]][["profileLikelihoodPlot_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_profile-plot-for-sigma-1-2")

  plotName <- results[["results"]][["profileLikelihoodPlot"]][["collection"]][["profileLikelihoodPlot_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_profile-plot-for-sigma-2-2")

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-random-estimates-s")

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer_tableInclusion"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-random-estimates-inclusion")

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer_tableInclusion1"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-random-estimates-inclusion1")

})

test_that("EffectSizeAggregation (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effects of Modified School Calendars on Student Achievement.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeAggregation", dataset, opts, view = FALSE)

  table <- results[["results"]][["summaryTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-summary")

})

test_that("ClassicalMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effects of Modified School Calendars on Student Achievement.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-tests")

})

