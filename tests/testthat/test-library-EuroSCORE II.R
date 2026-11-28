context("Library: EuroSCORE II")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ClassicalPredictionPerformance results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "EuroSCORE II.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalPredictionPerformance", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_forest-plot")

  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_D-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_debray")

  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-UW"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_egger-unweighted-")

  table <- results[["results"]][["funnelTestTable"]]
  jaspTools::expect_snapshot_table(table, "funnel-test")

  table <- results[["results"]][["summaryTable"]]
  jaspTools::expect_snapshot_table(table, "summary")

})

