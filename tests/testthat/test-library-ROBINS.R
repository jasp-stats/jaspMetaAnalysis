context("Library: ROBINS")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("RiskOfBiasPlot results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "ROBINS.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RiskOfBiasPlot", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["summaryPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_risk-of-bias-summary")

  plotName <- results[["results"]][["trafficLightPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_risk-of-bias-traffic-light")

})

