context("Library: EuroSCORE II")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ClassicalPredictionPerformance results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "EuroSCORE II.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalPredictionPerformance", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_forest-plot")

  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_D-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_debray")

  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-UW"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_egger-unweighted-")

  table <- results[["results"]][["funnelTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(21, "Egger (unweighted)", 0.311590678361251, -1.03686654753257,
     21, "Debray", 0.317958608756867, -1.02297870880589))

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.788860284346523, 0.764878400321528, 0.679256845535331, 0.811000494043596,
     0.868273578310498))

})

