context("Other: WAAP-WLS - bcg")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("WaapWls (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "WAAP-WLS - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.39828, -1.270512, "Pooled effect", 0.2740759, 0.473952, 39.6451934264023,
     "", "<unicode>", "", "", 6.2964429185376, "", "<unicode><unicode>",
     "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.30818566693114e-25, "Q<unicode>(3) = 118.94", "Heterogeneity",
     0.242129459903462, "t(3) = -1.45", "Pooled effect"))

})

test_that("WaapWls (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "WAAP-WLS - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["baujatPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_baujat-plot")

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_bubble-plots")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4, -106.928797754669, -324.770654515972, "Intercept", 0.244614054198113,
     78.4607109117628, -1.36283238466857, 110.913059006634, 4, 0.054348531576469,
     -0.0571924370254334, "jaspColumn1", 0.247522730048707, 0.0401740226713239,
     1.35282772206087, 0.165889500178371, 4, -0.847336602659409,
     -3.89979488565055, "jaspColumn3 (random)", 0.483872093402281,
     1.09941243832865, -0.770717678933622, 2.20512168033174, 4, -0.741844233660375,
     -3.41307793382219, "jaspColumn3 (systematic)", 0.48368884357541,
     0.962105713943699, -0.771063120100944, 1.92938946650144))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 4, 0.247522730048707, 1.83014284557641, "jaspColumn1", 2, 4,
     0.743214081923513, 0.319920116507993, "jaspColumn3"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7515606, -1.633168, "Pooled effect", 0.317531, 0.1300467, 19.1931483559435,
     "", "<unicode>", "", "", 4.38099855694379, "", "<unicode><unicode>",
     "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8.40210887779597e-16, "Q<unicode>(4) = 76.77", "Residual heterogeneity",
     0.0770817413296994, "t(4) = -2.37", "Pooled effect", 0.451718090107092,
     "F<unicode>(3, 4) = 1.08", "Moderation"))

  plotName <- results[["results"]][["residualFunnelPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_residual-funnel-plot")

})

test_that("WaapWls (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "WAAP-WLS - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.4279596, -0.8286244, "Pooled effect", 0.1737485, -0.02729491,
     16.9235963943981, "", "<unicode>", "", "", 4.11382989371195,
     "", "<unicode><unicode>", "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.15572349787282e-25, "Q<unicode>(8) = 135.39", "Heterogeneity",
     0.0391292466726024, "t(8) = -2.46", "Pooled effect"))

})

