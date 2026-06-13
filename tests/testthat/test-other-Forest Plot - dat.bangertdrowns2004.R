context("Other: Forest Plot - dat.bangertdrowns2004")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMetaAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(45, 0.719285570537313, 0.148836112152295, "Intercept", 0.0146220079710322,
     0.283227495437557, 2.5396036123756, 1.28973502892233, 45, -0.462546258121001,
     -1.06566085162577, "jaspColumn2 (no)", 0.12942910169691, 0.29944569719427,
     -1.54467491920886, 0.140568335383766, 45, -0.530857961005681,
     -1.11282981113919, "jaspColumn2 (yes)", 0.0727883526166574,
     0.288948349596339, -1.83720710551658, 0.0511138891278321))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 45, 0.180769731528536, 1.77723113404769, "jaspColumn2"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.2276246, 0.1295265, -0.2238935, "Pooled effect", 0.04870561,
     0.3257228, 0.6791428, 0.218823313153941, 0.156158927239362,
     "", "𝜏", 0.044793927455832, 0.380986674242148, "", 0.0478836423796678,
     0.0243856105565483, "", "𝜏<unicode>", 0.0196039112301249, 0.145150845950093,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.37535915773096e-06, "Q<unicode>(45) = 100.23", "Residual heterogeneity",
     2.70518349322662e-05, "t(45) = 4.67", "Pooled effect", 0.180769731528536,
     "F<unicode>(2, 45) = 1.78", "Moderation"))

})

