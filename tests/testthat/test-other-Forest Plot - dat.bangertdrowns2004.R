context("Other: Forest Plot - dat.bangertdrowns2004")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMetaAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(44, 0.248284560231861, 0.0669634117178548, "Intercept", 0.0084020260145086,
     0.0899692695334001, 2.75965962066289, 0.429605708745866, 44,
     -0.0625656750584828, -0.272859047512877, "wic (yes)", 0.551845284887846,
     0.104344922048493, -0.599604406522114, 0.147727697395912))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 44, 0.551845284887846, 0.359525444320737, "wic"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.2020404, 0.1101935, -0.1997385, "Pooled effect", 0.04557326,
     0.2938872, 0.6038192, 0.194078687887308, 0.125642828813893,
     "", "𝜏", 0.0439924856736385, 0.342899280617828, "", 0.0376665370920592,
     0.0157861204323573, "", "𝜏<unicode>", 0.0170760077928819, 0.117579916648224,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(6.25876712219827e-05, "Q<unicode>(44) = 89.40", "Residual heterogeneity",
     6.10354425498986e-05, "t(44) = 4.43", "Pooled effect", 0.551845284887846,
     "F<unicode>(1, 44) = 0.36", "Moderation"))

})

