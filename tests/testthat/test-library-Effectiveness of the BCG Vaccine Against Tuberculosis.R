context("Library: Effectiveness of the BCG Vaccine Against Tuberculosis")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("EffectSizeComputation (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeComputation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["computeSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(13, "RR", 1, 13))

})

test_that("FunnelPlot (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("FunnelPlot", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["funnelAsymetryTests"]][["collection"]][["funnelAsymetryTests_metaRegressionTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.510431883789501, 13, -1.11822404489433, 0.421784535672232,
     0.0973602773153323, -0.803329112070099))

  plotName <- results[["results"]][["funnelPlotContainer"]][["collection"]][["funnelPlotContainer_funnelPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_")

})

test_that("ClassicalMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_bubble-plots")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_adjustedEstimate"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_adjustedEstimate_estimatedMarginalMeansTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, -0.689171063554146, -1.02374056497082, -1.61214671719985, 0.00118562970725697,
     0.147898433812982, -4.65975903724313, -0.354601562137474, 0.233804590091553
    ))

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn3"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn3_contrastsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("alternate <unicode> random", 9, 0.267491666550525, -0.552339459310787,
     -0.920814009980363, 0.479262979895169, 0.362411214987031, 0.738088821451338,
     1.08732279241184, 1.45579734308141, "alternate <unicode> systematic",
     9, -0.0584619738158694, -0.946330874493171, -1.29468949766996,
     0.884875417595857, 0.392487717157123, -0.148952365285015, 0.829406926861432,
     1.17776555003823, "random <unicode> systematic", 9, -0.325953640366395,
     -1.05214214982371, -1.45169653162192, 0.33644180969223, 0.321015940625031,
     -1.01538147835198, 0.400234869090924, 0.79978925088913))

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn3"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn3_estimatedMarginalMeansTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, -0.619494499309261, -1.30623885441101, -1.72020568541061, 0.071691568426309,
     0.303579418086176, -2.04063405620406, 0.0672498557924919, 0.481216686792083,
     "alternate", 9, -0.886986165859786, -1.33424828387602, -1.856517345429,
     0.00151882753598632, 0.197714873825561, -4.48618836154103, -0.43972404784355,
     0.0825450137094298, "random", 9, -0.561032525493392, -1.13264752939699,
     -1.59383987375937, 0.0535398221266893, 0.252685805081963, -2.22027717509264,
     0.0105824784102028, 0.471774822772592, "systematic"))

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, 0.29319369372162, -0.654295239237754, "Intercept", 0.501614036693835,
     0.418843106279745, 0.700008402491879, 1.24068262668099, 9, -0.0272757391020723,
     -0.0487976390634342, "jaspColumn1", 0.0185690742051195, 0.00951388361308199,
     -2.86694059033548, -0.00575383914071043, 9, -0.267491666550525,
     -1.08732279241184, "jaspColumn3 (random)", 0.479262979895169,
     0.362411214987031, -0.738088821451338, 0.552339459310787, 9,
     0.0584619738158694, -0.829406926861432, "jaspColumn3 (systematic)",
     0.884875417595857, 0.392487717157123, 0.148952365285015, 0.946330874493171
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 9, 0.0185690742051195, 8.21934834851315, "jaspColumn1", 2,
     9, 0.568999143161717, 0.600727887026436, "jaspColumn3"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7455402, -1.062466, -1.662268, "Pooled effect", 0.1400989,
     -0.4286145, 0.1711873, 0.380257354576254, 0.151250833066226,
     "", "𝜏", 0.147747899851243, 0.958896220950987, "", 0.144595655709331,
     0.0228768145032273, "", "𝜏<unicode>", 0.112364451083262, 0.919481962554084,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00189209572183598, "Q<unicode>(9) = 26.20", "Residual heterogeneity",
     0.000480015770367093, "t(9) = -5.32", "Pooled effect", 0.0650072929979309,
     "F<unicode>(3, 9) = 3.45", "Moderation"))

})

