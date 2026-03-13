context("Other: GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("EffectSizeComputation (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeComputation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["computeSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(13, "OR", 1, 13))

})

test_that("ClassicalMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("ClassicalMantelHaenszelPeto (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMantelHaenszelPeto", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.473411, -0.5537848, "Pooled effect", 0.04100778, -0.3930372,
     92.6803640448177, "", "I<unicode>", "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8.24945255100163e-29, "Q<unicode>(12) = 163.94", "Heterogeneity",
     7.87716732544439e-31, "z = -11.54", "Pooled effect", 2.33369633183248e-31,
     "CMH(1) = 135.69", "Cochran-Mantel-Haenszel", 2.08678239453652e-30,
     "X<unicode>(12) = 171.76", "Tarone"))

})

test_that("ClassicalGeneralizedMetaAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalGeneralizedMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7639531, -1.140872, -1.97891, "Pooled effect", 0.1729928, -0.3870341,
     0.4510041, 0.530110783542294, "", "", "𝜏", "", "", "", 0.281017442827825,
     "", "", "𝜏<unicode>", "", "", "", 90.7030157246854, "", "",
     "I<unicode>", "", "", "", 10.7561760930929, "", "", "H<unicode>",
     "", "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.18877258872634e-28, "Q(12) = 163.16", "Heterogeneity (Wald)",
     1.79790026076891e-31, "Q(12) = 176.95", "Heterogeneity (LRT)",
     0.000841318972391844, "t(12) = -4.42", "Pooled effect"))

})

test_that("ClassicalMetaAnalysis (analysis 5) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[5]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

test_that("ClassicalGeneralizedMetaAnalysis (analysis 6) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "GLMM Meta-Analysis - Effectiveness of the BCG Vaccine Against Tuberculosis.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[6]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalGeneralizedMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-6_figure-1_bubble-plots")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_adjustedEstimate"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_adjustedEstimate_estimatedMarginalMeansTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, -0.721465978466196, -0.837276408346664, -0.837276408346664,
     1.937251048495e-07, 0.051194687878014, -14.0925945321768, -0.605655548585728,
     -0.605655548585728))

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn2"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn2_contrastsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("alternate <unicode> random", 0.0870918032909199, -0.186000076727456,
     -0.186000076727456, 0.470647986159603, 0.12072188639651, 0.721425135827216,
     0.360183683309296, 0.360183683309296, "alternate <unicode> systematic",
     0.131359908327105, -0.188827199022547, -0.188827199022547, 0.353370111878846,
     0.141540611154351, 0.928072213732747, 0.451547015676756, 0.451547015676756,
     "random <unicode> systematic", 0.0442681050361849, -0.206877158883643,
     -0.206877158883643, 0.690085529568204, 0.111020254494241, 0.398738998013026,
     0.295413368956012, 0.295413368956012))

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn2"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn2_estimatedMarginalMeansTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, -0.648648741260187, -0.859228459519615, -0.859228459519615,
     6.55164152010665e-05, 0.0930880142734858, -6.96812308569076,
     -0.43806902300076, -0.43806902300076, "alternate", 9, -0.735740544551107,
     -0.888540421698349, -0.888540421698349, 1.74895763023155e-06,
     0.0675460925792768, -10.8924219959517, -0.582940667403866, -0.582940667403866,
     "random", 9, -0.780008649587292, -1.00936030292111, -1.00936030292111,
     3.02031780731987e-05, 0.101386259586895, -7.69343550857371,
     -0.550656996253479, -0.550656996253479, "systematic"))

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-6_figure-2_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, 0.505284034903763, 0.158537891548917, "Intercept", 0.00928229779672967,
     0.153281190651641, 3.29645165695583, 0.852030178258609, 9, -0.0344853473336353,
     -0.0416446751694238, "jaspColumn1", 1.74351628413826e-06, 0.00316482336131444,
     -10.8964524703561, -0.0273260194978467, 9, -0.0870918032909199,
     -0.360183683309296, "jaspColumn2 (random)", 0.488964466534816,
     0.12072188639651, -0.721425135827216, 0.186000076727456, 9,
     -0.131359908327105, -0.451547015676756, "jaspColumn2 (systematic)",
     0.377588236727967, 0.141540611154351, -0.928072213732747, 0.188827199022547
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "", 1.19838972983152e-27, 118.732676438729, "jaspColumn1",
     2, "", 0.642476732410447, 0.88484935403135, "jaspColumn2"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7359628, -0.8546978, -0.8546978, "Pooled effect", 0.05248755,
     -0.6172277, -0.6172277, 0, "", "", "𝜏", "", "", "", 0, "", "",
     "𝜏<unicode>", "", "", "", 0, "", "", "I<unicode>", "", "", "",
     1, "", "", "H<unicode>", "", "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00381162858216863, "Q(9) = 24.33", "Residual heterogeneity (Wald)",
     0.00410189231585263, "Q(9) = 24.13", "Residual heterogeneity (LRT)",
     2.02361144093593e-07, "t(9) = -14.02", "Pooled effect", 7.48309276064969e-06,
     "F<unicode>(3, 9) = 47.76", "Moderation"))

})

