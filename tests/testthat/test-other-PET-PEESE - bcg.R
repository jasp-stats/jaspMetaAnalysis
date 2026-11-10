context("Other: PET-PEESE - bcg")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("PetPeese (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "PET-PEESE - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(10, -46.8150612276674, -95.0562544926107, "Intercept", 0.055894966602299,
     21.6508918292676, -2.16226941582069, 1.42613203727594, 10, 0.0236242645126282,
     -0.000818409225413402, "jaspColumn2", 0.056725177609597, 0.0109699957506024,
     2.15353451812694, 0.0480669382506698))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 10, 0.056725177609597, 4.63771092076424, "jaspColumn2"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.3643054, -0.8257048, "Pooled effect", 0.2070783, 0.09709387,
     8.82476235809716, "", "<unicode>", "", "", 2.97065015747347,
     "", "<unicode><unicode>", "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.19124310557088e-14, "Q<unicode>(10) = 88.25", "Residual heterogeneity",
     0.109033612986492, "t(10) = -1.76", "Pooled effect", 0.271933905540146,
     "t(10) = -1.16", "Publication bias (PET)", 0.056725177609597,
     "F<unicode>(1, 10) = 4.64", "Moderation"))

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(10, -1.54929254500519, -4.51819260981906, "SE", 0.271933905540146,
     1.33245738350967, -1.16273328076308, 1.41960751980868))

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_regression-plot")

})

test_that("PetPeese (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "PET-PEESE - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.2795329, -0.7013263, -1.086522, "Pooled effect", 0.1916385,
     0.1422606, 0.5274559))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.62194832686977e-32, "Q<unicode>(11) = 176.83", "Residual heterogeneity",
     0.172618106629771, "t(11) = -1.46", "Pooled effect", 0.00969816496306319,
     "t(11) = -3.12", "Publication bias (PET)"))

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(11, -1.66541437628309, -2.83914669874918, "SE", 0.00969816496306319,
     0.533275891063811, -3.12298831466171, -0.491682053816997))

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_regression-plot")

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn3", 3, 0.312579723198477, 0.0977060833548367))

})

test_that("PetPeese (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "PET-PEESE - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.6489064, -1.155552, -1.993234, "Pooled effect", 0.2301903,
     -0.142261, 0.6954212, 0.565747397198174, 0.352618047903139,
     "", "𝜏", 0.153497774666824, 1.14165115177837, "", 0.320070117436509,
     0.12433948770702, "", "𝜏<unicode>", 0.173681932986935, 1.30336735235688,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3.19098233336342e-25, "Q<unicode>(11) = 143.48", "Residual heterogeneity",
     0.0166975199678636, "t(11) = -2.82", "Pooled effect", 0.631654032290306,
     "t(11) = -0.49", "Publication bias (PEESE)"))

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(11, -0.647859555034875, -3.53976337562383, "SE<unicode>", 0.631654032290306,
     1.31391336617124, -0.493076310596296, 2.24404426555408))

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_regression-plot")

})

