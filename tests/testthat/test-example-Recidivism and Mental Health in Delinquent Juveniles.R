context("Example: Recidivism and Mental Health in Delinquent Juveniles")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("ClassicalMetaAnalysisMultilevelMultivariate results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Recidivism and Mental Health in Delinquent Juveniles.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.39771568614744, -0.291958411158904, -0.725283438967567, "Intercept",
     0.110222635456644, 0.117554800901594, -2.48359411031884, 0.14136661664976,
     1.91451712906245, 0.694827872650212, 0.29094190145514, "jaspColumn3 (general)",
     0.0184612201203287, 0.0899102943232201, 7.72801243595492, 1.09871384384529,
     2.25549954681634, 0.537954901240965, -0.0186032825168833, "jaspColumn3 (overt)",
     0.053554519218152, 0.143897745461458, 3.73845260407539, 1.09451308499881
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 0.884030969025433, 0.143280092555926, 35.4049489624971, "jaspColumn3"
    ))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.3199415, 0.1223349, -0.6925931, "Pooled effect", 0.09193274,
     0.517548, 1.332476))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8.47024802226508e-108, "Q<unicode>(97) = 783.02", "Residual heterogeneity",
     0.00379241956472367, "t(13.68) = 3.48", "Pooled effect", 0.143280092555926,
     "F<unicode>(2, 0.88) = 35.40", "Moderation"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn1", 17, 0.273412610220827, 0.0747544554277658, "jaspColumn1/jaspColumn5",
     100, 0.372416483224684, 0.138694036977441))

})

