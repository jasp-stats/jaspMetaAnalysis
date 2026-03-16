context("Library: Recidivism and Mental Health in Delinquent Juveniles")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ClassicalMetaAnalysisMultilevelMultivariate results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Recidivism and Mental Health in Delinquent Juveniles.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8.47024802226508e-108, "Q<unicode>(97) = 783.02", "Residual heterogeneity",
     0.00379241956472367, "t(13.68) = 3.48", "Pooled effect", 0.143280092555926,
     "F<unicode>(2, 0.88) = 35.40", "Moderation"))

})

