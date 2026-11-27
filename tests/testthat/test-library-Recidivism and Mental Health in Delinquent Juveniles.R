context("Library: Recidivism and Mental Health in Delinquent Juveniles")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ClassicalMetaAnalysisMultilevelMultivariate results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Recidivism and Mental Health in Delinquent Juveniles.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "tests")

})

