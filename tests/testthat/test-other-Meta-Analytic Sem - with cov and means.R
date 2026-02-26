context("Other: Meta-Analytic Sem - with cov and means")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("MetaAnalyticSem results match", {

  skip(message = "Encoding is not working for Meta-Analytic SEM")

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Meta-Analytic Sem - with cov and means.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("MetaAnalyticSem", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Basic check - analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

