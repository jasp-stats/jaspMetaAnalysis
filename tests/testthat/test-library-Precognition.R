context("Library: Precognition")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("PetPeese results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Precognition.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.02364776, -0.0413722, "Pooled effect", 0.02749695, 0.08866772,
     0.10890430076217, "", "<unicode>", "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.9978096529631, "Q<unicode>(7) = 0.76", "Residual heterogeneity",
     0.418258983005765, "t(7) = 0.86", "Pooled effect", 0.000116024151718745,
     "t(7) = 7.70", "Publication bias (PET)"))

})

