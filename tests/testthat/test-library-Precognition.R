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

  table <- results[["results"]][["estimates"]][["collection"]][["estimates_estimatesMean"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7, -0.181725064493159, -0.279313683977276, 0.00818058146506941,
     0.0497910269034957, -3.64975530320707, "PET", -0.0841364450090429,
     7, 0.0236477593506169, -0.0302452797821747, 0.418258983005773,
     0.0274969538001173, 0.860013786345891, "PEESE", 0.0775407984834085
    ))

  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7, 0.000116024151718747, 7.70233485131792, "PET"))

  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_effectTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7, 0.00818058146506941, -3.64975530320707, "PET"))

})

