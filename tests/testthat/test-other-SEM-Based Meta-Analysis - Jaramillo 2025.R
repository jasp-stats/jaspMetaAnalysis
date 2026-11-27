context("Other: SEM-Based Meta-Analysis - Jaramillo 2025")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("SemBasedMetaAnalysis results match", {

  skip(message = "Encoding is not working for non-preload dataset")

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "SEM-Based Meta-Analysis - Jaramillo 2025.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("SemBasedMetaAnalysis", dataset, opts, view = FALSE)

})

