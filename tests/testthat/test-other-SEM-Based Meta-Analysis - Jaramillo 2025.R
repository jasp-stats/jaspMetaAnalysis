context("Other: SEM-Based Meta-Analysis - Jaramillo 2025")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("encoded hybrid SEM model log-likelihoods match the Cheung reference", {
  jaspFile <- testthat::test_path("jaspfiles", "other", "SEM-Based Meta-Analysis - Jaramillo 2025.jasp")
  opts     <- jaspTools::analysisOptions(jaspFile)
  dataset  <- jaspTools::extractDatasetFromJASPFile(jaspFile)[c("yi", "vi")]

  names(dataset) <- c("JaspColumn_9_Encoded", "JaspColumn_0_Encoded")
  opts[["modelSummary"]]            <- FALSE
  opts[["pairwiseModelComparison"]] <- FALSE
  opts[["pathDiagram"]]             <- FALSE

  results <- jaspTools::runAnalysis(
    "SemBasedMetaAnalysis", dataset, opts, encodedDataset = TRUE, view = FALSE
  )

  expect_identical(results[["status"]], "complete")

  fitTable  <- results[["results"]][["modelFitTable"]][["data"]]
  hybridFit <- fitTable[grepl("^Hybrid models", vapply(fitTable, `[[`, character(1), "name"))]
  logLik    <- vapply(hybridFit, `[[`, numeric(1), "logLik")

  expect_equal(logLik, c(27.88701, 27.80180, 27.72565), tolerance = 1e-5)
})

