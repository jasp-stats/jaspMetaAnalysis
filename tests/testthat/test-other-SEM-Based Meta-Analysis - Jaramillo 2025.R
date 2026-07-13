context("Other: SEM-Based Meta-Analysis - Jaramillo 2025")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("hybrid SEM model log-likelihoods match the Cheung reference", {
  jaspFile <- testthat::test_path("jaspfiles", "other", "SEM-Based Meta-Analysis - Jaramillo 2025.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  OpenMx::mxSetDefaultOptions()
  logLikelihoods <- vapply(opts[["models"]][4:6], function(model) {
    model[["syntax"]][["model"]] <- model[["syntax"]][["modelOriginal"]]
    fit <- jaspMetaAnalysis:::.semmetaFitModelsFun(model, dataset, opts)
    as.numeric(logLik(fit[["mx.fit"]]))
  }, numeric(1))

  expect_equal(logLikelihoods, c(27.88701, 27.80180, 27.72565), tolerance = 1e-5)

})

