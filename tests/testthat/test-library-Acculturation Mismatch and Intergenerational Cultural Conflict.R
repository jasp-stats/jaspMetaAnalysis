context("Library: Acculturation Mismatch and Intergenerational Cultural Conflict")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("EffectSizeComputation (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeComputation", dataset, opts, view = FALSE)

  table <- results[["results"]][["computeSummary"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-compute-summary")

})

test_that("PetPeese (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-tests")

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-coefficient-estimates")

})

test_that("SelectionModels (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-tests")

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-selection-parameters")

})

test_that("RobustBayesianMetaAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, opts, view = FALSE)

  table <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_diagnosticsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-diagnostics")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-4-tests")

  plotName <- results[["results"]][["priorAndPosteriorPlotContainer"]][["collection"]][["priorAndPosteriorPlotContainer_pooledEffect"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_pooled-effect")

})

