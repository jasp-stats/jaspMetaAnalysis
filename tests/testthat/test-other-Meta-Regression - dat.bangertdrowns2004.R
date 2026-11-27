jaspTools::preserveSnapshotReferences()
context("Other: Meta-Regression - dat.bangertdrowns2004")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMetaAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Meta-Regression - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", dataset, opts, view = FALSE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-tests")

})

test_that("ClassicalMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Meta-Regression - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", dataset, opts, view = FALSE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-tests")

})

test_that("ClassicalMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Meta-Regression - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", dataset, opts, view = FALSE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-tests")

})

test_that("ClassicalMetaAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Meta-Regression - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", dataset, opts, view = FALSE)
  # Near-zero variance gives platform-dependent log-scale estimates and standard errors.
  testthat::skip_on_os(c("linux", "mac"))
  snapshotTableNode <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-effect-size-coefficient")

  snapshotTableNode <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-effect-size-terms")

  snapshotTableNode <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_heterogeneityCoefficientTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-heterogeneity-coefficient")

  snapshotTableNode <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_heterogeneityTermsTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-heterogeneity-terms")

  snapshotTableNode <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-pooled-estimates")

  snapshotTableNode <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-tests")

})

