context("Other: Effect Size Aggregation - dat.ishak2007")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Effect Size Aggregation - dat.ishak2007.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-27.20167, -29.13199, -38.14078, "Pooled effect", 0.9701636, -25.27134,
     -16.26255))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.13628015865022e-159, "Q<unicode>(81) = 1003.86", "Heterogeneity",
     1.85396853361382e-43, "t(81) = -28.04", "Pooled effect"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerG"]][["collection"]][["randomEstimatesContainer_containerG_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(29.2857791691486, 5.41163368763525, "𝜏<unicode>", 0.847080448797304,
     "", "<unicode>"))

})

test_that("EffectSizeAggregation (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Effect Size Aggregation - dat.ishak2007.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeAggregation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Autoregressive", 46, 82))

})

test_that("ClassicalMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Effect Size Aggregation - dat.ishak2007.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-27.60449, -29.58568, -38.59124, "Pooled effect", 0.9836601, -25.6233,
     -16.61774, 5.36548620206849, 3.92310724580388, "", "𝜏", 0.839352829974039,
     7.47902931880001, "", 28.7884421845874, 15.3907704620789, "",
     "𝜏<unicode>", 9.00707205578569, 55.9358795514702, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.33754320207953e-22, "Q<unicode>(45) = 205.41", "Heterogeneity",
     3.73848444374336e-30, "t(45) = -28.06", "Pooled effect"))

})

