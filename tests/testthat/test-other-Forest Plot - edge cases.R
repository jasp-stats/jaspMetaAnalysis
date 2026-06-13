context("Other: Forest Plot - edge cases")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMetaAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - edge cases.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7145323, -1.108444, -1.996017, "Pooled effect", 0.1807917,
     -0.320621, 0.5669523, 0.559681450452751, 0.345970103934773,
     "", "𝜏", 0.14867902354464, 1.05427048816489, "", 0.313243325980895,
     0.119695312816638, "", "𝜏<unicode>", 0.166425783098725, 1.11148626221544,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.99676459084581e-26, "Q<unicode>(12) = 152.23", "Heterogeneity",
     0.00192001508513394, "t(12) = -3.95", "Pooled effect"))

})

test_that("ClassicalMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - edge cases.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7145323, -1.108444, -1.996017, "Pooled effect", 0.1807917,
     -0.320621, 0.5669523, 0.559681450452751, 0.345970103934773,
     "", "𝜏", 0.14867902354464, 1.05427048816489, "", 0.313243325980895,
     0.119695312816638, "", "𝜏<unicode>", 0.166425783098725, 1.11148626221544,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.99676459084581e-26, "Q<unicode>(12) = 152.23", "Heterogeneity",
     0.00192001508513394, "t(12) = -3.95", "Pooled effect"))

})

test_that("ClassicalMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - edge cases.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.4894209, 0.3300722, 0.1358754, "Pooled effect", 0.7256983, 1.762886,
     0.559681450452751, 0.345970103934773, "", "𝜏", 1.05427048816489,
     "", 0.313243325980895, 0.119695312816638, "", "𝜏<unicode>",
     1.11148626221544, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.99676459084581e-26, "Q<unicode>(12) = 152.23", "Heterogeneity",
     0.00192001508513394, "t(12) = -3.95", "Pooled effect"))

})

test_that("ClassicalMetaAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - edge cases.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.4894209, 0.3300722, 0.1358754, "Pooled effect", 0.7256983, 1.762886,
     0.559681450452751, 0.345970103934773, "", "𝜏", 1.05427048816489,
     "", 0.313243325980895, 0.119695312816638, "", "𝜏<unicode>",
     1.11148626221544, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.99676459084581e-26, "Q<unicode>(12) = 152.23", "Heterogeneity",
     0.00192001508513394, "t(12) = -3.95", "Pooled effect"))

})

test_that("ClassicalMetaAnalysis (analysis 5) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - edge cases.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[5]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-5_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.4894209, 0.3300722, 0.1358754, "Pooled effect", 0.7256983, 1.762886,
     0.559681450452751, 0.345970103934773, "", "𝜏", 1.05427048816489,
     "", 0.313243325980895, 0.119695312816638, "", "𝜏<unicode>",
     1.11148626221544, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.99676459084581e-26, "Q<unicode>(12) = 152.23", "Heterogeneity",
     0.00192001508513394, "t(12) = -3.95", "Pooled effect"))

})

test_that("ClassicalMetaAnalysis (analysis 6) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - edge cases.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[6]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-6_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.3066914, 0.2165822, 0.07906377, "Pooled effect", 0.4103234,
     0.6557516, 0.559681450452751, 0.345970103934773, "", "𝜏", 1.05427048816489,
     "", 0.313243325980895, 0.119695312816638, "", "𝜏<unicode>",
     1.11148626221544, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.99676459084581e-26, "Q<unicode>(12) = 152.23", "Heterogeneity",
     0.00192001508513394, "t(12) = -3.95", "Pooled effect"))

})

test_that("ClassicalMetaAnalysis (analysis 7) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - edge cases.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[7]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-7_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.237449, 0.1338351, 0.02296604, "Pooled effect", 0.3742488, 0.7146267,
     0.559681450452751, 0.345970103934773, "", "𝜏", 1.05427048816489,
     "", 0.313243325980895, 0.119695312816638, "", "𝜏<unicode>",
     1.11148626221544, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.99676459084581e-26, "Q<unicode>(12) = 152.23", "Heterogeneity",
     0.00192001508513394, "t(12) = -3.95", "Pooled effect"))

})

test_that("ClassicalMetaAnalysis (analysis 8) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Forest Plot - edge cases.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[8]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-8_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.4894209, 0.3300722, 0.1358754, "Pooled effect", 0.7256983, 1.762886,
     0.559681450452751, 0.345970103934773, "", "𝜏", 1.05427048816489,
     "", 0.313243325980895, 0.119695312816638, "", "𝜏<unicode>",
     1.11148626221544, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.99676459084581e-26, "Q<unicode>(12) = 152.23", "Heterogeneity",
     0.00192001508513394, "t(12) = -3.95", "Pooled effect"))

})

