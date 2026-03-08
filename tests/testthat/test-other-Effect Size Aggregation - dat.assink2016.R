context("Other: Effect Size Aggregation - dat.assink2016")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Effect Size Aggregation - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.3618231, 0.1767571, -0.5896522, "Pooled effect", 0.09326904,
     0.5468891, 1.313298))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(5.73140695031607e-118, "Q<unicode>(99) = 840.94", "Heterogeneity",
     0.000188751148518895, "t(99) = 3.88", "Pooled effect"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn3", 17, 0.265436079806521, 0.0704563124630538, "jaspColumn3/jaspColumn4",
     100, 0.38831135490583, 0.150785708348801))

})

test_that("EffectSizeAggregation (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Effect Size Aggregation - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeAggregation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Constructs/groups/times", 17, 100))

})

test_that("ClassicalMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Effect Size Aggregation - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.2631714, 0.05344562, -0.4579155, "Pooled effect", 0.09893166,
     0.4728971, 0.9842583, 0.325445697527831, 0.238654066458671,
     "", "𝜏", 0.0704947854978921, 0.663035048122873, "", 0.105914902039377,
     0.0569557634372599, "", "𝜏<unicode>", 0.0458844492768727, 0.439615475039301,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.52903822584643e-14, "Q<unicode>(16) = 100.73", "Heterogeneity",
     0.0171116562450652, "t(16) = 2.66", "Pooled effect"))

})

test_that("EffectSizeAggregation (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Effect Size Aggregation - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeAggregation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Compound symmetry", 17, 100))

})

test_that("ClassicalMetaAnalysis (analysis 5) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Effect Size Aggregation - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[5]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.2844691, 0.1274051, -0.304575, "Pooled effect", 0.08013615,
     0.441533, 0.8735131, 0.289657391914321, 0.215671645623637, "",
     "𝜏", 0.0650686058246375, 0.632628047862643, "", 0.0839014046906066,
     0.0465142587260076, "", "𝜏<unicode>", 0.037695205317331, 0.400218246942498,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.00690163803178e-12, "Q<unicode>(16) = 90.52", "Heterogeneity",
     0.00038549153783302, "z = 3.55", "Pooled effect"))

})

