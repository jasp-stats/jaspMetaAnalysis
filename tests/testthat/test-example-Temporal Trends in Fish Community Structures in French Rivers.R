context("Example: Temporal Trends in Fish Community Structures in French Rivers")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("ClassicalMetaAnalysisMultilevelMultivariate results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Temporal Trends in Fish Community Structures in French Rivers.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(182.1199, 118.5497, -36.40474, "Pooled effect", 31.2808, 245.6901,
     400.6445))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7.50257965780061e-24, "Q<unicode>(34) = 191.48", "Heterogeneity",
     1.46608437476369e-06, "t(34) = 5.82", "Pooled effect"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerG"]][["collection"]][["randomEstimatesContainer_containerG_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3425.58619145785, 58.5285075109373, "𝜏<unicode>", 12.8755264659828,
     "", "<unicode>"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn3", 11, 84.6070278841486, 7158.34916738909, "jaspColumn3/jaspColumn4",
     35, 0.00388666709694833, 1.51061811225008e-05))

})

