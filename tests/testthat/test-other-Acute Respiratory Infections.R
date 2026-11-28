context("Other: Acute Respiratory Infections")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMantelHaenszelPeto (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Acute Respiratory Infections.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMantelHaenszelPeto", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["baujatPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_baujat-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_fitMeasuresTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-fit-measures")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-tests")

  plotName <- results[["results"]][["residualFunnelPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_residual-funnel-plot")

})

test_that("BayesianBinomialMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Acute Respiratory Infections.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianBinomialMetaAnalysis", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_forest-plot")

  table <- results[["results"]][["modelSpecification"]][["collection"]][["modelSpecification_componentPriors"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-model-specification-component-priors")

  table <- results[["results"]][["modelSpecification"]][["collection"]][["modelSpecification_componentPriorsNull"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-model-specification-component-priors-null")

  table <- results[["results"]][["modelSpecification"]][["collection"]][["modelSpecification_overallSummary"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-model-specification-overall-summary")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-tests")

  plotName <- results[["results"]][["priorAndPosteriorPlotContainer"]][["collection"]][["priorAndPosteriorPlotContainer_heterogeneity"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_heterogeneity")

  plotName <- results[["results"]][["priorAndPosteriorPlotContainer"]][["collection"]][["priorAndPosteriorPlotContainer_pooledEffect"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_pooled-effect")

})

