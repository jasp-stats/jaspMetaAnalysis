jaspTools::preserveSnapshotReferences()
context("Other: Network Meta-Analysis - Baker2009")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalNetworkMetaAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Baker2009.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["componentForestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_component-forest-plot")

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_heterogeneityTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-heterogeneity")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-input-summary")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-tests")

  plotName <- results[["results"]][["networkPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_network-plot")

  table <- results[["results"]][["rankingContainer"]][["collection"]][["rankingContainer_rankingTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-ranking")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_componentEffectsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-component-effects")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_leagueTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-league")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-treatment-effects")

})

test_that("ClassicalNetworkMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Baker2009.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_heterogeneityTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-heterogeneity")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-input-summary")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-tests")

  plotName <- results[["results"]][["networkPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_network-plot")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_componentEffectsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-component-effects")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-treatment-effects")

})

