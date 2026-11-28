context("Other: Network Meta-Analysis - Senn2013")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalNetworkMetaAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Senn2013.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", dataset, opts, view = FALSE)

  table <- results[["results"]][["evidenceContainer"]][["collection"]][["evidenceContainer_contributionMatrixTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-contribution-matrix")

  table <- results[["results"]][["evidenceContainer"]][["collection"]][["evidenceContainer_directEvidenceTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-direct-evidence")

  table <- results[["results"]][["evidenceContainer"]][["collection"]][["evidenceContainer_pairwiseMetaAnalysesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-pairwise-meta-analyses")

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_forest-plot")

  table <- results[["results"]][["inconsistencyContainer"]][["collection"]][["inconsistencyContainer_designDecompositionDetachTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-design-decomposition-detach")

  table <- results[["results"]][["inconsistencyContainer"]][["collection"]][["inconsistencyContainer_designDecompositionWithinTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-design-decomposition-within")

  plotName <- results[["results"]][["inconsistencyContainer"]][["collection"]][["inconsistencyContainer_netHeatPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_net-heat-plot")

  plotName <- results[["results"]][["inconsistencyContainer"]][["collection"]][["inconsistencyContainer_nodeSplittingForestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_direct-and-indirect-evidence")

  table <- results[["results"]][["inconsistencyContainer"]][["collection"]][["inconsistencyContainer_nodeSplittingTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-node-splitting")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_heterogeneityTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-heterogeneity")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-input-summary")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-tests")

  plotName <- results[["results"]][["networkPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_network-plot")

  table <- results[["results"]][["rankingContainer"]][["collection"]][["rankingContainer_rankingTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-ranking")

  plotName <- results[["results"]][["rankingContainer"]][["collection"]][["rankingContainer_rankogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-5_rankogram")

  plotName <- results[["results"]][["smallStudyEffectsContainer"]][["collection"]][["smallStudyEffectsContainer_funnelPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-6_comparison-adjusted-funnel-plot")

  table <- results[["results"]][["smallStudyEffectsContainer"]][["collection"]][["smallStudyEffectsContainer_funnelPlotAsymmetryTestsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-funnel-plot-asymmetry-tests")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_leagueTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-league")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-treatment-effects")

})

test_that("ClassicalNetworkMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Senn2013.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", dataset, opts, view = FALSE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_metaregressionCoefficientsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-metaregression-coefficients")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_metaregressionTestsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-metaregression-tests")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_heterogeneityTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-heterogeneity")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-input-summary")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-tests")

  plotName <- results[["results"]][["networkPlot"]][["collection"]][["networkPlot___fullDataset"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_full-dataset")

  plotName <- results[["results"]][["networkPlot"]][["collection"]][["networkPlot_subgroup2000 and later"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_subgroup-2000-and-later")

  plotName <- results[["results"]][["networkPlot"]][["collection"]][["networkPlot_subgroupbefore 2000"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_subgroup-before-2000")

  table <- results[["results"]][["subgroupDifferencesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-subgroup-differences")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-treatment-effects")

})

test_that("ClassicalNetworkMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Senn2013.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-input-summary")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-tests")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-treatment-effects")

})

