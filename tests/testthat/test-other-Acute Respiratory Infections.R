context("Other: Acute Respiratory Infections")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMantelHaenszelPeto (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Acute Respiratory Infections.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMantelHaenszelPeto", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["baujatPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_baujat-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_fitMeasuresTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(6.23389290828086, 10.2338929082809, 4.92704008884081, 0.579153710543459,
     -2.11694645414043, "ML", 2, 4.45786482685549, 8.45786482685549,
     2.45786482685549, 2.45786482685549, -1.22893241342774, "REML",
     2))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.174481, 0.6585981, "Pooled effect", 0.7734238, 3.690364, 0,
     "", "I<unicode>", "", "", 0.0155194047864926, "", "H<unicode>",
     "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.90085851458224, "Q<unicode>(1) = 0.02", "Heterogeneity", 0.00493111095883468,
     "z = 2.81", "Pooled effect"))

  plotName <- results[["results"]][["residualFunnelPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_residual-funnel-plot")

})

test_that("BayesianBinomialMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Acute Respiratory Infections.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianBinomialMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_forest-plot")

  table <- results[["results"]][["modelSpecification"]][["collection"]][["modelSpecification_componentPriors"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Student-t(0, 0.37, 2)", "InvGamma(1.35, 0.27)"))

  table <- results[["results"]][["modelSpecification"]][["collection"]][["modelSpecification_componentPriorsNull"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("independent contrast: Beta(1, 1)", "Spike(0)", "Spike(0)"))

  table <- results[["results"]][["modelSpecification"]][["collection"]][["modelSpecification_overallSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Pooled effect", "1/2", 0.5, "Heterogeneity", "1/2", 0.5, "Baseline",
     "0/1", 0))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.196026243391854, -1.46377470349988, 0.671910521645827, 0.365075334082838,
     "Pooled effect", 2.8796263557547, 3.20728958661247, 0, "", 0.400875253832723,
     0.100615270221908, "𝜏", 2.71865671196185, "", 0, "", 0.847708467442748,
     0.0101234326018276, "𝜏<unicode>", 7.39109624725573, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.37753264295362, 0.703925881490369, 0.5, "Pooled effect", 1.26050926623474,
     0.557621808971539, 0.5, "Heterogeneity"))

  plotName <- results[["results"]][["priorAndPosteriorPlotContainer"]][["collection"]][["priorAndPosteriorPlotContainer_heterogeneity"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_heterogeneity")

  plotName <- results[["results"]][["priorAndPosteriorPlotContainer"]][["collection"]][["priorAndPosteriorPlotContainer_pooledEffect"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-3_pooled-effect")

})

