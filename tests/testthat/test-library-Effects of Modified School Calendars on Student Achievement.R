context("Library: Effects of Modified School Calendars on Student Achievement")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ClassicalMetaAnalysisMultilevelMultivariate results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Effects of Modified School Calendars on Student Achievement.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.1847132, 0.01525932, -0.4645113, "Pooled effect", 0.08455592,
     0.354167, 0.8339376))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.7968303019345e-88, "Q<unicode>(55) = 578.86", "Heterogeneity",
     0.0332050188118433, "t(55) = 2.18", "Pooled effect"))

  plotName <- results[["results"]][["profileLikelihoodPlot"]][["collection"]][["profileLikelihoodPlot_plot1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_profile-plot-for-sigma-1-2")

  plotName <- results[["results"]][["profileLikelihoodPlot"]][["collection"]][["profileLikelihoodPlot_plot2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_profile-plot-for-sigma-2-2")

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn3", 11, 0.255072429499733, 0.0650619442908961, "jaspColumn3/jaspColumn4",
     56, 0.180932354841744, 0.0327365170285789))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer_tableInclusion"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(21.9174480674219, 22.388036302716, 27.9394476231193, "", 3, -7.95872403371093,
     "", "", 439.723624453116, 439.799096151229, 441.730957638348,
     421.806176385694, 1, -218.861812226558, "Component 1", 2.54655219172131e-92
    ))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer_tableInclusion1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(21.9174480674219, 22.388036302716, 27.9394476231193, "", 3, -7.95872403371093,
     "", "", 37.6909989052213, 37.9217681359905, 41.7056652756862,
     17.7735508377994, 2, -16.8454994526106, "jaspColumn3", 2.48818251251384e-05,
     68.4329523719603, 68.6637216027295, 72.4476187424252, 48.5155043045384,
     2, -32.2164761859801, "jaspColumn4", 3.27687921744215e-12))

})

