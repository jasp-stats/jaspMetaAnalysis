context("Other: Dependent Effect Sizes - dat.assink2016")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("WaapWls (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Dependent Effect Sizes - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.197848, 0.05052582, -0.1976956, "Pooled effect", 0.07012261,
     0.3451701, 0.5933915))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.30850937750997e-15, "Q<unicode>(18) = 109.27", "Heterogeneity",
     0.0113043309246028, "t(18) = 2.82", "Pooled effect"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn3", 6, 0.135299780236561, 0.0183060305320617, "jaspColumn3/jaspColumn4",
     19, 0.110557303334297, 0.0122229173205518))

})

test_that("PetPeese (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Dependent Effect Sizes - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0017591, -0.3961404, "Pooled effect", 0.1335419, 0.3996586,
     6.39294279922889, "", "<unicode>", "", "", 2.52842694164354,
     "", "<unicode><unicode>", "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(5.45147644842571e-78, "Q<unicode>(98) = 626.51", "Residual heterogeneity",
     0.990225147900357, "t(3.40) = 0.01", "Pooled effect", 0.110656763030059,
     "t(6.64) = 1.84", "Publication bias (PET)"))

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(6.64250086597331, 2.3901514778242, -0.715700368925361, "SE", 0.110656763030059,
     1.29915583101509, 1.83977273608253, 5.49600332457377))

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_regression-plot")

})

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Dependent Effect Sizes - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn2"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn2_estimatedMarginalMeansTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.226523390050997, "covert", -0.38210651208498, -0.38210651208498,
     0.0783903383254406, -0.0709402680170144, -0.0709402680170144,
     "covert", -0.226523390050997, "general", -0.38210651208498,
     -1.11460507773896, 0.0783903383254406, -0.0709402680170144,
     0.661558297636967, "covert", -0.226523390050997, "overt", -0.38210651208498,
     -0.97965484272174, 0.0783903383254406, -0.0709402680170144,
     0.526608062619746, "covert", 0.436911239322366, "covert", 0.215508422147968,
     0.215508422147968, 0.111553499618782, 0.658314056496764, 0.658314056496764,
     "general", 0.436911239322366, "general", 0.215508422147968,
     -0.465032319664349, 0.111553499618782, 0.658314056496764, 1.33885479830908,
     "general", 0.436911239322366, "overt", 0.215508422147968, -0.33251717488708,
     0.111553499618782, 0.658314056496764, 1.20633965353181, "general",
     0.373731875863292, "covert", 0.163310352702806, 0.163310352702806,
     0.106020589996273, 0.584153399023777, 0.584153399023777, "overt",
     0.373731875863292, "general", 0.163310352702806, -0.525579079009404,
     0.106020589996273, 0.584153399023777, 1.27304283073599, "overt",
     0.373731875863292, "overt", 0.163310352702806, -0.392608841147935,
     0.106020589996273, 0.584153399023777, 1.14007259287452, "overt"
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(97, -0.226523390050997, -0.38210651208498, "Intercept", 0.0047576191764636,
     0.0783903383254406, -2.88968506693486, -0.0709402680170144,
     97, 0.663434629373363, 0.392832864200626, "jaspColumn2 (general)",
     4.40763285457964e-06, 0.136342320722418, 4.86594790126878, 0.9340363945461,
     97, 0.600255265914289, 0.338562076719347, "jaspColumn2 (overt)",
     1.5396299513104e-05, 0.131853747182, 4.55243236345604, 0.861948455109231
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 97, 2.43258295481882e-05, 11.87739400628, "jaspColumn2"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.3689888, "covert", 0.1716106, 0.1716106, "Pooled effect", 0.09944871,
     0.566367, 0.566367, 0.3689888, "general", 0.1716106, -0.52736,
     "Pooled effect", 0.09944871, 0.566367, 1.265338, 0.3689888,
     "overt", 0.1716106, -0.3938736, "Pooled effect", 0.09944871,
     0.566367, 1.131851))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9.24741840861346e-104, "Q<unicode>(97) = 761.83", "Residual heterogeneity",
     0.000345163713664055, "t(97) = 3.71", "Pooled effect", 2.43258295481882e-05,
     "F<unicode>(2, 97) = 11.88", "Moderation"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerG"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerG_tempTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, "", "", "𝜏<unicode>[1]", "", "", 0.440538598816659, 0.194074257047345,
     0.305608735745413, 0.0933966993639098, "𝜏<unicode>[2]", 0.679021223167272,
     0.461069821511578, 0.371278945818433, 0.137848055608047, 0.227661753206661,
     0.0518298738731308, "𝜏<unicode>[3]", 0.743251189713122, 0.552422331009971
    ))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerG"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerG_tempTable2"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.999999999193545, 0.431252723086778, "<unicode>", 1))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerG"]][["collection"]][["randomEstimatesContainer_containerG_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 0, "covert", 1, "𝜏<unicode>[1]", 0.194074257047345, 0.440538598816659,
     "general", 17, "𝜏<unicode>[2]", 0.137848055608047, 0.371278945818433,
     "overt", 3, "𝜏<unicode>[3]", 0.999999999193545, "", "", "",
     "<unicode>"))

})

