context("Other: Autoregressive Meta-Analysis - dat.ishak2007")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Autoregressive Meta-Analysis - dat.ishak2007.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_bubble-plots")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(78, -24.2995242044541, -25.446152155362, "Intercept", 1.82914368669382e-55,
     0.575949789851185, -42.1903517157852, -23.1528962535461, 78,
     -2.24869599563987, -3.88970936199103, "jaspColumn1 (2)", 0.00786767112294646,
     0.824278967510374, -2.72807639679533, -0.607682629288719, 78,
     -3.27148188256713, -4.83728299969122, "jaspColumn1 (3)", 8.1277299300518e-05,
     0.786499948516235, -4.15954494178788, -1.70568076544304, 78,
     -0.609889411411914, -3.22478978899592, "jaspColumn1 (4)", 0.64369934425915,
     1.31346119877746, -0.464337592903074, 2.00501096617209))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3, 78, 0.000694518468714292, 6.30295454149285, "jaspColumn1"
    ))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-25.98205, -26.62773, -26.62773, "Pooled effect", 0.3243224, -25.33637,
     -25.33637))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.8088743110613e-40, "Q<unicode>(78) = 372.93", "Residual heterogeneity",
     1.13713055910041e-76, "t(78) = -80.11", "Pooled effect", 0.000694518468714292,
     "F<unicode>(3, 78) = 6.30", "Moderation"))

})

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Autoregressive Meta-Analysis - dat.ishak2007.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_bubble-plots")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(78, -25.9589184080251, -27.8851215339259, "Intercept", 3.91665993212198e-41,
     0.967529428089359, -26.8301073377043, -24.0327152821244, 78,
     -1.35298935400562, -3.21574177094594, "jaspColumn1 (2)", 0.152175274028203,
     0.935658216104044, -1.44602946964896, 0.509763062934687, 78,
     -2.59382071526369, -4.33056977177161, "jaspColumn1 (3)", 0.00391791838107781,
     0.872366885289967, -2.97331404825337, -0.857071658755779, 78,
     0.1668479590937, -3.00780045366052, "jaspColumn1 (4)", 0.916936670713832,
     1.59462193881578, 0.104631671640995, 3.34149637184792))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3, 78, 0.0197486814482477, 3.48196576634267, "jaspColumn1"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-27.09033, 1, -28.94402, -36.25524, "Pooled effect", 0.9311041,
     -25.23665, -17.92542, -27.09033, 2, -28.94402, -39.1789, "Pooled effect",
     0.9311041, -25.23665, -15.00177, -27.09033, 3, -28.94402, -37.48739,
     "Pooled effect", 0.9311041, -25.23665, -16.69328, -27.09033,
     4, -28.94402, -38.17476, "Pooled effect", 0.9311041, -25.23665,
     -16.00591))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.8088743110613e-40, "Q<unicode>(78) = 372.93", "Residual heterogeneity",
     1.24243761901993e-43, "t(78) = -29.09", "Pooled effect", 0.0197486814482477,
     "F<unicode>(3, 78) = 3.48", "Moderation"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerG"]][["collection"]][["randomEstimatesContainer_containerG_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(20.3254707402712, 4.50837783912032, 1, 24, "𝜏<unicode>[1]", 36.0030944947955,
     6.00025786902492, 2, 22, "𝜏<unicode>[2]", 26.4067954347798,
     5.13875426876785, 3, 25, "𝜏<unicode>[3]", 30.1322365302863,
     5.48928379028505, 4, 11, "𝜏<unicode>[4]", 0.999999999135574,
     "", "", "", "<unicode>"))

})

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Autoregressive Meta-Analysis - dat.ishak2007.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_bubble-plots")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(78, -25.9047260185467, -27.9192162292921, "Intercept", 1.05140868201176e-39,
     1.01187592070935, -25.6006942040748, -23.8902358078014, 78,
     -1.55693947185254, -3.06008698330256, "jaspColumn1 (2)", 0.0425289971872882,
     0.755029120517642, -2.062091950553, -0.0537919604025245, 78,
     -2.75208824298282, -4.46160269172588, "jaspColumn1 (3)", 0.00195780535604752,
     0.858686975772297, -3.20499590727764, -1.04257379423977, 78,
     -0.588432069670711, -3.12248258525558, "jaspColumn1 (4)", 0.645156936609913,
     1.27285041391823, -0.462294754541763, 1.94561844591415))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3, 78, 0.00325619366427714, 4.98055046189171, "jaspColumn1"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-27.24043, 1, -29.14602, -36.92161, "Pooled effect", 0.9571738,
     -25.33484, -17.55924, -27.24043, 2, -29.14602, -38.95967, "Pooled effect",
     0.9571738, -25.33484, -15.52119, -27.24043, 3, -29.14602, -37.59452,
     "Pooled effect", 0.9571738, -25.33484, -16.88634, -27.24043,
     4, -29.14602, -38.51558, "Pooled effect", 0.9571738, -25.33484,
     -15.96528))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.37703011073309e-131, "Q<unicode>(78) = 856.32", "Residual heterogeneity",
     6.01045647270738e-43, "t(78) = -28.46", "Pooled effect", 0.00325619366427714,
     "F<unicode>(3, 78) = 4.98", "Moderation"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerG"]][["collection"]][["randomEstimatesContainer_containerG_table1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(22.7311043952644, 4.76771479802058, 1, 24, "𝜏<unicode>[1]", 33.7353807526786,
     5.8082166585518, 2, 22, "𝜏<unicode>[2]", 26.1326418656241, 5.11200957213737,
     3, 25, "𝜏<unicode>[3]", 31.1589830965599, 5.58202320817102,
     4, 11, "𝜏<unicode>[4]", 0.883164853371297, "", "", "", "<unicode>"
    ))

})

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Autoregressive Meta-Analysis - dat.ishak2007.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_bubble-plots")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(17.3640495919758, -26.3232447338889, -29.2097599993433, "Intercept",
     3.86411100117269e-13, 1.37032155387511, -19.2095385637406, -23.4367294684344,
     6.24559222516162, -1.0488829264059, -3.7809470285319, "jaspColumn1 (2)",
     0.386653372391708, 1.12719314318405, -0.930526354554515, 1.6831811757201,
     7.28245792920836, -2.23794947327985, -4.09861185407287, "jaspColumn1 (3)",
     0.0246720474468098, 0.793067641996466, -2.82188977934598, -0.37728709248682,
     10.1185496865919, -0.31104391100342, -3.06683398169649, "jaspColumn1 (4)",
     0.806770400158237, 1.23877702074193, -0.251089506662892, 2.44474615968965
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3, 6.64221915032586, 0.0987277216972218, 3.16388791102218, "jaspColumn1"
    ))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-27.32868, -30.56271, -30.56271, "Pooled effect", 1.547319, -24.09465,
     -24.09465))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.37703011073309e-131, "Q<unicode>(78) = 856.32", "Residual heterogeneity",
     2.03297670322405e-13, "t(19.40) = -17.66", "Pooled effect",
     0.0987277216972218, "F<unicode>(3, 6.64) = 3.16", "Moderation"
    ))

})

