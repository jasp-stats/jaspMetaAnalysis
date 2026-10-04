context("Other: Selection Models - bcg")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("SelectionModels (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_fitMeasuresTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(32.9832328884349, 37.9832328884349, 35.2430303182811, 24.9832328884349,
     -12.4916164442175, "ML", 13))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.6711958, -1.219927, -1.817878, "Pooled effect", 0.2799699,
     -0.1224648, 0.4754866, 0.513715501734583, 0.307534728490296,
     "", "𝜏", 0.152086872628964, 1.02199735990967, "", 0.263903616722414,
     0.0945776092276, "", "𝜏<unicode>", 0.156258768159664, 1.04447860366233,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3.27468421810466e-26, "LR(1) = 112.17", "Heterogeneity", 0.0165125508254553,
     "z = -2.40", "Pooled effect", 0.840750861182679, "LR(2) = 0.35",
     "Publication bias"))

  plotName <- results[["results"]][["selectionDiagnostics"]][["collection"]][["selectionDiagnostics_selectionProfiles"]][["collection"]][["selectionDiagnostics_selectionProfiles_delta2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_selection-parameter-2")

  plotName <- results[["results"]][["selectionDiagnostics"]][["collection"]][["selectionDiagnostics_selectionProfiles"]][["collection"]][["selectionDiagnostics_selectionProfiles_delta3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_selection-parameter-3")

  plotName <- results[["results"]][["selectionDiagnostics"]][["collection"]][["selectionDiagnostics_selectionProfiles"]][["collection"]][["selectionDiagnostics_selectionProfiles_tau2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_-")

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionFrequencies"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8, "0     &lt; p &lt;= 0.025", 3, "0.025 &lt; p &lt;= 0.5", 2,
     "0.5   &lt; p &lt;= 1"))

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "", "[0, 0.025)", "", "", 0.631550088319224, 0, "[0.025, 0.5)",
     0.591229157859145, 1.79033794433309, 0.92784376191832, 0, "[0.5, 1)",
     1.35993039801969, 3.59325836351813))

  plotName <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_weight-function")

})

test_that("SelectionModels (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_bubble-plots")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn3"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn3_contrastsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("alternate <unicode> random", 0.181912585565443, -0.198636097368937,
     -0.30999069459157, 0.348801813940104, 0.194161059048074, 0.936915911240481,
     0.562461268499824, 0.673815865722457, "alternate <unicode> systematic",
     0.0441476663061063, -0.508772359922357, -0.590574392827356,
     0.875644832316948, 0.282107237984894, 0.15649249775176, 0.597067692534569,
     0.678869725439569, "random <unicode> systematic", -0.137764919259337,
     -0.659767869959869, -0.745744045123211, 0.604970616290495, 0.266332930001788,
     -0.517265811848396, 0.384238031441195, 0.470214206604537))

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn3"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn3_estimatedMarginalMeansTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.571546642217313, -0.887590387358584, -1.01543309818219, 0.161249771747942,
     -0.255502897076042, -0.127660186252436, "alternate", -0.753459227782756,
     -1.04724547218026, -1.18178374767781, 0.149893695350963, -0.45967298338525,
     -0.325134707887701, "random", -0.615694308523419, -1.01036972868621,
     -1.11860605448568, 0.201368710484448, -0.221018888360628, -0.112782562561154,
     "systematic"))

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.448869253346125, -0.0849914202839934, "Intercept", 0.0993660559530671,
     0.272382899808947, 1.64793477733355, 0.982729926976244, -0.181912585565443,
     -0.562461268499824, "jaspColumn3 (random)", 0.348801813940104,
     0.194161059048074, -0.936915911240481, 0.198636097368937, -0.0441476663061063,
     -0.597067692534569, "jaspColumn3 (systematic)", 0.875644832316948,
     0.282107237984894, -0.15649249775176, 0.508772359922357, -0.0304951876835051,
     -0.041677400570556, "jaspColumn1", 9.03928759632705e-08, 0.00570531549316969,
     -5.34504844123229, -0.0193129747964541))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 0.62253178949246, 0.947921168978811, "jaspColumn3", 1, 9.03928759632703e-08,
     28.5695428391197, "jaspColumn1"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_fitMeasuresTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(25.6732895981832, 39.6732895981832, 29.0629857429524, 13.6732895981832,
     -6.83664479909158, "ML", 13))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.6830835, -0.8784188, -1.050925, "Pooled effect", 0.0996627,
     -0.4877482, -0.3152419, 0.159029153327173, "", "", "𝜏", 0.156093426963696,
     "", "", 0.0252902716079574, "", "", "𝜏<unicode>", 0.0496468110599468,
     "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "LR(1) = 0.00", "Residual heterogeneity", 7.1836765833064e-12,
     "z = -6.85", "Pooled effect", 0.552063028519143, "LR(1) = 0.35",
     "Publication bias", 8.98470874624094e-08, "Q<unicode>(3) = 35.63",
     "Moderation"))

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "", "[0, 0.025)", "", "", 0.570731990855494, 0, "[0.025, 1)",
     0.540005185256366, 1))

})

test_that("SelectionModels (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["selectionComparison"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(29.3301526965537, 30.5301526965537, 30.4600514114767, 25.3301526965537,
     -12.6650763482768, "Model 1", 2, 0.334870971109887, 0.552632510555081,
     0.413838140496047, 31.0876532142855, 33.7543198809522, 32.7825012866701,
     25.0876532142855, -12.5438266071428, "Model 2", 3, 0.1390724524389,
     0.110234470861794, 0.129573721194448, 32.9832328884349, 37.9832328884349,
     35.2430303182811, 24.9832328884349, -12.4916164442175, "Model 3",
     4, 0.0539040277153269, 0.01330517619095, 0.0378634201697995,
     31.9059203521624, 36.9059203521624, 34.1657177820085, 23.9059203521624,
     -11.9529601760812, "Model 4", 4, 0.0923754701615727, 0.0228011144679658,
     0.0648866399850838, 31.1733075166257, 33.8399741832924, 32.8681555890103,
     25.1733075166257, -12.5866537583129, "Model 5", 3, 0.133242114989141,
     0.105613108741195, 0.124141599261339, 31.3295546198062, 33.9962212864729,
     33.0244026921908, 25.3295546198062, -12.6647773099031, "Model 6",
     3, 0.123228990404068, 0.0976762997545726, 0.11481237704286,
     31.3283055835086, 33.9949722501752, 33.0231536558932, 25.3283055835086,
     -12.6641527917543, "Model 7", 3, 0.123305973181104, 0.097737319428442,
     0.114884101850422))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_modelSummaryContainer"]][["collection"]][["selectionModelResults_model_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.620562, -1.132749, -1.795781, "Pooled effect", 0.2613245, -0.1083754,
     0.5546569, 0.539670917485144, 0.327336456136554, "", "𝜏", 0.144647347811801,
     0.944551928943936, "", 0.291244699179257, 0.107149155516038,
     "", "𝜏<unicode>", 0.156123933810775, 0.892178346471711, ""
    ))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_modelSummaryContainer"]][["collection"]][["selectionModelResults_model_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.10873300669221e-26, "LR(1) = 113.05", "Heterogeneity", 0.0175641805429672,
     "z = -2.37", "Pooled effect", 0.622406656819688, "LR(1) = 0.24",
     "Publication bias"))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_weightFunctionSummary"]][["collection"]][["selectionModelResults_model_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7111991, -1.048111, -1.801716, "Pooled effect", 0.1718968,
     -0.3742876, 0.3793173, 0.529176880683194, 0.345970103934773,
     "", "𝜏", 0.136298437540277, 1.05427048816489, "", 0.280028171049595,
     0.119695312816638, "", "𝜏<unicode>", 0.144251964039114, 1.11148626221544,
     ""))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.99676459084581e-26, "Q<unicode>(12) = 152.23", "Heterogeneity",
     3.51323567590701e-05, "z = -4.14", "Pooled effect"))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.6711958, -1.219927, -1.817878, "Pooled effect", 0.2799699,
     -0.1224648, 0.4754866, 0.513715501734583, 0.307534728490296,
     "", "𝜏", 0.152086872628964, 1.02199735990967, "", 0.263903616722414,
     0.0945776092276, "", "𝜏<unicode>", 0.156258768159664, 1.04447860366233,
     ""))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3.27468421810466e-26, "LR(1) = 112.17", "Heterogeneity", 0.0165125508254553,
     "z = -2.40", "Pooled effect", 0.840750861182679, "LR(2) = 0.35",
     "Publication bias"))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model3"]][["collection"]][["selectionModelResults_model3_modelSummaryContainer"]][["collection"]][["selectionModelResults_model3_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7892366, -1.890375, -2.455799, "Pooled effect", 0.5618156,
     0.3119018, 0.8773263, 0.638261770509382, 0.328394949869728,
     "", "𝜏", 0.302170606333307, 6.38261770509382, "", 0.407378087693771,
     0.107843243099941, "", "𝜏<unicode>", 0.38572789238838, 40.7378087693771,
     ""))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model3"]][["collection"]][["selectionModelResults_model3_modelSummaryContainer"]][["collection"]][["selectionModelResults_model3_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.6902168833355e-26, "LR(1) = 113.48", "Heterogeneity", 0.16008180241724,
     "z = -1.40", "Pooled effect", 0.490604893740814, "LR(2) = 1.42",
     "Publication bias"))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model3"]][["collection"]][["selectionModelResults_model3_weightFunctionSummary"]][["collection"]][["selectionModelResults_model3_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model4"]][["collection"]][["selectionModelResults_model4_modelSummaryContainer"]][["collection"]][["selectionModelResults_model4_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.6174447, -1.303471, -1.958847, "Pooled effect", 0.3500199,
     0.06858171, 0.7239578, 0.588125505740581, 0.327340514796111,
     "", "𝜏", 0.239194077654193, 1.74411379576515, "", 0.345891610502614,
     0.107151812626983, "", "𝜏<unicode>", 0.281352275781048, 3.04193293257831,
     ""))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model4"]][["collection"]][["selectionModelResults_model4_modelSummaryContainer"]][["collection"]][["selectionModelResults_model4_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(6.85323443008702e-27, "LR(1) = 115.27", "Heterogeneity", 0.0777273668246688,
     "z = -1.76", "Pooled effect", 0.692077849842567, "LR(1) = 0.16",
     "Publication bias"))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model4"]][["collection"]][["selectionModelResults_model4_weightFunctionSummary"]][["collection"]][["selectionModelResults_model4_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-5_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model5"]][["collection"]][["selectionModelResults_model5_modelSummaryContainer"]][["collection"]][["selectionModelResults_model5_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7024573, -1.293022, -1.903341, "Pooled effect", 0.3013142,
     -0.1118924, 0.4984263, 0.533497517213783, 0.324134608384967,
     "", "𝜏", 0.185141735731864, 1.27411174117295, "", 0.284619600873271,
     0.105063244352876, "", "𝜏<unicode>", 0.197545312691199, 1.62336072899477,
     ""))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model5"]][["collection"]][["selectionModelResults_model5_modelSummaryContainer"]][["collection"]][["selectionModelResults_model5_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7.414964066826e-27, "LR(1) = 115.12", "Heterogeneity", 0.0197369212533876,
     "z = -2.33", "Pooled effect", 0.980489193045497, "LR(1) = 0.00",
     "Publication bias"))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model5"]][["collection"]][["selectionModelResults_model5_weightFunctionSummary"]][["collection"]][["selectionModelResults_model5_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-6_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model6"]][["collection"]][["selectionModelResults_model6_modelSummaryContainer"]][["collection"]][["selectionModelResults_model6_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.6938589, -1.460914, -1.997542, "Pooled effect", 0.3913616,
     0.07319581, 0.6098238, 0.537837495928375, 0.324188048163867,
     "", "𝜏", 0.226416159046693, 1.36211116449188, "", 0.289269172026505,
     0.105097890572298, "", "𝜏<unicode>", 0.243550200038789, 1.85534682443343,
     ""))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model6"]][["collection"]][["selectionModelResults_model6_modelSummaryContainer"]][["collection"]][["selectionModelResults_model6_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7.41059077081358e-27, "LR(1) = 115.12", "Heterogeneity", 0.07623941393456,
     "z = -1.77", "Pooled effect", 0.965719028803835, "LR(1) = 0.00",
     "Publication bias"))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model6"]][["collection"]][["selectionModelResults_model6_weightFunctionSummary"]][["collection"]][["selectionModelResults_model6_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-7_weight-function")

})

test_that("SelectionModels (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["selectionComparison"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(32.9935796005493, 35.6602462672159, 34.6884276729339, 26.9935796005493,
     -13.4967898002746, "Model 1", 3, 0.256997435693694, 0.526234205893419,
     0.31450123157794, 32.935731979305, 37.935731979305, 35.1955294091511,
     24.935731979305, -12.4678659896525, "Model 2", 4, 0.264539325165704,
     0.168680014686841, 0.2440656201094, 31.7505530490704, 36.7505530490704,
     34.0103504789166, 23.7505530490704, -11.8752765245352, "Model 3",
     4, 0.478463239140603, 0.30508577941974, 0.44143314831266))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_modelSummaryContainer"]][["collection"]][["selectionModelResults_model_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7350765, -1.251556, -1.861929, "Pooled effect", 0.2635145,
     -0.2185976, 0.3917759, 0.510989867857309, 0.311153806350172,
     "", "𝜏", 0.148906505951752, 0.937427981307744, "", 0.26111064505283,
     0.0968166912062001, "", "𝜏<unicode>", 0.152179431598758, 0.878771220138711,
     ""))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_modelSummaryContainer"]][["collection"]][["selectionModelResults_model_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.12229997273262e-26, "LR(1) = 114.30", "Heterogeneity", 0.00527877799265947,
     "z = -2.79", "Pooled effect", 0.821017906948588, "LR(2) = 0.39",
     "Publication bias"))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_weightFunctionSummary"]][["collection"]][["selectionModelResults_model_weightFunctionSummary_selectionParameters"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.852671000803155, 1e-05, "<unicode>1", 0.802657515905965, 2.42585082389923,
     0.00310080481116186, 1e-05, "<unicode>2", 3.71380902686755,
     7.28203274293131))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_weightFunctionSummary"]][["collection"]][["selectionModelResults_model_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.2267047, -0.8298866, -1.642201, "Pooled effect", 0.3077515,
     0.3764772, 1.188791, 0.653352232488267, 0.373230859001808, "",
     "𝜏", 0.197245505554903, 1.23803269250627, "", 0.426869139697403,
     0.139301274111228, "", "𝜏<unicode>", 0.257741582805145, 1.53272494771432,
     ""))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.98199637546454e-26, "LR(1) = 113.17", "Heterogeneity", 0.46133614238138,
     "z = -0.74", "Pooled effect", 1, "LR(1) = 0.00", "Publication bias"
    ))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary_selectionParameters"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "", "[0, 0.025)", "", "", 0.304618457905443, 0, "[0.025, 0.5)",
     0.235662158717604, 0.766507801510909, 0.1, "", "[0.5, 1)", "",
     ""))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.9071518, -1.396256, -2.050596, "Pooled effect", 0.2495477,
     -0.4180473, 0.236292, 0.527334798351336, 0.319310404395331,
     "", "𝜏", 0.145957598913048, 0.96665376566778, "", 0.278081989552245,
     0.10195913435511, "", "𝜏<unicode>", 0.153937041981314, 0.934419502679699,
     ""))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.55350064306039e-27, "LR(1) = 116.09", "Heterogeneity", 0.0002777821944152,
     "z = -3.64", "Pooled effect", 0.453935653328873, "LR(2) = 1.58",
     "Publication bias"))

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary_selectionParameters"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "", "[0, 0.025)", "", "", 2.18481969028412, 0, "[0.025, 0.5)",
     2.07418510913153, 6.2501478014512, 4.08139830302146, 0, "[0.5, 1)",
     4.66923549841224, 13.2329317152454))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-3_weight-function")

})

test_that("SelectionModels (analysis 5) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[5]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.8001634, -1.182419, -1.867309, "Pooled effect", 0.195032, -0.4179077,
     0.2669821, 0.50834268527144, 0.338879737273723, "", "𝜏", 0.132833620578966,
     0.904948551065876, "", 0.258412285668978, 0.114839476334708,
     "", "𝜏<unicode>", 0.135049998758878, 0.818931880076228, ""
    ))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(5.24077124668018e-27, "LR(1) = 115.81", "Heterogeneity", 4.0830480052595e-05,
     "z = -4.10", "Pooled effect", 0.378847502494416, "LR(1) = 0.77",
     "Publication bias"))

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "", "[0, 0.025)", "", "", 2.62778983499746, 0, "[0.025, 1)",
     2.90052161773468, 8.31270774213728))

})

test_that("SelectionModels (analysis 6) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[6]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.7112847, -1.058942, -1.805194, "Pooled effect", 0.1773794,
     -0.3636274, 0.3826247, 0.529190501370829, 0.312547768340023,
     "", "𝜏", 0.139862057729802, 1.42040261582252, "", 0.28004258674111,
     0.0976861074943289, "", "𝜏<unicode>", 0.14802734490558, 2.01754359103546,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7.42046357801277e-27, "LR(1) = 115.12", "Heterogeneity", 6.07285888279219e-05,
     "z = -4.01", "Pooled effect", 1, "LR(1) = 0.00", "Publication bias"
    ))

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00133484913415478, 0, "<unicode>1", 3.61311425231623, 7.08290865570233
    ))

  plotName <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-6_figure-1_weight-function")

})

