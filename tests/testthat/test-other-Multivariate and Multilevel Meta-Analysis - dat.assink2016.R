context("Other: Multivariate and Multilevel Meta-Analysis - dat.assink2016")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Multivariate and Multilevel Meta-Analysis - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(97, -0.226523390050997, -0.382106512084979, "Intercept", 0.0047576191764637,
     0.0783903383254407, -2.88968506693485, -0.0709402680170139,
     97, 0.525676185612534, 0.366329489350914, "jaspColumn2 (general)",
     2.79017112935996e-09, 0.0802866099335716, 6.54749510593951,
     0.685022881874154, 97, 0.576600196056719, 0.409912830592533,
     "jaspColumn2 (overt)", 6.30434293814593e-10, 0.083985195839301,
     6.86549802372311, 0.743287561520905))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 97, 3.84900988921102e-09, 23.8168826480399, "jaspColumn2"
    ))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.2584621, 0.2271945, 0.2271945, "Pooled effect", 0.01575412,
     0.2897296, 0.2897296))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9.24741840861346e-104, "Q<unicode>(97) = 761.83", "Residual heterogeneity",
     9.84626988046686e-30, "t(97) = 16.41", "Pooled effect", 3.84900988921102e-09,
     "F<unicode>(2, 97) = 23.82", "Moderation"))

})

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Multivariate and Multilevel Meta-Analysis - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(97, -0.142932510852088, -0.563487172371937, "Intercept", 0.501571794452055,
     0.211895877714044, -0.674541252968485, 0.27762215066776, 97,
     0.771876230815213, 0.327179053114665, "jaspColumn2 (general)",
     0.000845330664953128, 0.224060050708459, 3.44495249543417, 1.21657340851576,
     97, 0.774844513142065, 0.229393289516799, "jaspColumn2 (overt)",
     0.0058332032818515, 0.274824835759954, 2.81941226672415, 1.32029573676733
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 97, 0.00338690628186807, 6.03478832902092, "jaspColumn2"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.5598607, 0.4326574, -0.6098794, "Pooled effect", 0.06409124,
     0.6870641, 1.729601))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9.24741840861346e-104, "Q<unicode>(97) = 761.83", "Residual heterogeneity",
     7.17853139223573e-14, "t(97) = 8.74", "Pooled effect", 0.00338690628186807,
     "F<unicode>(2, 97) = 6.03", "Moderation"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn4", 100, 0.585876751268124, 0.343251567676492))

})

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Multivariate and Multilevel Meta-Analysis - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(97, -0.259403755365688, -0.689807097253523, "Intercept", 0.234537763954767,
     0.216858121535948, -1.19619110194445, 0.170999586522146, 97,
     0.729655987251389, 0.347952935268316, "jaspColumn2 (general)",
     0.000258001981589365, 0.192320548614976, 3.79395749703352, 1.11135903923446,
     97, 0.507754241643615, 0.0897578553194056, "jaspColumn2 (overt)",
     0.0177960638024061, 0.210606894336581, 2.41090987663561, 0.925750627967824
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 97, 0.000946391704747064, 7.48746341914693, "jaspColumn2"
    ))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.375736, 0.1420317, -0.690225, "Pooled effect", 0.1177516, 0.6094402,
     1.441697))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9.24741840861346e-104, "Q<unicode>(97) = 761.83", "Residual heterogeneity",
     0.00191122895934047, "t(97) = 3.19", "Pooled effect", 0.000946391704747064,
     "F<unicode>(2, 97) = 7.49", "Moderation"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn4", 17, 0.435770926636072, 0.189896300501261, "jaspColumn4/jaspColumn5",
     100, 0.291026338090092, 0.0846963294621284))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer_tableInclusion"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(142.586735804069, 143.24607646341, 155.460290696586, "", 5, -66.2933679020346,
     "", "", 649.61986667386, 649.877931189989, 657.34399960937,
     511.033130869791, 3, -321.80993333693, "Component 1", 1.07291569104457e-111
    ))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer_tableInclusion1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(142.586735804069, 143.24607646341, 155.460290696586, "", 5, -66.2933679020346,
     "", "", 204.590413768619, 205.025196377314, 214.889257682632,
     64.0036779645494, 4, -98.2952068843093, "jaspColumn4", 1.24187152338927e-15,
     194.584687037828, 195.019469646524, 204.883530951841, 53.9979512337588,
     4, -93.292343518914, "jaspColumn5", 2.00698769247349e-13))

})

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Multivariate and Multilevel Meta-Analysis - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(97, -0.291958411158904, -0.769797147194585, "Intercept", 0.228204430484666,
     0.240758378499799, -1.21266147819295, 0.185880324876778, 97,
     0.694827872650212, 0.229740815014521, "jaspColumn2 (general)",
     0.00380813237016791, 0.234333463181709, 2.96512441379925, 1.1599149302859,
     97, 0.537954901240965, 0.0347302292837232, "jaspColumn2 (overt)",
     0.0364112693308257, 0.253549046790698, 2.1216995608942, 1.04117957319821
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 97, 0.0145120422260121, 4.42297421231345, "jaspColumn2"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.3199415, 0.1319072, -0.6160913, "Pooled effect", 0.0947408,
     0.5079757, 1.255974))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8.47024802226508e-108, "Q<unicode>(97) = 783.02", "Residual heterogeneity",
     0.00105569287726056, "t(97) = 3.38", "Pooled effect", 0.0145120422260121,
     "F<unicode>(2, 97) = 4.42", "Moderation"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn4", 17, 0.273412610220827, 0.0747544554277658, "jaspColumn4/jaspColumn5",
     100, 0.372416483224684, 0.138694036977441))

})

test_that("ClassicalMetaAnalysisMultilevelMultivariate (analysis 5) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Multivariate and Multilevel Meta-Analysis - dat.assink2016.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[5]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.39771568614744, -0.291958411158904, -0.725283438967567, "Intercept",
     0.110222635456644, 0.117554800901594, -2.48359411031884, 0.14136661664976,
     1.91451712906245, 0.694827872650212, 0.29094190145514, "jaspColumn3 (general)",
     0.0184612201203287, 0.0899102943232201, 7.72801243595492, 1.09871384384529,
     2.25549954681634, 0.537954901240965, -0.0186032825168833, "jaspColumn3 (overt)",
     0.053554519218152, 0.143897745461458, 3.73845260407539, 1.09451308499881
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, 0.884030969025433, 0.143280092555926, 35.4049489624971, "jaspColumn3"
    ))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.3199415, 0.1223349, -0.6925931, "Pooled effect", 0.09193274,
     0.517548, 1.332476))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8.47024802226508e-108, "Q<unicode>(97) = 783.02", "Residual heterogeneity",
     0.00379241956472367, "t(13.68) = 3.48", "Pooled effect", 0.143280092555926,
     "F<unicode>(2, 0.88) = 35.40", "Moderation"))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerS"]][["collection"]][["randomEstimatesContainer_confidenceIntervalsContainer_confidenceContainerS_tempTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.273412610220827, 0.0747544554277658, 0.106848251399879, 0.0114165488272118,
     "jaspColumn1", 0.514151202825638, 0.26435145936705, 0.372416483224684,
     0.138694036977441, 0.310365792855286, 0.0963269253746906, "jaspColumn1/jaspColumn5",
     0.450104920924196, 0.202594439840177))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_containerS"]][["collection"]][["randomEstimatesContainer_containerS_tableS"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn1", 17, 0.273412610220827, 0.0747544554277658, "jaspColumn1/jaspColumn5",
     100, 0.372416483224684, 0.138694036977441))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer_tableInclusion"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(142.222410234637, 142.881750893978, 155.095965127154, "", 5, -66.1112051173186,
     "", "", 593.745184529233, 594.003249045362, 601.469317464743,
     455.522774294595, 3, -293.872592264616, "Component 1", 1.21474849955262e-99
    ))

  table <- results[["results"]][["randomEstimatesContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer"]][["collection"]][["randomEstimatesContainer_inclusionTestsContainer_tableInclusion1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(142.222410234637, 142.881750893978, 155.095965127154, "", 5, -66.1112051173186,
     "", "", 147.391977208772, 147.826759817467, 157.690821122785,
     7.1695669741344, 4, -69.6959886043858, "jaspColumn1", 0.00741506716338275,
     553.627593811574, 554.06237642027, 563.926437725588, 413.405183576937,
     4, -272.813796905787, "jaspColumn5", 6.65139657208317e-92))

})

