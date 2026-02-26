context("Other: Meta-Regression - dat.bangertdrowns2004")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalMetaAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Meta-Regression - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(22, 0.47241396214642, 0.296509823410256, "Intercept", 1.342262652363e-05,
     0.0848191441696781, 5.5696619763266, 0.648318100882585, 22,
     -0.0201599020560654, -0.0312365471907467, "jaspColumn2", 0.00104362642988289,
     0.00534104295296917, -3.77452535648644, -0.00908325692138401
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 22, 0.00104362642988289, 14.2470416667591, "jaspColumn2"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.1977353, 0.1139639, -0.007199527, "Pooled effect", 0.04039369,
     0.2815067, 0.4026701, 0.090184452970439, 0, "", "𝜏", 0.0673316082638159,
     0.191634269670553, "", 0.00813323555757732, 0, "", "𝜏<unicode>",
     0.0121445285177842, 0.0367236933121662, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.405675933184118, "Q<unicode>(22) = 22.93", "Residual heterogeneity",
     6.79242567345173e-05, "t(22) = 4.90", "Pooled effect", 0.00104362642988289,
     "F<unicode>(1, 22) = 14.25", "Moderation"))

})

test_that("ClassicalMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Meta-Regression - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(10, 0.710945620497429, 0.221210597120263, "Intercept", 0.00895126983875199,
     0.219795558495195, 3.23457682841654, "yes", 1.20068064387459,
     10, 0.470856865282848, 0.236465353537849, "", 0.00118635998764145,
     0.105196097422767, 4.47599176032689, "no", 0.705248377027848,
     10, -0.0437868764823113, -0.0873241014697863, "jaspColumn2",
     0.0489281877415188, 0.019539727045585, -2.24091546315663, "yes",
     -0.000249651494836398, 10, -0.019193966266897, -0.0311596308766734,
     "", 0.00506087190762515, 0.00537025087063567, -3.57412842142047,
     "no", -0.00722830165712054))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 10, 0.0489281877415188, 5.02170211301451, "yes", "jaspColumn2",
     1, 10, 0.00506087190762515, 12.7743939728056, "no", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.2840236, 0.134232, -0.03751217, "Pooled effect", 0.06722721,
     "yes", 0.4338151, 0.6055593, 0.1349625, 0.03320579, 0.03320579,
     "", 0.04566891, "no", 0.2367191, 0.2367191, 0.127690932460808,
     0, "", "𝜏", 0.0945880189510315, "yes", 0.36737215592763, "",
     0, 0, "", "", Inf, "no", 0.196301929799156, "", 0.0163049742327106,
     0, "", "𝜏<unicode>", 0.0241560646789555, "yes", 0.134962300950915,
     "", 0, 0, "", "", 0.0133163523668545, "no", 0.0385344476428727,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.206163712069061, "Q<unicode>(10) = 13.32", "yes", "Residual heterogeneity",
     0.744650285498006, "Q<unicode>(10) = 6.79", "no", "", 0.00175812165045325,
     "t(10) = 4.22", "yes", "Pooled effect", 0.0144050079268653,
     "t(10) = 2.96", "no", "", 0.0489281877415188, "F<unicode>(1, 10) = 5.02",
     "yes", "Moderation", 0.00506087190762515, "F<unicode>(1, 10) = 12.77",
     "no", "", 0.0666390788447247, "Q<unicode>(1) = 3.36", "", "Subgroup differences"
    ))

})

test_that("ClassicalMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Meta-Regression - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(20, 0.488492400281355, 0.233597733171357, "Intercept", 0.000707414949312178,
     0.122195174342147, 3.99764068353121, 0.743387067391353, 20,
     -0.0192108908754727, -0.0321385397705634, "jaspColumn2", 0.00564734832990584,
     0.00619744747302993, -3.09980697038172, -0.00628324198038211,
     20, 0.207566123704415, -0.262920821049974, "jaspColumn3 (yes)",
     0.368398762288976, 0.225548988104787, 0.920270693513297, 0.678053068458803,
     20, -0.0237671729719295, -0.0604619129344197, "jaspColumn2<unicode><unicode><unicode>jaspColumn3 (yes)",
     0.191758017456955, 0.0175912670045044, -1.35107795054465, 0.0129275669905607
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 20, 0.00564734832990584, 9.60880325362709, "jaspColumn2", 1,
     20, 0.368398762288976, 0.846898149339445, "jaspColumn3", 1,
     20, 0.191758017456955, 1.82541162844793, "jaspColumn2<unicode><unicode><unicode>jaspColumn3"
    ))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.2146621, 0.1283922, 0.006098732, "Pooled effect", 0.04135733,
     0.300932, 0.4232255, 0.0910297233577162, 0, "", "𝜏", 0.0755484298980758,
     0.196892847058146, "", 0.00828641053458234, 0, "", "𝜏<unicode>",
     0.0137543053474633, 0.0387667932226623, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.450566893163572, "Q<unicode>(20) = 20.12", "Residual heterogeneity",
     4.44343472421509e-05, "t(20) = 5.19", "Pooled effect", 0.005454896279454,
     "F<unicode>(3, 20) = 5.70", "Moderation"))

})

test_that("ClassicalMetaAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Meta-Regression - dat.bangertdrowns2004.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(20, 0.470856865291475, 0.236160915438973, "Intercept", 0.000456505166789571,
     0.112512014608945, 4.18494742030903, 0.705552815143977, 20,
     -0.0191939662668959, -0.0311751724133729, "jaspColumn2", 0.00324945058653455,
     0.00574372775428118, -3.34172632966271, -0.00721276012041892,
     20, 0.240085052489594, -0.251644014551271, "jaspColumn3 (yes)",
     0.320620029873434, 0.235732350768365, 1.01846459218279, 0.731814119530458,
     20, -0.0245926981054849, -0.0648315910358425, "jaspColumn2<unicode><unicode><unicode>jaspColumn3 (yes)",
     0.216966636394573, 0.0192903154573969, -1.27487278058249, 0.0156461948248727
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 20, 0.00324945058653455, 11.167134862361, "jaspColumn2", 1,
     20, 0.320620029873434, 1.03727012553006, "jaspColumn3", 1, 20,
     0.216966636394573, 1.62530060667012, "jaspColumn2<unicode><unicode><unicode>jaspColumn3"
    ))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_heterogeneityCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(22, -26.5603875444667, -181733.224672667, "Intercept", 0.999760859953866,
     87617.0615729945, -0.000303141729106483, 181680.103897578, 22,
     22.443958385964, -181684.220333328, "jaspColumn3 (yes)", 0.999797922780334,
     87617.061576173, 0.000256159679201882, 181729.1082501))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_heterogeneityTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 22, 0.999797922780334, 6.56177812488112e-08, "jaspColumn3"
    ))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.2094922, 0.1260521, 0.1260465, "Pooled effect", 0.04000073,
     0.2929323, 0.2929379, 0.000466989317288451, 0, "", "𝜏", "",
     Inf, "", 2.18079022461534e-07, 0, "", "𝜏<unicode>", "", Inf,
     ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.450566893163572, "Q<unicode>(20) = 20.12", "Residual heterogeneity",
     3.99385050858783e-05, "t(20) = 5.24", "Pooled effect", 0.00362283244466927,
     "F<unicode>(3, 20) = 6.25", "Moderation effect size", 0.999797922780334,
     "F<unicode>(1, 22) = 0.00", "Moderation heterogeneity"))

})

