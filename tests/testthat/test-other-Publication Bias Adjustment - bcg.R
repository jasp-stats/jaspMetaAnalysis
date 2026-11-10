context("Other: Publication Bias Adjustment - bcg")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("WaapWls (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Publication Bias Adjustment - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["baujatPlot"]][["collection"]][["baujatPlot_subgroupalternate"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_subgroup-alternate")

  plotName <- results[["results"]][["baujatPlot"]][["collection"]][["baujatPlot_subgrouprandom"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_subgroup-random")

  plotName <- results[["results"]][["baujatPlot"]][["collection"]][["baujatPlot_subgroupsystematic"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_subgroup-systematic")

  table <- results[["results"]][["casewiseDiagnosticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(109.116686201956, 0.000552023332278838, 1.19904652851099, -0.0215555662829853,
     0.00916229419108989, "", "Aronson (1948)", -0.224160240025078,
     "random", 0, 0, 0.916229419108989, 101.516413020045, 0.00737151204854916,
     1.12251808483429, -0.081665093087674, 0.0153308983917266, "",
     "Ferguson &amp; Simes (1949)", -0.654482161407515, "", 0, 0,
     1.53308983917266, 107.521748399311, 0.00105993549365549, 1.17916341225807,
     -0.0300897003225347, 0.00718183309697583, "", "Rosenthal et al (1960)",
     -0.353781456927103, "", 0, 0, 0.718183309697583, 32.9449315584184,
     0.736971877745949, 0.421548425144934, -1.43336718174968, 0.149080392059149,
     "Yes", "Hart &amp; Sutherland (1977)", -3.4244549654385, "",
     0, 0, 14.9080392059149, 102.217979377317, 0.00590105145131208,
     1.12803625383144, -0.0728160905768963, 0.0133761107428961, "",
     "Vandiviere et al (1973)", -0.625371728501665, "", 0, 0, 1.33761107428961,
     14.2114018729378, 15.9336479293283, 0.626473254983273, 10.1476493444144,
     0.753008630357705, "Yes", "TPT Madras (1980)", 5.81174250160179,
     "", 0, 0, 75.3008630357705, 109.641086703251, 0.00173843107802596,
     1.26039442370519, -0.0381608795996584, 0.0528598411604576, "",
     "Coetzee &amp; Berjak (1968)", -0.161533489599366, "", 0, 0,
     5.28598411604576, 0, 0.134848569402076, 0.204017224906193, 0.866081398502094,
     0.118825165786766, "", "Frimodt-Moller et al (1973)", 2.35849814669926,
     "alternate", 0, 0, 11.8825165786766, 0, 7.41572568722114, 1.51293577537243,
     -6.4226220740964, 0.881174834213234, "Yes", "Stein &amp; Aronson (1953)",
     -2.35849814669926, "", 0, 0, 88.1174834213234, 2.239117778552,
     0.368464529558566, 0.23117052082132, -1.34915338943458, 0.124329785544192,
     "Yes", "Rosenthal et al (1961)", -3.58050286282684, "systematic",
     0, 0, 12.4329785544192, 14.9502791586845, 0.80852245277253,
     5.03328938251301, 0.773434153698094, 0.731469579087179, "Yes",
     "Comstock et al (1974)", 0.468621227971729, "", 0, 0, 73.1469579087179,
     15.181357670414, 0.00442375082514385, 1.39628868897131, 0.0567730173781788,
     0.0170498728063805, "", "Comstock &amp; Webster (1969)", 0.431069351987167,
     "", 0, 0, 1.70498728063805, 14.0755994029956, 0.0662768796589122,
     1.45788662663446, 0.228217809332961, 0.127150762562249, "",
     "Comstock et al (1976)", 0.59794283485837, "", 0, 0, 12.7150762562249
    ))

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.2945367, -0.8673245, "Pooled effect", 0.234086, "random", 0.2782512,
     -0.7185554, -3.056228, "", 0.1839788, "alternate", 1.619117,
     -0.4133284, -1.126462, "", 0.2240834, "systematic", 0.2998051,
     18.3688768625314, "", "<unicode>", "", "random", "", 5.56251350798384,
     "", "", "", "alternate", "", 5.53062119317857, "", "", "", "systematic",
     "", 4.2858927731024, "", "<unicode><unicode>", "", "random",
     "", 2.35849814669926, "", "", "", "alternate", "", 2.35172727865681,
     "", "", "", "systematic", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.83920754465686e-21, "Q<unicode>(6) = 110.21", "random", "Heterogeneity",
     0.0183490514099572, "Q<unicode>(1) = 5.56", "alternate", "",
     0.000857329632387995, "Q<unicode>(3) = 16.59", "systematic",
     "", 0.255050948928015, "t(6) = -1.26", "random", "Pooled effect",
     0.159572017106958, "t(1) = -3.91", "alternate", "", 0.1623049394051,
     "t(3) = -1.84", "systematic", "", 0.313960225965677, "Q<unicode>(2) = 2.32",
     "", "Subgroup differences"))

})

test_that("WaapWls (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Publication Bias Adjustment - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("WaapWls", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_bubble-plots")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2, -88.2355008361923, -619.999731956146, "Intercept", 0.549340042574413,
     123.589855961003, -0.713938050579439, 443.528730283761, 2, 0.0447570814726275,
     -0.227190659293973, "jaspColumn2", 0.5522692420412, 0.0632046688049667,
     0.708129356879256, 0.316704822239228, 2, -0.574434065890861,
     -7.44919738832706, "jaspColumn1 (random)", 0.753620146069464,
     1.59779646516732, -0.359516420529012, 6.30032925654534, 2, -0.454336819106728,
     -6.66803225627946, "jaspColumn1 (systematic)", 0.78284963603594,
     1.44415453150795, -0.314604018610337, 5.759358618066))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 2, 0.5522692420412, 0.501447186074229, "jaspColumn2", 2, 2,
     0.939228282699013, 0.0647038834119758, "jaspColumn1"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.6818567, -3.079059, "Pooled effect", 0.5571451, 1.715345, 37.8454134901227,
     "", "<unicode>", "", "", 6.15186260331964, "", "<unicode><unicode>",
     "", ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3.66391808861995e-17, "Q<unicode>(2) = 75.69", "Residual heterogeneity",
     0.345622706711569, "t(2) = -1.22", "Pooled effect", 0.777288509533027,
     "F<unicode>(3, 2) = 0.39", "Moderation"))

})

test_that("PetPeese (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Publication Bias Adjustment - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.415447, -1.324669, -1.846907, "Pooled effect", 0.4638973, "random",
     0.4937749, 1.016012, "", "", "", "", "", "alternate", "", "",
     -0.8297695, -2.23254, -2.825813, "", 0.7157122, "systematic",
     0.5730006, 1.166274, 0.564101324869639, 0.2727179702736, "",
     "𝜏", 0.249764435926271, "random", 1.36408513796476, "", "",
     "", "", "", "", "alternate", "", "", 0.724507616690344, 0.299953941152983,
     "", "", 0.41102567538529, "systematic", 4.8568611775136, "",
     0.318210304719682, 0.0743750913101522, "", "𝜏<unicode>", 0.281784898422655,
     "random", 1.86072826361633, "", "", "", "", "", "", "alternate",
     "", "", 0.524911286642323, 0.0899723668132074, "", "", 0.595582464943871,
     "systematic", 23.5891004976388, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.25720444244031e-12, "Q<unicode>(5) = 64.76", "random", "Residual heterogeneity",
     "", "", "alternate", "", 0.000255034382061586, "Q<unicode>(2) = 16.55",
     "systematic", "", 0.370488722937116, "z = -0.90", "random",
     "Pooled effect", "", "", "alternate", "", 0.246308648768954,
     "z = -1.16", "systematic", "", 0.162473238553167, "z = -1.40",
     "random", "Publication bias (PET)", "", "", "alternate", "",
     0.479289535503825, "z = 0.71", "systematic", "", 0.627124013788767,
     "Q<unicode>(1) = 0.24", "", "Subgroup differences"))

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-1.76329836602589, -4.23752324593405, "SE", 0.162473238553167,
     1.26238282918693, -1.3968016082424, "random", 0.710926513882263,
     1.43300250345014, -2.53710181969937, "", 0.479289535503825,
     2.02560065106563, 0.707445716260148, "systematic", 5.40310682659966
    ))

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["collection"]][["petPeeseSummary_regressionPlot_subgrouprandom"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_subgroup-random")

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["collection"]][["petPeeseSummary_regressionPlot_subgroupsystematic"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_subgroup-systematic")

  plotName <- results[["results"]][["profileLikelihoodPlot"]][["collection"]][["profileLikelihoodPlot_subgrouprandom"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_subgroup-random")

  plotName <- results[["results"]][["profileLikelihoodPlot"]][["collection"]][["profileLikelihoodPlot_subgroupsystematic"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-5_subgroup-systematic")

})

test_that("PetPeese (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Publication Bias Adjustment - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn2"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_jaspColumn2_estimatedMarginalMeansTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.472650670351467, 0.233659148826611, 0.161143927037911, 0.956087777027153,
     1.3863299740184, "Mean - 1SD", 0.483264179877859, 0.315677940729823,
     0.192953669485759, 0.73981814191097, 1.21036447855819, "Mean",
     0.494116018875746, 0.292457028857544, 0.187723024053165, 0.834825687258631,
     1.30058974566951, "Mean + 1SD"))

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, -3.79562988382569, -85.7509951571378, "Intercept", 0.918857515499877,
     36.2288556343876, -0.104768141785383, 78.1597353894864, 9, 0.00203122067084694,
     -0.0393159897470378, "jaspColumn2", 0.913951653703273, 0.0182777797660795,
     0.11113060212141, 0.0433784310887317, 9, -0.0276559464814083,
     -0.0554634729760006, "jaspColumn3", 0.0510177581537889, 0.0122924821280744,
     -2.24982604760074, 0.000151580013184074))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCorrelationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, -0.999954187186916, -0.57750926488372, "Intercept", -0.999954187186916,
     1, 0.570763765385701, "jaspColumn2", -0.57750926488372, 0.570763765385701,
     1, "jaspColumn3"))

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 9, 0.913951653703273, 0.0123500107278671, "jaspColumn2", 1,
     9, 0.0510177581537889, 5.06171724446276, "jaspColumn3"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.4832642, 0.3156779, 0.1929537, "Pooled effect", 0.7398181, 1.210364,
     0.359560706634237, 0.168554852933205, "", "𝜏", 1.08237234457067,
     "", 0.129283901755312, 0.0284107384473344, "", "𝜏<unicode>",
     1.17152989229142, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.00138732822808985, "Q<unicode>(9) = 27.02", "Residual heterogeneity",
     0.00382949758832331, "t(9) = -3.86", "Pooled effect", 0.693278837647351,
     "t(9) = -0.41", "Publication bias (PET)", 0.0579345035225321,
     "F<unicode>(2, 9) = 3.97", "Moderation"))

  table <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_coefficientEstimates"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(9, -0.0880787015994173, -2.96415400588664, "SE<unicode>", 0.946283411113466,
     1.271386158126, -0.0692776942996168, 2.7879966026878))

  plotName <- results[["results"]][["petPeeseSummary"]][["collection"]][["petPeeseSummary_regressionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_regression-plot")

  table <- results[["results"]][["varianceInflationContainer"]][["collection"]][["varianceInflationContainer_effectSize"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 1.29657609173229, "jaspColumn2", 1.68110956165179, 1, 1.24532044368049,
     "jaspColumn3", 1.55082300744858, 1, 1.06465500835752, "Standard error<unicode> (PEESE)",
     1.13349028682076))

})

