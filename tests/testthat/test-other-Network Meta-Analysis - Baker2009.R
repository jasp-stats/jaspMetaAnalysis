context("Other: Network Meta-Analysis - Baker2009")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalNetworkMetaAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Baker2009.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["componentForestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_component-forest-plot")

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_heterogeneityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.100292460868234, "", "𝜏", "", 0.0100585777070062, "", "𝜏<unicode>",
     "", 23.4920655321672, 0, "I<unicode>", 46.6618068929328))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(5, 13, 38, 74, 9, 8, 28217))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0749899101555783, "Q(48) = 62.74", "Additive model", 0.0699673700843432,
     "Q(46) = 60.86", "Standard model", 0.390827445753266, "Q(2) = 1.88",
     "Difference"))

  plotName <- results[["results"]][["networkPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_network-plot")

  table <- results[["results"]][["rankingContainer"]][["collection"]][["rankingContainer_rankingTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.858847281350161, "Tiotropium", 0.816127535999082, "Budesonide+Formoterol",
     0.703752502599232, "Fluticasone+Salmeterol", 0.690885999792646,
     "Budesonide", 0.445982812413093, "Salmeterol", 0.264867613841195,
     "Fluticasone", 0.176981862212671, "Formoterol", 0.0425543917919203,
     "Placebo"))

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_componentEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Budesonide", 0.731452471387602, 0.579741772368281, 0.00836888167112941,
     -2.63681355006519, 0.922863839383922, "Fluticasone", 0.89963469387502,
     0.813374949086696, 0.0397244780314012, -2.05660249515377, 0.995042425799292,
     "Formoterol", 0.94278528050132, 0.782469257402164, 0.535552362361855,
     -0.619552629202017, 1.13594761292087, "Salmeterol", 0.822925638758795,
     0.752741057142428, 1.82814304313241e-05, -4.28490863129722,
     0.899654138034396, "Tiotropium", 0.687722000221269, 0.613792760618284,
     1.10485575860045e-10, -6.45186006664131, 0.770555763987703
    ))

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_leagueTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Budesonide", "Budesonide", "1.06 [0.88, 1.28]", "0.81 [0.63, 1.05]",
     "0.99 [0.76, 1.29]", "0.78 [0.57, 1.05]", "0.73 [0.58, 0.92]",
     "0.89 [0.69, 1.14]", "1.06 [0.82, 1.38]", "Budesonide+Formoterol",
     "1.06 [0.88, 1.28]", "Budesonide+Formoterol", "0.77 [0.56, 1.04]",
     "0.93 [0.68, 1.28]", "0.73 [0.58, 0.92]", "0.69 [0.52, 0.92]",
     "0.84 [0.62, 1.14]", "1.00 [0.73, 1.37]", "Fluticasone", "0.81 [0.63, 1.05]",
     "0.77 [0.56, 1.04]", "Fluticasone", "1.22 [1.11, 1.33]", "0.95 [0.77, 1.18]",
     "0.90 [0.81, 1.00]", "1.09 [0.95, 1.26]", "1.31 [1.13, 1.51]",
     "Fluticasone+Salmeterol", "0.99 [0.76, 1.29]", "0.93 [0.68, 1.28]",
     "1.22 [1.11, 1.33]", "Fluticasone+Salmeterol", "0.79 [0.63, 0.98]",
     "0.74 [0.65, 0.84]", "0.90 [0.81, 1.00]", "1.08 [0.93, 1.25]",
     "Formoterol", "0.78 [0.57, 1.05]", "0.73 [0.58, 0.92]", "0.95 [0.77, 1.18]",
     "0.79 [0.63, 0.98]", "Formoterol", "0.94 [0.78, 1.14]", "1.15 [0.93, 1.41]",
     "1.37 [1.10, 1.70]", "Placebo", "0.73 [0.58, 0.92]", "0.69 [0.52, 0.92]",
     "0.90 [0.81, 1.00]", "0.74 [0.65, 0.84]", "0.94 [0.78, 1.14]",
     "Placebo", "1.22 [1.11, 1.33]", "1.45 [1.30, 1.63]", "Salmeterol",
     "0.89 [0.69, 1.14]", "0.84 [0.62, 1.14]", "1.09 [0.95, 1.26]",
     "0.90 [0.81, 1.00]", "1.15 [0.93, 1.41]", "1.22 [1.11, 1.33]",
     "Salmeterol", "1.20 [1.05, 1.36]", "Tiotropium", "1.06 [0.82, 1.38]",
     "1.00 [0.73, 1.37]", "1.31 [1.13, 1.51]", "1.08 [0.93, 1.25]",
     "1.37 [1.10, 1.70]", "1.45 [1.30, 1.63]", "1.20 [1.05, 1.36]",
     "Tiotropium"))

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Budesonide", 0.731452471387602, 0.579741772368281, 0.00836888167112941,
     -2.63681355006519, 0.922863839383922, "Budesonide+Formoterol",
     0.689602623410544, 0.51586719720161, 0.0120928629797108, -2.50942284584249,
     0.921849229403224, "Fluticasone", 0.89963469387502, 0.813374949086696,
     0.0397244780314012, -2.05660249515377, 0.995042425799292, "Fluticasone+Salmeterol",
     0.740332455106674, 0.650978322514538, 4.61850187024968e-06,
     -4.58141053152258, 0.841951452342002, "Formoterol", 0.94278528050132,
     0.782469257402164, 0.535552362361855, -0.619552629202017, 1.13594761292087,
     "Salmeterol", 0.822925638758795, 0.752741057142428, 1.82814304313241e-05,
     -4.28490863129722, 0.899654138034396, "Tiotropium", 0.687722000221269,
     0.613792760618284, 1.10485575860045e-10, -6.45186006664131,
     0.770555763987703))

})

test_that("ClassicalNetworkMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Baker2009.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_heterogeneityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.105474486190008, "", "𝜏", "", 0.0111248672370461, "", "𝜏<unicode>",
     "", 24.3410827921051, 0, "I<unicode>", 47.4150912902103))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(6, 13, 38, 74, 9, 8, 28217))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0687402915495964, "Q(47) = 62.12", "Additive model", 0.0699673700843432,
     "Q(46) = 60.86", "Standard model", 0.261407622909891, "Q(1) = 1.26",
     "Difference"))

  plotName <- results[["results"]][["networkPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_network-plot")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_componentEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Budesonide", 0.730863270469326, 0.577908633775861, 0.00886997709409627,
     -2.61702528352894, 0.924300294029335, "Fluticasone", 0.862172230289102,
     0.748163763497469, 0.0404297599706609, -2.04933099328561, 0.993553805929815,
     "Formoterol", 0.943570527130053, 0.781647782325837, 0.545385788791114,
     -0.604688803783563, 1.13903648139226, "Salmeterol", 0.801336637349188,
     0.720188663856281, 4.78980742503043e-05, -4.06565341050372,
     0.891628039408083, "Tiotropium", 0.685381494506769, 0.610483937764018,
     1.5711575787292e-10, -6.39830058892913, 0.769467899078309, "Fluticasone * Salmeterol",
     1.0857169987133, 0.90095319122776, 0.387541286389736, 0.864084839894436,
     1.30837141459997))

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Budesonide", 0.730863270469326, 0.577908633775861, 0.00886997709409627,
     -2.61702528352894, 0.924300294029335, "Budesonide+Formoterol",
     0.689621041376737, 0.514303866633964, 0.0130262555595863, -2.48305067406321,
     0.924700768481697, "Fluticasone", 0.862172230289102, 0.748163763497469,
     0.0404297599706609, -2.04933099328561, 0.993553805929815, "Fluticasone+Salmeterol",
     0.750111229863203, 0.655296356994868, 3.0413096362255e-05, -4.17035091458716,
     0.858644872904876, "Formoterol", 0.943570527130053, 0.781647782325837,
     0.545385788791114, -0.604688803783563, 1.13903648139226, "Salmeterol",
     0.801336637349188, 0.720188663856281, 4.78980742503043e-05,
     -4.06565341050372, 0.891628039408083, "Tiotropium", 0.685381494506769,
     0.610483937764018, 1.5711575787292e-10, -6.39830058892913, 0.769467899078309
    ))

})

