context("Other: Network Meta-Analysis - Dogliotti2014")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ClassicalNetworkMetaAnalysis (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Dogliotti2014.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_heterogeneityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(14.6993852067261, 0, "I<unicode>", 51.2419707360687))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(10, 19, 27, 4, 8, 79733))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.281455296945913, "Q(16) = 18.76", "Total", 0.282651475011595,
     "Q(11) = 13.17", "Within designs", 0.347987408972755, "Q(5) = 5.59",
     "Between designs"))

  plotName <- results[["results"]][["networkPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_network-plot")

  plotName <- results[["results"]][["smallStudyEffectsContainer"]][["collection"]][["smallStudyEffectsContainer_funnelPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_comparison-adjusted-funnel-plot")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Apixaban", 0.330323374484569, 0.249959037205915, 6.82346807226924e-15,
     -7.78770979865049, 0.4365256521651, "Aspirin", 0.776816726606723,
     0.625408036386331, 0.0224206494747534, -2.2831650247088, 0.964880832396629,
     "Aspirin+Clopidogtrel", 0.57530752368958, 0.44618738266421,
     2.01491972376758e-05, -4.26323106250029, 0.74179315613441, "Dabigatran 110mg",
     0.380087789663052, 0.27593157448892, 3.21324637301838e-09, -5.92031547616825,
     0.523559973585935, "Dabigatran 150mg", 0.266163816570791, 0.190583559654889,
     8.04613694862771e-15, -7.7668510480099, 0.371717148004862, "Rivaroxaban",
     0.32048591728543, 0.23525983493497, 5.4222943217105e-13, -7.21427283388588,
     0.436586309799437, "VKAs", 0.411699225565796, 0.323574794877413,
     5.14115328370303e-13, -7.22151504375983, 0.523824027751266
    ))

})

test_that("ClassicalNetworkMetaAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Dogliotti2014.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["evidenceContainer"]][["collection"]][["evidenceContainer_directEvidenceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Apixaban vs. Aspirin", 0.3764637359992, "Apixaban vs. Aspirin+Clopidogtrel",
     0, "Apixaban vs. Dabigatran 110mg", 0, "Apixaban vs. Dabigatran 150mg",
     0, "Apixaban vs. Placebo/Control", 0, "Apixaban vs. Rivaroxaban",
     0, "Apixaban vs. VKAs", 0.810221358685426, "Aspirin vs. Aspirin+Clopidogtrel",
     0.852716848066088, "Aspirin vs. Dabigatran 110mg", 0, "Aspirin vs. Dabigatran 150mg",
     0, "Aspirin vs. Placebo/Control", 0.82164039031361, "Aspirin vs. Rivaroxaban",
     0, "Aspirin vs. VKAs", 0.54728108888225, "Aspirin+Clopidogtrel vs. Dabigatran 110mg",
     0, "Aspirin+Clopidogtrel vs. Dabigatran 150mg", 0, "Aspirin+Clopidogtrel vs. Placebo/Control",
     0, "Aspirin+Clopidogtrel vs. Rivaroxaban", 0, "Aspirin+Clopidogtrel vs. VKAs",
     0.36708535421562, "Dabigatran 110mg vs. Dabigatran 150mg", 1,
     "Dabigatran 110mg vs. Placebo/Control", 0, "Dabigatran 110mg vs. Rivaroxaban",
     0, "Dabigatran 110mg vs. VKAs", 1, "Dabigatran 150mg vs. Placebo/Control",
     0, "Dabigatran 150mg vs. Rivaroxaban", 0, "Dabigatran 150mg vs. VKAs",
     1, "Placebo/Control vs. Rivaroxaban", 0, "Placebo/Control vs. VKAs",
     0.56800984589008, "Rivaroxaban vs. VKAs", 1))

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(10, 19, 27, 4, 8, 79733))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.368410627439776, "Q(5) = 5.41", "Between designs"))

  plotName <- results[["results"]][["networkPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_network-plot")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Apixaban", 0.326832869232469, 0.247548621610899, 3.0509908111783e-15,
     -7.88880284130391, 0.43151007553833, "Aspirin", 0.77430165239833,
     0.624062251243193, 0.020117238472682, -2.32415406063889, 0.960710326113197,
     "Aspirin+Clopidogtrel", 0.572403720015719, 0.444311669648574,
     1.58418939617568e-05, -4.31664456818239, 0.737423842472972,
     "Dabigatran 110mg", 0.375227628553696, 0.272679008209632, 1.76485714649735e-09,
     -6.01809124041722, 0.516342545597747, "Dabigatran 150mg", 0.262760394874026,
     0.18832916012543, 3.68492407377375e-15, -7.86520537070825, 0.366608256885818,
     "Rivaroxaban", 0.316387881953467, 0.232495165664466, 2.46706105259195e-13,
     -7.32068621430828, 0.430552143142046, "VKAs", 0.40643485081011,
     0.319867806445856, 1.73941244630555e-13, -7.36743572963738,
     0.516429864538423))

})

test_that("ClassicalNetworkMetaAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Dogliotti2014.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(10, 20, 28, 4, 8, 79808))

  plotName <- results[["results"]][["networkPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_network-plot")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Apixaban", 0.324468745755769, 0.24624699434152, 1.27249695377347e-15,
     -7.99722972263781, 0.427538079211269, "Aspirin", 0.768888579929225,
     0.620434185789519, 0.0163465106729333, -2.40108563287928, 0.952864400263948,
     "Aspirin+Clopidogtrel", 0.568789227125319, 0.442392304862094,
     1.08005282313643e-05, -4.40049285862783, 0.731299304572373,
     "Dabigatran 110mg", 0.372286195569017, 0.271247440047434, 9.57489894559195e-10,
     -6.11633989616107, 0.510961546354191, "Dabigatran 150mg", 0.261006282163924,
     0.187557139744591, 1.63120542279758e-15, -7.96658406076476,
     0.363218800530886, "Rivaroxaban", 0.314019021765341, 0.231366476832698,
     1.06648983456936e-13, -7.43239471621056, 0.426198070612301,
     "VKAs", 0.403159772792607, 0.318320910329256, 4.85828465128843e-14,
     -7.53567143216999, 0.510609881801562))

})

test_that("ClassicalNetworkMetaAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Network Meta-Analysis - Dogliotti2014.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["evidenceContainer"]][["collection"]][["evidenceContainer_directEvidenceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Apixaban vs. Aspirin", 0.365788332785408, "Apixaban vs. Aspirin+Clopidogtrel",
     0, "Apixaban vs. Dabigatran 110mg", 0, "Apixaban vs. Dabigatran 150mg",
     0, "Apixaban vs. Placebo/Control", 0, "Apixaban vs. Rivaroxaban",
     0, "Apixaban vs. VKAs", 0.790664999556613, "Aspirin vs. Aspirin+Clopidogtrel",
     0.774581314011499, "Aspirin vs. Dabigatran 110mg", 0, "Aspirin vs. Dabigatran 150mg",
     0, "Aspirin vs. Placebo/Control", 0.710207589496686, "Aspirin vs. Rivaroxaban",
     0, "Aspirin vs. VKAs", 0.519228357327166, "Aspirin+Clopidogtrel vs. Dabigatran 110mg",
     0, "Aspirin+Clopidogtrel vs. Dabigatran 150mg", 0, "Aspirin+Clopidogtrel vs. Placebo/Control",
     0, "Aspirin+Clopidogtrel vs. Rivaroxaban", 0, "Aspirin+Clopidogtrel vs. VKAs",
     0.343140430643289, "Dabigatran 110mg vs. Dabigatran 150mg",
     0.976566273354985, "Dabigatran 110mg vs. Placebo/Control", 0,
     "Dabigatran 110mg vs. Rivaroxaban", 0, "Dabigatran 110mg vs. VKAs",
     0.970395559852511, "Dabigatran 150mg vs. Placebo/Control", 0,
     "Dabigatran 150mg vs. Rivaroxaban", 0, "Dabigatran 150mg vs. VKAs",
     0.975742142299566, "Placebo/Control vs. Rivaroxaban", 0, "Placebo/Control vs. VKAs",
     0.502248276558417, "Rivaroxaban vs. VKAs", 0.970267195568268
    ))

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_forest-plot")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_inputSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(10, 19, 27, 4, 8, 79733))

  plotName <- results[["results"]][["networkPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_network-plot")

  table <- results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Apixaban", 0.350008001556347, 0.268423763179379, 8.9727376125189e-15,
     -7.75302721870061, 0.456388807393336, "Aspirin", 0.798893082402473,
     0.653718025326962, 0.0282145654497273, -2.1942900341228, 0.976308029431052,
     "Aspirin+Clopidogtrel", 0.607822742455335, 0.479491813798403,
     3.87800931219098e-05, -4.11463001420939, 0.770500091167886,
     "Dabigatran 110mg", 0.402412597016156, 0.29617836411694, 5.86328510531764e-09,
     -5.82061110628263, 0.546751275097697, "Dabigatran 150mg", 0.284152314522646,
     0.206149566455573, 1.53246223626747e-14, -7.68479161800397,
     0.391669695148146, "Rivaroxaban", 0.341017440047545, 0.253837253562307,
     9.20607595165802e-13, -7.14188327875911, 0.458139586623109,
     "VKAs", 0.434926270397071, 0.347217738347206, 4.31403172525592e-13,
     -7.24532509407703, 0.54479031394517))

})

