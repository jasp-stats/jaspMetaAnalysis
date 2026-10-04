context("Library: Acculturation Mismatch and Intergenerational Cultural Conflict")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("EffectSizeComputation (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("EffectSizeComputation", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["computeSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(18, "ZCOR", 1, 18))

})

test_that("PetPeese (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["estimates"]][["collection"]][["estimates_mean"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(16, -0.000872207822835111, -0.206619975057632, 0.993663506414102,
     0.104520755484612, -0.00806669053643172, "PET", 0.205022289978001,
     16, 0.113979304264294, -0.00656089452971465, 0.0822429941554453,
     0.0593407825514921, 1.85416107044634, "PEESE", 0.228123078852392
    ))

  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(16, 0.0519157244990328, 2.10025053076424, "PET"))

  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_effectTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(16, 0.993663506414102, -0.00806669053643172, "PET"))

})

test_that("SelectionModels (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.1697119, -0.04982383, -0.2275482, "Pooled effect", 0.373604,
     0.5185411, 0.171845505608945, 0.0960069367192093, "", "𝜏", 0.366425739000944,
     "", 0.0295308777979941, 0.00921733189820625, "", "𝜏<unicode>",
     0.134267822202388, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.21614422060384e-08, "LR(1) = 31.30", "Heterogeneity", 0.128964119019704,
     "z = 1.52", "Pooled effect", 0.538968465061593, "LR(2) = 1.24",
     "Publication bias"))

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "", "[0, 0.025)", "", "", 0.435511231401042, 0, "[0.025, 0.5)",
     0.3562708222836, 1.13378921181937, 0.18796847999249, 0, "[0.5, 1)",
     0.343299350813174, 0.860822843502293))

})

test_that("RobustBayesianMetaAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["diagnosticsContainer"]][["collection"]][["diagnosticsContainer_diagnosticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1084, 0.0303203309125258, 0.00340846264917594, 1.00508273336424,
     "mu", 2765, 0.0191623171076701, 0.0010536737489806, 1.00069963129628,
     "tau", 881, 0.0335461194683691, 0.0416189584441352, 1.00847133907787,
     "PET", 4264, 0.0153804229289932, 0.080443339296939, 1.00074978682054,
     "PEESE", "", "", "", "", "omega[0,0.025]", 9985, 0.010086362232534,
     0.00118609594216382, 1.00074256355268, "omega[0.025,0.05]",
     2479, 0.0202390149009357, 0.00706855298922059, 1.00107790761157,
     "omega[0.05,0.5]", 2282, 0.0209712964533381, 0.00825558017837499,
     1.00087400449267, "omega[0.5,0.95]", 2361, 0.020579153439889,
     0.00804732013842782, 1.00081507032642, "omega[0.95,0.975]",
     2377, 0.0205116967671772, 0.00804497233616516, 1.00075279263187,
     "omega[0.975,1]"))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, -0.297908245515315, 0.102449478678226, 0.0765638880005999,
     "Pooled effect", 0.292253764991898, 0.461474047155395, 0.0810093579629565,
     "", 0.161390435720052, 0.150632703524511, "𝜏", 0.292433308038319,
     "", 0.00656251608067695, "", 0.0290703158408391, 0.0226902113711034,
     "𝜏<unicode>", 0.0855172396529003, ""))

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.32149489322191, 0.569243075692431, 0.5, "Pooled effect", 30002,
     0.999966669999667, 0.5, "Heterogeneity", 5.0006, 0.8333499983335,
     0.5, "Publication bias"))

  plotName <- results[["results"]][["priorAndPosteriorPlotContainer"]][["collection"]][["priorAndPosteriorPlotContainer_pooledEffect"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_pooled-effect")

})

