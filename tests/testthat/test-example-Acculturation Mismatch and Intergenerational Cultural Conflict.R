context("Example: Acculturation Mismatch and Intergenerational Cultural Conflict")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("EffectSizeComputation (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
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
  jaspFile <- testthat::test_path("..", "..", "examples", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("PetPeese", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["estimates"]][["collection"]][["estimates_estimatesMean"]][["data"]]
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
  jaspFile <- testthat::test_path("..", "..", "examples", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 0.0871956871948914, 2.92539862891996, "Assuming homogeneity",
     1, 0.0779480017861305, 3.1071760337503, "Assuming heterogeneity"
    ))

  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_heterogeneityTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(17, 5.18834758470277e-09, 75.4999011503259))

  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_meanFE"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.207801809051088, 0.172749992783511, 2.67046218315371e-29, 0.0176934238092525,
     11.2374291313837, "Unadjusted", 0.242071241082355, 0.186709301877958,
     0.141245590530067, 3.76216117286539e-15, 0.0229186729664062,
     7.86260827697221, "Adjusted", 0.231004878574876))

  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_meanRE"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.249828986368464, 0.172357164320453, 1.12828330201181e-09, 0.0384616644696701,
     6.09011681436316, "Unadjusted", 0.322789379075183, 0.158935800368333,
     -0.00324200847584841, 0.0546975000934211, 0.0806346193688665,
     1.9212714056052, "Adjusted", 0.309263371437758))

})

test_that("RobustBayesianMetaAnalysis (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Acculturation Mismatch and Intergenerational Cultural Conflict.jasp")
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

