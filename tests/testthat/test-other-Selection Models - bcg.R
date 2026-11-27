jaspTools::preserveSnapshotReferences()
context("Other: Selection Models - bcg")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("SelectionModels (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_fitMeasuresTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-fit-measures")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-tests")

  plotName <- results[["results"]][["selectionDiagnostics"]][["collection"]][["selectionDiagnostics_selectionProfiles"]][["collection"]][["selectionDiagnostics_selectionProfiles_delta2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_selection-parameter-2")

  plotName <- results[["results"]][["selectionDiagnostics"]][["collection"]][["selectionDiagnostics_selectionProfiles"]][["collection"]][["selectionDiagnostics_selectionProfiles_delta3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_selection-parameter-3")

  plotName <- results[["results"]][["selectionDiagnostics"]][["collection"]][["selectionDiagnostics_selectionProfiles"]][["collection"]][["selectionDiagnostics_selectionProfiles_tau2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_-")

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionFrequencies"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-selection-frequencies")

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-1-selection-parameters")

  plotName <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-4_weight-function")

})

test_that("SelectionModels (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", dataset, opts, view = FALSE)

  plotName <- results[["results"]][["bubblePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_bubble-plots")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc_contrastsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-alloc-contrasts")

  table <- results[["results"]][["estimatedMarginalMeansAndContrastsContainer"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc"]][["collection"]][["estimatedMarginalMeansAndContrastsContainer_effectSize_alloc_estimatedMarginalMeansTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-alloc-estimated-marginal-means")

  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_forest-plot")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-coefficient")

  table <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeTermsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-effect-size-terms")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_fitMeasuresTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-fit-measures")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-tests")

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-2-selection-parameters")

})

test_that("SelectionModels (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", dataset, opts, view = FALSE)

  table <- results[["results"]][["selectionComparison"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-selection-comparison")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_modelSummaryContainer"]][["collection"]][["selectionModelResults_model_modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-pooled-estimates")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_modelSummaryContainer"]][["collection"]][["selectionModelResults_model_modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-tests")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_weightFunctionSummary"]][["collection"]][["selectionModelResults_model_weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-selection-parameters")

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_weightFunctionSummary"]][["collection"]][["selectionModelResults_model_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-1-pooled-estimates")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-1-tests")

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-2-pooled-estimates")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-2-tests")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-2-selection-parameters")

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-3_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model3"]][["collection"]][["selectionModelResults_model3_modelSummaryContainer"]][["collection"]][["selectionModelResults_model3_modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-3-pooled-estimates")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model3"]][["collection"]][["selectionModelResults_model3_modelSummaryContainer"]][["collection"]][["selectionModelResults_model3_modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-3-tests")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model3"]][["collection"]][["selectionModelResults_model3_weightFunctionSummary"]][["collection"]][["selectionModelResults_model3_weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-3-selection-parameters")

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model3"]][["collection"]][["selectionModelResults_model3_weightFunctionSummary"]][["collection"]][["selectionModelResults_model3_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-4_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model4"]][["collection"]][["selectionModelResults_model4_modelSummaryContainer"]][["collection"]][["selectionModelResults_model4_modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-4-pooled-estimates")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model4"]][["collection"]][["selectionModelResults_model4_modelSummaryContainer"]][["collection"]][["selectionModelResults_model4_modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-4-tests")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model4"]][["collection"]][["selectionModelResults_model4_weightFunctionSummary"]][["collection"]][["selectionModelResults_model4_weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-4-selection-parameters")

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model4"]][["collection"]][["selectionModelResults_model4_weightFunctionSummary"]][["collection"]][["selectionModelResults_model4_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-5_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model5"]][["collection"]][["selectionModelResults_model5_modelSummaryContainer"]][["collection"]][["selectionModelResults_model5_modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-5-pooled-estimates")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model5"]][["collection"]][["selectionModelResults_model5_modelSummaryContainer"]][["collection"]][["selectionModelResults_model5_modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-5-tests")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model5"]][["collection"]][["selectionModelResults_model5_weightFunctionSummary"]][["collection"]][["selectionModelResults_model5_weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-5-selection-parameters")

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model5"]][["collection"]][["selectionModelResults_model5_weightFunctionSummary"]][["collection"]][["selectionModelResults_model5_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-6_weight-function")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model6"]][["collection"]][["selectionModelResults_model6_modelSummaryContainer"]][["collection"]][["selectionModelResults_model6_modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-6-pooled-estimates")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model6"]][["collection"]][["selectionModelResults_model6_modelSummaryContainer"]][["collection"]][["selectionModelResults_model6_modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-6-tests")

  table <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model6"]][["collection"]][["selectionModelResults_model6_weightFunctionSummary"]][["collection"]][["selectionModelResults_model6_weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-3-model-6-selection-parameters")

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model6"]][["collection"]][["selectionModelResults_model6_weightFunctionSummary"]][["collection"]][["selectionModelResults_model6_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-7_weight-function")

})

test_that("SelectionModels (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", dataset, opts, view = FALSE)

  snapshotTableNode <- results[["results"]][["selectionComparison"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-selection-comparison")

  snapshotTableNode <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_modelSummaryContainer"]][["collection"]][["selectionModelResults_model_modelSummaryContainer_pooledEstimatesTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-model-pooled-estimates")

  snapshotTableNode <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_modelSummaryContainer"]][["collection"]][["selectionModelResults_model_modelSummaryContainer_testsTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-model-tests")

  selectionParameters <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_weightFunctionSummary"]][["collection"]][["selectionModelResults_model_weightFunctionSummary_selectionParameters"]]
  table <- selectionParameters[["data"]]
  # Boundary-parameter uncertainty varies slightly across platforms.
  testthat::expect_equal(as.numeric(table[[1]][["se"]]), 0.802657515905965, tolerance = 1e-3)
  testthat::expect_equal(as.numeric(table[[1]][["uCi"]]), 2.42585082389923, tolerance = 1e-3)
  testthat::expect_equal(as.numeric(table[[2]][["se"]]), 3.71380902686755, tolerance = 1e-3)
  testthat::expect_equal(as.numeric(table[[2]][["uCi"]]), 7.28203274293131, tolerance = 1e-3)
  jaspTools::expect_snapshot_table(selectionParameters, "analysis-4-selection-parameters",
    columns = list(se = c(rtol = 1e-3), uCi = c(rtol = 1e-3)))

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model"]][["collection"]][["selectionModelResults_model_weightFunctionSummary"]][["collection"]][["selectionModelResults_model_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  # The band draws from the covariance of the selection parameters: its lower edge changes about 2.5 times as much as their
  # standard errors, which vary across platforms within 1e-3 (above).
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_weight-function", tolerance = 1e-2)

  snapshotTableNode <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer_pooledEstimatesTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-model-1-pooled-estimates")

  snapshotTableNode <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer"]][["collection"]][["selectionModelResults_model1_modelSummaryContainer_testsTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-model-1-tests")

  snapshotTableNode <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary_selectionParameters"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-model-1-selection-parameters")

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model1"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary"]][["collection"]][["selectionModelResults_model1_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-2_weight-function")

  snapshotTableNode <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer_pooledEstimatesTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-model-2-pooled-estimates")

  snapshotTableNode <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer"]][["collection"]][["selectionModelResults_model2_modelSummaryContainer_testsTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-model-2-tests")

  snapshotTableNode <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary_selectionParameters"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-4-model-2-selection-parameters")

  plotName <- results[["results"]][["selectionModelResults"]][["collection"]][["selectionModelResults_model2"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary"]][["collection"]][["selectionModelResults_model2_weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-3_weight-function")

})

test_that("SelectionModels (analysis 5) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[5]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", dataset, opts, view = FALSE)

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-5-pooled-estimates")

  table <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  jaspTools::expect_snapshot_table(table, "analysis-5-tests")

  table <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]]
  jaspTools::expect_snapshot_table(table, "analysis-5-selection-parameters")

})

test_that("SelectionModels (analysis 6) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "Selection Models - bcg.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[6]]
  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  set.seed(1)
  results <- jaspTools::runAnalysis("SelectionModels", dataset, opts, view = FALSE)

  snapshotTableNode <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_pooledEstimatesTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-6-pooled-estimates")

  snapshotTableNode <- results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "analysis-6-tests")

  selectionParameters <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_selectionParameters"]]
  table <- selectionParameters[["data"]]
  # Boundary-parameter uncertainty varies slightly across platforms.
  testthat::expect_equal(as.numeric(table[[1]][["se"]]), 3.61311425231623, tolerance = 1e-3)
  testthat::expect_equal(as.numeric(table[[1]][["uCi"]]), 7.08290865570233, tolerance = 1e-3)
  jaspTools::expect_snapshot_table(selectionParameters, "analysis-6-selection-parameters",
    columns = list(se = c(rtol = 1e-3), uCi = c(rtol = 1e-3)))

  plotName <- results[["results"]][["weightFunctionSummary"]][["collection"]][["weightFunctionSummary_weightFunction"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-6_figure-1_weight-function", tolerance = 1e-3)

})

