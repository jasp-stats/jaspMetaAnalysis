context("Bayesian Prediction Model Performance")

skip("metamisc does not implement set seed function")
# load the test data
data("EuroSCORE", package = "metamisc")

# O/E: normal/log ----
options <- analysisOptions("BayesianPredictionPerformance")
options$chains <- 2
options$diagnosticsAcPlot <- TRUE
options$diagnosticsGelmanRubinPlot <- TRUE
options$diagnosticsRmPlot <- TRUE
options$exportComputedEffectSize <- FALSE
options$forestPlot <- TRUE
options$forestPlotEstimates <- TRUE
options$forestPlotLabels <- TRUE
options$funnelPlotAsymmetryTest <- FALSE
options$funnelPlotAsymmetryTestDebray <- FALSE
options$funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion <- FALSE
options$funnelPlotAsymmetryTestEggerUnweighted <- TRUE
options$funnelPlotAsymmetryTestMacaskill <- FALSE
options$funnelPlotAsymmetryTestMacaskillPooled <- FALSE
options$funnelPlotAsymmetryTestPeters <- FALSE
options$funnelPlotAsymmetryTestPlot <- FALSE
options$effectSizeCi <- list()
options$numberOfExpectedEvents <- "e.events"
options$studyLabel <- ""
options$effectSize <- ""
options$numberOfParticipants <- ""
options$numberOfObservedEvents <- "n.events"
options$effectSizeSe <- ""
options$withinStudyVariation <- "normal/logit"
options$withinStudyVariation <- "normal/log"
options$measure <- "oeRatio"
options$method <- "Restricted ML"
options$priorAndPosteriorPlot <- TRUE
options$priorMuNMeam <- 0
options$priorMuNSD <- 10
options$tauPrior <- "uniformPrior"
options$tauTPriorDf <- 3
options$tauTPriorLocation <- 0
options$tauTPriorMax <- 10
options$tauTPriorMin <- 0
options$tauTPriorScale <- 1.5
options$tauUniformPriorMax <- 2
options$tauUniformPriorMin <- 0
options$sample <- 5000
set.seed(1)
dataset <- EuroSCORE
results <- runAnalysis("BayesianPredictionPerformance", dataset, options)


test_that("Autocorrelations plot matches", {
  plotName <- results[["results"]][["acPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelations-plot-1")
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-1")
})

test_that("Autocorrelations plot matches", {
  plotName <- results[["results"]][["gRPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelations-plot-1")
})

test_that("Prior and posterior plot matches", {
  plotName <- results[["results"]][["priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-plot-1")
})

test_that("Running means plot matches", {
  plotName <- results[["results"]][["rmPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "running-means-plot-1")
})

test_that("Observed-Expected Ratio Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.09088086726799, 0.88575857156341, 0.285932954156389, 1.34586691302466,
                                      2.45439237587575))
})


# O/E: Poisson/log (+ asymmetry tests) ----
options <- analysisOptions("BayesianPredictionPerformance")
options$chains <- 2
options$diagnosticsAcPlot <- TRUE
options$diagnosticsGelmanRubinPlot <- TRUE
options$diagnosticsRmPlot <- TRUE
options$exportComputedEffectSize <- FALSE
options$forestPlot <- TRUE
options$forestPlotEstimates <- TRUE
options$forestPlotLabels <- TRUE
options$funnelPlotAsymmetryTest <- TRUE
options$funnelPlotAsymmetryTestDebray <- TRUE
options$funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion <- TRUE
options$funnelPlotAsymmetryTestEggerUnweighted <- TRUE
options$funnelPlotAsymmetryTestMacaskill <- TRUE
options$funnelPlotAsymmetryTestMacaskillPooled <- TRUE
options$funnelPlotAsymmetryTestPeters <- TRUE
options$funnelPlotAsymmetryTestPlot <- TRUE
options$effectSizeCi <- list()
options$numberOfExpectedEvents <- "e.events"
options$studyLabel <- ""
options$effectSize <- ""
options$numberOfParticipants <- ""
options$numberOfObservedEvents <- "n.events"
options$effectSizeSe <- ""
options$withinStudyVariation <- "normal/logit"
options$withinStudyVariation <- "poisson/log"
options$measure <- "oeRatio"
options$method <- "Restricted ML"
options$priorAndPosteriorPlot <- TRUE
options$priorMuNMeam <- 0
options$priorMuNSD <- 10
options$tauPrior <- "uniformPrior"
options$tauTPriorDf <- 3
options$tauTPriorLocation <- 0
options$tauTPriorMax <- 10
options$tauTPriorMin <- 0
options$tauTPriorScale <- 1.5
options$tauUniformPriorMax <- 2
options$tauUniformPriorMin <- 0
options$sample <- 5000
set.seed(1)
dataset <- EuroSCORE
results <- runAnalysis("BayesianPredictionPerformance", dataset, options)


test_that("Autocorrelations plot matches", {
  plotName <- results[["results"]][["acPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelations-plot-2")
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-2")
})

test_that("Debray plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Debray"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "debray-2")
})

test_that("Egger (multiplicative overdispersion) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Egger (multiplicative overdispersion)"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-multiplicative-overdispersion-2")
})

test_that("Egger (unweighted) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Egger (unweighted)"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-unweighted-2")
})

test_that("Funnel Plot Asymmetry Tests table results match", {
  table <- results[["results"]][["funnelTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21, "Egger (unweighted)", 0.224315493009025, 1.25205622210261,
                                      21, "Egger (multiplicative overdispersion)", 0.100605639897417,
                                      1.71747360546325, "Macaskill", "Macaskill (pooled)", "Peters",
                                      21, "Debray", 0.20777337274033, 1.29975103237035))
})

test_that("Autocorrelations plot matches", {
  plotName <- results[["results"]][["gRPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelations-plot-2")
})

test_that("Prior and posterior plot matches", {
  plotName <- results[["results"]][["priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-plot-2")
})

test_that("Running means plot matches", {
  plotName <- results[["results"]][["rmPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "running-means-plot-2")
})

test_that("Observed-Expected Ratio Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.09034489801066, 0.882063029177578, 0.278352216827254, 1.33485008678669,
                                      2.47944025400384))
})


# cstat: normal/logit (incomplete data + different priors) ----
options <- analysisOptions("BayesianPredictionPerformance")
options$chains <- 2
options$diagnosticsAcPlot <- TRUE
options$diagnosticsGelmanRubinPlot <- TRUE
options$diagnosticsRmPlot <- TRUE
options$exportComputedEffectSize <- FALSE
options$exportComputedEffectSizeCStatisticColumnName <- "JaspColumn_17_Encoded"
options$exportComputedEffectSizeCStatisticLCiColumnName <- "JaspColumn_18_Encoded"
options$exportComputedEffectSizeCStatisticUCiColumnName <- "JaspColumn_19_Encoded"
options$exportComputedEffectSizeOeRatioColumnName <- "JaspColumn_20_Encoded"
options$exportComputedEffectSizeOeRatioLCiColumnName <- "JaspColumn_21_Encoded"
options$exportComputedEffectSizeOeRatioUCiColumnName <- "JaspColumn_22_Encoded"
options$forestPlot <- TRUE
options$forestPlotEstimates <- TRUE
options$forestPlotLabels <- TRUE
options$funnelPlotAsymmetryTest <- FALSE
options$funnelPlotAsymmetryTestDebray <- TRUE
options$funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion <- TRUE
options$funnelPlotAsymmetryTestEggerUnweighted <- TRUE
options$funnelPlotAsymmetryTestMacaskill <- TRUE
options$funnelPlotAsymmetryTestMacaskillPooled <- TRUE
options$funnelPlotAsymmetryTestPeters <- TRUE
options$funnelPlotAsymmetryTestPlot <- TRUE
options$effectSizeCi <- list()
options$numberOfExpectedEvents <- "e.events"
options$studyLabel <- ""
options$effectSize <- "c.index"
options$numberOfParticipants <- ""
options$numberOfObservedEvents <- "n.events"
options$effectSizeSe <- "se.c.index"
options$withinStudyVariation <- "normal/logit"
options$measure <- "cStatistic"
options$method <- "Restricted ML"
options$priorAndPosteriorPlot <- TRUE
options$priorMuNMeam <- 0
options$priorMuNSD <- 1
options$tauPrior <- "tPrior"
options$tauTPriorDf <- 3
options$tauTPriorLocation <- 0
options$tauTPriorMax <- 10
options$tauTPriorMin <- 0
options$tauTPriorScale <- 1.5
options$tauUniformPriorMax <- 2
options$tauUniformPriorMin <- 0
options$sample <- 5000
set.seed(1)
dataset <- EuroSCORE
results <- runAnalysis("BayesianPredictionPerformance", dataset, options)


test_that("Autocorrelations plot matches", {
  plotName <- results[["results"]][["acPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelations-plot-3")
})

test_that("Forest plot matches", {
  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-3")
})

test_that("Autocorrelations plot matches", {
  plotName <- results[["results"]][["gRPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelations-plot-3")
})

test_that("Prior and posterior plot matches", {
  plotName <- results[["results"]][["priorAndPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-plot-3")
})

test_that("Running means plot matches", {
  plotName <- results[["results"]][["rmPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "running-means-plot-3")
})

test_that("Concordance Statistic Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.787289305305654, 0.756963344255794, 0.685190521403063, 0.813602339203174,
                                      0.878578576399679))
})

