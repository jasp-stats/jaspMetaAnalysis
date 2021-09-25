context("Bayesian Prediction Model Performance")

skip("metamisc does not implement set seed function")
# load the test data
data("EuroSCORE", package = "metamisc")

# O:E: normal/log ----
options <- analysisOptions("BayesianPredictionPerformance")
options$chains <- 2
options$diagnosticsAcPlot <- TRUE
options$diagnosticsGelmanRubinPlot <- TRUE
options$diagnosticsRmPlot <- TRUE
options$exportColumns <- FALSE
options$forestPlot <- TRUE
options$forestPlotEstimates <- TRUE
options$forestPlotLabels <- TRUE
options$funnelAsymmetryTest <- FALSE
options$funnelAsymmetryTestDebrayFIV <- FALSE
options$funnelAsymmetryTestEggerFIV <- FALSE
options$funnelAsymmetryTestEggerUW <- TRUE
options$funnelAsymmetryTestMacaskillFIV <- FALSE
options$funnelAsymmetryTestMacaskillFPV <- FALSE
options$funnelAsymmetryTestPeters <- FALSE
options$funnelAsymmetryTestPlot <- FALSE
options$inputCI <- list()
options$inputE <- "e.events"
options$inputLabels <- ""
options$inputMeasure <- ""
options$inputN <- ""
options$inputO <- "n.events"
options$inputSE <- ""
options$linkCstat <- "normal/logit"
options$linkOE <- "normal/log"
options$measure <- "OE"
options$method <- "Restricted ML"
options$priorAndPosteriorPlot <- TRUE
options$priorMuNMeam <- 0
options$priorMuNSD <- 10
options$priorTau <- "priorTauU"
options$priorTauTDf <- 3
options$priorTauTLocation <- 0
options$priorTauTMax <- 10
options$priorTauTMin <- 0
options$priorTauTScale <- 1.5
options$priorTauUMax <- 2
options$priorTauUMin <- 0
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


# O:E: Poisson/log (+ asymmetry tests) ----
options <- analysisOptions("BayesianPredictionPerformance")
options$chains <- 2
options$diagnosticsAcPlot <- TRUE
options$diagnosticsGelmanRubinPlot <- TRUE
options$diagnosticsRmPlot <- TRUE
options$exportColumns <- FALSE
options$forestPlot <- TRUE
options$forestPlotEstimates <- TRUE
options$forestPlotLabels <- TRUE
options$funnelAsymmetryTest <- TRUE
options$funnelAsymmetryTestDebrayFIV <- TRUE
options$funnelAsymmetryTestEggerFIV <- TRUE
options$funnelAsymmetryTestEggerUW <- TRUE
options$funnelAsymmetryTestMacaskillFIV <- TRUE
options$funnelAsymmetryTestMacaskillFPV <- TRUE
options$funnelAsymmetryTestPeters <- TRUE
options$funnelAsymmetryTestPlot <- TRUE
options$inputCI <- list()
options$inputE <- "e.events"
options$inputLabels <- ""
options$inputMeasure <- ""
options$inputN <- ""
options$inputO <- "n.events"
options$inputSE <- ""
options$linkCstat <- "normal/logit"
options$linkOE <- "poisson/log"
options$measure <- "OE"
options$method <- "Restricted ML"
options$priorAndPosteriorPlot <- TRUE
options$priorMuNMeam <- 0
options$priorMuNSD <- 10
options$priorTau <- "priorTauU"
options$priorTauTDf <- 3
options$priorTauTLocation <- 0
options$priorTauTMax <- 10
options$priorTauTMin <- 0
options$priorTauTScale <- 1.5
options$priorTauUMax <- 2
options$priorTauUMin <- 0
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
options$exportColumns <- FALSE
options$exportCstat <- "JaspColumn_17_Encoded"
options$exportCstatlCI <- "JaspColumn_18_Encoded"
options$exportCstatuCI <- "JaspColumn_19_Encoded"
options$exportOE <- "JaspColumn_20_Encoded"
options$exportOElCI <- "JaspColumn_21_Encoded"
options$exportOEuCI <- "JaspColumn_22_Encoded"
options$forestPlot <- TRUE
options$forestPlotEstimates <- TRUE
options$forestPlotLabels <- TRUE
options$funnelAsymmetryTest <- FALSE
options$funnelAsymmetryTestDebrayFIV <- TRUE
options$funnelAsymmetryTestEggerFIV <- TRUE
options$funnelAsymmetryTestEggerUW <- TRUE
options$funnelAsymmetryTestMacaskillFIV <- TRUE
options$funnelAsymmetryTestMacaskillFPV <- TRUE
options$funnelAsymmetryTestPeters <- TRUE
options$funnelAsymmetryTestPlot <- TRUE
options$inputCI <- list()
options$inputE <- "e.events"
options$inputLabels <- ""
options$inputMeasure <- "c.index"
options$inputN <- ""
options$inputO <- "n.events"
options$inputSE <- "se.c.index"
options$linkCstat <- "normal/logit"
options$linkOE <- "poisson/log"
options$measure <- "cstat"
options$method <- "Restricted ML"
options$priorAndPosteriorPlot <- TRUE
options$priorMuNMeam <- 0
options$priorMuNSD <- 1
options$priorTau <- "priorTauT"
options$priorTauTDf <- 3
options$priorTauTLocation <- 0
options$priorTauTMax <- 10
options$priorTauTMin <- 0
options$priorTauTScale <- 1.5
options$priorTauUMax <- 2
options$priorTauUMin <- 0
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

