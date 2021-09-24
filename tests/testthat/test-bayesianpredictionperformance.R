context("Bayesian Prediction Model Performance")

skip("metamisc does not implement set seed function")
# O:E: normal/log
{
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
  dataset <- structure(list(Study = structure(c(14L, 2L, 8L, 7L, 9L, 5L, 12L,
                                                11L, 10L, 20L, 3L, 18L, 19L, 21L, 13L, 20L, 15L, 1L, 1L, 17L,
                                                6L, 4L, 16L), .Label = c("Barilli", "Biancari", "Borde", "Borracci",
                                                                         "Carneo", "Carosella", "Chalmers", "Di Dedda", "Grant", "Howell",
                                                                         "Kirmani", "Kunt", "Laurent", "Nashef", "Nishida", "Osnabrugge",
                                                                         "Paparella", "Qadir", "Spiliopoulos", "Wang", "Wendt"), class = "factor"),
                            n = c(5553L, 1027L, 1090L, 5576L, 23740L, 3798L, 428L, 15497L,
                                  933L, 11170L, 498L, 2004L, 216L, 1066L, 314L, 818L, 461L,
                                  12201L, 1670L, 6191L, 250L, 503L, 50588L), n.events = c(232L,
                                                                                          28L, 41L, 191L, 746L, 215L, 34L, 547L, 90L, 226L, 8L, 76L,
                                                                                          14L, 45L, 18L, 13L, 33L, 210L, 125L, 300L, 9L, 21L, 1071L
                                  ), c.index = c(0.8095, 0.867, 0.81, 0.79, 0.808, 0.85, 0.72,
                                                 0.818, 0.67, 0.72, 0.72, 0.84, 0.77, 0.72, 0.77, 0.642, 0.7697,
                                                 0.8, 0.82, 0.83, 0.76, 0.856, 0.77), se.c.index = c(NA, NA,
                                                                                                     NA, 0.01, 0.008, 0.01, 0.051, 0.007, NA, 0.015, NA, NA, 0.067,
                                                                                                     0.034, 0.061, 0.071, NA, 0.015, 0.02, 0.012, 0.056, 0.033,
                                                                                                     0.01), c.index.95CIl = c(0.782, 0.798, 0.74, NA, NA, NA,
                                                                                                                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                              NA, NA), c.index.95CIu = c(0.836, 0.936, 0.88, NA, NA, NA,
                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                         NA, NA), Po = c(0.0418, 0.0273, 0.0376, 0.0343, 0.0314, 0.0566,
                                                                                                                                                                         0.0794, 0.0353, 0.0965, 0.0202, 0.0161, 0.0379, 0.0648, 0.0422,
                                                                                                                                                                         0.0573, 0.0159, 0.0716, 0.0172, 0.0749, 0.0485, 0.036, 0.0417,
                                                                                                                                                                         0.0212), Pe = c(0.0395, 0.045, 0.031, 0.0468, 0.0341, 0.0446,
                                                                                                                                                                                         0.017, 0.0253, 0.113, 0.0255, 0.0201, 0.0372, 0.0399, 0.032,
                                                                                                                                                                                         0.023, 0.016, 0.074, 0.025, 0.062, 0.044, 0.0164, 0.0318,
                                                                                                                                                                                         0.031), SD.Pe = c(4.9, 6.7, 5.9, NA, NA, 8.18, 1.06, 3.18,
                                                                                                                                                                                                           7.26, 6.51, 1.41, 5.11, 5.46, 4, 1.81, 1.33, 8.59, 2.8, 8.2,
                                                                                                                                                                                                           7.04, 2.46, 6.02, 5), e.events = c(222, 46, 34, 261, 809.59,
                                                                                                                                                                                                                                              171, 7, 387, 105, 290, 10, 74, 9, 34, 7, 21, 34, 305, 104,
                                                                                                                                                                                                                                              272, 4, 16, 1568), multicentre = c(TRUE, FALSE, FALSE, FALSE,
                                                                                                                                                                                                                                                                                 TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
                                                                                                                                                                                                                                                                                 FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
                                                                                                                                                                                                                                                                                 TRUE), mean.age = c(64.6, 67, 64.5, 69.3, 67.1, 67, 74.5,
                                                                                                                                                                                                                                                                                                     65.3, 74.3, 49, 60.5, 58.3, 66.2, 68.3, 73.4, 64.5, 63.5,
                                                                                                                                                                                                                                                                                                     67.3, 68.1, 67.4, 68.6, 66.4, 64.7), sd.age = c(12.5, 9.4,
                                                                                                                                                                                                                                                                                                                                                     13.5, 10.07, 11.8, 10.15, 3.9, 11, 7.7, 13, 7.51, 9.6, 12.83,
                                                                                                                                                                                                                                                                                                                                                     11.5, 9.7, 10, 15.03, 11.8, 11.4, 11.2, 13.3, 10.3, 11.2),
                            pts.before.2010 = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                                                TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
                                                TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)), row.names = c(NA,
                                                                                                            -23L), class = "data.frame")
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
}

# O:E: Poisson/log (+ asymmetry tests)
{
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
  dataset <- structure(list(Study = structure(c(14L, 2L, 8L, 7L, 9L, 5L, 12L,
                                                11L, 10L, 20L, 3L, 18L, 19L, 21L, 13L, 20L, 15L, 1L, 1L, 17L,
                                                6L, 4L, 16L), .Label = c("Barilli", "Biancari", "Borde", "Borracci",
                                                                         "Carneo", "Carosella", "Chalmers", "Di Dedda", "Grant", "Howell",
                                                                         "Kirmani", "Kunt", "Laurent", "Nashef", "Nishida", "Osnabrugge",
                                                                         "Paparella", "Qadir", "Spiliopoulos", "Wang", "Wendt"), class = "factor"),
                            n = c(5553L, 1027L, 1090L, 5576L, 23740L, 3798L, 428L, 15497L,
                                  933L, 11170L, 498L, 2004L, 216L, 1066L, 314L, 818L, 461L,
                                  12201L, 1670L, 6191L, 250L, 503L, 50588L), n.events = c(232L,
                                                                                          28L, 41L, 191L, 746L, 215L, 34L, 547L, 90L, 226L, 8L, 76L,
                                                                                          14L, 45L, 18L, 13L, 33L, 210L, 125L, 300L, 9L, 21L, 1071L
                                  ), c.index = c(0.8095, 0.867, 0.81, 0.79, 0.808, 0.85, 0.72,
                                                 0.818, 0.67, 0.72, 0.72, 0.84, 0.77, 0.72, 0.77, 0.642, 0.7697,
                                                 0.8, 0.82, 0.83, 0.76, 0.856, 0.77), se.c.index = c(NA, NA,
                                                                                                     NA, 0.01, 0.008, 0.01, 0.051, 0.007, NA, 0.015, NA, NA, 0.067,
                                                                                                     0.034, 0.061, 0.071, NA, 0.015, 0.02, 0.012, 0.056, 0.033,
                                                                                                     0.01), c.index.95CIl = c(0.782, 0.798, 0.74, NA, NA, NA,
                                                                                                                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                              NA, NA), c.index.95CIu = c(0.836, 0.936, 0.88, NA, NA, NA,
                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                         NA, NA), Po = c(0.0418, 0.0273, 0.0376, 0.0343, 0.0314, 0.0566,
                                                                                                                                                                         0.0794, 0.0353, 0.0965, 0.0202, 0.0161, 0.0379, 0.0648, 0.0422,
                                                                                                                                                                         0.0573, 0.0159, 0.0716, 0.0172, 0.0749, 0.0485, 0.036, 0.0417,
                                                                                                                                                                         0.0212), Pe = c(0.0395, 0.045, 0.031, 0.0468, 0.0341, 0.0446,
                                                                                                                                                                                         0.017, 0.0253, 0.113, 0.0255, 0.0201, 0.0372, 0.0399, 0.032,
                                                                                                                                                                                         0.023, 0.016, 0.074, 0.025, 0.062, 0.044, 0.0164, 0.0318,
                                                                                                                                                                                         0.031), SD.Pe = c(4.9, 6.7, 5.9, NA, NA, 8.18, 1.06, 3.18,
                                                                                                                                                                                                           7.26, 6.51, 1.41, 5.11, 5.46, 4, 1.81, 1.33, 8.59, 2.8, 8.2,
                                                                                                                                                                                                           7.04, 2.46, 6.02, 5), e.events = c(222, 46, 34, 261, 809.59,
                                                                                                                                                                                                                                              171, 7, 387, 105, 290, 10, 74, 9, 34, 7, 21, 34, 305, 104,
                                                                                                                                                                                                                                              272, 4, 16, 1568), multicentre = c(TRUE, FALSE, FALSE, FALSE,
                                                                                                                                                                                                                                                                                 TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
                                                                                                                                                                                                                                                                                 FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
                                                                                                                                                                                                                                                                                 TRUE), mean.age = c(64.6, 67, 64.5, 69.3, 67.1, 67, 74.5,
                                                                                                                                                                                                                                                                                                     65.3, 74.3, 49, 60.5, 58.3, 66.2, 68.3, 73.4, 64.5, 63.5,
                                                                                                                                                                                                                                                                                                     67.3, 68.1, 67.4, 68.6, 66.4, 64.7), sd.age = c(12.5, 9.4,
                                                                                                                                                                                                                                                                                                                                                     13.5, 10.07, 11.8, 10.15, 3.9, 11, 7.7, 13, 7.51, 9.6, 12.83,
                                                                                                                                                                                                                                                                                                                                                     11.5, 9.7, 10, 15.03, 11.8, 11.4, 11.2, 13.3, 10.3, 11.2),
                            pts.before.2010 = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                                                TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
                                                TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)), row.names = c(NA,
                                                                                                            -23L), class = "data.frame")
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
}

# cstat: normal/logit (incomplete data + different priors)
{
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
  dataset <- structure(list(Study = structure(c(14L, 2L, 8L, 7L, 9L, 5L, 12L,
                                                11L, 10L, 20L, 3L, 18L, 19L, 21L, 13L, 20L, 15L, 1L, 1L, 17L,
                                                6L, 4L, 16L), .Label = c("Barilli", "Biancari", "Borde", "Borracci",
                                                                         "Carneo", "Carosella", "Chalmers", "Di Dedda", "Grant", "Howell",
                                                                         "Kirmani", "Kunt", "Laurent", "Nashef", "Nishida", "Osnabrugge",
                                                                         "Paparella", "Qadir", "Spiliopoulos", "Wang", "Wendt"), class = "factor"),
                            n = c(5553L, 1027L, 1090L, 5576L, 23740L, 3798L, 428L, 15497L,
                                  933L, 11170L, 498L, 2004L, 216L, 1066L, 314L, 818L, 461L,
                                  12201L, 1670L, 6191L, 250L, 503L, 50588L), n.events = c(232L,
                                                                                          28L, 41L, 191L, 746L, 215L, 34L, 547L, 90L, 226L, 8L, 76L,
                                                                                          14L, 45L, 18L, 13L, 33L, 210L, 125L, 300L, 9L, 21L, 1071L
                                  ), c.index = c(0.8095, 0.867, 0.81, 0.79, 0.808, 0.85, 0.72,
                                                 0.818, 0.67, 0.72, 0.72, 0.84, 0.77, 0.72, 0.77, 0.642, 0.7697,
                                                 0.8, 0.82, 0.83, 0.76, 0.856, 0.77), se.c.index = c(NA, NA,
                                                                                                     NA, 0.01, 0.008, 0.01, 0.051, 0.007, NA, 0.015, NA, NA, 0.067,
                                                                                                     0.034, 0.061, 0.071, NA, 0.015, 0.02, 0.012, 0.056, 0.033,
                                                                                                     0.01), c.index.95CIl = c(0.782, 0.798, 0.74, NA, NA, NA,
                                                                                                                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                              NA, NA), c.index.95CIu = c(0.836, 0.936, 0.88, NA, NA, NA,
                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                         NA, NA), Po = c(0.0418, 0.0273, 0.0376, 0.0343, 0.0314, 0.0566,
                                                                                                                                                                         0.0794, 0.0353, 0.0965, 0.0202, 0.0161, 0.0379, 0.0648, 0.0422,
                                                                                                                                                                         0.0573, 0.0159, 0.0716, 0.0172, 0.0749, 0.0485, 0.036, 0.0417,
                                                                                                                                                                         0.0212), Pe = c(0.0395, 0.045, 0.031, 0.0468, 0.0341, 0.0446,
                                                                                                                                                                                         0.017, 0.0253, 0.113, 0.0255, 0.0201, 0.0372, 0.0399, 0.032,
                                                                                                                                                                                         0.023, 0.016, 0.074, 0.025, 0.062, 0.044, 0.0164, 0.0318,
                                                                                                                                                                                         0.031), SD.Pe = c(4.9, 6.7, 5.9, NA, NA, 8.18, 1.06, 3.18,
                                                                                                                                                                                                           7.26, 6.51, 1.41, 5.11, 5.46, 4, 1.81, 1.33, 8.59, 2.8, 8.2,
                                                                                                                                                                                                           7.04, 2.46, 6.02, 5), e.events = c(222, 46, 34, 261, 809.59,
                                                                                                                                                                                                                                              171, 7, 387, 105, 290, 10, 74, 9, 34, 7, 21, 34, 305, 104,
                                                                                                                                                                                                                                              272, 4, 16, 1568), multicentre = c(TRUE, FALSE, FALSE, FALSE,
                                                                                                                                                                                                                                                                                 TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
                                                                                                                                                                                                                                                                                 FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE,
                                                                                                                                                                                                                                                                                 TRUE), mean.age = c(64.6, 67, 64.5, 69.3, 67.1, 67, 74.5,
                                                                                                                                                                                                                                                                                                     65.3, 74.3, 49, 60.5, 58.3, 66.2, 68.3, 73.4, 64.5, 63.5,
                                                                                                                                                                                                                                                                                                     67.3, 68.1, 67.4, 68.6, 66.4, 64.7), sd.age = c(12.5, 9.4,
                                                                                                                                                                                                                                                                                                                                                     13.5, 10.07, 11.8, 10.15, 3.9, 11, 7.7, 13, 7.51, 9.6, 12.83,
                                                                                                                                                                                                                                                                                                                                                     11.5, 9.7, 10, 15.03, 11.8, 11.4, 11.2, 13.3, 10.3, 11.2),
                            pts.before.2010 = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                                                TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
                                                TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)), row.names = c(NA,
                                                                                                            -23L), class = "data.frame")
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
}
