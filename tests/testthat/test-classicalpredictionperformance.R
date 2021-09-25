context("Classical Prediction Model Performance")

# load the test data
data("EuroSCORE", package = "metamisc")

# OE: default settings all output ----
options <- analysisOptions("ClassicalPredictionPerformance")
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
options$inputN <- "n"
options$inputO <- "n.events"
options$inputSE <- ""
options$linkCstat <- "normal/logit"
options$linkOE <- "normal/log"
options$measure <- "OE"
options$method <- "Restricted ML"
options$priorAndPosteriorPlot <- FALSE
set.seed(1)
dataset <- EuroSCORE
results <- runAnalysis("ClassicalPredictionPerformance", dataset, options)


test_that("Forest plot matches", {
  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-1")
})

test_that("Debray plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Debray"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "debray-1")
})

test_that("Egger (multiplicative overdispersion) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Egger (multiplicative overdispersion)"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-multiplicative-overdispersion-1")
})

test_that("Egger (unweighted) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Egger (unweighted)"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-unweighted-1")
})

test_that("Macaskill plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Macaskill"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "macaskill-1")
})

test_that("Peters plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Peters"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "peters-1")
})

test_that("Funnel Plot Asymmetry Tests table results match", {
  table <- results[["results"]][["funnelTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21, "Egger (unweighted)", 0.247492409917581, 1.18956830232744,
                                      21, "Egger (multiplicative overdispersion)", 0.104342197958761,
                                      1.69766707464803, 21, "Macaskill", 0.00307473290181148, -3.34424947062035,
                                      21, "Macaskill (pooled)", 0.00290867961304971, -3.36795372835898,
                                      21, "Peters", 0.0378044019226087, 2.21684931244986, 21, "Debray",
                                      0.199963923458596, 1.32329829199447))
})

test_that("Observed-Expected Ratio Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.10759727531355, 0.899897320351101, 0.429525045622228, 1.36323522310676,
                                      2.85611220296795))
})


# OE: ML, Poisson/log (the most likely to break, treated differently internally in metamisc), no labels & no estimates in forest ----

options <- analysisOptions("ClassicalPredictionPerformance")
options$exportColumns <- FALSE
options$forestPlot <- TRUE
options$forestPlotEstimates <- FALSE
options$forestPlotLabels <- FALSE
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
options$inputN <- "n"
options$inputO <- "n.events"
options$inputSE <- ""
options$linkCstat <- "normal/logit"
options$linkOE <- "poisson/log"
options$measure <- "OE"
options$method <- "Maximum Likelihood"
options$priorAndPosteriorPlot <- FALSE
set.seed(1)
dataset <- EuroSCORE
results <- runAnalysis("ClassicalPredictionPerformance", dataset, options)


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

test_that("Macaskill plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Macaskill"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "macaskill-2")
})

test_that("Peters plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Peters"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "peters-2")
})

test_that("Funnel Plot Asymmetry Tests table results match", {
  table <- results[["results"]][["funnelTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21, "Egger (unweighted)", 0.0909012086250035, 1.7720228302703,
                                      21, "Egger (multiplicative overdispersion)", 0.0959248852877291,
                                      1.74319233389244, 21, "Macaskill", 0.633390338535319, -0.484006527182122,
                                      21, "Macaskill (pooled)", 0.37304065796014, -0.910211248058727,
                                      21, "Peters", 0.21186251897657, 1.28769520389975, 21, "Debray",
                                      0.78461295998471, -0.276835538099372))
})

test_that("Observed-Expected Ratio Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.08905040321886, 0.90027797667204, 0.44497408140484, 1.32538728478191,
                                      2.66539295279114))
})


# cstat: FE, Normal/logit (with CI) ----

options <- analysisOptions("ClassicalPredictionPerformance")
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
options$inputCI <- list(c("c.index.95CIl", "c.index.95CIu"))
options$inputE <- "e.events"
options$inputLabels <- ""
options$inputMeasure <- "c.index"
options$inputN <- "n"
options$inputO <- "n.events"
options$inputSE <- ""
options$linkCstat <- "normal/logit"
options$linkOE <- "poisson/log"
options$measure <- "cstat"
options$method <- "Fixed Effects"
options$priorAndPosteriorPlot <- FALSE
set.seed(1)
dataset <- EuroSCORE
results <- runAnalysis("ClassicalPredictionPerformance", dataset, options)


test_that("Forest plot matches", {
  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-3")
})

test_that("Debray plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Debray"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "debray-3")
})

test_that("Egger (multiplicative overdispersion) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Egger (multiplicative overdispersion)"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-multiplicative-overdispersion-3")
})

test_that("Egger (unweighted) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Egger (unweighted)"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-unweighted-3")
})

test_that("Macaskill plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Macaskill"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "macaskill-3")
})

test_that("Peters plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Peters"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "peters-3")
})

test_that("Funnel Plot Asymmetry Tests table results match", {
  table <- results[["results"]][["funnelTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21, "Egger (unweighted)", 0.391788729890032, -0.874404330413258,
                                      21, "Egger (multiplicative overdispersion)", 0.811502204713222,
                                      -0.241510337548532, 21, "Macaskill", 0.223469011948597, -1.25442984146189,
                                      21, "Macaskill (pooled)", 0.114801881008492, -1.6452645282483,
                                      21, "Peters", 0.486756685748436, -0.707955937641122, 21, "Debray",
                                      0.383839285489921, -0.889446332749469))
})

test_that("Concordance Statistic Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.791866075272939, 0.78489489216466, "", 0.798669285419758, ""
                                 ))
})


# cstat: ML, Normal/identity (incomplete data) ----
options <- analysisOptions("ClassicalPredictionPerformance")
options$exportColumns <- FALSE
options$forestPlot <- TRUE
options$forestPlotEstimates <- FALSE
options$forestPlotLabels <- FALSE
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
options$inputMeasure <- "c.index"
options$inputN <- ""
options$inputO <- "n.events"
options$inputSE <- "se.c.index"
options$linkCstat <- "normal/identity"
options$linkOE <- "poisson/log"
options$measure <- "cstat"
options$method <- "Maximum Likelihood"
options$priorAndPosteriorPlot <- FALSE
set.seed(1)
dataset <- EuroSCORE
results <- runAnalysis("ClassicalPredictionPerformance", dataset, options)


test_that("Forest plot matches", {
  plotName <- results[["results"]][["forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "forest-plot-4")
})

test_that("Debray plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Debray"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "debray-4")
})

test_that("Egger (multiplicative overdispersion) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Egger (multiplicative overdispersion)"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-multiplicative-overdispersion-4")
})

test_that("Egger (unweighted) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_Egger (unweighted)"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-unweighted-4")
})

test_that("Funnel Plot Asymmetry Tests table results match", {
  table <- results[["results"]][["funnelTestTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(14, "Egger (unweighted)", 0.016248803207124, -2.73070413450773,
                                      14, "Egger (multiplicative overdispersion)", 0.180192108157545,
                                      -1.41060848420324, "Macaskill", "Macaskill (pooled)", "Peters",
                                      14, "Debray", 0.55284921010877, -0.608115384974626))
})

test_that("Concordance Statistic Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.791292758203948, 0.765567095481128, 0.708105255501217, 0.817018420926768,
                                      0.874480260906679))
})
