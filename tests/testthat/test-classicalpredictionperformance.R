jaspTools::preserveSnapshotReferences()
context("Classical Prediction Model Performance")

# load the test data
data("EuroSCORE", package = "metamisc")

# OE: default settings all output ----
options <- analysisOptions("ClassicalPredictionPerformance")
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
options$numberOfParticipants <- "n"
options$numberOfObservedEvents <- "n.events"
options$effectSizeSe <- ""
options$withinStudyVariation <- "normal/logit"
options$withinStudyVariation <- "normal/log"
options$measure <- "oeRatio"
options$method <- "restrictedML"
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
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_D-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "debray-1")
})

test_that("Egger (multiplicative overdispersion) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-multiplicative-overdispersion-1")
})

test_that("Egger (unweighted) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-UW"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-unweighted-1")
})

test_that("Macaskill plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_M-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "macaskill-1")
})

test_that("Peters plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_P-FPV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "peters-1")
})

test_that("Funnel Plot Asymmetry Tests table results match", {
  table <- results[["results"]][["funnelTestTable"]]
  jaspTools::expect_snapshot_table(table, "funnel-plot-asymmetry-tests")
})

test_that("Observed-Expected Ratio Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]]
  jaspTools::expect_snapshot_table(table, "observed-expected-ratio-meta-analysis-summary")
})


# OE: ML, Poisson/log (the most likely to break, treated differently internally in metamisc), no labels & no estimates in forest ----

# options <- analysisOptions("ClassicalPredictionPerformance")
# options$exportComputedEffectSize <- FALSE
# options$forestPlot <- TRUE
# options$forestPlotEstimates <- FALSE
# options$forestPlotLabels <- FALSE
# options$funnelPlotAsymmetryTest <- TRUE
# options$funnelPlotAsymmetryTestDebray <- TRUE
# options$funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion <- TRUE
# options$funnelPlotAsymmetryTestEggerUnweighted <- TRUE
# options$funnelPlotAsymmetryTestMacaskill <- TRUE
# options$funnelPlotAsymmetryTestMacaskillPooled <- TRUE
# options$funnelPlotAsymmetryTestPeters <- TRUE
# options$funnelPlotAsymmetryTestPlot <- TRUE
# options$effectSizeCi <- list()
# options$numberOfExpectedEvents <- "e.events"
# options$studyLabel <- ""
# options$effectSize <- ""
# options$numberOfParticipants <- "n"
# options$numberOfObservedEvents <- "n.events"
# options$effectSizeSe <- ""
# options$withinStudyVariation <- "normal/logit"
# options$withinStudyVariation <- "poisson/log"
# options$measure <- "oeRatio"
# options$method <- "Maximum Likelihood"
# options$priorAndPosteriorPlot <- FALSE
# set.seed(1)
# dataset <- EuroSCORE
# results <- runAnalysis("ClassicalPredictionPerformance", dataset, options)
#
#
# test_that("Forest plot matches", {
#   plotName <- results[["results"]][["forestPlot"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "forest-plot-2")
# })
#
# test_that("Debray plot matches", {
#   plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_D-FIV"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "debray-2")
# })
#
# test_that("Egger (multiplicative overdispersion) plot matches", {
#   plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-FIV"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "egger-multiplicative-overdispersion-2")
# })
#
# test_that("Egger (unweighted) plot matches", {
#   plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-UW"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "egger-unweighted-2")
# })
#
# test_that("Macaskill plot matches", {
#   plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_M-FIV"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "macaskill-2")
# })
#
# test_that("Peters plot matches", {
#   plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_P-FPV"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "peters-2")
# })
#
# test_that("Funnel Plot Asymmetry Tests table results match", {
#   table <- results[["results"]][["funnelTestTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(21, "Egger (unweighted)", 0.0909012086250035, 1.7720228302703,
#                                       21, "Egger (multiplicative overdispersion)", 0.0959248852877291,
#                                       1.74319233389244, 21, "Macaskill", 0.633390338535319, -0.484006527182122,
#                                       21, "Macaskill (pooled)", 0.37304065796014, -0.910211248058727,
#                                       21, "Peters", 0.21186251897657, 1.28769520389975, 21, "Debray",
#                                       0.78461295998471, -0.276835538099372))
# })
#
# test_that("Observed-Expected Ratio Meta-Analysis Summary table results match", {
#   table <- results[["results"]][["summaryTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(1.08905040321886, 0.90027797667204, 0.44497408140484, 1.32538728478191,
#                                       2.66539295279114))
# })


# cstat: FE, Normal/logit (with CI) ----

options <- analysisOptions("ClassicalPredictionPerformance")
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
options$effectSizeCi <- list(c("c.index.95CIl", "c.index.95CIu"))
options$numberOfExpectedEvents <- "e.events"
options$studyLabel <- ""
options$effectSize <- "c.index"
options$numberOfParticipants <- "n"
options$numberOfObservedEvents <- "n.events"
options$effectSizeSe <- ""
options$withinStudyVariation <- "normal/logit"
options$measure <- "cStatistic"
options$method <- "fixedEffects"
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
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_D-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "debray-3")
})

test_that("Egger (multiplicative overdispersion) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-multiplicative-overdispersion-3")
})

test_that("Egger (unweighted) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-UW"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-unweighted-3")
})

test_that("Macaskill plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_M-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "macaskill-3")
})

test_that("Peters plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_P-FPV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "peters-3")
})

test_that("Funnel Plot Asymmetry Tests table results match", {
  table <- results[["results"]][["funnelTestTable"]]
  jaspTools::expect_snapshot_table(table, "funnel-plot-asymmetry-tests-2")
})

test_that("Concordance Statistic Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]]
  jaspTools::expect_snapshot_table(table, "concordance-statistic-meta-analysis-summary")
})


# cstat: ML, Normal/identity (incomplete data) ----
options <- analysisOptions("ClassicalPredictionPerformance")
options$exportComputedEffectSize <- FALSE
options$forestPlot <- TRUE
options$forestPlotEstimates <- FALSE
options$forestPlotLabels <- FALSE
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
options$effectSize <- "c.index"
options$numberOfParticipants <- ""
options$numberOfObservedEvents <- "n.events"
options$effectSizeSe <- "se.c.index"
options$withinStudyVariation <- "normal/identity"
options$measure <- "cStatistic"
options$method <- "maximumLikelihood"
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
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_D-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "debray-4")
})

test_that("Egger (multiplicative overdispersion) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-FIV"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-multiplicative-overdispersion-4")
})

test_that("Egger (unweighted) plot matches", {
  plotName <- results[["results"]][["funnelTestPlots"]][["collection"]][["funnelTestPlots_E-UW"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "egger-unweighted-4")
})

test_that("Funnel Plot Asymmetry Tests table results match", {
  table <- results[["results"]][["funnelTestTable"]]
  jaspTools::expect_snapshot_table(table, "funnel-plot-asymmetry-tests-3")
})

test_that("Concordance Statistic Meta-Analysis Summary table results match", {
  table <- results[["results"]][["summaryTable"]]
  jaspTools::expect_snapshot_table(table, "concordance-statistic-meta-analysis-summary-2")
})
