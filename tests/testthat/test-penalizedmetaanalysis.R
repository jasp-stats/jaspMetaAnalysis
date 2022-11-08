context("Bayesian Penalized Meta Analysis")

############################ NOTE ############################
# unit tests currently only work for linux due to rstan seed
# when they start failing it might be due to seed issues
# you can skip the tests if this is the case
##############################################################

# covariates, all output, no clustering
options <- analysisOptions("PenalizedMetaAnalysis")
options$.meta <- list(clustering = list(shouldEncode = TRUE), covariates = list(
  shouldEncode = TRUE), effectSize = list(shouldEncode = TRUE),
  effectSizeSe = list(shouldEncode = TRUE), factors = list(
    shouldEncode = TRUE), modelTerms = list(shouldEncode = TRUE),
  plotPosterior = list(shouldEncode = TRUE), scatterVariableX = list(
    shouldEncode = TRUE), scatterVariableY = list(shouldEncode = TRUE))
options$covariates <- "tneg"
options$effectSize <- "ES"
options$effectSizeSe <- "SE"
options$estimatesI2 <- TRUE
options$estimatesTau <- TRUE
options$factors <- "alloc"
options$mcmcBurnin <- 400
options$mcmcChains <- 2
options$mcmcSamples <- 400
options$modelTerms <- list(list(components = "tneg"), list(components = "alloc"))
options$plotPosterior <- list(list(variable = "tneg"), list(variable = "alloc"))
options$scatterVariableX <- list(list(variable = "JaspColumn_4_Encoded"))
options$scatterVariableY <- list()
options$setSeed <- TRUE
set.seed(1)
results <- runAnalysis("PenalizedMetaAnalysis", "BCG Vaccine", options)


test_that("posterior-plot-1 matches", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  plotName <- results[["results"]][["posteriorPlots"]][["collection"]][["posteriorPlots_posterior_1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-plot-0")
})

test_that("posterior-plot-2 matches", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  plotName <- results[["results"]][["posteriorPlots"]][["collection"]][["posteriorPlots_posterior_2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-plot-1")
})

test_that("posterior-plot-3 matches", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  plotName <- results[["results"]][["posteriorPlots"]][["collection"]][["posteriorPlots_posterior_3"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-plot-2")
})

test_that("I² table results match", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  table <- results[["results"]][["summaryI2Table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(85.6075094555512, 63.7334805491997, "I<unicode>", 96.0433243459796
                                 ))
})

test_that("Coefficients table results match", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.774643075737786, -1.35032967394689, 227.059931287272, 1.00345360847548,
                                      0.275256480193188, "Intercept", -0.300444994462903, 6.214293221514e-06,
                                      -2.34546568150371e-06, 78.2603484732879, 1.00257948519794, 6.26660878051603e-06,
                                      "tneg", 1.8967059938984e-05, -0.179532455624236, -0.912675847095722,
                                      246.262923988956, 0.999842283357177, 0.303451647350838, "alloc (random)",
                                      0.255416687626062, 0.0620319912478174, -0.537409492365956, 358.102897557216,
                                      1.0038558468335, 0.278141601512084, "alloc (systematic)", 0.760867284218734
                                 ))
})

test_that("Heterogeneity table results match", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  table <- results[["results"]][["summaryTauTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.390356010328009, 0.0792360525265774, "𝛕<unicode>", 1.09446374009974,
                                      0.592130103146157, 0.281488978889889, "𝛕", 1.0461650490889
                                 ))
})

# clustering + no intercept
options <- analysisOptions("PenalizedMetaAnalysis")
options$.meta <- list(clustering = list(shouldEncode = TRUE), covariates = list(
  shouldEncode = TRUE), effectSize = list(shouldEncode = TRUE),
  effectSizeSe = list(shouldEncode = TRUE), factors = list(
    shouldEncode = TRUE), modelTerms = list(shouldEncode = TRUE),
  plotPosterior = list(shouldEncode = TRUE), scatterVariableX = list(
    shouldEncode = TRUE), scatterVariableY = list(shouldEncode = TRUE))
options$clustering <- "ablat"
options$covariates <- ""
options$effectSize <- "ES"
options$effectSizeSe <- "SE"
options$estimatesI2 <- TRUE
options$estimatesTau <- TRUE
options$factors <- "alloc"
options$interceptTerm <- FALSE
options$lassoPriorDf <- 5
options$lassoPriorDfGlobal <- 2
options$lassoPriorDfSlab <- 1
options$lassoPriorScaleGlobal <- 5
options$lassoPriorScaleSlab <- 10
options$mcmcBurnin <- 500
options$mcmcChains <- 1
options$mcmcSamples <- 500
options$method <- "lasso"
options$modelTerms <- list(list(components = "alloc"))
options$plotPosterior <- list(list(variable = "Heterogeneity"))
options$scatterVariableX <- list(list(variable = "Heterogeneity"))
options$scatterVariableY <- list()
options$setSeed <- TRUE
set.seed(1)
results <- runAnalysis("PenalizedMetaAnalysis", "BCG Vaccine", options)


test_that("diagnostic-plot-2 matches", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  plotName <- results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_diagnostics_1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "diagnostic-plot-0")
})

test_that("diagnostic-plot-3 matches", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  plotName <- results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_diagnostics_2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "diagnostic-plot-1")
})

test_that("posterior-plot-5 matches", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  plotName <- results[["results"]][["posteriorPlots"]][["collection"]][["posteriorPlots_posterior_1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-plot-4")
})

test_that("posterior-plot-6 matches", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  plotName <- results[["results"]][["posteriorPlots"]][["collection"]][["posteriorPlots_posterior_2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "posterior-plot-5")
})

test_that("I² table results match", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  table <- results[["results"]][["summaryI2Table"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(18.7451143060603, 0.00853059634765431, "I<unicode> (within)",
                                      89.9798428856672, 77.3581592181074, 7.11435985722437, "I<unicode> (between)",
                                      98.5318544960056, 96.1032735241677, 90.6055567918329, "I<unicode>",
                                      98.8523012443759))
})

test_that("Coefficients table results match", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.00999865910675941, -1.00840341397566, 109.655893560877, 0.999399764052943,
                                      0.431877307442145, "alloc (alternate)", 0.903704345678235, 0.00394092040487817,
                                      -0.89373798660342, 78.0886570456313, 1.00219834133141, 0.395182561227997,
                                      "alloc (random)", 1.0234558149318, 0.142637149408643, -0.781075147931734,
                                      115.850760812657, 0.998057307023675, 0.470008203762782, "alloc (systematic)",
                                      1.30980021074463))
})

test_that("Heterogeneity table results match", {
  testthat::skip_on_os(c("windows", "mac", "solaris"))
  table <- results[["results"]][["summaryTauTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.250413446818812, 0.000176579102371538, "𝛕<unicode> (within)",
                                      1.25452505887974, 1.32047131218113, 0.0831109211305912, "𝛕<unicode> (between)",
                                      3.92056223081031, 0.38831921210789, 0.0132691968903436, "𝛕 (within)",
                                      1.12001712173437, 1.06637096006069, 0.288070754886098, "𝛕 (between)",
                                      1.98000102910291))
})
