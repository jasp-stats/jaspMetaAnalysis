jaspTools::preserveSnapshotReferences()
context("Bayesian Meta Analysis (Deprecated)")

options <- jaspTools::analysisOptions("BayesianMetaAnalysisDeprecated")
options$effectSize                              <- "yi"
options$effectSizeSe                            <- "sei"
options$modelProbability                        <- TRUE
options$effectSizePerStudy                      <- TRUE
options$priorPlot                               <-  TRUE
options$priorPosterior                          <- TRUE
options$forestPlot                              <- TRUE
options$cumulativeForestPlot                    <- FALSE
options$bfSequentialPlot                        <- FALSE
options$modelProbabilitySequentialPlot          <- FALSE
options$priorPosteriorAdditionalInfo            <- TRUE
options$priorPosteriorFixedAndRandom            <- TRUE
options$priorPosteriorCi                        <- TRUE
options$priorModelProbabilityFixedNull          <- 0.25
options$priorModelProbabilityFixedAlternative   <- 0.25
options$priorModelProbabilityRandomNull         <- 0.25
options$priorModelProbabilityRandomAlternative  <- 0.25
options$samples                                 <- 2000
options$priorEffectSize                         <- "cauchy"
options$priorStandardError                      <- "inverseGamma"
options$forestPlotEffect                        <- "observed"
options$forestPlotRowOrder                      <- "ascending"
options$bayesFactorComputation                  <- "integration"
options$bayesFactorType                         <- "BF10"
options$model                                   <- "averaging"
options$truncationLowerBound                    <- FALSE
options$truncationUpperBound                    <- FALSE
options$cauchyLocation                          <- 0
options$cauchyScale                             <- 0.707
options$inverseGammaShape                       <- 1
options$inverseGammaScale                       <- 0.15
options$chains                                  <- 4
options$seed                                    <- 1
options$.meta <- list(effectSizeCi = list(containsColumn = TRUE),
                      effectSize = list(containsColumn = TRUE),
                      effectSizeSe = list(containsColumn = TRUE),
                      studyLabel = list(containsColumn = TRUE))
options$effectSizeCi <- list()
set.seed(1)

dat <- data.frame(metafor::escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=metadat::dat.bcg, slab=paste0(author, ", ", year)))
dat$sei <- sqrt(dat$vi)

results <- jaspTools::runAnalysis("BayesianMetaAnalysisDeprecated", dataset = dat, options)

############################ NOTE ############################
# unit tests currently only work for Windows due to rstan seed
# when they start failing it might be due to seed issues
# you can skip the tests if this is the case
##############################################################

test_that("Posterior Estimates per Model table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  snapshotTableNode <- results[["results"]][["bmaTable"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "posterior-estimates-per-model")
})

test_that("Effect Sizes per Study table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  snapshotTableNode <- results[["results"]][["effectSizePerStudy"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "effect-sizes-per-study")
})

test_that("Observed study effects plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["forestContainer"]][["collection"]][["forestContainer_forestPlotEffect"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "observed-study-effects")
})

test_that("Model Probabilities table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  snapshotTableNode <- results[["results"]][["modelProbability"]]
  table <- snapshotTableNode[["data"]]
  jaspTools::expect_snapshot_table(snapshotTableNode, "model-probabilities")
})

test_that("Effect size plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size")
})

test_that("Heterogeneity plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity")
})

test_that("Effect Size plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size")
})

test_that("Heterogeneity plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity")
})
