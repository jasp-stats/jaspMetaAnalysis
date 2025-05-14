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
  table <- results[["results"]][["bmaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 1.08511272548537e+23, -0.428521199003929, 0.0396739532251755,
                                      -0.505741331731128, "Fixed effects", "<unicode>", -0.352341031368924,
                                      "TRUE", 43.7889383833054, -0.661196401908021, 0.18607180239536,
                                      -1.02044851688463, "Random effects", "<unicode>", -0.291587823303142,
                                      1, "FALSE", 4.17330245755268e+24, 0.560093493776082, 0.159545863387725,
                                      0.325104990622102, "Random effects", "<unicode>", 0.937685681730436,
                                      2, "TRUE", 43.7889383833053, -0.661196221977965, 0.186369709157622,
                                      -1.03500667416548, "Averaged", "<unicode>", -0.294553573304253,
                                      3, "FALSE", 4.26860740468391e+24, "", "", "", "Averaged", "<unicode>",
                                      ""))
})

test_that("Effect Sizes per Study table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  table <- results[["results"]][["effectSizePerStudy"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.775313265194565, -1.58426554420203, 0.0184220172845337, -0.889311333920205,
                                      "Study 1", -1.21083809615381, -1.93423973107235, -0.533046656274108,
                                      -1.58538865720143, "Study 2", -0.951368704667368, -1.88826310261639,
                                      -0.126476069498075, -1.34807314829969, "Study 3", -1.38647054034467,
                                      -1.65601029821987, -1.11891142615019, -1.44155119002131, "Study 4",
                                      -0.292914934443789, -0.69852287627473, 0.126306681114901, -0.217547322211296,
                                      "Study 5", -0.781481962907716, -0.945604805447621, -0.624881747496693,
                                      -0.786115585818864, "Study 6", -1.20245263758558, -1.98653671409264,
                                      -0.506742428922779, -1.62089822359839, "Study 7", 0.00113114041905089,
                                      -0.12370879175742, 0.12712665781556, 0.0119523335238405, "Study 8",
                                      -0.501379402657858, -0.924859696837206, -0.0728390501018448,
                                      -0.469417648738149, "Study 9", -1.22162733774407, -1.71101184145734,
                                      -0.741362587644194, -1.37134480347278, "Study 10", -0.354332007815156,
                                      -0.563879658486177, -0.142600383579625, -0.339358828338391,
                                      "Study 11", -0.262297229420223, -1.15433588027715, 0.760909037350747,
                                      0.445913400571379, "Study 12", -0.150215386445887, -0.625472372562452,
                                      0.346192699956957, -0.0173139482168798, "Study 13"))
})

test_that("Observed study effects plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["forestContainer"]][["collection"]][["forestContainer_forestPlotEffect"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "observed-study-effects")
})

test_that("Model Probabilities table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  table <- results[["results"]][["modelProbability"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Fixed H<unicode>", 2.15893183301841e-48, 0.25, "Fixed H<unicode>",
                                      2.34268440546373e-25, 0.25, "Random H<unicode>", 0.0223269413407829,
                                      0.25, "Random H<unicode>", 0.977673058659217, 0.25))
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
