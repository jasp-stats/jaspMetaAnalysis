context("Bayesian Meta Analysis")

options <- jaspTools::analysisOptions("BayesianMetaAnalysis")
options$effectSize                              <- "ES"
options$effectSizeSe                            <- "SE"
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
results <- jaspTools::runAnalysis("BayesianMetaAnalysis", "BCG Vaccine.csv", options)

############################ NOTE ############################
# unit tests currently only work for Windows due to rstan seed
# when they start failing it might be due to seed issues
# you can skip the tests if this is the case
##############################################################

test_that("Posterior Estimates per Model table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  table <- results[["results"]][["bmaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 4.70377989938849e+21, -0.434254495375077, 0.0413386758998794,
                                      -0.516616898694849, "Fixed effects", "<unicode>", -0.35404424507026,
                                      "TRUE", 46.1842149375581, -0.691684127331745, 0.194674438261359,
                                      -1.05371165363577, "Random effects", "<unicode>", -0.29988113833234,
                                      1, "FALSE", 8.63965964148833e+26, 0.585325517009774, 0.167159875616352,
                                      0.343234248728213, "Random effects", "<unicode>", 0.993677396740531,
                                      2, "TRUE", 46.184214937558, -0.691684144187806, 0.195426184793392,
                                      -1.05915644399236, "Averaged", "<unicode>", -0.277506371377729,
                                      3, "FALSE", 8.82672917711143e+26, "", "", "", "Averaged", "<unicode>",
                                      ""))
})

test_that("Effect Sizes per Study table results match", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  table <- results[["results"]][["effectSizePerStudy"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.809128072433214, -1.65195861984318, 0.0291494666483802, -0.9387,
                                      "Study 1", -1.27392183924008, -2.04585172754217, -0.550502001072192,
                                      -1.6662, "Study 2", -0.987605843372775, -1.89764125756983, -0.131147744375098,
                                      -1.3863, "Study 3", -1.4025680591068, -1.67918997790265, -1.13439697200656,
                                      -1.4564, "Study 4", -0.292551414527035, -0.713785940836336,
                                      0.150204176975293, -0.2191, "Study 5", -0.948574718058429, -1.14002067256022,
                                      -0.754825669673909, -0.9581, "Study 6", -1.23879932796539, -2.02118617092247,
                                      -0.514836725420342, -1.6338, "Study 7", 0.00199996360424655,
                                      -0.11868785432434, 0.121531187105811, 0.012, "Study 8", -0.511556112765042,
                                      -0.939053440577442, -0.0814585678966693, -0.4717, "Study 9",
                                      -1.25406731815491, -1.76606641390657, -0.751053323311322, -1.4012,
                                      "Study 10", -0.354049282748894, -0.568645513271642, -0.143878543957096,
                                      -0.3408, "Study 11", -0.264061608492473, -1.16024809868433,
                                      0.690555229532794, 0.4466, "Study 12", -0.147144689281277, -0.643591471297769,
                                      0.355581782256767, -0.0173, "Study 13"))
})

test_that("Observed study effects plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["forestContainer"]][["collection"]][["forestContainer_forestPlotEffect"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "observed-study-effects")
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

test_that("Model Probabilities table results match", {
  if (Sys.info()["sysname"] == "Darwin")
    testthat::skip("this test doesn't work on macOS + maybe a reason for future us to look at")

    table <- results[["results"]][["modelProbability"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("Fixed H<unicode>", 2.40853635687009e-49, 0.25, "Fixed H<unicode>",
                                        1.13292249023919e-27, 0.25, "Random H<unicode>", 0.0211935284146057,
                                        0.25, "Random H<unicode>", 0.978806471585394, 0.25))
})

test_that("Effect Size plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-2")
})

test_that("Heterogeneity plot matches", {
  testthat::skip_on_os(c("mac", "linux", "solaris"))
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity-2")
})
