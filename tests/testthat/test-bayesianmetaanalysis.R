context("Bayesian Meta Analysis")

options <- jaspTools::analysisOptions("BayesianMetaAnalysis")
options$effectSize                <- "ES"
options$standardError             <- "SE"
options$postTable                 <- TRUE
options$esTable                   <- TRUE
options$plotPrior                 <- TRUE
options$plotPosterior             <- TRUE
options$checkForestPlot           <- TRUE
options$plotCumForest             <- FALSE
options$plotSequential            <- FALSE
options$plotSeqPM                 <- FALSE
options$addInfo                   <- TRUE
options$addLines                  <- TRUE
options$shade                     <- TRUE
options$priorH0FE                 <- 0.25
options$priorH1FE                 <- 0.25
options$priorH0RE                 <- 0.25
options$priorH1RE                 <- 0.25
options$iterMCMC                  <- 2000
options$priorES                   <- "cauchy"
options$priorSE                   <- "inverseGamma"
options$forestPlot                <- "plotForestObserved"
options$orderForest               <- "ascendingForest"
options$BFComputation             <- "integration"
options$bayesFactorType           <- "BF10"
options$modelSpecification        <- "BMA"
options$checkLowerPrior           <- FALSE
options$checkUpperPrior           <- FALSE
options$informativeCauchyLocation <- 0
options$informativeCauchyScale    <- 0.707
options$inverseGammaShape         <- 1
options$inverseGammaScale         <- 0.15
options$chainsMCMC                <- 4
options$seed                      <- 1
options$.meta                     <- list(confidenceInterval = list(containsColumn = TRUE), 
                                          effectSize = list(containsColumn = TRUE), 
                                          standardError = list(containsColumn = TRUE), 
                                          studyLabels = list(containsColumn = TRUE))
options$confidenceInterval        <- list()
set.seed(1)
results <- jaspTools::runAnalysis("BayesianMetaAnalysis", "BCG Vaccine.csv", options)


test_that("Posterior Estimates per Model table results match", {
  table <- results[["results"]][["bmaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("TRUE", 4.70377989938849e+21, -0.434254495375077, 0.0413386758998794,
                                      -0.51661689869485, "Fixed effects", "<unicode><unicode>", -0.354044245070268,
                                      "TRUE", 46.1842149375581, -0.700934189539198, 0.191389586956929,
                                      -1.09512063075207, "Random effects", "<unicode><unicode>", -0.313314935687268,
                                      1, "FALSE", 8.63965964148833e+26, 0.587415453102927, 0.174455537927642,
                                      0.341965384464118, "Random effects", "<unicode><unicode>", 0.977068560913425,
                                      2, "TRUE", 46.184214937558, -0.70093422222471, 0.192829673436622,
                                      -1.10318957647626, "Averaged", "<unicode><unicode>", -0.322201994073094,
                                      3, "FALSE", 8.82672917711143e+26, "", "", "", "Averaged", "<unicode><unicode>",
                                      ""))
})

test_that("Effect Sizes per Study table results match", {
  table <- results[["results"]][["esTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.810675161337492, -1.68394255502823, 0.00390957421349684, -0.9387,
                                      "Study 1", -1.28269463814809, -2.02557257455892, -0.578076727966483,
                                      -1.6662, "Study 2", -0.996207535379598, -1.91697987764739, -0.137535263863821,
                                      -1.3863, "Study 3", -1.40296899560657, -1.68366211044064, -1.12263629534387,
                                      -1.4564, "Study 4", -0.291114685435556, -0.70248689247515, 0.129769093323344,
                                      -0.2191, "Study 5", -0.948970505192553, -1.14550800371907, -0.75676856452674,
                                      -0.9581, "Study 6", -1.23621664010226, -2.02727979088307, -0.506882158451203,
                                      -1.6338, "Study 7", 0.00212364835628054, -0.121206326357141,
                                      0.126228703001128, 0.012, "Study 8", -0.50606742399599, -0.934996647644956,
                                      -0.0771038611477936, -0.4717, "Study 9", -1.25397766949404,
                                      -1.74487198539962, -0.762447203053708, -1.4012, "Study 10",
                                      -0.355110794879739, -0.573286768149591, -0.141056308764899,
                                      -0.3408, "Study 11", -0.266903956259088, -1.19606414841548,
                                      0.753566949784523, 0.4466, "Study 12", -0.148194690282786, -0.628570090461296,
                                      0.345693436706941, -0.0173, "Study 13"))
})

test_that("Observed study effects plot matches", {
  plotName <- results[["results"]][["forestContainer"]][["collection"]][["forestContainer_forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "observed-study-effects")
})

test_that("Effect size plot matches", {
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size")
})

test_that("Heterogeneity plot matches", {
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity")
})

test_that("Model Probabilities table results match", {
  table <- results[["results"]][["postTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Fixed H<unicode><unicode><unicode>", 2.40853635687008e-49, 0.25,
                                      "Fixed H<unicode><unicode><unicode>", 1.13292249023919e-27,
                                      0.25, "Random H<unicode><unicode><unicode>", 0.0211935284146057,
                                      0.25, "Random H<unicode><unicode><unicode>", 0.978806471585394,
                                      0.25))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "effect-size-2")
})

test_that("Heterogeneity plot matches", {
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "heterogeneity-2")
})