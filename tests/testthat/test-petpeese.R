context("Meta Analysis - PET-PEESE")

# normal input ----
options <- analysisOptions("PetPeese")
options$inferenceRegressionEstimatesTable <- TRUE
options$inferenceMultiplicativeHeterogeneityEstimatesEstimatesTable <- TRUE
options$effectSize <- "es"
options$effectSizeSe <- "se"
options$plotsMeanModelEstimatesPlot <- TRUE
options$plotsRegressionEstimatePeesePlot <- TRUE
options$plotsRegressionEstimatePetPlot <- TRUE
set.seed(1)
dataset <- data.frame(
  es = runif(100, .1, .5),
  se = runif(100, .1, .2)
)
results <- runAnalysis("PetPeese", dataset, options)

test_that("Mean Estimates(mu) table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_estimatesMean"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98, 0.34337131183395, 0.230538979173726, 3.89620858572912e-08,
                                      0.0575685744994455, 5.96456165224757, "PET", 0.456203644494174,
                                      98, 0.320866166951152, 0.261857088589484, 4.55340539661329e-18,
                                      0.0301072258608448, 10.6574471003802, "PEESE", 0.379875245312821
                                 ))
})

test_that("Multiplicative Heterogeneity Estimates table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_heterogeneity"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.734706631055494, "PET", 0.7354181940792, "PEESE"))
})

test_that("PET-PEESE Regression Estimates table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_petPeese"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98, -0.243521024582982, -1.02570647178074, 0.543139330140677,
                                      0.399081540970924, -0.610203679154192, "PET", 0.538664422614775,
                                      98, -0.577782641339283, -3.23167943220266, 0.670529544485013,
                                      1.35405385598765, -0.426705805521928, "PEESE", 2.07611414952409
                                 ))
})

test_that("Test of Publication Bias table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98, 0.543139330140677, -0.610203679154192, "PET"))
})

test_that("Test of Effect table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_effectTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98, 3.89620858572912e-08, 5.96456165224757, "PET"))
})

test_that("Estimated PEESE Regression plot matches", {
  plotName <- results[["results"]][["peeseRegression"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "estimated-peese-regression-mu")
})

test_that("Estimated PET Regression plot matches", {
  plotName <- results[["results"]][["petRegression"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "estimated-pet-regression-mu")
})

test_that("Mean Model Estimates (mu) plot matches", {
  plotName <- results[["results"]][["plotEstimates"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mean-model-estimates-mu")
})


# correlation input ----
options <- analysisOptions("PetPeese")
options$inferenceRegressionEstimatesTable <- TRUE
options$inferenceMultiplicativeHeterogeneityEstimatesEstimatesTable <- TRUE
options$effectSize <- "es"
options$sampleSize  <- "n"
options$plotsMeanModelEstimatesPlot <- TRUE
options$plotsRegressionEstimatePeesePlot <- TRUE
options$plotsRegressionEstimatePetPlot <- TRUE
options$measures <- "correlation"
set.seed(1)
dataset <- data.frame(
  es = runif(100, .0, .4),
  n  = rnbinom(100, 200, .5)
)
results <- runAnalysis("PetPeese", dataset, options)

test_that("Mean Estimates(rho) table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_estimatesMean"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98, 0.316068973379294, -0.109050475616618, 0.141851714751037,
                                      0.154729061585343, 1.48086064171798, "PET", 0.577103478493575,
                                      98, 0.268334843625459, 0.0589382081417808, 0.0148999384382551,
                                      0.0909348541411814, 2.47856690268921, "PEESE", 0.435376939152145
                                 ))
})

test_that("Multiplicative Heterogeneity Estimates table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_heterogeneity"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.58472493612089, "PET", 1.58419696748956, "PEESE"))
})

test_that("PET-PEESE Regression Estimates table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_petPeese"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98, -1.77827077000991, -8.10081265604878, 0.582712064812319, 3.22584595222681,
                                      -0.551257188453888, "PET", 4.54427111602896, 98, -13.618705173589,
                                      -57.5348415994575, 0.544726907418124, 22.4066037806171, -0.607798723400013,
                                      "PEESE", 30.2974312522795))
})

test_that("Test of Publication Bias table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98, 0.582712064812319, -0.551257188453888, "PET"))
})

test_that("Test of Effect table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_effectTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(98, 0.141851714751037, 1.48086064171798, "PET"))
})

test_that("Estimated PEESE Regression plot matches", {
  plotName <- results[["results"]][["peeseRegression"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "estimated-peese-regression-rho")
})

test_that("Estimated PET Regression plot matches", {
  plotName <- results[["results"]][["petRegression"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "estimated-pet-regression-rho")
})

test_that("Mean Model Estimates (rho) plot matches", {
  plotName <- results[["results"]][["plotEstimates"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "mean-model-estimates-rho")
})

