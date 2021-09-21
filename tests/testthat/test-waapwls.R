context("Meta Analysis - WAAP-WLS")

# normal input
{

  options <- analysisOptions("WaapWls")
  options$estimatesSigma <- TRUE
  options$inputES <- "es"
  options$inputSE <- "se"
  options$plotModels <- TRUE
  set.seed(1)
  dataset <- data.frame(
    es = runif(100, .1, .5),
    se = runif(100, .1, .2)
  )
  results <- runAnalysis("WaapWls", dataset, options)

  test_that("Mean Estimates(mu) table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_estimatesMean"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, 0.308843185963856, 0.288136775996336, 1.70473190020944e-50,
                                        0.0105646890100273, 29.2335331092778, "WLS", 0.329549595931375,
                                        5, 0.314384734990578, 0.29342503257996, 8.53714809923574e-07,
                                        0.0106939222230329, 29.3984497393711, "WAAP", 0.335344437401195
                                   ))
  })

  test_that("Multiplicative Heterogeneity Estimates table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_heterogeneity"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.732373939542464, "WLS", 0.247179185318315, "WAAP"))
  })

  test_that("Test of Effect table results match", {
    table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_effectTest"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, 1.70473190020944e-50, 29.2335331092778, "WLS", 5, 8.53714809923574e-07,
                                        29.3984497393711, "WAAP"))
  })

  test_that("Mean Model Estimates (mu) plot matches", {
    plotName <- results[["results"]][["plotEstimates"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "mean-model-estimates-mu")
  })

}

# correlation input
{
  options <- analysisOptions("WaapWls")
  options$estimatesSigma <- TRUE
  options$inputES <- "es"
  options$inputN  <- "n"
  options$plotModels <- TRUE
  options$measures <- "correlation"
  set.seed(1)
  dataset <- data.frame(
    es = runif(100, .1, .2),
    n  = rnbinom(100, 200, .5)
  )
  results <- runAnalysis("WaapWls", dataset, options)

  test_that("Mean Estimates(rho) table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_estimatesMean"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, 0.152057482491801, 0.146823853705826, 7.49071897329892e-77,
                                        0.00265262252872538, 56.1079283503232, "WLS", 0.157282598014925,
                                        "WAAP"))
  })

  test_that("Multiplicative Heterogeneity Estimates table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_heterogeneity"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.38400346189483, "WLS"))
  })

  test_that("Test of Effect table results match", {
    table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_effectTest"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, 7.49071897329892e-77, 56.1079283503232, "WLS", "WAAP"))
  })

  test_that("Mean Model Estimates (rho) plot matches", {
    plotName <- results[["results"]][["plotEstimates"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "mean-model-estimates-rho")
  })
}
