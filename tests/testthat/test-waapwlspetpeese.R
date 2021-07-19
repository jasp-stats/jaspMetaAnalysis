context("Meta Analysis - WAAP-WLS & PET-PEESE")

### output for all default settings
{
  options <- analysisOptions("WaapWlsPetPeese")
  options$estimatesPetPeese <- TRUE
  options$estimatesSigma <- TRUE
  options$inputES <- "contNormal"
  options$inputSE <- "contGamma"
  options$plotModels <- TRUE
  options$regressionPeese <- TRUE
  options$regressionPet <- TRUE
  set.seed(1)
  results <- runAnalysis("WaapWlsPetPeese", "debug.csv", options)


  test_that("Mean Estimates(Î¼) table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_estimatesMean"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, -0.108777577319913, -0.317460586839519, 0.309438604923354,
                                        0.106472879688439, -1.02164586548442, "WLS", 0.0999054321996926,
                                        "WAAP", 98, -0.0534372106989058, -0.365542508416515, 0.737908055107035,
                                        0.159240322873, -0.33557587509744, "PET", 0.258668087018703,
                                        98, -0.099522276001487, -0.320999247127621, 0.380622055857872,
                                        0.113000531067467, -0.880723967058771, "PEESE", 0.121954695124647
                                   ))
  })

  test_that("Multiplicative Heterogeneity Estimates table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_heterogeneity"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1.30091891089807, "WLS", 1.30607530257008, "PET", 1.30710817871426,
                                        "PEESE"))
  })

  test_that("PET-PEESE Regression Estimates table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_petPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(98, -0.0912254002529703, -0.472565404562442, 0.640204641058565,
                                        0.194564801862398, -0.468868980307587, "PET", 0.290114604056502,
                                        98, -0.0138169763943549, -0.120304772731226, 0.799789900219542,
                                        0.0543315066893236, -0.254308728697008, "PEESE", 0.0926708199425164
                                   ))
  })

  test_that("Test of Publication Bias table results match", {
    table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(98, 0.640204641058565, -0.468868980307587, "PET"))
  })

  test_that("Test of Effect table results match", {
    table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_effectTest"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, 0.309438604923354, -1.02164586548442, "WLS", "WAAP", 98, 0.737908055107035,
                                        -0.33557587509744, "PET"))
  })

  test_that("Estimated PEESE Regression plot matches", {
    plotName <- results[["results"]][["peeseRegression"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-estimated-peese-regression")
  })

  test_that("Estimated PET Regression plot matches", {
    plotName <- results[["results"]][["petRegression"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-estimated-pet-regression")
  })

  test_that("Mean Model Estimates (Î¼) plot matches", {
    plotName <- results[["results"]][["plotEstimates"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-mean-model-estimates-î-")
  })
}

### correlation input
{
  options <- analysisOptions("WaapWlsPetPeese")
  options$estimatesPetPeese <- TRUE
  options$estimatesSigma <- TRUE
  options$inputES <- "es"
  options$inputN <- "n"
  options$measures <- "correlation"
  options$plotModels <- TRUE
  options$regressionPeese <- TRUE
  options$regressionPet <- TRUE
  set.seed(1)
  dataset <- data.frame(
    es = runif(100, .1, .3),
    n  = rnbinom(100, 200, .5)
  )
  results <- runAnalysis("WaapWlsPetPeese", dataset, options)


  test_that("Mean Estimates(Ï) table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_estimatesMean"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, 0.204494800935266, 0.194003420566749, 5.26148520294194e-60,
                                        0.00528471740121733, 37.2105528788957, "WLS", 0.214939408964263,
                                        74, 0.207788715062346, 0.195955387443811, 1.8221078673566e-46,
                                        0.00595663896635116, 33.5010375224788, "WAAP", 0.219561535437517,
                                        98, 0.257973623720358, 0.047088822495066, 0.0195398509124503,
                                        0.0922805546423889, 2.3741172881047, "PET", 0.427634849559746,
                                        98, 0.23278815316705, 0.12946176384105, 4.42134957919393e-05,
                                        0.0486358563634502, 4.27608870403368, "PEESE", 0.326248496575007
                                   ))
  })

  test_that("Multiplicative Heterogeneity Estimates table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_heterogeneity"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.783705550212468, "WLS", 0.782952688284678, "WAAP", 0.786466872404923,
                                        "PET", 0.786202556619872, "PEESE"))
  })

  test_that("PET-PEESE Regression Estimates table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_petPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(98, -0.885634178219958, -4.02338359417463, 0.58138452359721, 1.60092197647755,
                                        -0.553202586529912, "PET", 2.25211523773471, 98, -6.78354497523552,
                                        -28.5781699135719, 0.543250003765216, 11.1199109321649, -0.610035909155872,
                                        "PEESE", 15.0110799631009))
  })

  test_that("Test of Publication Bias table results match", {
    table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(98, 0.58138452359721, -0.553202586529912, "PET"))
  })

  test_that("Test of Effect table results match", {
    table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_effectTest"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, 5.26148520294194e-60, 37.2105528788957, "WLS", 74, 1.8221078673566e-46,
                                        33.5010375224788, "WAAP", 98, 0.0195398509124503, 2.3741172881047,
                                        "PET"))
  })

  test_that("Estimated PEESE Regression plot matches", {
    plotName <- results[["results"]][["peeseRegression"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-estimated-peese-regression")
  })

  test_that("Estimated PET Regression plot matches", {
    plotName <- results[["results"]][["petRegression"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-estimated-pet-regression")
  })

  test_that("Mean Model Estimates (Ï) plot matches", {
    plotName <- results[["results"]][["plotEstimates"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-mean-model-estimates-ï-")
  })
}
{
  options <- analysisOptions("WaapWlsPetPeese")
  options$estimatesPetPeese <- TRUE
  options$estimatesSigma <- TRUE
  options$inputES <- "es"
  options$inputN <- "n"
  options$measures <- "correlation"
  options$muTransform <- "cohensD"
  options$plotModels <- TRUE
  options$regressionPeese <- TRUE
  options$regressionPet <- TRUE
  set.seed(1)
  dataset <- data.frame(
    es = runif(100, .1, .3),
    n  = rnbinom(100, 200, .5)
  )
  results <- runAnalysis("WaapWlsPetPeese", dataset, options)

  test_that("Mean Estimates(Ï) table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_estimatesMean"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, 0.204177965488621, 0.193670346059103, 2.31975858627933e-59,
                                        0.00534349865123354, 36.6175938716485, "WLS", 0.214615485800175,
                                        74, 0.206226772290166, 0.194360875622359, 9.01931583618286e-46,
                                        0.00603147013438606, 32.7376328111126, "WAAP", 0.218002385351795,
                                        98, 0.0668210463696642, -0.0477142481674846, 0.254539518103992,
                                        0.0570007362484864, 1.14611237250867, "PET", 0.169485506372986,
                                        98, 0.0862988609408255, 0.0303359606087919, 0.00346914004315054,
                                        0.027736105328418, 2.99556512966557, "PEESE", 0.138359498977251
                                   ))
  })

  test_that("Multiplicative Heterogeneity Estimates table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_heterogeneity"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.797615124542611, "WLS", 0.797164547505541, "WAAP", 0.800090187034191,
                                        "PET", 0.800399326247497, "PEESE"))
  })

  test_that("PET-PEESE Regression Estimates table results match", {
    table <- results[["results"]][["estimates"]][["collection"]][["estimates_petPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(98, 1.02921858640322, -2.20742285810834, 0.534569165363249, 1.65137802022984,
                                        0.623248325819407, "PET", 4.26586003091477, 98, 3.20292121854694,
                                        -8.02767588530821, 0.577454674891103, 5.73000177168594, 0.558973861818639,
                                        "PEESE", 14.4335183224021))
  })

  test_that("Test of Publication Bias table results match", {
    table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(98, 0.534569165363249, 0.623248325819407, "PET"))
  })

  test_that("Test of Effect table results match", {
    table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_effectTest"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(99, 2.31975858627933e-59, 36.6175938716485, "WLS", 74, 9.01931583618286e-46,
                                        32.7376328111126, "WAAP", 98, 0.254539518103992, 1.14611237250867,
                                        "PET"))
  })

  test_that("Estimated PEESE Regression plot matches", {
    plotName <- results[["results"]][["peeseRegression"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test3-estimated-peese-regression")
  })

  test_that("Estimated PET Regression plot matches", {
    plotName <- results[["results"]][["petRegression"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test3-estimated-pet-regression")
  })

  test_that("Mean Model Estimates (Ï) plot matches", {
    plotName <- results[["results"]][["plotEstimates"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test3-mean-model-estimates-ï-")
  })
}
