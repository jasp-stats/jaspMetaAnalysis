context("Meta Analysis - Selection Models")

# output for all default settings ----
options <- jaspTools::analysisOptions("SelectionModels")
options$plotsWeightFunctionFixedEffectsPlot <- TRUE
options$inferenceFixedEffectsEstimatedWeightsTable <- TRUE
options$inferenceRandomEffectsEstimatedHeterogeneityTable <- TRUE
options$plotsWeightFunctionRandomEffectsPlot <- TRUE
options$inferenceRandomEffectsEstimatedWeightsTable <- TRUE
options$modelPValueCutoffs <- "(.05)"
options$effectSize <- "contNormal"
options$effectSizeSe <- "contGamma"
options$modelPValueFrequencyTable <- TRUE
options$plotsMeanModelEstimatesPlot <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("SelectionModels", "debug.csv", options)


test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_meanFE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.108777577319928, -0.269189581303854, 0.183822096876851, 0.0818443630848501,
                           -1.32907842666153, "Unadjusted", 0.0516344266639978, -0.11716380673004,
                           -0.285826305450531, 0.173350398233601, 0.086053876525732, -1.36151689453531,
                           "Adjusted", 0.0514986919904518))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_inferenceFixedEffectsEstimatedWeightsTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 0, 0, 1, 0.025, 0.376326541603695, 0.01803538997387, 0.025,
                           0.0395303509582151, 0.182804967058569, 2.0586231745176, 0.734617693233521,
                           0.975, 0.608603517567196, 0, 0.975, 0.171765955252482, 0.445357204743356,
                           1.3665514133041, 1.4814875991196, 1))
})

test_that("Weight Function (Fixed Effects) plot matches", {
  plotName <- results[["results"]][["FE_weights"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "weight-function-fixed-effects-1")
})

test_that("Heterogeneity Estimates (tau) table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedHeterogeneityTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.827282385784749, 0.532667964302723, 0.000814125410133252, 3.34794713005884,
                           "Unadjusted", 1.04166075641977, 0.943446276464169, 0.491474185571525,
                           0.00714643352932713, 2.68994248199423, "Adjusted", 1.24041721935203
                      ))
})

test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_meanRE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.144884644995104, -0.441684070668495, 0.338683528291211, 0.151431060986072,
                           -0.956769661730296, "Unadjusted", 0.151914780678286, -0.339172712645018,
                           -0.833591009895464, 0.178773096114283, 0.252258868606951, -1.34454227325299,
                           "Adjusted", 0.155245584605427))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedWeightsTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 0, 0, 1, 0.025, 1.14398176102268, 0, 0.025, 0.111466938672115,
                           0.71874669301328, 1.59163412109298, 2.55269939333597, 0.975,
                           0.439673866838272, 0, 0.975, 0.257423397168529, 0.388232271154997,
                           1.13250211150721, 1.20059513593825, 1))
})

test_that("Weight Function (Random Effects) plot matches", {
  plotName <- results[["results"]][["RE_weights"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "weight-function-random-effects-1")
})

test_that("Test of Publication Bias table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  expect_equal_tables(table,
                      list(2, 0.119168059504646, 4.25444103486723, "Assuming homogeneity",
                           2, 0.231243622868297, 2.92856695951119, "Assuming heterogeneity"
                      ))
})

test_that("Test of Heterogeneity table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_heterogeneityTest"]][["data"]]
  expect_equal_tables(table,
                      list(99, 2.70555845478692e-05, 167.546611260491))
})

test_that("p-value Frequency table results match", {
  table <- results[["results"]][["pFrequency"]][["data"]]
  expect_equal_tables(table,
                      list(5, 0, 0.025, 90, 0.025, 0.975, 5, 0.975, 1))
})

test_that("Mean Model Estimates (mu) plot matches", {
  plotName <- results[["results"]][["plotEstimates"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "mean-model-estimates-mu-1")
})


# weight function scaling works ----
options <- jaspTools::analysisOptions("SelectionModels")
options$inferenceFixedEffectsMeanEstimatesTable <- FALSE
options$plotsWeightFunctionFixedEffectsPlot <- TRUE
options$inferenceRandomEffectsMeanEstimatesTable <- FALSE
options$plotsWeightFunctionRandomEffectsPlot <- TRUE
options$modelPValueCutoffs <- "(.05)"
options$effectSize <- "contNormal"
options$effectSizeSe <- "contGamma"
options$plotsWeightFunctionRescaleXAxis <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("SelectionModels", "debug.csv", options)


test_that("[x-scaled] Weight Function (Fixed Effects) plot matches", {
  plotName <- results[["results"]][["FE_weights"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "weight-function-fixed-effects-2")
})

test_that("[x-scaled] Weight Function (Random Effects) plot matches", {
  plotName <- results[["results"]][["RE_weights"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "weight-function-random-effects-2")
})


# one sided selection & expected negative direction works ----
options <- jaspTools::analysisOptions("SelectionModels")
options$inferenceFixedEffectsEstimatedWeightsTable <- TRUE
options$inferenceRandomEffectsEstimatedHeterogeneityTable <- TRUE
options$inferenceRandomEffectsEstimatedWeightsTable <- TRUE
options$modelPValueCutoffs <- "(.05)"
options$modelExpectedDirectionOfEffectSizes <- "negative"
options$effectSize <- "contNormal"
options$effectSizeSe <- "contGamma"
options$modelTwoSidedSelection <- FALSE
set.seed(1)
results <- jaspTools::runAnalysis("SelectionModels", "debug.csv", options)


test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_meanFE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.108777577319928, -0.269189581303854, 0.183822096876851, 0.0818443630848501,
                           -1.32907842666153, "Unadjusted", 0.0516344266639978, -0.111944393246252,
                           -0.289513693849079, 0.216602360468526, 0.0905982467042613, -1.23561324107818,
                           "Adjusted", 0.0656249073565744))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_inferenceFixedEffectsEstimatedWeightsTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 0, 0, 1, 0.05, 1.0390114365016, 0.0922569515176939, 0.05,
                           0.0314798599518836, 0.48304687864256, 2.15095362881009, 1.98576592148551,
                           1))
})

test_that("Heterogeneity Estimates (Ď„) table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedHeterogeneityTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.827282385784749, 0.532667964302723, 0.000814125410133252, 3.34794713005884,
                           "Unadjusted", 1.04166075641977, 0.984135311386007, 0.579148049074138,
                           0.00271468346363547, 2.99832462391542, "Adjusted", 1.26555606730294
                      ))
})

test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_meanRE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.144884644995104, -0.441684070668495, 0.338683528291211, 0.151431060986072,
                           -0.956769661730296, "Unadjusted", 0.151914780678286, -0.558733086935526,
                           -1.06429019583397, 0.0303021145800322, 0.257942040203908, -2.16611873928669,
                           "Adjusted", -0.0531759780370833))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedWeightsTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 0, 0, 1, 0.05, 4.05572161073559, 0, 0.05, 0.0710518148153868,
                           2.24675526054569, 1.80514615096553, 8.45928100348104, 1))
})

test_that("Test of Publication Bias table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  expect_equal_tables(table,
                      list(1, 0.934145999669388, 0.00682766423935277, "Assuming homogeneity",
                           1, 0.00620168963938165, 7.49074491600277, "Assuming heterogeneity"
                      ))
})

test_that("Test of Heterogeneity table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_heterogeneityTest"]][["data"]]
  expect_equal_tables(table,
                      list(99, 2.70555845478692e-05, 167.546611260491))
})

# different cutoffs without automatic joining works ----
options <- jaspTools::analysisOptions("SelectionModels")
options$plotsWeightFunctionFixedEffectsPlot <- TRUE
options$inferenceFixedEffectsEstimatedWeightsTable <- TRUE
options$inferenceRandomEffectsEstimatedHeterogeneityTable <- TRUE
options$plotsWeightFunctionRandomEffectsPlot <- TRUE
options$inferenceRandomEffectsEstimatedWeightsTable <- TRUE
options$modelAutomaticallyJoinPValueIntervals <- FALSE
options$modelPValueCutoffs <- "(.3, .8, .999)"
options$modelExpectedDirectionOfEffectSizes <- "negative"
options$effectSize <- "contNormal"
options$effectSizeSe <- "contGamma"
options$modelPValueFrequencyTable <- TRUE
options$plotsWeightFunctionRescaleXAxis <- TRUE
options$modelTwoSidedSelection <- FALSE
set.seed(1)
results <- jaspTools::runAnalysis("SelectionModels", "debug.csv", options)


test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_meanFE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.108777577319928, -0.269189581303854, 0.183822096876851, 0.0818443630848501,
                           -1.32907842666153, "Unadjusted", 0.0516344266639978, -0.153847941286966,
                           -0.376005843961891, 0.174683758966281, 0.113347951506904, -1.35730676418616,
                           "Adjusted", 0.068309961387959))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_inferenceFixedEffectsEstimatedWeightsTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 0, 0, 1, 0.3, 1.69565266908451, 0.782559612780323, 0.3,
                           0.00027291713359262, 0.465872364750855, 3.63973654026748, 2.60874572538869,
                           0.8, 0.633848802320969, 0.0409549108661377, 0.8, 0.0361397995717329,
                           0.30250244194868, 2.09535102671502, 1.2267426937758, 0.999,
                           39.2483429180539, 0, 0.999, 0.227637391407715, 32.5316281308455,
                           1.20646721892286, 103.009162412961, 1))
})

test_that("Weight Function (Fixed Effects) plot matches", {
  plotName <- results[["results"]][["FE_weights"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "weight-function-fixed-effects-3")
})

test_that("Heterogeneity Estimates (tau) table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedHeterogeneityTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.827282385784749, 0.532667964302723, 0.000814125410133252, 3.34794713005884,
                           "Unadjusted", 1.04166075641977, 1.0747401921054, 0.525845482178296,
                           0.00997086944826132, 2.5768379111574, "Adjusted", 1.42605031114831
                      ))
})

test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_meanRE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.144884644995104, -0.441684070668495, 0.338683528291211, 0.151431060986072,
                           -0.956769661730296, "Unadjusted", 0.151914780678286, 0.0918536888331539,
                           -0.819418767124643, 0.843390247253674, 0.464943469954447, 0.197558831920262,
                           "Adjusted", 1.00312614479095))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedWeightsTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 0, 0, 1, 0.3, 2.04088487425868, 0.668265339061214, 0.3,
                           0.00356623838422433, 0.700328958095412, 2.91418033006802, 3.41350440945615,
                           0.8, 0.394220296413067, 0, 0.8, 0.130373022918058, 0.260618679700491,
                           1.51263254370759, 0.90502352232441, 0.999, 0.629143307089999,
                           0, 0.999, 0.464906129098306, 0.860905965056294, 0.73079213366684,
                           2.31648799267603, 1))
})

test_that("Weight Function (Random Effects) plot matches", {
  plotName <- results[["results"]][["RE_weights"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "weight-function-random-effects-3")
})

test_that("Test of Publication Bias table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  expect_equal_tables(table,
                      list(3, 0.00011133894641776, 20.8828940999699, "Assuming homogeneity",
                           3, 1.24070625524803e-05, 25.4541785368763, "Assuming heterogeneity"
                      ))
})

test_that("Test of Heterogeneity table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_heterogeneityTest"]][["data"]]
  expect_equal_tables(table,
                      list(99, 2.70555845478692e-05, 167.546611260491))
})

test_that("p-value Frequency table results match", {
  table <- results[["results"]][["pFrequency"]][["data"]]
  expect_equal_tables(table,
                      list(27, 0, 0.3, 63, 0.3, 0.8, 8, 0.8, 0.999, 2, 0.999, 1))
})

# different cutoffs with automatic joining works ----
options <- jaspTools::analysisOptions("SelectionModels")
options$plotsWeightFunctionFixedEffectsPlot <- TRUE
options$inferenceFixedEffectsEstimatedWeightsTable <- TRUE
options$inferenceRandomEffectsEstimatedHeterogeneityTable <- TRUE
options$plotsWeightFunctionRandomEffectsPlot <- TRUE
options$inferenceRandomEffectsEstimatedWeightsTable <- TRUE
options$modelPValueCutoffs <- "(.3, .8, .999)"
options$modelExpectedDirectionOfEffectSizes <- "negative"
options$effectSize <- "contNormal"
options$effectSizeSe <- "contGamma"
options$modelPValueFrequencyTable <- TRUE
options$plotsWeightFunctionRescaleXAxis <- TRUE
options$modelTwoSidedSelection <- FALSE
set.seed(1)
results <- jaspTools::runAnalysis("SelectionModels", "debug.csv", options)


test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_meanFE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.108777577319928, -0.269189581303854, 0.183822096876851, 0.0818443630848501,
                           -1.32907842666153, "Unadjusted", 0.0516344266639978, -0.12253530646152,
                           -0.355555366961861, 0.302699620457382, 0.118889970600671, -1.03066142452918,
                           "Adjusted", 0.110484754038821))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_inferenceFixedEffectsEstimatedWeightsTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 0, 0, 1, 0.3, 1.6273645316654, 0.745760379455773, 0.3, 0.000296976802455585,
                           0.449806302138001, 3.61792292355682, 2.50896868387502, 0.8,
                           0.734890315410054, 0.075702362948048, 0.8, 0.0288851901491054,
                           0.336326563988725, 2.18504987145378, 1.39407826787206, 1))
})


test_that("Heterogeneity Estimates (tau) table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedHeterogeneityTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.827282385784749, 0.532667964302723, 0.000814125410133252, 3.34794713005884,
                           "Unadjusted", 1.04166075641977, 1.12013432591779, 0.653723752589082,
                           0.00295522337136342, 2.97235752098961, "Adjusted", 1.44293003000819
                      ))
})

test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_meanRE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.144884644995104, -0.441684070668495, 0.338683528291211, 0.151431060986072,
                           -0.956769661730296, "Unadjusted", 0.151914780678286, 0.205359892764449,
                           -0.613990043882111, 0.623256252747511, 0.418043363607438, 0.491240647841718,
                           "Adjusted", 1.02470982941101))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedWeightsTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 0, 0, 1, 0.3, 1.95909314883835, 0.681531198843923, 0.3,
                           0.00265117591129094, 0.651829298942061, 3.00553097569873, 3.23665509883278,
                           0.8, 0.368087005023669, 0, 0.8, 0.118743607281528, 0.235942792209477,
                           1.56006886913872, 0.83052638016606, 1))
})

test_that("Test of Publication Bias table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  expect_equal_tables(table,
                      list(2, 0.0120813330893559, 8.83218747497003, "Assuming homogeneity",
                           2, 3.27514663506786e-06, 25.2582958316257, "Assuming heterogeneity"
                      ))
})

test_that("Test of Heterogeneity table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_heterogeneityTest"]][["data"]]
  expect_equal_tables(table,
                      list(99, 2.70555845478692e-05, 167.546611260491))
})

test_that("p-value Frequency table results match", {
  table <- results[["results"]][["pFrequency"]][["data"]]
  expect_equal_tables(table,
                      list(27, 0, 0.3, 63, 0.3, 0.8, 10, 0.8, 1))
})

# supplying p-values work ----
options <- jaspTools::analysisOptions("SelectionModels")
options$modelAutomaticallyJoinPValueIntervals <- FALSE
options$modelPValueCutoffs <- "(.01)"
options$modelExpectedDirectionOfEffectSizes <- "negative"
options$effectSize <- "ES"
options$effectSizeSe <- "SE"
options$pValue <- "pval"
options$modelPValueFrequencyTable <- TRUE
options$modelTwoSidedSelection <- FALSE
set.seed(1)
dataset <-
  structure(
    list(
      trial = 1:13,
      author = structure(
        c(1L, 5L, 8L,
          7L, 6L, 9L, 11L, 10L, 2L, 8L, 4L, 3L, 4L),
        .Label = c(
          "Aronson",
          "Coetzee & Berjak",
          "Comstock & Webster",
          "Comstock et al",
          "Ferguson & Simes",
          "Frimodt-Moller et al",
          "Hart & Sutherland",
          "Rosenthal et al",
          "Stein & Aronson",
          "TPT Madras",
          "Vandiviere et al"
        ),
        class = "factor"
      ),
      year = c(
        1948L,
        1949L,
        1960L,
        1977L,
        1973L,
        1953L,
        1973L,
        1980L,
        1968L,
        1961L,
        1974L,
        1969L,
        1976L
      ),
      tpos = c(4L, 6L,
               3L, 62L, 33L, 180L, 8L, 505L, 29L, 17L, 186L, 5L, 27L),
      tneg = c(
        119L,
        300L,
        228L,
        13536L,
        5036L,
        1361L,
        2537L,
        87886L,
        7470L,
        1699L,
        50448L,
        2493L,
        16886L
      ),
      cpos = c(11L, 29L, 11L, 248L, 47L,
               372L, 10L, 499L, 45L, 65L, 141L, 3L, 29L),
      cneg = c(
        128L,
        274L,
        209L,
        12619L,
        5761L,
        1079L,
        619L,
        87892L,
        7232L,
        1600L,
        27197L,
        2338L,
        17825L
      ),
      ablat = c(44L, 55L, 42L, 52L, 13L,
                44L, 19L, 13L, 27L, 42L, 18L, 33L, 33L),
      alloc = structure(
        c(2L,
          2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
        .Label = c("alternate",
                   "random", "systematic"),
        class = "factor"
      ),
      ES = c(
        -0.9387,-1.6662,
        -1.3863,
        -1.4564,
        -0.2191,
        -0.9581,
        -1.6338,
        0.012,-0.4717,
        -1.4012,
        -0.3408,
        0.4466,
        -0.0173
      ),
      SE = c(
        0.5976,
        0.4562,
        0.6583,
        0.1425,
        0.2279,
        0.0995,
        0.4765,
        0.0633,
        0.2387,
        0.2746,
        0.1119,
        0.7309,
        0.2676
      ),
      pval = c(
        0.000172347,
        0.000571675,
        0.00210445,
        0.00215962,
        0.0187103,
        4.25489e-13,
        1.47842e-17,
        5.34304e-07,
        0.00026339,
        8.66357e-23,
        0.00712692,
        8.36688e-12,
        0.0199237
      )
    ),
    class = "data.frame",
    row.names = c(NA,-13L)
  )
results <- jaspTools::runAnalysis("SelectionModels", dataset, options)


test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_meanFE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.436205104253414, -0.519035731383147, 5.62654240706622e-25,
                           0.0422613006070984, -10.3216204420397, "Unadjusted", -0.35337447712368,
                           -0.361655208018845, -0.457794033381546, 1.66835395003008e-13,
                           0.0490513224329791, -7.37299608003413, "Adjusted", -0.265516382656145
                      ))
})

test_that("Mean Estimates (mu) table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_meanRE"]][["data"]]
  expect_equal_tables(table,
                      list(-0.741950442278819, -1.0920698068352, 3.27528956955066e-05, 0.178635611326573,
                           -4.15342963684, "Unadjusted", -0.391831077722441, 0.380984681788735,
                           -0.688255571057507, 0.484951130715548, 0.545540765687672, 0.698361526307738,
                           "Adjusted", 1.45022493463498))
})

test_that("Test of Publication Bias table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1.67613271825012e-06, 22.9345348682999, "Assuming homogeneity",
                           1, 3.83999830480277e-05, 16.9488792381323, "Assuming heterogeneity"
                      ))
})

test_that("Test of Heterogeneity table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_heterogeneityTest"]][["data"]]
  expect_equal_tables(table,
                      list(12, 4.44385100747795e-28, 163.194038293184))
})

test_that("p-value Frequency table results match", {
  table <- results[["results"]][["pFrequency"]][["data"]]
  expect_equal_tables(table,
                      list(11, 0, 0.01, 2, 0.01, 1))
})

### output of the default settings with correlations as an input
options <- jaspTools::analysisOptions("SelectionModels")
options$inferenceFixedEffectsEstimatedWeightsTable <- TRUE
options$inferenceRandomEffectsEstimatedHeterogeneityTable <- TRUE
options$inferenceRandomEffectsEstimatedWeightsTable <- TRUE
options$modelPValueCutoffs <- "(.05, .10)"
options$effectSize <- "es"
options$sampleSize <- "n"
options$measures <- "correlation"
options$modelPValueFrequencyTable <- TRUE
set.seed(1)
dataset <- data.frame(
  es = runif(100, .1, .3),
  n  = rnbinom(100, 200, .5)
)
results <- jaspTools::runAnalysis("SelectionModels", dataset, options)


test_that("Mean Estimates table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_meanFE"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.204177965488621, 0.190993378036, 1.59205580346668e-187, 0.00669934469245971,
                                      29.2067466964043, "Unadjusted", 0.217252375590527, 0.195163279975223,
                                      0.176695769355625, 2.8851502702737e-89, 0.00937139161237495,
                                      20.0322183701612, "Adjusted", 0.213425528679819))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceFixedEffectsMeanEstimatesTable"]][["collection"]][["inferenceFixedEffectsMeanEstimatesTable_inferenceFixedEffectsEstimatedWeightsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 0, 0, 1, 0.025, 0.946505734640249, 0.180360120650839, 0.025,
                                      0.0154623999208921, 0.390897802221199, 2.42136366401121, 1.71265134862966,
                                      0.05, 0.424477454915518, 0.00516906460893385, 0.05, 0.0472418366269367,
                                      0.213936783335834, 1.98412562952852, 0.843785845222102, 1))
})

test_that("Heterogeneity Estimates table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedHeterogeneityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 1, 0, "Unadjusted", 0.103411887992993, 0, 0, 1, 0, "Adjusted",
                                      0.2426036713353))
})

test_that("Mean Estimates table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_meanRE"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.204177965488621, 0.190991487999372, 1.79846926082398e-187, 0.00670030120287679,
                                      29.2025772484021, "Unadjusted", 0.217254234170054, 0.195163505879826,
                                      0.0875521107541049, 0.000447334225995675, 0.0534773308147392,
                                      3.51045873910709, "Adjusted", 0.296177427245501))
})

test_that("Estimated Weights table results match", {
  table <- results[["results"]][["inferenceRandomEffectsMeanEstimatesTable"]][["collection"]][["inferenceRandomEffectsMeanEstimatesTable_inferenceRandomEffectsEstimatedWeightsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 0, 0, 1, 0.025, 0.946521152729813, 0, 0.025, 0.436970853703284,
                                      1.21767472715722, 0.777318549543651, 3.3331197628426, 0.05,
                                      0.424482984589334, 0, 0.05, 0.748207603440691, 1.32236174634355,
                                      0.321003678277194, 3.01626438195619, 1))
})

test_that("Test of Publication Bias table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_biasTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2, 0.175586023767488, 3.4792523845, "Assuming homogeneity", 2,
                                      0.175586023955723, 3.47925238235592, "Assuming heterogeneity"
                                 ))
})

test_that("Test of Heterogeneity table results match", {
  table <- results[["results"]][["fitTests"]][["collection"]][["fitTests_heterogeneityTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(99, 0.998589436447844, 62.9827988030128))
})

test_that("p-value Frequency table results match", {
  table <- results[["results"]][["pFrequency"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(86, 0, 0.025, 8, 0.025, 0.05, 6, 0.05, 1))
})

