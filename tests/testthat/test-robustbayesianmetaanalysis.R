context("Robust Bayesian Meta-Analysis")


### create a pre-fitted model (in case that the package needs to be updated)
if (FALSE){
  # fit a default model
  fit <- RoBMA::RoBMA(d = c(.3, .2, .1), n = c(30, 35, 40), iter = 4000, burnin = 4000, chains = 2, control = list(silent = TRUE), seed = 666)
  # remove majority of the samples to save space
  for(i in 2:length(fit$models)){
    for(j in seq_along(fit$models[[i]]$fit$mcmc)){
      fit$models[[i]]$fit$mcmc[[j]] <- fit$models[[i]]$fit$mcmc[[j]][1:100,]
    }
  }
  set.seed(666)
  for(i in 1:2){
    for(p in c("mu", "tau")){
      fit$RoBMA$samples[[i]][[p]] <- sample(fit$RoBMA$samples[[i]][[p]], 100)
    }
    for(p in c("omega", "theta")){
      fit$RoBMA$samples[[i]][[p]] <- fit$RoBMA$samples[[i]][[p]][sample(nrow(fit$RoBMA$samples[[i]][[p]]), 100),]
    }
  }
  saveRDS(fit, file = "robmaFit.RDS")
}

# path to the pre-fitted RoBMA model
fittedPath <- file.path("robmaFit.RDS")

### prior distibutions plots 
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(inputCI = list(containsColumn = TRUE), inputSE = list(
    containsColumn = TRUE), inputN = list(containsColumn = TRUE), 
    inputN1 = list(containsColumn = TRUE), inputN2 = list(containsColumn = TRUE), 
    inputSE = list(containsColumn = TRUE), inputLabels = list(
      containsColumn = TRUE), inputT = list(containsColumn = TRUE), 
    priorsMu = list(list(name = list(containsColumn = TRUE)), 
                     list(name = list(containsColumn = TRUE)), list(name = list(
                       containsColumn = TRUE)), list(name = list(containsColumn = TRUE)), 
                     list(name = list(containsColumn = TRUE)), list(name = list(
                       containsColumn = TRUE)), list(name = list(containsColumn = TRUE)), 
                     list(name = list(containsColumn = TRUE))), priorsOmega = list(
                       list(name = list(containsColumn = TRUE)), list(name = list(
                         containsColumn = TRUE))), priorsTau = list(list(
                           name = list(containsColumn = TRUE))))
  options$advancedControl <- "clever"
  options$fittedPath <- ""
  options$inputCI <- list()
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "observed"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "2", truncationUpper = "7", type = "normal"), 
                            list(name = "2", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "4", truncationUpper = "7", type = "t"), 
                            list(name = "3", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-3", truncationUpper = "Inf", type = "cauchy"), 
                            list(name = "4", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "gammaK0"), 
                            list(name = "5", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "invgamma"), 
                            list(name = "6", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"), 
                            list(name = "7", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "uniform"), 
                            list(name = "8", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "0", truncationUpper = "Inf", type = "gammaAB"))
  options$priorsMuNull <- list()
  options$priorsNull <- TRUE
  options$priorsOmega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"), list(name = "2", parAlpha = "(1,1,1)", 
                                                              parAlpha1 = "(1,1,1)", parAlpha2 = "(1,1)", parCuts = "(.05, .10)", 
                                                              priorOdds = "1/2", type = "Two-sided"))
  options$priorsOmegaNull <- list()
  options$priorsPlot <- TRUE
  options$priorsTau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priorsTauNull <- list()
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$measures  <- "cohensD"
  set.seed(1)
  dataset <- NULL
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_modelsSummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0.0625, "Normal(0, 1)[2, 7]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 2, 0.0625, "Normal(0, 1)[2, 7]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             3, 0.0625, "gen. Student-t(0, 1, 2)[4, 7]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 4, 0.0625, "gen. Student-t(0, 1, 2)[4, 7]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             5, 0.0625, "Cauchy(0, 1)[-3, Inf]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 6, 0.0625, "Cauchy(0, 1)[-3, Inf]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             7, 0.0625, "Gamma(1, 1)[0, Inf]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 8, 0.0625, "Gamma(1, 1)[0, Inf]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             9, 0.0625, "InvGamma(1, 0.15)[0, Inf]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 10, 0.0625, "InvGamma(1, 0.15)[0, Inf]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             11, 0.0625, "Spike(0)", "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             12, 0.0625, "Spike(0)", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 13, 0.0625, "Uniform(0, 1)", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 14, 0.0625, "Uniform(0, 1)", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 15, 0.0625, "Gamma(1, 0.15)[0, Inf]",
                             "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]", 16,
                             0.0625, "Gamma(1, 0.15)[0, Inf]", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_overallSummary"]][["data"]]
    expect_equal_tables(table,
                        list("16/16", 1, "Effect", "16/16", 1, "Heterogeneity", "16/16", 1,
                             "Publication bias"))
  })
  
  test_that("Priors plot mu (1) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-2-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (2) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-3-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (3) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-4-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (4) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_4"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-5-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (5) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_5"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-6-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (6) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_6"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-7-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (7) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_7"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-8-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (8) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_8"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-9-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot omega (1) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_omega"]][["collection"]][["priorPlots_omega_alternative"]][["collection"]][["priorPlots_omega_alternative_omega_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-10-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot omega (2) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_omega"]][["collection"]][["priorPlots_omega_alternative"]][["collection"]][["priorPlots_omega_alternative_omega_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-11-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot tau (1) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_tau"]][["collection"]][["priorPlots_tau_alternative"]][["collection"]][["priorPlots_tau_alternative_tau_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-12-default", dir="RobustBayesianMetaAnalysis")
  })
  
}
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedControl <- "clever"
  options$advancedMuTransform <- "cohens_d"
  options$fittedPath <- ""
  options$inputCI <- list()
  options$measures <- "correlation"
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "observed"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"), 
                            list(name = "2", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "3", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsOmega <- list()
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsPlot <- TRUE
  options$priorsTau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$priorsTauNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                       truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_modelsSummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0.166666666666667, "Spike(0)", "Spike(1)", "Spike(0)", 2, 0.166666666666667,
                             "Spike(0)", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]", 3, 0.166666666666667,
                             "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "Spike(0)", 4, 0.166666666666667,
                             "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]",
                             5, 0.166666666666667, "Normal(3, 1)[-Inf, Inf]", "Spike(1)",
                             "Spike(0)", 6, 0.166666666666667, "Normal(3, 1)[-Inf, Inf]",
                             "Spike(1)", "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_overallSummary"]][["data"]]
    expect_equal_tables(table,
                        list("4/6", 0.666666666666667, "Effect", "3/6", 0.5, "Heterogeneity",
                             "0/6", 0, "Publication bias"))
  })
  
  test_that("Priors plot mu (1) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-2-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (2) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-3-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (3) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_null"]][["collection"]][["priorPlots_mu_null_mu_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-4-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot omega (1) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_omega"]][["collection"]][["priorPlots_omega_null"]][["collection"]][["priorPlots_omega_null_omega_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-5-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot tau (1) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_tau"]][["collection"]][["priorPlots_tau_alternative"]][["collection"]][["priorPlots_tau_alternative_tau_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-6-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot tau (2) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_tau"]][["collection"]][["priorPlots_tau_null"]][["collection"]][["priorPlots_tau_null_tau_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-7-correlations", dir="RobustBayesianMetaAnalysis")
  })
}
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedControl <- "clever"
  options$advancedMuTransform <- "log_OR"
  options$fittedPath <- ""
  options$inputCI <- list()
  options$measures <- "OR"
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "observed"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = ".3", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"), 
                            list(name = "2", parA = "-.10", parAlpha = "1", parB = ".10", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "3", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "uniform"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsNull <- TRUE
  options$priorsOmega <- list()
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsPlot <- TRUE
  options$priorsTau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$priorsTauNull <- list()
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_modelsSummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0.333333333333333, "Spike(0)", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]",
                             2, 0.333333333333333, "Normal(0, 0.3)[-Inf, Inf]", "Spike(1)",
                             "InvGamma(1, 0.15)[0, Inf]", 3, 0.333333333333333, "Uniform(-0.1, 0.1)",
                             "Spike(1)", "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_overallSummary"]][["data"]]
    expect_equal_tables(table,
                        list("2/3", 0.666666666666667, "Effect", "3/3", 1, "Heterogeneity",
                             "0/3", 0, "Publication bias"))
  })
  
  test_that("Priors plot mu (1) (OR) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-2-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (2) (OR) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-3-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (3) (OR) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_null"]][["collection"]][["priorPlots_mu_null_mu_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-4-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot omega (1) (OR) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_omega"]][["collection"]][["priorPlots_omega_null"]][["collection"]][["priorPlots_omega_null_omega_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-5-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot tau (1) (OR) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_tau"]][["collection"]][["priorPlots_tau_alternative"]][["collection"]][["priorPlots_tau_alternative_tau_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-6-OR", dir="RobustBayesianMetaAnalysis")
  })
}

### fit a default model using d + se, (wihout the more complex weight function) and main output
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(inputCI = list(containsColumn = TRUE), inputSE = list(
    containsColumn = TRUE), inputN = list(containsColumn = TRUE), 
    inputN1 = list(containsColumn = TRUE), inputN2 = list(containsColumn = TRUE), 
    inputSE = list(containsColumn = TRUE), inputLabels = list(
      containsColumn = TRUE), inputT = list(containsColumn = TRUE), 
    priorsMu = list(list(name = list(containsColumn = TRUE))), 
    priorsMuNull = list(list(name = list(containsColumn = TRUE))), 
    priorsOmega = list(list(name = list(containsColumn = TRUE))), 
    priorsOmegaNull = list(list(name = list(containsColumn = TRUE))), 
    priorsTau = list(list(name = list(containsColumn = TRUE))), 
    priorsTauNull = list(list(name = list(containsColumn = TRUE))))
  options$advancedAdapt <- 500
  options$advancedBurnin <- 1000
  options$advancedChains <- 2
  options$advancedControl <- "clever"
  options$advancedIteration <- 4000
  options$fittedPath <- ""
  options$inputCI <- list()
  options$inputES <- "d"
  options$inputSE <- "se"
  options$plotsIndividualMu <- TRUE
  options$plotsIndividualOmega <- TRUE
  options$plotsIndividualTau <- TRUE
  options$plotsMu <- TRUE
  options$plotsOmega <- TRUE
  options$plotsTau <- TRUE
  options$plotsTheta <- TRUE
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "observed"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsOmega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"))
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsTau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priorsTauNull <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                       truncationUpper = "Inf", type = "spike"))
  options$resultsConditional <- TRUE
  options$resultsModels <- TRUE
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath  <- ""
  options$setSeed   <- TRUE
  options$measures  <- "cohensD"
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.169665754659546, 0.191326498116366, "Effect size (<unicode><unicode>)",
                                        0.332066667339197, 0, 0.0477941778146311, 0, "Heterogeneity (<unicode><unicode>)",
                                        0.308991841168739))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.00596182674913896, 0.05, 0.570655502031924,
                                        0.599436713343987, 1, 1))
  })
  
  test_that("Conditional Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.0631141049521503, 0.207480256967044, 0.21023752939073, "Effect size (<unicode><unicode>)",
                                        0.343879382227667, 0.0328510214539317, 0.153439804305514, 0.11916955264611,
                                        "Heterogeneity (<unicode><unicode>)", 0.474154155761386))
  })
  
  test_that("Conditional Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.00406094432096918, 0.05, 0.292282803470328,
                                        0.193635893099643, 0.926616550463227, 1))
  })
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.00205868250507496, -5.25591676931767, 1, 0.000411567043840429,
                                        0.166666666666667, "Spike(0)", "Spike(1)", "Spike(0)", 0.50525583116313,
                                        0.107274695486287, 2, 0.0439152191465916, 0.0833333333333333,
                                        "Spike(0)", "Two-sided((0.05), (1, 1))", "Spike(0)", 0.176776772721767,
                                        -0.83742827505857, 3, 0.0341480385349558, 0.166666666666667,
                                        "Spike(0)", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]", 1.2993959920163,
                                        0.985118417006257, 4, 0.105647138514749, 0.0833333333333333,
                                        "Spike(0)", "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                                        1.96127082927711, 1.27285245363283, 5, 0.281740342730032, 0.166666666666667,
                                        "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "Spike(0)", 6.07542424524418,
                                        2.19938060712744, 6, 0.355799314733647, 0.0833333333333333,
                                        "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))", "Spike(0)",
                                        0.413036202360501, -0.033408332340764, 7, 0.0763039793046989,
                                        0.166666666666667, "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]",
                                        1.2499124686911, 0.950323769130184, 8, 0.102034399991485, 0.0833333333333333,
                                        "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]"
                                   ))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(4.43118258355616, "4/8", 0.815878036759863, 0.5, "Effect", 0.466562857443191,
                                        "4/8", 0.318133556345889, 0.5, "Heterogeneity", 3.09419254197774,
                                        "4/8", 0.607396072386473, 0.333333333333333, "Publication bias"
                                   ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-models-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weights (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weights-conditional-models-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-models-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
}

### fit models with a truncated priors and t + se
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(inputCI = list(containsColumn = TRUE), inputSE = list(
    containsColumn = TRUE), inputN = list(containsColumn = TRUE), 
    inputN1 = list(containsColumn = TRUE), inputN2 = list(containsColumn = TRUE), 
    inputSE = list(containsColumn = TRUE), inputLabels = list(
      containsColumn = TRUE), inputT = list(containsColumn = TRUE), 
    priorsMu = list(list(name = list(containsColumn = TRUE))), 
    priorsMuNull = list(list(name = list(containsColumn = TRUE))), 
    priorsOmegaNull = list(list(name = list(containsColumn = TRUE))), 
    priorsTau = list(list(name = list(containsColumn = TRUE))), 
    priorsTauNull = list(list(name = list(containsColumn = TRUE))))
  options$advancedAdapt <- 500
  options$advancedBurnin <- 1000
  options$advancedChains <- 2
  options$advancedControl <- "clever"
  options$advancedIteration <- 4000
  options$fittedPath <- ""
  options$inputCI <- list()
  options$inputN <- "N"
  options$inputT <- "t"
  options$plotsMu <- TRUE
  options$plotsTau <- TRUE
  options$plotsTheta <- TRUE
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "both"
  options$plotsType <- "conditional"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = ".5", truncationUpper = "Inf", type = "normal"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsOmega <- list()
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsTau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = ".3", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = ".25", truncationUpper = ".50", type = "normal"))
  options$priorsTauNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                       truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$setSeed   <- TRUE
  options$measures  <- "cohensD"
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.534682640935616, 0.559207684613504, "Effect size (<unicode><unicode>)",
                                        0.82869545419437, 0, 0.109533178282388, 0, "Heterogeneity (<unicode><unicode>)",
                                        0.461004655042431))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(9.25894234523558, "2/4", 0.902524064728328, 0.5, "Effect", 0.460233999357121,
                                        "2/4", 0.315178251951223, 0.5, "Heterogeneity", "", "0/4", 0,
                                        0, "Publication bias"))
  })
  
  test_that("Effect size (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-truncated-priors", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-truncated-priors", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Conditional) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-conditional-truncated-priors", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with only an effect size, d + (N1 + N2) and names
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(inputCI = list(containsColumn = TRUE), inputSE = list(
    containsColumn = TRUE), inputN = list(containsColumn = TRUE), 
    inputN1 = list(containsColumn = TRUE), inputN2 = list(containsColumn = TRUE), 
    inputSE = list(containsColumn = TRUE), inputLabels = list(
      containsColumn = TRUE), inputT = list(containsColumn = TRUE), 
    priorsMu = list(list(name = list(containsColumn = TRUE))), 
    priorsMuNull = list(list(name = list(containsColumn = TRUE))), 
    priorsOmegaNull = list(list(name = list(containsColumn = TRUE))), 
    priorsTauNull = list(list(name = list(containsColumn = TRUE))))
  options$advancedAdapt <- 500
  options$advancedBurnin <- 1000
  options$advancedChains <- 2
  options$advancedControl <- "clever"
  options$advancedIteration <- 4000
  options$fittedPath <- ""
  options$inputCI <- list()
  options$measures <- "cohensD"
  options$inputES <- "d"
  options$inputN1 <- "N1"
  options$inputN2 <- "N2"
  options$inputLabels <- "study"
  options$plotsMu <- TRUE
  options$plotsTau <- TRUE
  options$plotsTheta <- TRUE
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "observed"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = ".15", type = "invgamma"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsNull <- TRUE
  options$priorsOmega <- list()
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsTau <- list()
  options$priorsTauNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                       truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.0829965287384381, 0.0947435368896556, "Effect size (<unicode><unicode>)",
                                        0.147272648338966, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                                        0))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(4.26762636300109, "1/2", 0.810161174865433, 0.5, "Effect", "",
                                        "0/2", 0, 0, "Heterogeneity", "", "0/2", 0, 0, "Publication bias"
                                   ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-d-and-n", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-d-and-n", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-d-and-n", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with only one publication bias function, y + (lCI & uCI)
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(inputCI = list(containsColumn = TRUE), inputSE = list(
    containsColumn = TRUE), inputN = list(containsColumn = TRUE), 
    inputN1 = list(containsColumn = TRUE), inputN2 = list(containsColumn = TRUE), 
    inputSE = list(containsColumn = TRUE), inputLabels = list(
      containsColumn = TRUE), inputT = list(containsColumn = TRUE), 
    priorsMuNull = list(list(name = list(containsColumn = TRUE))), 
    priorsOmega = list(list(name = list(containsColumn = TRUE))), 
    priorsOmegaNull = list(list(name = list(containsColumn = TRUE))), 
    priorsTauNull = list(list(name = list(containsColumn = TRUE))))
  options$advancedAdapt <- 500
  options$advancedBurnin <- 1000
  options$advancedChains <- 2
  options$advancedControl <- "clever"
  options$advancedIteration <- 4000
  options$fittedPath <- ""
  options$inputCI <- list(c("lCI", "uCI"))
  options$inputES <- "d"
  options$measures <- "general"
  options$plotsMu <- TRUE
  options$plotsOmega <- TRUE
  options$plotsTau <- TRUE
  options$plotsTheta <- TRUE
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "both"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list()
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsNull <- TRUE
  options$priorsOmega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"))
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsTau <- list()
  options$priorsTauNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                       truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0, 0, "Effect size (<unicode><unicode>)", 0, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                                        0))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.000674752411048917, 0.05, 0.0614219245352222,
                                        0.0223643133148829, 0.389613651090792, 1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("", "0/2", 0, 0, "Effect", "", "0/2", 0, 0, "Heterogeneity", 208.336334775513,
                                        "1/2", 0.990491419363494, 0.333333333333333, "Publication bias"
                                   ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-y-CI", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-y-CI", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-y-CI", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-y-CI", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with OR
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedAdapt <- 100
  options$advancedBurnin <- 1000
  options$advancedChains <- 2
  options$advancedControl <- "clever"
  options$advancedIteration <- 4000
  options$advancedMuTransform <- "cohens_d"
  options$fittedPath <- ""
  options$inputCI <- list(c("ORlCI", "ORuCI"))
  options$inputES <- "OR"
  options$measures <- "OR"
  options$plotsIndividualMu <- TRUE
  options$plotsMu <- TRUE
  options$plotsPriors <- FALSE
  options$plotsTau <- TRUE
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "observed"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualConditional <- FALSE
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsNull <- TRUE
  options$priorsOmega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"))
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsTau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$priorsTauNull <- list()
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = c("study one", "study two", "study three"
  ), t = c(2.51, 2.39, 2.55), N = c(100L, 150L, 97L), d = c(0.25, 
                                                            0.2, 0.26), se = c(0.1, 0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 
                                                                                                                              75L, 48L), lCI = c(0.05, 0.04, 0.06), uCI = c(0.45, 0.41, 0.38
                                                                                                                              ), OR = c(1.1, 1.05, 1.2), ORlCI = c(1, 0.98, 1.1), ORuCI = c(1.2, 
                                                                                                                                                                                            1.1, 1.3)), class = "data.frame", row.names = c(NA, -3L))
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 1.01333388540875, 1, "Effect size (OR)", 1.18540483760199,
                                        0.0371927644147617, 0.0982566858849932, 0.0836211693769752,
                                        "Heterogeneity (<unicode><unicode>)", 0.249272319211574))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.118453054690182, 0.05, 0.837781600778334,
                                        1, 1, 1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.130780995822761, "2/4", 0.115655459638853, 0.5, "Effect", "",
                                        "4/4", 1, 1, "Heterogeneity", 0.9992699980938, "2/4", 0.333171071203623,
                                        0.333333333333333, "Publication bias"))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size vs Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mutau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-vs-heterogeneity-model-averaged-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size (Models) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-models-OR", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with expected negative effect sizes
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedAdapt <- 100
  options$advancedBurnin <- 1000
  options$advancedChains <- 2
  options$advancedControl <- "clever"
  options$advancedIteration <- 4000
  options$advancedMuTransform <- "cohens_d"
  options$effect_direction <- "negative"
  options$fittedPath <- ""
  options$inputCI <- list()
  options$inputES <- "d"
  options$inputSE <- "se"
  options$measures <- "general"
  options$plotsMu <- TRUE
  options$plotsPriors <- FALSE
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "observed"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsNull <- TRUE
  options$priorsOmega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"))
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsTau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$priorsTauNull <- list()
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = c("study one", "study two", "study three"
  ), t = c(2.51, 2.39, 2.55), N = c(100L, 150L, 97L), d = c(0.25, 
                                                            0.2, 0.26), se = c(0.1, 0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 
                                                                                                                              75L, 48L), lCI = c(0.05, 0.04, 0.06), uCI = c(0.45, 0.41, 0.38
                                                                                                                              ), OR = c(1.1, 1.05, 1.2), ORlCI = c(1, 0.98, 1.1), ORuCI = c(1.2, 
                                                                                                                                                                                            1.1, 1.3)), class = "data.frame", row.names = c(NA, -3L))
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.11849465045327, 0.102774650217037, "Effect size (<unicode><unicode>)",
                                        0.367906824223158, 0.152480112226528, 0.0343895409531826, 0.119932781605495,
                                        "Heterogeneity (<unicode><unicode>)", 0.463396512265461))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.00466462118502418, 0.05, 0.519664115759027,
                                        0.438789179669389, 1, 1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1.29307535860093, "2/4", 0.563904432425576, 0.5, "Effect", "",
                                        "4/4", 1, 1, "Heterogeneity", 3.70602699822018, "2/4", 0.64949342149558,
                                        0.333333333333333, "Publication bias"))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-negative-ES", dir="RobustBayesianMetaAnalysis")
  })
}

### more options tested using a preloaded model
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(inputCI = list(containsColumn = TRUE), inputSE = list(
    containsColumn = TRUE), inputN = list(containsColumn = TRUE), 
    inputN1 = list(containsColumn = TRUE), inputN2 = list(containsColumn = TRUE), 
    inputSE = list(containsColumn = TRUE), inputLabels = list(
      containsColumn = TRUE), inputT = list(containsColumn = TRUE), 
    priorsMu = list(list(name = list(containsColumn = TRUE))), 
    priorsMuNull = list(list(name = list(containsColumn = TRUE))), 
    priorsOmega = list(list(name = list(containsColumn = TRUE)), 
                        list(name = list(containsColumn = TRUE))), priorsOmegaNull = list(
                          list(name = list(containsColumn = TRUE))), priorsTau = list(
                            list(name = list(containsColumn = TRUE))), priorsTauNull = list(
                              list(name = list(containsColumn = TRUE))))
  options$advancedControl <- "clever"
  options$diagnosticsAutocorrelation <- TRUE
  options$diagnosticsMu <- TRUE
  options$diagnosticsOmega <- TRUE
  options$diagnosticsOverview <- TRUE
  options$diagnosticsSamples <- TRUE
  options$diagnosticsSingle <- TRUE
  options$diagnosticsSingleModel <- 12
  options$diagnosticsTau <- TRUE
  options$diagnosticsTrace <- TRUE
  options$fittedPath <- fittedPath
  options$inputCI <- list()
  options$measures <- "fitted"
  options$plotsMu <- TRUE
  options$plotsOmega <- TRUE
  options$plotsTau <- TRUE
  options$plotsTheta <- TRUE
  options$plotsThetaOrder <- "labels"
  options$plotsThetaShow <- "observed"
  options$plotsType <- "conditional"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsOmega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"), list(name = "2", parAlpha = "(1,1,1)", 
                                                              parAlpha1 = "(1,1,1)", parAlpha2 = "(1,1)", parCuts = "(.05, .10)", 
                                                              priorOdds = "1/2", type = "Two-sided"))
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsTau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priorsTauNull <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                       truncationUpper = "Inf", type = "spike"))
  options$resultsIndividual <- TRUE
  options$resultsIndividualSingle <- TRUE
  options$resultsIndividualSingleNumber <- 12
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$resultsTheta <- TRUE
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Diagnostics autocorrelations (mu) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_autocor"]][["collection"]][["diagnostics_model_12_mu_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-0", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics samples (mu) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_samples"]][["collection"]][["diagnostics_model_12_mu_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics traceplot (mu) plot  matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_trace"]][["collection"]][["diagnostics_model_12_mu_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics autocorrelations (omega 1) plot  matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_autocor"]][["collection"]][["diagnostics_model_12_omega_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics autocorrelations (omega 2) plot ", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_autocor"]][["collection"]][["diagnostics_model_12_omega_autocor_autocor_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-4", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics samples (omega 1) plot  matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_samples"]][["collection"]][["diagnostics_model_12_omega_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-5", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics samples (omega 2) plotmatches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_samples"]][["collection"]][["diagnostics_model_12_omega_samples_samples_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-6", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics traceplot (omega 1) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_trace"]][["collection"]][["diagnostics_model_12_omega_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-7", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics traceplot (omega 2) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_trace"]][["collection"]][["diagnostics_model_12_omega_trace_trace_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-8", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics autocorrelation (tau) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_autocor"]][["collection"]][["diagnostics_model_12_tau_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-9", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics samples (tau) matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_samples"]][["collection"]][["diagnostics_model_12_tau_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-10", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics traceplot (tau) matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_trace"]][["collection"]][["diagnostics_model_12_tau_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-11", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_modelsDiagnostics"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("", "", "", 1, "Spike(0)", "Spike(1)", "Spike(0)", 2777, 1.00024441617386,
                                        0.00490177334310956, 2, "Spike(0)", "Two-sided((0.05), (1, 1))",
                                        "Spike(0)", 2251, 1.00001602516168, 0.00438302224679777, 3,
                                        "Spike(0)", "Two-sided((0.1, 0.05), (1, 1, 1))", "Spike(0)",
                                        2023, 1.00034572886243, 0.00335368688410083, 4, "Spike(0)",
                                        "Spike(1)", "InvGamma(1, 0.15)[0, Inf]", 1618, 1.00254130897672,
                                        0.00493833331468064, 5, "Spike(0)", "Two-sided((0.05), (1, 1))",
                                        "InvGamma(1, 0.15)[0, Inf]", 1691, 1.01364766827581, 0.00459321873981085,
                                        6, "Spike(0)", "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                                        7727, 1.00086053910424, 0.00216043057469905, 7, "Normal(0, 1)[-Inf, Inf]",
                                        "Spike(1)", "Spike(0)", 2778, 1.00027740876182, 0.00470297247752125,
                                        8, "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))", "Spike(0)",
                                        2109, 1.00129706919983, 0.00441142139337706, 9, "Normal(0, 1)[-Inf, Inf]",
                                        "Two-sided((0.1, 0.05), (1, 1, 1))", "Spike(0)", 1001, 1.00675173368904,
                                        0.00736389046136744, 10, "Normal(0, 1)[-Inf, Inf]", "Spike(1)",
                                        "InvGamma(1, 0.15)[0, Inf]", 755, 1.00288719068021, 0.00816904230873888,
                                        11, "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))",
                                        "InvGamma(1, 0.15)[0, Inf]", 882, 1.00619665834691, 0.00740936798507717,
                                        12, "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.1, 0.05), (1, 1, 1))",
                                        "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Model Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model_12"]][["collection"]][["individualModels_model_12_tempCoef"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.00740936798507717, 882, -0.298112954433753, 0.144543359786274,
                                        0.145610118691597, 1.00327918764033, "Effect size (<unicode><unicode>)",
                                        0.560274139990378, 0.00465756277124285, 1424, 0.0349154271745609,
                                        0.181391821438591, 0.130837110267033, 1.00619665834691, "Heterogeneity (<unicode><unicode>)",
                                        0.624177234567534))
  })
  
  test_that("Information table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model_12"]][["collection"]][["individualModels_model_12_tempInfo"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.180259295733511, -2.59886113749133, 0.0118745860806326, 0.0625
                                   ))
  })
  
  test_that("Priors table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model_12"]][["collection"]][["individualModels_model_12_tempPriors"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("Normal(0, 1)[-Inf, Inf]", "Two-sided((0.1, 0.05), (1, 1, 1))",
                                        "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Estimated Studies' Effects (theta) table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model_12"]][["collection"]][["individualModels_model_12_tempStudies"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.00775853627019784, 855, -0.270720099051176, 0.170035632027558,
                                        0.164009336133295, 1.00189805889533, "Study 1", 0.62568049934064,
                                        0.00711458848006568, 905, -0.285009431407208, 0.147833903492011,
                                        0.145923533777225, 1.00266486701686, "Study 2", 0.562315238784119,
                                        0.00696160495950572, 888, -0.274416438770882, 0.128132867393647,
                                        0.127580621830772, 1.00369316402907, "Study 3", 0.524550536497032
                                   ))
  })
  
  test_that("Estimated Weights (omega) table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model_12"]][["collection"]][["individualModels_model_12_tempWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("<unicode><unicode><unicode>", 0, 1, 0, 1, 1, 1.00014949817136,
                                        1, 0.05, 0.0042290972515831, 2222, 0.279161804717707, 0.05,
                                        0.725298516375089, 0.761583270187798, 1.00053343931519, 0.99039440873118,
                                        0.1, 0.00406531205440074, 2696, 0.114219694807999, 0.1, 0.485230155917205,
                                        0.477608086828115, 1.00089356701306, 0.888394523431416, 1))
  })
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-0.00991677655357163, 0.0479103036200819, 0, "Effect size (<unicode><unicode>)",
                                        0.383326476049302, 0, 0.0604137113604334, 0, "Heterogeneity (<unicode><unicode>)",
                                        0.363308747466872))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.296099177059036, 0.05, 0.844054390777628,
                                        1, 1, 0.1, 0.135752446058477, 0.1, 0.802984432472739, 1, 1,
                                        1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.282649411599802, "6/12", 0.220363732321261, 0.5, "Effect", 0.571597012412967,
                                        "6/12", 0.36370456796387, 0.5, "Heterogeneity", 0.638872869773129,
                                        "8/12", 0.389824544390419, 0.5, "Publication bias"))
  })
  
  test_that("Model Averaged Estimated Studies' Effects (theta) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_studiesSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-0.149488348181656, 0.0498797243212219, 0, "Study 1", 0.41948827590209,
                                        -0.200369344814511, 0.0409334110180495, 0, "Study 2", 0.440573849075256,
                                        -0.175527939108198, 0.0448298037917323, 0, "Study 3", 0.426362101335361
                                   ))
  })
  
  test_that("Effect size (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-prefitted-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-conditional-prefitted-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-prefitted-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Conditional) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-conditional-prefitted-1", dir="RobustBayesianMetaAnalysis")
  })
}
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(inputCI = list(containsColumn = TRUE), inputSE = list(
    containsColumn = TRUE), inputN = list(containsColumn = TRUE), 
    inputN1 = list(containsColumn = TRUE), inputN2 = list(containsColumn = TRUE), 
    inputSE = list(containsColumn = TRUE), inputLabels = list(
      containsColumn = TRUE), inputT = list(containsColumn = TRUE), 
    priorsMu = list(list(name = list(containsColumn = TRUE))), 
    priorsMuNull = list(list(name = list(containsColumn = TRUE))), 
    priorsOmega = list(list(name = list(containsColumn = TRUE)), 
                        list(name = list(containsColumn = TRUE))), priorsOmegaNull = list(
                          list(name = list(containsColumn = TRUE))), priorsTau = list(
                            list(name = list(containsColumn = TRUE))), priorsTauNull = list(
                              list(name = list(containsColumn = TRUE))))
  options$advancedControl <- "clever"
  options$bayesFactorType <- "BF01"
  options$diagnosticsSingleModel <- 12
  options$diagnosticsTransformed <- FALSE
  options$fittedPath <- fittedPath
  options$inputCI <- list()
  options$measures <- "fitted"
  options$plotsMu <- TRUE
  options$plotsOmega <- TRUE
  options$plotsPriors <- FALSE
  options$plotsTau <- TRUE
  options$plotsTheta <- TRUE
  options$plotsThetaOrder <- "ascending"
  options$plotsThetaShow <- "both"
  options$plotsTypeIndividualBy <- "model"
  options$plotsTypeIndividualOrder <- "ascending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsOmega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"), list(name = "2", parAlpha = "(1,1,1)", 
                                                              parAlpha1 = "(1,1,1)", parAlpha2 = "(1,1)", parCuts = "(.05, .10)", 
                                                              priorOdds = "1/2", type = "Two-sided"))
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsTau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priorsTauNull <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                       truncationUpper = "Inf", type = "spike"))
  options$resultsCI <- 0.8
  options$resultsIndividualSingle <- TRUE
  options$resultsIndividualSingleNumber <- 12
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.0479103036200819, 0, "Effect size (<unicode><unicode>)",
                                        0.19312376220552, 0, 0.0604137113604334, 0, "Heterogeneity (<unicode><unicode>)",
                                        0.243290057391617))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.493442571534098, 0.05, 0.844054390777628,
                                        1, 1, 0.1, 0.323336867699399, 0.1, 0.802984432472739, 1, 1,
                                        1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.53795181932266, "6/12", 0.220363732321261, 0.5, "Effect", 1.74948430149862,
                                        "6/12", 0.36370456796387, 0.5, "Heterogeneity", 1.5652566376079,
                                        "8/12", 0.389824544390419, 0.5, "Publication bias"))
  })
  
  test_that("Effect size (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size vs Heterogeneity (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mutau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-vs-heterogeneity-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) (observed + predicted) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
}
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(inputCI = list(containsColumn = TRUE), inputSE = list(
    containsColumn = TRUE), inputN = list(containsColumn = TRUE), 
    inputN1 = list(containsColumn = TRUE), inputN2 = list(containsColumn = TRUE), 
    inputSE = list(containsColumn = TRUE), inputLabels = list(
      containsColumn = TRUE), inputT = list(containsColumn = TRUE), 
    priorsMu = list(list(name = list(containsColumn = TRUE))), 
    priorsMuNull = list(list(name = list(containsColumn = TRUE))), 
    priorsOmega = list(list(name = list(containsColumn = TRUE)), 
                        list(name = list(containsColumn = TRUE))), priorsOmegaNull = list(
                          list(name = list(containsColumn = TRUE))), priorsTau = list(
                            list(name = list(containsColumn = TRUE))), priorsTauNull = list(
                              list(name = list(containsColumn = TRUE))))
  options$advancedControl <- "clever"
  options$bayesFactorType <- "LogBF10"
  options$diagnosticsSingleModel <- 12
  options$diagnosticsTransformed <- FALSE
  options$fittedPath <- fittedPath
  options$inputCI <- list()
  options$measures <- "fitted"
  options$plotsIndividualMu <- TRUE
  options$plotsIndividualOmega <- TRUE
  options$plotsIndividualTau <- TRUE
  options$plotsPriors <- FALSE
  options$plotsTheta <- TRUE
  options$plotsThetaOrder <- "descending"
  options$plotsThetaShow <- "observed"
  options$plotsTypeIndividualBy <- "prob"
  options$plotsTypeIndividualConditional <- FALSE
  options$plotsTypeIndividualOrder <- "descending"
  options$priorsMu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priorsMuNull <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priorsOmega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"), list(name = "2", parAlpha = "(1,1,1)", 
                                                              parAlpha1 = "(1,1,1)", parAlpha2 = "(1,1)", parCuts = "(.05, .10)", 
                                                              priorOdds = "1/2", type = "Two-sided"))
  options$priorsOmegaNull <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1", 
                                         type = "spike"))
  options$priorsTau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priorsTauNull <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                       truncationUpper = "Inf", type = "spike"))
  options$resultsCI <- 0.8
  options$resultsIndividualSingle <- TRUE
  options$resultsIndividualSingleNumber <- 12
  options$resultsModelsBF <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.0479103036200819, 0, "Effect size (<unicode><unicode>)",
                                        0.19312376220552, 0, 0.0604137113604334, 0, "Heterogeneity (<unicode><unicode>)",
                                        0.243290057391617))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.493442571534098, 0.05, 0.844054390777628,
                                        1, 1, 0.1, 0.323336867699399, 0.1, 0.802984432472739, 1, 1,
                                        1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-1.26354797759022, "6/12", 0.220363732321261, 0.5, "Effect", -0.559321059649379,
                                        "6/12", 0.36370456796387, 0.5, "Heterogeneity", -0.448049796239885,
                                        "8/12", 0.389824544390419, 0.5, "Publication bias"))
  })
  
  test_that("Forest plot (Model Averaged) (observed, descending) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-models-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weights (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_omega"]][["collection"]][["plotsIndividual_omega_plot_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-1-models-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weights (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_omega"]][["collection"]][["plotsIndividual_omega_plot_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-2-models-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-models-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
}