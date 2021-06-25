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
    expect_equal_plots(testPlot, "prior-plot-2-default")
  })
  
  test_that("Priors plot mu (2) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-3-default")
  })
  
  test_that("Priors plot mu (3) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-4-default")
  })
  
  test_that("Priors plot mu (4) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_4"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-5-default")
  })
  
  test_that("Priors plot mu (5) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_5"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-6-default")
  })
  
  test_that("Priors plot mu (6) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_6"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-7-default")
  })
  
  test_that("Priors plot mu (7) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_7"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-8-default")
  })
  
  test_that("Priors plot mu (8) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_8"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-9-default")
  })
  
  test_that("Priors plot omega (1) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_omega"]][["collection"]][["priorPlots_omega_alternative"]][["collection"]][["priorPlots_omega_alternative_omega_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-10-default")
  })
  
  test_that("Priors plot omega (2) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_omega"]][["collection"]][["priorPlots_omega_alternative"]][["collection"]][["priorPlots_omega_alternative_omega_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-11-default")
  })
  
  test_that("Priors plot tau (1) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_tau"]][["collection"]][["priorPlots_tau_alternative"]][["collection"]][["priorPlots_tau_alternative_tau_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-12-default")
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
    expect_equal_plots(testPlot, "prior-plot-2-correlations")
  })
  
  test_that("Priors plot mu (2) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-3-correlations")
  })
  
  test_that("Priors plot mu (3) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_null"]][["collection"]][["priorPlots_mu_null_mu_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-4-correlations")
  })
  
  test_that("Priors plot omega (1) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_omega"]][["collection"]][["priorPlots_omega_null"]][["collection"]][["priorPlots_omega_null_omega_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-5-correlations")
  })
  
  test_that("Priors plot tau (1) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_tau"]][["collection"]][["priorPlots_tau_alternative"]][["collection"]][["priorPlots_tau_alternative_tau_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-6-correlations")
  })
  
  test_that("Priors plot tau (2) (correlation) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_tau"]][["collection"]][["priorPlots_tau_null"]][["collection"]][["priorPlots_tau_null_tau_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-7-correlations")
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
    expect_equal_plots(testPlot, "prior-plot-2-OR")
  })
  
  test_that("Priors plot mu (2) (OR) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_alternative"]][["collection"]][["priorPlots_mu_alternative_mu_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-3-OR")
  })
  
  test_that("Priors plot mu (3) (OR) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_mu"]][["collection"]][["priorPlots_mu_null"]][["collection"]][["priorPlots_mu_null_mu_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-4-OR")
  })
  
  test_that("Priors plot omega (1) (OR) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_omega"]][["collection"]][["priorPlots_omega_null"]][["collection"]][["priorPlots_omega_null_omega_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-5-OR")
  })
  
  test_that("Priors plot tau (1) (OR) matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_tau"]][["collection"]][["priorPlots_tau_alternative"]][["collection"]][["priorPlots_tau_alternative_tau_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-6-OR")
  })
}

### fit a default model using d + se, (wihout the more complex weight function) and main output
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
                                   list(0, 0.169012354883644, 0.191058770286674, "Effect size (<unicode><unicode>)",
                                        0.331442606663106, 0, 0.0461545030507955, 0, "Heterogeneity (<unicode><unicode>)",
                                        0.294538609799362))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.00672231535859033, 0.05, 0.570425818572795,
                                        0.601495317746776, 1, 1))
  })
  
  test_that("Conditional Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.0648220927431506, 0.20775978862952, 0.209795557836815, "Effect size (<unicode><unicode>)",
                                        0.344513330970872, 0.0334524312101734, 0.141513655239605, 0.113264874368608,
                                        "Heterogeneity (<unicode><unicode>)", 0.42492060333899))
  })
  
  test_that("Conditional Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.0038873186384601, 0.05, 0.293330950924731,
                                        0.193230779638258, 0.930346780885645, 1))
  })
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.00205494154325941, -5.25591676931767, 1, 0.00041081946665412,
                                        "Spike(0)", "Spike(1)", 0.166666666666667, "Spike(0)", 0.50429600154999,
                                        0.107274695486287, 2, 0.0438354508161165, "Spike(0)", "Two-sided((0.05), (1, 1))",
                                        0.0833333333333333, "Spike(0)", 0.17625848690762, -0.83844625671059,
                                        3, 0.0340513301940838, "Spike(0)", "Spike(1)", 0.166666666666667,
                                        "InvGamma(1, 0.15)[0, Inf]", 1.3458398692244, 1.01828620772,
                                        4, 0.109011609050535, "Spike(0)", "Two-sided((0.05), (1, 1))",
                                        0.0833333333333333, "InvGamma(1, 0.15)[0, Inf]", 1.95631447639274,
                                        1.27285245363283, 5, 0.28122858491113, "Normal(0, 1)[-Inf, Inf]",
                                        "Spike(1)", 0.166666666666667, "Spike(0)", 6.05831088084406,
                                        2.19938060712744, 6, 0.355153034972962, "Normal(0, 1)[-Inf, Inf]",
                                        "Two-sided((0.05), (1, 1))", 0.0833333333333333, "Spike(0)",
                                        0.408181587275783, -0.0425161279106636, 7, 0.0754748302527677,
                                        "Normal(0, 1)[-Inf, Inf]", "Spike(1)", 0.166666666666667, "InvGamma(1, 0.15)[0, Inf]",
                                        1.23356328366391, 0.940310801665095, 8, 0.100834340335751, "Normal(0, 1)[-Inf, Inf]",
                                        "Two-sided((0.05), (1, 1))", 0.0833333333333333, "InvGamma(1, 0.15)[0, Inf]"
                                   ))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(4.33876579012402, "4/8", 0.81269079047261, 0.5, "Effect", 0.469231594013641,
                                        "4/8", 0.319372109833138, 0.5, "Heterogeneity", 3.11292450013238,
                                        "4/8", 0.608834435175364, 0.333333333333333, "Publication bias"
                                   ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-default-model")
  })
  
  test_that("Weight function (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-default-model")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-default-model")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-default-model")
  })
  
  test_that("Effect size (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-models-default-model")
  })
  
  test_that("Weights (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weights-conditional-models-default-model")
  })
  
  test_that("Heterogeneity (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-models-default-model")
  })
  
}

### fit models with a truncated priors and t + se
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
                                   list(0, 0.532967505483498, 0.559351413613886, "Effect size (<unicode><unicode>)",
                                        0.831234276058069, 0, 0.124766070829099, 0, "Heterogeneity (<unicode><unicode>)",
                                        0.461003184076632))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(8.53525341784542, "2/4", 0.89512601750799, 0.5, "Effect", 0.569453434786407,
                                        "2/4", 0.362835508314336, 0.5, "Heterogeneity", "", "0/4", 0,
                                        0, "Publication bias"))
  })
  
  test_that("Effect size (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-truncated-priors")
  })
  
  test_that("Heterogeneity (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-truncated-priors")
  })

  test_that("Forest plot (Conditional) matches", {
    skip("The individual study estimates are no longer estimated under the RoBMA 1.2.0 parametrization.")
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-conditional-truncated-priors")
  })
}

### fit models with only an effect size, d + (N1 + N2) and names
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
    expect_equal_plots(testPlot, "effect-size-model-averaged-d-and-n")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-d-and-n")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-d-and-n")
  })
}

### fit models with only one publication bias function, y + (lCI & uCI)
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
    expect_equal_plots(testPlot, "effect-size-model-averaged-y-CI")
  })
  
  test_that("Weight function (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-y-CI")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-y-CI")
  })

  test_that("Forest plot (Model Averaged) matches", {
    skip("The individual study estimates are no longer estimated under the RoBMA 1.2.0 parametrization.")
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-y-CI")
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
                                   list(1, 1.01405377373308, 1, "Effect size (OR)", 1.18734014384608,
                                        0.0357214160840386, 0.0974424482453894, 0.0813968379531897,
                                        "Heterogeneity (<unicode><unicode>)", 0.254675685828546))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.188907801338866, 0.05, 0.875636684486605,
                                        1, 1, 1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.133392931748063, "2/4", 0.117693456533497, 0.5, "Effect", "",
                                        "4/4", 1, 1, "Heterogeneity", 0.822709075341436, "2/4", 0.291460810654715,
                                        0.333333333333333, "Publication bias"))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-OR")
  })
  
  test_that("Effect size vs Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mutau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-vs-heterogeneity-model-averaged-OR")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-OR")
  })
  
  test_that("Effect size (Models) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-models-OR")
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
                                   list(0, 0.113504372519749, 0.0897456825723781, "Effect size (<unicode><unicode>)",
                                        0.373002888896244, 0.0342506647210613, 0.144534497342568, 0.114406312065542,
                                        "Heterogeneity (<unicode><unicode>)", 0.434639934296237))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.05, 0.00671362430311733, 0.05, 0.527043237975422,
                                        0.45447760025244, 1, 1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1.24604189695875, "2/4", 0.554772330224985, 0.5, "Effect", "",
                                        "4/4", 1, 1, "Heterogeneity", 3.80976336030243, "2/4", 0.655751899695982,
                                        0.333333333333333, "Publication bias"))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-negative-ES")
  })
}

### more options tested using a preloaded model
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  options$resultsTheta <- FALSE
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  results <- jaspTools::runAnalysis("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Diagnostics autocorrelations (mu) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_autocor"]][["collection"]][["diagnostics_model_12_mu_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-0")
  })
  
  test_that("Diagnostics samples (mu) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_samples"]][["collection"]][["diagnostics_model_12_mu_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-1")
  })
  
  test_that("Diagnostics traceplot (mu) plot  matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_trace"]][["collection"]][["diagnostics_model_12_mu_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-2")
  })
  
  test_that("Diagnostics autocorrelations (omega 1) plot  matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_autocor"]][["collection"]][["diagnostics_model_12_omega_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-3")
  })
  
  test_that("Diagnostics autocorrelations (omega 2) plot ", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_autocor"]][["collection"]][["diagnostics_model_12_omega_autocor_autocor_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-4")
  })
  
  test_that("Diagnostics samples (omega 1) plot  matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_samples"]][["collection"]][["diagnostics_model_12_omega_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-5")
  })
  
  test_that("Diagnostics samples (omega 2) plotmatches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_samples"]][["collection"]][["diagnostics_model_12_omega_samples_samples_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-6")
  })
  
  test_that("Diagnostics traceplot (omega 1) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_trace"]][["collection"]][["diagnostics_model_12_omega_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-7")
  })
  
  test_that("Diagnostics traceplot (omega 2) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_trace"]][["collection"]][["diagnostics_model_12_omega_trace_trace_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-8")
  })
  
  test_that("Diagnostics autocorrelation (tau) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_autocor"]][["collection"]][["diagnostics_model_12_tau_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-9")
  })
  
  test_that("Diagnostics samples (tau) matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_samples"]][["collection"]][["diagnostics_model_12_tau_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-10")
  })
  
  test_that("Diagnostics traceplot (tau) matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_trace"]][["collection"]][["diagnostics_model_12_tau_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-11")
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
    skip("The individual study estimates are no longer estimated under the RoBMA 1.2.0 parametrization.")
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
    skip("The individual study estimates are no longer estimated under the RoBMA 1.2.0 parametrization.")
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
    expect_equal_plots(testPlot, "effect-size-conditional-prefitted-1")
  })
  
  test_that("Weight function (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-conditional-prefitted-1")
  })
  
  test_that("Heterogeneity (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-prefitted-1")
  })
  
  test_that("Forest plot (Conditional) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-conditional-prefitted-1")
  })
}
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  options$plotsTheta <- FALSE # used to be TRUE
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
    expect_equal_plots(testPlot, "effect-size-model-averaged-prefitted-2")
  })
  
  test_that("Effect size vs Heterogeneity (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mutau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-vs-heterogeneity-model-averaged-prefitted-2")
  })
  
  test_that("Weight function (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-prefitted-2")
  })
  
  test_that("Heterogeneity (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-prefitted-2")
  })

  test_that("Forest plot (Model Averaged) (observed + predicted) matches", {
    skip("The individual study estimates are no longer estimated under the RoBMA 1.2.0 parametrization.")
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-prefitted-2")
  })
}
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
    expect_equal_plots(testPlot, "forest-plot-model-averaged-prefitted-3")
  })
  
  test_that("Effect size (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-models-prefitted-3")
  })
  
  test_that("Weights (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_omega"]][["collection"]][["plotsIndividual_omega_plot_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-1-models-prefitted-3")
  })
  
  test_that("Weights (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_omega"]][["collection"]][["plotsIndividual_omega_plot_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-2-models-prefitted-3")
  })
  
  test_that("Heterogeneity (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plotsIndividual"]][["collection"]][["plotsIndividual_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-models-prefitted-3")
  })
  
}