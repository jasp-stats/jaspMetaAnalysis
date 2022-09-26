context("Robust Bayesian Meta-Analysis")


### create a pre-fitted model (in case that the package needs to be updated)
if (FALSE){
  # fit a default model
  fit <- RoBMA::RoBMA(d = c(.3, .2, .1), n = c(30, 35, 40), parallel = TRUE)
  # remove majority of the samples to save space
  for(i in 2:length(fit$models)){
    for(j in seq_along(fit$models[[i]]$fit$mcmc)){
      fit$models[[i]]$fit$mcmc[[j]] <- fit$models[[i]]$fit$mcmc[[j]][1:100,]
    }
  }
  set.seed(1)
  for(i in 1:2){
    for(p in c("mu", "tau", "PET", "PEESE")){
      fit$RoBMA$posteriors[[p]] <- sample(fit$RoBMA$posteriors[[p]], 100)
    }
    for(p in c("omega")){
      fit$RoBMA$posteriors[[p]] <- fit$RoBMA$posteriors[[p]][sample(nrow(fit$RoBMA$posteriors[[p]]), 100),]
    }
  }
  saveRDS(fit, file = "tests/robmaFit.RDS")
}

# path to the pre-fitted RoBMA model
fittedPath <- file.path("robmaFit.RDS")

### RoBMA-PP/RoBMA-old model settings
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$autofitMcmcError <- FALSE
  options$autofitMcmcErrorSd <- FALSE
  options$autofitTime <- FALSE
  options$effect <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$effectNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                  parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                  parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                  truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$fittedPath <- ""
  options$heterogeneity <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                     parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                     parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                     truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$heterogeneityNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                         parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                         parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                         truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$inputCI <- list()
  options$measures <- "cohensD"
  options$modelType <- "PP"
  options$omega <- list(list(name = "#", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#2", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#3", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#4", parAlpha = "(1,1,1)", parCuts = "(.025, .05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#5", parAlpha = "(1,1,1)", parCuts = "(.05, .50)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#6", parAlpha = "(1,1,1,1)", parCuts = "(.025, .05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"))
  options$omegaNull <- list(list(name = "#", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                                 parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$peese <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                             parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                             parScale = "1", parScale2 = "5", parShape = "1", priorWeight = "1",
                             truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$peeseNull <- list()
  options$pet <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                           parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                           parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                           truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$petNull <- list()
  options$plotForestOrder <- "alphabetical"
  options$plotModelsOrder <- "decreasing"
  options$plotModelsOrderBy <- "model"
  options$resultsModelsBf <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Overview table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, "", "Spike(0)", "Spike(0)", 0.125, 2, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Spike(0)", "Spike(0)", 0.0625, 3, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Spike(0)", "Spike(0)", 0.0625, 4, "", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.125, 5, "PET ~ Cauchy(0, 1)[0, Inf]", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.0625, 6, "PEESE ~ Cauchy(0, 5)[0, Inf]", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.0625, 7, "", "Normal(0, 1)", "Spike(0)", 0.125, 8, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Normal(0, 1)", "Spike(0)", 0.0625, 9, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Normal(0, 1)", "Spike(0)", 0.0625, 10, "", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.125, 11, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0625, 12, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0625))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("6/12", 0.5, "Effect", "6/12", 0.5, "Heterogeneity", "8/12", 0.5,
                                        "Publication bias"))
  })
}
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$autofitMcmcError <- FALSE
  options$autofitMcmcErrorSd <- FALSE
  options$autofitTime <- FALSE
  options$effect <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$effectNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                  parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                  parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                  truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$fittedPath <- ""
  options$heterogeneity <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                     parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                     parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                     truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$heterogeneityNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                         parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                         parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                         truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$inputCI <- list()
  options$measures <- "cohensD"
  options$modelType <- "2w"
  options$omega <- list(list(name = "#", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#2", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#3", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#4", parAlpha = "(1,1,1)", parCuts = "(.025, .05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#5", parAlpha = "(1,1,1)", parCuts = "(.05, .50)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#6", parAlpha = "(1,1,1,1)", parCuts = "(.025, .05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"))
  options$omegaNull <- list(list(name = "#", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                                 parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$peese <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                             parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                             parScale = "1", parScale2 = "5", parShape = "1", priorWeight = "1",
                             truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$peeseNull <- list()
  options$pet <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                           parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                           parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                           truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$petNull <- list()
  options$plotForestOrder <- "alphabetical"
  options$plotModelsOrder <- "decreasing"
  options$plotModelsOrderBy <- "model"
  options$resultsModelsBf <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Overview table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, "", "Spike(0)", "Spike(0)", 0.125, 2, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0625, 3, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0625, 4, "", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.125, 5, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)",
                                        "InvGamma(1, 0.15)", 0.0625, 6, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0625, 7, "", "Normal(0, 1)",
                                        "Spike(0)", 0.125, 8, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0625, 9, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0625, 10, "", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.125, 11, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0625, 12, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0625))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("6/12", 0.5, "Effect", "6/12", 0.5, "Heterogeneity", "8/12", 0.5,
                                        "Publication bias"))
  })
}

### custom model settings (testing out the distributions)
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$autofitMcmcError <- FALSE
  options$autofitMcmcErrorSd <- FALSE
  options$autofitTime <- FALSE
  options$effect <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"),
                         list(name = "#2", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "t"),
                         list(name = "#3", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "cauchy"),
                         list(name = "#4", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "gammaAB"),
                         list(name = "#5", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "gammaK0"),
                         list(name = "#6", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "invgamma"),
                         list(name = "#7", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "lognormal"),
                         list(name = "#8", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "beta"),
                         list(name = "#9", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "uniform"),
                         list(name = "#10", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"),
                         list(name = "#11", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "none"))
  options$effectNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                  parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                  parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                  truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$fittedPath <- ""
  options$heterogeneity <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                     parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                     parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                     truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$heterogeneityNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                         parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                         parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                         truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$inputCI <- list()
  options$measures <- "cohensD"
  options$modelType <- "custom"
  options$omega <- list(list(name = "#", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#2", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#3", parAlpha = "(1,1)", parCuts = "(.05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided-fixed"),
                        list(name = "#4", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided-fixed"),
                        list(name = "#5", parAlpha = "(1,1,1)", parCuts = "(.05, .50)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$omegaNull <- list(list(name = "#", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                                 parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$peese <- list()
  options$peeseNull <- list()
  options$pet <- list()
  options$petNull <- list()
  options$plotForestOrder <- "alphabetical"
  options$plotModelsOrder <- "decreasing"
  options$plotModelsOrderBy <- "model"
  options$resultsModelsBf <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Overview table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, "", "Spike(0)", "Spike(0)", 0.00694444444444444, 2, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 3, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 4, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 5, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 6, "", "Spike(0)",
                                        "Spike(0)", 0.00694444444444444, 7, "", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.00694444444444444, 8, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 9, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 10, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 11, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 12, "",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 13, "",
                                        "Normal(0, 1)", "Spike(0)", 0.00694444444444444, 14, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.00694444444444444, 15, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.00694444444444444, 16, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.00694444444444444, 17, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.00694444444444444, 18, "", "Normal(0, 1)",
                                        "Spike(0)", 0.00694444444444444, 19, "", "Normal(0, 1)", "InvGamma(1, 0.15)",
                                        0.00694444444444444, 20, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444, 21,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 22, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444, 23,
                                        "omega[one-sided: .1, .05] = (0.1, 0.5, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 24, "", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 25, "", "Student-t(0, 1, 2)",
                                        "Spike(0)", 0.00694444444444444, 26, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Student-t(0, 1, 2)", "Spike(0)", 0.00694444444444444, 27, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Student-t(0, 1, 2)", "Spike(0)", 0.00694444444444444, 28, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Student-t(0, 1, 2)", "Spike(0)", 0.00694444444444444, 29, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Student-t(0, 1, 2)", "Spike(0)", 0.00694444444444444, 30, "",
                                        "Student-t(0, 1, 2)", "Spike(0)", 0.00694444444444444, 31, "",
                                        "Student-t(0, 1, 2)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        32, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Student-t(0, 1, 2)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 33, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Student-t(0, 1, 2)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        34, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)", "Student-t(0, 1, 2)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 35, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Student-t(0, 1, 2)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        36, "", "Student-t(0, 1, 2)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        37, "", "Cauchy(0, 1)", "Spike(0)", 0.00694444444444444, 38,
                                        "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Cauchy(0, 1)",
                                        "Spike(0)", 0.00694444444444444, 39, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Cauchy(0, 1)", "Spike(0)", 0.00694444444444444, 40, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Cauchy(0, 1)", "Spike(0)", 0.00694444444444444, 41, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Cauchy(0, 1)", "Spike(0)", 0.00694444444444444, 42, "", "Cauchy(0, 1)",
                                        "Spike(0)", 0.00694444444444444, 43, "", "Cauchy(0, 1)", "InvGamma(1, 0.15)",
                                        0.00694444444444444, 44, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Cauchy(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444, 45,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Cauchy(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 46, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Cauchy(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444, 47,
                                        "omega[one-sided: .1, .05] = (0.1, 0.5, 1)", "Cauchy(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 48, "", "Cauchy(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 49, "", "Gamma(1, 0.15)",
                                        "Spike(0)", 0.00694444444444444, 50, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Gamma(1, 0.15)", "Spike(0)", 0.00694444444444444, 51, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Gamma(1, 0.15)", "Spike(0)", 0.00694444444444444, 52, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Gamma(1, 0.15)", "Spike(0)", 0.00694444444444444, 53, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Gamma(1, 0.15)", "Spike(0)", 0.00694444444444444, 54, "", "Gamma(1, 0.15)",
                                        "Spike(0)", 0.00694444444444444, 55, "", "Gamma(1, 0.15)", "InvGamma(1, 0.15)",
                                        0.00694444444444444, 56, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Gamma(1, 0.15)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        57, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Gamma(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 58, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Gamma(1, 0.15)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        59, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)", "Gamma(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 60, "", "Gamma(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 61, "", "Gamma(1, 1)",
                                        "Spike(0)", 0.00694444444444444, 62, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Gamma(1, 1)", "Spike(0)", 0.00694444444444444, 63, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Gamma(1, 1)", "Spike(0)", 0.00694444444444444, 64, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Gamma(1, 1)", "Spike(0)", 0.00694444444444444, 65, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Gamma(1, 1)", "Spike(0)", 0.00694444444444444, 66, "", "Gamma(1, 1)",
                                        "Spike(0)", 0.00694444444444444, 67, "", "Gamma(1, 1)", "InvGamma(1, 0.15)",
                                        0.00694444444444444, 68, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Gamma(1, 1)", "InvGamma(1, 0.15)", 0.00694444444444444, 69,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Gamma(1, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 70, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Gamma(1, 1)", "InvGamma(1, 0.15)", 0.00694444444444444, 71,
                                        "omega[one-sided: .1, .05] = (0.1, 0.5, 1)", "Gamma(1, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 72, "", "Gamma(1, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 73, "", "InvGamma(1, 0.15)",
                                        "Spike(0)", 0.00694444444444444, 74, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "InvGamma(1, 0.15)", "Spike(0)", 0.00694444444444444, 75, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "InvGamma(1, 0.15)", "Spike(0)", 0.00694444444444444, 76, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "InvGamma(1, 0.15)", "Spike(0)", 0.00694444444444444, 77, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "InvGamma(1, 0.15)", "Spike(0)", 0.00694444444444444, 78, "",
                                        "InvGamma(1, 0.15)", "Spike(0)", 0.00694444444444444, 79, "",
                                        "InvGamma(1, 0.15)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        80, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "InvGamma(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 81, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "InvGamma(1, 0.15)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        82, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)", "InvGamma(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 83, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "InvGamma(1, 0.15)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        84, "", "InvGamma(1, 0.15)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        85, "", "Lognormal(0, 1)", "Spike(0)", 0.00694444444444444,
                                        86, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Lognormal(0, 1)",
                                        "Spike(0)", 0.00694444444444444, 87, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Lognormal(0, 1)", "Spike(0)", 0.00694444444444444, 88, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Lognormal(0, 1)", "Spike(0)", 0.00694444444444444, 89, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Lognormal(0, 1)", "Spike(0)", 0.00694444444444444, 90, "",
                                        "Lognormal(0, 1)", "Spike(0)", 0.00694444444444444, 91, "",
                                        "Lognormal(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        92, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Lognormal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 93, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Lognormal(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        94, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)", "Lognormal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 95, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Lognormal(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        96, "", "Lognormal(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444,
                                        97, "", "Beta(1, 0.15)", "Spike(0)", 0.00694444444444444, 98,
                                        "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Beta(1, 0.15)",
                                        "Spike(0)", 0.00694444444444444, 99, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Beta(1, 0.15)", "Spike(0)", 0.00694444444444444, 100, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Beta(1, 0.15)", "Spike(0)", 0.00694444444444444, 101, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Beta(1, 0.15)", "Spike(0)", 0.00694444444444444, 102, "", "Beta(1, 0.15)",
                                        "Spike(0)", 0.00694444444444444, 103, "", "Beta(1, 0.15)", "InvGamma(1, 0.15)",
                                        0.00694444444444444, 104, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Beta(1, 0.15)", "InvGamma(1, 0.15)", 0.00694444444444444, 105,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Beta(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 106, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Beta(1, 0.15)", "InvGamma(1, 0.15)", 0.00694444444444444, 107,
                                        "omega[one-sided: .1, .05] = (0.1, 0.5, 1)", "Beta(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 108, "", "Beta(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 109, "", "Uniform(0, 1)",
                                        "Spike(0)", 0.00694444444444444, 110, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Uniform(0, 1)", "Spike(0)", 0.00694444444444444, 111, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Uniform(0, 1)", "Spike(0)", 0.00694444444444444, 112, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Uniform(0, 1)", "Spike(0)", 0.00694444444444444, 113, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Uniform(0, 1)", "Spike(0)", 0.00694444444444444, 114, "", "Uniform(0, 1)",
                                        "Spike(0)", 0.00694444444444444, 115, "", "Uniform(0, 1)", "InvGamma(1, 0.15)",
                                        0.00694444444444444, 116, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Uniform(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444, 117,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Uniform(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 118, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Uniform(0, 1)", "InvGamma(1, 0.15)", 0.00694444444444444, 119,
                                        "omega[one-sided: .1, .05] = (0.1, 0.5, 1)", "Uniform(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 120, "", "Uniform(0, 1)",
                                        "InvGamma(1, 0.15)", 0.00694444444444444, 121, "", "Spike(0)",
                                        "Spike(0)", 0.00694444444444444, 122, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 123, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 124, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 125, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 126, "", "Spike(0)",
                                        "Spike(0)", 0.00694444444444444, 127, "", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.00694444444444444, 128, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 129, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 130, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 131, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 132, "",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 133, "",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 134, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 135, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 136, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 137, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "Spike(0)", 0.00694444444444444, 138, "", "Spike(0)",
                                        "Spike(0)", 0.00694444444444444, 139, "", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.00694444444444444, 140, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 141, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 142, "omega[two-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 143, "omega[one-sided: .1, .05] = (0.1, 0.5, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444, 144, "",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.00694444444444444))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("132/144", 0.916666666666667, "Effect", "72/144", 0.5, "Heterogeneity",
                                        "96/144", 0.666666666666667, "Publication bias"))
  })
}

### prior distributions plots (via RoBMA-PSMA)
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$autofitMcmcError <- FALSE
  options$autofitMcmcErrorSd <- FALSE
  options$autofitTime <- FALSE
  options$effect <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$effectNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                  parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                  parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                  truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$fittedPath <- ""
  options$heterogeneity <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                     parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                     parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                     truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$heterogeneityNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                         parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                         parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                         truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$inputCI <- list()
  options$measures <- "cohensD"
  options$omega <- list(list(name = "#", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#2", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#3", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#4", parAlpha = "(1,1,1)", parCuts = "(.025, .05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#5", parAlpha = "(1,1,1)", parCuts = "(.05, .50)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#6", parAlpha = "(1,1,1,1)", parCuts = "(.025, .05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"))
  options$omegaNull <- list(list(name = "#", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                                 parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$peese <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                             parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                             parScale = "1", parScale2 = "5", parShape = "1", priorWeight = "1",
                             truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$peeseNull <- list()
  options$pet <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                           parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                           parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                           truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$petNull <- list()
  options$plotForestOrder <- "alphabetical"
  options$plotModelsOrder <- "decreasing"
  options$plotModelsOrderBy <- "model"
  options$plotPriors <- TRUE
  options$resultsModelsBf <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Overview table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, "", "Spike(0)", "Spike(0)", 0.125, 2, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 3, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 4, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 5, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 6, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 7, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 8, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Spike(0)", "Spike(0)", 0.03125, 9, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Spike(0)", "Spike(0)", 0.03125, 10, "", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.125, 11, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)",
                                        "InvGamma(1, 0.15)", 0.0104166666666667, 12, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 13, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 14, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 15, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 16, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 17, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.03125, 18, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.03125, 19, "", "Normal(0, 1)",
                                        "Spike(0)", 0.125, 20, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 21, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 22, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 23, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 24, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 25, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 26, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Normal(0, 1)", "Spike(0)", 0.03125, 27, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Normal(0, 1)", "Spike(0)", 0.03125, 28, "", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.125, 29, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0104166666666667, 30,
                                        "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0104166666666667, 31, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0104166666666667, 32,
                                        "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0104166666666667, 33, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0104166666666667, 34,
                                        "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0104166666666667, 35,
                                        "PET ~ Cauchy(0, 1)[0, Inf]", "Normal(0, 1)", "InvGamma(1, 0.15)",
                                        0.03125, 36, "PEESE ~ Cauchy(0, 5)[0, Inf]", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.03125))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("18/36", 0.5, "Effect", "18/36", 0.5, "Heterogeneity", "32/36",
                                        0.5, "Publication bias"))
  })

  test_that("titleless-plot-2 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-2")
  })

  test_that("titleless-plot-3 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-3")
  })

  test_that("titleless-plot-4 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative4"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-4")
  })

  test_that("titleless-plot-5 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative5"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-5")
  })

  test_that("titleless-plot-6 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative6"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-6")
  })

  test_that("titleless-plot-7 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative7"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-7")
  })

  test_that("titleless-plot-8 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative8"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-8")
  })

  test_that("titleless-plot-9 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative9"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-9")
  })

  test_that("titleless-plot-10 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-10")
  })

  test_that("titleless-plot-11 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-11")
  })

  test_that("titleless-plot-12 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_heterogeneity"]][["collection"]][["priorPlots_heterogeneity_alternative"]][["collection"]][["priorPlots_heterogeneity_alternative_heterogeneityalternative1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-12")
  })

  test_that("titleless-plot-13 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_heterogeneity"]][["collection"]][["priorPlots_heterogeneity_alternative"]][["collection"]][["priorPlots_heterogeneity_alternative_heterogeneityalternative2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test1-titleless-plot-13")
  })
}

### fit a default model using d + se, with minimum samples, no autofit, & and the complete output
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedAdapt <- 100
  options$advancedBurnin <- 100
  options$advancedChains <- 2
  options$advancedIteration <- 100
  options$autofit <- FALSE
  options$autofitMcmcError <- FALSE
  options$autofitMcmcErrorSd <- FALSE
  options$autofitTime <- FALSE
  options$diagnosticsAutocorrelation <- TRUE
  options$diagnosticsMu <- TRUE
  options$diagnosticsOmega <- TRUE
  options$diagnosticsOverview <- TRUE
  options$diagnosticsPeese <- TRUE
  options$diagnosticsPet <- TRUE
  options$diagnosticsSamples <- TRUE
  options$diagnosticsSingle <- TRUE
  options$diagnosticsSingleModel <- 36
  options$diagnosticsTau <- TRUE
  options$diagnosticsTrace <- TRUE
  options$effect <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$effectNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                  parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                  parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                  truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$fittedPath <- ""
  options$heterogeneity <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                     parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                     parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                     truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$heterogeneityNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                         parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                         parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                         truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$inputCI <- list()
  options$inputES <- "d"
  options$inputSE <- "se"
  options$measures <- "cohensD"
  options$omega <- list(list(name = "#", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#2", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#3", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#4", parAlpha = "(1,1,1)", parCuts = "(.025, .05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#5", parAlpha = "(1,1,1)", parCuts = "(.05, .50)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#6", parAlpha = "(1,1,1,1)", parCuts = "(.025, .05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"))
  options$omegaNull <- list(list(name = "#", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                                 parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$peese <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                             parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                             parScale = "1", parScale2 = "5", parShape = "1", priorWeight = "1",
                             truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$peeseNull <- list()
  options$pet <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                           parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                           parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                           truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$petNull <- list()
  options$plotEstimatesMu <- TRUE
  options$plotEstimatesPetPeese <- TRUE
  options$plotEstimatesTau <- TRUE
  options$plotEstimatesWeightFunction <- TRUE
  options$plotForest <- TRUE
  options$plotForestOrder <- "alphabetical"
  options$plotModelsMu <- TRUE
  options$plotModelsOrder <- "decreasing"
  options$plotModelsOrderBy <- "model"
  options$plotModelsTau <- TRUE
  options$resultsConditional <- TRUE
  options$resultsIndividual <- TRUE
  options$resultsIndividualSingle <- TRUE
  options$resultsIndividualSingleNumber <- 36
  options$resultsModels <- TRUE
  options$resultsModelsBf <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = c("study one", "study two", "study three"
  ), t = c(2.51, 2.39, 2.55), N = c(100L, 150L, 97L), d = c(0.25,
                                                            0.2, 0.26), se = c(0.1, 0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L,
                                                                                                                              75L, 48L), lCI = c(0.05, 0.04, 0.06), uCI = c(0.45, 0.41, 0.38
                                                                                                                              )), class = "data.frame", row.names = c(NA, -3L))
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("", "", "", 1, "", "Spike(0)", "Spike(0)", "", 99, 0.00577198262881337,
                                        0.101, 2, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)",
                                        "Spike(0)", 1.04412693639134, 58, 0.0360956400065861, 0.132,
                                        3, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Spike(0)",
                                        "Spike(0)", 1.15758331317252, 99, 0.00577198262881337, 0.101,
                                        4, "omega[one-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)",
                                        "Spike(0)", 1.04412693639134, 75, 0.0282427869919743, 0.115,
                                        5, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)", "Spike(0)",
                                        "Spike(0)", 1.00863731152447, 61, 0.0225073942987947, 0.128,
                                        6, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)", "Spike(0)",
                                        "Spike(0)", 1.03168705870368, 52, 0.0281218814860331, 0.138,
                                        7, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 1.11763599143712, 107, 0.054957592458893,
                                        0.097, 8, "PET ~ Cauchy(0, 1)[0, Inf]", "Spike(0)", "Spike(0)",
                                        1.08682337576406, 83, 0.746161864355901, 0.109, 9, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Spike(0)", "Spike(0)", 1.07113335553397, 155, 0.0112547000844534,
                                        0.08, 10, "", "Spike(0)", "InvGamma(1, 0.15)", 1.01476266646023,
                                        75, 0.0276913846523718, 0.115, 11, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 1.20715783939968, 27, 0.0519636682009706,
                                        0.191, 12, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 1.15777371717931, 48, 0.0256429135486137,
                                        0.144, 13, "omega[one-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)",
                                        "InvGamma(1, 0.15)", 1.04560998980045, 22, 0.0408594776989356,
                                        0.211, 14, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 1.08693434028936, 66, 0.0293523974721818,
                                        0.123, 15, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 1.15817899534772, 44, 0.0420126975517002,
                                        0.151, 16, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 1.14135179881527, 90, 0.100747538780343,
                                        0.105, 17, "PET ~ Cauchy(0, 1)[0, Inf]", "Spike(0)", "InvGamma(1, 0.15)",
                                        1.03954023940562, 51, 0.891820225471813, 0.14, 18, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Spike(0)", "InvGamma(1, 0.15)", 1.01563682058572, 279, 0.00302864763479579,
                                        0.06, 19, "", "Normal(0, 1)", "Spike(0)", 1.01526650099895,
                                        56, 0.0375157991578566, 0.133, 20, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 1.01276231335407, 48, 0.0383036017554581,
                                        0.145, 21, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 1.02731396738185, 71, 0.0327310229778459,
                                        0.119, 22, "omega[one-sided: .05] ~ CumDirichlet(1, 1)", "Normal(0, 1)",
                                        "Spike(0)", 1.01905059521623, 47, 0.0399287698769616, 0.146,
                                        23, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)", "Normal(0, 1)",
                                        "Spike(0)", 1.04660628335263, 110, 0.0215046140708095, 0.095,
                                        24, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)", "Normal(0, 1)",
                                        "Spike(0)", 1.09617789532525, 48, 0.0282472239808333, 0.145,
                                        25, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 1.11499418394376, 58, 0.0802564595668697,
                                        0.132, 26, "PET ~ Cauchy(0, 1)[0, Inf]", "Normal(0, 1)", "Spike(0)",
                                        1.06005521722824, 45, 0.683114189167585, 0.149, 27, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Normal(0, 1)", "Spike(0)", 0.998210580499419, 79, 0.0102285741932111,
                                        0.113, 28, "", "Normal(0, 1)", "InvGamma(1, 0.15)", 1.08685169398323,
                                        66, 0.0335576746627229, 0.123, 29, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 1.01249923028229, 48, 0.0338105221128544,
                                        0.145, 30, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 1.16846698244661, 52, 0.0382808849262555,
                                        0.139, 31, "omega[one-sided: .05] ~ CumDirichlet(1, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 1.07916594588649, 34, 0.0428629665024059,
                                        0.172, 32, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 1.15534198810534, 37, 0.0435695086402922,
                                        0.165, 33, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 1.42204243271555, 29, 0.0315220790438008,
                                        0.184, 34, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 1.24574074911569, 16, 0.409178957202378,
                                        0.251, 35, "PET ~ Cauchy(0, 1)[0, Inf]", "Normal(0, 1)", "InvGamma(1, 0.15)",
                                        1.10699013590912, 47, 0.775699977928656, 0.146, 36, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 1.03077461409007))
  })

  test_that("titleless-plot-1 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model36"]][["collection"]][["diagnostics_model36_PEESE"]][["collection"]][["diagnostics_model36_PEESE_autocor"]][["collection"]][["diagnostics_model36_PEESE_autocor_autocor1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-titleless-plot-1")
  })

  test_that("titleless-plot-2 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model36"]][["collection"]][["diagnostics_model36_PEESE"]][["collection"]][["diagnostics_model36_PEESE_samples"]][["collection"]][["diagnostics_model36_PEESE_samples_samples1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-titleless-plot-2")
  })

  test_that("titleless-plot-3 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model36"]][["collection"]][["diagnostics_model36_PEESE"]][["collection"]][["diagnostics_model36_PEESE_trace"]][["collection"]][["diagnostics_model36_PEESE_trace_trace1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-titleless-plot-3")
  })

  test_that("titleless-plot-4 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model36"]][["collection"]][["diagnostics_model36_mu"]][["collection"]][["diagnostics_model36_mu_autocor"]][["collection"]][["diagnostics_model36_mu_autocor_autocor1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-titleless-plot-4")
  })

  test_that("titleless-plot-5 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model36"]][["collection"]][["diagnostics_model36_mu"]][["collection"]][["diagnostics_model36_mu_samples"]][["collection"]][["diagnostics_model36_mu_samples_samples1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-titleless-plot-5")
  })

  test_that("titleless-plot-6 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model36"]][["collection"]][["diagnostics_model36_mu"]][["collection"]][["diagnostics_model36_mu_trace"]][["collection"]][["diagnostics_model36_mu_trace_trace1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-titleless-plot-6")
  })

  test_that("titleless-plot-7 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model36"]][["collection"]][["diagnostics_model36_tau"]][["collection"]][["diagnostics_model36_tau_autocor"]][["collection"]][["diagnostics_model36_tau_autocor_autocor1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-titleless-plot-7")
  })

  test_that("titleless-plot-8 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model36"]][["collection"]][["diagnostics_model36_tau"]][["collection"]][["diagnostics_model36_tau_samples"]][["collection"]][["diagnostics_model36_tau_samples_samples1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-titleless-plot-8")
  })

  test_that("titleless-plot-9 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model36"]][["collection"]][["diagnostics_model36_tau"]][["collection"]][["diagnostics_model36_tau_trace"]][["collection"]][["diagnostics_model36_tau_trace_trace1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-titleless-plot-9")
  })

  test_that("Model Averaged Effect Size Estimate plot matches", {
    plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-model-averaged-effect-size-estimate")
  })

  test_that("Model Averaged PET-PEESE Regression Estimate plot matches", {
    plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_petPeese"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-model-averaged-pet-peese-regression-estimate")
  })

  test_that("Model Averaged Heterogeneity Estimate plot matches", {
    plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-model-averaged-heterogeneity-estimate")
  })

  test_that("Model Averaged Weight Function Estimate plot matches", {
    plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_weightFunction"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-model-averaged-weight-function-estimate")
  })

  test_that("Model Averaged Forest Plot matches", {
    plotName <- results[["results"]][["forestPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-model-averaged-forest-plot")
  })

  test_that("Model Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model36"]][["collection"]][["individualModels_model36_tempCoef"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(109, 0.0133174627995021, 0.00921572739484352, 0.096, 0.187850015622251,
                                        0.193046759006048, 1.03077461409007, "Effect size (<unicode><unicode>)",
                                        0.363022822529052, 82, 0.0384630753969871, 0.007643119151744,
                                        0.11, 0.109063867122999, 0.0890192355729749, 1.00912538129661,
                                        "Heterogeneity (<unicode><unicode>)", 0.2935995589628))
  })

  test_that("Information table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model36"]][["collection"]][["individualModels_model36_tempInfo"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.291623897357259, 1.88323121598695, 0.00931955140180143, 0.03125
                                   ))
  })

  test_that("PET-PEESE Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model36"]][["collection"]][["individualModels_model36_tempPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(47, 0.20803726893087, 0.775699977928656, 0.146, 5.38092555084993,
                                        3.89286324518531, 1.00327218755274, "PEESE", 18.6268088488227
                                   ))
  })

  test_that("Priors table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model36"]][["collection"]][["individualModels_model36_tempPriors"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("PEESE ~ Cauchy(0, 5)[0, Inf]", "Normal(0, 1)", "InvGamma(1, 0.15)"
                                   ))
  })

  test_that("Model Averaged PET-PEESE Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.420277393584139, 0, "PET", 2.90355280001627, 0, 2.33547596516304,
                                        0, "PEESE", 26.6984155287194))
  })

  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.107495292311495, 0.0928626398063777, "Effect size (<unicode><unicode>)",
                                        0.310066219922612, 0, 0.052391429109386, 0, "Heterogeneity (<unicode><unicode>)",
                                        0.30893471583895))
  })

  test_that("Model Averaged Weights (Ï‰) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.025, 0.0761856630936217, 0.025, 0.845257891559781,
                                        1, 1, 0.05, 0.00747458877302344, 0.05, 0.699461842009699, 1,
                                        1, 0.5, 0.00596990848521762, 0.5, 0.672106752809156, 1, 1, 0.95,
                                        0.00714387273029817, 0.95, 0.690504586286083, 1, 1, 0.975, 0.00917511394835559,
                                        0.975, 0.76254440786186, 1, 1, 1))
  })

  test_that("Conditional PET-PEESE Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.168238042831845, 1.96688111161975, 2.03974752254471, "PET",
                                        3.67095638980588, 0.522447912463836, 16.5866190532737, 18.3094472628903,
                                        "PEESE", 37.6359039029146))
  })

  test_that("Conditional Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.00921952557140301, 0.195555827188716, 0.205571625759725, "Effect size (<unicode><unicode>)",
                                        0.338950360476893, 0.0359837037719721, 0.150192620847865, 0.124975322315014,
                                        "Heterogeneity (<unicode><unicode>)", 0.418569910218565))
  })

  test_that("Conditional Weights (Ï‰) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.025, 0.025772098287472, 0.025, 0.62973176808877,
                                        0.678683347063942, 1, 0.05, 0.00311625825930277, 0.05, 0.280888273825145,
                                        0.193821810720746, 0.896858868222006, 0.5, 0.00261684906189619,
                                        0.5, 0.21315934798712, 0.127957467609235, 0.841786945400988,
                                        0.95, 0.00270003366290422, 0.95, 0.257675895489583, 0.157712366214809,
                                        0.906607798546843, 0.975, 0.00347360164224399, 0.975, 0.429595892651798,
                                        0.235127895361591, 1, 1))
  })

  test_that("Models Overview table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.000246601894468259, -3.13515764574646, 1, 0.000246601894468259,
                                        "", "Spike(0)", "Spike(0)", 0.125, 0.00438083436779663, 2.22696821114714,
                                        2, 0.00438083436779663, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 0.00453165595314799,
                                        2.26081643523809, 3, 0.00453165595314799, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 0.00439572921868446,
                                        2.23036244740856, 4, 0.00439572921868446, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 0.0156593221404707,
                                        3.50079541354657, 5, 0.0156593221404707, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 0.000796767591506016,
                                        0.522536763120254, 6, 0.000796767591506016, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 0.00277423091776359,
                                        1.77010257200419, 7, 0.00277423091776359, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Spike(0)", "Spike(0)", 0.0104166666666667, 0.131309531552704,
                                        4.52867409367494, 8, 0.131309531552704, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Spike(0)", "Spike(0)", 0.03125, 0.0694931865738306, 3.8923454341579,
                                        9, 0.0694931865738306, "PEESE ~ Cauchy(0, 5)[0, Inf]", "Spike(0)",
                                        "Spike(0)", 0.03125, 0.0201448460402386, 1.26777083621664, 10,
                                        0.0201448460402386, "", "Spike(0)", "InvGamma(1, 0.15)", 0.125,
                                        0.0112257809361823, 3.16794201233036, 11, 0.0112257809361823,
                                        "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.0104166666666667, 0.0128315692440138, 3.30163749164142, 12,
                                        0.0128315692440138, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.023118385469569,
                                        3.89035721870457, 13, 0.023118385469569, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.0489246863710918,
                                        4.6400111128297, 14, 0.0489246863710918, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.0158124562785326,
                                        3.51052701145984, 15, 0.0158124562785326, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.0245029127601291,
                                        3.94852100862248, 16, 0.0245029127601291, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.035956097173456,
                                        3.2334153925751, 17, 0.035956097173456, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.03125, 0.0239261278934227,
                                        2.82608780041041, 18, 0.0239261278934227, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.03125, 0.163112011135481,
                                        3.35925950986551, 19, 0.163112011135481, "", "Normal(0, 1)",
                                        "Spike(0)", 0.125, 0.0348971036725054, 4.30213284640799, 20,
                                        0.0348971036725054, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 0.0361691040954755,
                                        4.33793428635636, 21, 0.0361691040954755, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 0.0231798989141835,
                                        3.89301448696534, 22, 0.0231798989141835, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 0.0401745325451264,
                                        4.44296228605912, 23, 0.0401745325451264, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 0.0217825943953235,
                                        3.83084023862329, 24, 0.0217825943953235, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 0.0289075957799639,
                                        4.11383340031114, 25, 0.0289075957799639, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.0104166666666667, 0.034579208783771,
                                        3.19436932054824, 26, 0.034579208783771, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Normal(0, 1)", "Spike(0)", 0.03125, 0.037575845363527, 3.27747815471908,
                                        27, 0.037575845363527, "PEESE ~ Cauchy(0, 5)[0, Inf]", "Normal(0, 1)",
                                        "Spike(0)", 0.03125, 0.0442948398399929, 2.05569054828122, 28,
                                        0.0442948398399929, "", "Normal(0, 1)", "InvGamma(1, 0.15)",
                                        0.125, 0.00984251726326398, 3.03644050775525, 29, 0.00984251726326398,
                                        "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0104166666666667, 0.0109178134462958,
                                        3.14012472638149, 30, 0.0109178134462958, "omega[two-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.00929540736583293,
                                        2.97924945652325, 31, 0.00929540736583293, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.0158431791153678,
                                        3.51246807794606, 32, 0.0158431791153678, "omega[one-sided: .05, .025] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.00920105985898336,
                                        2.96904768945306, 33, 0.00920105985898336, "omega[one-sided: .5, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.0103202386645941,
                                        3.08383589617007, 34, 0.0103202386645941, "omega[one-sided: .5, .05, .025] ~ CumDirichlet(1, 1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0104166666666667, 0.0105567759815021,
                                        2.00788464818292, 35, 0.0105567759815021, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.03125, 0.00931955140180143,
                                        1.88323121598695, 36, 0.00931955140180143, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.03125))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1.22207051713741, "18/36", 0.549969277622992, 0.5, "Effect", 0.529132057013358,
                                        "18/36", 0.346034245104271, 0.5, "Heterogeneity", 3.38984840880788,
                                        "32/36", 0.772201701089819, 0.5, "Publication bias"))
  })

  test_that("Conditional Effect Size Estimates plot matches", {
    plotName <- results[["results"]][["modelsPlots"]][["collection"]][["modelsPlots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-conditional-effect-size-estimates")
  })

  test_that("Conditional Heterogeneity Estimates plot matches", {
    plotName <- results[["results"]][["modelsPlots"]][["collection"]][["modelsPlots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test2-conditional-heterogeneity-estimates")
  })
}

### fit a minimum model using r + N, with autofit & check diagnostics
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedAdapt <- 100
  options$advancedBurnin <- 100
  options$advancedChains <- 2
  options$advancedIteration <- 100
  options$autofitMcmcError <- FALSE
  options$autofitMcmcErrorSd <- FALSE
  options$autofitTime <- FALSE
  options$diagnosticsOverview <- TRUE
  options$effect <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$effectNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                  parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                  parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                  truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$fittedPath <- ""
  options$heterogeneity <- list()
  options$heterogeneityNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                         parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                         parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                         truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$inputCI <- list()
  options$inputES <- "d"
  options$inputN <- "N"
  options$measures <- "correlation"
  options$modelType <- "custom"
  options$omega <- list()
  options$omegaNull <- list(list(name = "#", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                                 parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$peese <- list()
  options$peeseNull <- list()
  options$pet <- list()
  options$petNull <- list()
  options$plotForestOrder <- "alphabetical"
  options$plotModelsOrder <- "decreasing"
  options$plotModelsOrderBy <- "model"
  options$resultsModelsBf <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = c("study one", "study two", "study three"
  ), t = c(2.51, 2.39, 2.55), N = c(100L, 150L, 97L), d = c(0.25,
                                                            0.2, 0.26), se = c(0.1, 0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L,
                                                                                                                              75L, 48L), lCI = c(0.05, 0.04, 0.06), uCI = c(0.45, 0.41, 0.38
                                                                                                                              )), class = "data.frame", row.names = c(NA, -3L))
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("", "", "", 1, "", "Spike(0)", "Spike(0)", "", 1419, 0.0029373457650719,
                                        0.027, 2, "", "Normal(0, 1)", "Spike(0)", 1.00244916961825
                                   ))
  })

  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.235030324916897, 0.459243524043423, 0.457954601758173, "Effect size (<unicode><unicode>)",
                                        0.675618118632162, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                                        0))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1137.167831503, "1/2", 0.999121394953959, 0.5, "Effect", 0, "0/2",
                                        0, 0, "Heterogeneity", 0, "0/2", 0, 0, "Publication bias"))
  })
}

### fit a minimum model using logOR + CI, with autofit & check diagnostics
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedAdapt <- 100
  options$advancedBurnin <- 100
  options$advancedChains <- 2
  options$advancedIteration <- 100
  options$autofitEss <- FALSE
  options$autofitExtendSamples <- 100
  options$autofitMcmcErrorSdValue <- 0.05
  options$autofitMcmcErrorValue <- 0.01
  options$autofitRhat <- FALSE
  options$autofitTime <- FALSE
  options$diagnosticsOverview <- TRUE
  options$effect <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$effectNull <- list()
  options$fittedPath <- ""
  options$heterogeneity <- list()
  options$heterogeneityNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                         parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                         parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                         truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$inputCI <- list(c("lCI", "uCI"))
  options$inputES <- "d"
  options$measures <- "logOR"
  options$modelType <- "custom"
  options$omega <- list()
  options$omegaNull <- list(list(name = "#", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                                 parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$peese <- list()
  options$peeseNull <- list()
  options$pet <- list()
  options$petNull <- list()
  options$plotForestOrder <- "alphabetical"
  options$plotModelsOrder <- "decreasing"
  options$plotModelsOrderBy <- "model"
  options$priorsNull <- TRUE
  options$resultsModelsBf <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = c("study one", "study two", "study three"
  ), t = c(2.51, 2.39, 2.55), N = c(100L, 150L, 97L), d = c(0.25,
                                                            0.2, 0.26), se = c(0.1, 0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L,
                                                                                                                              75L, 48L), lCI = c(0.05, 0.04, 0.06), uCI = c(0.45, 0.41, 0.38
                                                                                                                              )), class = "data.frame", row.names = c(NA, -3L))
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(459, 0.00131574351854272, 0.047, 1, "", "Normal(0, 1)", "Spike(0)",
                                        1.00569538319404))
  })

  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.0749305780530607, 0.130668040424297, 0.131189596656252, "Effect size (<unicode><unicode>)",
                                        0.184922013934393, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                                        0))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("<unicode><unicode><unicode>", "1/1", 1, 1, "Effect", 0, "0/1",
                                        0, 0, "Heterogeneity", 0, "0/1", 0, 0, "Publication bias"))
  })
}

### fit a minimum model using general effect sizes, with autofit & check diagnostics
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedAdapt <- 100
  options$advancedBurnin <- 100
  options$advancedChains <- 2
  options$advancedIteration <- 100
  options$autofitEss <- FALSE
  options$autofitExtendSamples <- 100
  options$autofitMcmcErrorSdValue <- 0.05
  options$autofitMcmcErrorValue <- 0.01
  options$autofitRhat <- FALSE
  options$autofitTime <- FALSE
  options$diagnosticsOverview <- TRUE
  options$effect <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$effectNull <- list()
  options$fittedPath <- ""
  options$heterogeneity <- list()
  options$heterogeneityNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                         parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                         parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                         truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$inputCI <- list(c("lCI", "uCI"))
  options$inputES <- "d"
  options$measures <- "general"
  options$modelType <- "custom"
  options$omega <- list()
  options$omegaNull <- list(list(name = "#", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                                 parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$peese <- list()
  options$peeseNull <- list()
  options$pet <- list()
  options$petNull <- list()
  options$plotForestOrder <- "alphabetical"
  options$plotModelsOrder <- "decreasing"
  options$plotModelsOrderBy <- "model"
  options$priorsNull <- TRUE
  options$resultsModelsBf <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$savePath <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = c("study one", "study two", "study three"
  ), t = c(2.51, 2.39, 2.55), N = c(100L, 150L, 97L), d = c(0.25,
                                                            0.2, 0.26), se = c(0.1, 0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L,
                                                                                                                              75L, 48L), lCI = c(0.05, 0.04, 0.06), uCI = c(0.45, 0.41, 0.38
                                                                                                                              )), class = "data.frame", row.names = c(NA, -3L))
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(400, 0.00278998991237116, 0.05, 1, "", "Normal(0, 1)", "Spike(0)",
                                        0.997954770616823))
  })

  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.133172039662128, 0.238447795286099, 0.238294812780349, "Effect size (<unicode><unicode>)",
                                        0.350093520452754, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                                        0))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("<unicode><unicode><unicode>", "1/1", 1, 1, "Effect", 0, "0/1",
                                        0, 0, "Heterogeneity", 0, "0/1", 0, 0, "Publication bias"))
  })
}

### more options tested using a pre-loaded model
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$autofitMcmcError <- FALSE
  options$autofitMcmcErrorSd <- FALSE
  options$autofitTime <- FALSE
  options$bayesFactorType <- "BF01"
  options$effect <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                              parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                              parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                              truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$effectNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                  parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                  parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                  truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$fittedPath <- fittedPath
  options$heterogeneity <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                     parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                     parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                     truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$heterogeneityNull <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                                         parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                                         parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                                         truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$inputCI <- list()
  options$measures <- "fitted"
  options$omega <- list(list(name = "#", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#2", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "two-sided"),
                        list(name = "#3", parAlpha = "(1,1)", parCuts = "(.05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#4", parAlpha = "(1,1,1)", parCuts = "(.025, .05)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#5", parAlpha = "(1,1,1)", parCuts = "(.05, .50)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"),
                        list(name = "#6", parAlpha = "(1,1,1,1)", parCuts = "(.025, .05, .10)",
                             parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "one-sided"))
  options$omegaNull <- list(list(name = "#", parAlpha = "(1,1,1)", parCuts = "(.05, .10)",
                                 parOmega = "(1, 0.5, 0.1)", priorWeight = "1", type = "none"))
  options$peese <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                             parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                             parScale = "1", parScale2 = "5", parShape = "1", priorWeight = "1",
                             truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$peeseNull <- list()
  options$pet <- list(list(name = "#", parA = "0", parAlpha = "1", parB = "1",
                           parBeta = "0.15", parDf = "2", parLocation = "0", parMean = "0",
                           parScale = "1", parScale2 = "1", parShape = "1", priorWeight = "1",
                           truncationLower = "0", truncationUpper = "Inf", type = "cauchy"))
  options$petNull <- list()
  options$plotForest <- TRUE
  options$plotForestOrder <- "increasing"
  options$plotForestType <- "conditional"
  options$plotModelsMu <- TRUE
  options$plotModelsOrder <- "decreasing"
  options$plotModelsOrderBy <- "BF"
  options$resultsCi <- 0.9
  options$resultsIndividual <- TRUE
  options$resultsIndividualSingle <- TRUE
  options$resultsIndividualSingleNumber <- 36
  options$resultsModelsBf <- "inclusion"
  options$resultsModelsOrder <- "default"
  options$resultsScale <- "r"
  options$savePath <- ""
  options$shortNames <- TRUE
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Conditional Forest Plot matches", {
    plotName <- results[["results"]][["forestPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test3-conditional-forest-plot")
  })

  test_that("Model Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model36"]][["collection"]][["individualModels_model36_tempCoef"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1208, -0.55265224544879, 0.00640202639197792, 0.000324591007338291,
                                        -0.138684314718299, -0.107475461708634, 1.00082978808933, "Effect size (<unicode>)",
                                        0.216973334570519, 5728, 0.0174537799642739, 0.0012522327854329,
                                        0.000132131883778871, 0.0927573625888651, 0.0659913245690277,
                                        1.0007087635349, "Heterogeneity (<unicode>)", 0.324255793050251
                                   ))
  })

  test_that("Information table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model36"]][["collection"]][["individualModels_model36_tempInfo"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.72727569165726, -0.0560597740495541, 0.0085803364481458, 0.03125
                                   ))
  })

  test_that("PET-PEESE Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model36"]][["collection"]][["individualModels_model36_tempPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1025, 0.296957780781618, 0.200721180153232, 0.000312344511712233,
                                        7.80944540517883, 6.21826436865384, 1.00022266168806, "PEESE",
                                        23.8723910117661))
  })

  test_that("Priors table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model36"]][["collection"]][["individualModels_model36_tempPriors"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("PEESE ~ C(0, 5)[0, Inf]", "N(0, 1)", "Ig(1, 0.15)"))
  })

  test_that("Model Averaged PET-PEESE Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.0381605068045753, 0, "PET", 0.023616278744856, 0, 0.328659845818013,
                                        0, "PEESE", 2.71010683008597))
  })

  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-0.0568053720471119, 0.0106824496399714, 0, "Effect size (<unicode>)",
                                        0.134825866250297, 0, 0.048035807303762, 0, "Heterogeneity (<unicode>)",
                                        0.212800710036252))
  })

  test_that("Model Averaged Weights (ω) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.025, 0.623198762393136, 0.025, 0.950451745491881,
                                        1, 1, 0.05, 0.391378018345742, 0.05, 0.892700675024233, 1, 1,
                                        0.5, 0.0128411575295533, 0.5, 0.80972860730324, 1, 1, 0.95,
                                        0.0128411575295533, 0.95, 0.80972860730324, 1, 1, 0.975, 0.0128411575295533,
                                        0.975, 0.814995515093576, 1, 1, 1))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.34694629322749, "18/36", 0.23004655050788, 0.5, "Effect", 1.72485968725674,
                                        "18/36", 0.366991373785838, 0.5, "Heterogeneity", 1.06264271661404,
                                        "32/36", 0.484814937626021, 0.5, "Publication bias"))
  })

  test_that("Conditional Effect Size Estimates plot matches", {
    plotName <- results[["results"]][["modelsPlots"]][["collection"]][["modelsPlots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "test3-conditional-effect-size-estimates")
  })
}
