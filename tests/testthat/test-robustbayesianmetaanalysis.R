context("Robust Bayesian Meta-Analysis")


### create a pre-fitted model (in case that the package needs to be updated)
if (FALSE){
  # fit a default model
  fit <- RoBMA::RoBMA(d = c(.3, .2, .1), n = c(30, 35, 40), parallel = TRUE, seed = 1)
  # remove majority of the samples to save space
  for(i in 2:length(fit$models)){
    for(j in seq_along(fit$models[[i]]$fit$mcmc)){
      fit$models[[i]]$fit$mcmc[[j]] <- fit$models[[i]]$fit$mcmc[[j]][1:100,]
    }
  }
  set.seed(1)
  for(p in c("mu", "tau", "PET", "PEESE")){
    fit$RoBMA$posteriors[[p]] <- sample(fit$RoBMA$posteriors[[p]], 100)
  }
  for(p in c("omega")){
    fit$RoBMA$posteriors[[p]] <- fit$RoBMA$posteriors[[p]][sample(nrow(fit$RoBMA$posteriors[[p]]), 100),]
  }
  saveRDS(fit, file = "tests/robmaFit.RDS")
}

# path to the pre-fitted RoBMA model
pathToFittedModel <- file.path("robmaFit.RDS")

### RoBMA-PP/RoBMA-old model settings
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(effectSize = list(shouldEncode = TRUE), effectSizeCi = list(
    shouldEncode = TRUE), effectSizeSe = list(shouldEncode = TRUE),
    modelsEffect = list(a = list(isRCode = TRUE), alpha = list(
      isRCode = TRUE), b = list(isRCode = TRUE), beta = list(
        isRCode = TRUE), k = list(isRCode = TRUE), mu = list(
          isRCode = TRUE), nu = list(isRCode = TRUE), priorWeight = list(
            isRCode = TRUE), sigma = list(isRCode = TRUE), theta = list(
              isRCode = TRUE), truncationLower = list(isRCode = TRUE),
      truncationUpper = list(isRCode = TRUE), x0 = list(isRCode = TRUE)),
    modelsEffectNull = list(a = list(isRCode = TRUE), alpha = list(
      isRCode = TRUE), b = list(isRCode = TRUE), beta = list(
        isRCode = TRUE), k = list(isRCode = TRUE), mu = list(
          isRCode = TRUE), nu = list(isRCode = TRUE), priorWeight = list(
            isRCode = TRUE), sigma = list(isRCode = TRUE), theta = list(
              isRCode = TRUE), truncationLower = list(isRCode = TRUE),
      truncationUpper = list(isRCode = TRUE), x0 = list(isRCode = TRUE)),
    modelsHeterogeneity = list(a = list(isRCode = TRUE), alpha = list(
      isRCode = TRUE), b = list(isRCode = TRUE), beta = list(
        isRCode = TRUE), k = list(isRCode = TRUE), mu = list(
          isRCode = TRUE), nu = list(isRCode = TRUE), priorWeight = list(
            isRCode = TRUE), sigma = list(isRCode = TRUE), theta = list(
              isRCode = TRUE), truncationLower = list(isRCode = TRUE),
      truncationUpper = list(isRCode = TRUE), x0 = list(isRCode = TRUE)),
    modelsHeterogeneityNull = list(a = list(isRCode = TRUE),
                                   alpha = list(isRCode = TRUE), b = list(isRCode = TRUE),
                                   beta = list(isRCode = TRUE), k = list(isRCode = TRUE),
                                   mu = list(isRCode = TRUE), nu = list(isRCode = TRUE),
                                   priorWeight = list(isRCode = TRUE), sigma = list(isRCode = TRUE),
                                   theta = list(isRCode = TRUE), truncationLower = list(
                                     isRCode = TRUE), truncationUpper = list(isRCode = TRUE),
                                   x0 = list(isRCode = TRUE)), modelsPeese = list(a = list(
                                     isRCode = TRUE), alpha = list(isRCode = TRUE), b = list(
                                       isRCode = TRUE), beta = list(isRCode = TRUE), k = list(
                                         isRCode = TRUE), mu = list(isRCode = TRUE), nu = list(
                                           isRCode = TRUE), sigma = list(isRCode = TRUE), truncationLower = list(
                                             isRCode = TRUE), truncationUpper = list(isRCode = TRUE),
                                     x0 = list(isRCode = TRUE)), modelsPet = list(a = list(
                                       isRCode = TRUE), alpha = list(isRCode = TRUE), b = list(
                                         isRCode = TRUE), beta = list(isRCode = TRUE), k = list(
                                           isRCode = TRUE), mu = list(isRCode = TRUE), nu = list(
                                             isRCode = TRUE), sigma = list(isRCode = TRUE), theta = list(
                                               isRCode = TRUE), truncationLower = list(isRCode = TRUE),
                                       truncationUpper = list(isRCode = TRUE), x0 = list(isRCode = TRUE)),
    modelsSelectionModelsNull = list(priorWeight = list(isRCode = TRUE)),
    sampleSize = list(shouldEncode = TRUE), studyLabel = list(
      shouldEncode = TRUE))
  options$advancedAutofitMaximumFittingTime <- FALSE
  options$advancedAutofitMcmcError <- FALSE
  options$advancedAutofitMcmcErrorSd <- FALSE
  options$advancedSaveFittedModel <- ""
  options$effectSizeCi <- list()
  options$inferenceModelsOverviewBfComparison <- "inclusion"
  options$inferenceModelsOverviewOrder <- "modelNumber"
  options$inputType <- "cohensD"
  options$modelEnsembleType <- "PP"
  options$modelsEffect <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                    mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                    theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                    type = "normal", x0 = "0"))
  options$modelsEffectNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                        mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                        theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                        type = "spike", x0 = "0"))
  options$modelsHeterogeneity <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                           mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                           theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                           type = "invgamma", x0 = "0"))
  options$modelsHeterogeneityNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                               mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                               theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                               type = "spike", x0 = "0"))
  options$modelsPeese <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                   mu = "0", name = "#", nu = "2", priorWeight = "1/4", sigma = "1",
                                   theta = "5", truncationLower = "0", truncationUpper = "Inf",
                                   type = "cauchy", x0 = "0"))
  options$modelsPeeseNull <- list()
  options$modelsPet <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                 mu = "0", name = "#", nu = "2", priorWeight = "1/4", sigma = "1",
                                 theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                 type = "cauchy", x0 = "0"))
  options$modelsPetNull <- list()
  options$modelsSelectionModels <- list(list(alpha = "(1,1)", name = "#", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05)", priorWeight = "1/12", type = "twoSided"),
                                        list(alpha = "(1,1,1)", name = "#2", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "twoSided"),
                                        list(alpha = "(1,1,1)", name = "#3", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#4", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#5", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#6", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"))
  options$modelsSelectionModelsNull <- list(list(alpha = "(1,1,1)", name = "#", omega = "(1, 0.5, 0.1)",
                                                 pValues = "(.05, .10)", priorWeight = "1", type = "none"))
  options$pathToFittedModel <- ""
  options$plotsForestPlotOrder <- "alphabetical"
  options$plotsIndividualModelsOrder <- "decreasing"
  options$plotsIndividualModelsOrderBy <- "modelNumber"
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Model Specification Preview table results match", {
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
  options$advancedAutofitMaximumFittingTime <- FALSE
  options$advancedAutofitMcmcError <- FALSE
  options$advancedAutofitMcmcErrorSd <- FALSE
  options$advancedSaveFittedModel <- ""
  options$effectSizeCi <- list()
  options$inferenceModelsOverviewBfComparison <- "inclusion"
  options$inferenceModelsOverviewOrder <- "modelNumber"
  options$inputType <- "cohensD"
  options$modelEnsembleType <- "original"
  options$modelsEffect <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                    mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                    theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                    type = "normal", x0 = "0"))
  options$modelsEffectNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                        mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                        theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                        type = "spike", x0 = "0"))
  options$modelsHeterogeneity <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                           mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                           theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                           type = "invgamma", x0 = "0"))
  options$modelsHeterogeneityNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                               mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                               theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                               type = "spike", x0 = "0"))
  options$modelsPeese <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                   mu = "0", name = "#", nu = "2", priorWeight = "1/4", sigma = "1",
                                   theta = "5", truncationLower = "0", truncationUpper = "Inf",
                                   type = "cauchy", x0 = "0"))
  options$modelsPeeseNull <- list()
  options$modelsPet <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                 mu = "0", name = "#", nu = "2", priorWeight = "1/4", sigma = "1",
                                 theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                 type = "cauchy", x0 = "0"))
  options$modelsPetNull <- list()
  options$modelsSelectionModels <- list(list(alpha = "(1,1)", name = "#", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05)", priorWeight = "1/12", type = "twoSided"),
                                        list(alpha = "(1,1,1)", name = "#2", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "twoSided"),
                                        list(alpha = "(1,1,1)", name = "#3", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#4", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#5", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#6", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"))
  options$modelsSelectionModelsNull <- list(list(alpha = "(1,1,1)", name = "#", omega = "(1, 0.5, 0.1)",
                                                 pValues = "(.05, .10)", priorWeight = "1", type = "none"))
  options$pathToFittedModel <- ""
  options$plotsForestPlotOrder <- "alphabetical"
  options$plotsIndividualModelsOrder <- "decreasing"
  options$plotsIndividualModelsOrderBy <- "modelNumber"
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Model Specification Preview table results match", {
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

### custom model settings (testing different distributions and prior plots)
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedAutofitMaximumFittingTime <- FALSE
  options$advancedAutofitMcmcError <- FALSE
  options$advancedAutofitMcmcErrorSd <- FALSE
  options$advancedSaveFittedModel <- ""
  options$effectSizeCi <- list()
  options$inferenceModelsOverviewBfComparison <- "inclusion"
  options$inferenceModelsOverviewOrder <- "modelNumber"
  options$inputType <- "cohensD"
  options$modelEnsembleType <- "custom"
  options$modelsEffect <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                    mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                    theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                    type = "normal", x0 = "0"), list(a = "0", alpha = "1", b = "1",
                                                                     beta = "0.15", k = "1", mu = "0", name = "#2", nu = "2",
                                                                     priorWeight = "1", sigma = "1", theta = "1", truncationLower = "-Inf",
                                                                     truncationUpper = "Inf", type = "t", x0 = "0"), list(a = "0",
                                                                                                                          alpha = "1", b = "1", beta = "0.15", k = "1", mu = "0", name = "#3",
                                                                                                                          nu = "2", priorWeight = "1", sigma = "1", theta = "1", truncationLower = "-Inf",
                                                                                                                          truncationUpper = "Inf", type = "cauchy", x0 = "0"), list(
                                                                                                                            a = "0", alpha = "1", b = "1", beta = "0.15", k = "1", mu = "0",
                                                                                                                            name = "#4", nu = "2", priorWeight = "1", sigma = "1", theta = "1",
                                                                                                                            truncationLower = "0", truncationUpper = "Inf", type = "gammaAB",
                                                                                                                            x0 = "0"), list(a = "0", alpha = "1", b = "1", beta = "0.15",
                                                                                                                                            k = "1", mu = "0", name = "#5", nu = "2", priorWeight = "1",
                                                                                                                                            sigma = "1", theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                                                                                                                            type = "invgamma", x0 = "0"), list(a = "0", alpha = "1",
                                                                                                                                                                               b = "1", beta = "0.15", k = "1", mu = "0", name = "#6", nu = "2",
                                                                                                                                                                               priorWeight = "1", sigma = "1", theta = "1", truncationLower = "0",
                                                                                                                                                                               truncationUpper = "Inf", type = "lognormal", x0 = "0"), list(
                                                                                                                                                                                 a = "0", alpha = "1", b = "1", beta = "0.15", k = "1", mu = "0",
                                                                                                                                                                                 name = "#7", nu = "2", priorWeight = "1", sigma = "1", theta = "1",
                                                                                                                                                                                 truncationLower = "0", truncationUpper = "1", type = "beta",
                                                                                                                                                                                 x0 = "0"), list(a = "0", alpha = "1", b = "1", beta = "0.15",
                                                                                                                                                                                                 k = "1", mu = "0", name = "#8", nu = "2", priorWeight = "1",
                                                                                                                                                                                                 sigma = "1", theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                                                                                                                                                                                 type = "uniform", x0 = "0"), list(a = "0", alpha = "1", b = "1",
                                                                                                                                                                                                                                   beta = "0.15", k = "1", mu = "0", name = "#10", nu = "2",
                                                                                                                                                                                                                                   priorWeight = "1", sigma = "1", theta = "1", truncationLower = "-Inf",
                                                                                                                                                                                                                                   truncationUpper = "Inf", type = "spike", x0 = "0"))
  options$modelsEffectNull <- list()
  options$modelsHeterogeneity <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                           mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                           theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                           type = "invgamma", x0 = "0"))
  options$modelsHeterogeneityNull <- list()
  options$modelsPeese <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                   mu = "0", name = "#", nu = "2", priorWeight = "1/4", sigma = "1",
                                   theta = "5", truncationLower = "0", truncationUpper = "Inf",
                                   type = "cauchy", x0 = "0"))
  options$modelsPeeseNull <- list()
  options$modelsPet <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                 mu = "0", name = "#", nu = "2", priorWeight = "1/4", sigma = "1",
                                 theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                 type = "cauchy", x0 = "0"))
  options$modelsPetNull <- list()
  options$modelsSelectionModels <- list(list(alpha = "(1,1)", name = "#", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05)", priorWeight = "1/12", type = "twoSided"),
                                        list(alpha = "(1,1,1)", name = "#4", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#5", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#6", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"))
  options$modelsSelectionModelsNull <- list()
  options$pathToFittedModel <- ""
  options$plotsForestPlotOrder <- "alphabetical"
  options$plotsIndividualModelsOrder <- "decreasing"
  options$plotsIndividualModelsOrderBy <- "modelNumber"
  options$priorDistributionPlot <- TRUE
  options$priorsNull <- TRUE
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Model Specification Preview table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 2, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0111111111111111, 3,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 4, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0111111111111111, 5,
                                        "PET ~ Cauchy(0, 1)[0, Inf]", "Normal(0, 1)", "InvGamma(1, 0.15)",
                                        0.0333333333333333, 6, "PEESE ~ Cauchy(0, 5)[0, Inf]", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0333333333333333, 7, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Student-t(0, 1, 2)", "InvGamma(1, 0.15)", 0.0111111111111111,
                                        8, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Student-t(0, 1, 2)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 9, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Student-t(0, 1, 2)", "InvGamma(1, 0.15)", 0.0111111111111111,
                                        10, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Student-t(0, 1, 2)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 11, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Student-t(0, 1, 2)", "InvGamma(1, 0.15)", 0.0333333333333333,
                                        12, "PEESE ~ Cauchy(0, 5)[0, Inf]", "Student-t(0, 1, 2)", "InvGamma(1, 0.15)",
                                        0.0333333333333333, 13, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Cauchy(0, 1)", "InvGamma(1, 0.15)", 0.0111111111111111, 14,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Cauchy(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 15, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Cauchy(0, 1)", "InvGamma(1, 0.15)", 0.0111111111111111, 16,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Cauchy(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 17, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Cauchy(0, 1)", "InvGamma(1, 0.15)", 0.0333333333333333, 18,
                                        "PEESE ~ Cauchy(0, 5)[0, Inf]", "Cauchy(0, 1)", "InvGamma(1, 0.15)",
                                        0.0333333333333333, 19, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Gamma(1, 0.15)", "InvGamma(1, 0.15)", 0.0111111111111111, 20,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Gamma(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 21, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Gamma(1, 0.15)", "InvGamma(1, 0.15)", 0.0111111111111111, 22,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Gamma(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 23, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Gamma(1, 0.15)", "InvGamma(1, 0.15)", 0.0333333333333333, 24,
                                        "PEESE ~ Cauchy(0, 5)[0, Inf]", "Gamma(1, 0.15)", "InvGamma(1, 0.15)",
                                        0.0333333333333333, 25, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "InvGamma(1, 0.15)", "InvGamma(1, 0.15)", 0.0111111111111111,
                                        26, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "InvGamma(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 27, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "InvGamma(1, 0.15)", "InvGamma(1, 0.15)", 0.0111111111111111,
                                        28, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "InvGamma(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 29, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "InvGamma(1, 0.15)", "InvGamma(1, 0.15)", 0.0333333333333333,
                                        30, "PEESE ~ Cauchy(0, 5)[0, Inf]", "InvGamma(1, 0.15)", "InvGamma(1, 0.15)",
                                        0.0333333333333333, 31, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Lognormal(0, 1)", "InvGamma(1, 0.15)", 0.0111111111111111,
                                        32, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Lognormal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 33, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Lognormal(0, 1)", "InvGamma(1, 0.15)", 0.0111111111111111,
                                        34, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Lognormal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 35, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Lognormal(0, 1)", "InvGamma(1, 0.15)", 0.0333333333333333,
                                        36, "PEESE ~ Cauchy(0, 5)[0, Inf]", "Lognormal(0, 1)", "InvGamma(1, 0.15)",
                                        0.0333333333333333, 37, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Beta(1, 0.15)", "InvGamma(1, 0.15)", 0.0111111111111111, 38,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Beta(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 39, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Beta(1, 0.15)", "InvGamma(1, 0.15)", 0.0111111111111111, 40,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Beta(1, 0.15)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 41, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Beta(1, 0.15)", "InvGamma(1, 0.15)", 0.0333333333333333, 42,
                                        "PEESE ~ Cauchy(0, 5)[0, Inf]", "Beta(1, 0.15)", "InvGamma(1, 0.15)",
                                        0.0333333333333333, 43, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Uniform(0, 1)", "InvGamma(1, 0.15)", 0.0111111111111111, 44,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Uniform(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 45, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Uniform(0, 1)", "InvGamma(1, 0.15)", 0.0111111111111111, 46,
                                        "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)", "Uniform(0, 1)",
                                        "InvGamma(1, 0.15)", 0.0111111111111111, 47, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Uniform(0, 1)", "InvGamma(1, 0.15)", 0.0333333333333333, 48,
                                        "PEESE ~ Cauchy(0, 5)[0, Inf]", "Uniform(0, 1)", "InvGamma(1, 0.15)",
                                        0.0333333333333333, 49, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0111111111111111, 50, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0111111111111111, 51, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0111111111111111, 52, "omega[one-sided: .1, .05] ~ CumDirichlet(1, 1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0111111111111111, 53, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0333333333333333, 54, "PEESE ~ Cauchy(0, 5)[0, Inf]",
                                        "Spike(0)", "InvGamma(1, 0.15)", 0.0333333333333333))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["modelPreview"]][["collection"]][["modelPreview_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("54/54", 1, "Effect", "54/54", 1, "Heterogeneity", "54/54", 1,
                                        "Publication bias"))
  })

  test_that("prior-distribution-plot-2 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-2")
  })

  test_that("prior-distribution-plot-3 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-3")
  })

  test_that("prior-distribution-plot-4 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-4")
  })

  test_that("prior-distribution-plot-5 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative4"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-5")
  })

  test_that("prior-distribution-plot-6 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative5"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-6")
  })

  test_that("prior-distribution-plot-7 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative6"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-7")
  })

  test_that("prior-distribution-plot-8 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-8")
  })

  test_that("prior-distribution-plot-9 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-9")
  })

  test_that("prior-distribution-plot-10 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-10")
  })

  test_that("prior-distribution-plot-11 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative4"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-11")
  })

  test_that("prior-distribution-plot-12 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative5"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-12")
  })

  test_that("prior-distribution-plot-13 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative6"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-13")
  })

  test_that("prior-distribution-plot-14 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative7"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-14")
  })

  test_that("prior-distribution-plot-15 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative8"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-15")
  })

  test_that("prior-distribution-plot-16 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative9"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-16")
  })

  test_that("prior-distribution-plot-17 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_heterogeneity"]][["collection"]][["priorPlots_heterogeneity_alternative"]][["collection"]][["priorPlots_heterogeneity_alternative_heterogeneityalternative1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-plot-17")
  })
}

### fit a small model using d + se, with minimum samples, no autofit, & and the complete output
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedAutofitMaximumFittingTime <- FALSE
  options$advancedAutofitMcmcError <- FALSE
  options$advancedAutofitMcmcErrorSd <- FALSE
  options$advancedMcmcAdaptation <- 100
  options$advancedMcmcBurnin <- 100
  options$advancedMcmcChains <- 1
  options$advancedMcmcSamples <- 100
  options$advancedSaveFittedModel <- ""
  options$autofit <- FALSE
  options$effectSize <- "ES"
  options$effectSizeCi <- list()
  options$effectSizeSe <- "SE"
  options$inferenceConditionalParameterEstimates <- TRUE
  options$inferenceIndividualModels <- TRUE
  options$inferenceIndividualModelsSingleModel <- TRUE
  options$inferenceIndividualModelsSingleModelNumber <- 12
  options$inferenceModelsOverview <- TRUE
  options$inferenceModelsOverviewBfComparison <- "inclusion"
  options$inferenceModelsOverviewOrder <- "modelNumber"
  options$inputType <- "cohensD"
  options$mcmcDiagnosticsOverviewTable <- TRUE
  options$mcmcDiagnosticsPlotEffect <- TRUE
  options$mcmcDiagnosticsPlotHeterogeneity <- TRUE
  options$mcmcDiagnosticsPlotPeese <- TRUE
  options$mcmcDiagnosticsPlotPet <- TRUE
  options$mcmcDiagnosticsPlotSingleModel <- TRUE
  options$mcmcDiagnosticsPlotSingleModelNumber <- 12
  options$mcmcDiagnosticsPlotTypeAutocorrelation <- TRUE
  options$mcmcDiagnosticsPlotTypePosteriorSamplesDensity <- TRUE
  options$mcmcDiagnosticsPlotTypeTrace <- TRUE
  options$mcmcDiagnosticsPlotWeights <- TRUE
  options$modelEnsembleType <- "custom"
  options$modelsEffect <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                    mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                    theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                    type = "normal", x0 = "0"))
  options$modelsEffectNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                        mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                        theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                        type = "spike", x0 = "0"))
  options$modelsHeterogeneity <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                           mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                           theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                           type = "invgamma", x0 = "0"))
  options$modelsHeterogeneityNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                               mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                               theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                               type = "spike", x0 = "0"))
  options$modelsPeese <- list()
  options$modelsPeeseNull <- list()
  options$modelsPet <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                 mu = "0", name = "#", nu = "2", priorWeight = "1/2", sigma = "1",
                                 theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                 type = "cauchy", x0 = "0"))
  options$modelsPetNull <- list()
  options$modelsSelectionModels <- list(list(alpha = "(1,1)", name = "#", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05)", priorWeight = "1/4", type = "twoSided"),
                                        list(alpha = "(1,1)", name = "#3", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05)", priorWeight = "1/4", type = "oneSided"))
  options$modelsSelectionModelsNull <- list(list(alpha = "(1,1,1)", name = "#", omega = "(1, 0.5, 0.1)",
                                                 pValues = "(.05, .10)", priorWeight = "1", type = "none"))
  options$pathToFittedModel <- ""
  options$plotsForestPlot <- TRUE
  options$plotsForestPlotOrder <- "alphabetical"
  options$plotsIndividualModelsEffect <- TRUE
  options$plotsIndividualModelsHeterogeneity <- TRUE
  options$plotsIndividualModelsOrder <- "decreasing"
  options$plotsIndividualModelsOrderBy <- "modelNumber"
  options$plotsPooledEstimatesEffect <- TRUE
  options$plotsPooledEstimatesHeterogeneity <- TRUE
  options$plotsPooledEstimatesPetPeese <- TRUE
  options$plotsPooledEstimatesWeightFunction <- TRUE
  options$setSeed <- TRUE
  options$studyLabel <- "author"
  set.seed(1)
  results <- runAnalysis("RobustBayesianMetaAnalysis", "BCG Vaccine.csv", options)


  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("", "", "", 1, "", "Spike(0)", "Spike(0)", "", 63, 0.00538237638788444,
                                        0.126, 2, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)",
                                        "Spike(0)", "", 29, 0.043691001883073, 0.185, 3, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", "", 30, 0.00315666001404022, 0.183,
                                        4, "PET ~ Cauchy(0, 1)[0, Inf]", "Spike(0)", "Spike(0)", "",
                                        61, 0.0233895559142464, 0.128, 5, "", "Spike(0)", "InvGamma(1, 0.15)",
                                        "", 42, 0.0289056409351148, 0.155, 6, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "InvGamma(1, 0.15)", "", 40, 0.0204452360320063,
                                        0.158, 7, "omega[one-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)",
                                        "InvGamma(1, 0.15)", "", 20, 0.0583883943624482, 0.226, 8, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Spike(0)", "InvGamma(1, 0.15)", "", 68, 0.00514090485707322,
                                        0.121, 9, "", "Normal(0, 1)", "Spike(0)", "", 40, 0.0378781552696003,
                                        0.158, 10, "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Normal(0, 1)",
                                        "Spike(0)", "", 55, 0.0281414847286837, 0.135, 11, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", "", 34, 0.015303543540099, 0.172,
                                        12, "PET ~ Cauchy(0, 1)[0, Inf]", "Normal(0, 1)", "Spike(0)",
                                        "", 37, 0.0220738243588192, 0.163, 13, "", "Normal(0, 1)", "InvGamma(1, 0.15)",
                                        "", 68, 0.0190547363349473, 0.122, 14, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", "", 10, 0.0754312469757917,
                                        0.317, 15, "omega[one-sided: .05] ~ CumDirichlet(1, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", "", 24, 0.0736995112533818, 0.205, 16,
                                        "PET ~ Cauchy(0, 1)[0, Inf]", "Normal(0, 1)", "InvGamma(1, 0.15)",
                                        ""))
  })

  test_that("diagnostics-plot-1 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model12"]][["collection"]][["diagnostics_model12_PET"]][["collection"]][["diagnostics_model12_PET_autocor"]][["collection"]][["diagnostics_model12_PET_autocor_autocor1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "diagnostics-plot-1")
  })

  test_that("diagnostics-plot-2 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model12"]][["collection"]][["diagnostics_model12_PET"]][["collection"]][["diagnostics_model12_PET_samples"]][["collection"]][["diagnostics_model12_PET_samples_samples1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "diagnostics-plot-2")
  })

  test_that("diagnostics-plot-3 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model12"]][["collection"]][["diagnostics_model12_PET"]][["collection"]][["diagnostics_model12_PET_trace"]][["collection"]][["diagnostics_model12_PET_trace_trace1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "diagnostics-plot-3")
  })

  test_that("diagnostics-plot-4 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model12"]][["collection"]][["diagnostics_model12_mu"]][["collection"]][["diagnostics_model12_mu_autocor"]][["collection"]][["diagnostics_model12_mu_autocor_autocor1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "diagnostics-plot-4")
  })

  test_that("diagnostics-plot-5 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model12"]][["collection"]][["diagnostics_model12_mu"]][["collection"]][["diagnostics_model12_mu_samples"]][["collection"]][["diagnostics_model12_mu_samples_samples1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "diagnostics-plot-5")
  })

  test_that("diagnostics-plot-6 matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model12"]][["collection"]][["diagnostics_model12_mu"]][["collection"]][["diagnostics_model12_mu_trace"]][["collection"]][["diagnostics_model12_mu_trace_trace1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "diagnostics-plot-6")
  })

  test_that("Model Averaged Effect Size Estimate plot matches", {
    plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "model-averaged-effect-size-estimate")
  })

  test_that("Model Averaged PET-PEESE Regression Estimate plot matches", {
    plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_petPeese"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "model-averaged-pet-peese-regression-estimate")
  })

  test_that("Model Averaged Heterogeneity Estimate plot matches", {
    plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "model-averaged-heterogeneity-estimate")
  })

  test_that("Model Averaged Weight Function Estimate plot matches", {
    plotName <- results[["results"]][["estimatesPlots"]][["collection"]][["estimatesPlots_weightFunction"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "model-averaged-weight-function-estimate")
  })

  test_that("Model Averaged Forest Plot matches", {
    plotName <- results[["results"]][["forestPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "model-averaged-forest-plot")
  })

  test_that("Model Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model12"]][["collection"]][["individualModels_model12_tempCoef"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(100, -0.524780611895799, 0.00352066072606666, 0.1, -0.467864231306738,
                                        -0.465701143736468, "", "Effect size (<unicode>)", -0.402642207650664
                                   ))
  })

  test_that("Information table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model12"]][["collection"]][["individualModels_model12_tempInfo"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(6.44331013829089e-28, -71.2552012405396, 4.29554009219413e-29,
                                        0.0625))
  })

  test_that("PET-PEESE Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model12"]][["collection"]][["individualModels_model12_tempPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(34, 0.00396776278573378, 0.015303543540099, 0.172, 0.106659628330167,
                                        0.082874546662272, "", "PET", 0.296852619199199))
  })

  test_that("Priors table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model12"]][["collection"]][["individualModels_model12_tempPriors"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("PET ~ Cauchy(0, 1)[0, Inf]", "Normal(0, 1)", "Spike(0)"))
  })

  test_that("Model Averaged PET-PEESE Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0, 0.0482944258562693, 0, "PET", 0.593045912942666))
  })

  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-1.0548745672607, -0.682057475942247, -0.691200147024086, "Effect size (<unicode>)",
                                        -0.228462221468931, 0.291391049340735, 0.522350704049743, 0.508932496408766,
                                        "Heterogeneity (<unicode>)", 0.824711235085215))
  })

  test_that("Model Averaged Weights (ω) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.025, 0.576966641336946, 0.025, 0.968777548310648,
                                        1, 1, 0.05, 0.364746575893931, 0.05, 0.92951480902186, 1, 1,
                                        0.975, 0.445930173002541, 0.975, 0.960737260711212, 1, 1, 1
                                   ))
  })

  test_that("Conditional PET-PEESE Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.0425009913889825, 0.406932623261252, 0.297915889499852, "PET",
                                        1.41843340240838))
  })

  test_that("Conditional Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-1.0548745672607, -0.690142316322625, -0.691561070387787, "Effect size (<unicode>)",
                                        -0.310998686162042, 0.291391049340735, 0.522350704049743, 0.508932496408766,
                                        "Heterogeneity (<unicode>)", 0.824711235085215))
  })

  test_that("Conditional Weights (ω) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.025, 0.326482799912229, 0.025, 0.8322063673464,
                                        1, 1, 0.05, 0.178992015083938, 0.05, 0.628077758203661, 0.645423077240284,
                                        0.980104857673904, 0.975, 0.215364818860542, 0.975, 0.795871390857261,
                                        0.984683851998813, 1, 1))
  })

  test_that("Models Overview table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(2.69350595729398e-51, -125.017864237744, 1, 3.84786565327692e-52,
                                        "", "Spike(0)", "Spike(0)", 0.125, 3.11725872531789e-46, -113.460611140016,
                                        2, 1.00556733074772e-47, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.03125, 1.49132625565452e-51, -125.710824778404,
                                        3, 4.8107298569498e-53, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Spike(0)", "Spike(0)", 0.03125, 4.80743188446076e-53, -129.112707940303,
                                        4, 3.20495458964061e-54, "PET ~ Cauchy(0, 1)[0, Inf]", "Spike(0)",
                                        "Spike(0)", 0.0625, 0.0613113427240547, -11.3773792145865, 5,
                                        0.00868271341515478, "", "Spike(0)", "InvGamma(1, 0.15)", 0.125,
                                        0.0543263739561374, -11.5931472558709, 6, 0.00174939791969529,
                                        "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.03125, 0.0119351085758192, -13.1073067156021, 7, 0.000384855331795104,
                                        "omega[one-sided: .05] ~ CumDirichlet(1, 1)", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.03125, 0.0114214673066011, -13.1188827801287, 8, 0.000760851817496161,
                                        "PET ~ Cauchy(0, 1)[0, Inf]", "Spike(0)", "InvGamma(1, 0.15)",
                                        0.0625, 1.0556886276389e-26, -68.389887306145, 9, 1.50812661091273e-27,
                                        "", "Normal(0, 1)", "Spike(0)", 0.125, 1.76275134999201e-26,
                                        -67.9789874266077, 10, 5.68629467739354e-28, "omega[two-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.03125, 9.7210870716576e-27, -68.574150924196,
                                        11, 3.13583453924437e-28, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "Spike(0)", 0.03125, 6.44331013829089e-28, -71.2552012405396,
                                        12, 4.29554009219413e-29, "PET ~ Cauchy(0, 1)[0, Inf]", "Normal(0, 1)",
                                        "Spike(0)", 0.0625, 15.5082193577278, -7.00346840362946, 13,
                                        0.689002497765481, "", "Normal(0, 1)", "InvGamma(1, 0.15)",
                                        0.125, 3.03524725139905, -7.66176744335208, 14, 0.0891795270056185,
                                        "omega[two-sided: .05] ~ CumDirichlet(1, 1)", "Normal(0, 1)",
                                        "InvGamma(1, 0.15)", 0.03125, 3.37163492431177, -7.56649755035549,
                                        15, 0.0980935277514818, "omega[one-sided: .05] ~ CumDirichlet(1, 1)",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.03125, 1.89468158801014,
                                        -8.12575891678406, 16, 0.112146628993278, "PET ~ Cauchy(0, 1)[0, Inf]",
                                        "Normal(0, 1)", "InvGamma(1, 0.15)", 0.0625))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(85.3720571686061, "8/16", 0.988422181515859, 0.5, "Effect", 4.10965389453328e+26,
                                        "8/16", 1, 0.5, "Heterogeneity", 0.433311160928554, "12/16",
                                        0.302314788819365, 0.5, "Publication bias"))
  })

  test_that("Conditional Effect Size Estimates plot matches", {
    plotName <- results[["results"]][["modelsPlots"]][["collection"]][["modelsPlots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "conditional-effect-size-estimates")
  })

  test_that("Conditional Heterogeneity Estimates plot matches", {
    plotName <- results[["results"]][["modelsPlots"]][["collection"]][["modelsPlots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "conditional-heterogeneity-estimates")
  })
}

### more options tested using a pre-loaded model: modify BF type, CI width, model ordering, output scale
{
  options <- analysisOptions("RobustBayesianMetaAnalysis")
  options$advancedAutofitMaximumFittingTime <- FALSE
  options$advancedAutofitMcmcError <- FALSE
  options$advancedAutofitMcmcErrorSd <- FALSE
  options$advancedSaveFittedModel <- ""
  options$bayesFactorType <- "BF01"
  options$effectSizeCi <- list()
  options$inferenceCiWidth <- 0.9
  options$inferenceConditionalParameterEstimates <- TRUE
  options$inferenceIndividualModels <- TRUE
  options$inferenceIndividualModelsSingleModel <- TRUE
  options$inferenceIndividualModelsSingleModelNumber <- 35
  options$inferenceModelsOverview <- TRUE
  options$inferenceModelsOverviewBfComparison <- "best"
  options$inferenceModelsOverviewOrder <- "marginalLikelihood"
  options$inferenceOutputScale <- "correlation"
  options$inferenceShortenPriorName <- TRUE
  options$inputType <- "fittedModel"
  options$mcmcDiagnosticsOverviewTable <- TRUE
  options$modelsEffect <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                    mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                    theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                    type = "normal", x0 = "0"))
  options$modelsEffectNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                        mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                        theta = "1", truncationLower = "-Inf", truncationUpper = "Inf",
                                        type = "spike", x0 = "0"))
  options$modelsHeterogeneity <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                           mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                           theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                           type = "invgamma", x0 = "0"))
  options$modelsHeterogeneityNull <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                               mu = "0", name = "#", nu = "2", priorWeight = "1", sigma = "1",
                                               theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                               type = "spike", x0 = "0"))
  options$modelsPeese <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                   mu = "0", name = "#", nu = "2", priorWeight = "1/4", sigma = "1",
                                   theta = "5", truncationLower = "0", truncationUpper = "Inf",
                                   type = "cauchy", x0 = "0"))
  options$modelsPeeseNull <- list()
  options$modelsPet <- list(list(a = "0", alpha = "1", b = "1", beta = "0.15", k = "1",
                                 mu = "0", name = "#", nu = "2", priorWeight = "1/4", sigma = "1",
                                 theta = "1", truncationLower = "0", truncationUpper = "Inf",
                                 type = "cauchy", x0 = "0"))
  options$modelsPetNull <- list()
  options$modelsSelectionModels <- list(list(alpha = "(1,1)", name = "#", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05)", priorWeight = "1/12", type = "twoSided"),
                                        list(alpha = "(1,1,1)", name = "#2", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "twoSided"),
                                        list(alpha = "(1,1,1)", name = "#3", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#4", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#5", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"),
                                        list(alpha = "(1,1,1)", name = "#6", omega = "(1, 0.5, 0.1)",
                                             pValues = "(.05, .10)", priorWeight = "1/12", type = "oneSided"))
  options$modelsSelectionModelsNull <- list(list(alpha = "(1,1,1)", name = "#", omega = "(1, 0.5, 0.1)",
                                                 pValues = "(.05, .10)", priorWeight = "1", type = "none"))
  options$pathToFittedModel <- pathToFittedModel
  options$plotsForestPlotOrder <- "alphabetical"
  options$plotsIndividualModelsOrder <- "decreasing"
  options$plotsIndividualModelsOrderBy <- "modelNumber"
  options$priorDistributionPlot <- TRUE
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("", "", "", 1, "", "S(0)", "S(0)", "", 4911, 0.00360180257472371,
                                        0.014, 2, "omega[2s: .05] ~ CumD(1, 1)", "S(0)", "S(0)", 1.00059600494088,
                                        4063, 0.00321605245120115, 0.016, 3, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "S(0)", "S(0)", 1.00053765510463, 5188, 0.0034980413194475,
                                        0.014, 4, "omega[1s: .05] ~ CumD(1, 1)", "S(0)", "S(0)", 1.00044862309896,
                                        3724, 0.00351078933688253, 0.016, 5, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "S(0)", "S(0)", 1.00111885217416, 3031, 0.00383661251811952,
                                        0.018, 6, "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "S(0)", "S(0)",
                                        1.00147273402086, 2941, 0.00317634213832056, 0.018, 7, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "S(0)", "S(0)", 1.00130576590478, 6757, 0.00473270314801651,
                                        0.012, 8, "PET ~ C(0, 1)[0, Inf]", "S(0)", "S(0)", 0.999969074811119,
                                        7096, 0.0140111209573542, 0.012, 9, "PEESE ~ C(0, 5)[0, Inf]",
                                        "S(0)", "S(0)", 1.00013380875205, 9190, 0.00168145526453234,
                                        0.01, 10, "", "S(0)", "Ig(1, 0.15)", 1.00053147056833, 5433,
                                        0.00332205802079373, 0.014, 11, "omega[2s: .05] ~ CumD(1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 1.00067715834799, 3978, 0.00318630164931771,
                                        0.016, 12, "omega[2s: .1, .05] ~ CumD(1, 1, 1)", "S(0)", "Ig(1, 0.15)",
                                        1.00069950836796, 4971, 0.0035122877937864, 0.014, 13, "omega[1s: .05] ~ CumD(1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 1.00070305637177, 3798, 0.00339716435400626,
                                        0.016, 14, "omega[1s: .05, .025] ~ CumD(1, 1, 1)", "S(0)", "Ig(1, 0.15)",
                                        1.00073184333587, 3003, 0.00388422808809718, 0.018, 15, "omega[1s: .5, .05] ~ CumD(1, 1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 1.00087835702693, 2805, 0.00326617316388823,
                                        0.019, 16, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)", "S(0)",
                                        "Ig(1, 0.15)", 1.00194305077717, 5281, 0.00596437787167845,
                                        0.014, 17, "PET ~ C(0, 1)[0, Inf]", "S(0)", "Ig(1, 0.15)", 1.00085129362792,
                                        6743, 0.0160448533927911, 0.012, 18, "PEESE ~ C(0, 5)[0, Inf]",
                                        "S(0)", "Ig(1, 0.15)", 1.00078975355455, 8654, 0.00218760970338267,
                                        0.011, 19, "", "N(0, 1)", "S(0)", 1.0003891436628, 4965, 0.00345568667900478,
                                        0.014, 20, "omega[2s: .05] ~ CumD(1, 1)", "N(0, 1)", "S(0)",
                                        1.00019195503165, 4197, 0.00309181634710325, 0.015, 21, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "S(0)", 1.00093082712407, 3962, 0.0038325175160188,
                                        0.016, 22, "omega[1s: .05] ~ CumD(1, 1)", "N(0, 1)", "S(0)",
                                        1.00062167318341, 3919, 0.00336451529081783, 0.016, 23, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "S(0)", 1.00077079999138, 2340, 0.00451158960297927,
                                        0.021, 24, "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "N(0, 1)",
                                        "S(0)", 1.00147185101072, 2135, 0.00390452683695856, 0.022,
                                        25, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)", "N(0, 1)",
                                        "S(0)", 1.00190639912654, 801, 0.0367373993991986, 0.035, 26,
                                        "PET ~ C(0, 1)[0, Inf]", "N(0, 1)", "S(0)", 1.0142617147448,
                                        792, 0.11320471962481, 0.036, 27, "PEESE ~ C(0, 5)[0, Inf]",
                                        "N(0, 1)", "S(0)", 1.00715114503126, 6368, 0.00280196877490588,
                                        0.013, 28, "", "N(0, 1)", "Ig(1, 0.15)", 1.00044730119542, 4985,
                                        0.00334340355142425, 0.014, 29, "omega[2s: .05] ~ CumD(1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 1.00051184744435, 3855, 0.00315062294863966,
                                        0.016, 30, "omega[2s: .1, .05] ~ CumD(1, 1, 1)", "N(0, 1)",
                                        "Ig(1, 0.15)", 1.00024650877671, 4181, 0.00375055626339309,
                                        0.015, 31, "omega[1s: .05] ~ CumD(1, 1)", "N(0, 1)", "Ig(1, 0.15)",
                                        1.0007278979497, 3505, 0.00346994657700325, 0.017, 32, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 1.00134755349406, 2214, 0.00473399074269384,
                                        0.021, 33, "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "N(0, 1)",
                                        "Ig(1, 0.15)", 1.00057755252099, 2367, 0.00483077587707808,
                                        0.021, 34, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)", "N(0, 1)",
                                        "Ig(1, 0.15)", 1.00177311864744, 972, 0.0342809554540649, 0.032,
                                        35, "PET ~ C(0, 1)[0, Inf]", "N(0, 1)", "Ig(1, 0.15)", 1.0011051021958,
                                        1025, 0.100360590076616, 0.031, 36, "PEESE ~ C(0, 5)[0, Inf]",
                                        "N(0, 1)", "Ig(1, 0.15)", 1.00083252601294))
  })

  test_that("Model Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model35"]][["collection"]][["individualModels_model35_tempCoef"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1150, -0.526610784729214, 0.00630577599225466, 0.000331343809352028,
                                        -0.0915887005957453, -0.0569303216543964, 1.0011051021958, "Effect size (<unicode>)",
                                        0.229312554399248, 7636, 0.0184568152888657, 0.000987442460084678,
                                        0.000114434070036032, 0.0916258574029156, 0.0670293462706857,
                                        1.00041315612977, "Heterogeneity (<unicode>)", 0.306552414977409
                                   ))
  })

  test_that("Information table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model35"]][["collection"]][["individualModels_model35_tempInfo"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.27109784953782, 0.0732962792575247, 0.00976524036628595, 0.03125
                                   ))
  })

  test_that("PET-PEESE Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model35"]][["collection"]][["individualModels_model35_tempPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(972, 0.0332494788915203, 0.0342809554540649, 0.032, 1.09825877393861,
                                        0.766133717274056, 1.00104101594135, "PET", 4.04562011137209
                                   ))
  })

  test_that("Priors table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model35"]][["collection"]][["individualModels_model35_tempPriors"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("PET ~ C(0, 1)[0, Inf]", "N(0, 1)", "Ig(1, 0.15)"))
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

  test_that("Conditional PET-PEESE Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.0602938958828328, 0.70813835145645, 0.553691385955032, "PET",
                                        1.8460597976053, 0.46098574680507, 4.92859744853954, 3.92919194395091,
                                        "PEESE", 13.4244104451216))
  })

  test_that("Conditional Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-0.265010879169807, 0.0277741928414064, 0.048990030069296, "Effect size (<unicode>)",
                                        0.240041778743682, 0.0211346062175942, 0.0857211955321524, 0.0639454871337059,
                                        "Heterogeneity (<unicode>)", 0.225458609293803))
  })

  test_that("Conditional Weights (ω) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.025, 0.400731659492476, 0.025, 0.846827532742659,
                                        0.959521222305644, 1, 0.05, 0.189473377407581, 0.05, 0.6015835659595,
                                        0.616424830031175, 0.956666710322452, 0.5, 0.0140399518886046,
                                        0.5, 0.324522264609448, 0.251949727927373, 0.856233509686038,
                                        0.95, 0.0140399518886046, 0.95, 0.342092605382979, 0.262118526705638,
                                        0.896497859022861, 0.975, 0.0140399518886046, 0.975, 0.39992704784217,
                                        0.274954843246093, 1, 1))
  })

  test_that("Models Overview table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 2.8413847019726, 6, 0.0518474271880311, "omega[1s: .5, .05] ~ CumD(1, 1, 1)",
                                        "S(0)", "S(0)", 0.0104166666666667, 1.06816097556242, 2.77544624661061,
                                        7, 0.048538964045875, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "S(0)", "S(0)", 0.0104166666666667, 1.80092439405714, 2.25308461663934,
                                        15, 0.0287893413844146, "omega[1s: .5, .05] ~ CumD(1, 1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 0.0104166666666667, 2.00910649373153,
                                        2.14369460921641, 16, 0.0258062115421938, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 0.0104166666666667, 2.50712025761697,
                                        1.92224991521258, 1, 0.248160862793135, "", "S(0)", "S(0)",
                                        0.125, 3.07736888727077, 1.71730972745251, 8, 0.0505439182827506,
                                        "PET ~ C(0, 1)[0, Inf]", "S(0)", "S(0)", 0.03125, 3.3417593712443,
                                        1.63488727581464, 2, 0.0155150091398489, "omega[2s: .05] ~ CumD(1, 1)",
                                        "S(0)", "S(0)", 0.0104166666666667, 3.38491773214997, 1.62205509955216,
                                        4, 0.0153171897489809, "omega[1s: .05] ~ CumD(1, 1)", "S(0)",
                                        "S(0)", 0.0104166666666667, 3.79130744765029, 1.50867376937297,
                                        5, 0.0136753423202764, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "S(0)", "S(0)", 0.0104166666666667, 4.00354260380006, 1.45420508186009,
                                        24, 0.0129503872742153, "omega[1s: .5, .05] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "S(0)", 0.0104166666666667, 4.26572736472134, 1.3907719928449,
                                        10, 0.145852998342527, "", "S(0)", "Ig(1, 0.15)", 0.125, 4.32649835004851,
                                        1.37662618226743, 9, 0.0359510784425352, "PEESE ~ C(0, 5)[0, Inf]",
                                        "S(0)", "S(0)", 0.03125, 4.5013965567827, 1.33699700739196,
                                        25, 0.0115180758980027, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "N(0, 1)", "S(0)", 0.0104166666666667, 4.70467692661148, 1.29282759726281,
                                        3, 0.0110204011873295, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "S(0)", "S(0)", 0.0104166666666667, 5.40076635246781, 1.15484384134799,
                                        17, 0.0288000389968768, "PET ~ C(0, 1)[0, Inf]", "S(0)", "Ig(1, 0.15)",
                                        0.03125, 6.12264111397921, 1.02939114396949, 13, 0.00846814736040188,
                                        "omega[1s: .05] ~ CumD(1, 1)", "S(0)", "Ig(1, 0.15)", 0.0104166666666667,
                                        6.26303437139466, 1.0067199104363, 33, 0.00827832391034533,
                                        "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "N(0, 1)", "Ig(1, 0.15)",
                                        0.0104166666666667, 6.30867742230102, 0.99945864765794, 11,
                                        0.00821843053898297, "omega[2s: .05] ~ CumD(1, 1)", "S(0)",
                                        "Ig(1, 0.15)", 0.0104166666666667, 7.20554706543184, 0.866533546821751,
                                        34, 0.0071954879646496, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 0.0104166666666667, 7.22906747566094,
                                        0.863274653973805, 14, 0.00717207680832869, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 0.0104166666666667, 7.52446982924558,
                                        0.82322434840501, 18, 0.0206715270436121, "PEESE ~ C(0, 5)[0, Inf]",
                                        "S(0)", "Ig(1, 0.15)", 0.03125, 8.302033497307, 0.724884217502028,
                                        19, 0.0749417749829835, "", "N(0, 1)", "S(0)", 0.125, 9.25106114532623,
                                        0.616646438614277, 12, 0.00560448432601974, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 0.0104166666666667, 9.83553208045272,
                                        0.555383150888035, 26, 0.0158143230373088, "PET ~ C(0, 1)[0, Inf]",
                                        "N(0, 1)", "S(0)", 0.03125, 11.2566700621459, 0.420423854601659,
                                        27, 0.0138177880941144, "PEESE ~ C(0, 5)[0, Inf]", "N(0, 1)",
                                        "S(0)", 0.03125, 13.0092099049663, 0.275727141118125, 20, 0.00398544012793877,
                                        "omega[2s: .05] ~ CumD(1, 1)", "N(0, 1)", "S(0)", 0.0104166666666667,
                                        13.4582921886162, 0.241789266295172, 28, 0.0462294262553343,
                                        "", "N(0, 1)", "Ig(1, 0.15)", 0.125, 13.6509095198572, 0.227578551070042,
                                        22, 0.00379809324152442, "omega[1s: .05] ~ CumD(1, 1)", "N(0, 1)",
                                        "S(0)", 0.0104166666666667, 15.9281569863959, 0.0732962792575247,
                                        35, 0.00976524036628595, "PET ~ C(0, 1)[0, Inf]", "N(0, 1)",
                                        "Ig(1, 0.15)", 0.03125, 17.168118259335, -0.00166937225030006,
                                        23, 0.00301998311083624, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "S(0)", 0.0104166666666667, 18.1277601996255, -0.0560597740495541,
                                        36, 0.0085803364481458, "PEESE ~ C(0, 5)[0, Inf]", "N(0, 1)",
                                        "Ig(1, 0.15)", 0.03125, 19.9984884552558, -0.154271991488075,
                                        21, 0.00259256729847525, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "S(0)", 0.0104166666666667, 23.2809699992173, -0.306251586655649,
                                        29, 0.00222703036814077, "omega[2s: .05] ~ CumD(1, 1)", "N(0, 1)",
                                        "Ig(1, 0.15)", 0.0104166666666667, 23.3345035673204, -0.308548403036193,
                                        31, 0.00222192115801608, "omega[1s: .05] ~ CumD(1, 1)", "N(0, 1)",
                                        "Ig(1, 0.15)", 0.0104166666666667, 30.0273502277736, -0.5607239386261,
                                        32, 0.00172667340923327, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 0.0104166666666667, 37.4707436179945,
                                        -0.782175756325808, 30, 0.00138367756232979, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 0.0104166666666667))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.34694629322749, "18/36", 0.23004655050788, 0.5, "Effect", 1.72485968725674,
                                        "18/36", 0.366991373785838, 0.5, "Heterogeneity", 1.06264271661404,
                                        "32/36", 0.484814937626021, 0.5, "Publication bias"))
  })

  test_that("prior-distribution-2-plot-13 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-13")
  })

  test_that("prior-distribution-2-plot-14 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-14")
  })

  test_that("prior-distribution-2-plot-15 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative4"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-15")
  })

  test_that("prior-distribution-2-plot-16 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative5"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-16")
  })

  test_that("prior-distribution-2-plot-17 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative6"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-17")
  })

  test_that("prior-distribution-2-plot-18 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative7"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-18")
  })

  test_that("prior-distribution-2-plot-19 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative8"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-19")
  })

  test_that("prior-distribution-2-plot-20 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_bias"]][["collection"]][["priorPlots_bias_alternative"]][["collection"]][["priorPlots_bias_alternative_biasalternative9"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-20")
  })

  test_that("prior-distribution-2-plot-21 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-21")
  })

  test_that("prior-distribution-2-plot-22 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_effect"]][["collection"]][["priorPlots_effect_alternative"]][["collection"]][["priorPlots_effect_alternative_effectalternative2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-22")
  })

  test_that("prior-distribution-2-plot-23 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_heterogeneity"]][["collection"]][["priorPlots_heterogeneity_alternative"]][["collection"]][["priorPlots_heterogeneity_alternative_heterogeneityalternative1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-23")
  })

  test_that("prior-distribution-2-plot-24 matches", {
    plotName <- results[["results"]][["priorPlots"]][["collection"]][["priorPlots_heterogeneity"]][["collection"]][["priorPlots_heterogeneity_alternative"]][["collection"]][["priorPlots_heterogeneity_alternative_heterogeneityalternative2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "prior-distribution-2-plot-24")
  })
}
