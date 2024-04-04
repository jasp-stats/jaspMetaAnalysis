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
  options <- initOpts("RobustBayesianMetaAnalysis")
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
  options <- initOpts("RobustBayesianMetaAnalysis")
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
  options <- initOpts("RobustBayesianMetaAnalysis")
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
  options <- initOpts("RobustBayesianMetaAnalysis")
  options$advancedAutofitMaximumFittingTime <- FALSE
  options$advancedAutofitMcmcError <- FALSE
  options$advancedAutofitMcmcErrorSd <- FALSE
  options$advancedMcmcAdaptation <- 100
  options$advancedMcmcBurnin <- 100
  options$advancedMcmcThin <- 1
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
  options$seed <- 1
  options$studyLabel <- "author"
  set.seed(1)
  options$advancedEstimationScale <- "fishersZ"
  options$advancedAutofitRHat <- FALSE
  options$advancedAutofitEss <- FALSE
  options$advancedAutofitMcmcError <- FALSE
  options$advancedAutofitMcmcErrorSd <- FALSE
  options$advancedAutofitMaximumFittingTime <- FALSE
  options$advancedAutofitExtendSamples <- 100
  options$advancedRemoveFailedModels <- FALSE
  options$advancedRemoveFailedModelsRHat <- FALSE
  options$advancedRemoveFailedModelsEss <- FALSE
  options$advancedRemoveFailedModelsMcmcError <- FALSE
  options$advancedRemoveFailedModelsMcmcErrorSd <- FALSE
  options$advancedRebalanceComponentProbabilityOnModelFailure <- FALSE

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
  options$priorDistributionPlot <- TRUE
  options$inputType <- "fittedModel"
  options$mcmcDiagnosticsOverviewTable <- TRUE
  options$mcmcDiagnosticsPlotEffect <- FALSE
  options$mcmcDiagnosticsPlotHeterogeneity <- FALSE
  options$mcmcDiagnosticsPlotWeights <- FALSE
  options$mcmcDiagnosticsPlotPet <- FALSE
  options$mcmcDiagnosticsPlotPeese <- FALSE
  options$mcmcDiagnosticsPlotTypeTrace <- FALSE
  options$mcmcDiagnosticsPlotTypeAutocorrelation <- FALSE
  options$mcmcDiagnosticsPlotTypePosteriorSamplesDensity <- FALSE
  options$mcmcDiagnosticsPlotSingleModel <- FALSE
  options$mcmcDiagnosticsPlotSingleModelNumber <- 1
  options$inferenceConditionalParameterEstimates <- TRUE
  options$inferenceModelsOverview <- TRUE
  options$inferenceIndividualModels <- TRUE
  options$inferenceIndividualModelsSingleModel <- TRUE
  options$inferenceIndividualModelsSingleModelNumber <- 35
  options$inferenceCiWidth <- 0.9
  options$inferenceOutputScale <- "correlation"
  options$inferenceShortenPriorName <- TRUE
  options$inferenceModelsOverviewBfComparison <- "best"
  options$inferenceModelsOverviewOrder <- "marginalLikelihood"
  options$bayesFactorType <- "BF01"
  options$plotsForestPlot <- FALSE
  options$plotsPooledEstimatesEffect <- FALSE
  options$plotsPooledEstimatesHeterogeneity <- FALSE
  options$plotsPooledEstimatesWeightFunction <- FALSE
  options$plotsPooledEstimatesWeightFunctionRescaleXAxis <- TRUE
  options$plotsPooledEstimatesPetPeese <- FALSE
  options$plotsPooledEstimatesPriorDistribution <- TRUE
  options$plotsIndividualModelsEffect <- FALSE
  options$plotsIndividualModelsHeterogeneity <- FALSE
  options$plotsIndividualModelsShowBayesianUpdating <- TRUE
  options$plotsIndividualModelsShowPosteriorEstimates <- FALSE
  options$plotsForestPlotOrder <- "alphabetical"
  options$plotsForestPlotType <- "averaged"
  options$plotsPooledEstimatesType <- "averaged"
  options$plotsIndividualModelsType <- "conditional"
  options$plotsIndividualModelsOrder <- "decreasing"
  options$plotsIndividualModelsOrderBy <- "modelNumber"
  options$advancedAutofitMaximumFittingTime <- FALSE
  options$advancedAutofitMcmcError <- FALSE
  options$advancedAutofitMcmcErrorSd <- FALSE
  options$advancedSaveFittedModel <- ""
  options$effectSizeCi <- list()
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
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("RobustBayesianMetaAnalysis", dataset, options)


  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_diagosticsTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list("", "S(0)", "S(0)", "", "", "", 1, "", "omega[2s: .05] ~ CumD(1, 1)",
                                        "S(0)", "S(0)", 4983, 0.00355944122091695, 0.014, 2, 0.999916790572259,
                                        "omega[2s: .1, .05] ~ CumD(1, 1, 1)", "S(0)", "S(0)", 4151,
                                        0.00322606923876815, 0.016, 3, 1.00046521940045, "omega[1s: .05] ~ CumD(1, 1)",
                                        "S(0)", "S(0)", 4802, 0.00363147139859074, 0.014, 4, 1.00047411817135,
                                        "omega[1s: .05, .025] ~ CumD(1, 1, 1)", "S(0)", "S(0)", 4096,
                                        0.00340225446434084, 0.016, 5, 1.00187861133097, "omega[1s: .5, .05] ~ CumD(1, 1, 1)",
                                        "S(0)", "S(0)", 3101, 0.00368967330590986, 0.018, 6, 1.00036745111215,
                                        "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)", "S(0)", "S(0)",
                                        2756, 0.00337001703752426, 0.019, 7, 1.00020934596346, "PET ~ C(0, 1)[0, Inf]",
                                        "S(0)", "S(0)", 6461, 0.00474833650429599, 0.012, 8, 1.00154401824817,
                                        "PEESE ~ C(0, 5)[0, Inf]", "S(0)", "S(0)", 7318, 0.0137206675032639,
                                        0.012, 9, 1.00109894587153, "", "S(0)", "Ig(1, 0.15)", 10928,
                                        0.0016448306073844, 0.01, 10, 1.00090428789057, "omega[2s: .05] ~ CumD(1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 5256, 0.00339341924752772, 0.014, 11,
                                        1.00112874700509, "omega[2s: .1, .05] ~ CumD(1, 1, 1)", "S(0)",
                                        "Ig(1, 0.15)", 4052, 0.00317548682591956, 0.016, 12, 1.00159999403619,
                                        "omega[1s: .05] ~ CumD(1, 1)", "S(0)", "Ig(1, 0.15)", 4817,
                                        0.00353887616927372, 0.014, 13, 1.00026337361816, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 3963, 0.00334933775005932, 0.016, 14,
                                        1.00258498117824, "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "S(0)",
                                        "Ig(1, 0.15)", 2747, 0.00414045669870134, 0.019, 15, 1.00076351281256,
                                        "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)", "S(0)", "Ig(1, 0.15)",
                                        2973, 0.00320382708866251, 0.018, 16, 1.00103887260189, "PET ~ C(0, 1)[0, Inf]",
                                        "S(0)", "Ig(1, 0.15)", 5787, 0.00565835637062769, 0.013, 17,
                                        1.00097825112801, "PEESE ~ C(0, 5)[0, Inf]", "S(0)", "Ig(1, 0.15)",
                                        5762, 0.017671840230274, 0.013, 18, 1.00199607285693, "", "N(0, 1)",
                                        "S(0)", 9651, 0.00203739360815102, 0.01, 19, 1.00052279227899,
                                        "omega[2s: .05] ~ CumD(1, 1)", "N(0, 1)", "S(0)", 5076, 0.00339910330368114,
                                        0.014, 20, 1.00097245258785, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "S(0)", 3808, 0.0032464328134086, 0.016, 21, 1.00025986588776,
                                        "omega[1s: .05] ~ CumD(1, 1)", "N(0, 1)", "S(0)", 4815, 0.00352051687342411,
                                        0.014, 22, 1.00059994323547, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "S(0)", 4169, 0.00324667292232271, 0.015, 23, 1.00056521547684,
                                        "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "N(0, 1)", "S(0)", 2738,
                                        0.0041906561460695, 0.019, 24, 1.00040481036697, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "N(0, 1)", "S(0)", 2176, 0.00378017249695577, 0.021, 25, 1.00065546117269,
                                        "PET ~ C(0, 1)[0, Inf]", "N(0, 1)", "S(0)", 732, 0.039597540511154,
                                        0.037, 26, 1.00054016495422, "PEESE ~ C(0, 5)[0, Inf]", "N(0, 1)",
                                        "S(0)", 701, 0.134405800144465, 0.038, 27, 1.00815277085521,
                                        "", "N(0, 1)", "Ig(1, 0.15)", 6114, 0.00273398448077168, 0.013,
                                        28, 1.00058770462662, "omega[2s: .05] ~ CumD(1, 1)", "N(0, 1)",
                                        "Ig(1, 0.15)", 4530, 0.00349530712932726, 0.015, 29, 1.00088387197696,
                                        "omega[2s: .1, .05] ~ CumD(1, 1, 1)", "N(0, 1)", "Ig(1, 0.15)",
                                        3955, 0.00311147304406426, 0.016, 30, 1.00059477679424, "omega[1s: .05] ~ CumD(1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 4535, 0.00355828830697028, 0.015,
                                        31, 1.00064539216412, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 3315, 0.00356388818544122, 0.017,
                                        32, 1.00061800318753, "omega[1s: .5, .05] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 2452, 0.00488092334771977, 0.02, 33,
                                        1.00032433531313, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 2217, 0.00496877406646441, 0.021,
                                        34, 1.00196468669491, "PET ~ C(0, 1)[0, Inf]", "N(0, 1)", "Ig(1, 0.15)",
                                        814, 0.0386256439938968, 0.035, 35, 1.00263981196894, "PEESE ~ C(0, 5)[0, Inf]",
                                        "N(0, 1)", "Ig(1, 0.15)", 1052, 0.0983074439360106, 0.031, 36,
                                        1.00304101631523))
  })

  test_that("Model Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model35"]][["collection"]][["individualModels_model35_tempCoef"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(935, -1.28683914919869, 0.0144750153420371, 0.033, -0.189794400938439,
                                        -0.111263606819964, 1.00263981196894, "Effect size (<unicode>)",
                                        0.47082188776779, 7267, 0.0356417879003374, 0.00219617595784277,
                                        0.012, 0.186549304136102, 0.13405222874938, 1.00018612983625,
                                        "Heterogeneity (<unicode>)", 0.637647844609294))
  })

  test_that("Information table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model35"]][["collection"]][["individualModels_model35_tempInfo"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.22564143137825, 0.0839530963349834, 0.0099014916073319, 0.03125
                                   ))
  })

  test_that("PET-PEESE Estimates table results match", {
    table <- results[["results"]][["individualModels"]][["collection"]][["individualModels_model35"]][["collection"]][["individualModels_model35_tempPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(814, 0.0351076634485686, 0.0386256439938968, 0.035, 1.10924884610786,
                                        0.76263201645664, 1.00115809737675, "PET", 4.18448167221435
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
                                   list(0, 0.0552551594164715, 0, "PET", 0.46030695675256, 0, 0.172644259842497,
                                        0, "PEESE", 0.958661587395958))
  })

  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-0.0131365202182923, 0.0299433502107886, 0, "Effect size (<unicode>)",
                                        0.328154435799253, 0, 0.0655792464205272, 0, "Heterogeneity (<unicode>)",
                                        0.358944254383455))
  })

  test_that("Model Averaged Weights (ω) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_averagedWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.025, 0.612481261756634, 0.025, 0.94350785294243,
                                        1, 1, 0.05, 0.394993075686365, 0.05, 0.875166715403702, 1, 1,
                                        0.5, 0.0615525480453087, 0.5, 0.783914396825957, 1, 1, 0.95,
                                        0.0615525480453087, 0.95, 0.792759255847103, 1, 1, 0.975, 0.0615525480453087,
                                        0.975, 0.823940963495895, 1, 1, 1))
  })

  test_that("Conditional PET-PEESE Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalPetPeese"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(0.0623484887052414, 0.708724344892466, 0.550156072008637, "PET",
                                        1.81105831223164, 0.233765261326344, 2.48435644042878, 1.92986142512675,
                                        "PEESE", 6.87028575755263))
  })

  test_that("Conditional Estimates table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(-0.55797758951788, 0.053100484224657, 0.100859485439393, "Effect size (<unicode>)",
                                        0.489156100307947, 0.0425061789955954, 0.171999385389545, 0.128412494292397,
                                        "Heterogeneity (<unicode>)", 0.441722850161361))
  })

  test_that("Conditional Weights (ω) table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_conditionalWeights"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 0, 1, 1, 1, 0.025, 0.391946538865097, 0.025, 0.846073734097024,
                                        0.958547888464143, 1, 0.05, 0.192252055973059, 0.05, 0.603912824053928,
                                        0.619961549622256, 0.958694417460028, 0.5, 0.0136108833815115,
                                        0.5, 0.328315394025579, 0.260476334829904, 0.855050848122679,
                                        0.95, 0.0136108833815115, 0.95, 0.345801478640319, 0.27253551977268,
                                        0.895700260880139, 0.975, 0.0136108833815115, 0.975, 0.40404429758613,
                                        0.288097185522883, 1, 1))
  })

  test_that("Models Overview table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_modelsSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "S(0)", "S(0)", 2.8408614327177,
                                        6, 0.0519863643893533, 0.0104166666666667, 1.08824313712092,
                                        "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)", "S(0)", "S(0)",
                                        2.75629683763152, 7, 0.047770909474228, 0.0104166666666667,
                                        1.78568536189166, "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "S(0)",
                                        "Ig(1, 0.15)", 2.26105913493661, 15, 0.029112835608554, 0.0104166666666667,
                                        2.02993132881362, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 2.13285946840682, 16, 0.0256099128337195,
                                        0.0104166666666667, 2.50580870184615, "", "S(0)", "S(0)", 1.92224991521258,
                                        1, 0.248956104355703, 0.125, 3.1082956161427, "PET ~ C(0, 1)[0, Inf]",
                                        "S(0)", "S(0)", 1.70678689014638, 8, 0.0501751160211718, 0.03125,
                                        3.38076471302134, "omega[1s: .05] ~ CumD(1, 1)", "S(0)", "S(0)",
                                        1.62275950235663, 4, 0.0153771021654133, 0.0104166666666667,
                                        3.39661787878029, "omega[2s: .05] ~ CumD(1, 1)", "S(0)", "S(0)",
                                        1.6180812377145, 2, 0.0153053320228124, 0.0104166666666667,
                                        3.807225235282, "omega[1s: .05, .025] ~ CumD(1, 1, 1)", "S(0)",
                                        "S(0)", 1.50396079361012, 5, 0.0136546595424903, 0.0104166666666667,
                                        4.05930781149469, "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "N(0, 1)",
                                        "S(0)", 1.43984896341873, 24, 0.0128067066612057, 0.0104166666666667,
                                        4.25832276608375, "", "S(0)", "Ig(1, 0.15)", 1.39198606684501,
                                        10, 0.146498141860196, 0.125, 4.35067108669517, "PEESE ~ C(0, 5)[0, Inf]",
                                        "S(0)", "S(0)", 1.37053132671245, 9, 0.0358471348581141, 0.03125,
                                        4.4975149715096, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "N(0, 1)", "S(0)", 1.3373364170292, 25, 0.0115589085792201,
                                        0.0104166666666667, 4.74828284275629, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "S(0)", "S(0)", 1.28307838681863, 3, 0.0109484557072376, 0.0104166666666667,
                                        5.47548373975845, "PET ~ C(0, 1)[0, Inf]", "S(0)", "Ig(1, 0.15)",
                                        1.14058080662437, 17, 0.0284831625077459, 0.03125, 6.13570084615507,
                                        "omega[1s: .05] ~ CumD(1, 1)", "S(0)", "Ig(1, 0.15)", 1.02673712372733,
                                        13, 0.00847276712030876, 0.0104166666666667, 6.24972787740819,
                                        "omega[1s: .5, .05] ~ CumD(1, 1, 1)", "N(0, 1)", "Ig(1, 0.15)",
                                        1.00832350953196, 33, 0.00831818047266922, 0.0104166666666667,
                                        6.32567231561173, "omega[2s: .05] ~ CumD(1, 1)", "S(0)", "Ig(1, 0.15)",
                                        0.996245108794652, 11, 0.00821831448035194, 0.0104166666666667,
                                        7.21964684970857, "omega[1s: .05, .025] ~ CumD(1, 1, 1)", "S(0)",
                                        "Ig(1, 0.15)", 0.864055393791984, 14, 0.00720067968303072, 0.0104166666666667,
                                        7.23761445017748, "omega[1s: .5, .05, .025] ~ CumD(1, 1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", 0.861569776447596, 34, 0.0071828037742572,
                                        0.0104166666666667, 7.58297321497732, "PEESE ~ C(0, 5)[0, Inf]",
                                        "S(0)", "Ig(1, 0.15)", 0.814956065251309, 18, 0.0205670109529097,
                                        0.03125, 8.29789587668239, "", "N(0, 1)", "S(0)", 0.724859458887395,
                                        19, 0.0751800675669189, 0.125, 9.37311468604769, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "S(0)", "Ig(1, 0.15)", 0.60301598123949, 12, 0.00554632756886432,
                                        0.0104166666666667, 9.85522268805464, "PET ~ C(0, 1)[0, Inf]",
                                        "N(0, 1)", "S(0)", 0.552859895914268, 26, 0.0158250196981439,
                                        0.03125, 11.3777335243237, "PEESE ~ C(0, 5)[0, Inf]", "N(0, 1)",
                                        "S(0)", 0.409203186919818, 27, 0.0137073954873917, 0.03125,
                                        13.0701779249241, "omega[2s: .05] ~ CumD(1, 1)", "N(0, 1)",
                                        "S(0)", 0.270528291943619, 20, 0.00397747947181485, 0.0104166666666667,
                                        13.4801067509138, "", "N(0, 1)", "Ig(1, 0.15)", 0.239646408057645,
                                        28, 0.0462782961737266, 0.125, 13.73347987317, "omega[1s: .05] ~ CumD(1, 1)",
                                        "N(0, 1)", "S(0)", 0.2210247947108, 22, 0.00378537449134903,
                                        0.0104166666666667, 15.7510705813834, "PET ~ C(0, 1)[0, Inf]",
                                        "N(0, 1)", "Ig(1, 0.15)", 0.0839530963349834, 35, 0.0099014916073319,
                                        0.03125, 16.960612810876, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "S(0)", 0.00996767028997336, 23, 0.00306512299815117,
                                        0.0104166666666667, 18.3731088868427, "PEESE ~ C(0, 5)[0, Inf]",
                                        "N(0, 1)", "Ig(1, 0.15)", -0.0700266893619579, 36, 0.00848844330747667,
                                        0.03125, 19.8709849132079, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "S(0)", -0.148399190469005, 21, 0.0026161946484494,
                                        0.0104166666666667, 23.1107986602793, "omega[2s: .05] ~ CumD(1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", -0.299438550048859, 29, 0.00224944040894193,
                                        0.0104166666666667, 23.5141289709662, "omega[1s: .05] ~ CumD(1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", -0.31674004057145, 31, 0.00221085647924883,
                                        0.0104166666666667, 30.1446122263816, "omega[1s: .05, .025] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", -0.565144775527778, 32, 0.00172456570344788,
                                        0.0104166666666667, 37.3111095882058, "omega[2s: .1, .05] ~ CumD(1, 1, 1)",
                                        "N(0, 1)", "Ig(1, 0.15)", -0.778429693773115, 30, 0.00139332131805017,
                                        0.0104166666666667))
  })

  test_that("Model Summary table results match", {
    table <- results[["results"]][["mainSummary"]][["collection"]][["mainSummary_overallSummary"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(3.34273434709712, "18/36", 0.230269668847795, 0.5, "Effect", 1.72141017743707,
                                        "18/36", 0.367456551860831, 0.5, "Heterogeneity", 1.07001884257431,
                                        "32/36", 0.483087390043455, 0.5, "Publication bias"))
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
