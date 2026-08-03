#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Main function ----

BayesianMetaAnalysisDeprecated <- function(jaspResults, dataset, ready, options) {

  # the module here is remnant of the analysis being imported into Cochrane
  options[["analysis"]] <- "bmaDeprecated"

  # Ready: variables needed for the analysis (confidence interval missing)
  ready <- options[["effectSize"]] != "" && (options[["effectSizeSe"]] != "" || (all(unlist(options$effectSizeCi) != "") && !is.null(unlist(options[["effectSizeCi"]]))))

  # Dependencies: basically everything
  # dependencies <- .bmaDependencies

  # Table: Posterior Model Estimates
  .bmaMainTable(jaspResults, dataset, options, ready, .bmaDependencies)

  # Table: Model Probabilities
  if(options$modelProbability){
    .bmaPostModelTable(jaspResults, dataset, options, ready, .bmaDependencies)
  }

  # Table: Effect Sizes per Study
  if(options$effectSizePerStudy){
    .bmaEffectSizeTable(jaspResults, dataset, options, ready, .bmaDependencies)
  }

  # Plot: Prior(s); only when checked
  if(options$priorPlot){
    .bmaPriorPlot(jaspResults, dataset, options, ready)
  }

  # Plot: Prior(s) and Posterior(s); only when checked
  if(options$priorPosterior){
    .bmaPriorAndPosteriorPlot(jaspResults, dataset, options, ready, .bmaDependencies)
  }

  # Plot: Forest plot; only when checked
  if(options$forestPlot || options$cumulativeForestPlot){
    .bmaForestPlot(jaspResults, dataset, options, ready, .bmaDependencies)
  }

  # Plot: Cumulative forest plot and sequential; only when checked
  if(options$bfSequentialPlot || options$modelProbabilitySequentialPlot){
    .bmaSequentialPlot(jaspResults, dataset, options, ready, .bmaDependencies)
  }
}

.bmaDependencies <- c(
  "effectSize", "effectSizeSe", "effectSizeCi", "model",
  "positive", "negative",
  "priorModelProbabilityFixedNull", "priorModelProbabilityFixedAlternative",
  "priorModelProbabilityRandomNull", "priorModelProbabilityRandomAlternative",
  "priorEffectSize", "cauchyLocation", "cauchyScale",
  "truncationLowerBound", "truncationUpperBound",
  "truncationLowerBoundValue", "truncationUpperBoundValue",
  "normalMean", "normalSd",
  "tLocation", "tScale", "tDf",
  "priorStandardError", "inverseGammaShape", "inverseGammaScale",
  "halfTScale", "halfTDf",
  "bayesFactorComputation", "bridgeSamplingSamples", "samples",
  "chains", "seed", "setSeed"
)

# Save priors for later use (without data)
.bmaPriors <- function(jaspResults, options) {
  if (!is.null(jaspResults[["bmaPriors"]])) return(jaspResults[["bmaPriors"]]$object)

  # Effect size prior parameters
  # Lower and upper limits without truncation
  lowerES <- -Inf
  upperES <- Inf

  # prior distribution
  if(options$priorEffectSize == "cauchy"){
    familyES <- "t"
    paramES <- c(options$cauchyLocation,
                 options$cauchyScale, 1)
  } else if(options$priorEffectSize == "normal"){
    familyES <- "norm"
    paramES <- c(options$normalMean,
                 options$normalSd)
  } else if(options$priorEffectSize == "t"){
    familyES <- "t"
    paramES <- c(options$tLocation,
                 options$tScale,
                 options$tDf)
  }

  # If truncated is checked
  if(options$truncationLowerBound){
    lowerES <- options$truncationLowerBoundValue
  }
  if(options$truncationUpperBound){
    upperES <- options$truncationUpperBoundValue
  }

  if (lowerES >= upperES)
    .quitAnalysis(gettext("The prior lower bound is not smaller than the upper bound."))

  # Heterogeneity prior parameters
  # Inverse gamma prior
  if(options$priorStandardError == "inverseGamma"){
    familySE <- "invgamma"
    paramSE <- c(options$inverseGammaShape,
                 options$inverseGammaScale)
  }
  # Half t prior
  if(options$priorStandardError == "halfT"){
    familySE <- "t"
    paramSE <- c(0, # location is always zero
                 options$halfTScale,
                 options$halfTDf)
  }

  # Make priors (probability density functions)
  d <- metaBMA::prior(familyES, paramES, lowerES, upperES)
  tau <- metaBMA::prior(familySE, paramSE, 0)

  if(options[["model"]] == "constrainedRandom" && options$constrainedRandomDirection == "positive"){
    x <- seq(-1, -1e-05, 0.001)
    if(any(d(x) > 0))
      .quitAnalysis(gettext("Your prior contains negative values."))
  }
  if(options[["model"]] == "constrainedRandom" && options$constrainedRandomDirection == "negative"){
    x <- seq(1e-05, 1, 0.001)
    if(any(d(x) > 0))
      .quitAnalysis(gettext("Your prior contains positive values."))
  }


  # Save priors
  jaspResults[["bmaPriors"]] <- createJaspState(
    object=list(d = d, tau = tau),
    dependencies=c("priorEffectSize",
                   "cauchy", "cauchyLocation", "cauchyScale",
                   "truncationLowerBound", "truncationLowerBoundValue",
                   "truncationUpperBound", "truncationUpperBoundValue",
                   "normal", "normalMean", "normalSd",
                   "t", "tLocation", "tScale","tDf",
                   "priorStandardError", "inverseGamma",
                   "inverseGammaShape", "inverseGammaScale",
                   "halfT", "halfTScale", "halfTDf"))

  return(jaspResults[["bmaPriors"]]$object)
}

#For state
.bmaResultsState <- function(jaspResults, dataset, options, .bmaDependencies) {

  if(!is.null(jaspResults[["bmaResults"]])) return(jaspResults[["bmaResults"]]$object)

  results <- .bmaResults(jaspResults, dataset, options)

  # The results object is too large for .jasp files. Break it up and reassemble only the required components.
  bmaResults <- list()

  # Averaged model
  bma                       <- list()
  bma[["estimates"]]        <- results$estimates
  if(options[["model"]] != "constrainedRandom")
    anchorPoint             <- results$estimates["averaged", 1]
  if(options[["model"]] == "constrainedRandom")
    anchorPoint             <- results$estimates["ordered", 1]
  bma[["xPost"]]            <- seq(anchorPoint - 2, anchorPoint + 2, .001)
  bma[["yPost"]]            <- results$posterior_d(bma[["xPost"]])
  bma[["yPrior"]]           <- results$meta$fixed$prior_d(bma[["xPost"]])
  bma[["dfPointsY"]]        <- data.frame(prior = results$meta$fixed$prior_d(0),
                                          posterior = results$posterior_d(0))
  bmaResults[["bma"]]       <- bma

  # Prior and posterior models
  models                    <- list()
  models[["prior"]]         <- results$prior_models
  models[["posterior"]]     <- results$posterior_models
  bmaResults[["models"]]    <- models

  # Bayes factors
  bf <- list()
  bf[["BF"]]                <- results$BF
  bf[["inclusionBF"]]       <- results$inclusion$incl.BF
  bf[["fixedBF"]]           <- results$meta$fixed$BF
  bf[["randomBF"]]          <- results$meta$random$BF
  bmaResults[["bf"]]        <- bf

  # Fixed effects model
  fixed <- list()
  fixed[["estimates"]]      <- results$meta$fixed$estimates
  ## Prior and posterior - effect size
  anchorPoint               <- results$meta$fixed$estimates["d", 1]
  fixed[["xPost"]]          <- seq(anchorPoint - 2, anchorPoint + 2, .001)
  fixed[["yPost"]]          <- results$meta$fixed$posterior_d(fixed[["xPost"]])
  fixed[["yPrior"]]         <- results$meta$fixed$prior_d(fixed[["xPost"]])
  fixed[["dfPointsY"]]      <- data.frame(prior = results$meta$fixed$prior_d(0),
                                          posterior = results$meta$fixed$posterior_d(0))

  bmaResults[["fixed"]]     <- fixed

  # Random effects model
  random <- list()
  random[["estimates"]]     <- results$meta$random$estimates
  random[["summary"]]       <- rstan::summary(results$meta$random$stanfit_dstudy)$summary
  ## Prior and posterior - effect size
  anchorPoint               <- random[["estimates"]]["d", 1]
  random[["xPost"]]         <- seq(anchorPoint - 2, anchorPoint + 2, .001)
  random[["yPost"]]         <- results$meta$random$posterior_d(random[["xPost"]])
  random[["yPrior"]]        <- results$meta$random$prior_d(random[["xPost"]])
  ## Prior and posterior - heterogeneity
  anchorPoint               <- random[["estimates"]][2, "mean"]
  random[["xPostTau"]]      <- seq(-0.05, anchorPoint + 4, .001)
  random[["yPostTau"]]      <- results$meta$random$posterior_tau(random[["xPostTau"]])
  random[["yPriorTau"]]     <- results$meta$random$prior_tau(random[["xPostTau"]])
  random[["dfPointsY"]]     <- data.frame(prior = results$meta$random$prior_d(0),
                                          posterior = results$meta$random$posterior_d(0))

  bmaResults[["random"]]    <- random

  # Ordered effects model
  if(options[["model"]] == "constrainedRandom"){

    ordered                 <- list()
    ordered[["estimates"]]  <- results$meta$ordered$estimates
    ordered[["summary"]]    <- rstan::summary(results$meta$ordered$stanfit_dstudy)$summary
    ## Prior and posterior - effect size
    anchorPoint             <- results$meta$ordered$estimates[2, "mean"]
    if(options$constrainedRandomDirection == "positive") xSeq <- seq(-0.05, anchorPoint + 4, .001)
    if(options$constrainedRandomDirection == "negative") xSeq <- seq(anchorPoint - 4, 0.05, .001)
    ordered[["xPost"]]   <- xSeq
    ordered[["yPost"]]   <- results$meta$ordered$posterior_d(ordered[["xPost"]])
    ordered[["yPrior"]]  <- results$meta$ordered$prior_d(ordered[["xPost"]])

    ## Prior and posterior - heterogeneity
    anchorPoint             <- results$meta$ordered$estimates[2, "mean"]
    ordered[["xPostTau"]]   <- seq(-0.05, anchorPoint + 4, .001)
    ordered[["yPostTau"]]   <- results$meta$ordered$posterior_tau(ordered[["xPostTau"]])
    ordered[["yPriorTau"]]  <- results$meta$ordered$prior_tau(ordered[["xPostTau"]])
    ordered[["dfPointsY"]]  <- data.frame(prior = results$meta$ordered$prior_d(0),
                                          posterior = results$meta$ordered$posterior_d(0))

    bmaResults[["ordered"]] <- ordered
  }

  # Save trimmed down list in state and return
  jaspResults[["bmaResults"]] <- createJaspState(object=bmaResults, dependencies=.bmaDependencies)
  return(jaspResults[["bmaResults"]]$object)
}

# Save the Bayesian meta-analysis
.bmaResults <- function(jaspResults, dataset, options) {


  varES <- options[["effectSize"]]

  # Get necessary variables
  y <- dataset[, options[["effectSize"]]]

  if(options[["model"]] == "constrainedRandom" && options[["constrainedRandomDirection"]] == "positive"){

    negativeValues <- function(){
      if(all(dataset[, options[["effectSize"]]] < 0))
        return(gettextf("No positive numbers found in %s", options[["effectSize"]]))
    }

    .hasErrors(dataset = dataset,
               exitAnalysisIfErrors= TRUE,
               custom = negativeValues)

  } else if(options[["model"]] == "constrainedRandom" && options[["constrainedRandomDirection"]] == "negative"){

    positiveValues <- function(){
      if(all(dataset[, options[["effectSize"]]] > 0))
        return(gettextf("No negative numbers found in %s", options[["effectSize"]]))
    }

    .hasErrors(dataset = dataset,
               exitAnalysisIfErrors= TRUE,
               custom = positiveValues)
  }

  if(all(unlist(options[["effectSizeCi"]]) != "") && !is.null(unlist(options[["effectSizeCi"]]))){
    lower <- dataset[, options$effectSizeCi[[1]][[1]]]
    upper <- dataset[, options$effectSizeCi[[1]][[2]]]

    .hasErrors(dataset = dataset,
               exitAnalysisIfErrors= TRUE,
               custom = function() {
                 if (!all(lower < upper))
                   return(gettextf("The 95%% CI Lower Bound must be smaller than the Upper Bound."))
               })

    SE <- (upper - lower)/2/qnorm(0.975)
  }
  if(options$effectSizeSe != ""){
    SE <- dataset[, options[["effectSizeSe"]]]
    .hasErrors(dataset              = dataset,
               seCheck.target       = options[["effectSizeSe"]],
               custom               = .maCheckStandardErrors,
               exitAnalysisIfErrors = TRUE)
  }

  # Advanced: estimation settings
  iter <- options[["samples"]]
  chains <- options[["chains"]]

  # Advanced: bayes factor computation
  if(options$bayesFactorComputation == "integration"){
    logml <- "integrate"
    logml_iter <- 5000
  } else if(options$bayesFactorComputation == "bridgeSampling"){
    logml <- "stan"
    logml_iter <- options[["bridgeSamplingSamples"]]
  }

  # Prior model probabilities
  prior <- c(options[["priorModelProbabilityFixedNull"]], options[["priorModelProbabilityFixedAlternative"]],
             options[["priorModelProbabilityRandomNull"]], options[["priorModelProbabilityRandomAlternative"]])

  if(all(prior == 0) && options[["model"]] != "constrainedRandom")
    .quitAnalysis(gettext("You cannot set all the prior model probabilties to zero."))

  # Get priors from jasp state
  .bmaPriors(jaspResults, options)

  d   <- jaspResults[["bmaPriors"]]$object[["d"]]
  tau <- jaspResults[["bmaPriors"]]$object[["tau"]]


  # Bayesian meta analysis
  .setSeedJASP(options)
  if(options$model != "constrainedRandom"){
    p <- try({
      # Bayesian model averaging (includes fixed and random effects)
      results <- metaBMA::meta_bma(y     = y,
                                   SE    = SE,
                                   prior = prior,
                                   d     = d,
                                   tau   = tau,
                                   logml   = logml,
                                   logml_iter = logml_iter,
                                   iter     = iter,
                                   chains = chains)
    })
  } else {
    p <- try({
      # Ordered effects
      results <- metaBMA::meta_ordered(y = y,
                                       SE = SE,
                                       d = d,
                                       tau = tau,
                                       # logml = logml,
                                       # logml_iter = logml_iter,
                                       iter = 10000 # because of an issue with stored variables, it is not yet possible to make it reactive.
                                       # chains = chains
      )
    })
  }

  if(isTryError(p)){
    .quitAnalysis(gettextf("The model could not be fit. Please check the following: Do you have at least n=2 studies? If the prior is truncated, is it consistent with the data (when most effect sizes are negative, the analysis may not work when the prior is constrained to be postive)?"))
  }

  return(results)
}

.bmaGetModelName <- function(options) {
  if(options[["model"]] == "constrainedRandom") return("ordered")
  if(options[["model"]] == "averaging") return("averaged")
  if(options[["model"]] == "random")  return("random")
  return("fixed")
}

.bmaCalculateBFHeterogeneity <- function(prior_models, posterior_models){
  postOdds <- (posterior_models["random_H0"] + posterior_models["random_H1"]) / (posterior_models["fixed_H0"] + posterior_models["fixed_H1"])
  priorOdds <- (prior_models[3] + prior_models[4]) / (prior_models[1] + prior_models[2])
  BFheterogeneity <- postOdds/priorOdds
  return(BFheterogeneity)
}

.bmaFillSequentialResults <- function(i, bmaResults, seqResults, options, sequential){
  modelName <- .bmaGetModelName(options)

  if(sequential){
    seqResults$mean[i]      <- bmaResults$estimates[modelName, "mean"]
    seqResults$lowerMain[i] <- bmaResults$estimates[modelName, "2.5%"]
    seqResults$upperMain[i] <- bmaResults$estimates[modelName, "97.5%"]

    if(options[["model"]] == "averaging"){
      seqResults$BFs[i] <- bmaResults$inclusion$incl.BF
      seqResults$BFsHeterogeneity[[i]] <- .bmaCalculateBFHeterogeneity(bmaResults$prior_models, bmaResults$posterior_models)
    }
    if(options[["model"]] == "fixed")  seqResults$BFs[i] <- bmaResults$BF["fixed_H1", "fixed_H0"]
    if(options[["model"]] == "random"){
      seqResults$BFs[i] <- bmaResults$BF["random_H1", "random_H0"]
      seqResults$BFsHeterogeneity[[i]] <- bmaResults$BF["random_H1", "fixed_H1"]
    }
    if(options[["model"]] == "constrainedRandom"){
      seqResults$BFs[i] <- bmaResults$BF["ordered", "null"]
      seqResults$BFsHeterogeneity[[i]] <- bmaResults$BF["ordered", "fixed"]
    }

    seqResults$posterior_models[[i]] <- bmaResults$posterior_models
  } else {
    # The results in the state are saved differently.
    # Therefore I need different code to extract what I need for the sequential analysis.
    seqResults$mean[i]      <- bmaResults[["bma"]]$estimates[modelName, "mean"]
    seqResults$lowerMain[i] <- bmaResults[["bma"]]$estimates[modelName, "2.5%"]
    seqResults$upperMain[i] <- bmaResults[["bma"]]$estimates[modelName, "97.5%"]

    if(options[["model"]] == "averaging"){
      seqResults$BFs[i] <- bmaResults[["bf"]]$inclusionBF
      seqResults$BFsHeterogeneity[[i]] <- .bmaCalculateBFHeterogeneity(bmaResults[["models"]]$prior, bmaResults[["models"]]$posterior)
    }
    if(options[["model"]] == "fixed")  seqResults$BFs[i] <- bmaResults[["bf"]]$BF["fixed_H1", "fixed_H0"]
    if(options[["model"]] == "random"){
      seqResults$BFs[i] <- bmaResults[["bf"]]$BF["random_H1", "random_H0"]
      seqResults$BFsHeterogeneity[[i]] <- bmaResults[["bf"]]$BF["random_H1", "fixed_H1"]
    }
    if(options[["model"]] == "constrainedRandom"){
      seqResults$BFs[i] <- bmaResults[["bf"]]$BF["ordered", "null"]
      seqResults$BFsHeterogeneity[[i]] <- bmaResults[["bf"]]$BF["ordered", "fixed"]
    }

    seqResults$posterior_models[[i]] <- bmaResults[["models"]]$posterior
  }

  return(seqResults)
}

.bmaSequentialResults <- function(jaspResults, dataset, options, .bmaDependencies) {

  if(!is.null(jaspResults[["bmaSeqResults"]])) return(jaspResults[["bmaSeqResults"]]$object)

  n <- nrow(dataset)
  startProgressbar(n-2)

  seqResults <- list(mean=numeric(), lowerMain=numeric(), upperMain=numeric(),
                     BFs=numeric(1), posterior_models=list(), BFsHeterogeneity = numeric(1))

  d                       <- .bmaPriors(jaspResults, options)[["d"]]
  # Fix voor truncated priors
  priorSamples            <- sample(seq(-10, 10, by = 0.0001), size = 1e6, replace = TRUE, prob = d(seq(-10, 10, by = 0.0001)))
  seqResults$mean[1]      <- mean(priorSamples)
  seqResults$lowerMain[1] <- quantile(priorSamples, probs = 0.025)
  seqResults$upperMain[1] <- quantile(priorSamples, probs = 0.975)

  # meta analysis cannot run with only 1 study so it starts with 2
  # the final result is already in the state, so we do not have to run it again (n-1)
  for(i in 2:(n-1)){
    bmaResults <- .bmaResults(jaspResults, dataset[1:i, ], options)
    seqResults <- .bmaFillSequentialResults(i, bmaResults, seqResults, options, sequential = TRUE)

    progressbarTick()
  }

  # Get results from state
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)
  seqResults <- .bmaFillSequentialResults(n, bmaResults, seqResults, options, sequential = F)

  jaspResults[["bmaSeqResults"]] <- createJaspState(object=seqResults, dependencies=.bmaDependencies)

  return(jaspResults[["bmaSeqResults"]]$object)
}
