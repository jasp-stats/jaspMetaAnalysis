context("Robust Bayesian Meta-Analysis")
testthat::skip("re-create once unit tests can be re-created directly from data library")

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
