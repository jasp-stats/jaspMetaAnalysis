# Deprecated Bayesian meta-analysis tables.
#
# Preserves posterior-model and study-effect table output for legacy analyses.

.bmaMainTable <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  if (!is.null(jaspResults[["bmaTable"]])) return()
  bmaTable <- createJaspTable(title = gettext("Posterior Estimates per Model"))
  bmaTable$position <- 1

  # Add standard depencies
  bmaTable$dependOn(options = c(.bmaDependencies, "bayesFactorType"))

  if (options$bayesFactorType == "BF10")
    bfTitle <- gettextf("BF%1$s%2$s", "\u2081", "\u2080")
  else if (options$bayesFactorType == "BF01")
    bfTitle <- gettextf("BF%1$s%2$s", "\u2080", "\u2081")
  else
    bfTitle <- gettextf("Log(BF%1$s%2$s)", "\u2081", "\u2080")

  # Add columns
  bmaTable$addColumnInfo(name = "model", title = "", type = "string", combine = TRUE)
  bmaTable$addColumnInfo(name = "parameter", title = "", type = "string")
  bmaTable$addColumnInfo(name = "ES", title = gettext("Mean"), type = "number")
  bmaTable$addColumnInfo(name = "SD", title = gettext("SD"), type = "number")
  bmaTable$addColumnInfo(name = "lb", title = gettext("Lower"), type = "number",
                         overtitle = gettextf("95%% Credible Interval"))
  bmaTable$addColumnInfo(name = "ub", title = gettext("Upper"), type = "number",
                         overtitle = gettextf("95%% Credible Interval"))
  bmaTable$addColumnInfo(name = "BF", title = bfTitle, type = "number")

  # Row names (tried to get modelRE idented, but failed)
  modelBMA <- gettext("Averaged")
  modelFE  <- gettext("Fixed effects")
  modelRE  <- gettext("Random effects")
  modelCRE <- gettext("Ordered effects")

  tau <- "\u03C4"
  mu <- "\u03BC"

  if(options[["model"]] == "fixed"){
    model <- modelFE
    parameter <- mu
    group <- T
    bmaTable$setExpectedSize(1)
  }
  if(options[["model"]] == "random"){
    model <- c(modelRE, modelRE)
    parameter <- c(mu, tau)
    group <- c(T, F)
    bmaTable$setExpectedSize(2)
  }
  if(options[["model"]] == "averaging"){
    model <- c(modelFE, modelRE, modelRE, modelBMA, modelBMA)
    parameter <- c(mu, mu, tau, mu, tau)
    group <- c(T, T, F, T, F)
    bmaTable$setExpectedSize(5)
  }
  if(options[["model"]] == "constrainedRandom"){
    model <- c(modelFE, modelCRE, modelCRE, modelRE, modelRE)
    parameter <- c(mu, mu, tau, mu, tau)
    group <- c(T, T, F, T, F)
    bmaTable$setExpectedSize(5)
  }

  if(options$model != "fixed"){
    bmaTable$addFootnote(gettextf("%1$s and %2$s are the group-level effect size and standard deviation, respectively.", "\u03BC", "\u03C4"))
  } else {
    bmaTable$addFootnote(gettextf("%s is the group-level effect size.", "\u03BC"))
  }

  jaspResults[["bmaTable"]] <- bmaTable

  # Check if ready
  if(!ready){
    rows <- data.frame(model = model,
                       parameter = parameter,
                       ES = ".",
                       SD = ".",
                       lb = ".",
                       ub = ".",
                       BF = ".",
                       .isNewGroup = group)
    row.names(rows) <- paste0("row", 1:length(model))
    bmaTable$addRows(rows)
    return()
  }

  # Get analysis results
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)

  # Get results per column (different per model)
  if(options[["model"]] == "averaging"){
    meanES <- c(bmaResults[["bma"]]$estimates["fixed", "mean"],
                bmaResults[["bma"]]$estimates["random", "mean"],
                bmaResults[["random"]]$estimates["tau", "mean"],
                bmaResults[["bma"]]$estimates["averaged", "mean"],
                NA)
    meanSD <- c(bmaResults[["bma"]]$estimates["fixed", "sd"],
                bmaResults[["bma"]]$estimates["random", "sd"],
                bmaResults[["random"]]$estimates["tau", "sd"],
                bmaResults[["bma"]]$estimates["averaged", "sd"],
                NA)
    lower <- c(bmaResults[["bma"]]$estimates["fixed", "2.5%"],
               bmaResults[["bma"]]$estimates["random", "2.5%"],
               bmaResults[["random"]]$estimates["tau", "2.5%"],
               bmaResults[["bma"]]$estimates["averaged", "2.5%"],
               NA)
    upper <- c(bmaResults[["bma"]]$estimates["fixed", "97.5%"],
               bmaResults[["bma"]]$estimates["random", "97.5%"],
               bmaResults[["random"]]$estimates["tau", "97.5%"],
               bmaResults[["bma"]]$estimates["averaged", "97.5%"],
               NA)
    BF <- c(bmaResults[["bf"]]$BF["fixed_H1", "fixed_H0"],
            bmaResults[["bf"]]$BF["random_H1", "random_H0"],
            bmaResults[["bf"]]$BF["random_H1", "fixed_H1"],
            bmaResults[["bf"]]$inclusionBF,
            .bmaCalculateBFHeterogeneity(prior_models = bmaResults[["models"]]$prior,
                                         posterior_models = bmaResults[["models"]]$posterior))
  }
  else if(options[["model"]] == "random"){
    meanES <- bmaResults[["random"]]$estimates[, "mean"]
    meanSD <- bmaResults[["random"]]$estimates[, "sd"]
    lower <- bmaResults[["random"]]$estimates[, "2.5%"]
    upper <- bmaResults[["random"]]$estimates[, "97.5%"]
    BF <- c(bmaResults[["bf"]]$BF["random_H1", "random_H0"],
            bmaResults[["bf"]]$BF["random_H1", "fixed_H1"])
  }
  else if(options[["model"]] == "fixed"){
    meanES <- bmaResults[["fixed"]]$estimates[, "mean"]
    meanSD <- bmaResults[["fixed"]]$estimates[, "sd"]
    lower <- bmaResults[["fixed"]]$estimates[, "2.5%"]
    upper <- bmaResults[["fixed"]]$estimates[, "97.5%"]
    BF <- bmaResults[["bf"]]$BF["fixed_H1", "fixed_H0"]
  }
  else if(options[["model"]] == "constrainedRandom"){
    meanES <- c(bmaResults[["bma"]]$estimates["fixed", "mean"],
                bmaResults[["ordered"]]$estimates[c("average_effect", "tau"), "mean"],
                bmaResults[["random"]]$estimates[, "mean"])
    meanSD <- c(bmaResults[["bma"]]$estimates["fixed", "sd"],
                bmaResults[["ordered"]]$estimates[c("average_effect", "tau"), "sd"],
                bmaResults[["random"]]$estimates[, "sd"])
    lower <- c(bmaResults[["bma"]]$estimates["fixed", "2.5%"],
               bmaResults[["ordered"]]$estimates[c("average_effect", "tau"), "2.5%"],
               bmaResults[["random"]]$estimates[, "2.5%"])
    upper <- c(bmaResults[["bma"]]$estimates["fixed", "97.5%"],
               bmaResults[["ordered"]]$estimates[c("average_effect", "tau"), "97.5%"],
               bmaResults[["random"]]$estimates[, "97.5%"])
    BF <- c(bmaResults[["bf"]]$BF["fixed", "null"],
            bmaResults[["bf"]]$BF["ordered", "null"],
            bmaResults[["bf"]]$BF["ordered", "fixed"],
            bmaResults[["bf"]]$BF["random", "null"],
            bmaResults[["bf"]]$BF["random", "fixed"])
  }


  footnoteRandomBFtau <- gettextf("Bayes factor of the random effects H%1$s over the fixed effects H%1$s.", "\u2081")

  footnoteAverage <- gettextf("Posterior estimates are based on the models that assume an effect to be present. The Bayes factor is based on all four models: fixed effects H%2$s & random effects H%2$s over the fixed effects H%1$s & random effects H%1$s.", "\u2080", "\u2081")
  footnoteAverageBFtau <- gettextf("Model averaged posterior estimates for %3$s are not yet available, but will be added in the future. The Bayes factor is based on all four models: random effects H%1$s & H%2$s over the fixed effects H%1$s & H%2$s.", "\u2080", "\u2081", "\u03C4")
  footnoteOrderedBFtau <- gettextf("Bayes factor of the (unconstrained/constrained) random effects H%1$s over the fixed effects H%1$s.", "\u2081")

  if(options[["model"]] == "constrainedRandom")
    creBF <- bmaResults[["bf"]]$BF["ordered", "random"]

  if(options[["bayesFactorType"]] == "BF01"){
    BF <- 1/BF
    footnoteRandomBFtau <- gettextf("Bayes factor of the fixed effects H%1$s over the random effects H%1$s.", "\u2081")
    footnoteAverage <- gettextf("Model averaged posterior estimates are based on the models that assume an effect to be present. The Bayes factor is based on all four models: fixed effects H%1$s & random effects H%1$s over the fixed effects H%2$s & random effects H%2$s.", "\u2080", "\u2081")
    footnoteAverageBFtau <- gettextf("Model averaged posterior estimates for %3$s are not yet available, but will be added in the future. The Bayes factor is based on all four models: fixed effects H%1$s & H%2$s over the random effects H%1$s & H%2$s.", "\u2080", "\u2081", "\u03C4")
    footnoteOrderedBFtau <- gettextf("Bayes factor of the fixed effects H%1$s over the (unconstrained/constrained) random effects H%1$s.", "\u2081")

    if(options[["model"]] == "constrainedRandom"){
      creBF <- 1/bmaResults[["bf"]]$BF["ordered", "random"]
    }
  }
  if(options[["bayesFactorType"]] == "LogBF10"){
    BF <- log(BF)
    if(options[["model"]] == "constrainedRandom"){
      creBF <- log(bmaResults[["bf"]]$BF["ordered", "random"])
    }
  }
  # Add results to table
  rows <- data.frame(model = model,
                     parameter = parameter,
                     ES = meanES,
                     SD = meanSD,
                     lb = lower,
                     ub = upper,
                     BF = BF,
                     .isNewGroup = group)
  row.names(rows) <- paste0("row", 1:length(model))

  bmaTable$addRows(rows)

  if(options$model == "random") bmaTable$addFootnote(footnoteRandomBFtau, colNames = "BF", rowNames = "row1")

  if(options$model == "averaging") {
    bmaTable$addFootnote(footnoteAverage,
                         colNames = "parameter", rowNames="row3")
    bmaTable$addFootnote(footnoteRandomBFtau,
                         colNames = "BF", rowNames = "row2")
    bmaTable$addFootnote(footnoteAverageBFtau,
                         colNames = "parameter", rowNames = "row4")
  }

  if(options$model == "constrainedRandom"){
    if(options[["bayesFactorType"]] == "BF10" || options[["bayesFactorType"]] == "LogBF10"){
      footnoteCREbf <- gettextf("Bayes factor of the ordered effects H%1$s over the fixed effects H%2$s. The Bayes factor for the ordered effects H%1$s versus the unconstrained (random) effects H%1$s model is %3$.3f.", "\u2081", "\u2080", creBF)
    } else if(options[["bayesFactorType"]] == "BF01"){
      footnoteCREbf <-gettextf("Bayes factor of the fixed effects H%2$s over the ordered effects H%1$s. The Bayes factor for the unconstrained (random) effects H%1$s versus the ordered effects H%1$s model is %3$.3f.", "\u2081", "\u2080", creBF)
    }


    bmaTable$addFootnote(footnoteCREbf,
                         colNames = "BF", rowNames="row1")
    bmaTable$addFootnote(footnoteOrderedBFtau, colNames = "BF", rowNames = c("row2", "row4"))
  }
}

.bmaPostModelTable <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  if (!is.null(jaspResults[["modelProbability"]])) return()
  postTable <- createJaspTable(title = gettext("Model Probabilities"))
  postTable$dependOn(c(.bmaDependencies, "modelProbability"))
  postTable$position <- 2

  # Add columns
  postTable$addColumnInfo(name = "model", title = "", type = "string")
  postTable$addColumnInfo(name = "priorProb",   title = gettext("Prior"),   type = "number")
  postTable$addColumnInfo(name = "postProb",   title = gettext("Posterior"),   type = "number")

  # Add table to output
  jaspResults[["modelProbability"]] <- postTable

  modelFixedH0 <- gettextf("Fixed H%s", "\u2080")
  modelFixedH1 <- gettextf("Fixed H%s", "\u2081")
  modelRandomH0 <- gettextf("Random H%s", "\u2080")
  modelRandomH1 <- gettextf("Random H%s", "\u2081")
  modelOrderedH1 <- gettextf("Ordered H%s", "\u2081")

  if(options$model == "averaging"){
    model <- c(modelFixedH0, modelFixedH1, modelRandomH0, "Random H\u2081")
  }
  if(options$model == "fixed"){
    model <- c(modelFixedH0, modelFixedH1)
  }
  if(options$model == "random"){
    model <- c(modelRandomH0, "Random H\u2081")
  }
  if(options$model == "constrainedRandom"){
    model <- c(modelFixedH0, modelFixedH1, modelOrderedH1, modelRandomH1)
  }

  # Check if ready
  if(!ready){
    row <- data.frame(model = model, priorProb = ".", postProb = ".")
    postTable$addRows(row)
    return()
  }

  # Get results from jasp state
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)

  # Get results per column (different per model)
  if(options$model == "averaging"){
    postProb <- bmaResults[["models"]]$posterior
    priorProb <- bmaResults[["models"]]$prior
  }
  if(options$model == "fixed"){
    postProb <- bmaResults[["models"]]$posterior[c("fixed_H0", "fixed_H1")]
    priorProb <- bmaResults[["models"]]$prior[1:2]
  }
  if(options$model == "random"){
    postProb <- bmaResults[["models"]]$posterior[c("random_H0", "random_H1")]
    priorProb <- bmaResults[["models"]]$prior[3:4]
  }
  if(options$model == "constrainedRandom"){
    postProb <- bmaResults[["models"]]$posterior
    priorProb <- bmaResults[["models"]]$prior
  }

  # Fill table
  row <- data.frame(model = model, priorProb =  priorProb, postProb = postProb)
  postTable$addRows(row)
}

.bmaEffectSizeTable <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  if (!is.null(jaspResults[["effectSizePerStudy"]])) return()
  esTable <- createJaspTable(title = gettext("Effect Sizes per Study"))
  esTable$dependOn(c(.bmaDependencies, "effectSizePerStudy", "studyLabel"))
  esTable$position <- 3

  # Add standard columns
  esTable$addColumnInfo(name = "study", title = "", type = "string")
  esTable$addColumnInfo(name = "observedES", title = gettext("Observed"), type = "number")

  # Add conditional columns
  if(options$model != "fixed"){
    esTable$addColumnInfo(name = "estimatedES", title = gettext("Mean"), type = "number",
                          overtitle = gettext("Estimated"))
    esTable$addColumnInfo(name = "estimatedLower", title = gettext("Lower"), type = "number",
                          overtitle = gettext("Estimated"))
    esTable$addColumnInfo(name = "estimatedUpper", title = gettext("Upper"), type = "number",
                          overtitle = gettext("Estimated"))
  }

  # Only show conditional columns for right analysis
  esTable$showSpecifiedColumnsOnly <- TRUE

  # Add table to output
  jaspResults[["effectSizePerStudy"]] <- esTable

  # Check if ready
  if(!ready){
    if(options[["studyLabel"]] != ""){
      studyLabels <- dataset[, options[["studyLabel"]]]
      row <- data.frame(study = studyLabels,
                        observedES = ".",
                        estimatedES = ".",
                        estimatedLower = ".",
                        estimatedUpper = ".")
      esTable$addRows(row)
    }
    return()
  }

  # Get results from jasp state
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)

  # Get effect size variable
  varES <- dataset[, options[["effectSize"]]]

  # Create empty vectors
  estimatedES <- rep(NA, length(varES))
  estimatedLower <- rep(NA, length(varES))
  estimatedUpper <- rep(NA, length(varES))

  # Fill vectors with estimation variables if not fixed
  if(options$model != "fixed"){
    estimatedES    <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "mean"]
    estimatedLower <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "2.5%"]
    estimatedUpper <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "97.5%"]
  }

  if(options$model == "constrainedRandom"){
    estimatedES <- bmaResults[["ordered"]]$summary[3:(length(varES) + 2), "mean"]
    estimatedLower <- bmaResults[["ordered"]]$summary[3:(length(varES) + 2), "2.5%"]
    estimatedUpper <- bmaResults[["ordered"]]$summary[3:(length(varES) + 2), "97.5%"]
  }

  # Add studylabels when given, otherwise use "Study n"
  if(options[["studyLabel"]] != ""){
    studyLabels <- dataset[, options[["studyLabel"]]]
  } else {
    studyLabels <- paste(gettext("Study"), 1:length(varES))
  }

  # Add results to table
  row <- data.frame(study = studyLabels,
                    observedES = varES,
                    estimatedES = estimatedES,
                    estimatedLower = estimatedLower,
                    estimatedUpper = estimatedUpper)
  esTable$addRows(row)

  if(options$model != "fixed"){
    esTable$addFootnote(gettextf("Posterior mean and 95%% credible interval estimates from the random effects model."),
                        colNames = c("estimatedES", "estimatedLower", "estimatedUpper"))
  } else if(options$model == "constrainedRandom"){
    esTable$addFootnote(gettextf("Posterior mean and 95%% credible interval estimates from the constrained random effects model."),
                        colNames = c("estimatedES", "estimatedLower", "estimatedUpper"))
  }
}
