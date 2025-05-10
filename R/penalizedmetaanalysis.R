#
# Copyright (C) 2013-2018 University of Amsterdam
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

# TODO:
# - centering
# - different CI widths

PenalizedMetaAnalysis <- function(jaspResults, dataset = NULL, options, ...) {

  if (.pemaCheckReady(options)) {
    # get the data
    dataset <- .pemaCheckData(jaspResults, dataset, options)

    # fit the models
    .pemaFit(jaspResults, dataset, options)
  }

  if (options[["inferenceEstimatesTable"]])
    .pemaSummaryTable(jaspResults, options)

  if (options[["inferenceHeterogeneityTable"]])
    .pemaSummaryTableTau(jaspResults, options)

  if (options[["inferenceHeterogeneityTable"]] && options[["inferenceHeterogeneityI2"]])
    .pemaSummaryTableI2(jaspResults, options)

  if (length(options[["posteriorPlotsSelectedTerms"]]) > 0)
    .pemaposteriorPlotsSelectedTerms(jaspResults, options)

  if (length(options[["scatterVariableX"]]) > 0)
    .pemaDiagnostics(jaspResults, options)

  return()
}

.pemaDependencies <- c(
  "effectSize", "effectSizeSe", "method", "studyLabels", "covariates", "factors", "clustering", "modelComponents", "modelTerms", "modelIncludeIntercept", "modelScalePredictors",
  "horseshoePriorDf", "horseshoePriorScale", "lassoPriorDf", "lassoPriorDfGlobal", "lassoPriorDfSlab", "lassoPriorScaleGlobal", "lassoPriorScaleSlab",
  "mcmcBurnin", "mcmcSamples", "mcmcChains", "mcmcAdaptDelta", "mcmcMaxTreedepth", "setSeed", "seed"
)

# check and load functions
.pemaCheckReady                <- function(options) {
  return(options[["effectSize"]] != "" && options[["effectSizeSe"]] != "" && length(options[["modelTerms"]]) > 0)
}
.pemaCheckData                 <- function(jaspResults, dataset, options) {

  # precompute variance
  dataset$JASP_computed_variance__ <- dataset[,stderrName]^2

  # check the data
  dataset <- .pemaCheckData(jaspResults, dataset, options)

  return(dataset)
}
.pemaCheckData                 <- function(jaspResults, dataset, options) {

  datasetOld <- dataset
  dataset    <- na.omit(dataset)

  # store the number of missing values
  if (nrow(datasetOld) > nrow(dataset)) {
    if (!is.null(jaspResults[["nOmitted"]])) {
      nOmitted <- jaspResults[["nOmitted"]]
    } else {
      nOmitted <- createJaspState()
      nOmitted$dependOn(.pemaDependencies)
      jaspResults[["nOmitted"]] <- nOmitted
    }
    nOmitted$object <- nrow(datasetOld) - nrow(dataset)
  }

  .hasErrors(dataset               = dataset,
             type                  = c("infinity", "observations"),
             observations.amount   = "< 2",
             exitAnalysisIfErrors  = TRUE)

  if (options[["effectSizeSe"]] != "")
    .hasErrors(dataset               = dataset,
               type                  = c("negativeValues"),
               negativeValues.target = options[["inputSE"]],
               exitAnalysisIfErrors  = TRUE)

  return(dataset)
}
.pemaFormula                   <- function(options) {

  if (length(options[["modelTerms"]]) == 0 && options[["modelIncludeIntercept"]])
    formula <- paste(options[["effectSize"]], "~", "1")
  else if (length(options[["modelTerms"]]) > 0 && options[["modelIncludeIntercept"]])
    formula <- paste(options[["effectSize"]], "~", paste0(sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":")), collapse = "+"))
  else if (length(options[["modelTerms"]]) > 0 && !options[["modelIncludeIntercept"]])
    formula <- paste(options[["effectSize"]], "~", paste0(sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":")), collapse = "+"), "-1")
  else
    .quitAnalysis(gettext("The model should contain at least one predictor or an intercept."))


  return(as.formula(formula, env = parent.frame(1)))
}
.pemaVariableNames             <- function(varName, terms) {

  if (varName == "Intercept")
    return(gettext("Intercept"))
  else if(varName == "tau2")
    return("\U1D6D5\U00B2")
  else if(varName == "tau2_w")
    return("\U1D6D5\U00B2 (within)")
  else if(varName == "tau2_b")
    return("\U1D6D5\U00B2 (between)")

  for (vn in terms) {
    inf <- regexpr(vn, varName, fixed = TRUE)

    if (inf[1] != -1) {
      varName <- paste0(
        substr(varName, 0, inf[1] - 1),
        substr(varName, inf[1], inf[1] + attr(inf, "match.length") - 1),
        " (",
        substr(varName, inf[1] + attr(inf, "match.length"), nchar(varName))
      )
    }

  }

  varName <- gsub(":", paste0(")", jaspBase::interactionSymbol), varName, fixed = TRUE)
  varName <- paste0(varName, ")")
  varName <- gsub(" ()", "", varName, fixed = TRUE)

  return(varName)
}
.pemaPredictorCoefficientNames <- function(predictor, coefficientNames, options){
  # this functions find coefficients matching to the given predictor
  # i.e., what are the dummy coefficients for a given factor parameter

  if (predictor == "Intercept")
    return("Intercept")
  else if(predictor == "Heterogeneity")
    return("tau2")


  predictors <- sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":"))
  predictors <- predictors[predictor != predictors]


  if (grepl(":", predictor, fixed = TRUE)) {

    # filter out non interactions if an interaction is searched for
    coefficientNames <- coefficientNames[grepl(":", coefficientNames, perl = TRUE)]
    predictors       <- predictors[grepl(":", predictors, perl = TRUE)]

    coefficientNamesSplit <- lapply(coefficientNames, function(coefficientName) unlist(strsplit(coefficientName, ":", fixed = TRUE)))

    # remove all coefficients that match other terms
    indx <- rep(TRUE, length(coefficientNames))
    for (i in seq_along(predictors)) {

      tempTest <- unlist(strsplit(predictors[i], ":", fixed = TRUE))
      indx     <- indx & !sapply(coefficientNamesSplit, function(coefficientNameSplit) {
        all(sapply(tempTest, function(test) any(grepl(test, coefficientNameSplit))))
      })

    }

    coefficientNames <- coefficientNames[indx]
    return(coefficientNames)

  } else {

    # filter out interactions if a non interaction is searched for
    coefficientNames <- coefficientNames[!grepl(":", coefficientNames, perl = TRUE)]
    predictors       <- predictors[!grepl(":", predictors, perl = TRUE)]

    coefficientNames <- coefficientNames[grepl(predictor, coefficientNames)]
    return(coefficientNames)
  }
}
.pemaPriors                    <- function(options) {
  switch(
    options[["method"]],
    horseshoe = c(
      df    = options[["horseshoePriorDf"]],
      scale = options[["horseshoePriorScale"]]),
    lasso = c(
      df            = options[["lassoPriorDf"]],
      df_global     = options[["lassoPriorDfGlobal"]],
      df_slab       = options[["lassoPriorDfSlab"]],
      scale_global  = options[["lassoPriorScaleGlobal"]],
      scale_slab    = options[["lassoPriorScaleSlab"]],
      relevant_pars = NULL)
  )
}
.pemaFit                       <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["model"]])) {
    return()
  } else {
    model <- createJaspState()
    model$dependOn(.pemaDependencies)
    jaspResults[["model"]] <- model
  }

  .setSeedJASP(options)
  fit <- try(pema::brma(
    formula     = .pemaFormula(options),
    data        = dataset,
    vi          = "JASP_computed_variance__",
    study       = if (options[["clustering"]] != "") options[["clustering"]],
    method      = .pemaMethod(options),
    prior       = .pemaPriors(options),
    standardize = options[["modelScalePredictors"]],
    mute_stan   = TRUE,
    chains      = options[["mcmcChains"]],
    warmup      = options[["mcmcBurnin"]],
    iter        = options[["mcmcBurnin"]] + options[["mcmcSamples"]],
    seed        = if (options[["setSeed"]]) .getSeedJASP(options) else sample.int(.Machine$integer.max, 1),
    control     = list(adapt_delta = options[["mcmcAdaptDelta"]], max_treedepth = options[["mcmcMaxTreedepth"]])
  ))
  model[["object"]] <- fit

  return()
}
.pemaSummaryTable              <- function(jaspResults, options) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  model <- jaspResults[["model"]]$object

  summaryTable <- createJaspTable(title = gettext("Coefficients"))
  summaryTable$position <- 1
  summaryTable$dependOn(c(.pemaDependencies, "inferenceEstimatesTable"))

  summaryTable$addColumnInfo(name = "term",     title = "Term",           type = "string")
  summaryTable$addColumnInfo(name = "estimate", title = "Estimate",       type = "number")
  summaryTable$addColumnInfo(name = "se",       title = "SE",             type = "number")
  summaryTable$addColumnInfo(name = "lowerCI",  title = gettext("Lower"), type = "number", overtitle = gettextf("%s%% CI", 100 * .95))
  summaryTable$addColumnInfo(name = "upperCI",  title = gettext("Upper"), type = "number", overtitle = gettextf("%s%% CI", 100 * .95))
  summaryTable$addColumnInfo(name = "rhat",     title = "R-hat",          type = "number")
  summaryTable$addColumnInfo(name = "neff",     title = "ESS",            type = "number")

  jaspResults[["summaryTable"]] <- summaryTable

  if (is.null(model)) {
    if (options[["effectSize"]] != "" && options[["effectSizeSe"]] != "" && length(options[["modelTerms"]]) == 0)
      summaryTable$addFootnote(gettext("At least one predictor needs to be specified."))

    return()
  } else if (jaspBase::isTryError(model)) {
    summaryTable$setError(model)
    return()
  }


  modelSummary <- model[["coefficients"]]
  modelSummary <- modelSummary[!rownames(modelSummary) %in% c("tau2", "tau2_w", "tau2_b"),,drop=FALSE]

  for (i in 1:nrow(modelSummary)) {

    tempRow <- list(
      term     = .pemaVariableNames(rownames(modelSummary)[i], sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":"))),
      estimate = modelSummary[i, "mean"],
      se       = modelSummary[i, "sd"],
      lowerCI  = modelSummary[i, "2.5%"],
      upperCI  = modelSummary[i, "97.5%"],
      rhat     = modelSummary[i, "Rhat"],
      neff     = modelSummary[i, "n_eff"]
    )

    summaryTable$addRows(tempRow)
  }

  # check model fit
  parNames      <- rownames(rstan::summary(model$fit)$summary)
  divIterations <- rstan::get_num_divergent(model$fit)
  lowBmfi       <- rstan::get_low_bfmi_chains(model$fit)
  maxTreedepth  <- rstan::get_num_max_treedepth(model$fit)
  minESS        <- min(rstan::summary(model$fit)$summary[parNames != "sigma", "n_eff"])

  if (any(is.infinite(rstan::summary(model$fit)$summary[parNames != "sigma", "Rhat"])))
    maxRhat     <- Inf
  else
    maxRhat     <- max(rstan::summary(model$fit)$summary[parNames != "sigma", "Rhat"])

  if (divIterations != 0)
    summaryTable$addFootnote(.pemaMessageDivergentIter(divIterations), symbol = gettext("Warning:"))

  if (length(lowBmfi) != 0)
    summaryTable$addFootnote(.pemaMessageLowBMFI(length(lowBmfi)), symbol = gettext("Warning:"))

  if (maxTreedepth != 0)
    summaryTable$addFootnote(.pemaMessageMaxTreedepth(maxTreedepth))

  if (maxRhat > 1.01)
    summaryTable$addFootnote(.pemaMessageMaxRhat(maxRhat), symbol = gettext("Warning:"))

  if (minESS < 100 * options$mcmcChains || is.nan(minESS))
    summaryTable$addFootnote(.pemaMessageMinESS(minESS, 100 * options$mcmcChains), symbol = gettext("Warning:"))

  if (!is.null(jaspResults[["nOmitted"]]))
    summaryTable$addFootnote(gettextf("%1$s observations were ommited due to missing values.", jaspResults[["nOmitted"]]$object))

  return()
}
.pemaSummaryTableTau           <- function(jaspResults, options) {

  if (!is.null(jaspResults[["summaryTauTable"]]))
    return()

  model <- jaspResults[["model"]]$object

  summaryTauTable <- createJaspTable(title = gettext("Heterogeneity"))
  summaryTauTable$position <- 2
  summaryTauTable$dependOn(c(.pemaDependencies, "inferenceHeterogeneityTable"))

  summaryTauTable$addColumnInfo(name = "term",     title = "Term",           type = "string")
  summaryTauTable$addColumnInfo(name = "estimate", title = "Estimate",       type = "number")
  summaryTauTable$addColumnInfo(name = "lowerCI",  title = gettext("Lower"), type = "number", overtitle = gettextf("%s%% CI", 100 * .95))
  summaryTauTable$addColumnInfo(name = "upperCI",  title = gettext("Upper"), type = "number", overtitle = gettextf("%s%% CI", 100 * .95))

  jaspResults[["summaryTauTable"]] <- summaryTauTable

  if (is.null(model))
    return()
  else if (jaspBase::isTryError(model)) {
    summaryTable$setError(model)
    return()
  }

  modelSamples <- rstan::extract(model$fit)

  if (options[["clustering"]] == "") {

    tau2Samples  <- modelSamples$tau2

    summaryTauTable$addRows(list(
      term     = "\U1D6D5\U00B2",
      estimate = mean(tau2Samples),
      lowerCI  = quantile(tau2Samples, probs = .025),
      upperCI  = quantile(tau2Samples, probs = .975)
    ))

    summaryTauTable$addRows(list(
      term     = "\U1D6D5",
      estimate = mean(sqrt(tau2Samples)),
      lowerCI  = quantile(sqrt(tau2Samples), probs = .025),
      upperCI  = quantile(sqrt(tau2Samples), probs = .975)
    ))

  } else {

    tau2WSamples  <- modelSamples$tau2_w
    tau2BSamples  <- modelSamples$tau2_b

    summaryTauTable$addRows(list(
      term     = "\U1D6D5\U00B2 (within)",
      estimate = mean(tau2WSamples),
      lowerCI  = quantile(tau2WSamples, probs = .025),
      upperCI  = quantile(tau2WSamples, probs = .975)
    ))
    summaryTauTable$addRows(list(
      term     = "\U1D6D5\U00B2 (between)",
      estimate = mean(tau2BSamples),
      lowerCI  = quantile(tau2BSamples, probs = .025),
      upperCI  = quantile(tau2BSamples, probs = .975)
    ))

    summaryTauTable$addRows(list(
      term     = "\U1D6D5 (within)",
      estimate = mean(sqrt(tau2WSamples)),
      lowerCI  = quantile(sqrt(tau2WSamples), probs = .025),
      upperCI  = quantile(sqrt(tau2WSamples), probs = .975)
    ))
    summaryTauTable$addRows(list(
      term     = "\U1D6D5 (between)",
      estimate = mean(sqrt(tau2BSamples)),
      lowerCI  = quantile(sqrt(tau2BSamples), probs = .025),
      upperCI  = quantile(sqrt(tau2BSamples), probs = .975)
    ))

  }


  return()
}
.pemaSummaryTableI2            <- function(jaspResults, options) {

  if (!is.null(jaspResults[["summaryI2Table"]]))
    return()

  model <- jaspResults[["model"]]$object

  summaryI2Table <- createJaspTable(title = "I\U00B2")
  summaryI2Table$position <- 3
  summaryI2Table$dependOn(c(.pemaDependencies, "inferenceHeterogeneityTable", "inferenceHeterogeneityI2"))

  summaryI2Table$addColumnInfo(name = "term",     title = "Term",           type = "string")
  summaryI2Table$addColumnInfo(name = "estimate", title = "Estimate",       type = "number")
  summaryI2Table$addColumnInfo(name = "lowerCI",  title = gettext("Lower"), type = "number", overtitle = gettextf("%s%% CI", 100 * .95))
  summaryI2Table$addColumnInfo(name = "upperCI",  title = gettext("Upper"), type = "number", overtitle = gettextf("%s%% CI", 100 * .95))

  jaspResults[["summaryI2Table"]] <- summaryI2Table

  if (is.null(model))
    return()
  else if (jaspBase::isTryError(model)) {
    summaryTable$setError(model)
    return()
  }

  summaryI2 <- pema::I2(model)

  if (options[["clustering"]] == "") {

    summaryI2Table$addRows(list(
      term     = "I\U00B2",
      estimate = summaryI2["I2", "mean"],
      lowerCI  = summaryI2["I2", "2.5%"],
      upperCI  = summaryI2["I2", "97.5%"]
    ))

  } else {

    summaryI2Table$addRows(list(
      term     = "I\U00B2 (within)",
      estimate = summaryI2["I2_w", "mean"],
      lowerCI  = summaryI2["I2_w", "2.5%"],
      upperCI  = summaryI2["I2_w", "97.5%"]
    ))
    summaryI2Table$addRows(list(
      term     = "I\U00B2 (between)",
      estimate = summaryI2["I2_b", "mean"],
      lowerCI  = summaryI2["I2_b", "2.5%"],
      upperCI  = summaryI2["I2_b", "97.5%"]
    ))
    summaryI2Table$addRows(list(
      term     = "I\U00B2",
      estimate = summaryI2["I2mat", "mean"],
      lowerCI  = summaryI2["I2mat", "2.5%"],
      upperCI  = summaryI2["I2mat", "97.5%"]
    ))

  }


  return()
}
.pemaposteriorPlotsSelectedTerms             <- function(jaspResults, options) {

  if (!is.null(jaspResults[["posteriorPlots"]]))
    return()

  posteriorPlots <- createJaspContainer(title = gettext("Posterior distribution"))
  posteriorPlots$position <- 4
  posteriorPlots$dependOn(c(.pemaDependencies, "posteriorPlotsSelectedTerms"))
  jaspResults[["posteriorPlots"]] <- posteriorPlots

  model <- jaspResults[["model"]]$object

  stanFit          <- pema::as.stan(model)
  stanSamples      <- rstan::extract(stanFit)
  coefficientNames <- rownames(model$coefficients)

  posteriorVariables <- lapply(options[["posteriorPlotsSelectedTerms"]], function(var) var$variable)
  coefficients       <- lapply(posteriorVariables, function(posteriorVariable).pemaPredictorCoefficientNames(paste0(posteriorVariable, collapse = ":"), coefficientNames, options))
  coefficients       <- do.call(c, coefficients)

  # deal with two heterogeneity estimates in case that clustering is set
  if(any(coefficients == "tau2") && options[["clustering"]] != ""){
    coefficients <- coefficients[coefficients != "tau2"]
    coefficients <- c(coefficients, "tau2_w", "tau2_b")
  }


  for(i in seq_along(coefficients)){

    tempPlot <- rstan::stan_dens(stanFit, coefficients[i], separate_chains = FALSE, fill = NA, size = 1)

    parTicks <- jaspGraphs::getPrettyAxisBreaks(range(stanSamples[[coefficients[i]]]))

    tempPlot <- tempPlot +
      ggplot2::scale_x_continuous(.pemaVariableNames(
        coefficients[i],
        sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":"))),
        breaks = parTicks,
        limits = range(parTicks))
    tempPlot <- tempPlot + jaspGraphs::geom_rangeframe(sides = "b") + jaspGraphs::themeJaspRaw() +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y  = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )

    tempJaspPlot            <- createJaspPlot(title = "", width = 400, height = 300)
    tempJaspPlot$plotObject <- tempPlot

    posteriorPlots[[paste0("posterior", "_", i)]] <- tempJaspPlot
  }

  return()
}
.pemaDiagnostics               <- function(jaspResults, options) {

  if (!is.null(jaspResults[["diagnosticPlots"]]))
    return()

  diagnosticPlots <- createJaspContainer(title = gettext("Sampling diagnostics"))
  diagnosticPlots$position <- 5
  diagnosticPlots$dependOn(c(.pemaDependencies, "diagnosticsType", "scatterVariableX", "scatterVariableY"))
  jaspResults[["diagnosticPlots"]] <- diagnosticPlots

  model <- jaspResults[["model"]]$object

  if ((options[["diagnosticsType"]] == "scatter" && length(options[["scatterVariableY"]]) == 0) || is.null(model) || jaspBase::isTryError(model)) {
    diagnosticPlots[["emptyPlot"]] <- createJaspPlot()
    return()
  }

  stanFit          <- pema::as.stan(model)
  stanSamples      <- rstan::extract(stanFit)
  coefficientNames <- rownames(model$coefficients)

  if (options[["diagnosticsType"]] == "scatter") {

    coefficients1 <- .pemaPredictorCoefficientNames(paste0(options[["scatterVariableX"]][[1]]$variable, collapse = ":"), coefficientNames, options)
    coefficients2 <- .pemaPredictorCoefficientNames(paste0(options[["scatterVariableY"]][[1]]$variable, collapse = ":"), coefficientNames, options)

    # deal with two heterogeneity estimates in case that clustering is set
    if(coefficients1 == "tau2" && options[["clustering"]] != ""){
      coefficients1 <- c("tau2_w", "tau2_b")
    }
    if(coefficients2 == "tau2" && options[["clustering"]] != ""){
      coefficients2 <- c("tau2_w", "tau2_b")
    }


    for(i in seq_along(coefficients1)){
      for(j in seq_along(coefficients2)){

        tempPlot <- rstan::stan_scat(object = stanFit, pars = c(coefficients1[i], coefficients2[j]))

        xTicks <- jaspGraphs::getPrettyAxisBreaks(range(stanSamples[[coefficients1[i]]]))
        yTicks <- jaspGraphs::getPrettyAxisBreaks(range(stanSamples[[coefficients2[j]]]))

        tempPlot <- tempPlot +
          ggplot2::scale_x_continuous(.pemaVariableNames(
            coefficients1[i],
            sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":"))),
            breaks = xTicks,
            limits = range(xTicks)) +
          ggplot2::scale_y_continuous(.pemaVariableNames(
            coefficients2[j],
            sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":"))),
            breaks = yTicks,
            limits = range(yTicks))
        tempPlot <- tempPlot + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()

        tempJaspPlot            <- createJaspPlot(title = "", width = 400, height = 300)
        tempJaspPlot$plotObject <- tempPlot

        diagnosticPlots[[paste0("diagnostics", "_", i, "_", j)]] <- tempJaspPlot
      }
    }

  } else {

    coefficients <- .pemaPredictorCoefficientNames(paste0(options[["scatterVariableX"]][[1]]$variable, collapse = ":"), coefficientNames, options)

    # deal with two heterogeneity estimates in case that clustering is set
    if(coefficients == "tau2" && options[["clustering"]] != ""){
      coefficients <- c("tau2_w", "tau2_b")
    }


    for(i in seq_along(coefficients)){

      tempPlot <- switch(
        options[["diagnosticsType"]],
        "trace"           = rstan::traceplot(stanFit, coefficients[i]),
        "histogram"       = rstan::stan_hist(stanFit, coefficients[i]),
        "density"         = rstan::stan_dens(stanFit, coefficients[i], separate_chains = TRUE),
        "autocorrelation" = .fixStanAc(rstan::stan_ac(stanFit, coefficients[i]))
      )

      parTicks <- jaspGraphs::getPrettyAxisBreaks(range(stanSamples[[coefficients[i]]]))

      if (options[["diagnosticsType"]] %in% c("histogram", "density")) {
        tempPlot <- tempPlot +
          ggplot2::scale_x_continuous(.pemaVariableNames(
            coefficients[i],
            sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":"))),
            breaks = parTicks,
            limits = range(parTicks))
        tempPlot <- tempPlot + jaspGraphs::geom_rangeframe(sides = "b") + jaspGraphs::themeJaspRaw() +
          ggplot2::theme(
            axis.title.y = ggplot2::element_blank(),
            axis.text.y  = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
          )
      } else if (options[["diagnosticsType"]] %in% c("trace")) {
        tempPlot <- tempPlot +
          ggplot2::scale_y_continuous(.pemaVariableNames(
            coefficients[i],
            sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":"))),
            breaks = parTicks,
            limits = range(parTicks))
        tempPlot <- tempPlot + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw() +
          ggplot2::theme(plot.margin = ggplot2::margin(r = 10 * (nchar(options[["mcmcBurnin"]] + options[["mcmcSamples"]]) - 2)))
      } else if (options[["diagnosticsType"]] == "autocorrelation") {
        tempPlot <- tempPlot +
          ggplot2::scale_x_continuous(
            gettext("Lag"),
            breaks = jaspGraphs::getPrettyAxisBreaks(c(0, 25)),
            limits = c(-1, 26)) +
          ggplot2::scale_y_continuous(gettextf("Autocorrelation (%1$s)", .pemaVariableNames(
            coefficients[i],
            sapply(options[["modelTerms"]], function(term) paste0(term[[1]], collapse = ":")))),
            breaks = jaspGraphs::getPrettyAxisBreaks(c(0, 1)),
            limits = c(0, 1))
        tempPlot <- tempPlot + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()
      }

      tempJaspPlot            <- createJaspPlot(title = "", width = 400, height = 300)
      tempJaspPlot$plotObject <- tempPlot

      diagnosticPlots[[paste0("diagnostics", "_", i)]] <- tempJaspPlot
    }
  }

  return()
}
.pemaMethod                    <- function(options){
  return(switch(options[["method"]],
    "horseshoe" = "hs",
    "lasso"     = "lasso"
  ))
}
.fixStanAc <- function(gg) {

  if (identical(gg$layers[[1]]$stat_params[["fun.data"]], "mean_se")) {
    # fixes missing export from rstan:
    gg$layers[[1]]$stat_params[["fun.data"]] <- function(y) {
      data.frame(y = mean(y))
    }
  } else {
    warning("Rstan got updated! Check if results still work!", domain = NA)
  }

  return(gg)
}

.pemaMessageDivergentIter <- function(iterations) {
  sprintf(
    ngettext(
      iterations,
      "The Hamiltonian Monte Carlo procedure might be invalid -- There was %i divergent transition after warmup. This can be solved by carefully increasing 'Adapt delta' until there are no divergent transitions.",
      "The Hamiltonian Monte Carlo procedure might be invalid -- There were %i divergent transitions after warmup. This can be solved by carefully increasing 'Adapt delta' until there are no divergent transitions."
    ),
    iterations
  )
}
.pemaMessageLowBMFI       <- function(nChains) {
  sprintf(
    ngettext(
      nChains,
      "Bayesian Fraction of Missing Information (BFMI) that was too low in %i chain indicating that the posterior distribution was not explored efficiently. Try increasing number of 'Burnin' and 'Iterations'.",
      "Bayesian Fraction of Missing Information (BFMI) that was too low in %i chains indicating that the posterior distribution was not explored efficiently. Try increasing number of 'Burnin' and 'Iterations'."
    ),
    nChains
  )
}
.pemaMessageMaxTreedepth  <- function(iterations) {
  sprintf(
    ngettext(
      iterations,
      "The Hamiltonian Monte Carlo procedure might be inefficient -- %i transition exceeded the maximum tree depth. This can be solved by carefully increasing 'Maximum tree depth'.",
      "The Hamiltonian Monte Carlo procedure might be inefficient -- %i transitions exceeded the maximum tree depth. This can be solved by carefully increasing 'Maximum tree depth'."
    ),
    iterations
  )
}
.pemaMessageMaxRhat       <- function(Rhat) {
  gettextf(
    "Inference possibly unreliable -- MCMC chains might not have converged; The largest R-hat is %1$.3f > 1.01. To lower R-hat please increase 'Iterations', or 'Adapt delta' in the Options section.",
    Rhat
  )
}
.pemaMessageMinESS        <- function(ESS, treshold) {
  gettextf(
    "Low estimation accuracy -- The smallest Effective Sample Size (ESS) is %1$.2f < %2$1.0f. To increase accuracy please increase 'Iterations', or 'Adapt delta' in the Options section.",
    ESS,
    treshold
  )
}

