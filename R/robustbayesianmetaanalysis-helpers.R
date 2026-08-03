# Robust Bayesian meta-analysis shared helpers.
#
# Containers, predicates, names, table rows, formatting, and warnings.

.robmaExtractModelSummaryContainer           <- function(jaspResults) {

  if (!is.null(jaspResults[["modelSummaryContainer"]]))
    return(jaspResults[["modelSummaryContainer"]])

  # create the output container
  modelSummaryContainer <- createJaspContainer(gettext("Model Summary"))
  modelSummaryContainer$dependOn(.robmaDependencies)
  modelSummaryContainer$position <- 2
  jaspResults[["modelSummaryContainer"]] <- modelSummaryContainer

  return(modelSummaryContainer)
}

.robmaExtractMetaregressionContainer         <- function(jaspResults) {

  if (!is.null(jaspResults[["metaregressionContainer"]]))
    return(jaspResults[["metaregressionContainer"]])

  # create the output container
  metaregressionContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  metaregressionContainer$dependOn(c(.robmaDependencies))
  metaregressionContainer$position <- 3
  jaspResults[["metaregressionContainer"]] <- metaregressionContainer

  return(metaregressionContainer)
}

.robmaExtractPublicationBiasContainer        <- function(jaspResults) {

  if (!is.null(jaspResults[["publicationBiasContainer"]]))
    return(jaspResults[["publicationBiasContainer"]])

  # create the output container
  publicationBiasContainer <- createJaspContainer(gettext("Publication Bias Adjustment Summary"))
  publicationBiasContainer$dependOn(c(.robmaDependencies, "confidenceIntervals"))
  publicationBiasContainer$position <- 4
  jaspResults[["publicationBiasContainer"]] <- publicationBiasContainer

  return(publicationBiasContainer)
}

.robmaExtractEstimatedMarginalMeansContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["estimatedMarginalMeansContainer"]]))
    return(jaspResults[["estimatedMarginalMeansContainer"]])

  # create the output container
  estimatedMarginalMeansContainer <- createJaspContainer(gettext("Estimated Marginal Means Summary"))
  estimatedMarginalMeansContainer$dependOn(c(.robmaDependencies, "confidenceIntervals", "confidenceIntervalsLevel", "includeFullDatasetInSubgroupAnalysis"))
  estimatedMarginalMeansContainer$position <- 5
  jaspResults[["estimatedMarginalMeansContainer"]] <- estimatedMarginalMeansContainer

  return(estimatedMarginalMeansContainer)
}

.robmaExtractPriorAndPosteriorPlotContainer  <- function(jaspResults) {

  if (!is.null(jaspResults[["priorAndPosteriorPlotContainer"]]))
    return(jaspResults[["priorAndPosteriorPlotContainer"]])

  # create the output container
  priorAndPosteriorPlotContainer <- createJaspContainer(gettext("Prior and Posterior Plots"))
  priorAndPosteriorPlotContainer$dependOn(c(.robmaDependencies, "includeFullDatasetInSubgroupAnalysis", "priorAndPosteriorPlotType", "priorAndPosteriorPlotIncludePriorDistribution"))
  priorAndPosteriorPlotContainer$position <- 6
  jaspResults[["priorAndPosteriorPlotContainer"]] <- priorAndPosteriorPlotContainer

  return(priorAndPosteriorPlotContainer)
}

.robmaExtractDiagnosticsContainer            <- function(jaspResults) {

  if (!is.null(jaspResults[["diagnosticsContainer"]]))
    return(jaspResults[["diagnosticsContainer"]])

  # create the output container
  diagnosticsContainer <- createJaspContainer(gettext("Diagnostics"))
  diagnosticsContainer$dependOn(.robmaDependencies)
  diagnosticsContainer$position <- 8
  jaspResults[["diagnosticsContainer"]] <- diagnosticsContainer

  return(diagnosticsContainer)
}

.robmaExtractDiagnosticsSubContainer         <- function(diagnosticsContainer, parameter, title, position, dependencies) {

  if (!is.null(diagnosticsContainer[[parameter]]))
    return(diagnosticsContainer[[parameter]])

  # create the output container
  diagnosticsSubContainer <- createJaspContainer(title)
  diagnosticsSubContainer$dependOn(dependencies)
  diagnosticsSubContainer$position <- position
  diagnosticsContainer[[parameter]] <- diagnosticsSubContainer

  return(diagnosticsSubContainer)
}

.robmaIsMetaregressionCentered         <- function(options){

  if (!.maIsMetaregression(options))
    return(FALSE)

  priors          <- attr(options, "priors")
  priorModerators <- priors[["moderators"]]

  for (priorModerator in priorModerators) {
    for (i in seq_along(priorModerator)) {
      if (BayesTools::is.prior.treatment(priorModerator[[i]]) || BayesTools::is.prior.independent(priorModerator[[i]]))
        return(FALSE)
    }
  }

  return(TRUE)
}

.robmaGetEstimatedMarginalMeansOptions <- function(options){

  return(options[c(
    "estimatedMarginalMeansEffectSizeTestAgainst0",
    "estimatedMarginalMeansEffectSizeAddAdjustedEstimate",

    "conditionalEstimates",
    "bayesFactorType",
    "confidenceIntervals",
    "confidenceIntervalsLevel",
    "transformEffectSize"
  )])
}

.robmaEstimatedMarginalMeansWarnings   <- function(fit, options, parameter) {

  if (parameter == "")
    parameter <- "intercept"

  messages <- NULL

  if (options[["subgroup"]] == "") {

    tempFit      <- fit[[1]]
    tempWarnings <- attr(tempFit, "warnings")
    tempWarnings <- gsub("mu_", "", tempWarnings)
    messages     <- tempWarnings[grep(parameter, tempWarnings)]

  } else {
    for (i in seq_along(fit)) {

      tempFit      <- fit[[i]]
      tempWarnings <- attr(tempFit, "warnings")
      tempWarnings <- gsub("mu_", "", tempWarnings)
      tempWarnings <- tempWarnings[grep(parameter, tempWarnings)]

      if (length(tempWarnings) > 0) {
        messages <- c(messages, sapply(tempWarnings, function(x) gettextf("Subgroup %1$s, %2$s", tempFit$subgroup[1], x)))
      }
    }
  }

  if (parameter == "intercept")
    messages <- messages[!grepl("do not span", messages)]

  return(messages)
}

.robmaComponentNames      <- function(component, options) {
  return(switch(
    tolower(component),
    "effectsize"    = if (.maIsMetaregression(options) && .robmaIsMetaregressionCentered(options)) gettext("Adjusted effect") else if (.maIsMetaregression(options)) gettext("Effect intercept") else gettext("Pooled effect"),
    "effect"        = if (.maIsMetaregression(options) && .robmaIsMetaregressionCentered(options)) gettext("Adjusted effect") else if (.maIsMetaregression(options)) gettext("Effect intercept") else gettext("Pooled effect"),
    "heterogeneity" = gettext("Heterogeneity"),
    "bias"          = gettext("Publication bias"),
    "baseline"      = gettext("Baseline")
  ))
}

.robmaVariableNames       <- function(varNames, variables) {

  return(sapply(varNames, function(varName){

    if (varName %in% c("intrcpt", "intercept"))
      return(gettext("Intercept"))
    #     # TODO: figure out how to handle this when interactions are present
    #     # (will need ignoring inside of square brackets for [dif: A] or [A])
    #     for (vn in variables) {
    #       inf <- regexpr(vn, varName, fixed = TRUE)
    #
    #       if (inf[1] != -1) {
    #         varName <- paste0(
    #           substr(varName, 0, inf[1] - 1),
    #           substr(varName, inf[1], inf[1] + attr(inf, "match.length") - 1),
    #           " (",
    #           substr(varName, inf[1] + attr(inf, "match.length"), nchar(varName))
    #         )
    #       }
    #
    #     }
    #
    #     varName <- gsub(":", paste0(")", jaspBase::interactionSymbol), varName, fixed = TRUE)
    #     varName <- paste0(varName, ")")
    #     varName <- gsub(" ()", "", varName, fixed = TRUE)
    #     varName <- gsub(" (/", "/", varName, fixed = TRUE)

    return(varName)

  }))
}

.robmaHasHeterogeneity    <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["heterogeneity"]])) {
    if (priors[["heterogeneity"]][[i]][["distribution"]] != "point" ||
        (priors[["heterogeneity"]][[i]][["distribution"]] == "point" && priors[["heterogeneity"]][[i]][["parameters"]][["location"]] != 0))
      return(TRUE)
  }

  return(FALSE)
}

.robmaHasWeightfunction   <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["bias"]])) {
    if (BayesTools::is.prior.weightfunction(priors[["bias"]][[i]]))
      return(TRUE)
  }

  return(FALSE)
}

.robmaHasPetPeese         <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["bias"]])) {
    if (BayesTools::is.prior.PET(priors[["bias"]][[i]]) || BayesTools::is.prior.PEESE(priors[["bias"]][[i]]))
      return(TRUE)
  }

  return(FALSE)
}

.robmaHasPet              <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["bias"]])) {
    if (BayesTools::is.prior.PET(priors[["bias"]][[i]]))
      return(TRUE)
  }

  return(FALSE)
}

.robmaHasPeese            <- function(options) {

  priors <- attr(options, "priors")

  for (i in seq_along(priors[["bias"]])) {
    if (BayesTools::is.prior.PEESE(priors[["bias"]][[i]]))
      return(TRUE)
  }

  return(FALSE)
}

.robmaAddBfColumn         <- function(tempTable, options) {

  titleBF <- switch(
    options[["bayesFactorType"]],
    "BF10"    = gettext("Inclusion BF"),
    "BF01"    = gettext("Exclusion BF"),
    "LogBF10" = gettext("log(Inclusion BF)")
  )

  tempTable$addColumnInfo(name = "bf", title = titleBF, type = "number")

  return()
}

.robmaPrintBf             <- function(bf) {

  bf <- sapply(bf, function(x) {
    if (is.na(x)) {
      return("NA")
    } else if (is.infinite(x) && x > 0) {
      return("\U221E")
    } else if (is.infinite(x) && x < 0) {
      return("-\U221E")
    } else if (x < 1) {
      return(sprintf("%1$.3f", x))
    } else if (x < 10) {
      return(sprintf("%1$.2f", x))
    } else if (x < 100) {
      return(sprintf("%1$.1f", x))
    } else if (x < 100) {
      return(sprintf("%1$.1f", x))
    } else if (x < 1e5){
      return(sprintf("%1$.0f", x))
    } else {
      return(sprintf("%1$.3g", x))
    }
  })
  return(bf)
}

.robmaPrintBfTest         <- function(out, options) {

  bfText <- switch(
    options[["bayesFactorType"]],
    "BF10"    = "BF\U2081\U2080",
    "BF01"    = "BF\U2080\U2081",
    "LogBF10" = "logBF\U2081\U2080"
  )

  return(sprintf("%1$s = %2$s", bfText, .robmaPrintBf(out[["bf"]])))
}

.robmaPrintTest           <- function(fit, options, component, includeName = TRUE) {

  out <- .robmaRowTests(fit, options)
  out <- out[out$test == .robmaComponentNames(component, options),, drop = FALSE]

  if (includeName) {
    return(sprintf("%1$s: %2$s", out[["test"]], .robmaPrintBfTest(out, options)))
  } else {
    return(.robmaPrintBfTest(out, options))
  }
}

.robmaPrintPooledEstimate <- function(fit, options, digits, parameter, conditional) {

  options[["heterogeneityTau"]]    <- parameter == "tau"
  options[["heterogeneityTau2"]]   <- parameter == "tau2"
  options[["heterogeneityI2"]]     <- parameter == "I2"
  options[["heterogeneityH2"]]     <- parameter == "H2"

  out <- .robmaRowPooledEstimates(fit, options, conditional)
  out <- out[rownames(out) == switch(
    parameter,
    "effect" = "effectSize",
    "tau"    = "heterogeneityTau",
    "tau2"   = "heterogeneityTau2",
    "I2"     = "heterogeneityI2",
    "H2"     = "heterogeneityH2"
  ),,drop = FALSE]

  return(sprintf(paste0(
    "%1$s  = ",
    "%2$.", digits, "f",
    " [",
    "%3$.", digits, "f",
    ", ",
    "%4$.", digits, "f",
    "]"
  ), out$par, out$mean, out$lCi, out$uCi))
}

.robmaLargeBayesFactorWarning <- function() {
  return(gettext("Large Bayes factors, i.e., BF > 100 or BF < 1/100, might be computationally unstable (a small change in posterior inclusion probability corresponds to a very large change in the Bayes factor). Consider increasing the number of chains and posterior samples to obtain more stable estimates."))
}
