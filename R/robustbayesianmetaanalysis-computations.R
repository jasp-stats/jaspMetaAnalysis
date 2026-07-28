# Robust Bayesian meta-analysis computations.
#
# Computes pooled, adjusted, intercept, and term-level model summaries.

.robmaComputePooledEffect           <- function(fit, options, conditional, returnRaw = FALSE) {

  if (!.maIsMetaregression(options)) {
    # the adjusted estimate corresponds to the pooled estimate for non-regression models
    return(.robmaComputeInterceptEffect(fit, options, conditional, returnRaw))
  }

  # the following function is necessary only for a meta-regression
  estimate <- RoBMA::pooled_effect(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]

  estimate <- data.frame(
    "par"    = gettext("Pooled effect"),
    "mean"   = estimate[["Mean"]][1],
    "median" = estimate[["Median"]][1],
    "lCi"    = estimate[[3]][1],
    "uCi"    = estimate[[4]][1],
    "lPi"    = estimate[[3]][2],
    "uPi"    = estimate[[4]][2]
  )

  # return for the plotting function: requires different post-formatting
  if (returnRaw) {
    return(estimate)
  }

  # to data.frame
  estimate <- data.frame(estimate)

  # apply effect size transformation
  if (options[["transformEffectSize"]] != "none")
    estimate[,c("mean", "median", "lCi", "uCi", "lPi", "uPi")] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(estimate[,c("mean", "median", "lCi", "uCi", "lPi", "uPi")]))

  # remove non-requested columns
  estimate <- estimate[,c(
    "par", "mean", "median",
    if (options[["confidenceIntervals"]]) c("lCi", "uCi"),
    if (options[["predictionIntervals"]]) c("lPi", "uPi")
  )]

  return(as.list(estimate))
}

.robmaComputeAdjustedEffect         <- function(fit, options, conditional, returnRaw = FALSE) {

  if (!.maIsMetaregression(options) || (.maIsMetaregression(options) && .robmaIsMetaregressionCentered(options))) {
    # the adjusted estimate corresponds to the pooled estimate for non-regression models
    # and to the intercept estimate for centered regression models
    return(.robmaComputeInterceptEffect(fit, options, conditional, returnRaw))
  }

  # the following function is necessary only for a meta-regression
  estimate <- RoBMA::adjusted_effect(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]

  estimate <- data.frame(
    "par"    = gettext("Adjusted effect"),
    "mean"   = estimate[["Mean"]][1],
    "median" = estimate[["Median"]][1],
    "lCi"    = estimate[[3]][1],
    "uCi"    = estimate[[4]][1],
    "lPi"    = estimate[[3]][2],
    "uPi"    = estimate[[4]][2]
  )

  # return for the plotting function: requires different post-formatting
  if (returnRaw) {
    return(estimate)
  }

  # to data.frame
  estimate <- data.frame(estimate)

  # apply effect size transformation
  if (options[["transformEffectSize"]] != "none")
    estimate[,c("mean", "median", "lCi", "uCi", "lPi", "uPi")] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(estimate[,c("mean", "median", "lCi", "uCi", "lPi", "uPi")]))

  # remove non-requested columns
  estimate <- estimate[,c(
    "par", "mean", "median",
    if (options[["confidenceIntervals"]]) c("lCi", "uCi"),
    if (options[["predictionIntervals"]]) c("lPi", "uPi")
  )]

  return(as.list(estimate))
}

.robmaComputeInterceptEffect        <- function(fit, options, conditional, returnRaw = FALSE) {

  # effect size summary
  fitSummary <- summary(
    fit,
    conditional = conditional,
    probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
  )[[if (conditional) "estimates_conditional" else "estimates"]]
  fitSummary <- fitSummary[rownames(fitSummary) == "mu",,drop=FALSE]

  estimate <- list(
    par    = .robmaComponentNames("effect", options),
    mean   = fitSummary[["Mean"]],
    median = fitSummary[["Median"]],
    lCi    = fitSummary[[3]],
    uCi    = fitSummary[[4]]
  )

  # prediction intervals
  if (options[["predictionIntervals"]]) {

    hetSummary <- RoBMA::summary_heterogeneity(
      fit,
      conditional = conditional,
      probs       = c(.5 + c(-1, 1) * options[["confidenceIntervalsLevel"]] / 2)
    )[[if (conditional) "estimates_conditional" else "estimates"]]

    estimate$lPi <- hetSummary["PI", 3]
    estimate$uPi <- hetSummary["PI", 4]

  } else {
    estimate$lPi <- NA
    estimate$uPi <- NA
  }

  # return for the plotting function: requires different post-formatting
  if (returnRaw) {
    return(estimate)
  }

  # to data.frame
  estimate <- data.frame(estimate)

  # apply effect size transformation
  if (options[["transformEffectSize"]] != "none")
    estimate[,c("mean", "median", "lCi", "uCi", "lPi", "uPi")] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(estimate[,c("mean", "median", "lCi", "uCi", "lPi", "uPi")]))

  # remove non-requested columns
  estimate <- estimate[,c(
    "par", "mean", "median",
    if (options[["confidenceIntervals"]]) c("lCi", "uCi"),
    if (options[["predictionIntervals"]]) c("lPi", "uPi")
  )]

  return(as.list(estimate))
}
