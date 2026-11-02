# WAAP-WLS reporting.
#
# Model-selection notes and the WLS/WAAP comparison table. The remaining output
# uses the classical meta-analysis builders.

# Model and inference notes ----

.wwDecisionMessage       <- function(fit, options) {

  decision  <- attr(fit, "waapWlsDecision")
  selection <- decision[["selection"]]

  if (is.null(decision) || jaspBase::isTryError(fit))
    return(NULL)

  if (decision[["selected"]] == "waap") {
    message <- gettext("Results are based on the WAAP model.")
  } else if (!is.null(decision[["message"]])) {
    message <- gettextf("Results are based on the WLS model. %1$s", decision[["message"]])
  } else if (options[["noSelection"]] == "") {
    message <- gettextf("Results are based on the WLS model: the WAAP model requires at least %1$i adequately powered estimates.", decision[["required"]])
  } else {
    message <- gettextf("Results are based on the WLS model: the WAAP model requires at least %1$i adequately powered or unaffected estimates.", decision[["required"]])
  }

  if (is.null(selection))
    return(message)

  paste(c(
    message,
    sprintf(ngettext(sum(selection[["powered"]]),
      "%1$i of %2$i estimates is adequately powered (power ≥ %3$s%%).",
      "%1$i of %2$i estimates are adequately powered (power ≥ %3$s%%)."
    ), sum(selection[["powered"]]), sum(selection[["selected"]]), format(100 * options[["powerCriterion"]])),
    .wwPowerEffectMessage(options)
  ), collapse = " ")
}

.wwPowerEffectMessage    <- function(options) {

  if (!.maIsMetaregressionEffectSize(options))
    return(NULL)

  if (options[["powerConditionalOnModerators"]])
    return(gettext("The power is evaluated at the moderator-specific predicted effects of the WLS model."))

  return(gettext("The power is evaluated at the pooled effect of the WLS model."))
}

.wwNoSelectionMessage    <- function(fit, options) {

  selection <- attr(fit, "waapWlsDecision")[["selection"]]

  if (options[["noSelection"]] == "" || is.null(selection))
    return(NULL)

  sprintf(ngettext(sum(!selection[["selected"]]),
    "%1$i estimate at level '%2$s' is unaffected and always retained.",
    "%1$i estimates at level '%2$s' are unaffected and always retained."
  ), sum(!selection[["selected"]]), options[["noSelectionLevel"]])
}

.wwAddInferenceFootnotes <- function(table, fits, options) {

  for (fit in fits) {
    .ppAddSubgroupFootnotes(table, c(
      .wwDecisionMessage(fit, options),
      .wwNoSelectionMessage(fit, options)
    ), fit, options)
  }
}

# WLS and WAAP estimates ----

.wwEstimatesTable        <- function(jaspResults, options) {

  if (!options[["waapWlsEstimates"]] || !is.null(jaspResults[["waapWlsEstimates"]]))
    return()

  table <- createJaspTable(gettext("WLS and WAAP Estimates"))
  table$dependOn(c(.wwDependencies, "waapWlsEstimates", "includeFullDatasetInSubgroupAnalysis",
                   "confidenceIntervals", "confidenceIntervalsLevel", "standardErrors", "transformEffectSize"))
  table$showSpecifiedColumnsOnly <- TRUE
  table$position                 <- 4
  jaspResults[["waapWlsEstimates"]] <- table

  table$addColumnInfo(name = "par", title = "", type = "string")
  .maAddSubgroupColumn(table, options)
  table$addColumnInfo(name = "k",   title = gettext("k"), type = "integer")
  table$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
  .maAddSeColumn(table, options)
  .maAddCiColumn(table, options)
  table$addColumnInfo(name = "stat", title = if (.maIsMetaregressionFtest(options)) gettext("t") else gettext("z"), type = "number")
  if (.maIsMetaregressionFtest(options))
    table$addColumnInfo(name = "df", title = gettext("df"), type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"), type = "pvalue")

  fits <- .maExtractFit(jaspResults, options)

  if (is.null(fits))
    return()

  if (length(fits) == 1 && jaspBase::isTryError(fits[[1]])) {
    table$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  table$setData(.maSafeOrderAndSimplify(.maSafeRbind(lapply(fits, .wwRowEstimates, options = options)), "par", options))

  table$addFootnote(gettextf(
    "WLS includes all estimates, WAAP the adequately powered estimates (power ≥ %1$s%%)%2$s. The reported model is WAAP if it is estimable, WLS otherwise.",
    format(100 * options[["powerCriterion"]]),
    if (options[["noSelection"]] != "") gettext(" and the unaffected estimates") else ""
  ))
  if (.maIsMetaregressionEffectSize(options))
    table$addFootnote(gettext("The estimates are evaluated at the mean of the moderators."))

  return()
}

.wwRowEstimates          <- function(fit, options) {

  decision <- attr(fit, "waapWlsDecision")

  if (is.null(decision))
    return(NULL)

  # a single estimate, also for models with multiple heterogeneity levels
  options[["predictionIntervals"]] <- FALSE

  selection <- decision[["selection"]]

  rows <- lapply(c("wls", "waap"), function(candidate) {

    candidateFit <- attr(fit, "candidateFits")[[candidate]]
    row          <- data.frame(
      par      = .wwCandidateLabel(candidate),
      subgroup = attr(fit, "subgroup"),
      k        = if (!is.list(selection)) NA_integer_ else if (candidate == "wls") length(selection[["retained"]]) else sum(selection[["retained"]])
    )

    # the WAAP estimate is shown only for the reported (estimable) WAAP model
    if (is.null(candidateFit) || jaspBase::isTryError(candidateFit) || (candidate == "waap" && decision[["selected"]] != "waap"))
      return(row)

    estimate <- try(.maComputePooledEffectPlot(candidateFit, options))
    if (jaspBase::isTryError(estimate))
      return(row)

    cbind(row, data.frame(
      est  = estimate[["est"]],
      se   = estimate[["se"]],
      lCi  = estimate[["lCi"]],
      uCi  = estimate[["uCi"]],
      stat = estimate[["stat"]],
      df   = if (is.null(estimate[["df"]])) NA_real_ else estimate[["df"]],
      pval = estimate[["pval"]]
    ))
  })

  return(.maSafeRbind(rows))
}
