# WAAP-WLS reporting.
#
# Model-selection notes. The output uses the classical meta-analysis builders.

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
