# PET-PEESE reporting.
#
# Publication bias test, bias coefficient, and model-selection notes. The
# remaining output uses the classical meta-analysis builders.

# Publication bias test ----

.ppBiasTest             <- function(fit) {

  index <- match(.maBiasTerm(fit), rownames(fit[["beta"]]))
  ddf   <- .maExtractDdf(fit)

  # coefficient-specific degrees of freedom are only stored for cluster-robust fits
  return(list(
    stat = fit[["zval"]][index],
    df   = if (fit[["test"]] != "z") ddf[min(index, length(ddf))],
    pval = fit[["pval"]][index]
  ))
}

.ppBiasTestFit          <- function(fit, options) {

  # The publication bias test is the PET test of the standard error coefficient (also
  # when conditional PET-PEESE reports PEESE). Only an explicitly selected PEESE model
  # is tested via its own squared standard error coefficient.
  if (options[["publicationBiasAdjustment"]] == "peese")
    return(fit)

  return(attr(fit, "petFit"))
}

.ppBiasTestLabel        <- function(options) {

  if (options[["publicationBiasAdjustment"]] == "peese")
    return(gettext("Publication bias (PEESE)"))

  return(gettext("Publication bias (PET)"))
}

.ppRowBiasTest          <- function(fit, options) {

  biasFit <- .ppBiasTestFit(fit, options)
  row     <- data.frame(
    subgroup = attr(fit, "subgroup"),
    test     = .ppBiasTestLabel(options)
  )

  if (is.null(biasFit) || jaspBase::isTryError(biasFit))
    return(row)

  test <- .ppBiasTest(biasFit)

  cbind(row, data.frame(
    stat = if (is.null(test[["df"]])) {
      sprintf("z = %1$.2f", test[["stat"]])
    } else {
      sprintf("t(%1$s) = %2$.2f", .maPrintDf(test[["df"]]), test[["stat"]])
    },
    pval = test[["pval"]]
  ))
}

.ppPrintBiasTest        <- function(fit, options) {

  biasFit <- .ppBiasTestFit(fit, options)

  if (is.null(biasFit) || jaspBase::isTryError(biasFit))
    return(gettextf("%1$s: not available", .ppBiasTestLabel(options)))

  paste0(.ppBiasTestLabel(options), ": ", .maPrintCoefficientTest(.ppBiasTest(biasFit)))
}

# Bias coefficient ----

.ppExtractSummaryContainer <- function(jaspResults, options) {

  if (!is.null(jaspResults[["petPeeseSummary"]]))
    return(jaspResults[["petPeeseSummary"]])

  container          <- createJaspContainer(gettext("PET-PEESE Summary"))
  container$position <- 4
  container$dependOn(c(.ppDependencies, "includeFullDatasetInSubgroupAnalysis"))
  jaspResults[["petPeeseSummary"]] <- container

  return(container)
}

.ppCoefficientTable     <- function(jaspResults, options) {

  if (!options[["petPeeseCoefficientEstimates"]])
    return()

  container <- .ppExtractSummaryContainer(jaspResults, options)

  if (!is.null(container[["coefficientEstimates"]]))
    return()

  table <- createJaspTable(gettext("Bias Coefficient Estimates"))
  table$dependOn(c("petPeeseCoefficientEstimates", "confidenceIntervals", "standardErrors", "transformEffectSize"))
  table$showSpecifiedColumnsOnly      <- TRUE
  table$position                      <- 1
  container[["coefficientEstimates"]] <- table

  table$addColumnInfo(name = "par", title = "", type = "string")
  .maAddSubgroupColumn(table, options)
  table$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
  .maAddSeColumn(table, options, noTransformation = TRUE)
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

  table$setData(.maSafeOrderAndSimplify(.maSafeRbind(lapply(fits, .ppRowBiasEstimate, options = options)), "par", options))

  if (options[["transformEffectSize"]] != "none")
    table$addFootnote(gettext("The coefficient is not transformed."))

  return()
}

.ppRowBiasEstimate      <- function(fit, options) {

  candidate <- attr(fit, "petPeeseCandidate")

  if (jaspBase::isTryError(fit) || is.null(candidate))
    return(NULL)

  index <- match(.maBiasTerm(fit), rownames(fit[["beta"]]))
  test  <- .ppBiasTest(fit)

  data.frame(
    par      = switch(candidate, pet = "SE", peese = "SE\u00B2"),
    est      = fit[["beta"]][index, 1],
    se       = fit[["se"]][index],
    lCi      = fit[["ci.lb"]][index],
    uCi      = fit[["ci.ub"]][index],
    stat     = test[["stat"]],
    df       = if (is.null(test[["df"]])) NA_real_ else test[["df"]],
    pval     = test[["pval"]],
    subgroup = attr(fit, "subgroup")
  )
}

# Model and inference notes ----

.ppAddSubgroupFootnotes <- function(table, messages, fit, options) {

  for (message in messages) {
    if (options[["subgroup"]] != "")
      message <- gettextf("%1$s: %2$s", attr(fit, "subgroup"), message)

    table$addFootnote(message)
  }
}

.ppDecisionMessage      <- function(fit, options) {

  decision <- attr(fit, "petPeeseDecision")

  if (is.null(decision) || is.na(decision[["selected"]]))
    return(NULL)

  if (options[["publicationBiasAdjustment"]] != "petPeese")
    return(gettextf("Results are based on the %1$s model.", .ppCandidateLabel(decision[["selected"]])))

  gettextf(
    "Results are based on the %1$s model: the PET adjusted effect test resulted in %2$s (threshold %3$s).",
    .ppCandidateLabel(decision[["selected"]]),
    .maPrintPValue(decision[["pval"]]),
    format(options[["petPeeseThreshold"]])
  )
}

.ppAddInferenceFootnotes <- function(table, fits, options) {

  for (fit in fits) {
    .ppAddSubgroupFootnotes(table, c(
      .ppDecisionMessage(fit, options),
      .ppNoSelectionMessage(fit, options)
    ), fit, options)
  }
}

.ppNoSelectionMessage   <- function(fit, options) {

  if (options[["noSelection"]] == "" || jaspBase::isTryError(fit))
    return(NULL)

  selected <- .ppSelected(attr(fit, "dataset"), options)

  paste(
    sprintf(ngettext(sum(selected),
      "The bias adjustment applies to %1$i estimate.",
      "The bias adjustment applies to %1$i estimates."
    ), sum(selected)),
    sprintf(ngettext(sum(!selected),
      "%1$i estimate at level '%2$s' is unaffected.",
      "%1$i estimates at level '%2$s' are unaffected."
    ), sum(!selected), options[["noSelectionLevel"]])
  )
}
