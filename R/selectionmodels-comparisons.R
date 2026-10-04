# Selection-model comparisons.
#
# Reports information-criterion weights and publication-bias comparisons.

# Null models and parameter restrictions ----

.smNoBiasNested                         <- function(fit) {

  if (!inherits(fit, "rma.uni.selmodel"))
    return(FALSE)

  spec <- attr(fit, "selectionSpecification")

  if (!length(spec[["delta"]]))
    return(TRUE)

  if (spec[["type"]] == "negexppow")
    return(any(is.na(spec[["delta"]]) | spec[["delta"]] == 0))

  null <- switch(
    spec[["type"]],
    beta      = c(1, 1),
    stepfun   = rep(1, length(fit$delta)),
    trunc     = 1,
    truncest  = c(1, NA),
    0
  )
  fixed <- !is.na(spec[["delta"]]) & !is.na(null)

  all(spec[["delta"]][fixed] == null[fixed])
}

.smPublicationBiasComparison             <- function(fit) {

  list(
    stat = fit$LRT,
    df   = fit$LRTdf,
    pval = fit$LRTp
  )
}

.smPrintLikelihoodRatio                  <- function(comparison) {

  if (!is.finite(comparison$stat))
    return(NA_character_)

  if (is.finite(comparison$df) && comparison$df > 0)
    return(sprintf("LR(%i) = %.2f", comparison$df, comparison$stat))

  sprintf("LR = %.2f", comparison$stat)
}

# Comparison rows and tables ----

.smFitStatistics                        <- function(fit) {

  if (inherits(fit, "try-error"))
    return(c(k = NA, parameters = NA, logLik = NA, dev = NA, AIC = NA, BIC = NA, AICc = NA))

  statistics <- fit[["fit.stats"]]

  # rma.uni reports deviance relative to the saturated model; selmodel reports
  # -2 log-likelihood. Use the latter for every candidate.
  c(
    k          = fit[["k"]],
    parameters = fit[["parms"]],
    logLik     = statistics["ll", "ML"],
    dev        = -2 * statistics["ll", "ML"],
    AIC        = statistics["AIC", "ML"],
    BIC        = statistics["BIC", "ML"],
    AICc       = statistics["AICc", "ML"]
  )
}

.smInformationCriterionWeights          <- function(values) {

  weights  <- rep(NA_real_, length(values))
  eligible <- which(is.finite(values))

  if (length(eligible)) {
    relative <- exp(-.5 * (values[eligible] - min(values[eligible])))
    weights[eligible] <- relative / sum(relative)
  }

  weights
}

.smComparisonRows                       <- function(fits, options) {

  rows <- list()

  for (scope in .smVisibleScopes(fits, options)) {

    scopeRows <- lapply(seq_along(scope$models), function(i) {

      fit        <- scope$models[[i]]
      statistics <- .smFitStatistics(fit)[c("parameters", "logLik", "dev", "AIC", "AICc", "BIC")]

      data.frame(
        model    = gettextf("Model %1$i", i),
        subgroup = scope$label,
        as.list(statistics),
        check.names = FALSE
      )
    })
    scopeRows <- do.call(rbind, scopeRows)

    if (is.null(scopeRows))
      next

    for (criterion in c("AIC", "AICc", "BIC"))
      scopeRows[[paste0("weight", criterion)]] <- .smInformationCriterionWeights(scopeRows[[criterion]])

    rows[[length(rows) + 1L]] <- scopeRows
  }

  .maSafeOrderAndSimplify(.maSafeRbind(rows), "model", options)
}

.smComparisonTable                     <- function(jaspResults, fits, options, specifications) {

  if (!.smHasMultipleModels(options) && length(specifications))
    return()

  if (!options[["modelComparison"]])
    return()

  if (!is.null(jaspResults[["selectionComparison"]]))
    return()

  table <- .smTable(
    container = jaspResults,
    key       = "selectionComparison",
    title     = gettext("Model Comparison"),
    columns   = c(
      list(model = gettext("Model")),
      if (options[["subgroup"]] != "") list(subgroup = gettext("Subgroup")),
      list(
        parameters = gettext("Parameters"),
        logLik     = gettext("Log Lik."),
        dev        = gettext("Deviance"),
        AIC        = "AIC",
        AICc       = "AICc",
        BIC        = "BIC",
        weightAIC  = gettext("AIC weight"),
        weightAICc = gettext("AICc weight"),
        weightBIC  = gettext("BIC weight")
      )
    ),
    strings = c("model", "subgroup")
  )
  table$position <- 1
  table$dependOn(c(.smDependencies, "modelComparison", "includeFullDatasetInSubgroupAnalysis"))
  table$addCitation("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48.")

  if (!length(specifications)) {
    table$setError(gettext("Add at least one weight-function model in the Custom section."))
    return()
  }

  if (is.null(fits))
    return()

  table$setData(.smComparisonRows(fits, options))

  for (i in seq_along(specifications))
    table$addFootnote(gettextf("Model %1$i: %2$s", i, .smSpecificationLabel(specifications[[i]])))

  for (scope in .smVisibleScopes(fits, options)) {
    for (i in seq_along(scope$models)) {

      fit      <- scope$models[[i]]
      messages <- c(
        if (inherits(fit, "try-error")) as.character(fit),
        attr(fit, "selectionWarnings")
      )

      for (message in messages)
        table$addFootnote(gettextf("Model %1$i, %2$s: %3$s", i, scope$label, message))
    }
  }

}
