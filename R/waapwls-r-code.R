# WAAP-WLS R code.
#
# Reproducible calls reuse the classical metafor call: the WLS model of all
# estimates, the power-based selection, and the WAAP model of the retained estimates.

.wwShowRCode                            <- function(jaspResults, options) {

  if (!.maReady(options) || !is.null(jaspResults[["metaforRCode"]]))
    return()

  html          <- createJaspHtml(title = gettext("Metafor R Code"))
  html$position <- 99
  html$dependOn(c(.wwDependencies, "showMetaforRCode", "includeFullDatasetInSubgroupAnalysis"))
  jaspResults[["metaforRCode"]] <- html

  fits <- .maExtractFit(jaspResults, options, nonClustered = TRUE)
  code <- vapply(names(fits), function(scope) {
    paste0(
      if (options[["subgroup"]] != "") {
        paste0("# ", gettextf("Subgroup: %1$s", gsub("[\r\n]", " ", attr(fits[[scope]], "subgroup"))), "\n")
      },
      .wwCode(fits[[scope]], options, subgroup = scope != "__fullDataset")
    )
  }, character(1))

  html$text <- .maTransformToHtml(paste(code, collapse = "\n"))

  return()
}

.wwFitCode                              <- function(options, dataName, rows) {

  fitCode <- .maMakeMetaforCallText(.ppModelOptions(options))
  fitCode <- gsub("data = dataset", paste0("data = ", dataName), fitCode, fixed = TRUE)
  fitCode <- gsub("dataset$", paste0(dataName, "$"), fitCode, fixed = TRUE)

  # a precomputed variance-covariance matrix corresponds to the complete data set
  if (length(rows) > 0 && .maIsMultilevelMultivariate(options) && options[["varianceCovarianceMatrixType"]] == "precomputed")
    fitCode <- sub(", header = FALSE))", paste0(", header = FALSE))", paste0("[", rows, ", ", rows, "]", collapse = "")), fitCode, fixed = TRUE)

  return(fitCode)
}

.wwSelectionCode                        <- function(fit, options, dataName) {

  # the power is evaluated at the effects of the WLS model
  wlsFit <- attr(fit, "candidateFits")[["wls"]]

  if (.maIsMetaregressionEffectSize(options) && options[["powerConditionalOnModerators"]]) {
    effectCode <- "fitted(fit)"
  } else if (.maIsMetaregressionEffectSize(options)) {
    newmods <- t(colMeans(stats::model.matrix(wlsFit)))
    if (wlsFit[["int.incl"]])
      newmods <- newmods[, -1, drop = FALSE]
    effectCode <- paste0("predict(fit, newmods = c(", paste(signif(newmods[1, ], 8), collapse = ", "), "))$pred")
  } else {
    effectCode <- "predict(fit)$pred"
  }

  paste0(
    "\n# ", gettextf("adequately powered estimates: power of a two-sided z-test (alpha = 0.05) of at least %1$s%%", format(100 * options[["powerCriterion"]])), "\n",
    "powerEffect <- ", effectCode, "\n",
    "waapRows    <- abs(powerEffect) / ", dataName, "$", options[["effectSizeStandardError"]], " >= ", signif(.wwCriticalZ(options[["powerCriterion"]]), 8),
    if (options[["noSelection"]] != "") {
      paste0(" | ", dataName, "$", options[["noSelection"]], " == ", .smCodeString(options[["noSelectionLevel"]]))
    },
    "\n",
    "waapData    <- droplevels(", dataName, "[waapRows, ])\n\n"
  )
}

.wwCode                                 <- function(fit, options, subgroup) {

  decision <- attr(fit, "waapWlsDecision")

  if (is.null(decision))
    return(paste0("# ", gettextf("R code unavailable: %1$s", .maTryCleanErrorMessages(fit)), "\n"))

  # subgroup models are fitted to the corresponding rows of the dataset
  dataName <- if (subgroup) "subgroupData" else "dataset"
  dataCode <- if (subgroup) {
    paste0(
      "subgroupRows <- dataset$", options[["subgroup"]], " == ", .smCodeString(attr(fit, "subgroup")), "\n",
      "subgroupData <- droplevels(dataset[subgroupRows, ])\n"
    )
  }
  rows     <- if (subgroup) "subgroupRows"

  code <- paste0(
    if (!jaspBase::isTryError(fit)) paste0("# ", gsub("[\r\n]+\\s*", " ", .wwDecisionMessage(fit, options)), "\n"),
    dataCode,
    .wwFitCode(options, dataName, rows)
  )

  if (jaspBase::isTryError(fit))
    return(paste0(code, "# ", gettextf("The module reported: %1$s", .maTryCleanErrorMessages(fit)), "\n"))

  if (decision[["selected"]] == "waap") {
    code <- paste0(
      code,
      .wwSelectionCode(fit, options, dataName),
      .wwFitCode(options, "waapData", c(rows, "waapRows"))
    )
  }

  # the reported estimate is the pooled effect (at the mean of the moderators)
  if (!.maIsMetaregressionEffectSize(options))
    return(paste0(code, "\npredict(fit)\n"))

  newmods <- t(colMeans(stats::model.matrix(fit)))
  if (fit[["int.incl"]])
    newmods <- newmods[, -1, drop = FALSE]

  paste0(code, "\npredict(fit, newmods = c(", paste(signif(newmods[1, ], 8), collapse = ", "), "))\n")
}
