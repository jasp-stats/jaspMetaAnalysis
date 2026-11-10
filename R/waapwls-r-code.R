# WAAP-WLS R code.
#
# Reproducible calls reuse the classical metafor call: the WLS model of all
# estimates and, if reported, the WAAP model of the retained estimates.

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

  # estimates with a missing no-selection indicator are excluded from all models
  if (options[["noSelection"]] != "")
    code <- c(paste0("dataset <- dataset[!is.na(dataset$", options[["noSelection"]], "), ]\n"), code)

  html$text <- .maTransformToHtml(paste(code, collapse = "\n"))

  return()
}

.wwFitCode                              <- function(options, dataName, rows, waap = FALSE) {

  fitCode <- .maMakeMetaforCallText(.ppModelOptions(options))
  fitCode <- gsub("data = dataset", paste0("data = ", dataName), fitCode, fixed = TRUE)
  fitCode <- gsub("dataset$", paste0(dataName, "$"), fitCode, fixed = TRUE)

  if (!.maIsMultilevelMultivariate(options) || !.mammVarianceCovarianceMatrixReady(options))
    return(fitCode)

  if (waap) {
    # the WAAP model uses the retained rows of the variance-covariance matrix of all estimates
    fitCode <- sub("^[\\s\\S]*?(?=fit <- rma\\.mv\\()", "", fitCode, perl = TRUE)
    fitCode <- sub("V = effectSizeVarianceCovarianceMatrix", "V = effectSizeVarianceCovarianceMatrix[waapRows, waapRows]", fitCode, fixed = TRUE)
  } else if (!is.null(rows) && options[["varianceCovarianceMatrixType"]] == "precomputed") {
    # a precomputed variance-covariance matrix corresponds to the complete data set
    fitCode <- sub(", header = FALSE))", paste0(", header = FALSE))[", rows, ", ", rows, "]"), fitCode, fixed = TRUE)
  }

  return(fitCode)
}

.wwSelectionCode                        <- function(options, dataName) {

  # the power is evaluated at the predicted effects of the WLS model (or at its pooled effect at the mean of the moderators)
  effectCode <- "predict(fit)$pred"
  if (.maIsMetaregressionEffectSize(options) && !options[["powerConditionalOnModerators"]])
    effectCode <- paste0("predict(fit, newmods = colMeans(model.matrix(fit))", if (options[["effectSizeModelIncludeIntercept"]]) "[-1]", ")$pred")

  paste0(
    "\n",
    "z        <- abs(", effectCode, ") / ", dataName, "$", options[["effectSizeStandardError"]], "\n",
    "power    <- pnorm(z - qnorm(0.975)) + pnorm(-z - qnorm(0.975))\n",
    "waapRows <- power >= ", format(options[["powerCriterion"]]),
    if (options[["noSelection"]] != "") {
      paste0(" | ", dataName, "$", options[["noSelection"]], " == ", .smCodeString(options[["noSelectionLevel"]]))
    },
    "\n",
    "waapData <- ", dataName, "[waapRows, ]\n\n"
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

  code <- paste0(dataCode, .wwFitCode(options, dataName, rows = if (subgroup) "subgroupRows"))

  if (jaspBase::isTryError(fit))
    return(paste0(code, "# ", gettextf("The module reported: %1$s", .maTryCleanErrorMessages(fit)), "\n"))

  # the WAAP model refits the WLS model to the retained estimates
  if (decision[["selected"]] == "waap")
    code <- paste0(code, .wwSelectionCode(options, dataName), .wwFitCode(options, "waapData", rows = NULL, waap = TRUE))

  return(code)
}
