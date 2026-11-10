# PET-PEESE R code.
#
# Reproducible calls reuse the classical metafor call with the bias term added
# to the moderators.

.ppShowRCode                            <- function(jaspResults, options) {

  if (!.maReady(options) || !is.null(jaspResults[["metaforRCode"]]))
    return()

  html          <- createJaspHtml(title = gettext("Metafor R Code"))
  html$position <- 99
  html$dependOn(c(.ppDependencies, "showMetaforRCode", "includeFullDatasetInSubgroupAnalysis"))
  jaspResults[["metaforRCode"]] <- html

  fits <- .maExtractFit(jaspResults, options, nonClustered = TRUE)
  code <- vapply(names(fits), function(scope) {
    paste0(
      if (options[["subgroup"]] != "") {
        paste0("# ", gettextf("Subgroup: %1$s", gsub("[\r\n]", " ", attr(fits[[scope]], "subgroup"))), "\n")
      },
      .ppCode(fits[[scope]], options, subgroup = scope != "__fullDataset")
    )
  }, character(1))

  html$text <- .maTransformToHtml(paste(code, collapse = "\n"))

  return()
}

.ppCode                                 <- function(fit, options, subgroup) {

  candidate <- attr(fit, "petPeeseCandidate")

  if (jaspBase::isTryError(fit) && is.null(candidate))
    return(paste0("# ", gettextf("R code unavailable: %1$s", .maTryCleanErrorMessages(fit)), "\n"))

  # subgroup models are fitted to the corresponding rows of the dataset
  dataName <- if (subgroup) "subgroupData" else "dataset"
  dataCode <- if (subgroup) {
    paste0(
      "subgroupRows <- dataset$", options[["subgroup"]], " == ", .smCodeString(attr(fit, "subgroup")), "\n",
      "subgroupData <- droplevels(dataset[subgroupRows, ])\n"
    )
  }

  # the bias term is the standard error (PET) or the sampling variance (PEESE),
  # set to zero for estimates at the unaffected level
  biasTerm <- .ppBiasTerms[[candidate]]
  biasCode <- paste0(
    dataName, "$", biasTerm, " <- ", dataName, "$", options[["effectSizeStandardError"]], if (candidate == "peese") "^2",
    if (options[["noSelection"]] != "") {
      paste0(" * (", dataName, "$", options[["noSelection"]], " != ", .smCodeString(options[["noSelectionLevel"]]), ")")
    },
    "\n"
  )

  fitCode <- .maMakeMetaforCallText(.ppCandidateOptions(options, candidate))
  fitCode <- gsub("data = dataset", paste0("data = ", dataName), fitCode, fixed = TRUE)
  fitCode <- gsub("dataset$", paste0(dataName, "$"), fitCode, fixed = TRUE)

  # a precomputed variance-covariance matrix corresponds to the complete data set
  if (subgroup && .maIsMultilevelMultivariate(options) && options[["varianceCovarianceMatrixType"]] == "precomputed")
    fitCode <- sub(", header = FALSE))", ", header = FALSE))[subgroupRows, subgroupRows]", fitCode, fixed = TRUE)

  code <- paste0(dataCode, biasCode, fitCode)

  if (jaspBase::isTryError(fit))
    return(paste0(code, "# ", gettextf("The module reported: %1$s", .maTryCleanErrorMessages(fit)), "\n"))

  return(code)
}
