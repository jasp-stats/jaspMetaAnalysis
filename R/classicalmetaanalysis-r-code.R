# Classical meta-analysis R-code output.
#
# Constructs and formats reproducible metafor calls shown in JASP output.

.maShowMetaforRCode                      <- function(jaspResults, options, makeCallText = .maMakeMetaforCallText) {

  if (!.maReady(options) || !is.null(jaspResults[["metaforRCode"]]))
    return()

  metaforRCode <- createJaspHtml(title = gettext("Metafor R Code"))
  metaforRCode$dependOn(c(.maDependencies, "showMetaforRCode"))
  metaforRCode$position <- 99

  metaforRCode$text <- .maTransformToHtml(makeCallText(options))

  jaspResults[['metaforRCode']] <- metaforRCode

  return()
}

.maMakeMetaforCallText             <- function(options) {

  if (options[["analysis"]] == "metaAnalysis") {
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      sei  = as.name(options[["effectSizeStandardError"]]),
      data = as.name("dataset")
    )
  } else if (options[["analysis"]] == "metaAnalysisMultilevelMultivariate") {

    if (.mammVarianceCovarianceMatrixReady(options)) {
      vcalcInput <-.mammGetVarianceCovarianceMatrix(NULL, options, returnCall = TRUE)
      vcalcInput$data <- as.name("dataset")
    }

    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      V    = if (.mammVarianceCovarianceMatrixReady(options)) "effectSizeVarianceCovarianceMatrix" else paste0(options[["effectSizeStandardError"]], "^2"),
      data = as.name("dataset")
    )
  }

  # add formulas if specified
  rmaInput$mods  <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  rmaInput$scale <- if (!.maIsUnrestrictedWeightedLeastSquares(options)) .maGetFormula(options[["heterogeneityModelTerms"]], options[["heterogeneityModelIncludeIntercept"]])

  # add random effects
  if (.maIsMultilevelMultivariate(options)) {
    randomFormulaList <- .mammGetRandomFormulaList(options)
    if (length(randomFormulaList) != 0) {
      struct      <- do.call(c, lapply(randomFormulaList, attr, "structure"))
      dist        <- unlist(unname(lapply(randomFormulaList, attr, which = "dist")), recursive = FALSE)
      R           <- unlist(unname(lapply(randomFormulaList, attr, which = "R")), recursive = FALSE)
      # change distance matrix into a variable
      for (i in seq_along(dist)) {
        if (is.matrix(dist[[i]]))
          dist[[i]] <- paste0(names(dist)[i], gettext(" Distance Matrix"))
      }
      # change correlation matrix into a variable
      for (i in seq_along(R)) {
        R[[i]] <- paste0(names(R)[i], gettext(" Correlation Matrix"))
      }

      if (length(randomFormulaList) > 1)
        randomFormulaList <- paste0("list(\n\t\t", paste0("'", names(randomFormulaList), "' = ", randomFormulaList, collapse = "\n\t\t"),")")
      rmaInput$random <- randomFormulaList
      if (length(struct) != 0)
        struct <- paste0("c(", paste0("'", names(struct), "' = '", struct, "'", collapse = ", "),")")
      rmaInput$struct <- struct
      if (length(dist) > 0)
        dist <- paste0("list(", paste0(names(dist), ifelse(names(dist) == "", "'", " = '"), dist, "'", collapse = ", "),")")
      rmaInput$dist <- dist
      if (length(R) > 0)
        R <- paste0("list(", paste0(names(R), " = '", R, "'", collapse = ", "),")")
      rmaInput$R <- R
    }
  }

  # specify method and fixed effect terms test
  rmaInput$method <- paste0("'", .maGetMethodOptions(options), "'")
  rmaInput$test   <- paste0("'", .maGetFixedEffectTestOptions(options), "'")

  if (!options[["weightedEstimation"]])
    rmaInput$weighted <- FALSE

  # add fixed parameters if needed
  if (options[["fixParametersWeights"]] && options[["fixParametersWeightsVariable"]] != "")
    rmaInput$weights <- as.name(options[["fixParametersWeightsVariable"]])
  if (options[["fixParametersTau2"]])
    rmaInput$tau2 <- .maGetFixedTau2Options(options)

  # add link function if needed
  if (.maIsMetaregressionHeterogeneity(options))
    rmaInput$link <- paste0("'", options[["heterogeneityModelLink"]], "'")

  if (.maIsMultilevelMultivariate(options)) {
    rmaInput$sparse <- if (options[["useSparseMatricies"]])       options[["useSparseMatricies"]]
    rmaInput$cvvc   <- if (!options[["computeCovarianceMatrix"]]) !options[["computeCovarianceMatrix"]]
  }

  # add control options if needed
  control <- .maGetControlOptions(options)
  if (length(control) != 0)
    rmaInput$control <- control

  # additional input
  rmaInput$level <- 100 * options[["confidenceIntervalsLevel"]]

  # add additional options
  if (options[["advancedExtendMetaforCall"]])
    rmaInput <- c(rmaInput, .maExtendMetaforCallFromOptions(options))

  ### fit the model
  if (.maIsMultilevelMultivariate(options)) {
    fit <- paste0("fit <- rma.mv(\n\t", paste(names(rmaInput), "=", rmaInput, collapse = ",\n\t"), "\n)\n")
  } else {
    fit <- paste0("fit <- rma(\n\t", paste(names(rmaInput), "=", rmaInput, collapse = ",\n\t"), "\n)\n")
  }

  if (.maIsMultilevelMultivariate(options) &&  .mammVarianceCovarianceMatrixReady(options)) {
    if (options[["varianceCovarianceMatrixType"]] == "precomputed") {
      fit <- paste0(
        paste0("effectSizeVarianceCovarianceMatrix <- ", vcalcInput[["file"]], "\n"), "\n",
        fit
      )
    } else {
      fit <- paste0(
        paste0("effectSizeVarianceCovarianceMatrix <- vcalc(\n\t", paste(names(vcalcInput), "=", vcalcInput, collapse = ",\n\t"), "\n)\n"), "\n",
        fit
      )
    }
  }

  # add clustering if specified
  if (options[["clustering"]] != "") {

    robustInput <- list(
      cluster      = as.name(options[["clustering"]]),
      clubSandwich = options[["clusteringUseClubSandwich"]],
      adjust       = options[["clusteringSmallSampleCorrection"]]
    )

    fit <- paste0(
      fit, "\n",
      "fit <- robust(\n",
      "\tfit,\n\t",
      paste(names(robustInput), "=", robustInput, collapse = ",\n\t"), "\n)\n"
    )
  }

  # add permutation if specified
  if (.maIsPermutation(options)) {

    if (options[["setSeed"]])
      fit <- paste0(fit, "\nset.seed(", options[["seed"]], ")\n")

    fit <- paste0(
      fit, "\n",
      "fitPermutation <- permutest(\n",
      "\tfit,\n",
      "\texact = ", options[["permutationTestType"]] == "exact", ",\n",
      "\titer  = ", options[["permutationTestIteration"]], "\n",
      ")\n"
    )
  }

  return(fit)
}

.maTransformToHtml                    <- function(rCode) {

  # Replace special characters with HTML entities
  htmlCode <- gsub("&", "&amp;", rCode)
  htmlCode <- gsub("<", "&lt;", htmlCode)
  htmlCode <- gsub(">", "&gt;", htmlCode)

  # Wrap the code in <pre> and <code> tags
  htmlCode <- paste0(
    "<pre><code>", htmlCode, "\n</code></pre>"
  )

  return(htmlCode)
}
