# PET-PEESE.
#
# Fits PET and PEESE meta-regressions with metafor, selects the reported model,
# and coordinates the classical meta-analysis output builders.

# Analysis and dependencies ----

PetPeese                                <- function(jaspResults, dataset = NULL, options, ...) {

  options[["analysis"]] <- "petPeese"

  if (.maReady(options)) {
    dataset <- .ppCheckData(dataset, options)
    if (.maIsMultilevelMultivariate(options)) .mammCheckErrors(dataset, options) else .maCheckErrors(dataset, options)
  }

  fits <- .ppFitModels(jaspResults, dataset, options)
  .ppReportingFits(jaspResults, fits, options)

  .maModelSummaryTables(jaspResults, options)
  .maMetaregressionTables(jaspResults, options)
  .maEstimatedMarginalMeansAndContrasts(jaspResults, options)

  .maUltimateForestPlot(jaspResults, options)
  .maBubblePlot(jaspResults, options)
  .ppCoefficientTable(jaspResults, options)
  .ppRegressionPlot(jaspResults, options)

  .ppDiagnostics(jaspResults, options)
  .maExportColumns(jaspResults, dataset, options)

  if (options[["showMetaforRCode"]])
    .ppShowRCode(jaspResults, options)

  if (.maIsMultilevelMultivariate(options) && options[["varianceCovarianceMatrixSaveComputedVarianceCovarianceMatrix"]] != "")
    .mammExportVarianceCovarianceMatrix(dataset, options)

  return()
}

# Model inputs (data, moderators, method, clustering); changing them refits all candidates.
.ppBaseDependencies     <- c(.maDependencies, "noSelection", "noSelectionLevel", "multilevelMultivariate", "useSparseMatrices")

# Model selection; changing it only re-selects the cached candidates.
.ppSelectionDependencies <- c("publicationBiasAdjustment", "petPeeseThreshold")

.ppDependencies         <- c(.ppBaseDependencies, .ppSelectionDependencies)

# Bias terms ----

.ppBiasTerms            <- c(pet = "petBias", peese = "peeseBias")

.ppBiasTermLabel        <- function(term) {

  switch(
    term,
    petBias   = gettext("Standard error (PET)"),
    peeseBias = gettext("Standard error\u00B2 (PEESE)")
  )
}

.ppCandidateLabel       <- function(candidate) {

  switch(candidate, pet = "PET", peese = "PEESE")
}

.ppBiasValues           <- function(dataset, options, candidate) {

  standardErrors <- dataset[[options[["effectSizeStandardError"]]]]
  bias           <- switch(candidate, pet = standardErrors, peese = standardErrors^2)

  # estimates at the unaffected level do not contribute to the small-study effect
  if (options[["noSelection"]] != "")
    bias <- bias * .ppSelected(dataset, options)

  return(bias)
}

.ppSelected             <- function(dataset, options) {

  if (options[["noSelection"]] == "")
    return(rep(TRUE, nrow(dataset)))

  return(as.character(dataset[[options[["noSelection"]]]]) != options[["noSelectionLevel"]])
}

# Shared data ----

.ppCheckData            <- function(dataset, options) {

  # Check the assigned level before dropping observations. The chosen level can
  # legitimately disappear from a complete-case dataset or an individual subgroup.
  if (options[["noSelection"]] != "") {
    indicator       <- dataset[[options[["noSelection"]]]]
    availableLevels <- if (is.factor(indicator)) levels(indicator) else unique(as.character(indicator[!is.na(indicator)]))

    if (!options[["noSelectionLevel"]] %in% availableLevels)
      .quitAnalysis(gettext("The unaffected level is not present in the no-selection variable. Choose an available level."))
  }

  dataset <- if (.maIsMultilevelMultivariate(options)) .mammCheckData(dataset, options) else .maCheckData(dataset, options)

  # The classical data check does not consider the no-selection indicator.
  if (options[["noSelection"]] != "") {
    keep    <- !is.na(dataset[[options[["noSelection"]]]])
    nasIds  <- attr(dataset, "NasIds")
    nasIds[which(!nasIds)[!keep]] <- TRUE

    dataset <- droplevels(dataset[keep, , drop = FALSE])
    attr(dataset, "NAs")    <- sum(nasIds)
    attr(dataset, "NasIds") <- nasIds
  }

  return(dataset)
}

# Candidate fitting ----

.ppError                <- function(message) {

  structure(message, class = "try-error", condition = simpleError(message))
}

.ppCandidateOptions     <- function(options, candidate) {

  # The bias term enters the fitting call only; the GUI model terms stay unchanged.
  options[["effectSizeModelTerms"]] <- c(
    options[["effectSizeModelTerms"]],
    list(list(components = .ppBiasTerms[[candidate]]))
  )

  return(.ppModelOptions(options))
}

.ppModelOptions         <- function(options) {

  # (shared with WAAP-WLS)
  # UWLS is defined by the inverse-variance weights
  if (.maIsUnrestrictedWeightedLeastSquares(options)) {
    options[["fixParametersWeights"]] <- FALSE
    options[["weightedEstimation"]]   <- TRUE
  }

  # fixing tau2 or weights is not offered for the multilevel/multivariate model
  # (the GUI keeps the values of the hidden options)
  if (.maIsMultilevelMultivariate(options)) {
    options[["fixParametersTau2"]]    <- FALSE
    options[["fixParametersWeights"]] <- FALSE
  }

  return(options)
}

.ppUnadjustedOptions    <- function(options, benchmark) {

  # Unadjusted comparison models share the scope, moderators, test, and clustering.
  options[["method"]] <- switch(
    benchmark,
    fixedEffect   = "fixedEffects",
    randomEffects = "restrictedML"
  )
  options[["fixParametersWeights"]] <- FALSE
  options[["fixParametersTau2"]]    <- FALSE
  options[["weightedEstimation"]]   <- TRUE

  # rma.mv without random effects is the fixed effect model with the same sampling (co)variances
  if (.maIsMultilevelMultivariate(options)) {
    options[["method"]] <- "restrictedML"
    if (benchmark == "fixedEffect")
      options[c("randomEffects", "randomEffectsSpecification")] <- list(list(), list())
  }

  return(options)
}

.ppBiasTermError        <- function(fit, dataset, options, biasTerm) {

  if (jaspBase::isTryError(fit) || biasTerm %in% rownames(fit[["beta"]]))
    return(NULL)

  # metafor drops redundant predictors; a model without the bias term is no PET-PEESE model
  if (options[["noSelection"]] != "" && all(dataset[[biasTerm]] == 0))
    return(gettext("No estimates are subject to selection (all estimates are at the unaffected level). The publication bias adjustment is not estimable."))

  return(gettext("The publication bias adjustment is not estimable: the standard errors are constant or redundant with the moderators."))
}

.ppFitCandidate         <- function(dataset, options, subgroupName, candidate) {

  biasTerm            <- .ppBiasTerms[[candidate]]
  dataset[[biasTerm]] <- .ppBiasValues(dataset, options, candidate)

  output <- .maFitModelFun(dataset, .ppCandidateOptions(options, candidate), subgroupName)
  error  <- .ppBiasTermError(output[["fit"]], dataset, options, biasTerm)

  for (type in names(output)) {

    fit <- output[[type]]

    if (is.null(fit))
      next

    if (!is.null(error)) {
      fit <- .ppError(error)
      attr(fit, "dataset") <- dataset
    }

    attr(fit, "subgroup")          <- subgroupName
    attr(fit, "biasTerm")          <- biasTerm
    attr(fit, "petPeeseCandidate") <- candidate
    output[[type]] <- fit
  }

  return(output)
}

.ppFitScope             <- function(dataset, options, subgroupName) {

  scope <- list()

  for (candidate in names(.ppBiasTerms))
    scope[[candidate]] <- .ppFitCandidate(dataset, options, subgroupName, candidate)

  for (benchmark in c("fixedEffect", "randomEffects"))
    scope[[benchmark]] <- .maFitModelFun(dataset, .ppUnadjustedOptions(options, benchmark), subgroupName)

  return(scope)
}

.ppScopeDatasets        <- function(dataset, options) {

  # scope names follow the classical fit layout used by the output builders
  scopes <- list()
  scopes[["__fullDataset"]] <- dataset
  attr(scopes[["__fullDataset"]], "subgroupName") <- gettext("Full dataset")

  if (options[["subgroup"]] != "") {

    subgroupLevels <- unique(dataset[[options[["subgroup"]]]])

    for (subgroupLevel in subgroupLevels) {

      subgroupIndx <- dataset[[options[["subgroup"]]]] == subgroupLevel
      subgroupData <- droplevels(dataset[subgroupIndx, ])

      # forward NAs information and additional attributes
      tempNasIds <- attr(dataset, "NasIds")[!attr(dataset, "NasIds")]
      attr(subgroupData, "NAs")          <- sum(tempNasIds[subgroupIndx])
      attr(subgroupData, "NasIds")       <- tempNasIds[subgroupIndx]
      attr(subgroupData, "subgroupIndx")  <- subgroupIndx
      attr(subgroupData, "datasetNasIds") <- attr(dataset, "NasIds")
      attr(subgroupData, "subgroupName")  <- as.character(subgroupLevel)

      scopes[[paste0("subgroup", subgroupLevel)]] <- subgroupData
    }
  }

  return(scopes)
}

.ppAttachDatasets       <- function(scopes, dataset, options) {

  # Plotting variables (e.g., study labels) do not refit the models; the fits
  # carry the current data, with the bias term of each candidate.
  scopeData <- .ppScopeDatasets(dataset, options)

  for (scope in names(scopes)) {
    for (model in names(scopes[[scope]])) {

      data <- scopeData[[scope]]
      if (model %in% names(.ppBiasTerms))
        data[[.ppBiasTerms[[model]]]] <- .ppBiasValues(data, options, model)

      for (type in names(scopes[[scope]][[model]])) {
        if (!is.null(scopes[[scope]][[model]][[type]]))
          attr(scopes[[scope]][[model]][[type]], "dataset") <- data
      }
    }
  }

  return(scopes)
}

.ppFitCacheKey          <- function(dataset, options, dependencies) {

  # (shared with WAAP-WLS)
  columns <- unique(c(
    options[["effectSize"]],
    options[["effectSizeStandardError"]],
    if (length(options[["predictors"]])) unlist(options[["predictors"]]),
    options[["subgroup"]],
    options[["clustering"]],
    options[["noSelection"]],
    if (options[["fixParametersWeights"]]) options[["fixParametersWeightsVariable"]],
    if (.maIsMultilevelMultivariate(options)) c(unlist(.mammExtractRandomVariableNames(options)), .mammExtractVarianceCovarianceMatrixNames(options))
  ))
  columns <- columns[columns != ""]

  # A data change without an option change must also refit the models.
  return(list(
    data    = dataset[, columns, drop = FALSE],
    options = options[dependencies]
  ))
}

.ppFitModels            <- function(jaspResults, dataset, options) {

  if (!.maReady(options))
    return(NULL)

  if (is.null(jaspResults[["petPeeseFitCache"]])) {
    state <- createJaspState()
    state$dependOn(.ppBaseDependencies)
    jaspResults[["petPeeseFitCache"]] <- state
  }

  cache <- jaspResults[["petPeeseFitCache"]]$object
  key   <- .ppFitCacheKey(dataset, options, .ppBaseDependencies)

  if (!identical(cache[["key"]], key)) {
    scopes <- lapply(.ppScopeDatasets(dataset, options), function(data) .ppFitScope(data, options, attr(data, "subgroupName")))
    cache  <- list(key = key, scopes = scopes)
    jaspResults[["petPeeseFitCache"]]$object <- cache
  }

  return(.ppAttachDatasets(cache[["scopes"]], dataset, options))
}

# Model selection ----

.ppReportedFit          <- function(output, options) {

  if (.maIsClustered(options))
    return(output[["fitClustered"]])

  return(output[["fit"]])
}

.ppDecision             <- function(scope, options) {

  mode <- options[["publicationBiasAdjustment"]]

  if (mode != "petPeese")
    return(list(selected = mode))

  # Conditional PET-PEESE: PEESE is reported if PET's two-sided test of the adjusted effect is significant.
  pet <- .ppReportedFit(scope[["pet"]], options)

  if (jaspBase::isTryError(pet))
    return(list(selected = NA_character_, message = gettextf(
      "PET-PEESE model selection is unavailable because the PET model failed: %1$s",
      .maTryCleanErrorMessages(pet)
    )))

  # a single test of the adjusted effect, also for models with multiple heterogeneity levels
  options[["predictionIntervals"]] <- FALSE
  test <- try(.maComputePooledEffectPlot(pet, options))

  if (jaspBase::isTryError(test) || !.maIsFiniteScalar(test[["pval"]]) ||
      !.maIsFiniteScalar(test[["se"]]) || test[["se"]] <= 0)
    return(list(selected = NA_character_, message = gettext(
      "PET-PEESE model selection is unavailable because the test of the PET adjusted effect could not be computed."
    )))

  return(list(
    selected = if (test[["pval"]] < options[["petPeeseThreshold"]]) "peese" else "pet",
    pval     = test[["pval"]]
  ))
}

.ppSelectScope          <- function(scope, options) {

  decision <- .ppDecision(scope, options)

  if (is.na(decision[["selected"]])) {
    error  <- .ppError(decision[["message"]])
    attr(error, "subgroup") <- attr(scope[["pet"]][["fit"]], "subgroup")
    attr(error, "dataset")  <- attr(scope[["pet"]][["fit"]], "dataset")
    output <- list(fit = error, fitClustered = if (.maIsClustered(options)) error)
  } else {
    output <- scope[[decision[["selected"]]]]
  }

  for (type in names(output)) {
    if (is.null(output[[type]]))
      next

    attr(output[[type]], "petPeeseDecision") <- decision
    attr(output[[type]], "petFit")           <- scope[["pet"]][[type]]
    attr(output[[type]], "unadjustedFits")   <- lapply(scope[c("fixedEffect", "randomEffects")], `[[`, type)
  }

  return(output)
}

.ppReportingFits        <- function(jaspResults, fits, options) {

  # Adapt the selected candidates to the state layout expected by the shared builders.
  if (is.null(jaspResults[["fit"]])) {
    state <- createJaspState()
    state$dependOn(.ppDependencies)
    jaspResults[["fit"]] <- state
  }

  jaspResults[["fit"]]$object <- if (is.null(fits)) NULL else lapply(fits, .ppSelectScope, options = options)

  return()
}

# Diagnostics ----

.ppDiagnostics          <- function(jaspResults, options) {

  if (.maIsMetaregressionEffectSize(options) && options[["diagnosticsVarianceInflationFactor"]])
    .maVarianceInflationTable(jaspResults, options, "effectSize")
  if (options[["diagnosticsCasewiseDiagnostics"]])
    .maCasewiseDiagnosticsTable(jaspResults, options)
  if (options[["diagnosticsPlotsProfileLikelihood"]])
    .maProfileLikelihoodPlot(jaspResults, options)
  if (options[["diagnosticsPlotsBaujat"]] && !.maIsMultilevelMultivariate(options))
    .maBaujatPlot(jaspResults, options)
  if (options[["diagnosticsResidualFunnel"]])
    .maResidualFunnelPlot(jaspResults, options)

  return()
}
