# WAAP-WLS.
#
# Fits the WLS meta-analysis of all estimates and the WAAP meta-analysis of the
# adequately powered estimates with metafor, selects the reported model, and
# coordinates the classical meta-analysis output builders.

# Analysis and dependencies ----

WaapWls                                 <- function(jaspResults, dataset = NULL, options, ...) {

  options[["analysis"]] <- "waapWls"

  if (.maReady(options)) {
    dataset <- .ppCheckData(dataset, options)
    if (.maIsMultilevelMultivariate(options)) .mammCheckErrors(dataset, options) else .maCheckErrors(dataset, options)
  }

  fits <- .wwFitModels(jaspResults, dataset, options)
  .wwReportingFits(jaspResults, fits, options)

  .maModelSummaryTables(jaspResults, options)
  .maMetaregressionTables(jaspResults, options)
  .maEstimatedMarginalMeansAndContrasts(jaspResults, options)

  .maUltimateForestPlot(jaspResults, options)
  .maBubblePlot(jaspResults, options)

  .ppDiagnostics(jaspResults, options)
  .maExportColumns(jaspResults, dataset, options)
  .wwExportColumns(jaspResults, dataset, options)

  if (options[["showMetaforRCode"]])
    .wwShowRCode(jaspResults, options)

  if (.maIsMultilevelMultivariate(options) && options[["varianceCovarianceMatrixSaveComputedVarianceCovarianceMatrix"]] != "")
    .mammExportVarianceCovarianceMatrix(dataset, options)

  return()
}

# Model inputs (data, moderators, method, clustering); changing them refits all models.
.wwBaseDependencies      <- c(.maDependencies, "noSelection", "noSelectionLevel", "multilevelMultivariate", "useSparseMatrices")

# Power-based selection; changing it refits the WAAP model only if the retained estimates change.
.wwSelectionDependencies <- c("powerCriterion", "powerConditionalOnModerators")

.wwDependencies          <- c(.wwBaseDependencies, .wwSelectionDependencies)

# Power-based selection ----

.wwPower                 <- function(z) {

  # power of a two-sided z-test (alpha = .05) for an effect of z standard errors
  zAlpha <- stats::qnorm(0.975)

  return(stats::pnorm(abs(z) - zAlpha) + stats::pnorm(-abs(z) - zAlpha))
}

.wwCriticalZ             <- function(power) {

  # |effect| / SE at which the power criterion is attained
  # (every estimate has at least 5% power and none has 100% power)
  if (power <= 0.05)
    return(0)
  if (power >= 1)
    return(Inf)

  return(stats::uniroot(
    function(z) .wwPower(z) - power,
    interval = c(0, stats::qnorm(0.975) + stats::qnorm(power) + 1),
    tol      = 1e-10
  )$root)
}

.wwPowerEffects          <- function(fit, options) {

  # effect sizes (model scale) at which the power is evaluated: the predicted effects of the
  # estimates (a single value without moderators) or the pooled effect at the mean of the moderators
  if (!.maIsMetaregressionEffectSize(options) || options[["powerConditionalOnModerators"]])
    return(stats::predict(fit)[["pred"]])

  # a single pooled effect, also for models with multiple heterogeneity levels
  options[["predictionIntervals"]] <- FALSE

  return(.maComputePooledEffect(fit, options, returnRaw = TRUE)[["pred"]][1])
}

.wwPowerSelection        <- function(fit, dataset, options) {

  if (jaspBase::isTryError(fit))
    return(NULL)

  effects <- try(.wwPowerEffects(fit, options))
  if (jaspBase::isTryError(effects))
    return(effects)

  # estimates at the unaffected level are not subject to selection and are always retained
  z        <- abs(effects) / dataset[[options[["effectSizeStandardError"]]]]
  selected <- .ppSelected(dataset, options)
  powered  <- selected & z >= .wwCriticalZ(options[["powerCriterion"]])

  return(list(
    power    = .wwPower(z),
    selected = selected,
    powered  = powered,
    retained = powered | !selected
  ))
}

.wwRequiredEstimates     <- function(fit) {

  # the WAAP model needs residual degrees of freedom (at least two estimates without moderators)
  return(nrow(fit[["beta"]]) + 1)
}

# Model fitting ----

.wwRetainedData          <- function(dataset, retained) {

  # The retained rows are tracked against the complete-case dataset (as for subgroups):
  # exports map them back and a precomputed variance-covariance matrix is subset to them.
  nasIds    <- attr(dataset, "NasIds")
  scopeRows <- attr(dataset, "subgroupIndx")
  if (is.null(scopeRows))
    scopeRows <- rep(TRUE, nrow(dataset))
  scopeRows[scopeRows] <- retained

  # factor levels are kept: metafor drops moderators that are not estimable from the retained estimates
  data <- dataset[retained, , drop = FALSE]
  attr(data, "NAs")           <- attr(dataset, "NAs")
  attr(data, "NasIds")        <- nasIds[!nasIds][retained]
  attr(data, "subgroupIndx")  <- scopeRows
  attr(data, "datasetNasIds") <- if (is.null(attr(dataset, "datasetNasIds"))) nasIds else attr(dataset, "datasetNasIds")
  attr(data, "subgroupName")  <- attr(dataset, "subgroupName")

  return(data)
}

.wwFitScope              <- function(dataset, options, subgroupName) {

  # the WLS model includes all estimates
  scope <- list(wls = .maFitModelFun(dataset, .ppModelOptions(options), subgroupName))

  for (benchmark in c("fixedEffect", "randomEffects"))
    scope[[benchmark]] <- .maFitModelFun(dataset, .ppUnadjustedOptions(options, benchmark), subgroupName)

  return(scope)
}

.wwFitWaap               <- function(dataset, selection, wlsFit, options) {

  # too few retained estimates: the WLS model is reported
  if (!is.list(selection) || sum(selection[["retained"]]) < .wwRequiredEstimates(wlsFit))
    return(NULL)

  data <- .wwRetainedData(dataset, selection[["retained"]])

  return(.maFitModelFun(data, .ppModelOptions(options), attr(dataset, "subgroupName")))
}

.wwAttachDatasets        <- function(scopes, dataset, options) {

  # Plotting variables (e.g., study labels) do not refit the models; the fits
  # carry the current data, the WAAP model only its retained estimates.
  scopeData <- .ppScopeDatasets(dataset, options)

  for (scope in names(scopes)) {
    for (model in c("wls", "waap", "fixedEffect", "randomEffects")) {

      if (is.null(scopes[[scope]][[model]]))
        next

      data <- scopeData[[scope]]
      if (model == "waap")
        data <- .wwRetainedData(data, scopes[[scope]][["selection"]][["retained"]])

      for (type in names(scopes[[scope]][[model]])) {
        if (!is.null(scopes[[scope]][[model]][[type]]))
          attr(scopes[[scope]][[model]][[type]], "dataset") <- data
      }
    }
  }

  return(scopes)
}

.wwFitModels             <- function(jaspResults, dataset, options) {

  if (!.maReady(options))
    return(NULL)

  if (is.null(jaspResults[["waapWlsFitCache"]])) {
    state <- createJaspState()
    state$dependOn(.wwBaseDependencies)
    jaspResults[["waapWlsFitCache"]] <- state
  }

  cache     <- jaspResults[["waapWlsFitCache"]]$object
  key       <- .ppFitCacheKey(dataset, options, .wwBaseDependencies)
  scopeData <- .ppScopeDatasets(dataset, options)

  if (!identical(cache[["key"]], key)) {
    scopes <- lapply(scopeData, function(data) .wwFitScope(data, options, attr(data, "subgroupName")))
    cache  <- list(key = key, scopes = scopes, waap = list())
  }

  # the power-based selection is cheap; the WAAP model is refitted only if the retained estimates change
  scopes <- cache[["scopes"]]
  for (scope in names(scopes)) {

    wlsFit    <- scopes[[scope]][["wls"]][["fit"]]
    selection <- .wwPowerSelection(wlsFit, scopeData[[scope]], options)

    if (is.null(cache[["waap"]][[scope]]) || !identical(cache[["waap"]][[scope]][["selection"]], selection))
      cache[["waap"]][[scope]] <- list(selection = selection, output = .wwFitWaap(scopeData[[scope]], selection, wlsFit, options))

    scopes[[scope]][["selection"]] <- selection
    scopes[[scope]]["waap"]        <- list(cache[["waap"]][[scope]][["output"]])
  }

  jaspResults[["waapWlsFitCache"]]$object <- cache

  return(.wwAttachDatasets(scopes, dataset, options))
}

# Model selection ----

.wwDecision              <- function(scope, options) {

  wls       <- .ppReportedFit(scope[["wls"]], options)
  waap      <- if (!is.null(scope[["waap"]])) .ppReportedFit(scope[["waap"]], options)
  selection <- scope[["selection"]]

  # a failed WLS model is reported with its error
  if (jaspBase::isTryError(wls))
    return(list(selected = "wls"))

  if (jaspBase::isTryError(selection))
    return(list(selected = "wls", message = gettextf(
      "The power of the estimates could not be computed: %1$s", .maTryCleanErrorMessages(selection)
    )))

  decision <- list(selected = "wls", selection = selection, required = .wwRequiredEstimates(wls))

  if (is.null(waap))
    return(decision)

  if (jaspBase::isTryError(waap))
    return(c(decision, message = gettextf("The WAAP model failed: %1$s", .maTryCleanErrorMessages(waap))))

  # metafor drops moderators that are not estimable from the retained estimates
  if (!identical(rownames(waap[["beta"]]), rownames(wls[["beta"]])))
    return(c(decision, message = gettext("The moderators of the WAAP model are not estimable from the retained estimates.")))

  decision[["selected"]] <- "waap"

  return(decision)
}

.wwSelectScope           <- function(scope, options) {

  decision <- .wwDecision(scope, options)
  output   <- scope[[decision[["selected"]]]]

  for (type in names(output)) {
    if (is.null(output[[type]]))
      next

    attr(output[[type]], "waapWlsDecision") <- decision
    attr(output[[type]], "wlsFit")          <- scope[["wls"]][[type]]
    attr(output[[type]], "unadjustedFits")  <- lapply(scope[c("fixedEffect", "randomEffects")], `[[`, type)
  }

  return(output)
}

.wwReportingFits         <- function(jaspResults, fits, options) {

  # Adapt the selected models to the state layout expected by the shared builders.
  if (is.null(jaspResults[["fit"]])) {
    state <- createJaspState()
    state$dependOn(.wwDependencies)
    jaspResults[["fit"]] <- state
  }

  jaspResults[["fit"]]$object <- if (is.null(fits)) NULL else lapply(fits, .wwSelectScope, options = options)

  return()
}
