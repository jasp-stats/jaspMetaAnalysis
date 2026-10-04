# Selection-model reporting.
#
# Uses the classical meta-analysis builders for tests, pooled estimates,
# meta-regression and study plots, with selection-specific rows and notes.

# Table creation and cached heterogeneity intervals ----

.smModelContainer                       <- function(container, id, index, options) {

  if (!.smHasMultipleModels(options))
    return(container)

  if (is.null(container[[id]])) {
    model           <- createJaspContainer(gettextf("Model %1$i", index))
    model$position  <- index
    container[[id]] <- model
  }

  container[[id]]
}

.smTable                                <- function(container, key, title, columns, strings = character()) {

  table                          <- createJaspTable(title)
  container[[key]]               <- table
  table$showSpecifiedColumnsOnly <- TRUE

  for (name in names(columns)) {
    table$addColumnInfo(
      name  = name,
      title = columns[[name]],
      type  = if (name %in% strings) {
        "string"
      } else if (name == "pval") {
        "pvalue"
      } else if (name %in% c("k", "parameters", "df")) {
        "integer"
      } else {
        "number"
      }
    )
  }

  table
}

.smPrepareInference                     <- function(jaspResults, fit, id, scope, options) {

  if (!inherits(fit, "rma.uni.selmodel"))
    return(fit)

  # Equal- and fixed-effects models have no tau2 to profile.
  needed <- options[["confidenceIntervals"]] && .smHasEstimatedHeterogeneity(fit) &&
    (options[["heterogeneityTau"]] ||
     options[["heterogeneityTau2"]] ||
     options[["forestPlotHeterogeneityEstimateTau"]] ||
     options[["forestPlotHeterogeneityEstimateTau2"]])

  # Profile intervals can be expensive; table and forest output share them.
  if (needed) {
    attr(fit, "selectionTauCI") <- .smCachedResult(jaspResults, id, scope, "tauCI", function() {
      .smCapture(stats::confint(
        fit,
        tau2  = 1,
        level = 100 * options[["confidenceIntervalsLevel"]]
      ))
    })
  }

  return(fit)
}

# Selection-specific summary rows ----

.smHeterogeneity                        <- function(fit, options) {

  out <- data.frame(
    par = c("\U1D70F", "\U1D70F\u00b2"),
    est = c(sqrt(fit$tau2), fit$tau2),
    se  = c(
      if (fit$tau2 > 0) .maGetSqrtTransformationSeDeltaMethod(fit$tau2, fit$se.tau2) else NA_real_,
      fit$se.tau2
    ),
    lCi = NA_real_,
    uCi = NA_real_
  )
  ci <- attr(fit, "selectionTauCI")

  if (!is.null(ci) && !inherits(ci, "try-error") && !is.null(ci$random)) {
    out$lCi <- c(sqrt(ci$random[1, "ci.lb"]), ci$random[1, "ci.lb"])
    out$uCi <- c(sqrt(ci$random[1, "ci.ub"]), ci$random[1, "ci.ub"])
  }

  out[c(options[["heterogeneityTau"]], options[["heterogeneityTau2"]]), , drop = FALSE]
}

.smRowPublicationBiasTest               <- function(fit) {

  if (!inherits(fit, "rma.uni.selmodel"))
    return(NULL)

  comparison <- .smPublicationBiasComparison(fit)

  data.frame(
    subgroup = attr(fit, "subgroup"),
    test     = gettext("Publication bias"),
    stat     = .smPrintLikelihoodRatio(comparison),
    pval     = comparison$pval
  )
}

# Model and inference notes ----

.smAddSubgroupFootnotes                  <- function(table, messages, fit, options) {

  for (message in messages) {
    if (options[["subgroup"]] != "")
      message <- gettextf("%1$s: %2$s", attr(fit, "subgroup"), message)

    table$addFootnote(message)
  }
}

.smAddInferenceFootnotes                <- function(table, fits, options) {

  selectionFits <- Filter(function(fit) inherits(fit, "rma.uni.selmodel"), fits)

  if (length(selectionFits) && options[["pValue"]] != "")
    table$addFootnote(gettext("Selection p-values were supplied rather than calculated from effect sizes and standard errors."))

  for (fit in fits) {
    .smAddSubgroupFootnotes(table, .smInferenceMessages(fit, options), fit, options)
  }
}

.smInferenceMessages                    <- function(fit, options) {

  warnings <- attr(fit, "selectionWarnings")

  if (!inherits(fit, "rma.uni.selmodel"))
    return(warnings)

  c(
    warnings,
    if (any(!is.finite(fit$se)))
      gettext("Coefficient standard errors could not be estimated. Wald tests and coefficient confidence intervals are unavailable; review the fitting warnings before interpreting this model."),
    if (options[["noSelection"]] != "")
      gettextf(
        "Selection applies to %1$i studies; %2$i studies at level '%3$s' are unaffected.",
        fit$k1, fit$k0, options[["noSelectionLevel"]]
      ),
    .smClippedPValueMessage(fit, options),
    if (!.smNoBiasNested(fit) || fit$LRTdf == 0)
      gettext("The publication-bias test is not applicable with fixed selection parameters.")
  )
}

.smClippedPValueMessage                  <- function(fit, options) {

  if (fit$pval.min <= 0)
    return(NULL)

  # Report clipping only for studies to which selection actually applies.
  rawPValues <- if (options[["pValue"]] != "") {
    attr(fit, "dataset")[[options[["pValue"]]]]
  } else {
    z <- fit$yi / sqrt(fit$vi)
    switch(
      fit$alternative,
      greater   = stats::pnorm(z, lower.tail = FALSE),
      less      = stats::pnorm(z),
      two.sided = 2 * stats::pnorm(abs(z), lower.tail = FALSE)
    )
  }
  selected <- attr(fit, "selectionArguments")[["subset"]]
  clipped  <- rawPValues < fit$pval.min | rawPValues > 1 - fit$pval.min

  if (!is.null(selected))
    clipped <- clipped & selected

  if (!any(clipped))
    return(NULL)

  gettextf(
    "For numerical stability, metafor clips selection p-values to [%1$s, %2$s]. Affected selected studies: %3$i.",
    fit$pval.min, 1 - fit$pval.min, sum(clipped)
  )
}

.smAddHeterogeneityFootnotes            <- function(table, fits, options) {

  for (fit in fits) {

    ci       <- attr(fit, "selectionTauCI")
    messages <- c(
      if (inherits(ci, "try-error")) {
        gettextf("Heterogeneity confidence interval unavailable: %1$s", as.character(ci))
      },
      attr(ci, "selectionWarnings")
    )

    .smAddSubgroupFootnotes(table, unique(messages), fit, options)
  }
}

# Weight-function output ----

.smExtractWeightFunctionContainer        <- function(jaspResults, options) {

  if (!is.null(jaspResults[["weightFunctionSummary"]]))
    return(jaspResults[["weightFunctionSummary"]])

  container          <- createJaspContainer(gettext("Weight Function Summary"))
  container$position <- 4
  container$dependOn(c(.smDependencies, "includeFullDatasetInSubgroupAnalysis"))
  jaspResults[["weightFunctionSummary"]] <- container

  container
}

.smSelectionTable                       <- function(jaspResults, options, specification) {

  if (!options[["weightFunctionEstimates"]])
    return()

  container <- .smExtractWeightFunctionContainer(jaspResults, options)

  if (!is.null(container[["selectionParameters"]]))
    return()

  if (!inherits(specification, "try-error") && specification[["type"]] == "none") {
    message <- createJaspHtml(
      title = gettext("Weight Function Estimates"),
      text  = gettext("The unadjusted model has no selection parameters.")
    )
    message$dependOn("weightFunctionEstimates")
    message$position                   <- 1
    container[["selectionParameters"]] <- message
    return()
  }

  stepFunction <- !inherits(specification, "try-error") && specification[["type"]] == "stepfun"
  table        <- createJaspTable(
    if (stepFunction) {
      gettext("Weight Function Estimates")
    } else {
      gettext("Selection Parameter Estimates")
    }
  )
  table$dependOn(c(
    "weightFunctionEstimates", "confidenceIntervals", "standardErrors",
    "includeFullDatasetInSubgroupAnalysis"
  ))
  table$showSpecifiedColumnsOnly     <- TRUE
  table$position                     <- 1
  container[["selectionParameters"]] <- table

  if (stepFunction) {
    table$addColumnInfo(name = "par", title = gettext("<em>p</em>-Values Interval"), type = "string")
  } else {
    table$addColumnInfo(name = "par", title = "", type = "string")
  }

  .maAddSubgroupColumn(table, options)
  table$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
  .maAddSeColumn(table, options, noTransformation = TRUE)
  .maAddCiColumn(table, options)

  fits <- .maExtractFit(jaspResults, options)

  if (is.null(fits))
    return()

  if (length(fits) == 1 && jaspBase::isTryError(fits[[1]])) {
    table$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  rows <- lapply(fits, .smRowWeightFunctionEstimates, stepFunction = stepFunction)
  table$setData(.maSafeOrderAndSimplify(.maSafeRbind(rows), "par", options))

  for (fit in fits) {
    if (!inherits(fit, "rma.uni.selmodel"))
      next

    message <- if (fit$alternative == "two.sided") {
      gettext("Selection uses two-sided p-values.")
    } else {
      gettextf(
        "Expected effect direction: %1$s.",
        switch(fit$alternative, greater = gettext("positive"), less = gettext("negative"))
      )
    }
    .smAddSubgroupFootnotes(table, message, fit, options)
  }

  if (any(vapply(
    fits,
    function(fit) inherits(fit, "rma.uni.selmodel") && any(fit$delta.fix),
    logical(1)
  )))
    table$addFootnote(gettext("Fixed selection parameters have no standard errors or confidence intervals."))
}

.smRowWeightFunctionEstimates            <- function(fit, stepFunction) {

  if (!inherits(fit, "rma.uni.selmodel"))
    return(NULL)

  labels <- if (stepFunction) {
    paste0("[", head(c(0, fit$steps), -1), ", ", fit$steps, ")")
  } else {
    paste0("\u03b4", seq_along(fit$delta))
  }

  # Fixed weights retain their estimates but have no sampling uncertainty.
  data.frame(
    par      = labels,
    subgroup = attr(fit, "subgroup"),
    est      = fit$delta,
    se       = ifelse(fit$delta.fix, NA_real_, fit$se.delta),
    lCi      = ifelse(fit$delta.fix, NA_real_, fit$ci.lb.delta),
    uCi      = ifelse(fit$delta.fix, NA_real_, fit$ci.ub.delta)
  )
}

.smFrequencyTable                       <- function(jaspResults, options) {

  if (!options[["weightFunctionPValueFrequencyTable"]])
    return()

  fits          <- .maExtractFit(jaspResults, options)
  frequencyFits <- Filter(
    function(fit) inherits(fit, "rma.uni.selmodel") && !all(is.na(fit$ptable)),
    fits
  )
  container <- .smExtractWeightFunctionContainer(jaspResults, options)

  if (!is.null(container[["selectionFrequencies"]]))
    return()

  if (!length(frequencyFits)) {
    message <- createJaspHtml(
      title = gettext("P-Value Frequencies"),
      text  = gettext("P-value frequencies require a fitted selection model with p-value cutoffs.")
    )
    message$dependOn(c("weightFunctionPValueFrequencyTable", "includeFullDatasetInSubgroupAnalysis"))
    message$position                    <- 3
    container[["selectionFrequencies"]] <- message
    return()
  }

  table <- createJaspTable(gettext("P-Value Frequencies"))
  table$dependOn(c("weightFunctionPValueFrequencyTable", "includeFullDatasetInSubgroupAnalysis"))
  table$showSpecifiedColumnsOnly      <- TRUE
  table$position                      <- 3
  container[["selectionFrequencies"]] <- table

  table$addColumnInfo(name = "interval", title = gettext("Interval"), type = "string")
  .maAddSubgroupColumn(table, options)
  table$addColumnInfo(
    name  = "count",
    title = if (options[["noSelection"]] != "") gettext("Selected Studies") else gettext("Studies"),
    type  = "integer"
  )

  if (options[["noSelection"]] != "")
    table$addFootnote(gettext("P-value frequencies include only studies to which selection applies."))

  rows <- lapply(frequencyFits, function(fit) {
    counts <- as.data.frame(fit$ptable)
    data.frame(
      interval = rownames(counts),
      subgroup = attr(fit, "subgroup"),
      count    = counts[["k"]]
    )
  })
  table$setData(.maSafeOrderAndSimplify(.maSafeRbind(rows), "interval", options))
}

# Standard output for each displayed candidate ----

.smModelOutputs                         <- function(jaspResults, fits, options, specifications) {

  multiple <- .smHasMultipleModels(options)

  # Replacing the detailed view must not invalidate independently cached fits.
  if (multiple) {

    if (is.null(jaspResults[["selectionModelResults"]])) {
      container <- createJaspContainer(gettext("Model Results"))
      container$dependOn(c(
        .smDependencies, "showSelectionModels", "includeFullDatasetInSubgroupAnalysis"
      ))
      container$position                     <- 3
      jaspResults[["selectionModelResults"]] <- container
    }

    container <- jaspResults[["selectionModelResults"]]

  } else {
    container <- jaspResults
  }

  # Hidden full-dataset fits need no inference; the shared builders drop them.
  scopes   <- .smVisibleScopes(fits, options)
  modelIds <- .smDisplayedModelIds(scopes)

  if (!is.null(fits) && !length(modelIds)) {
    if (is.null(container[["unavailable"]])) {
      table <- .smTable(container, "unavailable", gettext("Model Results"), list(model = gettext("Model")), "model")
      table$dependOn(c(
        .smDependencies, "showSelectionModels", "includeFullDatasetInSubgroupAnalysis"
      ))
      table$setError(gettext("No fitted model is available for the requested selection."))
    }
    return()
  }

  for (i in seq_along(specifications)) {

    id <- names(specifications)[i]

    if (multiple && !is.null(fits) && !id %in% modelIds)
      next

    target <- .smModelContainer(container, id, i, options)

    # Adapt selection fits to the state layout expected by the shared builders.
    if (is.null(target[["fit"]])) {
      state <- createJaspState()
      state$dependOn(.smDependencies)
      target[["fit"]] <- state
    }

    modelFits <- .smModelFits(scopes, id)
    reportingFits <- lapply(names(modelFits), function(scope) {
      fit <- .smPrepareInference(jaspResults, modelFits[[scope]], id, scope, options)
      list(fit = fit, fitClustered = NULL)
    })
    names(reportingFits) <- ifelse(names(modelFits) == "full", "__fullDataset", names(modelFits))

    target[["fit"]]$object <- if (is.null(fits)) NULL else reportingFits

    .maModelSummaryTables(target, options)
    .maMetaregressionTables(target, options)
    .smSelectionTable(target, options, specifications[[i]])
    .smModelPlots(target, options)
    .smFrequencyTable(target, options)

    .maEstimatedMarginalMeansAndContrasts(target, options)
    .maUltimateForestPlot(target, options)
    .maBubblePlot(target, options)
    .smShowRCode(target, options)
  }
}
