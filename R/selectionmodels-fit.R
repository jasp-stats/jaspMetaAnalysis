# Selection-model fitting.
#
# Builds the metafor calls and caches references and candidate fits independently
# of the output containers.

# Metafor arguments and warning capture ----

.smError                                <- function(message) {

  structure(message, class = "try-error")
}

.smCapture                              <- function(expression) {

  warnings <- character()
  value    <- tryCatch(
    withCallingHandlers(
      expression,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) .smError(conditionMessage(e))
  )

  attr(value, "selectionWarnings") <- unique(warnings)

  return(value)
}

.smBaseArguments                        <- function(dataset, options, sampleSize = "", dataName = NULL) {

  args <- list(
    yi        = as.name(options[["effectSize"]]),
    sei       = as.name(options[["effectSizeStandardError"]]),
    data      = if (is.null(dataName)) dataset else as.name(dataName),
    method    = .maGetMethodOptions(options),
    test      = "z",
    level     = 100 * options[["confidenceIntervalsLevel"]],
    intercept = options[["effectSizeModelIncludeIntercept"]]
  )

  args$mods <- .maGetFormula(
    options[["effectSizeModelTerms"]],
    options[["effectSizeModelIncludeIntercept"]]
  )

  if (sampleSize != "") {
    args$ni <- if (is.null(dataName)) {
      dataset[[sampleSize]]
    } else {
      as.name(sampleSize)
    }
  }

  return(args)
}

.smSelectionArguments                   <- function(spec, dataset, options) {

  if (spec[["type"]] == "none")
    return(list())

  direction <- options[["modelExpectedDirectionOfTheEffect"]]

  if (direction == "detect")
    direction <- if (median(dataset[[options[["effectSize"]]]]) >= 0) "positive" else "negative"

  args <- list(
    type        = spec[["type"]],
    alternative = if (spec[["sidedness"]] == "twoSided") {
      "two.sided"
    } else if (direction == "positive") {
      "greater"
    } else {
      "less"
    }
  )

  if (length(spec[["steps"]]))
    args$steps <- spec[["steps"]]

  if (length(spec[["delta"]]))
    args$delta <- spec[["delta"]]

  if (spec[["prec"]] != "none") {
    args$prec      <- spec[["prec"]]
    args$scaleprec <- if (spec[["scaleprec"]]) TRUE else FALSE
  }

  if (spec[["type"]] == "stepfun")
    args$decreasing <- spec[["decreasing"]]

  if (options[["pValue"]] != "")
    args$pval <- dataset[[options[["pValue"]]]]

  # selmodel's subset marks studies subject to selection. It does not remove the
  # other studies: their likelihood contributions remain unadjusted.
  if (options[["noSelection"]] != "")
    args$subset <- as.character(dataset[[options[["noSelection"]]]]) != options[["noSelectionLevel"]]

  # nlminb uses different control names from the optim-based methods.
  control <- list()

  if (options[["optimizerMethod"]] != "default")
    control$optimizer <- options[["optimizerMethod"]]

  if (options[["optimizerMaximumIterations"]]) {
    control[[if (options[["optimizerMethod"]] == "nlminb") "iter.max" else "maxit"]] <-
      options[["optimizerMaximumIterationsValue"]]
  }

  if (options[["optimizerConvergenceRelativeTolerance"]]) {
    control[[if (options[["optimizerMethod"]] == "nlminb") "rel.tol" else "reltol"]] <-
      options[["optimizerConvergenceRelativeToleranceValue"]]
  }

  if (length(control))
    args$control <- control

  return(args)
}

# Candidate fitting ----

.smFitOne                               <- function(reference, spec, dataset, options) {

  if (inherits(reference, "try-error"))
    return(reference)

  if (inherits(spec, "try-error"))
    return(spec)

  if (spec[["type"]] == "none")
    return(reference)

  if (.smNeedsSampleSize(spec)) {
    if (spec[["sampleSize"]] == "")
      return(.smError(gettext("Assign a sample-size variable for this precision measure.")))

    # Add this candidate's sample sizes through the native metafor call.
    reference <- .smCapture(do.call(
      metafor::rma.uni,
      .smBaseArguments(dataset, options, spec[["sampleSize"]])
    ))

    if (inherits(reference, "try-error"))
      return(reference)
  }

  args <- .smSelectionArguments(spec, dataset, options)

  # metafor cannot fit a selection model without selected studies, even with fixed parameters.
  if (!is.null(args$subset) && !any(args$subset)) {
    return(.smError(gettext("No studies are marked as subject to selection. Use the unadjusted model (None) or choose a different unaffected level.")))
  }

  # Do not silently merge empty intervals. Fixed non-reference weights are allowed.
  if (spec[["type"]] == "stepfun" &&
      (!length(spec[["delta"]]) || anyNA(spec[["delta"]][-1]))) {

    frequencies <- .smCapture(do.call(
      metafor::selmodel,
      c(list(x = reference), args, list(ptable = TRUE))
    ))

    if (inherits(frequencies, "try-error"))
      return(frequencies)

    delta <- if (length(spec[["delta"]])) {
      spec[["delta"]]
    } else {
      c(1, rep(NA_real_, length(spec[["steps"]]) - 1L))
    }
    empty <- frequencies[["k"]] == 0

    if (empty[1] || any(empty & is.na(delta))) {
      return(.smError(gettext("The reference interval or an interval with an estimated weight contains no selected studies. Change the cutoffs or fix the empty interval's weight; intervals are not merged automatically.")))
    }
  }

  fit <- .smCapture(do.call(metafor::selmodel, c(list(x = reference), args)))

  attr(fit, "selectionWarnings") <- unique(c(
    attr(reference, "selectionWarnings"),
    attr(fit, "selectionWarnings")
  ))
  attr(fit, "selectionArguments") <- args

  return(fit)
}

# Reference and candidate caches ----

.smAttachFitMetadata                    <- function(fit, spec, dataset, subgroup) {

  attr(fit, "dataset")                <- dataset
  attr(fit, "subgroup")               <- subgroup
  attr(fit, "selectionSpecification") <- spec

  return(fit)
}

.smFitCandidates                        <- function(cache, dataset, options, specifications, subgroup) {

  if (is.null(cache)) {
    reference <- .smCapture(do.call(
      metafor::rma.uni,
      .smBaseArguments(dataset, options)
    ))
    cache <- list(reference = reference, models = list())
  }

  models <- list()

  for (id in names(specifications)) {
    spec  <- specifications[[id]]
    key   <- list(
      spec       = spec,
      direction  = options[["modelExpectedDirectionOfTheEffect"]],
      sampleSize = if (.smNeedsSampleSize(spec) && spec[["sampleSize"]] != "") dataset[[spec[["sampleSize"]]]]
    )
    model <- cache[["models"]][[id]]

    # Editing one candidate reuses the reference and unaffected candidates.
    if (is.null(model) || !identical(model[["key"]], key))
      model <- list(key = key, fit = .smFitOne(cache[["reference"]], spec, dataset, options))

    model[["fit"]] <- .smAttachFitMetadata(
      fit       = model[["fit"]],
      spec      = spec,
      dataset   = dataset,
      subgroup  = subgroup
    )
    models[[id]] <- model
  }

  cache[["models"]] <- models
  cache
}

.smFitModels                            <- function(jaspResults, dataset, options, specifications) {

  if (!.maReady(options))
    return(NULL)

  if (is.null(jaspResults[["selectionFitCache"]])) {
    state <- createJaspState()
    state$dependOn(.smBaseDependencies)
    jaspResults[["selectionFitCache"]] <- state
  }

  cache           <- jaspResults[["selectionFitCache"]]$object

  columns <- unique(c(
    options[["effectSize"]],
    options[["effectSizeStandardError"]],
    if (length(options[["predictors"]])) unlist(options[["predictors"]]),
    options[["subgroup"]],
    options[["pValue"]],
    options[["noSelection"]]
  ))
  columns <- columns[columns != ""]

  # A shared data/model change invalidates references and all candidate fits.
  key <- list(
    data    = dataset[, columns, drop = FALSE],
    options = options[.smBaseDependencies]
  )

  if (!identical(cache[["key"]], key))
    cache <- list(key = key, scopes = list())

  scopes <- list(full = seq_len(nrow(dataset)))

  if (options[["subgroup"]] != "") {
    groups <- unique(as.character(dataset[[options[["subgroup"]]]]))

    for (i in seq_along(groups)) {
      scopes[[paste0("group", i)]] <- which(
        as.character(dataset[[options[["subgroup"]]]]) == groups[i]
      )
    }
  }

  output <- list()

  for (scope in names(scopes)) {

    rows      <- scopes[[scope]]
    scopeData <- droplevels(dataset[rows, , drop = FALSE])
    label     <- if (scope == "full") {
      gettext("Full Dataset")
    } else {
      as.character(scopeData[[options[["subgroup"]]]][1])
    }

    attr(scopeData, "NAs") <- if (scope == "full") {
      attr(dataset, "NAs")
    } else {
      as.integer(attr(dataset, "selectionOmittedBySubgroup")[[label]])
    }
    attr(scopeData, "NasIds")             <- attr(dataset, "NasIds")
    attr(scopeData, "selectionRows")      <- attr(dataset, "selectionRows")[rows]
    attr(scopeData, "selectionTotalRows") <- attr(dataset, "selectionTotalRows")

    scopeCache <- .smFitCandidates(
      cache           = cache[["scopes"]][[scope]],
      dataset         = scopeData,
      options         = options,
      specifications  = specifications,
      subgroup        = label
    )
    cache[["scopes"]][[scope]] <- scopeCache
    output[[scope]]            <- list(label = label, models = lapply(scopeCache[["models"]], `[[`, "fit"))
  }

  jaspResults[["selectionFitCache"]]$object <- cache

  return(output)
}

# Derived results (profile intervals and likelihoods) share the lifetime of the
# cached candidate fit: refitting a candidate discards them, while editing
# another candidate keeps them.
.smCachedResult                         <- function(jaspResults, id, scope, name, compute) {

  cache <- jaspResults[["selectionFitCache"]]$object
  model <- cache[["scopes"]][[scope]][["models"]][[id]]

  if (!name %in% names(model)) {
    model[[name]]                                <- compute()
    cache[["scopes"]][[scope]][["models"]][[id]] <- model
    jaspResults[["selectionFitCache"]]$object    <- cache
  }

  return(model[[name]])
}

.smHasEstimatedHeterogeneity             <- function(fit) {

  !fit$tau2.fix && !fit$method %in% c("FE", "EE", "CE")
}
