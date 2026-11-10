# Classical network meta-analysis fit.
#
# Defines the pairwise() and netmeta() calls once (call specification) and uses it both to fit the
# network meta-analysis (all data, one fit per subgroup) and to print the reproducible R code.

# Call specification ----
#
# A call specification is a list(fun, arguments). The arguments are literals, references to columns
# of the data (.nmaColumn), or references to previous results (.nmaObject, e.g., the pairwise object).

.nmaColumn              <- function(name) structure(list(name = name), class = "nmaColumn")

.nmaObject              <- function(name) structure(list(name = name), class = "nmaObject")

# R code evaluated in the data (the name of the data set) and the previous results (e.g., the fit); the value is used as an argument
.nmaExpression          <- function(code) structure(list(code = code), class = "nmaExpression")

# A name passed to the function without being evaluated (e.g., netmetareg() evaluates the covariate in the data of the fit).
# The columns of the data of the fit are defined by the 'fitData' element of a call: a list of R code (see .nmaEvaluateCall()); its 'fitRows'
# element is R code that gives the rows of the data of the fit that are kept.
.nmaSymbol              <- function(name) structure(list(name = name), class = "nmaSymbol")

.nmaRetainedTreatments  <- function(dataset, options, pairwise) {

  # contrast-based data: all treatments of the (complete) rows are part of the network
  if (!.nmaIsArmBased(options) || is.null(pairwise) || jaspBase::isTryError(pairwise))
    return(unique(unlist(lapply(dataset[.nmaTreatmentVariables(options)], as.character))))

  # arm-based data: comparisons without an effect size (e.g., a study without events) are excluded from the network
  if (.nmaPoolingMethod(options) == "inverseVariance") {
    retained <- is.finite(pairwise[["TE"]]) & is.finite(pairwise[["seTE"]]) & pairwise[["seTE"]] > 0
  } else if (.nmaPoolingMethod(options) == "penalizedLogisticRegression") {
    # penalized logistic regression keeps all studies (also those without events)
    retained <- rep(TRUE, nrow(pairwise))
  } else {
    # the Mantel-Haenszel and the non-central hypergeometric methods exclude studies without any events
    studyEvents <- tapply(pairwise[["event1"]] + pairwise[["event2"]], as.character(pairwise[["studlab"]]), sum)
    retained    <- studyEvents[as.character(pairwise[["studlab"]])] > 0
  }

  return(unique(as.character(unlist(pairwise[retained, c("treat1", "treat2")]))))
}

.nmaReferenceGroup      <- function(dataset, options, pairwise) {

  # a reference treatment missing in the network (e.g., in a subgroup or only in excluded comparisons) is replaced by the default of netmeta, the first
  # treatment of the network (.nmaFitScope() checks the fitted network: the pooling method can exclude the treatment after this resolution)
  if (options[["referenceTreatment"]] %in% .nmaRetainedTreatments(dataset, options, pairwise))
    return(options[["referenceTreatment"]])

  return(NULL)
}

# the summary measure labels the netmeta objects; with backtransf = FALSE it does not change the estimates of contrast-based
# and general arm-level data ("general" = no measure), for the other arm-level data it defines the computed effect sizes
.nmaSummaryMeasureArgument <- function(options) {
  if (options[["effectSizeMeasure"]] == "general")
    return(list())
  return(list(sm = options[["effectSizeMeasure"]]))
}

.nmaPairwiseSpecification <- function(options) {

  outcomeArguments <- switch(
    options[["outcomeType"]],
    binary        = list(event = .nmaColumn(options[["armEvents"]]), n = .nmaColumn(options[["armSampleSize"]])),
    continuous    = list(mean = .nmaColumn(options[["armMean"]]), sd = .nmaColumn(options[["armStandardDeviation"]]), n = .nmaColumn(options[["armSampleSize"]])),
    incidenceRate = list(event = .nmaColumn(options[["armEvents"]]), time = .nmaColumn(options[["armPersonTime"]])),
    general       = list(TE = .nmaColumn(options[["armEffectSize"]]), seTE = .nmaColumn(options[["armEffectSizeStandardError"]]))
  )

  arguments <- c(list(treat = .nmaColumn(options[["treatment"]])), outcomeArguments, list(studlab = .nmaColumn(options[["studyId"]])))

  arguments <- c(arguments, .nmaSummaryMeasureArgument(options))

  # zero frequencies
  if (options[["outcomeType"]] %in% c("binary", "incidenceRate")) {
    arguments[["incr"]]        <- if (options[["advancedTo"]] == "none") 0 else options[["advancedAdd"]]
    arguments[["method.incr"]] <- switch(options[["advancedTo"]], all = "all", onlyZero = "only0", ifAnyZero = "if0all", none = "only0")
    arguments[["allstudies"]]  <- options[["advancedDropStudiesWithNoCasesOrEvents"]] == "no"
  }

  return(list(fun = "meta::pairwise", arguments = arguments))
}

# 'reference': NA resolves the reference treatment against the retained treatments of the contrasts, and a treatment (of any name) is used as it is
.nmaModelArguments      <- function(dataset, options, netmetabin, pairwise, reference = NA) {

  random    <- .nmaIsRandomEffects(options)
  level     <- options[["confidenceIntervalsLevel"]]
  arguments <- list(common = !random, random = random)

  if (netmetabin) {
    # netmetabin provides no prediction intervals and no between-study variance estimators
    arguments[["prediction"]] <- FALSE
  } else if (random) {
    arguments[["method.tau"]]       <- .nmaMethodTau(options)
    arguments[["method.random.ci"]] <- if (options[["fixedEffectTest"]] == "t") "t-dist" else "classic"
    arguments[["prediction"]]       <- TRUE
    arguments[["level.predict"]]    <- level

    if (options[["fixParametersTau2"]])
      arguments[["tau.preset"]]     <- sqrt(.nmaFixedTau2(options))
  }

  arguments[["level.ma"]]        <- level
  arguments[["tol.multiarm"]]    <- options[["advancedMultiArmTolerance"]]
  arguments[["reference.group"]] <- if (is.na(reference)) .nmaReferenceGroup(dataset, options, pairwise) else reference
  arguments[["backtransf"]]      <- FALSE

  return(arguments)
}

.nmaCallSpecification   <- function(dataset, options, pairwise = NULL, reference = NA) {

  if (!.nmaIsArmBased(options)) {

    columns <- list(
      TE      = .nmaColumn(options[["effectSize"]]),
      seTE    = .nmaColumn(options[["effectSizeStandardError"]]),
      treat1  = .nmaColumn(options[["treatment1"]]),
      treat2  = .nmaColumn(options[["treatment2"]]),
      studlab = .nmaColumn(options[["studyId"]])
    )

    return(list(
      pairwise   = NULL,
      connection = list(fun = "netmeta::netconnection", arguments = columns[c("treat1", "treat2", "studlab")]),
      fit        = list(fun = "netmeta::netmeta", arguments = c(columns, .nmaSummaryMeasureArgument(options), .nmaModelArguments(dataset, options, netmetabin = FALSE, pairwise = NULL, reference = reference)))
    ))
  }

  # arm-based data: contrasts are computed by pairwise()
  pooling <- .nmaPoolingMethod(options)

  if (pooling == "inverseVariance") {
    fit <- list(fun = "netmeta::netmeta", arguments = c(list(.nmaObject("pw")), .nmaModelArguments(dataset, options, netmetabin = FALSE, pairwise = pairwise, reference = reference)))
  } else {
    method <- c(mantelHaenszel = "MH", nonCentralHypergeometric = "NCH", penalizedLogisticRegression = "LRP")[[pooling]]
    fit    <- list(fun = "netmeta::netmetabin", arguments = c(list(.nmaObject("pw"), method = method), .nmaModelArguments(dataset, options, netmetabin = TRUE, pairwise = pairwise, reference = reference)))
  }

  # comparisons without an effect size can still contribute to the count-based methods
  connectionArguments <- c(list(.nmaObject("pw")), if (pooling != "inverseVariance") list(drop.NA = FALSE))

  return(list(
    pairwise   = .nmaPairwiseSpecification(options),
    connection = list(fun = "netmeta::netconnection", arguments = connectionArguments),
    fit        = fit
  ))
}

.nmaEvaluateCode        <- function(code, dataset, objects) {

  # the code refers to the data by the name used in the R code of the (sub)group and to the previous results by their names
  environment <- list2env(c(objects, list(dataset = dataset, subgroupData = dataset)), parent = baseenv())

  return(eval(parse(text = code)[[1]], envir = environment))
}

.nmaEvaluateCall        <- function(call, dataset, objects) {

  # the rows of the data of the fit that are kept (R code that gives the rows) and the columns that are defined by code
  if (!is.null(call[["fitRows"]]))
    objects[["fit"]][["data"]] <- objects[["fit"]][["data"]][.nmaEvaluateCode(call[["fitRows"]], dataset, objects), , drop = FALSE]

  for (name in names(call[["fitData"]]))
    objects[["fit"]][["data"]][[name]] <- .nmaEvaluateCode(call[["fitData"]][[name]], dataset, objects)

  arguments <- lapply(call[["arguments"]], function(argument) {
    if (inherits(argument, "nmaColumn")) {
      # factors (nominal variables) are passed as labels
      return(if (is.factor(dataset[[argument[["name"]]]])) as.character(dataset[[argument[["name"]]]]) else dataset[[argument[["name"]]]])
    } else if (inherits(argument, "nmaObject")) {
      return(objects[[argument[["name"]]]])
    } else if (inherits(argument, "nmaExpression")) {
      return(.nmaEvaluateCode(argument[["code"]], dataset, objects))
    } else if (inherits(argument, "nmaSymbol")) {
      return(as.name(argument[["name"]]))
    }
    return(argument)
  })

  # the arguments are values: no NULL elements are specified
  arguments <- arguments[!vapply(arguments, is.null, logical(1))]

  package  <- strsplit(call[["fun"]], "::", fixed = TRUE)[[1]]
  fun      <- getExportedValue(package[1], package[2])

  return(do.call(fun, arguments))
}

# Fitting ----

.nmaError               <- function(message) {

  return(structure(message, class = "try-error", condition = simpleError(message)))
}

.nmaTryCall             <- function(call, dataset, objects) {

  # netmeta reports the excluded comparisons (e.g., missing effect sizes) as warnings and printed output
  warnings <- character()
  result   <- NULL

  utils::capture.output(
    result <- withCallingHandlers(
      try(.nmaEvaluateCall(call, dataset, objects), silent = TRUE),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )

  attr(result, "warnings") <- unique(warnings)

  return(result)
}

.nmaDisconnectedMessage <- function(connection, options) {

  subnetworks <- sort(unique(connection[["subnet"]]))
  treatments  <- vapply(seq_along(subnetworks), function(i) {
    rows <- connection[["subnet"]] == subnetworks[i]
    paste0("(", i, ") ", paste(sort(unique(c(connection[["treat1"]][rows], connection[["treat2"]][rows]))), collapse = ", "))
  }, character(1))

  message <- gettextf(
    "The network is disconnected: it consists of %1$s separate subnetworks, %2$s.",
    length(subnetworks), paste(treatments, collapse = "; ")
  )

  # (the component model is not available for the count-based pooling methods)
  if (.nmaIsCountBased(options))
    return(message)

  return(paste(message, gettext("A component network meta-analysis can analyse disconnected networks.")))
}

# Messages of netmeta that refer to arguments of netmeta() are rewritten in terms of the module (other messages are kept).
.nmaCleanNetmetaMessage <- function(message, dataset, options) {

  lines <- strsplit(message, "\n", fixed = TRUE)[[1]]

  if (length(lines) == 0)
    return(message)

  # a multi-arm study of arm-based data with comparisons without an effect size (pairwise() drops them, e.g., without events in two arms)
  # (netmeta() states the removed comparisons first and the studies in the following line)
  if (.nmaIsArmBased(options) && grepl("wrong number of comparisons", message, fixed = TRUE)) {

    studyLine <- lines[grepl("wrong number of comparisons", lines, fixed = TRUE)][1]
    studies   <- regmatches(studyLine, gregexpr("'[^']+'", studyLine))[[1]]
    studies <- substr(studies, 2, nchar(studies) - 1)

    if (length(studies) == 0)
      return(message)

    return(sprintf(
      ngettext(length(studies),
               "The multi-arm study %1$s has comparisons without an effect size (e.g., events in all or in none of the participants of two of its arms), but a multi-arm study needs all of its comparisons. Set 'Drop studies with no cases/events' in the 'Advanced' section to 'No' to keep these comparisons (with the continuity correction), or remove the study.",
               "The multi-arm studies %1$s have comparisons without an effect size (e.g., events in all or in none of the participants of two of their arms), but a multi-arm study needs all of its comparisons. Set 'Drop studies with no cases/events' in the 'Advanced' section to 'No' to keep these comparisons (with the continuity correction), or remove the studies."),
      .nmaStudyNames(studies)
    ))
  }

  # a multi-arm study of contrast-based data without all of its comparisons: the number of arms is the number of its treatments
  if (!.nmaIsArmBased(options) && grepl("wrong number of comparisons", lines[1], fixed = TRUE)) {

    studies      <- regmatches(lines[1], gregexpr("'[^']+'", lines[1]))[[1]]
    studies      <- substr(studies, 2, nchar(studies) - 1)
    datasetStudy <- as.character(dataset[[options[["studyId"]]]])

    if (length(studies) == 0 || !all(studies %in% datasetStudy))
      return(message)

    treatments <- lapply(.nmaTreatmentVariables(options), function(variable) as.character(dataset[[variable]]))
    arms       <- vapply(studies, function(study) length(unique(unlist(lapply(treatments, function(x) x[datasetStudy == study])))), numeric(1))
    texts      <- gettextf(
      "Study '%1$s' has a wrong number of comparisons: a study with %2$s arms needs all %3$s pairwise comparisons.",
      studies, arms, arms * (arms - 1) / 2
    )

    if (length(texts) > 5)
      texts <- c(texts[1:5], sprintf(
        ngettext(length(texts) - 5, "%1$i more study has a wrong number of comparisons.", "%1$i more studies have a wrong number of comparisons."),
        length(texts) - 5
      ))

    return(paste(texts, collapse = "\n"))
  }

  # inconsistent, incomplete, or duplicated comparisons of multi-arm studies: the lines about the studies are kept
  if (grepl("in multi-arm studies!", lines[1], fixed = TRUE)) {

    problems <- trimws(lines[-1])
    problems <- problems[startsWith(problems, "- ") & !grepl("netmeta()", problems, fixed = TRUE) & !grepl("rgument", problems, fixed = TRUE)]

    if (length(problems) == 0)
      return(message)

    hint <- if (grepl("tol.multiarm", message, fixed = TRUE)) {
      gettext("Please check the data of the multi-arm studies. The tolerance of the consistency check can be changed by the 'Multi-arm consistency tolerance' option in the 'Advanced' section.")
    } else {
      gettext("Please check the data of the multi-arm studies.")
    }

    return(paste(c(lines[1], problems, hint), collapse = "\n"))
  }

  return(message)
}

# The Mantel-Haenszel and the non-central hypergeometric methods exclude the studies and designs with zero cells before they fit the network
# (netmetabin reports them as warnings). If the fit fails after exclusions, the exclusions can have disconnected the network (netmetabin then
# stops with a singular system of equations, whose message depends on the language of R): the message names the exclusions that netmetabin
# reported and gives the error (NULL if there are no exclusions or for the other pooling methods).
.nmaExclusionMessage    <- function(message, warnings, options) {

  excluded <- warnings[grepl("excluded from network meta-analysis", warnings, fixed = TRUE)]

  if (!.nmaPoolingMethod(options) %in% c("mantelHaenszel", "nonCentralHypergeometric") || length(excluded) == 0)
    return(NULL)

  return(paste(c(
    gettext("The analysis failed after the studies and designs that the selected pooling method cannot analyse (studies without events or with all events, and treatment arms without events or with all events in a design) were excluded: the exclusions can disconnect the network. Penalized logistic regression keeps all studies and designs."),
    excluded,
    gettextf("The error of the analysis: %1$s", message)
  ), collapse = "\n"))
}

.nmaCleanFitError       <- function(fit, dataset, options) {

  if (!jaspBase::isTryError(fit) || is.null(attr(fit, "condition")))
    return(fit)

  message   <- conditionMessage(attr(fit, "condition"))
  cleaned   <- .nmaCleanNetmetaMessage(message, dataset, options)
  exclusion <- .nmaExclusionMessage(cleaned, attr(fit, "warnings"), options)

  if (!is.null(exclusion))
    cleaned <- exclusion

  if (identical(cleaned, message))
    return(fit)

  result <- .nmaError(cleaned)
  attr(result, "warnings") <- attr(fit, "warnings")

  return(result)
}

# Studies of arm-based data with a single arm: pairwise() drops them without a message
.nmaSingleArmStudies    <- function(dataset, options, pairwise) {

  if (!.nmaIsArmBased(options) || is.null(pairwise) || jaspBase::isTryError(pairwise))
    return(NULL)

  return(setdiff(unique(as.character(dataset[[options[["studyId"]]]])), as.character(pairwise[["studlab"]])))
}

# Studies that lost comparisons (contrast-based data) or arms (arm-based data) to missing values and remain in the analysis with the others
# (a study of arm-based data without a comparison in the fitted network is not in the analysis: pairwise() drops a study with a single
# remaining arm, and netmeta() drops the comparisons without an effect size, e.g., of a study without events in the remaining arms)
.nmaIncompleteStudies   <- function(dataset, options, fit) {

  studies    <- as.character(dataset[[options[["studyId"]]]])
  incomplete <- intersect(unique(attr(dataset, "omittedStudies")), studies)

  if (.nmaIsArmBased(options)) {
    arms       <- table(studies)
    incomplete <- incomplete[arms[incomplete] >= 2]

    if (!jaspBase::isTryError(fit) && !is.null(fit[["studlab"]]))
      incomplete <- intersect(incomplete, as.character(fit[["studlab"]]))
  }

  return(incomplete)
}

# Studies whose comparisons netmeta() excluded because their effect size or its standard error is missing or zero (e.g., arm-based binary
# data with events in all or in none of the participants of the compared arms); netmeta() reports them by a generic warning. (A multi-arm
# study that keeps only some of its comparisons stops netmeta() with an error, see .nmaCleanNetmetaMessage.)
.nmaExcludedStudies     <- function(fit) {

  data <- if (!jaspBase::isTryError(fit)) fit[["data"]]

  if (is.null(data) || is.null(data[[".excl"]]))
    return(NULL)

  excluded <- data[[".excl"]] %in% TRUE & (is.na(data[[".TE"]]) | is.na(data[[".seTE"]]) | data[[".seTE"]] %in% 0)

  return(unique(as.character(data[[".studlab"]][excluded])))
}

# The rows of the data of a netmeta fit that were analysed: the data keep the comparisons that netmeta excluded and mark them in '.excl'.
.nmaAnalysedRows        <- function(fit) {

  excluded <- fit[["data"]][[".excl"]]

  return(if (is.null(excluded)) rep(TRUE, nrow(fit[["data"]])) else !excluded %in% TRUE)
}

.nmaFitScope            <- function(dataset, options, subgroupName, fullDataset = FALSE) {

  specification     <- .nmaCallSpecification(dataset, options)
  objects           <- list()
  connection        <- NULL
  fallbackReference <- NULL

  # contrasts of arm-based data
  if (!is.null(specification[["pairwise"]]))
    objects[["pw"]] <- .nmaTryCall(specification[["pairwise"]], dataset, objects)

  if (jaspBase::isTryError(objects[["pw"]])) {
    fit <- objects[["pw"]]
  } else {

    # the reference treatment is resolved against the treatments retained in the contrasts
    specification <- .nmaCallSpecification(dataset, options, objects[["pw"]])

    # a disconnected network cannot be analyzed by the standard network meta-analysis
    connection <- .nmaTryCall(specification[["connection"]], dataset, objects)

    if (!jaspBase::isTryError(connection) && connection[["n.subnets"]] > 1) {
      fit <- .nmaError(.nmaDisconnectedMessage(connection, options))
    } else if (.nmaPoolingMethod(options) == "penalizedLogisticRegression" && length(.nmaRetainedTreatments(dataset, options, objects[["pw"]])) < 3) {
      # (netmetabin() fails for penalized logistic regression with two treatments)
      fit <- .nmaError(gettext("Penalized logistic regression requires at least three treatments."))
    } else {
      fit <- .nmaTryCall(specification[["fit"]], dataset, objects)

      # the reference treatment (the requested one, or the default of netmetabin, which is selected before the studies and designs that the
      # pooling method cannot analyse are dropped) can be missing in the fitted network: the network is fitted again with the default
      # reference treatment of netmeta among its treatments (the first one)
      if (!jaspBase::isTryError(fit) && !fit[["reference.group"]] %in% fit[["trts"]]) {
        fallbackReference <- fit[["trts"]][1]
        specification     <- .nmaCallSpecification(dataset, options, objects[["pw"]], reference = fallbackReference)
        fit               <- .nmaTryCall(specification[["fit"]], dataset, objects)
      }

      fit <- .nmaCleanFitError(fit, dataset, options)
    }
  }

  fit <- .nmaSetFitAttributes(fit, dataset, options, subgroupName, fullDataset, objects[["pw"]])

  scope <- list(
    data              = dataset,
    pairwise          = objects[["pw"]],
    connection        = connection,
    fit               = fit,
    fallbackReference = fallbackReference
  )

  # the component model replaces the standard model as the fit of the outputs (the standard model, which netcomb() extends and the
  # network plot describes, is kept)
  if (.nmaIsComponentModel(options)) {
    scope[["standardFit"]] <- fit
    componentFit           <- .nmaComponentModelFit(scope, options)
    attr(componentFit, "warnings") <- c(attr(fit, "warnings"), attr(componentFit, "warnings"))
    scope[["fit"]]         <- .nmaSetFitAttributes(componentFit, dataset, options, subgroupName, fullDataset, objects[["pw"]])
    # (the comparisons that netmeta() excluded are recorded in the data of the standard model; the component model of a disconnected network has
    # no standard model: discomb() records them in its own data)
    if (!jaspBase::isTryError(fit))
      attr(scope[["fit"]], "excludedStudies") <- attr(fit, "excludedStudies")
  }

  return(scope)
}

# The scope of a fit (subgroup, full dataset, data) and the studies that are dropped or changed without an error (reported by the notes
# of the tests table). The warnings of pairwise() and the fit are reported by the notes too.
.nmaSetFitAttributes    <- function(fit, dataset, options, subgroupName, fullDataset, pairwise) {

  attr(fit, "warnings")          <- unique(c(attr(pairwise, "warnings"), attr(fit, "warnings")))
  attr(fit, "subgroup")          <- subgroupName
  attr(fit, "fullDataset")       <- fullDataset
  attr(fit, "dataset")           <- dataset
  attr(fit, "singleArmStudies")  <- .nmaSingleArmStudies(dataset, options, pairwise)
  attr(fit, "incompleteStudies") <- .nmaIncompleteStudies(dataset, options, fit)
  attr(fit, "excludedStudies")   <- .nmaExcludedStudies(fit)

  return(fit)
}

# The standard network meta-analysis of a scope (also with the component model).
.nmaStandardFit         <- function(scope) {

  return(if (!is.null(scope[["standardFit"]])) scope[["standardFit"]] else scope[["fit"]])
}

.nmaScopeDatasets       <- function(dataset, options) {

  # the full dataset and one dataset per subgroup level
  scopes <- list()
  scopes[["__fullDataset"]] <- dataset
  attr(scopes[["__fullDataset"]], "subgroupName") <- gettext("Full dataset")

  if (options[["subgroup"]] != "") {

    for (subgroupLevel in unique(dataset[[options[["subgroup"]]]])) {

      subgroupData <- droplevels(dataset[dataset[[options[["subgroup"]]]] == subgroupLevel, , drop = FALSE])

      # observations omitted due to missing values belonging to the subgroup
      attr(subgroupData, "NAs")            <- sum(attr(dataset, "omittedSubgroup") == as.character(subgroupLevel), na.rm = TRUE)
      attr(subgroupData, "omittedStudies") <- attr(dataset, "omittedStudies")[which(attr(dataset, "omittedSubgroup") == as.character(subgroupLevel))]
      attr(subgroupData, "subgroupName")   <- as.character(subgroupLevel)

      scopes[[paste0("subgroup", subgroupLevel)]] <- subgroupData
    }
  }

  return(scopes)
}

.nmaFitModels           <- function(jaspResults, dataset, options) {

  if (!.nmaReady(options) || !is.null(jaspResults[["nmaFit"]]))
    return()

  state <- createJaspState()
  state$dependOn(.nmaModelDependencies)
  jaspResults[["nmaFit"]] <- state

  scopes <- .nmaScopeDatasets(dataset, options)
  fits   <- list()

  startProgressbar(expectedTicks = length(scopes), label = gettext("Estimating Network Meta-Analysis"))
  for (scope in names(scopes)) {
    fits[[scope]] <- .nmaFitScope(scopes[[scope]], options, attr(scopes[[scope]], "subgroupName"), fullDataset = scope == "__fullDataset")
    progressbarTick()
  }

  state$object <- fits

  return()
}

.nmaExtractScopes       <- function(jaspResults, options) {

  scopes <- jaspResults[["nmaFit"]]$object

  if (is.null(scopes))
    return(NULL)

  # remove the full dataset fit if requested (in subgroup analysis)
  if (options[["subgroup"]] != "" && !options[["includeFullDatasetInSubgroupAnalysis"]])
    scopes <- scopes[names(scopes) != "__fullDataset"]

  return(scopes)
}

.nmaExtractFits         <- function(jaspResults, options) {

  scopes <- .nmaExtractScopes(jaspResults, options)

  if (is.null(scopes))
    return(NULL)

  return(lapply(scopes, function(scope) scope[["fit"]]))
}
