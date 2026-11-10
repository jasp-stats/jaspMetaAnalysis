# Classical network meta-analysis tables.
#
# Builds the input summary, the overall tests, the heterogeneity, the treatment effects,
# and the league table from the netmeta fits.

# Helpers ----

.nmaSuccessfulFits      <- function(fits) {

  return(fits[!vapply(fits, jaspBase::isTryError, logical(1))])
}

.nmaIsRandomFit         <- function(fit) {

  # the tables follow the fitted model, not the (possibly disabled) options:
  # only netmeta provides the between-study variance and the prediction intervals of the random-effects model
  return(!jaspBase::isTryError(fit) && isTRUE(fit[["random"]]) && !inherits(fit, "netmetabin"))
}

.nmaIsRandomModel       <- function(fit) {

  # the model that was fitted (also penalized logistic regression with random effects, which has no between-study variance)
  return(!jaspBase::isTryError(fit) && isTRUE(fit[["random"]]))
}

.nmaIsTestFit           <- function(fit) {

  # (netmeta reports z-tests when the network has no degrees of freedom for heterogeneity)
  return(.nmaIsRandomFit(fit) && identical(fit[["method.random.ci"]], "t-dist") && isTRUE(fit[["df.Q"]] > 0))
}

.nmaIsTestWithoutDf     <- function(fit) {

  return(.nmaIsRandomFit(fit) && identical(fit[["method.random.ci"]], "t-dist") && !isTRUE(fit[["df.Q"]] > 0))
}

# netmeta computes prediction intervals from two degrees of freedom of the overall Q statistic on (the intervals use the t-distribution
# with these degrees of freedom); the intervals of the first few degrees of freedom are very wide
.nmaHasPredictionIntervals <- function(fit) {

  return(.nmaIsRandomFit(fit) && isTRUE(fit[["df.Q"]] >= 2))
}

.nmaHasWidePredictionIntervals <- function(fit) {

  return(.nmaHasPredictionIntervals(fit) && fit[["df.Q"]] < 4)
}

# A note that applies to some of the fits only names them (in a subgroup analysis).
.nmaAddFitNote          <- function(table, note, selected, fits, options) {

  if (length(selected) == 0)
    return()

  isScoped <- options[["subgroup"]] != "" && length(selected) < length(.nmaSuccessfulFits(fits))

  table$addFootnote(if (isScoped) {
    gettextf("%1$s: %2$s", paste(vapply(selected, .nmaScopeQuoted, character(1)), collapse = ", "), note)
  } else {
    note
  })

  return()
}

.nmaAnyFit              <- function(fits, options, isFit, fallback) {

  # table columns follow the fitted models; without a successful fit (e.g., before the data are assigned) the options are used
  successful <- .nmaSuccessfulFits(fits)

  if (length(successful) == 0)
    return(fallback(options))

  return(any(vapply(successful, isFit, logical(1))))
}

.nmaOptionsRandomEffects <- function(options) {

  return(.nmaIsRandomEffects(options) && .nmaPoolingMethod(options) == "inverseVariance")
}

.nmaOptionsTTest        <- function(options) {

  return(.nmaOptionsRandomEffects(options) && options[["fixedEffectTest"]] == "t")
}

.nmaEffectsElement      <- function(fit) {

  return(if (isTRUE(fit[["random"]])) "random" else "common")
}

.nmaIsFailedSingleFit   <- function(fits) {

  return(length(fits) == 1 && jaspBase::isTryError(fits[[1]]))
}

.nmaTransformEffects    <- function(data, options, columns = c("est", "lCi", "uCi", "lPi", "uPi")) {

  columns <- intersect(columns, colnames(data))

  if (options[["transformEffectSize"]] != "none" && length(columns) > 0)
    data[, columns] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(as.matrix(data[, columns, drop = FALSE])))

  return(data)
}

.nmaTransformationMessage <- function(options, anyNA) {

  if (options[["transformEffectSize"]] == "none")
    return(NULL)

  transformation <- .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])

  if (anyNA)
    return(gettextf("NAs in the treatment effects were introduced due to the %1$s transformation. Please verify that you are using the correct effect size transformation.", transformation))

  return(gettextf("The treatment effects are transformed using %1$s transformation. For nonlinear transformations, the transformed estimates are interpreted as medians on the transformed scale.", transformation))
}

# The direction of the treatment effects (the first treatment relative to the second; ratios after the exponential transformation,
# the other transformations keep the sign); 'reference' names the second treatment of all rows
.nmaEffectDirectionMessage <- function(options, reference = NULL) {

  ratio <- options[["transformEffectSize"]] == "exponential"

  if (!is.null(reference)) {
    if (ratio)
      return(gettextf("Estimates above one indicate a larger effect for the treatment than for %1$s.", reference))
    return(gettextf("Positive estimates indicate a larger effect for the treatment than for %1$s.", reference))
  }

  if (ratio)
    return(gettext("Estimates above one indicate a larger effect for the first treatment of the comparison than for the second."))
  return(gettext("Positive estimates indicate a larger effect for the first treatment of the comparison than for the second."))
}

# The scope of a fit or a result: the full dataset or a subgroup (titles of the outputs and notes)
.nmaScopeTitle          <- function(x) {

  return(if (isTRUE(attr(x, "fullDataset"))) gettext("Full dataset") else gettextf("Subgroup: %1$s", attr(x, "subgroup")))
}

.nmaScopeQuoted         <- function(x) {

  return(if (isTRUE(attr(x, "fullDataset"))) gettext("Full dataset") else gettextf("Subgroup '%1$s'", as.character(attr(x, "subgroup"))[1]))
}

.nmaFailedFitFootnotes  <- function(table, fits) {

  # errors of individual subgroup fits
  for (fit in fits[vapply(fits, jaspBase::isTryError, logical(1))])
    table$addFootnote(
      gettextf("%1$s: the model failed with the following error: %2$s", .nmaScopeQuoted(fit), .maTryCleanErrorMessages(fit)),
      symbol = gettext("Error:")
    )

  return()
}

# Downstream results ----

# Output containers (the model dependencies and the checkboxes of the outputs; an emptied container is removed)
.nmaExtractOutputContainer <- function(jaspResults, key, title, position, flags) {

  if (!is.null(jaspResults[[key]]))
    return(jaspResults[[key]])

  container <- createJaspContainer(title)
  container$dependOn(c(.nmaModelDependencies, "includeFullDatasetInSubgroupAnalysis", flags))
  container$position <- position
  jaspResults[[key]] <- container

  return(container)
}

# Results computed from the fits (e.g., rankogram, netsplit) are cached in a state that is independent of the display options.
.nmaExtractState <- function(jaspResults, key, dependencies, compute) {

  if (!is.null(jaspResults[[key]]))
    return(jaspResults[[key]]$object)

  state <- createJaspState()
  state$dependOn(c(.nmaModelDependencies, "includeFullDatasetInSubgroupAnalysis", dependencies))
  jaspResults[[key]] <- state

  state$object <- compute()

  return(state$object)
}

# The ranking needs all contrasts of the treatments (a component fit of a disconnected network can lack some)
.nmaStopIfNotEstimable  <- function(fit) {

  if (.nmaHasNotEstimableContrasts(fit))
    stop(gettext("The ranking is not available because some treatment comparisons are not estimable (the treatments are in different subnetworks and their components do not identify the contrasts)."), call. = FALSE)

  return()
}

# One result per fit: failures (also of the fit itself) are kept as try-error with the subgroup attribute.
# A label shows the progress of slow computations.
.nmaComputeResults      <- function(fits, compute, label = NULL) {

  if (!is.null(label))
    startProgressbar(expectedTicks = length(fits), label = label)

  return(lapply(fits, function(fit) {

    if (!is.null(label))
      on.exit(progressbarTick())

    if (jaspBase::isTryError(fit))
      return(fit)

    result <- NULL
    utils::capture.output(result <- suppressWarnings(try(compute(fit), silent = TRUE)))

    if (!is.null(result)) {
      attr(result, "subgroup")    <- attr(fit, "subgroup")
      attr(result, "fullDataset") <- attr(fit, "fullDataset")
    }

    return(result)
  }))
}

.nmaSuccessfulResults   <- function(results) {

  return(results[!vapply(results, jaspBase::isTryError, logical(1))])
}

.nmaFailureMessage      <- function(results) {

  failed <- results[vapply(results, jaspBase::isTryError, logical(1))]

  if (length(failed) == 0)
    return(NULL)

  messages <- vapply(failed, .maTryCleanErrorMessages, character(1))

  if (length(unique(messages)) == 1)
    return(unname(messages[1]))

  scopes <- vapply(failed, .nmaScopeQuoted, character(1))

  return(paste(gettextf("%1$s: %2$s", scopes, messages), collapse = "\n"))
}

# Footnotes for the failed results of one or more result lists (the same failure is reported once).
.nmaFailedResultFootnotes <- function(element, ...) {

  results <- do.call(c, list(...))
  failed  <- results[vapply(results, jaspBase::isTryError, logical(1))]

  if (length(failed) == 0)
    return()

  messages <- vapply(failed, .maTryCleanErrorMessages, character(1))
  scopes   <- vapply(failed, .nmaScopeQuoted, character(1))

  # the scopes are named when the results belong to several fits
  isSubgroup <- length(unique(vapply(results, function(result) as.character(attr(result, "subgroup"))[1], character(1)))) > 1
  texts     <- if (isSubgroup) {
    gettextf("%1$s: the results could not be computed: %2$s", scopes, messages)
  } else {
    unname(messages)
  }

  for (text in unique(texts))
    element$addFootnote(text, symbol = gettext("Warning:"))

  return()
}

# Whether the element has nothing to show (the error is displayed); otherwise failures of single fits become footnotes (tables only).
.nmaReportFailedResults <- function(element, results, footnotes = TRUE) {

  if (length(results) > 0 && all(vapply(results, jaspBase::isTryError, logical(1)))) {
    element$setError(.nmaFailureMessage(results))
    return(TRUE)
  }

  if (footnotes)
    .nmaFailedResultFootnotes(element, results)

  return(FALSE)
}

# The separator of the treatments in the names of the comparisons (netmeta changes ":" if a treatment name contains it)
.nmaTreatmentSeparator  <- function(fit) {

  return(if (is.null(fit[["sep.trts"]])) ":" else fit[["sep.trts"]])
}

# Comparisons named "A:B" by netmeta as pairs of treatments (the orientation of the names)
.nmaComparisonPairs     <- function(comparisons, treatments, separator = ":") {

  candidates <- expand.grid(first = treatments, second = treatments, stringsAsFactors = FALSE)
  candidates <- candidates[candidates$first != candidates$second, , drop = FALSE]
  index      <- match(comparisons, paste(candidates$first, candidates$second, sep = separator))

  return(data.frame(first = candidates$first[index], second = candidates$second[index]))
}

# Comparison labels "A vs. B"; with a reference treatment, the comparisons are oriented versus the reference.
.nmaComparisonLabels    <- function(comparisons, treatments, separator = ":", reference = NULL) {

  pairs     <- .nmaComparisonPairs(comparisons, treatments, separator)
  unmatched <- is.na(pairs$first)

  if (!is.null(reference)) {
    swap                 <- !unmatched & pairs$first == reference
    first                <- pairs$first
    pairs$first[swap]    <- pairs$second[swap]
    pairs$second[swap]   <- first[swap]
  }

  labels            <- gettextf("%1$s vs. %2$s", pairs$first, pairs$second)
  labels[unmatched] <- gsub(separator, " vs. ", comparisons[unmatched], fixed = TRUE)

  return(labels)
}

.nmaTreatmentColors     <- function(n, aesthetic) {

  colors <- if (n <= 8) jaspGraphs::JASPcolors()[seq_len(n)] else grDevices::hcl.colors(n, "Dark 3")

  return(if (aesthetic == "color") ggplot2::scale_color_manual(values = colors) else ggplot2::scale_fill_manual(values = colors))
}

# Model summary ----

.nmaModelSummaryTables  <- function(jaspResults, options) {

  if (options[["inputSummary"]])
    .nmaInputSummaryTable(jaspResults, options)

  .nmaTestsTable(jaspResults, options)
  .nmaHeterogeneityTable(jaspResults, options)

  return()
}

.nmaExtractModelSummaryContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["modelSummaryContainer"]]))
    return(jaspResults[["modelSummaryContainer"]])

  modelSummaryContainer <- createJaspContainer(gettext("Network Meta-Analysis Summary"))
  modelSummaryContainer$dependOn(.nmaModelDependencies)
  modelSummaryContainer$position <- 1
  jaspResults[["modelSummaryContainer"]] <- modelSummaryContainer

  return(modelSummaryContainer)
}

# Input summary ----

.nmaInputSummaryTable   <- function(jaspResults, options) {

  modelSummaryContainer <- .nmaExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["inputSummaryTable"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  inputSummaryTable          <- createJaspTable(gettext("Input Summary"))
  inputSummaryTable$position <- 1
  inputSummaryTable$showSpecifiedColumnsOnly <- TRUE
  inputSummaryTable$dependOn(c("inputSummary", "includeFullDatasetInSubgroupAnalysis"))
  modelSummaryContainer[["inputSummaryTable"]] <- inputSummaryTable

  .maAddSubgroupColumn(inputSummaryTable, options)
  inputSummaryTable$addColumnInfo(name = "k",        title = gettext("Studies (k)"),           type = "integer")
  inputSummaryTable$addColumnInfo(name = "m",        title = gettext("Comparisons (m)"),       type = "integer")
  inputSummaryTable$addColumnInfo(name = "n",        title = gettext("Treatments (n)"),        type = "integer")
  inputSummaryTable$addColumnInfo(name = "d",        title = gettext("Designs (d)"),           type = "integer")
  inputSummaryTable$addColumnInfo(name = "multiarm", title = gettext("Multi-arm studies"),     type = "integer")
  if (.nmaIsComponentModel(options))
    inputSummaryTable$addColumnInfo(name = "components", title = gettext("Components"),     type = "integer")
  if (.nmaIsArmBased(options) && options[["outcomeType"]] %in% c("binary", "continuous"))
    inputSummaryTable$addColumnInfo(name = "observations", title = gettext("Observations"), type = "integer")
  # (netmeta provides no number of participants for incidence rates)
  if (.nmaIsArmBased(options) && options[["outcomeType"]] == "incidenceRate")
    inputSummaryTable$addColumnInfo(name = "observations", title = gettext("Person-time"), type = "number")

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    inputSummaryTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  # (the notes on the data and the fits are reported by the tests table)
  .nmaFailedFitFootnotes(inputSummaryTable, fits)

  # (the rows of failed fits are reported by a footnote)
  rows <- .maSafeRbind(lapply(fits, .nmaRowInputSummary, options = options))
  rows <- .maSafeOrderAndSimplify(rows, "subgroup", options)

  if (!is.null(rows))
    inputSummaryTable$setData(rows)

  return()
}

# The warnings of pairwise() and netmeta(); with subgroups, each warning names the subgroup (or the full dataset) it belongs to.
.nmaNetmetaWarnings     <- function(fits, options) {

  warnings <- unlist(lapply(fits, function(fit) {

    messages <- attr(fit, "warnings")

    # (the comparisons without an effect size are named by a note of the excluded studies: the warnings about them are not repeated; discomb()
    # adds a warning about replacing the excluded comparisons)
    if (length(attr(fit, "excludedStudies")) > 0) {
      excluded <- c("Comparisons with missing TE / seTE or zero seTE", "Comparison with missing TE / seTE or zero seTE", "number of items to replace is not a multiple of replacement length")
      messages <- messages[!vapply(messages, function(message) any(startsWith(message, excluded)), logical(1))]
    }

    if (length(messages) > 0 && options[["subgroup"]] != "")
      messages <- gettextf("%1$s: %2$s", .nmaScopeQuoted(fit), messages)

    return(messages)
  }))

  return(unique(warnings))
}

.nmaRowInputSummary     <- function(fit, options) {

  if (jaspBase::isTryError(fit))
    return(NULL)

  row <- data.frame(
    subgroup = attr(fit, "subgroup"),
    k        = fit[["k"]],
    m        = fit[["m"]],
    n        = fit[["n"]],
    d        = fit[["d"]],
    multiarm = sum(fit[["narms"]] > 2)
  )

  # (the components include the interactions)
  if (.nmaIsComponentFit(fit))
    row[["components"]] <- length(fit[["comps"]])

  # the number of participants is not available for all data (e.g., contrast-based); incidence rates report the person-time
  row[["observations"]] <- if (!is.null(fit[["n.trts"]])) {
    sum(fit[["n.trts"]])
  } else if (.nmaIsArmBased(options) && options[["outcomeType"]] == "incidenceRate") {
    .nmaPersonTime(fit, options)
  } else {
    NA_integer_
  }

  return(row)
}

# The person-time of the arms in the analysed comparisons (as netmeta sums the sample sizes of these arms for n.trts)
.nmaPersonTime          <- function(fit, options) {

  data       <- fit[["data"]]
  analysed   <- if (is.null(data[[".excl"]])) rep(TRUE, nrow(data)) else !data[[".excl"]]
  arms       <- unique(data.frame(
    study     = rep(as.character(data[[".studlab"]]), 2),
    treatment = c(as.character(data[[".treat1"]]), as.character(data[[".treat2"]]))
  )[rep(analysed, 2), , drop = FALSE])

  dataset    <- attr(fit, "dataset")
  personTime <- data.frame(
    study     = as.character(dataset[[options[["studyId"]]]]),
    treatment = as.character(dataset[[options[["treatment"]]]]),
    time      = dataset[[options[["armPersonTime"]]]]
  )

  return(sum(merge(arms, personTime, by = c("study", "treatment"))[["time"]]))
}

# The names of studies for a note: the first five, then the number of the others
.nmaStudyNames          <- function(studies) {

  shown <- paste0("'", utils::head(studies, 5), "'", collapse = ", ")

  if (length(studies) > 5)
    shown <- paste0(shown, ", ", gettextf("and %1$i more", length(studies) - 5))

  return(shown)
}

.nmaModelSummaryMessages <- function(fits, options) {

  messages <- NULL
  isSubgroup <- options[["subgroup"]] != ""

  # omitted observations
  for (fit in fits) {
    omitted <- attr(attr(fit, "dataset"), "NAs")
    if (omitted > 0) {
      message  <- sprintf(ngettext(omitted, "%1$i observation was omitted due to missing values.", "%1$i observations were omitted due to missing values."), omitted)
      messages <- c(messages, if (isSubgroup) gettextf("%1$s: %2$s", .nmaScopeQuoted(fit), message) else message)
    }
  }

  # studies that the data preparation or pairwise() changed or dropped without an error
  for (fit in fits) {

    singleArm <- attr(fit, "singleArmStudies")
    if (length(singleArm) > 0) {
      message  <- sprintf(
        ngettext(length(singleArm), "%1$i study with a single arm was excluded (a study needs at least two arms): %2$s.", "%1$i studies with a single arm were excluded (a study needs at least two arms): %2$s."),
        length(singleArm), .nmaStudyNames(singleArm)
      )
      messages <- c(messages, if (isSubgroup) gettextf("%1$s: %2$s", .nmaScopeQuoted(fit), message) else message)
    }

    excluded <- attr(fit, "excludedStudies")
    if (length(excluded) > 0) {
      message  <- sprintf(
        ngettext(length(excluded),
                 "%1$i study was excluded because none of its effect sizes could be computed (e.g., events in all or in none of the participants of the compared arms): %2$s.",
                 "%1$i studies were excluded because none of their effect sizes could be computed (e.g., events in all or in none of the participants of the compared arms): %2$s."),
        length(excluded), .nmaStudyNames(excluded)
      )
      messages <- c(messages, if (isSubgroup) gettextf("%1$s: %2$s", .nmaScopeQuoted(fit), message) else message)
    }

    incomplete <- attr(fit, "incompleteStudies")
    if (length(incomplete) > 0) {
      message  <- if (.nmaIsArmBased(options)) {
        sprintf(
          ngettext(length(incomplete), "%1$i study had arms omitted due to missing values and is analysed with its remaining arms: %2$s.", "%1$i studies had arms omitted due to missing values and are analysed with their remaining arms: %2$s."),
          length(incomplete), .nmaStudyNames(incomplete)
        )
      } else {
        sprintf(
          ngettext(length(incomplete), "%1$i multi-arm study had comparisons omitted due to missing values and is analysed with its remaining comparisons: %2$s.", "%1$i multi-arm studies had comparisons omitted due to missing values and are analysed with their remaining comparisons: %2$s."),
          length(incomplete), .nmaStudyNames(incomplete)
        )
      }
      messages <- c(messages, if (isSubgroup) gettextf("%1$s: %2$s", .nmaScopeQuoted(fit), message) else message)
    }
  }

  # pooling method (netmetabin fits store the method)
  for (fit in .nmaSuccessfulFits(fits)) {
    if (inherits(fit, "netmetabin")) {
      messages <- c(messages, switch(
        fit[["method"]],
        MH  = NULL,
        NCH = NULL,
        LRP = if (isTRUE(fit[["random"]]))
          gettext("Penalized logistic regression models random effects by a dispersion factor. The between-study variance, the Q statistics, I\u00B2, and the prediction intervals are not available.")
      ))
    }
  }

  return(unique(messages))
}

# Overall tests ----

.nmaTestsTable          <- function(jaspResults, options) {

  modelSummaryContainer <- .nmaExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["testsTable"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  testsTable          <- createJaspTable(gettext("Network Meta-Analytic Tests"))
  testsTable$position <- 2
  testsTable$showSpecifiedColumnsOnly <- TRUE
  testsTable$dependOn("includeFullDatasetInSubgroupAnalysis")
  modelSummaryContainer[["testsTable"]] <- testsTable

  testsTable$addColumnInfo(name = "test", type = "string", title = "")
  .maAddSubgroupColumn(testsTable, options)
  testsTable$addColumnInfo(name = "stat", type = "string", title = gettext("Test"))
  testsTable$addColumnInfo(name = "pval", type = "pvalue", title = gettext("p"))

  if (is.null(fits)) {
    # (the analysis waits for the variables of the selected model)
    for (message in .nmaMissingVariablesMessage(options))
      testsTable$addFootnote(message)
    return()
  }

  if (.nmaIsFailedSingleFit(fits)) {
    testsTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  # notes on the data and the fits (omitted observations and studies, reference treatment, pooling method), as in the classical meta-analysis
  for (message in .nmaModelSummaryMessages(fits, options))
    testsTable$addFootnote(message)
  for (warning in .nmaNetmetaWarnings(fits, options))
    testsTable$addFootnote(warning, symbol = gettext("Warning:"))
  .nmaFailedFitFootnotes(testsTable, fits)

  tests <- .maSafeRbind(lapply(fits, .nmaRowTests))
  tests <- .maSafeOrderAndSimplify(tests, "test", options)

  # (no rows without a test statistic)
  successful <- .nmaSuccessfulFits(fits)
  if (length(successful) > 0 && (is.null(tests) || all(is.na(tests[["stat"]])))) {
    testsTable$addFootnote(if (any(vapply(successful, inherits, logical(1), "netmetabin"))) {
      gettext("Heterogeneity and inconsistency tests are not available for the selected pooling method.")
    } else {
      gettext("No heterogeneity or inconsistency tests are available because the network has no degrees of freedom.")
    })
  }

  for (message in .nmaComponentMessages(fits, options))
    testsTable$addFootnote(message)

  if (!is.null(tests))
    testsTable$setData(tests)

  return()
}

.nmaTestRow             <- function(test, stat, df, pval, subgroup) {

  # tests without degrees of freedom (e.g., a single design) or not provided by the method are not reported
  if (!.maIsFiniteScalar(stat) || !.maIsFiniteScalar(df) || df <= 0)
    return(NULL)

  return(data.frame(
    subgroup = subgroup,
    test     = test,
    stat     = sprintf(paste0("Q(%1$s) = ", if (stat < 1e5) "%2$.2f" else "%2$.3g"), .maPrintDf(df), stat),
    pval     = pval
  ))
}

.nmaRowTests            <- function(fit) {

  # failed fits are reported by a footnote
  if (jaspBase::isTryError(fit))
    return(NULL)

  subgroup <- attr(fit, "subgroup")

  # the component model: the additive model, the standard model, and their difference (the standard model is not available for a
  # disconnected network)
  if (.nmaIsComponentFit(fit)) {
    disconnected <- isTRUE(attr(fit, "components")[["disconnected"]])
    return(.maSafeRbind(list(
      .nmaTestRow(gettext("Additive model"), fit[["Q.additive"]], fit[["df.Q.additive"]], fit[["pval.Q.additive"]], subgroup),
      if (!disconnected) .nmaTestRow(gettext("Standard model"), fit[["Q.standard"]], fit[["df.Q.standard"]], fit[["pval.Q.standard"]], subgroup),
      if (!disconnected) .nmaTestRow(gettext("Difference"),     fit[["Q.diff"]],     fit[["df.Q.diff"]],     fit[["pval.Q.diff"]],     subgroup)
    )))
  }

  rows     <- list(
    .nmaTestRow(gettext("Total"),          fit[["Q"]],               fit[["df.Q"]],               fit[["pval.Q"]],               subgroup),
    .nmaTestRow(gettext("Within designs"), fit[["Q.heterogeneity"]], fit[["df.Q.heterogeneity"]], fit[["pval.Q.heterogeneity"]], subgroup),
    .nmaTestRow(gettext("Between designs"), fit[["Q.inconsistency"]], fit[["df.Q.inconsistency"]], fit[["pval.Q.inconsistency"]], subgroup)
  )

  # full design-by-treatment interaction model of the random-effects model (not implemented for netmetabin)
  if (isTRUE(fit[["random"]]) && !inherits(fit, "netmetabin")) {
    # (a fixed between-study variance defines the preset version of the interaction model)
    decomposition <- suppressWarnings(try(.nmaRunCall(.nmaCallDecompDesign(fit), fit), silent = TRUE))
    interaction   <- if (!jaspBase::isTryError(decomposition)) decomposition[[if (is.null(fit[["tau.preset"]])) "Q.inc.random" else "Q.inc.random.preset"]]
    if (!is.null(interaction))
      rows <- c(rows, list(.nmaTestRow(
        gettext("Between designs (design-by-treatment interaction)"),
        interaction[["Q"]], interaction[["df"]], interaction[["pval"]], subgroup
      )))
  }

  return(.maSafeRbind(rows))
}

# Heterogeneity ----

.nmaFitHeterogeneityParameters <- function(fit, options) {

  # the between-study variance is provided by the random-effects model of netmeta, I^2 by netmeta (not netmetabin)
  random <- .nmaIsRandomFit(fit)

  return(c(
    tau  = random && options[["heterogeneityTau"]],
    tau2 = random && options[["heterogeneityTau2"]],
    I2   = !inherits(fit, "netmetabin") && options[["heterogeneityI2"]]
  ))
}

.nmaHeterogeneityParameters <- function(fits, options) {

  successful <- .nmaSuccessfulFits(fits)

  # without a successful fit (e.g., before the data are assigned), the options define the parameters
  if (length(successful) == 0) {
    random <- .nmaOptionsRandomEffects(options)
    return(c(
      tau  = random && options[["heterogeneityTau"]],
      tau2 = random && options[["heterogeneityTau2"]],
      I2   = .nmaPoolingMethod(options) == "inverseVariance" && options[["heterogeneityI2"]]
    ))
  }

  return(Reduce(`|`, lapply(successful, .nmaFitHeterogeneityParameters, options = options)))
}

.nmaHeterogeneityTable  <- function(jaspResults, options) {

  fits <- .nmaExtractFits(jaspResults, options)

  if (!any(.nmaHeterogeneityParameters(fits, options)))
    return()

  modelSummaryContainer <- .nmaExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["heterogeneityTable"]]))
    return()

  heterogeneityTable          <- createJaspTable(gettext("Heterogeneity"))
  heterogeneityTable$position <- 3
  heterogeneityTable$showSpecifiedColumnsOnly <- TRUE
  heterogeneityTable$dependOn(c("heterogeneityTau", "heterogeneityTau2", "heterogeneityI2", "confidenceIntervals",
                                "includeFullDatasetInSubgroupAnalysis"))
  modelSummaryContainer[["heterogeneityTable"]] <- heterogeneityTable

  heterogeneityTable$addColumnInfo(name = "par", type = "string", title = "")
  .maAddSubgroupColumn(heterogeneityTable, options)
  heterogeneityTable$addColumnInfo(name = "est", type = "number", title = gettext("Estimate"))
  .maAddCiColumn(heterogeneityTable, options)

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    heterogeneityTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  .nmaFailedFitFootnotes(heterogeneityTable, fits)

  # a fixed between-study variance is stored in the fit
  fixed <- vapply(.nmaSuccessfulFits(fits), function(fit) !is.null(fit[["tau.preset"]]), logical(1))
  if (any(fixed))
    heterogeneityTable$addFootnote(gettextf("The between-study variance \U1D70F\U00B2 was fixed to %1$s.", .nmaFixedTau2(options)))

  rows <- .maSafeRbind(lapply(fits, .nmaRowHeterogeneity, options = options))
  rows <- .maSafeOrderAndSimplify(rows, "par", options)

  if (!is.null(rows))
    heterogeneityTable$setData(rows)

  return()
}

.nmaRowHeterogeneity    <- function(fit, options) {

  labels <- c(tau = "\U1D70F", tau2 = "\U1D70F\U00B2", I2 = "I\U00B2")

  # failed fits are reported by a footnote
  if (jaspBase::isTryError(fit))
    return(NULL)

  parameters <- .nmaFitHeterogeneityParameters(fit, options)

  if (!any(parameters))
    return(NULL)

  # (I^2 is a percentage)
  rows <- data.frame(
    par      = unname(labels),
    est      = c(fit[["tau"]], fit[["tau2"]], 100 * fit[["I2"]]),
    lCi      = c(NA, NA, 100 * fit[["lower.I2"]]),
    uCi      = c(NA, NA, 100 * fit[["upper.I2"]]),
    subgroup = attr(fit, "subgroup")
  )

  return(rows[parameters, , drop = FALSE])
}

# Treatment effects ----

.nmaTreatmentEffectsTables <- function(jaspResults, options) {

  if (options[["treatmentEffects"]])
    .nmaTreatmentEffectsTable(jaspResults, options)

  if (options[["leagueTable"]])
    .nmaLeagueTable(jaspResults, options)

  if (.nmaIsComponentModel(options) && options[["componentEffects"]])
    .nmaComponentEffectsTable(jaspResults, options)

  return()
}

.nmaExtractTreatmentEffectsContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["treatmentEffectsContainer"]]))
    return(jaspResults[["treatmentEffectsContainer"]])

  treatmentEffectsContainer <- createJaspContainer(gettext("Network Treatment Effects"))
  treatmentEffectsContainer$dependOn(c(.nmaModelDependencies, "treatmentEffects", "leagueTable", "componentEffects"))
  treatmentEffectsContainer$position <- 2
  jaspResults[["treatmentEffectsContainer"]] <- treatmentEffectsContainer

  return(treatmentEffectsContainer)
}

.nmaTreatmentEffectsTable <- function(jaspResults, options) {

  treatmentEffectsContainer <- .nmaExtractTreatmentEffectsContainer(jaspResults)

  if (!is.null(treatmentEffectsContainer[["treatmentEffectsTable"]]))
    return()

  fits       <- .nmaExtractFits(jaspResults, options)
  successful <- .nmaSuccessfulFits(fits)

  # versus a single reference treatment, the title names the reference and the rows name the treatments
  references  <- unique(vapply(successful, function(fit) fit[["reference.group"]], character(1)))
  byReference <- options[["treatmentEffectsComparisons"]] == "versusReference" && length(references) == 1
  title       <- if (byReference) gettextf("Treatment Effects vs. %1$s", references) else gettext("Treatment Effects")

  treatmentEffectsTable          <- createJaspTable(title)
  treatmentEffectsTable$showSpecifiedColumnsOnly <- TRUE
  treatmentEffectsTable$position <- 1
  treatmentEffectsTable$dependOn(c("treatmentEffects", "treatmentEffectsComparisons", "confidenceIntervals", "predictionIntervals",
                                   "standardErrors", "transformEffectSize", "includeFullDatasetInSubgroupAnalysis"))
  treatmentEffectsContainer[["treatmentEffectsTable"]] <- treatmentEffectsTable

  # the columns follow the fitted models
  random        <- .nmaAnyFit(fits, options, .nmaIsRandomFit, .nmaOptionsRandomEffects)
  tTest         <- .nmaAnyFit(fits, options, .nmaIsTestFit, .nmaOptionsTTest)
  # (no prediction intervals are available if the network has fewer than two degrees of freedom for heterogeneity)
  # (the component model provides no prediction intervals)
  intervals     <- options[["predictionIntervals"]] && random && !.nmaIsComponentModel(options) &&
    (length(successful) == 0 || any(vapply(successful, .nmaHasPredictionIntervals, logical(1))))

  # the statistic of a row follows its fit: a subgroup without degrees of freedom for heterogeneity reports z-tests among t-tests
  mixedTests    <- tTest && length(successful) > 0 && !all(vapply(successful, .nmaIsTestFit, logical(1)))

  treatmentEffectsTable$addColumnInfo(name = "comparison", type = "string", title = if (byReference) gettext("Treatment") else gettext("Comparison"))
  .maAddSubgroupColumn(treatmentEffectsTable, options)
  treatmentEffectsTable$addColumnInfo(name = "est", type = "number", title = gettext("Estimate"))
  .maAddSeColumn(treatmentEffectsTable, options)
  .maAddCiColumn(treatmentEffectsTable, options)
  if (intervals)
    .maAddPiColumn(treatmentEffectsTable, options)
  treatmentEffectsTable$addColumnInfo(name = "stat", type = "number", title = if (mixedTests) gettext("Statistic") else if (tTest) "t" else "z")
  if (tTest)
    treatmentEffectsTable$addColumnInfo(name = "df", type = "integer", title = "df")
  treatmentEffectsTable$addColumnInfo(name = "pval", type = "pvalue", title = gettext("p"))

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    treatmentEffectsTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  effects <- .maSafeRbind(lapply(fits, .nmaRowsTreatmentEffects, options = options))
  anyNA   <- FALSE

  if (!is.null(effects)) {
    if (byReference)
      effects[["comparison"]] <- effects[["treatment"]]
    columns            <- intersect(c("est", "lCi", "uCi", "lPi", "uPi"), colnames(effects))
    effectsTransformed <- .nmaTransformEffects(effects, options)
    anyNA              <- sum(is.na(effectsTransformed[, columns])) > sum(is.na(effects[, columns]))
    effects            <- .maSafeOrderAndSimplify(effectsTransformed, "comparison", options)
  }

  # messages
  if (!is.null(effects))
    treatmentEffectsTable$addFootnote(.nmaEffectDirectionMessage(options, if (byReference) references))
  if (any(vapply(successful, .nmaIsComponentFit, logical(1)))) {
    .nmaComponentUnidentifiedFootnote(treatmentEffectsTable, fits)
    .nmaAddFitNote(
      treatmentEffectsTable, .nmaNotEstimableMessage(),
      successful[vapply(successful, .nmaHasNotEstimableContrasts, logical(1))], fits, options
    )
  }
  if (random && options[["fixedEffectTest"]] == "t" && any(vapply(successful, .nmaIsTestWithoutDf, logical(1)))) {
    withoutDf <- successful[vapply(successful, .nmaIsTestWithoutDf, logical(1))]
    treatmentEffectsTable$addFootnote(if (mixedTests && options[["subgroup"]] != "") {
      gettextf(
        "%1$s: the t-distribution is not available because the network has no degrees of freedom for heterogeneity; z-tests are reported (the statistic is z, df is empty).",
        paste(vapply(withoutDf, .nmaScopeQuoted, character(1)), collapse = ", ")
      )
    } else {
      gettext("The t-distribution is not available because the network has no degrees of freedom for heterogeneity; z-tests are reported.")
    })
  }
  if (random)
    treatmentEffectsTable$addFootnote(if (tTest) {
      gettext("Fixed effects tested using t-distribution.")
    } else {
      gettext("Fixed effects tested using z-distribution.")
    })
  if (intervals) {
    .nmaAddFitNote(
      treatmentEffectsTable, gettext("Prediction intervals are based on very few degrees of freedom and may be very wide."),
      successful[vapply(successful, .nmaHasWidePredictionIntervals, logical(1))], fits, options
    )
  }
  if (options[["predictionIntervals"]] && random)
    .nmaAddFitNote(
      treatmentEffectsTable,
      gettext("Prediction intervals are not available because the network has fewer than two degrees of freedom for heterogeneity (the degrees of freedom of the overall Q statistic)."),
      # (the component model provides no prediction intervals; the form disables them)
      successful[vapply(successful, function(fit) .nmaIsRandomFit(fit) && !.nmaIsComponentFit(fit) && !.nmaHasPredictionIntervals(fit), logical(1))], fits, options
    )
  for (message in .nmaTransformationMessage(options, anyNA))
    treatmentEffectsTable$addFootnote(message)

  # a reference treatment missing in the network (of a subgroup) is replaced by the netmeta default
  for (fit in successful) {
    if (options[["referenceTreatment"]] != "" && fit[["reference.group"]] != options[["referenceTreatment"]])
      treatmentEffectsTable$addFootnote(if (options[["subgroup"]] != "") {
        gettextf(
          "Reference treatment '%1$s' is not part of the analysed network (%2$s); '%3$s' is used as reference.",
          options[["referenceTreatment"]], .nmaScopeQuoted(fit), fit[["reference.group"]]
        )
      } else {
        gettextf(
          "Reference treatment '%1$s' is not part of the analysed network; '%2$s' is used as reference.",
          options[["referenceTreatment"]], fit[["reference.group"]]
        )
      })
  }
  .nmaFailedFitFootnotes(treatmentEffectsTable, fits)

  if (!is.null(effects))
    treatmentEffectsTable$setData(effects)

  return()
}

.nmaNotEstimableMessage <- function() {

  return(gettext("The effect of a comparison is not estimable (NA) if the treatments are in different subnetworks and their components do not identify the contrast."))
}

.nmaRowsTreatmentEffects <- function(fit, options) {

  # failed fits are reported by a footnote
  if (jaspBase::isTryError(fit))
    return(NULL)

  element    <- .nmaEffectsElement(fit)
  treatments <- fit[["trts"]]
  reference  <- fit[["reference.group"]]
  matrixOf   <- function(name) fit[[paste0(name, ".", element)]]

  if (options[["treatmentEffectsComparisons"]] == "versusReference") {
    pairs <- data.frame(first = setdiff(treatments, reference), second = reference)
  } else {
    pairs <- do.call(rbind, lapply(seq_along(treatments)[-length(treatments)], function(i) data.frame(first = treatments[i], second = treatments[-seq_len(i)])))
  }

  index <- cbind(match(pairs[["first"]], treatments), match(pairs[["second"]], treatments))

  rows <- data.frame(
    comparison = gettextf("%1$s vs. %2$s", pairs[["first"]], pairs[["second"]]),
    treatment  = pairs[["first"]],
    subgroup   = attr(fit, "subgroup"),
    est        = matrixOf("TE")[index],
    se         = matrixOf("seTE")[index],
    lCi        = matrixOf("lower")[index],
    uCi        = matrixOf("upper")[index],
    stat       = matrixOf("statistic")[index],
    # (the degrees of freedom belong to the t-tests only)
    df         = if (.nmaIsTestFit(fit)) fit[["df.Q"]] else NA_real_,
    pval       = matrixOf("pval")[index]
  )

  # prediction intervals of the random-effects model
  if (options[["predictionIntervals"]] && .nmaIsRandomFit(fit)) {
    rows[["lPi"]] <- fit[["lower.predict"]][index]
    rows[["uPi"]] <- fit[["upper.predict"]][index]
  }

  return(rows)
}

# League table ----

.nmaLeagueTable         <- function(jaspResults, options) {

  treatmentEffectsContainer <- .nmaExtractTreatmentEffectsContainer(jaspResults)

  if (!is.null(treatmentEffectsContainer[["leagueTable"]]))
    return()

  fits         <- .nmaExtractFits(jaspResults, options)
  dependencies <- c("leagueTable", "leagueTableOrder", "leagueTableDirectEstimates", "leagueTableConfidenceIntervals",
                    "beneficialEffect", "transformEffectSize", "includeFullDatasetInSubgroupAnalysis")

  if (is.null(fits) || options[["subgroup"]] == "") {

    leagueTable          <- createJaspTable(gettext("League Table"))
    leagueTable$position <- 2
    leagueTable$dependOn(dependencies)
    treatmentEffectsContainer[["leagueTable"]] <- leagueTable

    .nmaFillLeagueTable(leagueTable, fits[[1]], options)

  } else {

    leagueContainer          <- createJaspContainer(gettext("League Table"))
    leagueContainer$position <- 2
    leagueContainer$dependOn(dependencies)
    treatmentEffectsContainer[["leagueTable"]] <- leagueContainer

    for (i in seq_along(fits)) {
      leagueTable          <- createJaspTable(.nmaScopeTitle(fits[[i]]))
      leagueTable$position <- i
      leagueContainer[[names(fits)[i]]] <- leagueTable
      .nmaFillLeagueTable(leagueTable, fits[[i]], options)
    }
  }

  return()
}

# The treatments in the order of the P-score ranking (best first), or the failure (e.g., a fit with contrasts that are not estimable has no ranking)
.nmaRankedTreatments    <- function(fit, options) {

  ranking <- try({
    .nmaStopIfNotEstimable(fit)
    .nmaRunCall(.nmaCallNetrank(options), fit)
  }, silent = TRUE)

  if (jaspBase::isTryError(ranking))
    return(ranking)

  pScores <- ranking[[paste0("ranking.", .nmaEffectsElement(fit))]]

  return(names(sort(pScores[fit[["trts"]]], decreasing = TRUE)))
}

# The note of an order by the ranking that is not available (the outputs use the treatment order)
.nmaRankedOrderMessage  <- function(failure) {

  return(gettextf("The treatments could not be ordered by the ranking: %1$s", .maTryCleanErrorMessages(failure)))
}

.nmaLeagueTreatmentOrder <- function(fit, options) {

  # treatment order or the P-score ranking (best first)
  if (options[["leagueTableOrder"]] == "treatmentOrder")
    return(fit[["trts"]])

  return(.nmaRankedTreatments(fit, options))
}

.nmaLeagueCell          <- function(est, lCi, uCi, options) {

  if (is.na(est))
    return("")

  if (options[["leagueTableConfidenceIntervals"]] && !is.na(lCi) && !is.na(uCi))
    return(sprintf("%.2f [%.2f, %.2f]", est, lCi, uCi))

  return(sprintf("%.2f", est))
}

# the elements of the estimate, the lower and the upper limit of a network meta-analysis (the matrices TE.common, lower.common, ...)
.nmaEstimateNames       <- c(est = "TE", lCi = "lower", uCi = "upper")

# The matrices of the direct estimates (est, lCi, uCi) of the treatments. netmetabin() stores the limits of the common-effect model as the
# limits of its random-effects model (penalized logistic regression): they are computed from the estimates and their standard errors.
.nmaDirectEstimates     <- function(fit, element, treatments) {

  direct <- lapply(.nmaEstimateNames, function(name) fit[[paste0(name, ".direct.", element)]][treatments, treatments, drop = FALSE])

  if (inherits(fit, "netmetabin") && element == "random") {
    margin         <- stats::qnorm(1 - (1 - fit[["level.ma"]]) / 2) * fit[["seTE.direct.random"]][treatments, treatments, drop = FALSE]
    direct$lCi[]   <- direct$est - margin
    direct$uCi[]   <- direct$est + margin
  }

  return(direct)
}

.nmaFillLeagueTable     <- function(leagueTable, fit, options) {

  # the first column holds the treatments, the other columns are added for each treatment
  leagueTable$addColumnInfo(name = "treatment", type = "string", title = gettext("Treatment"))

  if (is.null(fit))
    return()

  if (jaspBase::isTryError(fit)) {
    leagueTable$setError(.maTryCleanErrorMessages(fit))
    return()
  }

  element    <- .nmaEffectsElement(fit)
  treatments <- .nmaLeagueTreatmentOrder(fit, options)

  if (jaspBase::isTryError(treatments)) {
    leagueTable$addFootnote(.nmaRankedOrderMessage(treatments), symbol = gettext("Warning:"))
    treatments <- fit[["trts"]]
  }

  # network estimates (lower triangle: column vs. row) and direct estimates (upper triangle: row vs. column) or the network estimates
  # (upper triangle: row vs. column; the component model has no direct estimates)
  direct         <- options[["leagueTableDirectEstimates"]] && !.nmaIsComponentFit(fit)
  network        <- lapply(.nmaEstimateNames, function(name) fit[[paste0(name, ".", element)]][treatments, treatments, drop = FALSE])
  upper          <- if (direct) .nmaDirectEstimates(fit, element, treatments) else network

  # transformation of the estimates and the confidence limits
  transformation <- .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]])
  network        <- lapply(network, function(x) { x[] <- transformation(x); x })
  upper          <- lapply(upper,   function(x) { x[] <- transformation(x); x })

  n     <- length(treatments)
  cells <- matrix("", nrow = n, ncol = n)
  for (row in seq_len(n)) {
    for (column in seq_len(n)) {
      if (row == column) {
        cells[row, column] <- treatments[row]
      } else if (row > column) {
        cells[row, column] <- .nmaLeagueCell(network$est[column, row], network$lCi[column, row], network$uCi[column, row], options)
      } else {
        cells[row, column] <- .nmaLeagueCell(upper$est[row, column], upper$lCi[row, column], upper$uCi[row, column], options)
      }
    }
  }

  data <- data.frame(treatment = treatments, stringsAsFactors = FALSE)
  for (i in seq_len(n)) {
    leagueTable$addColumnInfo(name = paste0("trt", i), type = "string", title = treatments[i])
    data[[paste0("trt", i)]] <- cells[, i]
  }
  leagueTable$setData(data)

  # footnotes
  leagueTable$addFootnote(if (direct) {
    gettext("Lower triangle: network estimates of the column treatment versus the row treatment. Upper triangle: direct estimates of the row treatment versus the column treatment.")
  } else {
    gettext("Lower triangle: network estimates of the column treatment versus the row treatment. Upper triangle: network estimates of the row treatment versus the column treatment.")
  })
  # (netleague() shows the confidence intervals of the direct estimates computed with the normal distribution)
  if (direct && options[["leagueTableConfidenceIntervals"]] && .nmaIsTestFit(fit))
    leagueTable$addFootnote(gettext("The confidence intervals of the direct estimates (upper triangle) are based on the normal distribution; the confidence intervals of the network estimates (lower triangle) are based on the t-distribution."))
  if (.nmaHasNotEstimableContrasts(fit))
    leagueTable$addFootnote(.nmaNotEstimableMessage())
  for (message in .nmaTransformationMessage(options, FALSE))
    leagueTable$addFootnote(message)

  return()
}
