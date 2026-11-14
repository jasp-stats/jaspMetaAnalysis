# Classical network meta-analysis evidence structure.
#
# Builds the direct evidence proportion, the contribution matrix, the study impact, and the pairwise
# meta-analyses of the direct comparisons from the netmeta fits.

# Entry ----

.nmaEvidenceOutputs     <- function(jaspResults, options) {

  # the form disables the contribution matrix for the count-based pooling methods ('enabled: !poolingMethod.countBased')
  if (.nmaIsCountBased(options))
    options[["contributionMatrix"]] <- FALSE

  if (options[["directEvidence"]])
    .nmaDirectEvidenceTable(jaspResults, options)

  if (options[["contributionMatrix"]])
    .nmaContributionMatrixTables(jaspResults, options)

  if (options[["studyImpact"]])
    .nmaStudyImpactTables(jaspResults, options)

  if (options[["pairwiseMetaAnalyses"]])
    .nmaPairwiseMetaAnalysesTable(jaspResults, options)

  return()
}

.nmaExtractEvidenceContainer <- function(jaspResults) {

  return(.nmaExtractOutputContainer(
    jaspResults, "evidenceContainer", gettext("Evidence Structure"), 8,
    c("directEvidence", "contributionMatrix", "studyImpact", "pairwiseMetaAnalyses")
  ))
}

# Direct evidence ----

.nmaDirectEvidenceTable <- function(jaspResults, options) {

  evidenceContainer <- .nmaExtractEvidenceContainer(jaspResults)

  if (!is.null(evidenceContainer[["directEvidenceTable"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  directEvidenceTable          <- createJaspTable(gettext("Direct Evidence"))
  directEvidenceTable$position <- 1
  directEvidenceTable$showSpecifiedColumnsOnly <- TRUE
  directEvidenceTable$dependOn("directEvidence")
  evidenceContainer[["directEvidenceTable"]] <- directEvidenceTable

  directEvidenceTable$addColumnInfo(name = "comparison", type = "string", title = gettext("Comparison"))
  .maAddSubgroupColumn(directEvidenceTable, options)
  directEvidenceTable$addColumnInfo(name = "proportion", type = "number", title = gettext("Direct evidence proportion"))
  # (the mean path length and the minimal parallelism that no fit provides are not shown: the notes say why)
  if (!.nmaIsCountBased(options)) {
    directEvidenceTable$addColumnInfo(name = "meanPath", type = "number", title = gettext("Mean path length"))
    if (length(.nmaSuccessfulFits(fits)) == 0 || !all(vapply(.nmaSuccessfulFits(fits), .nmaIsTwoTreatmentFit, logical(1))))
      directEvidenceTable$addColumnInfo(name = "minPar", type = "number", title = gettext("Minimal parallelism"))
  }

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    directEvidenceTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  # (the rows of each fit are built in the computation of the fit: a failing subgroup does not remove the rows of the others)
  evidence <- .nmaComputeResults(fits, function(fit) {
    .nmaRowsDirectEvidence(.nmaRunCall(.nmaCallNetmeasures(fit), fit), fit)
  })

  if (.nmaReportFailedResults(directEvidenceTable, evidence))
    return()

  rows <- .maSafeRbind(lapply(evidence, function(rows) if (!jaspBase::isTryError(rows)) rows))
  rows <- .maSafeOrderAndSimplify(rows, "comparison", options)

  # (netmeasures() provides the mean path length and the minimal parallelism only for inverse-variance fits: the count-based pooling methods return NA)
  if (any(vapply(.nmaSuccessfulFits(fits), inherits, logical(1), "netmetabin")))
    directEvidenceTable$addFootnote(gettext("The mean path length and the minimal parallelism are not available for the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods."))
  successful <- .nmaSuccessfulFits(fits)
  # (netmeasures() divides the variance of the network estimate of the non-central hypergeometric method by the variance of the Mantel-Haenszel
  # direct estimate)
  .nmaAddFitNote(
    directEvidenceTable,
    gettext("For the non-central hypergeometric method, the direct evidence proportion compares the variance of the network estimate with the variance of the Mantel-Haenszel direct estimate, so comparisons with only direct evidence can have proportions below 1."),
    successful[vapply(successful, function(fit) inherits(fit, "netmetabin") && identical(fit[["method"]], "NCH"), logical(1))], fits, options
  )
  # (netmeasures() provides no minimal parallelism for a network of two treatments)
  .nmaAddFitNote(
    directEvidenceTable,
    gettext("The minimal parallelism is not defined for a network of two treatments."),
    successful[vapply(successful, function(fit) length(fit[["trts"]]) == 2, logical(1))], fits, options
  )
  if (!is.null(rows))
    directEvidenceTable$setData(rows)

  return()
}

.nmaRowsDirectEvidence  <- function(measures, fit) {

  # (netmeasures() provides no minimal parallelism for a network of two treatments)
  return(data.frame(
    comparison = .nmaComparisonLabels(names(measures[["proportion"]]), fit[["trts"]], .nmaTreatmentSeparator(fit)),
    subgroup   = attr(fit, "subgroup"),
    proportion = unname(measures[["proportion"]]),
    meanPath   = unname(measures[["meanpath"]]),
    minPar     = if (is.null(measures[["minpar"]])) NA_real_ else unname(measures[["minpar"]])
  ))
}

# Contribution matrix ----

.nmaContributionMatrixState <- function(jaspResults, options) {

  fits <- .nmaExtractFits(jaspResults, options)

  if (is.null(fits))
    return(NULL)

  return(.nmaExtractState(jaspResults, "contributionMatrixState", "contributionMatrixMethod", function() {
    .nmaComputeResults(fits, function(fit) .nmaRunCall(.nmaCallNetcontrib(options), fit), label = gettext("Computing Contribution Matrix"))
  }))
}

.nmaContributionMatrixTables <- function(jaspResults, options) {

  evidenceContainer <- .nmaExtractEvidenceContainer(jaspResults)

  if (!is.null(evidenceContainer[["contributionMatrixTable"]]))
    return()

  fits          <- .nmaExtractFits(jaspResults, options)
  contributions <- .nmaContributionMatrixState(jaspResults, options)

  .nmaMatrixTables(
    evidenceContainer, "contributionMatrixTable", gettext("Contribution Matrix"), 2, c("contributionMatrix", "contributionMatrixMethod"),
    fits, contributions, options,
    matrixOf = function(contribution, fit) {
      # percentages: rows are the network estimates, columns the direct comparisons
      matrix <- 100 * contribution[[.nmaEffectsElement(fit)]]
      list(
        values = matrix,
        rows   = .nmaComparisonLabels(rownames(matrix), fit[["trts"]], .nmaTreatmentSeparator(fit)),
        columns = .nmaComparisonLabels(colnames(matrix), fit[["trts"]], .nmaTreatmentSeparator(fit))
      )
    },
    rowTitle = gettext("Network comparison"),
    footnote = gettext("Percentage contribution of each direct comparison (columns) to each network estimate (rows).")
  )

  return()
}

# Study impact ----

.nmaStudyImpactState    <- function(jaspResults, options) {

  fits <- .nmaExtractFits(jaspResults, options)

  if (is.null(fits))
    return(NULL)

  # (the computation refits the network without each study and can take minutes)
  return(.nmaExtractState(jaspResults, "studyImpactState", NULL, function() {
    .nmaComputeResults(fits, function(fit) {
      tryCatch(
        .nmaRunCall(.nmaCallNetimpact(), fit),
        error = function(e) {
          # removing a study can disconnect the network
          if (grepl("singular", conditionMessage(e), fixed = TRUE))
            stop(gettext("The study impact cannot be computed because removing a single study disconnects the network."), call. = FALSE)
          stop(e)
        }
      )
    }, label = gettext("Computing Study Impact"))
  }))
}

.nmaStudyImpactTables   <- function(jaspResults, options) {

  evidenceContainer <- .nmaExtractEvidenceContainer(jaspResults)

  if (!is.null(evidenceContainer[["studyImpactTable"]]))
    return()

  fits    <- .nmaExtractFits(jaspResults, options)
  impacts <- .nmaStudyImpactState(jaspResults, options)

  .nmaMatrixTables(
    evidenceContainer, "studyImpactTable", gettext("Study Impact"), 3, c("studyImpact", "treatmentEffects", "treatmentEffectsComparisons"),
    fits, impacts, options,
    matrixOf = function(impact, fit) {
      # percentages: rows are the studies, columns the network estimates (all comparisons if selected for the treatment effects, otherwise versus the reference treatment)
      matrix     <- 100 * impact[[paste0("impact.", .nmaEffectsElement(fit))]]
      separator  <- .nmaTreatmentSeparator(fit)
      reference  <- if (!(options[["treatmentEffects"]] && options[["treatmentEffectsComparisons"]] == "allPairwise")) fit[["reference.group"]]
      if (!is.null(reference)) {
        pairs  <- .nmaComparisonPairs(colnames(matrix), fit[["trts"]], separator)
        matrix <- matrix[, !is.na(pairs$first) & (pairs$first == reference | pairs$second == reference), drop = FALSE]
      }
      # the comparison is named by the treatment versus the reference (the same orientation as the treatment effects table)
      list(
        values  = matrix,
        rows    = rownames(matrix),
        columns = .nmaComparisonLabels(colnames(matrix), fit[["trts"]], separator, reference = reference)
      )
    },
    rowTitle = gettext("Study"),
    footnote = gettext("Percentage loss of precision of each network estimate (columns) when the study (rows) is removed.")
  )

  return()
}

# Matrix tables ----

# Tables with one column per comparison are specific to a network: one table per fit when subgroups are used.
.nmaMatrixTables        <- function(evidenceContainer, key, title, position, dependencies, fits, results, options, matrixOf, rowTitle, footnote) {

  if (is.null(fits) || options[["subgroup"]] == "") {

    matrixTable          <- createJaspTable(title)
    matrixTable$position <- position
    matrixTable$dependOn(dependencies)
    evidenceContainer[[key]] <- matrixTable

    .nmaFillMatrixTable(matrixTable, fits[[1]], results[[1]], matrixOf, rowTitle, footnote)

  } else {

    matrixContainer          <- createJaspContainer(title)
    matrixContainer$position <- position
    matrixContainer$dependOn(dependencies)
    evidenceContainer[[key]] <- matrixContainer

    for (i in seq_along(fits)) {
      matrixTable          <- createJaspTable(.nmaScopeTitle(fits[[i]]))
      matrixTable$position <- i
      matrixContainer[[names(fits)[i]]] <- matrixTable
      .nmaFillMatrixTable(matrixTable, fits[[i]], results[[i]], matrixOf, rowTitle, footnote)
    }
  }

  return()
}

.nmaFillMatrixTable     <- function(matrixTable, fit, result, matrixOf, rowTitle, footnote) {

  matrixTable$addColumnInfo(name = "row", type = "string", title = rowTitle)

  if (is.null(fit) || is.null(result))
    return()

  if (jaspBase::isTryError(fit) || jaspBase::isTryError(result)) {
    matrixTable$setError(.maTryCleanErrorMessages(if (jaspBase::isTryError(fit)) fit else result))
    return()
  }

  matrix <- try(matrixOf(result, fit), silent = TRUE)

  if (jaspBase::isTryError(matrix)) {
    matrixTable$setError(.maTryCleanErrorMessages(matrix))
    return()
  }

  data   <- data.frame(row = matrix[["rows"]])

  for (i in seq_along(matrix[["columns"]])) {
    matrixTable$addColumnInfo(name = paste0("comparison", i), type = "number", title = matrix[["columns"]][i], format = "dp:1")
    data[[paste0("comparison", i)]] <- matrix[["values"]][, i]
  }

  matrixTable$addFootnote(footnote)
  matrixTable$setData(data)

  return()
}

# Pairwise meta-analyses ----

.nmaPairwiseMetaAnalysesTable <- function(jaspResults, options) {

  evidenceContainer <- .nmaExtractEvidenceContainer(jaspResults)

  if (!is.null(evidenceContainer[["pairwiseMetaAnalysesTable"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  pairwiseTable          <- createJaspTable(gettext("Pairwise Meta-Analyses"))
  pairwiseTable$position <- 4
  pairwiseTable$showSpecifiedColumnsOnly <- TRUE
  pairwiseTable$dependOn(c("pairwiseMetaAnalyses", "confidenceIntervals", "standardErrors", "transformEffectSize"))
  evidenceContainer[["pairwiseMetaAnalysesTable"]] <- pairwiseTable

  # the columns follow the fitted models
  random <- .nmaAnyFit(fits, options, .nmaIsRandomModel, .nmaIsRandomEffects)

  pairwiseTable$addColumnInfo(name = "comparison", type = "string",  title = gettext("Comparison"))
  .maAddSubgroupColumn(pairwiseTable, options)
  pairwiseTable$addColumnInfo(name = "k",   type = "integer", title = "k")
  pairwiseTable$addColumnInfo(name = "est", type = "number",  title = gettext("Estimate"))
  .maAddSeColumn(pairwiseTable, options)
  .maAddCiColumn(pairwiseTable, options)
  pairwiseTable$addColumnInfo(name = "stat", type = "number", title = "z")
  pairwiseTable$addColumnInfo(name = "pval", type = "pvalue", title = gettext("p"))
  if (random) {
    # (penalized logistic regression provides no between-study variance: the note says why)
    if (.nmaPoolingMethod(options) != "penalizedLogisticRegression")
      pairwiseTable$addColumnInfo(name = "tau2", type = "number", title = "\U1D70F\U00B2")
    pairwiseTable$addColumnInfo(name = "I2",   type = "number", title = "I\U00B2 (%)", format = "dp:1")
  }

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    pairwiseTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  pairwise <- .nmaComputeResults(fits, function(fit) .nmaRunCall(.nmaCallNetpairwise(), fit))

  if (.nmaReportFailedResults(pairwiseTable, pairwise))
    return()

  rows <- try(.maSafeRbind(lapply(seq_along(pairwise), function(i) {
    if (jaspBase::isTryError(pairwise[[i]])) NULL else .nmaRowsPairwise(pairwise[[i]], fits[[i]])
  })), silent = TRUE)

  if (jaspBase::isTryError(rows)) {
    pairwiseTable$setError(.maTryCleanErrorMessages(rows))
    return()
  }

  anyNA <- FALSE
  if (!is.null(rows)) {
    columns     <- intersect(c("est", "lCi", "uCi"), colnames(rows))
    transformed <- .nmaTransformEffects(rows, options, columns)
    anyNA       <- sum(is.na(transformed[, columns])) > sum(is.na(rows[, columns]))
    rows        <- .maSafeOrderAndSimplify(transformed, "comparison", options)
  }

  if (random)
    .nmaPairwiseVarianceFootnotes(pairwiseTable, fits, options)
  # (netpairwise() computes the intervals and tests with the normal distribution, while the network meta-analysis uses the t-distribution)
  successful <- .nmaSuccessfulFits(fits)
  .nmaAddFitNote(
    pairwiseTable,
    gettext("The confidence intervals and tests of the pairwise meta-analyses use the normal distribution, also if the t-distribution is used for the treatment effects of the network."),
    successful[vapply(successful, .nmaIsTestFit, logical(1))], fits, options
  )
  for (message in .nmaTransformationMessage(options, anyNA))
    pairwiseTable$addFootnote(message)

  if (!is.null(rows))
    pairwiseTable$setData(rows)

  return()
}

# The between-study variance of the pairwise meta-analyses: each comparison has its own (netpairwise() estimates it by the estimator of the
# network meta-analysis, also if the network meta-analysis fixes the between-study variance); penalized logistic regression has none.
.nmaPairwiseVarianceFootnotes <- function(pairwiseTable, fits, options) {

  successful <- .nmaSuccessfulFits(fits)
  estimated  <- successful[vapply(successful, .nmaIsRandomFit, logical(1))]
  isFixed    <- vapply(estimated, function(fit) !is.null(fit[["tau.preset"]]), logical(1))
  dispersion <- successful[vapply(successful, function(fit) .nmaIsRandomModel(fit) && !.nmaIsRandomFit(fit), logical(1))]

  fixed <- estimated[isFixed]
  for (method in unique(vapply(fixed, function(fit) fit[["method.tau"]], character(1)))) {
    .nmaAddFitNote(
      pairwiseTable,
      gettextf(
        "The fixed between-study variance of the network meta-analysis is not applied: each pairwise meta-analysis estimates its own between-study variance by the %1$s estimator (a comparison with a single study has no between-study variance).",
        switch(method, REML = gettext("restricted maximum likelihood"), ML = gettext("maximum likelihood"), DL = gettext("DerSimonian-Laird"), method)
      ),
      fixed[vapply(fixed, function(fit) identical(fit[["method.tau"]], method), logical(1))], fits, options
    )
  }

  .nmaAddFitNote(
    pairwiseTable,
    gettext("Penalized logistic regression provides no between-study variance: \U1D70F\U00B2 is not available for the pairwise meta-analyses."),
    dispersion, fits, options
  )

  return()
}

.nmaRowsPairwise        <- function(pairwise, fit) {

  element <- .nmaEffectsElement(fit)
  random  <- isTRUE(fit[["random"]])

  # the comparisons are matched by their names (the results are not ordered as the network estimates)
  rows <- do.call(rbind, lapply(pairwise, function(meta) {

    data <- data.frame(
      comparison = gettextf("%1$s vs. %2$s", meta[["label.e"]], meta[["label.c"]]),
      subgroup   = attr(fit, "subgroup"),
      k          = meta[["k"]],
      est        = meta[[paste0("TE.", element)]],
      se         = meta[[paste0("seTE.", element)]],
      lCi        = meta[[paste0("lower.", element)]],
      uCi        = meta[[paste0("upper.", element)]],
      stat       = meta[[paste0("statistic.", element)]],
      pval       = meta[[paste0("pval.", element)]]
    )

    if (random) {
      data[["tau2"]] <- if (is.null(meta[["tau2"]])) NA_real_ else meta[["tau2"]]
      data[["I2"]]   <- if (is.null(meta[["I2"]])) NA_real_ else 100 * meta[["I2"]]
    }

    return(data)
  }))

  # the order of the treatments in the network
  index <- order(match(vapply(pairwise, function(meta) meta[["label.e"]], character(1)), fit[["trts"]]),
                 match(vapply(pairwise, function(meta) meta[["label.c"]], character(1)), fit[["trts"]]))

  return(rows[index, , drop = FALSE])
}
