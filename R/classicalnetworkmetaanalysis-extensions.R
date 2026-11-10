# Classical network meta-analysis extensions.
#
# Defines the calls of the netmeta functions that use a fitted network (call specifications: the outputs execute them, and the R code prints
# the calls of the component model and the network meta-regression), builds the network meta-regression and the test for subgroup
# differences, and fits the component model and its outputs.

# Entry ----

.nmaExtensionOutputs    <- function(jaspResults, options) {

  # (the network meta-regression belongs to the standard model)
  if (options[["predictor"]] != "" && !.nmaIsComponentModel(options))
    .nmaMetaregressionOutputs(jaspResults, options)

  if (options[["subgroup"]] != "")
    .nmaSubgroupDifferencesTable(jaspResults, options)

  return()
}

# Follow-up calls ----
#
# The calls of the netmeta functions that use the fitted network meta-analysis (available as 'fit'). Each call is defined
# once as a call specification (see classicalnetworkmetaanalysis-fit.R): the outputs execute it (the R code prints only the calls that fit
# the models: the component model and the network meta-regression, below).

.nmaRunCall             <- function(call, fit, objects = list(), dataset = NULL) {

  return(.nmaEvaluateCall(call, dataset, c(list(fit = fit), objects)))
}

.nmaCallNetrank         <- function(options) {

  return(list(
    fun       = "netmeta::netrank",
    arguments = list(.nmaObject("fit"), method = "P-score", small.values = .nmaSmallValues(options))
  ))
}

.nmaCallRankogram       <- function(options, fit) {

  return(list(
    fun       = "netmeta::rankogram",
    arguments = list(
      .nmaObject("fit"),
      nsim         = options[["rankingNumberOfSimulations"]],
      small.values = .nmaSmallValues(options),
      common       = fit[["common"]],
      random       = fit[["random"]]
    )
  ))
}

# (a fixed between-study variance defines the preset versions of the random-effects decomposition)
.nmaCallDecompDesign    <- function(fit) {

  return(list(
    fun       = "netmeta::decomp.design",
    arguments = c(list(.nmaObject("fit")), if (!is.null(fit[["tau.preset"]])) list(tau.preset = fit[["tau.preset"]]))
  ))
}

.nmaCallNetsplit        <- function(options, fit) {

  # (the Mantel-Haenszel and non-central hypergeometric fits provide SIDDE only; the form disables node splitting for penalized logistic regression)
  method <- if (inherits(fit, "netmetabin") || options[["nodeSplittingMethod"]] == "sidde") "SIDDE" else "Back-calculation"

  return(list(fun = "netmeta::netsplit", arguments = list(.nmaObject("fit"), method = method)))
}

# the measures of the model that was fitted
.nmaCallNetmeasures     <- function(fit) {

  return(list(fun = "netmeta::netmeasures", arguments = list(.nmaObject("fit"), random = isTRUE(fit[["random"]]))))
}

.nmaCallNetcontrib      <- function(options) {

  method <- c(shortestPath = "shortestpath", randomWalk = "randomwalk", pseudoinverse = "pseudoinverse")[[options[["contributionMatrixMethod"]]]]

  return(list(fun = "netmeta::netcontrib", arguments = list(.nmaObject("fit"), method = method)))
}

.nmaCallNetimpact       <- function() {

  return(list(fun = "netmeta::netimpact", arguments = list(.nmaObject("fit"))))
}

.nmaCallNetpairwise     <- function() {

  return(list(fun = "netmeta::netpairwise", arguments = list(.nmaObject("fit"), separate = TRUE)))
}

# the coordinates of the nodes and edges (the module draws the network plot itself)
.nmaCallNetgraph        <- function(options, fit) {

  # (netgraph() of a discomb fit, the component model of a disconnected network, sets the sequence of the treatments and the iteration
  # itself and passes only the other arguments on: the layout is the circle; a network of two treatments has no layout to optimize and the
  # stress majorization fails for it: its layout is the circle)
  layout <- if (inherits(fit, "discomb")) {
    list(start.layout = "circle")
  } else {
    switch(
      if (length(fit[["trts"]]) < 3) "circle" else options[["networkPlotLayout"]],
      circle  = list(start.layout = "circle", iterate = FALSE),
      # (the optimal sequence of the circle uses the defaults of netgraph(): the start layout of the search is the eigen layout)
      optimal = list(seq = "optimal"),
      stress  = list(start.layout = "eigen", iterate = TRUE)
    )
  }

  return(list(fun = "netmeta::netgraph", arguments = c(list(.nmaObject("fit"), figure = FALSE), layout)))
}

# the comparison-adjusted effects (the study effects centered at the pooled direct effect of their comparison)
.nmaCallFunnel          <- function(fit, order) {

  return(list(
    fun       = "meta::funnel",
    arguments = list(.nmaObject("fit"), order = order, pooled = if (isTRUE(fit[["random"]])) "random" else "common")
  ))
}

# (the funnel data are available as 'funnelData')
.nmaCallMetagenAdjusted <- function() {

  return(list(
    fun       = "meta::metagen",
    arguments = list(TE = .nmaExpression("funnelData$TE.adj"), seTE = .nmaExpression("funnelData$seTE"), method.tau = "DL", method.tau.ci = "")
  ))
}

# (the meta-analysis of the comparison-adjusted effects is available as 'adjusted')
.nmaCallMetabias        <- function(method) {

  return(list(fun = "meta::metabias", arguments = list(.nmaObject("adjusted"), method.bias = method)))
}

# Network meta-regression ----

.nmaMetaregressionOutputs <- function(jaspResults, options) {

  metaregressionContainer <- .nmaExtractOutputContainer(
    jaspResults, "metaregressionContainer", gettext("Network Meta-Regression"), 3, "metaregressionAssumption"
  )

  .nmaMetaregressionTestsTable(metaregressionContainer, jaspResults, options)
  .nmaMetaregressionCoefficientsTable(metaregressionContainer, jaspResults, options)

  return()
}

# netmetareg() supports the common-effect (FE), ML, and REML models; the DerSimonian-Laird estimator is replaced by REML
.nmaMetaregressionMethod <- function(options) {

  return(switch(
    options[["method"]],
    equalEffects      = "FE",
    maximumLikelihood = "ML",
    "REML"
  ))
}

# The labels of the levels of a factor predictor passed to netmetareg(): a fixed prefix and the index of the level ("level1", "level2"). The
# levels of the data are not passed because netmetareg() fails if a level has the name of a treatment (e.g., the levels "A" and "B" of a
# predictor and the treatments "A" and "B") and removes the columns of its model matrix by a regular expression (the columns whose names
# end with the label of the first level). The labels contain only letters and digits (no regular expression characters), and the prefix
# is extended until no treatment name (also in its syntactic form) ends with a label. The tables and notes show the levels of the data.
.nmaMetaregressionLevelLabels <- function(levels, treatments) {

  names  <- c(treatments, make.names(gsub(" ", "_", treatments)))
  prefix <- "level"

  repeat {
    labels <- paste0(prefix, seq_along(levels))
    if (!any(vapply(labels, function(label) any(endsWith(names, label)), logical(1))))
      return(labels)
    prefix <- paste0(prefix, "x")
  }
}

# The predictor of the studies of the analysed comparisons of the fitted network: the values aligned with the analysed rows of the fit (the
# predictor is constant within a study). A study that is not part of the network (e.g., a single-arm study, which pairwise() drops) and a study
# whose comparisons netmeta excluded (e.g., without events in both arms) do not contribute their values or levels.
.nmaMetaregressionPredictor <- function(dataset, fit, options) {

  studies   <- as.character(fit[["data"]][[".studlab"]])[.nmaAnalysedRows(fit)]
  predictor <- dataset[[options[["predictor"]]]][match(studies, as.character(dataset[[options[["studyId"]]]]))]

  return(if (is.factor(predictor)) droplevels(predictor) else predictor)
}

# The levels of a factor predictor among the analysed rows of the fit (their order defines the labels of .nmaMetaregressionLevelLabels()).
.nmaMetaregressionPredictorLevels <- function(dataset, fit, options) {

  return(levels(.nmaMetaregressionPredictor(dataset, fit, options)))
}

# The study-level covariate aligned with the rows of the network meta-analysis (the predictor is constant within a study).
# netmetareg() evaluates the covariate in the data of the fit and reads all its rows, also the comparisons that netmeta excluded: these rows
# are removed, so that the regression is the regression of the analysed comparisons. For a predictor with two levels, netmetareg() also reads the
# treatments of the comparisons (treat1, treat2) from these data (it fails without them: "undefined columns selected"; the data of a
# contrast-based fit contain only .treat1 and .treat2).
.nmaMetaregressionFitData <- function(dataset, fit, options, dataName) {

  code <- paste0(
    dataName, "$", .metaAsRName(options[["predictor"]]),
    "[match(fit$data$.studlab, ", dataName, "$", .metaAsRName(options[["studyId"]]), ")]"
  )

  if (!is.factor(dataset[[options[["predictor"]]]]))
    return(list(covariate = code))

  # the levels of the predictor among the rows of the fit are renamed
  labels <- .nmaMetaregressionLevelLabels(.nmaMetaregressionPredictorLevels(dataset, fit, options), fit[["trts"]])

  return(list(
    covariateLevels = paste0("droplevels(", code, ")"),
    covariate       = paste0("factor(fit$data$covariateLevels, labels = ", .nmaCodeValue(labels, NULL), ")"),
    treat1          = "fit$data$.treat1",
    treat2          = "fit$data$.treat2"
  ))
}

.nmaCallMetaregression  <- function(dataset, fit, options, dataName = "dataset") {

  return(list(
    fun       = "netmeta::netmetareg",
    fitRows   = if (!is.null(fit[["data"]][[".excl"]])) "!fit$data$.excl",
    fitData   = .nmaMetaregressionFitData(dataset, fit, options, dataName),
    arguments = list(
      .nmaObject("fit"),
      covar           = .nmaSymbol("covariate"),
      assumption      = options[["metaregressionAssumption"]],
      method.tau      = .nmaMetaregressionMethod(options),
      reference.group = fit[["reference.group"]]
    )
  ))
}

# Why the meta-regression is not available (a netmetabin fit, a predictor with more than two levels or without variation).
.nmaMetaregressionProblem <- function(fit, options) {

  if (inherits(fit, "netmetabin"))
    return(gettext("Not available for the selected pooling method."))

  predictor <- .nmaMetaregressionPredictor(attr(fit, "dataset"), fit, options)

  if (!is.numeric(predictor) && !(is.factor(predictor) && nlevels(predictor) <= 2))
    return(gettext("Network meta-regression supports a continuous predictor or a predictor with two levels."))

  if (length(unique(predictor)) < 2)
    return(gettext("The predictor has no variation among the studies."))

  return(NULL)
}

# Whether netmetareg() treats the comparisons of a multi-arm study as independent. netmeta 3.7-0 (netmetareg.netmeta) uses the covariance of the
# comparisons of a study (shared reference arm) only if the data of the fit contain the sample sizes of the arms and the events, times, or standard
# deviations, `if (available.n & (available.events | available.times | available.sds)) V <- bldiag(lapply(split(dat, dat$studlab), calcV, sm = sm))
# else V <- dat$seTE^2`, and calcV() has a formula for the summary measure (OR, RR, RD, ASD, MD, SMD, IRR, IRD, IRSD; otherwise, e.g., for ROM, it
# returns diag(seTE^2)). A multi-arm study has more than two treatments (netmeta stops unless a study has all its pairwise comparisons).
.nmaMetaregressionIndependentComparisons <- function(fit) {

  data      <- fit[["data"]]
  available <- function(...) all(vapply(c(...), function(column) !is.null(data[[column]]), logical(1)))

  if (available(".n1", ".n2") && (available(".event1", ".event2") || available(".time1", ".time2") || available(".sd1", ".sd2")) &&
      fit[["sm"]] %in% c("OR", "RR", "RD", "ASD", "MD", "SMD", "IRR", "IRD", "IRSD"))
    return(FALSE)

  rows       <- .nmaAnalysedRows(fit)
  treatments <- split(
    c(as.character(data[[".treat1"]][rows]), as.character(data[[".treat2"]][rows])),
    rep(as.character(data[[".studlab"]][rows]), 2)
  )

  return(any(vapply(treatments, function(x) length(unique(x)) > 2, logical(1))))
}

# The note of the meta-regression tables if the comparisons of multi-arm studies are treated as independent in some fits (the note names the
# fits if it applies only to some of them).
.nmaMetaregressionIndependentComparisonsNote <- function(results, options) {

  independent <- vapply(results, function(result) result[["independentComparisons"]], logical(1))

  if (!any(independent))
    return(NULL)

  note <- gettext("For this input, netmeta treats the comparisons of a multi-arm study as independent in the meta-regression, so the standard errors may be understated.")

  if (all(independent) || options[["subgroup"]] == "")
    return(note)

  return(gettextf("%1$s: %2$s", paste(vapply(results[independent], .nmaScopeQuoted, character(1)), collapse = ", "), note))
}

# The meta-regression of one fit: the extracted results (the netmetareg object contains the whole network).
.nmaMetaregressionFit   <- function(fit, options) {

  problem <- .nmaMetaregressionProblem(fit, options)

  if (!is.null(problem))
    stop(problem, call. = FALSE)

  dataset   <- attr(fit, "dataset")
  predictor <- .nmaMetaregressionPredictor(dataset, fit, options)

  model <- tryCatch(
    suppressWarnings(.nmaRunCall(.nmaCallMetaregression(dataset, fit, options), fit, dataset = dataset)),
    error = function(e) {
      # every comparison with the reference treatment has the same value of the predictor
      if (grepl("Insufficient variation", conditionMessage(e), fixed = TRUE))
        stop(gettextf(
          "The predictor does not vary among the studies that include the reference treatment '%1$s'. Please, select another reference treatment.",
          fit[["reference.group"]]
        ), call. = FALSE)
      stop(e)
    }
  )

  coefficients <- model[["results"]]
  slopes       <- which(coefficients[["type"]] == "beta")

  # the Wald test of the slopes (the omnibus test of the model includes the treatment effects)
  moderation <- if (length(slopes) > 0) {
    test <- stats::anova(model, btt = slopes)
    c(stat = test[["QM"]], df = test[["QMdf"]][1], pval = test[["QMp"]])
  }

  # the treatment names of the coefficients are made syntactic by netmeta
  treatments <- fit[["trts"]]
  treatment  <- treatments[match(as.character(coefficients[["treat"]]), make.names(gsub(" ", "_", treatments)))]
  treatment  <- ifelse(is.na(treatment), as.character(coefficients[["treat"]]), treatment)

  return(list(
    coefficients = data.frame(
      type      = coefficients[["type"]],
      treatment = treatment,
      est       = coefficients[["coef"]],
      se        = coefficients[["se"]],
      lCi       = coefficients[["lower"]],
      uCi       = coefficients[["upper"]],
      stat      = coefficients[["z"]],
      pval      = coefficients[["pval"]]
    ),
    moderation = moderation,
    residual   = c(stat = model[["QE"]], df = model[["QEdf"]], pval = model[["QEp"]]),
    reference  = fit[["reference.group"]],
    levels     = if (is.factor(predictor)) levels(predictor),
    independentComparisons = .nmaMetaregressionIndependentComparisons(fit)
  ))
}

.nmaMetaregressionState <- function(jaspResults, options) {

  fits <- .nmaExtractFits(jaspResults, options)

  if (is.null(fits))
    return(NULL)

  return(.nmaExtractState(jaspResults, "metaregressionState", "metaregressionAssumption", function() {
    .nmaComputeResults(fits, function(fit) .nmaMetaregressionFit(fit, options))
  }))
}

# Notes about the differences between the meta-regression and the network meta-analysis (same for both tables).
.nmaMetaregressionMessages <- function(options) {

  messages <- NULL

  if (.nmaOptionsRandomEffects(options)) {
    if (options[["method"]] == "derSimonianLaird")
      messages <- c(messages, gettext("The network meta-regression does not support the DerSimonian-Laird estimator: the restricted maximum likelihood estimator is used."))
    if (options[["fixedEffectTest"]] == "t")
      messages <- c(messages, gettext("The network meta-regression uses z-tests (the t-distribution is not available)."))
    if (options[["fixParametersTau2"]])
      messages <- c(messages, gettext("The fixed value of the between-study variance is not applied in the network meta-regression: the between-study variance is estimated."))
  }

  return(messages)
}

.nmaMetaregressionTestsTable <- function(metaregressionContainer, jaspResults, options) {

  if (!is.null(metaregressionContainer[["metaregressionTestsTable"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  testsTable          <- createJaspTable(gettext("Network Meta-Regression Tests"))
  testsTable$position <- 1
  testsTable$showSpecifiedColumnsOnly <- TRUE
  metaregressionContainer[["metaregressionTestsTable"]] <- testsTable

  testsTable$addColumnInfo(name = "test", type = "string", title = "")
  .maAddSubgroupColumn(testsTable, options)
  testsTable$addColumnInfo(name = "stat", type = "string", title = gettext("Test"))
  testsTable$addColumnInfo(name = "pval", type = "pvalue", title = gettext("p"))

  if (is.null(fits))
    return()

  results <- .nmaMetaregressionState(jaspResults, options)

  if (.nmaReportFailedResults(testsTable, results))
    return()

  rows <- .maSafeRbind(lapply(.nmaSuccessfulResults(results), function(result) {
    .maSafeRbind(list(
      .nmaTestRow(gettext("Moderation"),            result[["moderation"]][["stat"]], result[["moderation"]][["df"]], result[["moderation"]][["pval"]], attr(result, "subgroup")),
      .nmaTestRow(gettext("Residual heterogeneity"), result[["residual"]][["stat"]],   result[["residual"]][["df"]],   result[["residual"]][["pval"]],   attr(result, "subgroup"))
    ))
  }))
  rows <- .maSafeOrderAndSimplify(rows, "test", options)

  for (message in .nmaMetaregressionMessages(options))
    testsTable$addFootnote(message)
  for (message in .nmaMetaregressionIndependentComparisonsNote(.nmaSuccessfulResults(results), options))
    testsTable$addFootnote(message)

  if (!is.null(rows))
    testsTable$setData(rows)

  return()
}

# The value of the predictor at which the treatment effects of a fit are estimated: the reference level (the first level) of a predictor with
# two levels, otherwise zero. The levels belong to the fit (a subgroup can contain other levels than the other subgroups): the notes of
# fits with a different note name their subgroups.
.nmaMetaregressionConditioningNotes <- function(results, covariate, options) {

  notes <- unname(vapply(results, function(result) {
    levels <- result[["levels"]]
    if (length(levels) == 2) {
      gettextf(
        "The treatment effects are estimated at %1$s = %2$s (the reference level of the predictor); the slopes are the differences in the treatment effects between %1$s = %3$s and %1$s = %2$s.",
        covariate, levels[1], levels[2]
      )
    } else {
      gettextf("The treatment effects are estimated at %1$s = 0.", covariate)
    }
  }, character(1)))

  if (length(unique(notes)) <= 1 || options[["subgroup"]] == "")
    return(unique(notes))

  return(vapply(unique(notes), function(note) {
    gettextf("%1$s: %2$s", paste(vapply(results[notes == note], .nmaScopeQuoted, character(1)), collapse = ", "), note)
  }, character(1), USE.NAMES = FALSE))
}

.nmaMetaregressionCoefficientsTable <- function(metaregressionContainer, jaspResults, options) {

  if (!is.null(metaregressionContainer[["metaregressionCoefficientsTable"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  coefficientsTable          <- createJaspTable(gettext("Network Meta-Regression Coefficients"))
  coefficientsTable$position <- 2
  coefficientsTable$showSpecifiedColumnsOnly <- TRUE
  coefficientsTable$dependOn(c("confidenceIntervals", "standardErrors", "transformEffectSize"))
  metaregressionContainer[["metaregressionCoefficientsTable"]] <- coefficientsTable

  coefficientsTable$addColumnInfo(name = "term", type = "string", title = "")
  .maAddSubgroupColumn(coefficientsTable, options)
  coefficientsTable$addColumnInfo(name = "est", type = "number", title = gettext("Estimate"))
  # (the slopes are not transformed: under a transformation their standard errors are shown only if the 'Standard errors' option is checked
  # (the option is disabled then, so it keeps the value it had when the transformation was selected))
  .maAddSeColumn(coefficientsTable, options, noTransformation = TRUE)
  .maAddCiColumn(coefficientsTable, options)
  coefficientsTable$addColumnInfo(name = "stat", type = "number", title = "z")
  coefficientsTable$addColumnInfo(name = "pval", type = "pvalue", title = gettext("p"))

  if (is.null(fits))
    return()

  results <- .nmaMetaregressionState(jaspResults, options)

  if (.nmaReportFailedResults(coefficientsTable, results))
    return()

  covariate <- .nmaVariableName(options[["predictor"]])
  rows      <- .maSafeRbind(lapply(.nmaSuccessfulResults(results), .nmaRowsMetaregression, covariate = covariate, options = options))

  # the treatment effects are transformed (without standard errors), the slopes are not
  anyNA <- FALSE
  if (!is.null(rows) && options[["transformEffectSize"]] != "none") {
    effects                <- rows[["type"]] == "d"
    transformed            <- .nmaTransformEffects(rows[effects, , drop = FALSE], options, c("est", "lCi", "uCi"))
    columns                <- c("est", "lCi", "uCi")
    anyNA                  <- sum(is.na(transformed[, columns])) > sum(is.na(rows[effects, columns]))
    rows[effects, ]        <- transformed
    rows[effects, "se"]    <- NA_real_
  }
  rows <- .maSafeOrderAndSimplify(rows, "term", options)

  for (note in .nmaMetaregressionConditioningNotes(.nmaSuccessfulResults(results), covariate, options))
    coefficientsTable$addFootnote(note)
  for (message in .nmaMetaregressionMessages(options))
    coefficientsTable$addFootnote(message)
  for (message in .nmaMetaregressionIndependentComparisonsNote(.nmaSuccessfulResults(results), options))
    coefficientsTable$addFootnote(message)
  for (message in .nmaTransformationMessage(options, anyNA))
    coefficientsTable$addFootnote(message)
  # (the standard errors of the slopes follow the 'Standard errors' option, which the transformation of the effect sizes does not change)
  if (options[["transformEffectSize"]] != "none") {
    if (options[["standardErrors"]])
      coefficientsTable$addFootnote(gettext("The slopes and their standard errors are not transformed (they are shown on the scale of the analysis). No standard errors are shown for the transformed treatment effects."))
    else
      coefficientsTable$addFootnote(gettext("The slopes are not transformed (they are shown on the scale of the analysis)."))
  }
  .nmaFailedResultFootnotes(coefficientsTable, results)

  if (!is.null(rows))
    coefficientsTable$setData(rows)

  return()
}

.nmaRowsMetaregression  <- function(result, covariate, options) {

  coefficients <- result[["coefficients"]]
  reference    <- result[["reference"]]

  # the slopes of a predictor with two levels are the differences of the second versus the first level
  predictor <- if (length(result[["levels"]]) == 2) gettextf("%1$s (%2$s)", covariate, result[["levels"]][2]) else covariate
  term      <- ifelse(
    coefficients[["type"]] == "d",
    gettextf("%1$s vs. %2$s", coefficients[["treatment"]], reference),
    if (options[["metaregressionAssumption"]] == "independent") {
      gettextf("%1$s vs. %2$s \U00D7 %3$s", coefficients[["treatment"]], reference, predictor)
    } else {
      predictor
    }
  )

  return(data.frame(
    term     = term,
    subgroup = attr(result, "subgroup"),
    coefficients[, c("type", "est", "se", "lCi", "uCi", "stat", "pval")]
  ))
}

# Subgroup differences ----

# Cochran's Q test of the subgroup-specific network estimates of each comparison: the subgroup fits are independent
# (inverse-variance weights 1/se^2, df = number of subgroups - 1). The estimates are those of the fits (common or random effects).
.nmaSubgroupDifferenceRows <- function(fits, treatments) {

  rows <- list()

  for (i in seq_len(length(treatments) - 1)) {
    for (j in (i + 1):length(treatments)) {

      pair      <- treatments[c(i, j)]
      estimates <- vapply(fits, function(fit) {
        element <- .nmaEffectsElement(fit)
        if (!all(pair %in% fit[["trts"]]))
          return(c(NA_real_, NA_real_))
        return(c(fit[[paste0("TE.", element)]][pair[1], pair[2]], fit[[paste0("seTE.", element)]][pair[1], pair[2]]))
      }, numeric(2))

      estimable <- is.finite(estimates[1, ]) & is.finite(estimates[2, ]) & estimates[2, ] > 0

      if (sum(estimable) < 2)
        next

      weights <- 1 / estimates[2, estimable]^2
      pooled  <- sum(weights * estimates[1, estimable]) / sum(weights)
      q       <- sum(weights * (estimates[1, estimable] - pooled)^2)
      df      <- sum(estimable) - 1

      rows[[length(rows) + 1]] <- data.frame(
        comparison = gettextf("%1$s vs. %2$s", pair[1], pair[2]),
        Q          = q,
        df         = df,
        pval       = stats::pchisq(q, df, lower.tail = FALSE)
      )
    }
  }

  return(.maSafeRbind(rows))
}

# the treatment order of the full dataset (the other treatments, i.e., those absent from the full dataset network, follow)
.nmaSubgroupDifferenceTreatments <- function(scopes, fits) {

  all    <- sort(unique(unlist(lapply(fits, `[[`, "trts"))))
  full   <- scopes[["__fullDataset"]][["fit"]]
  first  <- if (!is.null(full) && !jaspBase::isTryError(full)) intersect(full[["trts"]], all)

  return(c(first, setdiff(all, first)))
}

.nmaSubgroupDifferencesTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["subgroupDifferencesTable"]]))
    return()

  differencesTable          <- createJaspTable(gettext("Test for Subgroup Differences"))
  differencesTable$position <- 4
  differencesTable$showSpecifiedColumnsOnly <- TRUE
  differencesTable$dependOn(c(.nmaModelDependencies, "transformEffectSize"))
  jaspResults[["subgroupDifferencesTable"]] <- differencesTable

  differencesTable$addColumnInfo(name = "comparison", type = "string",  title = gettext("Comparison"))
  differencesTable$addColumnInfo(name = "Q",          type = "number",  title = "Q")
  differencesTable$addColumnInfo(name = "df",         type = "integer", title = "df")
  differencesTable$addColumnInfo(name = "pval",       type = "pvalue",  title = gettext("p"))

  # the full dataset is not a subgroup
  scopes <- jaspResults[["nmaFit"]]$object

  if (is.null(scopes))
    return()

  fits       <- lapply(scopes[names(scopes) != "__fullDataset"], `[[`, "fit")
  successful <- .nmaSuccessfulFits(fits)

  if (length(successful) < 2) {
    differencesTable$setError(paste(c(
      gettext("The test for subgroup differences requires network estimates from at least two subgroups."),
      .nmaFailureMessage(fits[vapply(fits, jaspBase::isTryError, logical(1))])
    ), collapse = "\n"))
    return()
  }

  rows <- .nmaSubgroupDifferenceRows(successful, .nmaSubgroupDifferenceTreatments(scopes, successful))

  if (is.null(rows)) {
    differencesTable$setError(gettext("No treatment comparison is estimated in at least two subgroups."))
    return()
  }

  if (options[["transformEffectSize"]] != "none")
    differencesTable$addFootnote(gettext("The test is based on the estimates on the scale of the analysis (not transformed)."))
  .nmaFailedFitFootnotes(differencesTable, fits)

  differencesTable$setData(rows)

  return()
}

# Component network meta-analysis ----
#
# The component model (additive component network meta-analysis) replaces the standard model as the fit of the outputs: netcomb() extends
# the standard network meta-analysis of a connected network, discomb() fits a disconnected network (see .nmaFitScope()).

# the components are defined by the treatment names (arm-based data may use indicator variables)
.nmaComponentMode       <- function(options) {

  return(if (.nmaIsArmBased(options) && options[["componentSpecification"]] == "indicatorVariables") "indicatorVariables" else "separator")
}

.nmaComponentTreatments <- function(scope, options) {

  # a disconnected network is analyzed with the treatments of the retained comparisons
  standard <- .nmaStandardFit(scope)

  if (!jaspBase::isTryError(standard))
    return(standard[["trts"]])

  return(sort(.nmaRetainedTreatments(scope[["data"]], options, scope[["pairwise"]])))
}

# The C matrix: treatments (rows) by components (columns), 1 if the treatment contains the component. Interactions are products of
# the (active) components of the specified treatments. Components are the parts of the treatment names or the indicator variables.
.nmaComponentMatrix     <- function(treatments, dataset, options) {

  dropped  <- character(0)
  inactive <- NULL

  if (.nmaComponentMode(options) == "separator") {

    separator <- options[["componentSeparator"]]
    selected  <- options[["componentInactiveTreatment"]]

    if (nchar(separator) != 1)
      stop(gettext("The component separator must be a single character."), call. = FALSE)

    parts <- lapply(strsplit(treatments, separator, fixed = TRUE), trimws)
    names(parts) <- treatments

    # the inactive treatment is a treatment without active components (if present in the network)
    if (selected != "" && selected %in% treatments && length(parts[[selected]]) != 1)
      stop(gettextf("The inactive treatment '%1$s' must consist of a single component.", selected), call. = FALSE)

    # the component of the inactive treatment is not a component of the model also in a scope (e.g., a subgroup) without the treatment
    # itself, in which it is a part of the treatments
    components <- setdiff(sort(unique(unlist(parts))), selected)
    inactive   <- if (selected != "" && selected %in% unlist(parts)) selected

    matrix <- t(vapply(parts, function(part) as.numeric(components %in% part), numeric(length(components))))
    matrix <- matrix(matrix, nrow = length(treatments), ncol = length(components), dimnames = list(treatments, components))

  } else {

    variables <- options[["componentVariables"]]

    if (length(variables) == 0)
      stop(gettext("Please, assign the component indicator variables."), call. = FALSE)

    treatmentValues <- as.character(dataset[[options[["treatment"]]]])
    matrix          <- vapply(variables, function(variable) {

      values  <- dataset[[variable]]
      name    <- .nmaVariableName(variable)

      if (any(!values %in% c(0, 1)))
        stop(gettextf("The component indicator '%1$s' must contain only the values 0 and 1.", name), call. = FALSE)

      return(vapply(treatments, function(treatment) {
        indicator <- unique(values[treatmentValues == treatment])
        if (length(indicator) > 1)
          stop(gettextf("The component indicator '%1$s' is not constant within the treatment '%2$s'.", name, treatment), call. = FALSE)
        return(as.numeric(indicator))
      }, numeric(1)))
    }, numeric(length(treatments)))

    matrix <- matrix(matrix, nrow = length(treatments), ncol = length(variables), dimnames = list(treatments, vapply(variables, .nmaVariableName, character(1))))

    # the indicators that are 0 for all treatments of the scope (e.g., of a subgroup) are no components of the scope, as the components of the
    # split names are the parts of the treatments of the scope (the note names them)
    empty   <- colSums(matrix) == 0
    dropped <- colnames(matrix)[empty]
    matrix  <- matrix[, !empty, drop = FALSE]

    if (ncol(matrix) == 0)
      stop(gettext("The component indicators are 0 for all treatments."), call. = FALSE)

    # the treatment with all indicators equal to zero is the inactive treatment (none if there is no such treatment)
    inactive <- if (any(rowSums(matrix) == 0)) treatments[rowSums(matrix) == 0]
  }

  # Interactions between the components of the checked treatment combinations (each combination once; with indicators, an interaction is an
  # additional indicator variable: the list applies to split treatment names only). The form lists the treatments of the data: an interaction is
  # defined by the components of its combination and applies to a scope (e.g., a subgroup) if a treatment of the scope contains all of them,
  # also if the combination itself is not a treatment of the scope (its column is the product of the indicators of the components).
  interactions <- if (.nmaComponentMode(options) == "separator") .nmaComponentInteractions(options)

  added <- list()

  for (interaction in interactions) {

    # (the active components of the combination in the order of its name)
    interactionComponents <- setdiff(trimws(strsplit(interaction, options[["componentSeparator"]], fixed = TRUE)[[1]]), options[["componentInactiveTreatment"]])

    if (length(interactionComponents) < 2)
      stop(gettextf("The interaction combination '%1$s' must consist of at least two active components.", interaction), call. = FALSE)

    if (!all(interactionComponents %in% colnames(matrix)) || !any(rowSums(matrix[, interactionComponents, drop = FALSE]) == length(interactionComponents)))
      next

    # combinations of the same components (e.g., 'A+B' and 'B+A') are one interaction (the sets of components are compared: the labels of different
    # sets can be equal, e.g., 'A+B * C' and 'A * B+C' with the separator '+')
    sorted <- sort(interactionComponents)

    if (any(vapply(added, identical, logical(1), sorted)))
      next

    added[[length(added) + 1]] <- sorted
    matrix <- cbind(matrix, apply(matrix[, interactionComponents, drop = FALSE], 1, prod))
    colnames(matrix)[ncol(matrix)] <- paste(interactionComponents, collapse = " * ")
  }

  # (the labels of different interactions, and of an interaction and a component, can be equal)
  colnames(matrix) <- make.unique(colnames(matrix))

  return(list(matrix = matrix, inactive = inactive, dropped = dropped))
}

# the checked treatment combinations of the interactions (one row per combination listed by the form)
.nmaComponentInteractions <- function(options) {

  checked <- Filter(function(row) row[["include"]], options[["componentInteractions"]])

  # (the form stores the value of a row as a term: a list of its parts, joined by ':')
  return(vapply(checked, function(row) paste(unlist(row[["value"]]), collapse = ":"), character(1)))
}

.nmaComponentModelArguments <- function(options) {

  random    <- .nmaIsRandomEffects(options)
  arguments <- list(common = !random, random = random)

  # the heterogeneity is estimated by the method of moments of the component model unless it is fixed
  if (random && options[["fixParametersTau2"]])
    arguments[["tau.preset"]] <- sqrt(.nmaFixedTau2(options))

  # the components of the treatment names are split by the separator (the separator of the interactions is changed if needed)
  if (.nmaComponentMode(options) == "separator") {
    arguments[["sep.comps"]] <- options[["componentSeparator"]]
    if (options[["componentSeparator"]] == "*")
      arguments[["sep.ia"]] <- ":"
  }

  return(arguments)
}

# Whether the network of the scope is disconnected (the component model can still be fitted).
.nmaIsDisconnectedScope <- function(scope) {

  return(
    jaspBase::isTryError(.nmaStandardFit(scope)) && !is.null(scope[["connection"]]) &&
    !jaspBase::isTryError(scope[["connection"]]) && scope[["connection"]][["n.subnets"]] > 1
  )
}

# The reference treatment of the component model: the reference treatment of the standard model, or for a disconnected network the
# selected reference treatment, the inactive treatment, or the first treatment.
.nmaComponentReference  <- function(scope, treatments, inactive, options) {

  standard <- .nmaStandardFit(scope)

  if (!jaspBase::isTryError(standard))
    return(standard[["reference.group"]])

  if (options[["referenceTreatment"]] %in% treatments)
    return(options[["referenceTreatment"]])

  # (the inactive treatment can be a component of the treatments without being a treatment of the network)
  if (any(inactive %in% treatments))
    return(inactive[inactive %in% treatments][1])

  return(treatments[1])
}

# netcomb() of the standard model ('nma') for a connected network and discomb() for a disconnected network (the C matrix is
# available as 'C', the contrasts of arm-based data as 'pw')
.nmaCallComponentModel  <- function(scope, options, reference) {

  arguments <- .nmaComponentModelArguments(options)

  if (!.nmaIsDisconnectedScope(scope))
    return(list(fun = "netmeta::netcomb", arguments = c(list(.nmaObject("nma"), C.matrix = .nmaObject("C")), arguments)))

  arguments <- c(
    arguments,
    list(
      level.ma        = options[["confidenceIntervalsLevel"]],
      tol.multiarm    = options[["advancedMultiArmTolerance"]],
      reference.group = reference,
      backtransf      = FALSE
    )
  )

  if (.nmaIsArmBased(options))
    return(list(fun = "netmeta::discomb", arguments = c(list(.nmaObject("pw"), C.matrix = .nmaObject("C")), arguments)))

  return(list(
    fun       = "netmeta::discomb",
    arguments = c(
      list(
        TE       = .nmaColumn(options[["effectSize"]]),
        seTE     = .nmaColumn(options[["effectSizeStandardError"]]),
        treat1   = .nmaColumn(options[["treatment1"]]),
        treat2   = .nmaColumn(options[["treatment2"]]),
        studlab  = .nmaColumn(options[["studyId"]]),
        C.matrix = .nmaObject("C")
      ),
      # (netcomb() takes the measure from the network meta-analysis; the pairwise object of arm-based data carries it)
      .nmaSummaryMeasureArgument(options),
      arguments
    )
  ))
}

# Whether the contrast of two treatments is estimable by the additive model (a matrix of the treatments by the treatments). The contrast
# is the difference d of the rows of the C matrix (with the interactions) of the treatments; it is estimable if d lies in the row space of
# X = B C of the fit, with B the design of the analysed comparisons (+1 for the first and -1 for the second treatment of a comparison): the
# distance ||d - d X+ X|| must not exceed 1e-8 max(1, ||d||), with X+ the Moore-Penrose pseudoinverse of X (the singular values below 1e-10 of
# the largest are zero; X+ X is the projection on the row space of X). netcomb() and discomb() return finite values for all contrasts of
# the treatments, also for those that involve a component that the data do not identify (they mask only the components and the combinations).
.nmaEstimableContrasts  <- function(fit) {

  components <- fit[["C.matrix"]]
  X          <- fit[["X.matrix"]]
  svd        <- svd(X)
  rowSpace   <- svd[["v"]][, svd[["d"]] > 1e-10 * max(svd[["d"]]), drop = FALSE]
  residual   <- diag(ncol(X)) - tcrossprod(rowSpace)

  # the distance of a contrast to the row space is the distance of the residuals of the rows of the C matrix (the contrast is their difference)
  distance <- as.matrix(stats::dist(components %*% residual))
  size     <- as.matrix(stats::dist(components))

  return(distance <= 1e-8 * pmax(1, size))
}

# The estimates, standard errors, confidence limits, statistics, and p-values of the contrasts that are not estimable are missing.
.nmaMaskNotEstimable    <- function(fit, estimable) {

  for (element in c("common", "random")) {
    for (name in c("TE", "seTE", "lower", "upper", "statistic", "pval")) {

      values <- fit[[paste0(name, ".", element)]]

      if (is.null(values))
        next

      values[!estimable[rownames(values), colnames(values)]] <- NA
      fit[[paste0(name, ".", element)]] <- values
    }
  }

  return(fit)
}

# Whether some contrasts of a component fit are not estimable (the treatments of a connected network are always estimable).
.nmaHasNotEstimableContrasts <- function(fit) {

  return(.nmaIsComponentFit(fit) && any(!attr(fit, "components")[["estimable"]]))
}

# The component model of one scope (all data or a subgroup); the C matrix, the inactive treatment, and whether the network is
# disconnected are kept with the fit.
.nmaComponentFit        <- function(scope, options) {

  # (netcomb() is not available for the count-based pooling methods)
  if (.nmaIsCountBased(options))
    stop(gettext("The component model is not available for the selected pooling method."), call. = FALSE)

  standard     <- .nmaStandardFit(scope)
  disconnected <- .nmaIsDisconnectedScope(scope)

  if (jaspBase::isTryError(standard) && !disconnected)
    stop(.maTryCleanErrorMessages(standard), call. = FALSE)

  treatments <- .nmaComponentTreatments(scope, options)
  matrix     <- .nmaComponentMatrix(treatments, scope[["data"]], options)
  reference  <- .nmaComponentReference(scope, treatments, matrix[["inactive"]], options)
  objects    <- list(nma = standard, C = matrix[["matrix"]], pw = scope[["pairwise"]])
  model      <- .nmaTryCall(.nmaCallComponentModel(scope, options, reference), scope[["data"]], objects)

  if (jaspBase::isTryError(model))
    return(model)

  # (netcomb() and discomb() report the between-study standard deviation only)
  model[["tau2"]] <- if (!is.null(model[["tau"]])) model[["tau"]]^2

  # the contrasts that the components do not identify have no estimate
  estimable <- .nmaEstimableContrasts(model)
  model     <- .nmaMaskNotEstimable(model, estimable)
  attr(model, "components") <- list(
    matrix = matrix[["matrix"]], inactive = matrix[["inactive"]], dropped = matrix[["dropped"]], disconnected = disconnected, estimable = estimable
  )

  return(model)
}

.nmaComponentModelFit   <- function(scope, options) {

  fit <- try(.nmaComponentFit(scope, options), silent = TRUE)

  # (discomb() reports incomplete multi-arm studies in the terms of netmeta)
  return(.nmaCleanFitError(fit, scope[["data"]], options))
}

.nmaIsComponentFit      <- function(fit) {

  return(!jaspBase::isTryError(fit) && inherits(fit, "netcomb"))
}

# Notes of the component model: disconnected networks and indicators that are not used as components.
.nmaComponentMessages   <- function(fits, options) {

  successful <- Filter(.nmaIsComponentFit, fits)
  messages   <- NULL

  if (any(vapply(successful, function(fit) isTRUE(attr(fit, "components")[["disconnected"]]), logical(1))))
    messages <- c(messages, gettext("The network is disconnected: the component model is fitted to the subnetworks jointly (the standard model is not available)."))

  for (fit in successful) {

    dropped <- attr(fit, "components")[["dropped"]]

    if (length(dropped) > 0) {
      # (under a subgroup prefix, the network is the network of the scope)
      isSubgroup <- options[["subgroup"]] != ""
      message    <- sprintf(
        if (isSubgroup) {
          ngettext(length(dropped),
                   "The component indicator %1$s is 0 for all treatments of the scope and is not used as a component.",
                   "The component indicators %1$s are 0 for all treatments of the scope and are not used as components.")
        } else {
          ngettext(length(dropped),
                   "The component indicator %1$s is 0 for all treatments of the network and is not used as a component.",
                   "The component indicators %1$s are 0 for all treatments of the network and are not used as components.")
        },
        paste0("'", dropped, "'", collapse = ", ")
      )
      messages <- c(messages, if (isSubgroup) gettextf("%1$s: %2$s", .nmaScopeQuoted(fit), message) else message)
    }
  }

  return(messages)
}

.nmaComponentUnidentifiedFootnote <- function(table, fits) {

  unidentified <- unique(unlist(lapply(Filter(.nmaIsComponentFit, fits), function(fit) fit[["comps.unident"]])))

  if (length(unidentified) > 0)
    table$addFootnote(gettextf("The following components are not uniquely identifiable: %1$s.", paste(unidentified, collapse = ", ")), symbol = gettext("Warning:"))

  return()
}

# Component effects ----

.nmaComponentEffectsTable <- function(jaspResults, options) {

  treatmentEffectsContainer <- .nmaExtractTreatmentEffectsContainer(jaspResults)

  if (!is.null(treatmentEffectsContainer[["componentEffectsTable"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  effectsTable          <- createJaspTable(gettext("Component Effects"))
  effectsTable$position <- 3
  effectsTable$showSpecifiedColumnsOnly <- TRUE
  effectsTable$dependOn(c("componentEffects", "confidenceIntervals", "standardErrors", "transformEffectSize", "includeFullDatasetInSubgroupAnalysis"))
  treatmentEffectsContainer[["componentEffectsTable"]] <- effectsTable

  effectsTable$addColumnInfo(name = "component", type = "string", title = gettext("Component"))
  .maAddSubgroupColumn(effectsTable, options)
  effectsTable$addColumnInfo(name = "est", type = "number", title = gettext("Estimate"))
  .maAddSeColumn(effectsTable, options)
  .maAddCiColumn(effectsTable, options)
  effectsTable$addColumnInfo(name = "stat", type = "number", title = "z")
  effectsTable$addColumnInfo(name = "pval", type = "pvalue", title = gettext("p"))

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    effectsTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  rows <- .maSafeRbind(lapply(Filter(.nmaIsComponentFit, fits), function(fit) {
    element <- .nmaEffectsElement(fit)
    data.frame(
      component = fit[["comps"]],
      subgroup  = attr(fit, "subgroup"),
      est       = fit[[paste0("Comp.", element)]],
      se        = fit[[paste0("seComp.", element)]],
      lCi       = fit[[paste0("lower.Comp.", element)]],
      uCi       = fit[[paste0("upper.Comp.", element)]],
      stat      = fit[[paste0("statistic.Comp.", element)]],
      pval      = fit[[paste0("pval.Comp.", element)]]
    )
  }))

  anyNA <- FALSE
  if (!is.null(rows)) {
    columns     <- c("est", "lCi", "uCi")
    transformed <- .nmaTransformEffects(rows, options, columns)
    anyNA       <- sum(is.na(transformed[, columns])) > sum(is.na(rows[, columns]))
    rows        <- .maSafeOrderAndSimplify(transformed, "component", options)
  }

  inactive <- unique(unlist(lapply(Filter(.nmaIsComponentFit, fits), function(fit) attr(fit, "components")[["inactive"]])))
  if (length(inactive) > 0)
    effectsTable$addFootnote(gettextf("The effects of the components are relative to the inactive treatment %1$s.", paste(inactive, collapse = ", ")))
  .nmaComponentUnidentifiedFootnote(effectsTable, fits)
  for (message in .nmaTransformationMessage(options, anyNA))
    effectsTable$addFootnote(message)
  .nmaFailedFitFootnotes(effectsTable, fits)

  if (!is.null(rows))
    effectsTable$setData(rows)

  return()
}

# the components as rows of an additional section of the module forest plot
.nmaForestComponentSection <- function(fit, forest) {

  element        <- .nmaEffectsElement(fit)
  transformation <- .nmaForestTransformation(forest)
  noTest         <- .forestPlotResolveTestPlacement(FALSE, forest[["options"]])
  state          <- .forestPlotCreateAdditionalSectionState()

  for (i in seq_along(fit[["comps"]])) {
    state <- .forestPlotAppendEstimateWithInterval(
      state                   = state,
      label                   = fit[["comps"]][i],
      estimate                = list(
        est = transformation(fit[[paste0("Comp.", element)]][i]),
        lCi = transformation(fit[[paste0("lower.Comp.", element)]][i]),
        uCi = transformation(fit[[paste0("upper.Comp.", element)]][i])
      ),
      testText                = "",
      testPlacement           = noTest,
      showPredictionIntervals = FALSE
    )
  }

  finalized <- .forestPlotFinalizeAdditionalSectionState(state)

  return(.forestPlotCreateAdditionalSection(
    heading                = gettext("Components"),
    information            = finalized[["information"]],
    objects                = finalized[["objects"]],
    showHeading            = TRUE,
    includeWithFullDataset = TRUE
  ))
}

.nmaComponentForestPlot <- function(jaspResults, options) {

  if (!is.null(jaspResults[["componentForestPlot"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  forestPlot          <- createJaspPlot(title = gettext("Component Forest Plot"), width = 600, height = 400)
  forestPlot$position <- 11
  forestPlot$dependOn(c(.nmaModelDependencies, "includeFullDatasetInSubgroupAnalysis", "componentForestPlot", .nmaForestRendererDependencies))
  jaspResults[["componentForestPlot"]] <- forestPlot

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    forestPlot$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  forest <- .nmaForestPrepare(options)
  items  <- list()

  for (i in seq_along(fits)) {
    if (!.nmaIsComponentFit(fits[[i]]))
      next

    items[[length(items) + 1]] <- .forestPlotCreateInputItem(
      key           = names(fits)[i],
      index         = length(items) + 1,
      subgroup      = if (options[["subgroup"]] != "") attr(fits[[i]], "subgroup"),
      isFullDataset = FALSE,
      sections      = list(study = NULL, additional = list(components = .nmaForestComponentSection(fits[[i]], forest)))
    )
  }

  .nmaFillForestPlot(forestPlot, items, forest, .nmaFailureMessage(fits))

  return()
}
