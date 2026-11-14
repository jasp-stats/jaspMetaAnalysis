context("Classical Network Meta-Analysis")

# Edge cases of the Classical Network Meta-Analysis that the three example files cannot reach, because they need other data. The example files
# (jaspfiles/other/Network Meta-Analysis - Senn2013, Dogliotti2014, Baker2009 and their generated tests test-other-Network Meta-Analysis - *.R)
# run the analysis on the data of three published examples; the tests below build small data sets for:
#   - disconnected networks and the contrasts that the components do not identify,
#   - component models with indicator variables and interactions in subgroups,
#   - the reference treatment and excluded designs of the count-based pooling methods (Mantel-Haenszel, non-central hypergeometric, penalized
#     logistic regression),
#   - networks of two treatments,
#   - the messages of the data checks and of netmeta(),
#   - network meta-regression (failed fits in the R code, predictors of excluded studies, the note on multi-arm studies),
#   - effect size measures without an example file (continuous and incidence rate arm-level data).
# The tests compare the tables of the module with netmeta/meta functions run on the same data (netcomb(), discomb(), netmetabin(), pairwise()) and
# check the texts of the messages. They run the analysis with the GUI defaults of the QML interface (nmaTestDefaults()), except for the network
# plot and the four ranking metrics from the simulation, which are switched off (the plots take time, the simulation is random without a seed).
#
# The analyses of the three example files were checked once against their sources (2026-11-08; the script is not kept in the repository).
# 'max diff' is the maximal absolute difference over the compared values.
#
#   Example analysis                     Source                                               Compared values                 max diff
#   ------------------------------------ ---------------------------------------------------- ------------------------------- --------------------------------------------
#   Dogliotti2014 #1, inverse variance,  JSS Figure 9 (Balduzzi et al., 2023): printed odds   7 treatments: OR, 95% CI        4.9e-3 (printed to two decimals: 5e-3)
#   equal effects                        ratios
#   Dogliotti2014 #2, Mantel-Haenszel    JSS Figure 9: printed odds ratios                    7 treatments: OR, 95% CI        4.8e-3 (5e-3)
#   Dogliotti2014 #2, Mantel-Haenszel    netmetabin(method = "MH", common)                    7 treatments: OR, 95% CI, z, p  0 (the same call)
#   Dogliotti2014 #3, penalized logistic netmetabin(method = "LRP", random)                   7 treatments: OR, 95% CI, z, p  0 (the same call)
#   regression, random
#   Dogliotti2014 #4, non-central        netmetabin(method = "NCH", common)                   7 treatments: OR, 95% CI, z, p  0 (the same call)
#   hypergeometric
#   Baker2009 #1, component model        JSS Section 5.3: printed component odds ratios       5 components: OR, 95% CI        5.0e-3 (5e-3)
#   Baker2009 #1, component model        JSS Section 5.3: printed Q statistics and p value    3 tests: Q, df; p of the        Q, df: 0; p: 2.7e-5 (5e-5)
#                                                                                             difference
#   Baker2009 #2, component model with   netcomb(), C matrix with the product column, random  6 components, 7 treatments: OR, 0 (the same call); Q: 9e-4 (5e-3)
#   the interaction                                                                           95% CI, z, p; Q
#   Senn2013 #1, DerSimonian-Laird       independent GLS in plain matrix algebra, at the      9 treatments: estimate, SE, 95% 3.3e-14
#                                        fitted tau2                                          CI
#   Senn2013 #3, equal effects           metafor::rma.mv(yi, vi, mods = ~ 0 + study +         9 treatments: estimate, SE, 95% estimate 1.6e-4, SE 2.3e-5, CI 1.7e-4, z
#                                        treatment), the model of ?metadat::dat.senn2013      CI, z, p                        1.3e-3, p 3.0e-8 (*)
#                                        (arm-level data)
#   Senn2013 #2, REML, full dataset      metafor::rma.mv(..., random = ~ treatment | study,   45 comparisons: estimate, SE;   estimate 8.2e-4, SE 2.4e-3, tau2 1.1e-3
#                                        rho = 1/2), the model of ?metadat::dat.senn2013      tau2                            (0.10105 vs 0.09991) (*)
# (*) The Senn2013 differences come from the data, not from the module: netmeta::Senn2013 holds the contrasts rounded to two decimals (the standard
# errors to four decimals), whereas metadat::dat.senn2013 holds the arm-level means. netmeta() on the arm-level data equals rma.mv() (common effect
# 3e-14, REML 8e-10, tau2 0.099915 for both), and netmeta() on the rounded contrasts differs from netmeta() on the arm-level data by the same 1.6e-4
# (estimate) and 2.3e-5 (SE) as the module from rma.mv().

# Helpers ----

nmaTestDefaults <- function() {

  list(
    # variables
    studyId                    = "",
    effectSize                 = "",
    effectSizeStandardError    = "",
    treatment1                 = "",
    treatment2                 = "",
    treatment                  = "",
    armEvents                  = "",
    armMean                    = "",
    armStandardDeviation       = "",
    armSampleSize              = "",
    armPersonTime              = "",
    armEffectSize              = "",
    armEffectSizeStandardError = "",
    componentVariables         = character(0),
    predictor                  = "",
    subgroup                   = "",

    # data format and model
    dataFormat                 = "contrastBased",
    inputSummary               = FALSE,
    outcomeType                = "general",
    effectSizeMeasure          = "general",
    poolingMethod              = "inverseVariance",
    method                     = "derSimonianLaird",
    fixedEffectTest            = "z",
    referenceTreatment         = "",
    metaregressionAssumption   = "independent",

    # statistics
    heterogeneityTau                = TRUE,
    heterogeneityTau2               = TRUE,
    heterogeneityI2                 = TRUE,
    treatmentEffects                = TRUE,
    treatmentEffectsComparisons     = "versusReference",
    leagueTable                     = FALSE,
    leagueTableOrder                = "treatmentOrder",
    leagueTableDirectEstimates      = TRUE,
    leagueTableConfidenceIntervals  = TRUE,
    standardErrors                  = TRUE,
    confidenceIntervals             = TRUE,
    confidenceIntervalsLevel        = 0.95,
    predictionIntervals             = TRUE,
    transformEffectSize             = "none",

    # ranking (the GUI default of the four metrics from the simulation, SUCRA to median rank, is TRUE: switched off, they are simulated at random;
    # the tests of the metrics and of the rankogram switch them on with a seed)
    beneficialEffect           = "negative",
    rankingTable               = FALSE,
    rankingPScore              = TRUE,
    rankingSucra               = FALSE,
    rankingProbabilityOfBeingBest = FALSE,
    rankingMeanRank            = FALSE,
    rankingMedianRank          = FALSE,
    rankogram                  = FALSE,
    rankogramCumulative        = FALSE,
    rankingNumberOfSimulations = 1000,

    # inconsistency
    designDecomposition        = FALSE,
    nodeSplitting              = FALSE,
    nodeSplittingMethod        = "backCalculation",
    nodeSplittingShow          = "all",
    nodeSplittingForestPlot    = FALSE,
    netHeatPlot                = FALSE,

    # network plot (the GUI default is TRUE: switched off to speed up the tests)
    networkPlot                = FALSE,
    networkPlotLayout          = "circle",
    networkPlotEdgeWidth       = "numberOfStudies",
    networkPlotNodeSize        = "equal",
    networkPlotSizeNodes       = 1,
    networkPlotSizeEdges       = 1,
    networkPlotSizeText        = 1,
    networkPlotNumberOfStudies = FALSE,
    networkPlotMultiArmStudies = FALSE,

    # forest plot
    forestPlot                 = FALSE,
    forestPlotComparisons      = "versusReference",
    forestPlotOrder            = "treatmentOrder",
    forestPlotPredictionIntervals                      = TRUE,
    forestPlotEstimatesAndConfidenceIntervals          = TRUE,
    forestPlotConditionalEstimates                     = FALSE,
    forestPlotTestsInRightPanel                        = FALSE,
    forestPlotAlignLeftPanel                           = "right",
    forestPlotSubgroupShowTitles                       = TRUE,
    forestPlotSubgroupPanelsWithinSubgroup             = FALSE,
    forestPlotSubgroupFullDatasetEstimatedMarginalMeans = TRUE,
    forestPlotSubgroupFullDatasetModelInformation      = TRUE,
    forestPlotAuxiliaryDigits                          = 2,
    forestPlotAuxiliaryTestsInformation                = "statisticAndPValue",
    forestPlotAuxiliaryPlotColor                       = "black",
    forestPlotAuxiliaryAddVerticalLine                 = FALSE,
    forestPlotAuxiliaryAddVerticalLineValue            = 0,
    forestPlotAuxiliaryAddVerticalLine2                = FALSE,
    forestPlotAuxiliaryAddVerticalLineValue2           = 0,
    forestPlotAuxiliaryEffectLabel                     = "Effect Size",
    forestPlotAuxiliarySetXAxisLimit                   = FALSE,
    forestPlotAuxiliarySetXAxisLimitLower              = -1,
    forestPlotAuxiliarySetXAxisLimitUpper              = 1,
    forestPlotAuxiliarySetXAxisTicks                   = FALSE,
    forestPlotAuxiliarySetXAxisTicksValues             = "-1, -0.5, 0, 0.5, 1",
    forestPlotAuxiliaryXAxisTransformLabelsOnly        = TRUE,
    forestPlotSizeEstimates                            = 1,
    forestPlotSizeText                                 = 1,
    forestPlotSizeAxisLabels                           = 1,
    forestPlotSizeRow                                  = 1,
    forestPlotSizeLeftPanel                            = 1,
    forestPlotSizePlotArea                             = 1,
    forestPlotSizeRightPanel                           = 1,
    forestPlotMappingColor                             = "",
    forestPlotMappingShape                             = "",

    # small-study effects
    funnelPlot                           = FALSE,
    funnelPlotTreatmentOrder             = "referenceFirst",
    funnelPlotTreatmentOrderCustom       = list(),
    funnelPlotPseudoConfidenceIntervals  = TRUE,
    funnelPlotEggerTest                  = FALSE,
    funnelPlotBeggTest                   = FALSE,
    funnelPlotThompsonSharpTest          = FALSE,

    # evidence structure
    directEvidence             = FALSE,
    contributionMatrix         = FALSE,
    contributionMatrixMethod   = "shortestPath",
    studyImpact                = FALSE,
    pairwiseMetaAnalyses       = FALSE,

    # model (standard or component) and the outputs of the component model
    model                        = "standard",
    componentSpecification       = "separator",
    componentSeparator           = "+",
    componentInactiveTreatment   = "",
    componentInteractions        = list(),
    componentEffects             = TRUE,
    componentForestPlot          = FALSE,

    # advanced
    showNetmetaRCode                      = FALSE,
    includeFullDatasetInSubgroupAnalysis  = FALSE,
    advancedMultiArmTolerance             = 0.001,
    setSeed                               = FALSE,
    seed                                  = 1,
    fixParametersTau2                     = FALSE,
    fixParametersTau2Value                = "1",
    advancedAdd                           = 0.5,
    advancedTo                            = "onlyZero",
    advancedDropStudiesWithNoCasesOrEvents = "yes"
  )
}

nmaTestRun <- function(dataset, ...) {

  options  <- nmaTestDefaults()
  override <- list(...)
  stopifnot(all(names(override) %in% names(options)))
  options[names(override)] <- override

  return(jaspTools::runAnalysis("ClassicalNetworkMetaAnalysis", dataset, options, view = FALSE))
}

# a table of the results as a data.frame (the path: the container and the table; the fields are the columns)
nmaTestTable <- function(results, ...) {

  keys <- c(...)
  x    <- results[["results"]][[keys[1]]]
  for (key in keys[-1])
    x <- x[["collection"]][[key]]

  rows   <- x[["data"]]
  fields <- unique(unlist(lapply(rows, names)))
  table  <- data.frame(row.names = seq_along(rows))
  for (field in fields) {
    values <- unlist(lapply(rows, function(row) if (is.null(row[[field]])) NA else row[[field]]))
    # numeric columns with empty cells (e.g., no interval for tau) are read as character: convert them back
    if (is.character(values)) {
      numbers <- suppressWarnings(as.numeric(values))
      if (all(is.na(numbers) == (is.na(values) | values == "")))
        values <- numbers
    }
    table[[field]] <- values
  }

  return(table)
}

nmaTestEffectsTable <- function(results) {

  return(nmaTestTable(results, "treatmentEffectsContainer", "treatmentEffectsContainer_treatmentEffectsTable"))
}

nmaTestNumbers <- function(text) {

  # the numbers of a test statistic such as "Q(27) = 48.15"
  return(as.numeric(regmatches(text, gregexpr("-?[0-9]+[.]?[0-9]*", text))[[1]]))
}

nmaTestData <- function(name) {

  environment <- new.env()
  data(list = name, package = "netmeta", envir = environment)
  data <- environment[[name]]

  if (name == "Baker2009") {
    data <- data.frame(study = factor(data$study), treatment = factor(data$treatment), exac = data$exac, total = data$total)
  }

  return(data)
}

# the footnotes of a table (the path as for nmaTestTable)
nmaTestFootnotes <- function(results, ...) {

  keys <- c(...)
  x    <- results[["results"]][[keys[1]]]
  for (key in keys[-1])
    x <- x[["collection"]][[key]]

  return(vapply(x[["footnotes"]], function(footnote) footnote[["text"]], character(1)))
}

# the R code of the analysis (the text of the HTML element)
nmaTestCode <- function(results) {

  code <- results[["results"]][["netmetaRCode"]][["text"]]
  code <- gsub("<br/>", "\n", code, fixed = TRUE)
  code <- gsub("<[^>]+>", "", code)

  for (entity in list(c("&lt;", "<"), c("&gt;", ">"), c("&amp;", "&"), c("&quot;", "\"")))
    code <- gsub(entity[1], entity[2], code, fixed = TRUE)

  return(code)
}

# the R code evaluated in a new environment that contains only the data (the objects of the script are in the returned environment)
nmaTestEvaluate <- function(code, data) {

  environment <- new.env(parent = globalenv())
  environment[["dataset"]] <- data

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  suppressWarnings(suppressMessages(utils::capture.output(eval(parse(text = code), envir = environment))))

  return(environment)
}

# a connected contrast-based network of five treatments in two subgroups (G1, G2) with study-level variables: a numeric covariate and a
# factor (moderator) with the levels L1 and L2 in G1, and L2 and L3 in G2
nmaTestSynthetic <- function() {

  set.seed(21)
  effects <- c(Placebo = 0, A = -0.3, B = -0.5, C = -0.2, D = -0.7)
  pairs   <- list(c("A", "Placebo"), c("B", "Placebo"), c("C", "Placebo"), c("D", "Placebo"), c("A", "B"), c("B", "C"), c("C", "D"), c("A", "D"), c("A", "C"))
  rows    <- list()

  for (group in c("G1", "G2")) {
    for (i in 1:20) {
      pair <- pairs[[(i - 1) %% length(pairs) + 1]]
      rows[[length(rows) + 1]] <- data.frame(
        TE        = effects[[pair[1]]] - effects[[pair[2]]] + stats::rnorm(1, 0, 0.3),
        seTE      = stats::runif(1, 0.1, 0.3),
        treat1    = pair[1],
        treat2    = pair[2],
        study     = paste0(group, "-s", i),
        group     = group,
        moderator = if (group == "G1") c("L1", "L2")[i %% 2 + 1] else c("L2", "L3")[i %% 2 + 1],
        covariate = round(stats::rnorm(1), 2),
        stringsAsFactors = FALSE
      )
    }
  }

  data <- do.call(rbind, rows)
  rownames(data) <- NULL
  for (variable in c("treat1", "treat2", "study", "group", "moderator"))
    data[[variable]] <- factor(data[[variable]])

  return(data)
}

nmaTestSyntheticOptions <- list(
  studyId = "study", effectSize = "TE", effectSizeStandardError = "seTE", treatment1 = "treat1", treatment2 = "treat2", referenceTreatment = "Placebo"
)

# the variables of contrast-based data (as in the Senn2013 example file)
nmaTestSennOptions <- list(
  studyId = "study", effectSize = "TE", effectSizeStandardError = "seTE", treatment1 = "treat1", treatment2 = "treat2", referenceTreatment = "Placebo"
)

# a column filled forward (the subgroup of a table is shown in the first row of its rows only)
nmaTestFill <- function(x) {

  for (i in seq_along(x)[-1])
    if (is.na(x[i]) || x[i] == "") x[i] <- x[i - 1]

  return(x)
}

# the numbers of a table column (empty cells and NaN are NA)
nmaTestNumeric <- function(x) {

  return(suppressWarnings(as.numeric(x)))
}

# the error message of an output element (NULL if the element has no error)
nmaTestError <- function(element) {

  return(element[["error"]][["errorMessage"]])
}

# the labels of the left panel of a forest plot from top to bottom (the first text layer with labels)
nmaTestForestLabels <- function(results) {

  object <- results[["state"]][["figures"]][[results[["results"]][["forestPlot"]][["data"]]]][["obj"]]

  for (layer in object[["layers"]])
    if (class(layer[["geom"]])[1] == "GeomText" && is.data.frame(layer[["data"]]) && "label" %in% colnames(layer[["data"]]))
      return(as.character(layer[["data"]][["label"]][order(-layer[["data"]][["y"]])]))

  return(character(0))
}

# the component effects of a result as a data.frame (the subgroup column is filled)
nmaTestComponentTable <- function(results) {

  table <- nmaTestTable(results, "treatmentEffectsContainer", "treatmentEffectsContainer_componentEffectsTable")

  # (the component is shown in the first row of its rows only)
  table[["component"]] <- nmaTestFill(table[["component"]])

  return(table)
}

# two subnetworks without a common treatment, {Placebo, A, B} and {C, A+B, A+B+C}; with 'third', a third subnetwork of the
# treatments D and A+D (D is not identified by the other subnetworks)
nmaTestDisconnected <- function(third = FALSE) {

  data <- data.frame(
    TE     = c(-0.45, -0.35, -0.15, 0.55, 0.65, -0.75, -0.25),
    seTE   = c(0.20, 0.20, 0.25, 0.20, 0.25, 0.20, 0.30),
    treat1 = c("A", "B", "A", "C", "C", "A+B+C", "A+B+C"),
    treat2 = c("Placebo", "Placebo", "B", "A+B", "A+B", "C", "A+B"),
    study  = paste0("s", 1:7),
    stringsAsFactors = FALSE
  )

  if (third)
    data <- rbind(data, data.frame(TE = 0.30, seTE = 0.25, treat1 = "A+D", treat2 = "D", study = "s8", stringsAsFactors = FALSE))

  for (variable in c("treat1", "treat2", "study"))
    data[[variable]] <- factor(data[[variable]])

  return(data)
}

nmaTestDisconnectedOptions <- list(
  studyId = "study", effectSize = "TE", effectSizeStandardError = "seTE", treatment1 = "treat1", treatment2 = "treat2",
  model = "component", componentInactiveTreatment = "Placebo", referenceTreatment = "Placebo"
)

# arm-based continuous data with component indicators (P/Placebo = all zero): g1 holds Placebo, A, and B (cC is 0 for all its treatments), g2 holds
# Placebo, A, A+C, and B+C
nmaTestIndicatorData <- function() {

  set.seed(2)
  arms <- list(
    s1 = c("Placebo", "A"), s2 = c("Placebo", "B"), s3 = c("A", "B"), s4 = c("Placebo", "A", "B"),
    s5 = c("Placebo", "A"), s6 = c("Placebo", "A+C"), s7 = c("A", "A+C"), s8 = c("Placebo", "B+C"), s9 = c("A+C", "B+C")
  )
  group      <- c(s1 = "g1", s2 = "g1", s3 = "g1", s4 = "g1", s5 = "g2", s6 = "g2", s7 = "g2", s8 = "g2", s9 = "g2")
  effects    <- c(Placebo = 0, A = -0.4, B = -0.3, "A+C" = -0.7, "B+C" = -0.55)
  indicators <- data.frame(row.names = names(effects), cA = c(0, 1, 0, 1, 0), cB = c(0, 0, 1, 0, 1), cC = c(0, 0, 0, 1, 1))

  data       <- do.call(rbind, lapply(names(arms), function(study) data.frame(study = study, treatment = arms[[study]], group = group[[study]])))
  data$mean  <- effects[data$treatment] + stats::rnorm(nrow(data), 0, 0.05)
  data$sd    <- 1
  data$n     <- 50
  data       <- cbind(data, indicators[data$treatment, ])
  rownames(data) <- NULL

  for (variable in c("study", "treatment", "group"))
    data[[variable]] <- factor(data[[variable]])

  return(data)
}

nmaTestIndicatorOptions <- list(
  dataFormat = "armBased", outcomeType = "continuous", effectSizeMeasure = "MD", studyId = "study", treatment = "treatment",
  armMean = "mean", armStandardDeviation = "sd", armSampleSize = "n",
  model = "component", componentSpecification = "indicatorVariables", componentVariables = c("cA", "cB", "cC")
)

# netcomb() of the random-effects (DerSimonian-Laird) network meta-analysis of arm-based continuous data with the C matrix of the indicators
nmaTestIndicatorModel <- function(data, indicators) {

  pairwise <- suppressWarnings(meta::pairwise(
    treat = as.character(data$treatment), mean = data$mean, sd = data$sd, n = data$n, studlab = as.character(data$study), sm = "MD"
  ))
  network  <- suppressWarnings(netmeta::netmeta(pairwise, common = FALSE, random = TRUE, method.tau = "DL", backtransf = FALSE))
  C        <- as.matrix(unique(data[, c("treatment", indicators)])[, indicators, drop = FALSE])
  rownames(C) <- as.character(unique(data[, c("treatment", indicators)])$treatment)

  return(suppressWarnings(netmeta::netcomb(network, C.matrix = C[network[["trts"]], , drop = FALSE], common = FALSE, random = TRUE)))
}

# contrast-based network of two subgroups with the same pairs of treatments (two-arm studies, no noise)
nmaTestComponentSubgroups <- function(effects, noise = 0) {

  rows <- list()
  for (group in names(effects)) {
    pairs <- t(utils::combn(names(effects[[group]]), 2))
    for (replicate in 1:2)
      rows[[length(rows) + 1]] <- data.frame(
        study  = paste0(group, "-r", replicate, "-", seq_len(nrow(pairs))),
        treat1 = pairs[, 1], treat2 = pairs[, 2],
        TE     = unname(effects[[group]][pairs[, 1]] - effects[[group]][pairs[, 2]]) + stats::rnorm(nrow(pairs), 0, noise),
        seTE   = 0.2, group = group, stringsAsFactors = FALSE
      )
  }

  data <- do.call(rbind, rows)
  for (variable in c("study", "treat1", "treat2", "group"))
    data[[variable]] <- factor(data[[variable]])

  return(data)
}

# the messages of all elements with an error in a result
nmaTestAllErrors <- function(x) {

  if (!is.list(x))
    return(character(0))

  messages <- if (is.list(x[["error"]]) && !is.null(x[["error"]][["errorMessage"]])) x[["error"]][["errorMessage"]] else character(0)

  return(c(messages, unlist(lapply(x, nmaTestAllErrors), use.names = FALSE)))
}

# the names of the columns of a table
nmaTestFields <- function(element) {

  return(vapply(element[["schema"]][["fields"]], function(field) field[["name"]], character(1)))
}

nmaTestBinaryOptions <- list(
  dataFormat = "armBased", outcomeType = "binary", effectSizeMeasure = "OR", studyId = "study", treatment = "treatment", armEvents = "events",
  armSampleSize = "n"
)

# arm-based binary data without a transformation of the pairwise() call of the module
nmaTestPairwise <- function(data) {

  return(suppressWarnings(meta::pairwise(
    treat = as.character(data$treatment), event = data$events, n = data$n, studlab = as.character(data$study), sm = "OR",
    incr = 0.5, method.incr = "only0", allstudies = FALSE
  )))
}

# six trials with 100 participants per arm: four of A vs. B and two of B vs. C, with opposite effects (a large dispersion)
nmaTestLrpData <- function() {

  return(data.frame(
    study     = factor(rep(paste0("s", 1:6), each = 2)),
    treatment = factor(c("A", "B", "A", "B", "A", "B", "A", "B", "B", "C", "B", "C")),
    events    = c(10, 40, 40, 10, 10, 40, 40, 10, 10, 40, 40, 10),
    n         = 100
  ))
}

# the effects table of a result: its title
nmaTestEffectsTitle <- function(results) {

  return(results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]][["title"]])
}

# A, B, C, D with a sparse A-B design (A 1/100, B 0/100): the Mantel-Haenszel and the non-central hypergeometric methods exclude it, and A
# is not in the network (netmetabin selects A as its reference treatment before it excludes the design)
nmaTestSparseDesignData <- function() {

  return(data.frame(
    study     = factor(rep(paste0("s", 1:4), each = 2)),
    treatment = factor(c("A", "B", "B", "C", "C", "D", "B", "D")),
    events    = c(1, 0, 10, 15, 10, 20, 12, 18),
    n         = 100
  ))
}

# two components of studies that share no treatment: {A, B} (s1, s2) and {C, D} (s3, s4)
nmaTestDisconnectedCounts <- function() {

  return(data.frame(
    study     = factor(rep(paste0("s", 1:4), each = 2)),
    treatment = factor(c("A", "B", "A", "B", "C", "D", "C", "D")),
    events    = c(5, 9, 6, 10, 8, 12, 7, 11),
    n         = 50
  ))
}

# the agreement of two vectors of values: the same length (not zero), the same missing values, and a maximal absolute difference below the
# tolerance (a table or column that is missing fails instead of passing vacuously)
nmaTestAgree <- function(actual, expected, tolerance = 1e-10, info = NULL) {

  expect_gt(length(expected), 0)
  expect_identical(length(actual), length(expected), info = info)

  actual   <- as.numeric(unname(actual))
  expected <- as.numeric(unname(expected))
  both     <- !is.na(actual) & !is.na(expected)

  expect_identical(is.na(actual), is.na(expected), info = info)
  expect_lt(if (any(both)) max(abs(actual[both] - expected[both])) else 0, tolerance, label = paste(c("the maximal difference", info), collapse = " "))
}

# the treatment effects table of a result against the effects of a netmeta fit (versus its reference treatment): estimate, standard error,
# limits, statistic, p-value
nmaTestAgreeNetwork <- function(results, network, tolerance = 1e-10, info = NULL) {

  element   <- if (isTRUE(network[["random"]])) "random" else "common"
  reference <- network[["reference.group"]]
  others    <- setdiff(network[["trts"]], reference)
  effects   <- nmaTestEffectsTable(results)

  expect_setequal(effects$comparison, others)

  for (pair in list(c("est", "TE"), c("se", "seTE"), c("lCi", "lower"), c("uCi", "upper"), c("stat", "statistic"), c("pval", "pval")))
    nmaTestAgree(effects[[pair[1]]][match(others, effects$comparison)], network[[paste(pair[2], element, sep = ".")]][others, reference], tolerance, info = paste(info, pair[1]))

  return(invisible(effects))
}

# the component effects table of a result (the component column is filled forward) against netcomb() or discomb()
nmaTestAgreeComponents <- function(results, model, element, tolerance = 1e-10, info = NULL) {

  components <- nmaTestComponentTable(results)

  expect_identical(components$component, model[["comps"]])

  for (pair in list(c("est", "Comp"), c("se", "seComp"), c("lCi", "lower.Comp"), c("uCi", "upper.Comp"), c("stat", "statistic.Comp"), c("pval", "pval.Comp")))
    nmaTestAgree(components[[pair[1]]], model[[paste(pair[2], element, sep = ".")]], tolerance, info = paste(info, pair[1]))

  return(invisible(components))
}

# Baker2009 with one 0/1 indicator variable per component (the parts of the original treatment names; Placebo has all indicators 0). The
# treatments are renamed T1, T2, ... : the components cannot be derived from the names, only the indicators define them.
nmaTestBakerIndicators <- function() {

  data       <- nmaTestData("Baker2009")
  original   <- as.character(data$treatment)
  components <- sort(setdiff(unique(unlist(strsplit(original, "+", fixed = TRUE))), "Placebo"))

  for (component in components)
    data[[paste0("c", component)]] <- as.numeric(vapply(strsplit(original, "+", fixed = TRUE), function(parts) component %in% parts, logical(1)))

  labels         <- setNames(paste0("T", seq_along(sort(unique(original)))), sort(unique(original)))
  data$treatment <- factor(unname(labels[original]))

  return(list(data = data, variables = paste0("c", components), components = components, labels = labels))
}

# arm-level data of ten studies (four treatments, one study with three arms) with every type of outcome: events (with a single zero cell in the
# study s9 and no events in both arms of the study s10) and sample sizes, means and standard deviations (positive means), events and person-time,
# and an effect size with its standard error
nmaTestArmData <- function() {

  designs <- list(
    s1 = c("A", "B"), s2 = c("A", "B"), s3 = c("A", "C"), s4 = c("B", "C"), s5 = c("A", "B", "C"), s6 = c("B", "D"), s7 = c("C", "D"), s8 = c("A", "D"), s9 = c("A", "B"), s10 = c("C", "D")
  )
  data <- do.call(rbind, lapply(names(designs), function(study) data.frame(study = study, treatment = designs[[study]], stringsAsFactors = FALSE)))

  data$events <- c(12, 18, 10, 17, 15, 21, 14, 19, 11, 16, 20, 13, 22, 17, 25, 9, 20, 0, 6, 0, 0)
  data$n      <- c(100, 100, 80, 80, 120, 120, 90, 90, 70, 70, 70, 100, 100, 110, 110, 60, 60, 50, 50, 40, 40)
  data$mean   <- c(5.1, 4.2, 5.4, 4.9, 5.0, 4.1, 4.6, 4.0, 5.2, 4.4, 4.1, 4.5, 3.8, 4.2, 3.6, 5.3, 3.9, 5.0, 4.1, 4.3, 3.7)
  data$sd     <- c(1.2, 1.1, 1.3, 1.2, 1.0, 1.1, 1.2, 1.3, 1.1, 1.2, 1.0, 1.1, 1.2, 1.0, 1.1, 1.3, 1.2, 1.1, 1.2, 1.0, 1.1)
  data$time   <- c(200, 210, 150, 160, 240, 230, 170, 180, 140, 150, 140, 190, 200, 210, 220, 120, 115, 100, 95, 80, 85)
  data$te     <- c(0.50, 0.30, 0.55, 0.35, 0.45, 0.20, 0.40, 0.25, 0.50, 0.30, 0.15, 0.35, 0.10, 0.30, 0.05, 0.60, 0.20, 0.45, 0.25, 0.40, 0.10)
  data$se     <- c(0.20, 0.18, 0.25, 0.22, 0.15, 0.17, 0.21, 0.19, 0.16, 0.18, 0.20, 0.23, 0.19, 0.22, 0.20, 0.26, 0.24, 0.30, 0.28, 0.27, 0.25)
  data$study     <- factor(data$study)
  data$treatment <- factor(data$treatment)

  return(data)
}

nmaTestArmOptions <- list(
  binary        = list(dataFormat = "armBased", outcomeType = "binary", studyId = "study", treatment = "treatment", armEvents = "events", armSampleSize = "n"),
  continuous    = list(dataFormat = "armBased", outcomeType = "continuous", studyId = "study", treatment = "treatment", armMean = "mean", armStandardDeviation = "sd", armSampleSize = "n"),
  incidenceRate = list(dataFormat = "armBased", outcomeType = "incidenceRate", studyId = "study", treatment = "treatment", armEvents = "events", armPersonTime = "time"),
  general       = list(dataFormat = "armBased", outcomeType = "general", studyId = "study", treatment = "treatment", armEffectSize = "te", armEffectSizeStandardError = "se")
)

# the random-effects (DerSimonian-Laird) network meta-analysis of meta::pairwise() with the arguments of the module for the data of nmaTestArmData()
nmaTestArmNetwork <- function(data, arguments) {

  pairwise <- do.call(meta::pairwise, c(list(treat = as.character(data$treatment), studlab = as.character(data$study)), arguments))

  return(suppressWarnings(netmeta::netmeta(pairwise, common = FALSE, random = TRUE, method.tau = "DL", reference.group = "A", backtransf = FALSE)))
}

# the module against pairwise() and netmeta(): treatment effects, the between-study variance, and the total Q statistic
nmaTestAgreeArmModel <- function(results, network, info) {

  expect_identical(results[["status"]], "complete", info = info)
  nmaTestAgreeNetwork(results, network, 1e-10, info = info)

  heterogeneity <- nmaTestTable(results, "modelSummaryContainer", "modelSummaryContainer_heterogeneityTable")
  nmaTestAgree(heterogeneity$est[2], network[["tau"]]^2, 1e-10, info = paste(info, "tau2"))

  tests <- nmaTestTable(results, "modelSummaryContainer", "modelSummaryContainer_testsTable")
  nmaTestAgree(nmaTestNumbers(tests$stat[1])[2], network[["Q"]], 0.005 + 1e-9, info = paste(info, "Q"))
}

# four contrasts of three treatments in four studies, with a study-level variable (x) and a grouping variable (g)
nmaTestValidationContrast <- function() {

  data <- data.frame(
    TE = c(0.2, 0.4, 0.1, 0.3), seTE = c(0.1, 0.2, 0.15, 0.2), treat1 = c("A", "A", "B", "A"), treat2 = c("B", "C", "C", "B"), study = paste0("s", 1:4),
    x = c(1, 2, 3, 4), g = c("u", "u", "v", "v"), stringsAsFactors = FALSE
  )

  for (variable in c("treat1", "treat2", "study", "g"))
    data[[variable]] <- factor(data[[variable]])

  return(data)
}

# four studies of two arms with every type of arm-level variables
nmaTestValidationArms <- function() {

  data <- data.frame(
    study = rep(paste0("s", 1:4), each = 2), treatment = c("A", "B", "A", "C", "B", "C", "A", "B"),
    events = c(5, 9, 8, 12, 3, 7, 10, 15), n = c(50, 50, 60, 60, 40, 40, 80, 80), mean = 1:8, sd = rep(1, 8), time = c(10, 12, 11, 13, 14, 15, 16, 17),
    te = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), se = rep(c(0.1, 0.2), 4), stringsAsFactors = FALSE
  )

  for (variable in c("study", "treatment"))
    data[[variable]] <- factor(data[[variable]])

  return(data)
}

# the error of the tests table (the table that reports the failure of the network meta-analysis of the full data)
nmaTestFitError <- function(results) {

  return(nmaTestError(results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]))
}

# the contrast-based network of nmaTestSynthetic() with a three-arm study (Placebo, A, B: all three comparisons, arm variance 0.04) in each of the
# given subgroups
nmaTestMultiArmContrastData <- function(groups) {

  data <- nmaTestSynthetic()
  for (variable in c("treat1", "treat2", "study", "group", "moderator"))
    data[[variable]] <- as.character(data[[variable]])

  rows <- do.call(rbind, lapply(groups, function(group) data.frame(
    TE = c(-0.3, -0.5, 0.2), seTE = sqrt(0.08), treat1 = c("A", "B", "A"), treat2 = c("Placebo", "Placebo", "B"), study = paste0(group, "-multi"),
    group = group, moderator = if (group == "G1") "L1" else "L2", covariate = 0.5, stringsAsFactors = FALSE
  )))

  data <- rbind(data, rows)
  for (variable in c("treat1", "treat2", "study", "group", "moderator"))
    data[[variable]] <- factor(data[[variable]])

  return(data)
}

# the network meta-regression of an analysis: whether the two tables have the note about the multi-arm studies (the same footnote in both), and
# whether the sampling covariance of the netmetareg() of the printed code has non-zero covariances (netmeta's own choice: the full covariance of the
# comparisons of a multi-arm study, or the diagonal matrix of the squared standard errors)
nmaTestCovarianceNote <- "For this input, netmeta treats the comparisons of a multi-arm study as independent in the meta-regression, so the standard errors may be understated."

nmaTestCovarianceCase <- function(data, options, multiArm, note, info) {

  results <- do.call(nmaTestRun, c(list(data), options, list(method = "restrictedML", treatmentEffects = FALSE, showNetmetaRCode = TRUE)))
  expect_identical(results[["status"]], "complete", info = info)

  for (table in c("metaregressionTestsTable", "metaregressionCoefficientsTable")) {
    footnotes <- nmaTestFootnotes(results, "metaregressionContainer", paste0("metaregressionContainer_", table))
    expect_identical(nmaTestCovarianceNote %in% footnotes, note, info = paste(info, table))
  }

  # (netmeta uses the full covariance of multi-arm studies unless the note applies; without multi-arm studies the matrix is diagonal)
  covariance <- nmaTestEvaluate(nmaTestCode(results), data)[["metareg"]][["V"]]
  expect_identical(any(covariance[row(covariance) != col(covariance)] != 0), multiArm && !note, info = info)

  return(results)
}

# Disconnected networks and component contrasts ----

test_that("the network plot of the component model of a disconnected network renders for each layout", {

  data <- nmaTestDisconnected()

  for (layout in c("circle", "optimal", "stress")) {

    results <- do.call(nmaTestRun, c(list(data), nmaTestDisconnectedOptions, list(networkPlot = TRUE, networkPlotLayout = layout)))
    plot    <- results[["results"]][["networkPlot"]]

    expect_identical(results[["status"]], "complete", info = layout)
    expect_null(nmaTestError(plot), info = layout)
    expect_identical(plot[["status"]], "complete", info = layout)

    # the nodes (six treatments) and the edges (six direct comparisons) of the network
    object <- results[["state"]][["figures"]][[plot[["data"]]]][["obj"]]
    layers <- vapply(object[["layers"]], function(layer) class(layer[["geom"]])[1], character(1))
    expect_identical(nrow(object[["layers"]][[which(layers == "GeomSegment")[1]]][["data"]]), 6L, info = layout)
    expect_identical(nrow(object[["layers"]][[which(layers == "GeomPoint")[1]]][["data"]]), 6L, info = layout)
  }
})

test_that("contrasts that the components do not identify are NA in every output, the estimable ones keep their values", {

  data    <- nmaTestDisconnected(third = TRUE)
  results <- do.call(nmaTestRun, c(list(data), nmaTestDisconnectedOptions, list(
    method = "equalEffects", treatmentEffectsComparisons = "allPairwise", leagueTable = TRUE, forestPlot = TRUE, forestPlotComparisons = "allPairwise"
  )))
  expect_identical(results[["status"]], "complete")

  # the estimability by the rank of the design: the contrast of two treatments is estimable if the difference of their component vectors
  # is in the row space of the design of the comparisons (independent of the module: ranks by qr())
  treatments <- sort(unique(c(as.character(data$treat1), as.character(data$treat2))))
  components <- c("A", "B", "C", "D")
  C          <- t(vapply(strsplit(treatments, "+", fixed = TRUE), function(parts) as.numeric(components %in% parts), numeric(length(components))))
  rownames(C) <- treatments
  X          <- t(vapply(seq_len(nrow(data)), function(i) C[as.character(data$treat1[i]), ] - C[as.character(data$treat2[i]), ], numeric(length(components))))
  estimable  <- function(first, second) qr(rbind(X, C[first, ] - C[second, ]))$rank == qr(X)$rank

  model <- suppressWarnings(netmeta::discomb(data$TE, data$seTE, as.character(data$treat1), as.character(data$treat2), as.character(data$study),
                                             inactive = "Placebo", common = TRUE, random = FALSE, reference.group = "Placebo", backtransf = FALSE))

  effects <- nmaTestEffectsTable(results)
  pairs   <- strsplit(effects$comparison, " vs. ", fixed = TRUE)
  first   <- vapply(pairs, `[`, character(1), 1)
  second  <- vapply(pairs, `[`, character(1), 2)
  ok      <- unname(mapply(estimable, first, second))

  expect_equal(nrow(effects), length(treatments) * (length(treatments) - 1) / 2)
  expect_true(any(!ok))
  expect_true(estimable("C", "Placebo") && estimable("A+B+C", "Placebo"))   # contrasts between the subnetworks that the components identify

  # non-estimable contrasts have no estimate, standard error, interval, statistic, and p-value
  for (column in c("est", "se", "lCi", "uCi", "stat", "pval"))
    expect_true(all(is.na(nmaTestNumeric(effects[[column]][!ok]))), info = column)

  # estimable contrasts keep the values of discomb()
  index <- cbind(first[ok], second[ok])
  expect_lt(max(abs(nmaTestNumeric(effects$est[ok])  - model[["TE.common"]][index])), 1e-10)
  expect_lt(max(abs(nmaTestNumeric(effects$se[ok])   - model[["seTE.common"]][index])), 1e-10)
  expect_lt(max(abs(nmaTestNumeric(effects$lCi[ok])  - model[["lower.common"]][index])), 1e-10)
  expect_lt(max(abs(nmaTestNumeric(effects$pval[ok]) - model[["pval.common"]][index])), 1e-10)

  # the note under the table
  notes <- nmaTestFootnotes(results, "treatmentEffectsContainer", "treatmentEffectsContainer_treatmentEffectsTable")
  expect_true(any(grepl("not estimable", notes, fixed = TRUE)))

  # the league table shows no value for them
  league <- nmaTestTable(results, "treatmentEffectsContainer", "treatmentEffectsContainer_leagueTable")
  expect_true(all(is.na(league[[paste0("trt", match("D", sort(treatments)))]][league$treatment == "Placebo"]) |
                  league[[paste0("trt", match("D", sort(treatments)))]][league$treatment == "Placebo"] == ""))

  # the forest plot is drawn
  expect_identical(results[["results"]][["forestPlot"]][["status"]], "complete")

  # the outputs that need all contrasts are not available: the ranking, the order of the league table and of the forest plot by the ranking
  results <- do.call(nmaTestRun, c(list(data), nmaTestDisconnectedOptions, list(
    method = "equalEffects", rankingTable = TRUE, leagueTable = TRUE, leagueTableOrder = "ranking", forestPlot = TRUE, forestPlotOrder = "ranking"
  )))
  expect_identical(results[["status"]], "complete")
  expect_true(grepl("not estimable", nmaTestError(results[["results"]][["rankingContainer"]][["collection"]][["rankingContainer_rankingTable"]]), fixed = TRUE))
  expect_true(any(grepl("not estimable", nmaTestFootnotes(results, "treatmentEffectsContainer", "treatmentEffectsContainer_leagueTable"), fixed = TRUE)))
  # the forest plot falls back to the treatment order (as the league table), with the note of the reason
  expect_identical(results[["results"]][["forestPlot"]][["status"]], "complete")
  expect_null(nmaTestError(results[["results"]][["forestPlot"]]))
  expect_true(grepl("not estimable", results[["results"]][["forestPlotWarning"]][["text"]], fixed = TRUE))
  # (the rows are the treatments in the treatment order, versus the reference treatment)
  expect_identical(nmaTestForestLabels(results), paste(setdiff(treatments, "Placebo"), "vs. Placebo"))
})

test_that("a disconnected network is reported by the standard model with its subnetworks, the component model analyses it", {

  # contrast-based and arm-based networks of the subnetworks {A, B} and {C, D}
  contrast <- data.frame(TE = c(0.2, 0.4, 0.1, 0.3), seTE = 0.2, treat1 = c("A", "A", "C", "C"), treat2 = c("B", "B", "D", "D"), study = paste0("s", 1:4), stringsAsFactors = FALSE)
  contrast[c("treat1", "treat2", "study")] <- lapply(contrast[c("treat1", "treat2", "study")], factor)
  arms     <- nmaTestDisconnectedCounts()
  expected <- "The network is disconnected: it consists of 2 separate subnetworks, (1) A, B; (2) C, D. A component network meta-analysis can analyse disconnected networks."

  expect_identical(netmeta::netconnection(as.character(contrast$treat1), as.character(contrast$treat2), as.character(contrast$study))[["n.subnets"]], 2L)

  for (case in list(
    list(data = contrast, options = nmaTestSennOptions[c("studyId", "effectSize", "effectSizeStandardError", "treatment1", "treatment2")]),
    list(data = arms, options = nmaTestBinaryOptions)
  )) {
    results <- do.call(nmaTestRun, c(list(case$data), case$options, list(method = "equalEffects")))
    expect_identical(results[["status"]], "complete")
    expect_identical(nmaTestFitError(results), expected)
    expect_identical(nmaTestError(results[["results"]][["treatmentEffectsContainer"]][["collection"]][["treatmentEffectsContainer_treatmentEffectsTable"]]), expected)

    # the component model fits the network (the treatments are components of their own)
    component <- do.call(nmaTestRun, c(list(case$data), case$options, list(method = "equalEffects", model = "component", componentInactiveTreatment = "")))
    expect_identical(component[["status"]], "complete")
    expect_length(nmaTestAllErrors(component[["results"]]), 0)
    expect_true(any(grepl("The network is disconnected: the component model is fitted to the subnetworks jointly", nmaTestFootnotes(component, "modelSummaryContainer", "modelSummaryContainer_testsTable"), fixed = TRUE)))
  }
})

# Indicator variables and interactions ----

test_that("the component model with indicator variables equals netcomb() with the C matrix of the indicators", {

  baker   <- nmaTestBakerIndicators()
  data    <- baker$data
  options <- list(
    dataFormat = "armBased", outcomeType = "binary", effectSizeMeasure = "OR", studyId = "study", treatment = "treatment", armEvents = "exac", armSampleSize = "total",
    referenceTreatment = unname(baker$labels["Placebo"]), method = "derSimonianLaird", model = "component", componentSpecification = "indicatorVariables",
    componentVariables = baker$variables, rankingTable = TRUE
  )
  results <- do.call(nmaTestRun, c(list(data), options))
  expect_identical(results[["status"]], "complete")

  # netcomb() of the network meta-analysis of the contrasts with the C matrix of the indicators
  pairwise   <- suppressWarnings(meta::pairwise(
    treat = as.character(data$treatment), event = data$exac, n = data$total, studlab = as.character(data$study), sm = "OR", incr = 0.5, method.incr = "only0", allstudies = FALSE
  ))
  reference  <- unname(baker$labels["Placebo"])
  network    <- suppressWarnings(netmeta::netmeta(pairwise, common = FALSE, random = TRUE, method.tau = "DL", reference.group = reference, backtransf = FALSE))
  indicators <- unique(data[, c("treatment", baker$variables)])
  C          <- as.matrix(indicators[, baker$variables])
  rownames(C) <- as.character(indicators$treatment)
  C          <- C[network[["trts"]], , drop = FALSE]
  expect_identical(sum(rowSums(C) == 0), 1L)
  model      <- suppressWarnings(netmeta::netcomb(network, C.matrix = C, common = FALSE, random = TRUE))

  # the components are the indicator variables (the effects are relative to the treatment without active components)
  nmaTestAgreeComponents(results, model, "random")

  # the treatment effects versus the reference treatment (the table of the component model: no prediction intervals)
  effects <- nmaTestEffectsTable(results)
  others  <- setdiff(model[["trts"]], reference)
  expect_setequal(effects$comparison, others)
  for (pair in list(c("est", "TE"), c("se", "seTE"), c("lCi", "lower"), c("uCi", "upper"), c("stat", "statistic"), c("pval", "pval")))
    nmaTestAgree(effects[[pair[1]]][match(others, effects$comparison)], model[[paste0(pair[2], ".random")]][others, reference], 1e-10, info = pair[1])
  expect_false(any(c("lPi", "uPi", "df") %in% colnames(effects)))

  # the tests of the additive and the standard model (the statistics are printed with two decimals), the heterogeneity of the additive model
  tests <- nmaTestTable(results, "modelSummaryContainer", "modelSummaryContainer_testsTable")
  expect_identical(tests$test, c("Additive model", "Standard model", "Difference"))
  q <- lapply(tests$stat, nmaTestNumbers)
  nmaTestAgree(vapply(q, `[`, numeric(1), 2), c(model[["Q.additive"]], model[["Q.standard"]], model[["Q.diff"]]), 0.005 + 1e-9)
  nmaTestAgree(vapply(q, `[`, numeric(1), 1), c(model[["df.Q.additive"]], model[["df.Q.standard"]], model[["df.Q.diff"]]), 1e-12)
  nmaTestAgree(tests$pval, c(model[["pval.Q.additive"]], model[["pval.Q.standard"]], model[["pval.Q.diff"]]), 1e-10)
  heterogeneity <- nmaTestTable(results, "modelSummaryContainer", "modelSummaryContainer_heterogeneityTable")
  nmaTestAgree(heterogeneity$est, c(model[["tau"]], model[["tau"]]^2, 100 * model[["I2"]]), 1e-10)

  # the P-scores of the component model
  ranking <- nmaTestTable(results, "rankingContainer", "rankingContainer_rankingTable")
  nmaTestAgree(ranking$pScore, netmeta::netrank(model, small.values = "desirable")[["ranking.random"]][ranking$treatment], 1e-10)

  # the note on the inactive treatment (the treatment without active components)
  expect_true(any(grepl(paste("inactive treatment", reference), nmaTestFootnotes(results, "treatmentEffectsContainer", "treatmentEffectsContainer_componentEffectsTable"), fixed = TRUE)))

  # (the treatment names have no components: the separator of the names would give the treatments as components of their own)
  separator <- do.call(nmaTestRun, c(list(data), options[!names(options) %in% c("componentSpecification", "componentVariables")], list(componentSpecification = "separator", componentInactiveTreatment = reference)))
  expect_gt(length(nmaTestComponentTable(separator)$component), length(baker$variables))
})

test_that("with indicators, a subgroup without a component is fitted without it and the note names the dropped indicators", {

  data    <- nmaTestIndicatorData()
  results <- do.call(nmaTestRun, c(list(data), nmaTestIndicatorOptions, list(subgroup = "group", includeFullDatasetInSubgroupAnalysis = TRUE)))
  expect_identical(results[["status"]], "complete")

  components <- nmaTestComponentTable(results)

  # g1 has no treatment with cC: the model has the components cA and cB; the other scopes have all three
  expect_equal(components$component[components$subgroup == "g1"], c("cA", "cB"))
  expect_equal(components$component[components$subgroup == "g2"], c("cA", "cB", "cC"))
  expect_equal(components$component[components$subgroup == "Full dataset"], c("cA", "cB", "cC"))

  # the estimates equal netcomb() with the indicators of the subgroup
  for (scope in c("g1", "g2")) {
    model <- nmaTestIndicatorModel(droplevels(data[data$group == scope, ]), if (scope == "g1") c("cA", "cB") else c("cA", "cB", "cC"))
    rows  <- components[components$subgroup == scope, ]
    expect_equal(rows$component, model[["comps"]], info = scope)
    expect_lt(max(abs(rows$est - model[["Comp.random"]])), 1e-10)
    expect_lt(max(abs(rows$se  - model[["seComp.random"]])), 1e-10)
  }

  # no subgroup fails; the note names the dropped indicator and the subgroup
  notes <- nmaTestFootnotes(results, "modelSummaryContainer", "modelSummaryContainer_testsTable")
  expect_false(any(grepl("the model failed", notes, fixed = TRUE)))
  expect_true(any(grepl("'cC'", notes, fixed = TRUE) & grepl("Subgroup 'g1'", notes, fixed = TRUE) & grepl("not used as a component", notes, fixed = TRUE)))
  expect_identical(sum(grepl("not used as a component", notes, fixed = TRUE)), 1L)
})

test_that("an interaction applies to every subgroup with a treatment that contains all its components", {

  # G1 contains the treatment A+B; G2 contains A+B+C, not A+B (A, B, C: 2; A+B: 7; A+B+C: 10 = 6 + 4)
  data    <- nmaTestComponentSubgroups(list(G1 = c(P = 0, A = 2, B = 2, "A+B" = 7), G2 = c(P = 0, A = 2, B = 2, C = 2, "A+B+C" = 10)))
  results <- do.call(nmaTestRun, c(list(data), nmaTestDisconnectedOptions[c("studyId", "model")], list(
    effectSize = "TE", effectSizeStandardError = "seTE", treatment1 = "treat1", treatment2 = "treat2", componentInactiveTreatment = "P", referenceTreatment = "P",
    method = "equalEffects", subgroup = "group", componentInteractions = list(list(value = list("A+B"), include = TRUE))
  )))
  expect_identical(results[["status"]], "complete")

  components <- nmaTestComponentTable(results)
  expect_setequal(components$component[components$subgroup == "G1"], c("A", "B", "A * B"))
  expect_setequal(components$component[components$subgroup == "G2"], c("A", "B", "C", "A * B"))

  # G2 equals netcomb() with the product column
  g2      <- droplevels(data[data$group == "G2", ])
  network <- suppressWarnings(netmeta::netmeta(g2$TE, g2$seTE, as.character(g2$treat1), as.character(g2$treat2), as.character(g2$study),
                                               common = TRUE, random = FALSE, reference.group = "P", backtransf = FALSE))
  C       <- t(vapply(strsplit(network[["trts"]], "+", fixed = TRUE), function(parts) as.numeric(c("A", "B", "C") %in% parts), numeric(3)))
  dimnames(C) <- list(network[["trts"]], c("A", "B", "C"))
  C       <- cbind(C, "A * B" = C[, "A"] * C[, "B"])
  model   <- suppressWarnings(netmeta::netcomb(network, C.matrix = C, common = TRUE, random = FALSE))

  rows <- components[components$subgroup == "G2", ]
  expect_setequal(rows$component, model[["comps"]])
  rows <- rows[match(model[["comps"]], rows$component), ]
  expect_lt(max(abs(rows$est - model[["Comp.common"]])), 1e-8)
  expect_lt(abs(rows$est[rows$component == "A * B"] - 4), 1e-8)

  tests <- nmaTestTable(results, "modelSummaryContainer", "modelSummaryContainer_testsTable")
  tests$test <- nmaTestFill(tests$test)
  expect_equal(sum(tests$subgroup == "G2" & tests$test == "Additive model"), 1)
  q     <- nmaTestNumbers(tests$stat[tests$subgroup == "G2" & tests$test == "Additive model"])
  expect_lt(abs(q[2] - model[["Q.additive"]]), 0.005 + 1e-9)
  expect_lt(q[2], 0.005)
})

test_that("the inactive component is not estimated in a subgroup without the inactive treatment", {

  # G1: P is inactive (P+A = A); G2 has no standalone P: P+A and P+B differ from A and B by 0.6 in the data
  set.seed(6)
  data    <- nmaTestComponentSubgroups(list(G1 = c(P = 0, A = 1, B = 2, "P+A" = 1, "P+B" = 2), G2 = c(A = 1, B = 2, "P+A" = 1.6, "P+B" = 2.6)), noise = 0.05)
  results <- do.call(nmaTestRun, c(list(data), nmaTestDisconnectedOptions[c("studyId", "model")], list(
    effectSize = "TE", effectSizeStandardError = "seTE", treatment1 = "treat1", treatment2 = "treat2", componentInactiveTreatment = "P",
    method = "equalEffects", subgroup = "group", treatmentEffectsComparisons = "allPairwise"
  )))
  expect_identical(results[["status"]], "complete")

  components <- nmaTestComponentTable(results)
  expect_equal(components$component[components$subgroup == "G1"], c("A", "B"))
  expect_equal(components$component[components$subgroup == "G2"], c("A", "B"))

  # P+A is A and P+B is B in G2
  effects <- nmaTestEffectsTable(results)
  effects$comparison <- nmaTestFill(effects$comparison)
  g2 <- effects[effects$subgroup == "G2", ]
  expect_equal(sum(g2$comparison == "A vs. P+A"), 1)
  expect_lt(abs(nmaTestNumeric(g2$est[g2$comparison == "A vs. P+A"])), 1e-10)
  expect_lt(abs(nmaTestNumeric(g2$est[g2$comparison == "B vs. P+B"])), 1e-10)
})

# Reference treatment and exclusions under the count-based pooling methods ----

test_that("the default reference treatment (the first treatment) outside the fitted network is replaced by a treatment of the network", {

  data <- nmaTestSparseDesignData()

  for (method in c("MH", "NCH")) {

    # the reference treatment of the form is never empty: it defaults to the first treatment (A)
    pooling <- c(MH = "mantelHaenszel", NCH = "nonCentralHypergeometric")[[method]]
    results <- do.call(nmaTestRun, c(list(data), nmaTestBinaryOptions, list(poolingMethod = pooling, method = "equalEffects", referenceTreatment = "A", showNetmetaRCode = TRUE)))
    expect_identical(results[["status"]], "complete", info = method)

    # netmetabin selects A as its reference treatment before it excludes the design A:B, and A is not in the network (B, C, D)
    default <- suppressWarnings(netmeta::netmetabin(nmaTestPairwise(data), method = method, common = TRUE, random = FALSE, backtransf = FALSE))
    expect_identical(default[["reference.group"]], "A", info = method)
    expect_false("A" %in% default[["trts"]], info = method)

    fit <- suppressWarnings(netmeta::netmetabin(nmaTestPairwise(data), method = method, common = TRUE, random = FALSE, reference.group = "B", backtransf = FALSE))
    expect_identical(fit[["trts"]], c("B", "C", "D"), info = method)

    effects <- nmaTestEffectsTable(results)
    expect_identical(nmaTestEffectsTitle(results), "Treatment Effects vs. B", info = method)
    expect_setequal(effects$comparison, c("C", "D"))
    expect_false(anyNA(effects$est), info = method)
    expect_equal(effects$est[match(c("C", "D"), effects$comparison)], unname(fit$TE.common[c("C", "D"), "B"]), tolerance = 1e-10, info = method)

    # the title names the reference treatment of the network, and a note names the replaced one
    expect_true("Reference treatment 'A' is not part of the analysed network; 'B' is used as reference." %in% nmaTestFootnotes(results, "treatmentEffectsContainer", "treatmentEffectsContainer_treatmentEffectsTable"), info = method)

    # the printed code fits the same network
    code <- nmaTestCode(results)
    expect_true(grepl("reference.group = 'B'", code, fixed = TRUE), info = method)
    script <- nmaTestEvaluate(code, data)
    expect_identical(script[["fit"]][["reference.group"]], "B", info = method)
    expect_equal(script[["fit"]][["TE.common"]], fit[["TE.common"]], tolerance = 1e-10, info = method)
  }
})

test_that("penalized logistic regression keeps a reference treatment that only studies without events contain", {

  # P occurs in the studies s1 and s4 without events only
  data <- data.frame(
    study     = factor(c("s1", "s1", "s2", "s2", "s3", "s3", "s4", "s4", "s5", "s5")),
    treatment = factor(c("P", "A", "A", "B", "A", "B", "P", "B", "A", "B")),
    events    = c(0, 0, 5, 8, 6, 4, 0, 0, 7, 9),
    n         = c(50, 50, 50, 50, 50, 50, 40, 40, 60, 60)
  )
  footnote <- function(results) nmaTestFootnotes(results, "treatmentEffectsContainer", "treatmentEffectsContainer_treatmentEffectsTable")

  # penalized logistic regression keeps all studies: the effects are versus P, without a note on the reference treatment
  results <- do.call(nmaTestRun, c(list(data), nmaTestBinaryOptions, list(poolingMethod = "penalizedLogisticRegression", method = "equalEffects", referenceTreatment = "P")))
  expect_identical(results[["status"]], "complete")
  fit     <- suppressWarnings(netmeta::netmetabin(nmaTestPairwise(data), method = "LRP", common = TRUE, random = FALSE, reference.group = "P", backtransf = FALSE))
  effects <- nmaTestEffectsTable(results)
  expect_identical(nmaTestEffectsTitle(results), "Treatment Effects vs. P")
  expect_setequal(effects$comparison, c("A", "B"))
  expect_equal(effects$est[match(c("A", "B"), effects$comparison)], unname(fit$TE.common[c("A", "B"), "P"]), tolerance = 1e-10)
  expect_false(any(grepl("not part of the analysed network", footnote(results), fixed = TRUE)))

  # Mantel-Haenszel excludes the studies without events: P is not in the network and the netmeta default is used
  results <- do.call(nmaTestRun, c(list(data), nmaTestBinaryOptions, list(poolingMethod = "mantelHaenszel", method = "equalEffects", referenceTreatment = "P")))
  expect_identical(results[["status"]], "complete")
  expect_identical(nmaTestEffectsTitle(results), "Treatment Effects vs. A")
  expect_true("Reference treatment 'P' is not part of the analysed network; 'A' is used as reference." %in% footnote(results))
})

test_that("a network that the exclusion of designs disconnects ends in the message of the module", {

  # s3 is the only link between {A, B} (s1, s4) and {C, D} (s2, s5): B has no events in s3, and the design B vs. C is excluded
  data <- data.frame(
    study     = rep(paste0("s", 1:5), each = 2),
    treatment = c("A", "B", "C", "D", "B", "C", "A", "B", "C", "D"),
    events    = c(3, 6, 4, 8, 0, 5, 2, 5, 6, 9),
    n         = c(50, 50, 60, 60, 70, 70, 40, 40, 55, 55),
    stringsAsFactors = FALSE
  )
  # the same with no events in both arms of s3 (the study is excluded)
  noEvents <- data
  noEvents$events[5:6] <- 0

  for (pooling in c("mantelHaenszel", "nonCentralHypergeometric")) {
    for (scenario in list(list(data = data, excluded = "design 'B:C'"), list(data = noEvents, excluded = "Study 's3' without any events"))) {

      results <- do.call(nmaTestRun, c(list(scenario$data), nmaTestBinaryOptions, list(poolingMethod = pooling, method = "equalEffects")))
      info    <- paste(pooling, scenario$excluded)
      expect_identical(results[["status"]], "complete", info = info)

      message <- nmaTestError(results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]])
      expect_false(is.null(message), info = info)
      expect_true(grepl("the exclusions can disconnect the network", message, fixed = TRUE), info = info)
      expect_true(grepl("Penalized logistic regression keeps all studies and designs.", message, fixed = TRUE), info = info)
      expect_true(grepl(scenario$excluded, message, fixed = TRUE), info = info)

      # the original error of netmetabin() follows the exclusions (it is matched by its text, not by its words)
      error <- tryCatch(
        suppressWarnings(netmeta::netmetabin(nmaTestPairwise(scenario$data), method = c(mantelHaenszel = "MH", nonCentralHypergeometric = "NCH")[[pooling]], common = TRUE, random = FALSE, backtransf = FALSE)),
        error = function(e) conditionMessage(e)
      )
      expect_true(is.character(error), info = info)
      expect_true(grepl(error, message, fixed = TRUE), info = info)
    }
  }
})

# Networks of two treatments ----

test_that("for a network of two treatments the resampled ranking metrics and the rankogram are not available and the P-scores remain", {

  data <- data.frame(TE = c(0.2, 0.4, 0.1), seTE = c(0.1, 0.2, 0.15), treat1 = "A", treat2 = "B", study = factor(paste0("s", 1:3)))
  options <- list(
    studyId = "study", effectSize = "TE", effectSizeStandardError = "seTE", treatment1 = "treat1", treatment2 = "treat2", method = "derSimonianLaird",
    rankingTable = TRUE, rankingPScore = TRUE, rankingSucra = TRUE, rankingProbabilityOfBeingBest = TRUE, rankingMeanRank = TRUE, rankingMedianRank = TRUE,
    rankogram = TRUE
  )
  message <- "Not available for a network of two treatments."

  results <- do.call(nmaTestRun, c(list(data), options))
  expect_identical(results[["status"]], "complete")

  ranking <- results[["results"]][["rankingContainer"]][["collection"]]
  table   <- nmaTestTable(results, "rankingContainer", "rankingContainer_rankingTable")
  fit     <- suppressWarnings(netmeta::netmeta(
    TE = data$TE, seTE = data$seTE, treat1 = data$treat1, treat2 = data$treat2, studlab = data$study, common = FALSE, random = TRUE, method.tau = "DL", backtransf = FALSE
  ))
  pScores <- netmeta::netrank(fit, method = "P-score", small.values = "desirable")$ranking.random

  expect_equal(table$pScore[match(c("A", "B"), table$treatment)], unname(pScores[c("A", "B")]), tolerance = 1e-10)
  # (the columns of the metrics are declared and empty)
  expect_false(any(c("sucra", "pBest", "meanRank", "medianRank") %in% nmaTestFields(ranking[["rankingContainer_rankingTable"]])))
  expect_true(message %in% nmaTestFootnotes(results, "rankingContainer", "rankingContainer_rankingTable"))
  expect_identical(nmaTestError(ranking[["rankingContainer_rankogram"]]), message)

  # a subgroup of two treatments does not remove the metrics of a subgroup of three treatments
  data <- data.frame(
    TE = c(0.2, 0.4, 0.1, 0.3, 0.5, 0.2), seTE = c(0.1, 0.2, 0.15, 0.2, 0.2, 0.1), treat1 = c("A", "A", "B", "A", "A", "A"), treat2 = c("B", "C", "C", "B", "B", "B"),
    study = factor(paste0("s", 1:6)), group = factor(rep(c("G1", "G2"), each = 3))
  )
  results <- do.call(nmaTestRun, c(list(data), options, list(subgroup = "group")))
  expect_identical(results[["status"]], "complete")
  table   <- nmaTestTable(results, "rankingContainer", "rankingContainer_rankingTable")
  table[["subgroup"]] <- nmaTestFill(table[["subgroup"]])
  expect_false(anyNA(table[table$subgroup == "G1", c("pScore", "sucra", "pBest", "meanRank", "medianRank")]))
  expect_false(anyNA(table[table$subgroup == "G2", "pScore"]))
  expect_true(all(is.na(table[table$subgroup == "G2", "sucra"])))
  expect_true(any(grepl(message, nmaTestFootnotes(results, "rankingContainer", "rankingContainer_rankingTable"), fixed = TRUE)))
  rankograms <- results[["results"]][["rankingContainer"]][["collection"]][["rankingContainer_rankogram"]][["collection"]]
  expect_null(nmaTestError(rankograms[["rankingContainer_rankogram_subgroupG1"]]))
  expect_identical(nmaTestError(rankograms[["rankingContainer_rankogram_subgroupG2"]]), message)
})

test_that("the direct evidence of a network of two treatments is reported and a failing subgroup does not remove the others", {

  data    <- data.frame(TE = c(0.2, 0.4, 0.1), seTE = c(0.1, 0.2, 0.15), treat1 = "A", treat2 = "B", study = factor(paste0("s", 1:3)))
  options <- list(studyId = "study", effectSize = "TE", effectSizeStandardError = "seTE", treatment1 = "treat1", treatment2 = "treat2", method = "derSimonianLaird", directEvidence = TRUE)

  results <- do.call(nmaTestRun, c(list(data), options))
  expect_identical(results[["status"]], "complete")
  table   <- nmaTestTable(results, "evidenceContainer", "evidenceContainer_directEvidenceTable")
  expect_null(nmaTestError(results[["results"]][["evidenceContainer"]][["collection"]][["evidenceContainer_directEvidenceTable"]]))
  expect_identical(nrow(table), 1L)
  expect_equal(table$proportion, 1)
  expect_equal(table$meanPath, 1)
  expect_null(table[["minPar"]])

  # one subgroup of three treatments, one of two treatments
  data <- data.frame(
    TE = c(0.2, 0.4, 0.1, 0.3, 0.5, 0.2), seTE = c(0.1, 0.2, 0.15, 0.2, 0.2, 0.1), treat1 = c("A", "A", "B", "A", "A", "A"), treat2 = c("B", "C", "C", "B", "B", "B"),
    study = factor(paste0("s", 1:6)), group = factor(rep(c("G1", "G2"), each = 3))
  )
  results <- do.call(nmaTestRun, c(list(data), options, list(subgroup = "group")))
  expect_identical(results[["status"]], "complete")
  expect_null(nmaTestError(results[["results"]][["evidenceContainer"]][["collection"]][["evidenceContainer_directEvidenceTable"]]))
  table <- nmaTestTable(results, "evidenceContainer", "evidenceContainer_directEvidenceTable")
  table[["subgroup"]] <- nmaTestFill(table[["subgroup"]])
  expect_setequal(unique(table$subgroup), c("G1", "G2"))
  expect_identical(sum(table$subgroup == "G2"), 1L)

  # the rows of the subgroup of three treatments equal netmeasures() of its network meta-analysis
  first    <- data[data$group == "G1", ]
  network  <- suppressWarnings(netmeta::netmeta(
    TE = first$TE, seTE = first$seTE, treat1 = first$treat1, treat2 = first$treat2, studlab = first$study, common = FALSE, random = TRUE, method.tau = "DL", backtransf = FALSE
  ))
  measures <- netmeta::netmeasures(network, random = TRUE)
  rows     <- table[table$subgroup == "G1", ]
  labels   <- sub(":", " vs. ", names(measures$proportion), fixed = TRUE)
  expect_setequal(rows$comparison, labels)
  expect_equal(rows$proportion[match(labels, rows$comparison)], unname(measures$proportion), tolerance = 1e-10)
  expect_equal(rows$meanPath[match(labels, rows$comparison)],   unname(measures$meanpath),   tolerance = 1e-10)
  expect_equal(rows$minPar[match(labels, rows$comparison)],     unname(measures$minpar),     tolerance = 1e-10)
})

test_that("penalized logistic regression of a network of two treatments ends in the message of the module, other scopes are not affected", {

  message <- "Penalized logistic regression requires at least three treatments."
  options <- c(nmaTestBinaryOptions, list(poolingMethod = "penalizedLogisticRegression", method = "equalEffects"))
  notes   <- function(results) nmaTestFootnotes(results, "modelSummaryContainer", "modelSummaryContainer_testsTable")

  # two treatments (the raw error of netmetabin(): "attempt to set 'rownames' on an object with no dimensions")
  data    <- data.frame(study = factor(rep(paste0("S", 1:4), each = 2)), treatment = factor(rep(c("A", "B"), 4)), events = c(5, 9, 8, 12, 3, 7, 10, 15), n = c(50, 50, 60, 60, 40, 40, 80, 80))
  results <- do.call(nmaTestRun, c(list(data), options))
  expect_identical(results[["status"]], "complete")
  expect_identical(nmaTestError(results[["results"]][["modelSummaryContainer"]][["collection"]][["modelSummaryContainer_testsTable"]]), message)

  # a subgroup of three and a subgroup of two treatments
  three <- nmaTestLrpData()
  three$group <- "G1"
  two   <- data.frame(study = factor(rep(paste0("t", 1:4), each = 2)), treatment = factor(rep(c("A", "B"), 4)), events = c(5, 9, 8, 12, 3, 7, 10, 15), n = c(50, 50, 60, 60, 40, 40, 80, 80), group = "G2")
  both  <- rbind(transform(three, study = as.character(study), treatment = as.character(treatment)), transform(two, study = as.character(study), treatment = as.character(treatment)))
  for (variable in c("study", "treatment", "group"))
    both[[variable]] <- factor(both[[variable]])

  results <- do.call(nmaTestRun, c(list(both), options, list(subgroup = "group", referenceTreatment = "A")))
  expect_identical(results[["status"]], "complete")
  expect_true(paste0("Subgroup 'G2': the model failed with the following error: ", message) %in% notes(results))

  effects            <- nmaTestEffectsTable(results)
  effects$subgroup   <- nmaTestFill(effects$subgroup)
  fit <- suppressWarnings(netmeta::netmetabin(nmaTestPairwise(three), method = "LRP", common = TRUE, random = FALSE, reference.group = "A", backtransf = FALSE))
  rows <- effects[effects$subgroup == "G1", ]
  expect_false(any(effects$subgroup == "G2"))
  expect_equal(rows$est[match(c("B", "C"), nmaTestFill(rows$comparison))], unname(fit$TE.common[c("B", "C"), "A"]), tolerance = 1e-10)
})

# Messages ----

test_that("each validation stop of the data checks ends the analysis with its message", {

  contrast <- nmaTestValidationContrast()
  arms     <- nmaTestValidationArms()
  options  <- nmaTestSennOptions[c("studyId", "effectSize", "effectSizeStandardError", "treatment1", "treatment2")]
  change   <- function(data, variable, row, value) { data[[variable]][row] <- value; data }
  stopMessage <- function(data, base, ...) {
    results <- do.call(nmaTestRun, c(list(data), base, list(...)))
    expect_identical(results[["status"]], "validationError", info = paste(unlist(list(...)), collapse = " "))
    results[["results"]][["errorMessage"]]
  }

  # the fixed between-study variance (a FormulaField: a positive number is required); the form offers the method of the random effects
  # (dispersion) only with penalized logistic regression, so no options of the form reach the check of the method in R
  for (value in c("-1", "abc", "c(1, 2)"))
    expect_identical(stopMessage(contrast, options, method = "derSimonianLaird", fixParametersTau2 = TRUE, fixParametersTau2Value = value), "The fixed value for tau2 must be a positive number.", info = value)
  results <- do.call(nmaTestRun, c(list(contrast), options, list(method = "derSimonianLaird", fixParametersTau2 = TRUE, fixParametersTau2Value = "0.1")))
  expect_identical(results[["status"]], "complete")
  expect_true(any(grepl("fixed to 0.1", nmaTestFootnotes(results, "modelSummaryContainer", "modelSummaryContainer_heterogeneityTable"), fixed = TRUE)))

  # infinite values and too few observations (the messages of the checks of the module framework)
  expect_true(grepl("Infinity found in TE", stopMessage(change(contrast, "TE", 2, Inf), options), fixed = TRUE))
  expect_true(grepl("Infinity found in seTE", stopMessage(change(contrast, "seTE", 2, Inf), options), fixed = TRUE))
  expect_true(grepl("Number of observations is < 2 in TE, seTE", stopMessage(contrast[1, ], options), fixed = TRUE))

  # the ranges of the variables of each data format and outcome type (positive: the standard errors, the sample sizes, the person-time, and the
  # standard deviations of the standardized mean difference; non-negative: the events and the standard deviations of the mean difference)
  positive    <- function(variable, value, study) sprintf("The variable '%s' must contain positive values, but it contains %s in the study '%s'.", variable, value, study)
  nonNegative <- function(variable, value, study) sprintf("The variable '%s' must contain non-negative values, but it contains %s in the study '%s'.", variable, value, study)

  expect_identical(stopMessage(change(contrast, "seTE", 2, 0), options), positive("seTE", "0", "s2"))
  expect_identical(stopMessage(change(contrast, "seTE", 3, -0.1), options), positive("seTE", "-0.1", "s3"))
  expect_identical(stopMessage(change(arms, "events", 1, -1), nmaTestBinaryOptions), nonNegative("events", "-1", "s1"))
  expect_identical(stopMessage(change(arms, "n", 2, 0), nmaTestBinaryOptions), positive("n", "0", "s1"))
  continuous <- nmaTestArmOptions$continuous
  expect_identical(stopMessage(change(arms, "sd", 3, -1), continuous, effectSizeMeasure = "MD"), nonNegative("sd", "-1", "s2"))
  expect_identical(stopMessage(change(arms, "sd", 3, -1), continuous, effectSizeMeasure = "SMD"), positive("sd", "-1", "s2"))
  expect_identical(stopMessage(change(arms, "sd", 3, 0), continuous, effectSizeMeasure = "SMD"), positive("sd", "0", "s2"))
  expect_identical(stopMessage(change(arms, "n", 4, 0), continuous, effectSizeMeasure = "MD"), positive("n", "0", "s2"))
  expect_identical(stopMessage(change(arms, "events", 1, -2), nmaTestArmOptions$incidenceRate, effectSizeMeasure = "IRR"), nonNegative("events", "-2", "s1"))
  expect_identical(stopMessage(change(arms, "time", 2, 0), nmaTestArmOptions$incidenceRate, effectSizeMeasure = "IRR"), positive("time", "0", "s1"))
  expect_identical(stopMessage(change(arms, "se", 2, 0), nmaTestArmOptions$general, effectSizeMeasure = "general"), positive("se", "0", "s1"))

  # (a standard deviation of zero is accepted for the mean difference)
  results <- do.call(nmaTestRun, c(list(change(arms, "sd", 3, 0)), continuous, list(effectSizeMeasure = "MD")))
  expect_identical(results[["status"]], "complete")

  # more events than participants
  expect_identical(
    stopMessage(change(arms, "events", 2, 60), nmaTestBinaryOptions),
    "The number of events exceeds the sample size in the study 's1' (treatment 'B')."
  )

  # the treatments: one treatment only, a repeated arm, a comparison with itself
  same <- contrast
  same$treat1 <- factor("A")
  same$treat2 <- factor("A")
  expect_identical(stopMessage(same, options), "The network meta-analysis requires at least two different treatments.")
  expect_identical(
    stopMessage(change(arms, "treatment", 2, "A"), nmaTestBinaryOptions),
    "The study 's1' contains the treatment 'A' in more than one arm. Each treatment can appear in a study only once."
  )
  self <- data.frame(TE = c(0.2, 0.4), seTE = c(0.1, 0.2), treat1 = factor(c("A", "A")), treat2 = factor(c("A", "C")), study = factor(c("s1", "s2")))
  expect_identical(
    stopMessage(self, options),
    "The study 's1' contains a comparison of the treatment 'A' with itself. The two treatments of a comparison must differ."
  )

  # the predictor and the subgroup are study-level variables (here the variables differ within the study s1)
  varying <- data.frame(
    TE = c(0.2, 0.4, 0.1), seTE = c(0.1, 0.2, 0.15), treat1 = factor(c("A", "A", "B")), treat2 = factor(c("B", "C", "C")), study = factor(c("s1", "s1", "s2")),
    x = c(1, 2, 3), g = factor(c("u", "v", "v"))
  )
  expect_identical(
    stopMessage(varying, options, predictor = "x", method = "restrictedML"),
    "The variable 'x' is not constant within the study 's1'. The predictor and the subgroup must be study-level variables."
  )
  expect_identical(
    stopMessage(varying, options, subgroup = "g"),
    "The variable 'g' is not constant within the study 's1'. The predictor and the subgroup must be study-level variables."
  )
})

test_that("the messages of netmeta() about multi-arm studies are rewritten in the terms of the module", {

  # contrast-based data: a study with two of the three comparisons of its three treatments and a study with two of the six comparisons of its four treatments
  options <- nmaTestSennOptions[c("studyId", "effectSize", "effectSizeStandardError", "treatment1", "treatment2")]
  build   <- function(treat1, treat2, study, TE = rep(0.2, length(study))) {
    data <- data.frame(TE = TE, seTE = 0.2, treat1 = treat1, treat2 = treat2, study = study, stringsAsFactors = FALSE)
    for (variable in c("treat1", "treat2", "study"))
      data[[variable]] <- factor(data[[variable]])
    data
  }
  raw <- function(data, ...) tryCatch(
    suppressWarnings(netmeta::netmeta(data$TE, data$seTE, as.character(data$treat1), as.character(data$treat2), as.character(data$study), common = FALSE, random = TRUE, method.tau = "DL", backtransf = FALSE, ...)),
    error = function(e) conditionMessage(e)
  )

  wrong   <- build(c("A", "A", "A", "C", "B", "C", "B"), c("B", "C", "B", "D", "C", "D", "D"), c("x1", "x1", "x2", "x2", "s3", "s4", "s5"))
  results <- do.call(nmaTestRun, c(list(wrong), options))
  expect_true(grepl("wrong number of comparisons", raw(wrong), fixed = TRUE))
  expect_identical(nmaTestFitError(results), paste(
    "Study 'x1' has a wrong number of comparisons: a study with 3 arms needs all 3 pairwise comparisons.",
    "Study 'x2' has a wrong number of comparisons: a study with 4 arms needs all 6 pairwise comparisons.", sep = "\n"
  ))

  # a study with three of the six comparisons of four treatments (an incomplete set of comparisons) and inconsistent treatment effects
  incomplete <- build(c("A", "A", "A", "B", "C"), c("B", "C", "D", "C", "D"), c("m1", "m1", "m1", "s2", "s3"))
  expect_true(grepl("incomplete set of comparisons", raw(incomplete), fixed = TRUE))
  expect_true(grepl("netmeta()", raw(incomplete), fixed = TRUE))
  results <- do.call(nmaTestRun, c(list(incomplete), options))
  expect_identical(nmaTestFitError(results), paste(
    "Problem in multi-arm studies!", "- Study 'm1' has an incomplete set of comparisons.", "Please check the data of the multi-arm studies.", sep = "\n"
  ))

  inconsistent <- build(c("A", "B", "A", "A", "B", "C"), c("B", "C", "C", "D", "D", "D"), c("m1", "m1", "m1", "s2", "s3", "s4"), TE = c(0.5, 0.5, 0.2, 0.1, 0.3, 0.2))
  expect_true(grepl("inconsistent treatment estimates", raw(inconsistent), fixed = TRUE))
  expect_true(grepl("netmeta()", raw(inconsistent), fixed = TRUE))
  results <- do.call(nmaTestRun, c(list(inconsistent), options))
  expect_identical(nmaTestFitError(results), paste(
    "Problem in multi-arm studies!", "- Study with inconsistent treatment estimates: 'm1'",
    "Please check the data of the multi-arm studies. The tolerance of the consistency check can be changed by the 'Multi-arm consistency tolerance' option in the 'Advanced' section.",
    sep = "\n"
  ))

  # the tolerance of the option accepts the inconsistency (netmeta() with the same tolerance fits the network)
  expect_s3_class(raw(inconsistent, tol.multiarm = 0.5), "netmeta")
  results <- do.call(nmaTestRun, c(list(inconsistent), options, list(advancedMultiArmTolerance = 0.5)))
  expect_null(nmaTestFitError(results))

  # arm-based data: multi-arm studies without an effect size of some of their comparisons (no events in two of their arms)
  arms <- data.frame(
    study = c("m1", "m1", "m1", "s2", "s2", "s3", "s3", "s4", "s4"), treatment = c("A", "B", "C", "A", "B", "A", "C", "B", "C"),
    events = c(5, 0, 0, 8, 12, 9, 14, 6, 11), n = 50, stringsAsFactors = FALSE
  )
  arms[c("study", "treatment")] <- lapply(arms[c("study", "treatment")], factor)
  results <- do.call(nmaTestRun, c(list(arms), nmaTestBinaryOptions))
  expect_identical(nmaTestFitError(results), paste(
    "The multi-arm study 'm1' has comparisons without an effect size (e.g., events in all or in none of the participants of two of its arms), but a multi-arm study needs all of its comparisons.",
    "Set 'Drop studies with no cases/events' in the 'Advanced' section to 'No' to keep these comparisons (with the continuity correction), or remove the study."
  ))

  more <- rbind(arms, data.frame(study = factor("m2"), treatment = factor(c("A", "B", "C")), events = c(0, 0, 4), n = 50))
  results <- do.call(nmaTestRun, c(list(more), nmaTestBinaryOptions))
  expect_true(grepl("The multi-arm studies 'm1', 'm2' have comparisons without an effect size", nmaTestFitError(results), fixed = TRUE))

  # the option named by the message keeps the comparisons: the network is fitted
  results <- do.call(nmaTestRun, c(list(arms), nmaTestBinaryOptions, list(advancedDropStudiesWithNoCasesOrEvents = "no")))
  expect_null(nmaTestFitError(results))
  expect_identical(results[["status"]], "complete")
})

# Network meta-regression ----

test_that("the R code of a failed network meta-regression is commented out with the message of the module and the script runs", {

  # the predictor is constant among the studies that include the reference treatment A in the subgroup Bad (but varies in the subgroup)
  data <- data.frame(
    study  = paste0("s", 1:8),
    treat1 = c("B", "C", "C", "B", "B", "C", "C", "B"),
    treat2 = c("A", "A", "B", "A", "A", "A", "B", "C"),
    TE     = c(0.30, 0.45, 0.12, 0.25, 0.35, 0.50, 0.10, -0.05),
    seTE   = c(0.15, 0.20, 0.18, 0.12, 0.14, 0.16, 0.20, 0.17),
    g      = c(rep("Bad", 3), rep("Good", 5)),
    x      = c(0, 0, 1, 0, 1, 0.5, 1, 0)
  )
  for (variable in c("study", "treat1", "treat2", "g"))
    data[[variable]] <- factor(data[[variable]], levels = unique(data[[variable]]))

  results <- do.call(nmaTestRun, c(list(data), nmaTestSyntheticOptions[c("studyId", "effectSize", "effectSizeStandardError", "treatment1", "treatment2")], list(
    referenceTreatment = "A", method = "equalEffects", subgroup = "g", predictor = "x", showNetmetaRCode = TRUE
  )))
  expect_identical(results[["status"]], "complete")

  # the table reports the failure
  coefficients <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_metaregressionCoefficientsTable"]]
  expect_true(any(grepl("does not vary among the studies that include the reference treatment", vapply(coefficients[["footnotes"]], `[[`, character(1), "text"), fixed = TRUE)))

  # the call is a comment followed by the message of the module
  code  <- nmaTestCode(results)
  lines <- strsplit(code, "\n")[[1]]
  expect_true(any(grepl("^# metareg <- netmeta::netmetareg[(]$", lines)))
  expect_true(any(grepl("^# The module reported: The predictor does not vary among the studies that include the reference treatment 'A'", lines)))

  # the script runs to the end: both subgroups are collected, the meta-regression of the subgroup Good is fitted
  script <- nmaTestEvaluate(code, data)
  expect_setequal(names(script[["subgroupFits"]]), c("Bad", "Good"))
  expect_s3_class(script[["metareg"]], "netmetareg")
})

test_that("the predictor of the network meta-regression is checked among the analysed comparisons, not among those that netmeta excluded", {

  # ten studies, s10 (C versus D) has no events in both arms: netmeta excludes it (the odds ratio of the inverse variance pooling drops it)
  data    <- nmaTestArmData()
  number  <- as.integer(sub("s", "", as.character(data$study)))
  options <- c(nmaTestArmOptions$binary, list(effectSizeMeasure = "OR", method = "restrictedML", referenceTreatment = "A", treatmentEffects = FALSE))
  coefficients <- function(results) nmaTestTable(results, "metaregressionContainer", "metaregressionContainer_metaregressionCoefficientsTable")
  elements     <- function(results) results[["results"]][["metaregressionContainer"]][["collection"]]

  # the factor: L1 in the odd, L2 in the even studies, and L3 only in the excluded study s10 (a third level among the rows of the fit)
  data$mod <- factor(ifelse(number == 10, "L3", ifelse(number %% 2 == 1, "L1", "L2")))
  without  <- droplevels(data[data$study != "s10", ])

  results <- do.call(nmaTestRun, c(list(data), options, list(predictor = "mod", showNetmetaRCode = TRUE)))
  expect_identical(results[["status"]], "complete")
  expect_length(nmaTestAllErrors(elements(results)), 0)

  # the regression of the data without the excluded study (an independent pairwise(), netmeta(), and netmetareg())
  network    <- nmaTestArmNetwork(without, list(event = without$events, n = without$n, sm = "OR", incr = 0.5, method.incr = "only0", allstudies = FALSE))
  # (for a factor, netmetareg() reads the treatments of the comparisons from treat1 and treat2; the labels of the levels are the labels of the module)
  network[["data"]][["treat1"]]    <- network[["data"]][[".treat1"]]
  network[["data"]][["treat2"]]    <- network[["data"]][[".treat2"]]
  network[["data"]][["covariate"]] <- factor(without$mod[match(network[["data"]][[".studlab"]], without$study)], labels = c("level1", "level2"))
  regression <- suppressWarnings(netmeta::netmetareg(network, covar = covariate, assumption = "independent", method.tau = "REML", reference.group = "A"))
  table      <- coefficients(results)
  for (pair in list(c("est", "coef"), c("se", "se"), c("lCi", "lower"), c("uCi", "upper"), c("pval", "pval")))
    nmaTestAgree(table[[pair[1]]], regression[["results"]][[pair[2]]], 1e-8, info = pair[1])

  # the module without the study gives the same results
  reference <- do.call(nmaTestRun, c(list(without), options, list(predictor = "mod")))
  nmaTestAgree(table$est, coefficients(reference)$est, 1e-10)

  # the notes name the levels of the analysed studies, and the printed code reproduces the coefficients (without the level of the excluded study)
  notes <- nmaTestFootnotes(results, "metaregressionContainer", "metaregressionContainer_metaregressionCoefficientsTable")
  expect_true(any(grepl("mod = L1", notes, fixed = TRUE)))
  expect_false(any(grepl("L3", notes, fixed = TRUE)))
  code <- nmaTestCode(results)
  expect_false(grepl("L3", code, fixed = TRUE))
  script <- nmaTestEvaluate(code, data)
  expect_s3_class(script[["metareg"]], "netmetareg")
  nmaTestAgree(table$est, script[["metareg"]][["results"]][["coef"]], 1e-8)

  # a predictor that varies only in the excluded study has no variation among the analysed studies (the message of the module, not the error of netmetareg())
  message <- "The predictor has no variation among the studies."
  data$mod <- factor(ifelse(number == 10, "L2", "L1"))
  results  <- do.call(nmaTestRun, c(list(data), options, list(predictor = "mod", showNetmetaRCode = TRUE)))
  expect_identical(results[["status"]], "complete")
  expect_true(message %in% nmaTestAllErrors(elements(results)))
  expect_true(any(grepl(message, nmaTestCode(results), fixed = TRUE)))

  data$num <- ifelse(number == 10, 7, 0)
  results  <- do.call(nmaTestRun, c(list(data), options, list(predictor = "num")))
  expect_identical(results[["status"]], "complete")
  expect_true(message %in% nmaTestAllErrors(elements(results)))
})

test_that("the meta-regression tables have a note if netmeta treats the comparisons of a multi-arm study as independent", {

  # contrast-based data: no arm-level data, netmeta uses the diagonal covariance
  multiArm <- nmaTestMultiArmContrastData(c("G1", "G2"))
  nmaTestCovarianceCase(multiArm, c(nmaTestSyntheticOptions, list(predictor = "covariate")), multiArm = TRUE, note = TRUE, info = "contrast-based, multi-arm")

  # contrast-based data of two-arm studies only: nothing to treat as independent
  nmaTestCovarianceCase(nmaTestSynthetic(), c(nmaTestSyntheticOptions, list(predictor = "covariate")), multiArm = FALSE, note = FALSE, info = "contrast-based, two-arm")

  # a predictor with two levels (moderator: L1 and L2 in the subgroup G1)
  nmaTestCovarianceCase(droplevels(multiArm[multiArm$group == "G1", ]), c(nmaTestSyntheticOptions, list(predictor = "moderator")), multiArm = TRUE, note = TRUE, info = "contrast-based, multi-arm, factor")

  # arm-based data (nmaTestArmData() has the three-arm study s5): the arm-level data of the summary measures with a covariance formula in netmeta
  # (calcV) give the full covariance, the other inputs (ROM, incidence rates, general effect sizes) the diagonal matrix
  # (not whole numbers: a column of a few whole numbers is read as a nominal variable, and a predictor with three levels is not supported)
  data     <- nmaTestArmData()
  data$cov <- as.integer(sub("s", "", as.character(data$study))) %% 3 + 0.5
  reference <- list(referenceTreatment = "A", predictor = "cov")

  cases <- list(
    list(type = "binary", measure = "OR", note = FALSE), list(type = "binary", measure = "RR", note = FALSE),
    list(type = "binary", measure = "RD", note = FALSE), list(type = "binary", measure = "ASD", note = FALSE),
    list(type = "continuous", measure = "MD", note = FALSE), list(type = "continuous", measure = "SMD", note = FALSE),
    list(type = "continuous", measure = "ROM", note = TRUE),
    list(type = "incidenceRate", measure = "IRR", note = TRUE), list(type = "incidenceRate", measure = "IRD", note = TRUE),
    list(type = "incidenceRate", measure = "IRSD", note = TRUE),
    list(type = "general", measure = "general", note = TRUE)
  )

  for (case in cases) {
    measure <- if (case$type == "general") list() else list(effectSizeMeasure = case$measure)
    nmaTestCovarianceCase(data, c(nmaTestArmOptions[[case$type]], measure, reference), multiArm = TRUE, note = case$note, info = paste(case$type, case$measure))
  }

  # arm-based data without the multi-arm study: no note for any input
  twoArm <- droplevels(data[data$study != "s5", ])
  nmaTestCovarianceCase(twoArm, c(nmaTestArmOptions$incidenceRate, list(effectSizeMeasure = "IRR"), reference), multiArm = FALSE, note = FALSE, info = "incidence rate, two-arm")
  nmaTestCovarianceCase(twoArm, c(nmaTestArmOptions$general, reference), multiArm = FALSE, note = FALSE, info = "general, two-arm")
})

# Effect size measures without an example file ----

test_that("the effect size measures of continuous arm-level data equal pairwise() and netmeta()", {

  data <- nmaTestArmData()

  for (measure in c("MD", "SMD", "ROM")) {

    results <- do.call(nmaTestRun, c(list(data), nmaTestArmOptions$continuous, list(effectSizeMeasure = measure, method = "derSimonianLaird", referenceTreatment = "A")))
    network <- nmaTestArmNetwork(data, list(mean = data$mean, sd = data$sd, n = data$n, sm = measure))

    expect_identical(network[["sm"]], measure)
    nmaTestAgreeArmModel(results, network, measure)
  }
})

test_that("the effect size measures of incidence rates equal pairwise() and netmeta()", {

  data <- nmaTestArmData()

  for (measure in c("IRR", "IRD", "IRSD")) {

    results <- do.call(nmaTestRun, c(list(data), nmaTestArmOptions$incidenceRate, list(effectSizeMeasure = measure, method = "derSimonianLaird", referenceTreatment = "A")))
    network <- nmaTestArmNetwork(data, list(event = data$events, time = data$time, sm = measure, incr = 0.5, method.incr = "only0", allstudies = FALSE))

    expect_identical(network[["sm"]], measure)
    nmaTestAgreeArmModel(results, network, measure)
  }
})
