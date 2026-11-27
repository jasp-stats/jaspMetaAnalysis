# Classical network meta-analysis ranking.
#
# Builds the treatment ranking table (P-scores, and SUCRA, the probability of being best, the mean rank,
# and the median rank from resampling) and the rankogram from the netmeta fits.

# Dependencies ----

# options that define the rank probabilities (shared by the resampling state, the table, and the rankogram)
.nmaRankingDependencies <- c("beneficialEffect", "rankingNumberOfSimulations", "setSeed", "seed")

# the metrics of the ranking table from the rank probabilities (named by the columns of the table)
.nmaRankingResampledOptions <- c(
  sucra      = "rankingSucra",
  pBest      = "rankingProbabilityOfBeingBest",
  meanRank   = "rankingMeanRank",
  medianRank = "rankingMedianRank"
)

.nmaRankingResampling   <- function(options) {

  return(any(vapply(.nmaRankingResampledOptions, function(name) options[[name]], logical(1))))
}

# Entry ----

.nmaRankingOutputs      <- function(jaspResults, options) {

  # the rank probabilities (rankogram()) need the standard model and the inverse variance pooling: the component model and the count-based
  # pooling methods rank by the P-score only (the form disables them: resamplingAvailable in NetworkMetaAnalysisRanking.qml)
  if (.nmaIsComponentModel(options) || .nmaIsCountBased(options))
    options[c(.nmaRankingResampledOptions, "rankogram")] <- FALSE

  # the table needs at least one metric
  if (options[["rankingTable"]] && (options[["rankingPScore"]] || .nmaRankingResampling(options)))
    .nmaRankingTable(jaspResults, options)

  if (options[["rankogram"]])
    .nmaRankogram(jaspResults, options)

  return()
}

.nmaExtractRankingContainer <- function(jaspResults) {

  return(.nmaExtractOutputContainer(
    jaspResults, "rankingContainer", gettext("Treatment Ranking"), 6,
    c("rankingTable", "rankingPScore", .nmaRankingResampledOptions, "rankogram")
  ))
}

# Rank probabilities ----

.nmaRankogramState      <- function(jaspResults, options) {

  fits <- .nmaExtractFits(jaspResults, options)

  if (is.null(fits))
    return(NULL)

  # one simulation shared by the ranking table and the rankogram (rankogram() has no seed argument: the seed is set before the
  # simulation of each fit, so that the results of a subgroup do not depend on the other subgroups)
  return(.nmaExtractState(jaspResults, "rankogramState", .nmaRankingDependencies, function() {

    return(.nmaComputeResults(fits, function(fit) {
      .setSeedJASP(options)
      # (rankogram() fails for a network of two treatments)
      if (length(fit[["trts"]]) < 3)
        stop(gettext("Not available for a network of two treatments."), call. = FALSE)
      .nmaRunCall(.nmaCallRankogram(options, fit), fit)
    }, label = gettext("Simulating Rank Probabilities")))
  }))
}

# Ranking table ----

.nmaRankingTable        <- function(jaspResults, options) {

  rankingContainer <- .nmaExtractRankingContainer(jaspResults)

  if (!is.null(rankingContainer[["rankingTable"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  rankingTable          <- createJaspTable(gettext("Ranking of Treatments"))
  rankingTable$position <- 1
  rankingTable$showSpecifiedColumnsOnly <- TRUE
  rankingTable$dependOn(c("rankingTable", "rankingPScore", .nmaRankingResampledOptions, .nmaRankingDependencies))
  rankingContainer[["rankingTable"]] <- rankingTable

  rankingTable$addColumnInfo(name = "treatment", type = "string", title = gettext("Treatment"))
  .maAddSubgroupColumn(rankingTable, options)
  if (options[["rankingPScore"]])
    rankingTable$addColumnInfo(name = "pScore", type = "number", title = gettext("P-score"), format = "dp:3")
  # (the simulated metrics are not available for a network of two treatments: only the note is shown if no fit has them)
  resampled <- length(.nmaSuccessfulFits(fits)) == 0 || !all(vapply(.nmaSuccessfulFits(fits), .nmaIsTwoTreatmentFit, logical(1)))
  if (options[["rankingSucra"]] && resampled)
    rankingTable$addColumnInfo(name = "sucra",      type = "number", title = gettext("SUCRA"),       format = "dp:3")
  if (options[["rankingProbabilityOfBeingBest"]] && resampled)
    rankingTable$addColumnInfo(name = "pBest",      type = "number", title = gettext("P(best)"),     format = "dp:3")
  if (options[["rankingMeanRank"]] && resampled)
    rankingTable$addColumnInfo(name = "meanRank",   type = "number", title = gettext("Mean rank"),   format = "dp:2")
  if (options[["rankingMedianRank"]] && resampled)
    rankingTable$addColumnInfo(name = "medianRank", type = "number", title = gettext("Median rank"), format = "dp:1")

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    rankingTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  pScores    <- if (options[["rankingPScore"]]) .nmaComputeResults(fits, function(fit) {
    .nmaStopIfNotEstimable(fit)
    .nmaRunCall(.nmaCallNetrank(options), fit)
  })
  rankograms <- if (.nmaRankingResampling(options)) .nmaRankogramState(jaspResults, options)

  # the table fails when none of the requested metrics is available (the P-scores remain available when the resampling metrics are not)
  metrics <- Filter(Negate(is.null), list(pScores, rankograms))
  if (length(metrics) > 0 && all(vapply(metrics, function(metric) all(vapply(metric, jaspBase::isTryError, logical(1))), logical(1)))) {
    rankingTable$setError(.nmaFailureMessage(unlist(metrics, recursive = FALSE)))
    return()
  }

  # footnotes
  rankingTable$addFootnote(if (options[["beneficialEffect"]] == "negative") {
    gettext("Negative treatment effects are considered beneficial.")
  } else {
    gettext("Positive treatment effects are considered beneficial.")
  })
  if (.nmaRankingResampling(options) && length(.nmaSuccessfulResults(rankograms)) > 0)
    rankingTable$addFootnote(gettextf(
      "The ranking metrics other than the P-score are based on %1$s simulations of the treatment effects.",
      options[["rankingNumberOfSimulations"]]
    ))
  # (a subgroup without degrees of freedom for heterogeneity reports z-tests among t-tests: the notes name the subgroups they belong to)
  if (options[["rankingPScore"]]) {
    successful <- .nmaSuccessfulFits(fits)
    .nmaAddFitNote(
      rankingTable,
      gettext("The P-scores are based on the p-values of the normal distribution because the network has no degrees of freedom for heterogeneity (the t-distribution is not available)."),
      successful[vapply(successful, .nmaIsTestWithoutDf, logical(1))], fits, options
    )
  }
  if (length(metrics) > 0) {
    do.call(.nmaFailedResultFootnotes, c(list(rankingTable), metrics))
  } else {
    .nmaFailedFitFootnotes(rankingTable, fits)
  }

  rows <- .maSafeRbind(lapply(seq_along(fits), function(i) {
    .nmaRowsRanking(fits[[i]], pScores[[i]], rankograms[[i]])
  }))
  rows <- .maSafeOrderAndSimplify(rows, "subgroup", options)

  if (!is.null(rows))
    rankingTable$setData(rows)

  return()
}

.nmaRowsRanking         <- function(fit, pScore, rankogram) {

  # failed fits are reported by a footnote
  if (jaspBase::isTryError(fit))
    return(NULL)

  element    <- .nmaEffectsElement(fit)
  treatments <- fit[["trts"]]
  rows       <- data.frame(treatment = treatments, subgroup = attr(fit, "subgroup"))

  if (!is.null(pScore) && !jaspBase::isTryError(pScore))
    rows[["pScore"]] <- unname(pScore[[paste0("ranking.", element)]][treatments])

  if (!is.null(rankogram) && !jaspBase::isTryError(rankogram)) {
    rows[["sucra"]]      <- unname(rankogram[[paste0("ranking.", element)]][treatments])
    rows[["pBest"]]      <- unname(rankogram[[paste0("ranking.matrix.", element)]][treatments, 1])
    rows[["meanRank"]]   <- unname(rankogram[[paste0("meanranks.", element)]][treatments])
    rows[["medianRank"]] <- unname(rankogram[[paste0("medianranks.", element)]][treatments])
  }

  # the best treatment first
  score <- if (!is.null(rows[["pScore"]])) rows[["pScore"]] else rows[["sucra"]]
  if (!is.null(score))
    rows <- rows[order(-score), , drop = FALSE]

  return(rows)
}

# Rankogram ----

.nmaRankogram           <- function(jaspResults, options) {

  rankingContainer <- .nmaExtractRankingContainer(jaspResults)

  if (!is.null(rankingContainer[["rankogram"]]))
    return()

  fits         <- .nmaExtractFits(jaspResults, options)
  rankograms   <- .nmaRankogramState(jaspResults, options)
  dependencies <- c("rankogram", "rankogramCumulative", .nmaRankingDependencies)

  if (is.null(fits) || options[["subgroup"]] == "") {

    rankogramPlot          <- createJaspPlot(title = gettext("Rankogram"), width = 600, height = 400)
    rankogramPlot$position <- 2
    rankogramPlot$dependOn(dependencies)
    rankingContainer[["rankogram"]] <- rankogramPlot

    .nmaFillRankogram(rankogramPlot, rankograms[[1]], options)

  } else {

    rankogramContainer          <- createJaspContainer(gettext("Rankogram"))
    rankogramContainer$position <- 2
    rankogramContainer$dependOn(dependencies)
    rankingContainer[["rankogram"]] <- rankogramContainer

    for (i in seq_along(fits)) {
      rankogramPlot          <- createJaspPlot(title = .nmaScopeTitle(fits[[i]]), width = 600, height = 400)
      rankogramPlot$position <- i
      rankogramContainer[[names(fits)[i]]] <- rankogramPlot
      .nmaFillRankogram(rankogramPlot, rankograms[[i]], options)
    }
  }

  return()
}

.nmaFillRankogram       <- function(rankogramPlot, rankogram, options) {

  if (is.null(rankogram))
    return()

  if (jaspBase::isTryError(rankogram)) {
    rankogramPlot$setError(.maTryCleanErrorMessages(rankogram))
    return()
  }

  rankogramRender <- try(.nmaRankogramRender(rankogram, options), silent = TRUE)

  if (jaspBase::isTryError(rankogramRender)) {
    rankogramPlot$setError(.maTryCleanErrorMessages(rankogramRender))
    return()
  }

  rankogramPlot$width      <- rankogramRender[["width"]]
  rankogramPlot$height     <- rankogramRender[["height"]]
  rankogramPlot$plotObject <- rankogramRender[["plot"]]

  return()
}

# the ticks of the rank axis start at the first rank (at most 'maximum' ticks)
.nmaRankBreaks          <- function(ranks, maximum) {

  return(seq(1, ranks, by = max(1, ceiling((ranks - 1) / (maximum - 1)))))
}

.nmaRankogramRender     <- function(rankogram, options) {

  element       <- .nmaEffectsElement(rankogram)
  cumulative    <- options[["rankogramCumulative"]]
  probabilities <- rankogram[[paste0(if (cumulative) "cumrank.matrix." else "ranking.matrix.", element)]]
  treatments    <- rownames(probabilities)
  nTreatments   <- nrow(probabilities)

  data <- data.frame(
    treatment   = factor(rep(treatments, times = ncol(probabilities)), levels = treatments),
    rank        = rep(seq_len(ncol(probabilities)), each = nTreatments),
    probability = as.vector(probabilities)
  )

  if (cumulative) {

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = rank, y = probability, group = treatment, color = treatment)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_x_continuous(name = gettext("Rank"), breaks = .nmaRankBreaks(ncol(probabilities), 10)) +
      ggplot2::scale_y_continuous(name = gettext("Cumulative probability"), limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      .nmaTreatmentColors(nTreatments, "color") +
      ggplot2::labs(color = gettext("Treatment")) +
      jaspGraphs::geom_rangeframe(sides = "bl") +
      jaspGraphs::themeJaspRaw(legend.position = "right")

    return(list(plot = plot, width = 600, height = 400))
  }

  # one panel with the rank probabilities per treatment
  columns   <- min(nTreatments, 3)
  rows      <- ceiling(nTreatments / columns)
  yMaximum  <- min(1, max(0.1, ceiling(max(data$probability) * 10) / 10))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = rank, y = probability)) +
    ggplot2::geom_col(fill = "grey70", color = "black", linewidth = 0.3, width = 0.8) +
    ggplot2::facet_wrap(ggplot2::vars(treatment), ncol = columns) +
    ggplot2::scale_x_continuous(name = gettext("Rank"), breaks = .nmaRankBreaks(ncol(probabilities), 4)) +
    ggplot2::scale_y_continuous(name = gettext("Probability"), limits = c(0, yMaximum), breaks = pretty(c(0, yMaximum), n = 3)) +
    jaspGraphs::themeJaspRaw()

  return(list(plot = plot, width = 600, height = 90 + 150 * rows))
}
