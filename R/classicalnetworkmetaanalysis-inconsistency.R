# Classical network meta-analysis inconsistency.
#
# Builds the design decomposition tables, the node-splitting table and forest plot (direct versus
# indirect evidence), and the net heat plot from the netmeta fits.

# Entry ----

.nmaInconsistencyOutputs <- function(jaspResults, options) {

  # the form disables the design decomposition and the net heat plot for the count-based pooling methods ('enabled: !poolingMethod.countBased')
  # and the node splitting (and its forest plot) for penalized logistic regression ('enabled: !poolingMethod.dispersionModel')
  if (.nmaIsCountBased(options))
    options[c("designDecomposition", "netHeatPlot")] <- FALSE
  if (.nmaPoolingMethod(options) == "penalizedLogisticRegression")
    options[c("nodeSplitting", "nodeSplittingForestPlot")] <- FALSE

  if (options[["designDecomposition"]])
    .nmaDesignDecompositionTables(jaspResults, options)

  if (options[["nodeSplitting"]])
    .nmaNodeSplittingTable(jaspResults, options)

  if (options[["nodeSplitting"]] && options[["nodeSplittingForestPlot"]])
    .nmaNodeSplittingForestPlot(jaspResults, options)

  if (options[["netHeatPlot"]])
    .nmaNetHeatPlot(jaspResults, options)

  return()
}

.nmaExtractInconsistencyContainer <- function(jaspResults) {

  return(.nmaExtractOutputContainer(
    jaspResults, "inconsistencyContainer", gettext("Inconsistency"), 7,
    c("designDecomposition", "nodeSplitting", "nodeSplittingForestPlot", "netHeatPlot")
  ))
}

# Design decomposition ----

.nmaDesignDecompositionState <- function(jaspResults, options) {

  fits <- .nmaExtractFits(jaspResults, options)

  if (is.null(fits))
    return(NULL)

  return(.nmaExtractState(jaspResults, "designDecompositionState", NULL, function() {
    .nmaComputeResults(fits, function(fit) .nmaRunCall(.nmaCallDecompDesign(fit), fit))
  }))
}

.nmaDesignDecompositionTables <- function(jaspResults, options) {

  inconsistencyContainer <- .nmaExtractInconsistencyContainer(jaspResults)

  # the two tables share the decomposition
  tables <- list(
    designDecompositionWithinTable = list(title = gettext("Within-Design Heterogeneity"),                                  column = gettext("Design"),          element = "Q.het.design", position = 1),
    designDecompositionDetachTable = list(title = gettext("Between-Designs Inconsistency After Detaching Single Designs"), column = gettext("Detached design"), element = "Q.inc.detach", position = 2)
  )

  for (key in names(tables)) {

    if (!is.null(inconsistencyContainer[[key]]))
      next

    fits <- .nmaExtractFits(jaspResults, options)

    decompositionTable          <- createJaspTable(tables[[key]][["title"]])
    decompositionTable$position <- tables[[key]][["position"]]
    decompositionTable$showSpecifiedColumnsOnly <- TRUE
    decompositionTable$dependOn("designDecomposition")
    inconsistencyContainer[[key]] <- decompositionTable

    decompositionTable$addColumnInfo(name = "design", type = "string",  title = tables[[key]][["column"]])
    .maAddSubgroupColumn(decompositionTable, options)
    decompositionTable$addColumnInfo(name = "Q",    type = "number",  title = "Q")
    decompositionTable$addColumnInfo(name = "df",   type = "integer", title = "df")
    decompositionTable$addColumnInfo(name = "pval", type = "pvalue",  title = gettext("p"))

    if (is.null(fits))
      next

    if (.nmaIsFailedSingleFit(fits)) {
      decompositionTable$setError(.maTryCleanErrorMessages(fits[[1]]))
      next
    }

    decompositions <- .nmaDesignDecompositionState(jaspResults, options)

    if (.nmaReportFailedResults(decompositionTable, decompositions))
      next

    rows <- .maSafeRbind(lapply(decompositions, .nmaRowsDesignDecomposition, element = tables[[key]][["element"]]))
    rows <- .maSafeOrderAndSimplify(rows, "design", options)

    # (decomp.design() provides no decomposition for a network of two treatments)
    successful    <- .nmaSuccessfulFits(fits)
    twoTreatments <- successful[vapply(successful, function(fit) length(fit[["trts"]]) < 3, logical(1))]
    .nmaAddFitNote(decompositionTable, gettext("Not available for a network of two treatments."), twoTreatments, fits, options)

    if (length(twoTreatments) < length(successful)) {
      if (key == "designDecompositionDetachTable")
        decompositionTable$addFootnote(gettext("Designs whose removal disconnects the network cannot be detached and are not shown."))
    }

    if (!is.null(rows))
      decompositionTable$setData(rows)
  }

  return()
}

.nmaRowsDesignDecomposition <- function(decomposition, element) {

  # failed results are reported by a footnote
  if (jaspBase::isTryError(decomposition))
    return(NULL)

  q <- decomposition[[element]]

  # (a design with a single study has no degrees of freedom for the heterogeneity within the design: the print of netmeta lists the designs with df > 0)
  if (element == "Q.het.design" && !is.null(q))
    q <- q[q[["df"]] > 0, , drop = FALSE]

  if (is.null(q) || nrow(q) == 0)
    return(NULL)

  return(data.frame(
    design   = as.character(q[["design"]]),
    subgroup = attr(decomposition, "subgroup"),
    Q        = q[["Q"]],
    df       = q[["df"]],
    pval     = q[["pval"]]
  ))
}

# Node splitting ----

# The method of the node splitting matters for the inverse variance fits only (the count-based fits provide SIDDE only).
.nmaNodeSplittingMethodDependency <- function(options) {

  return(if (.nmaPoolingMethod(options) == "inverseVariance") "nodeSplittingMethod")
}

.nmaNetsplitState       <- function(jaspResults, options) {

  fits <- .nmaExtractFits(jaspResults, options)

  if (is.null(fits))
    return(NULL)

  return(.nmaExtractState(jaspResults, "netsplitState", .nmaNodeSplittingMethodDependency(options), function() {
    .nmaComputeResults(fits, function(fit) {
      .nmaRunCall(.nmaCallNetsplit(options, fit), fit)
    }, label = gettext("Separating Direct and Indirect Evidence"))
  }))
}

.nmaNodeSplittingData   <- function(split, fit, options) {

  # one row per comparison: A vs. B (the orientation of the treatment effects TE[A, B])
  element   <- .nmaEffectsElement(fit)
  network   <- split[[element]]
  direct    <- split[[paste0("direct.", element)]]
  indirect  <- split[[paste0("indirect.", element)]]
  compare   <- split[[paste0("compare.", element)]]

  data <- data.frame(
    comparison   = .nmaComparisonLabels(split[["comparison"]], fit[["trts"]], split[["sep.trts"]]),
    subgroup     = attr(split, "subgroup"),
    k            = split[["k"]],
    # (netsplit() of the Mantel-Haenszel and non-central hypergeometric fits provides no direct evidence proportion)
    proportion   = if (is.null(split[[paste0("prop.", element)]])) NA_real_ else split[[paste0("prop.", element)]],
    est          = network[["TE"]], lCi = network[["lower"]], uCi = network[["upper"]],
    estDirect    = direct[["TE"]],   lCiDirect   = direct[["lower"]],   uCiDirect   = direct[["upper"]],
    estIndirect  = indirect[["TE"]], lCiIndirect = indirect[["lower"]], uCiIndirect = indirect[["upper"]],
    estDifference = compare[["TE"]], lCiDifference = compare[["lower"]], uCiDifference = compare[["upper"]],
    z            = compare[["z"]],
    pval         = compare[["p"]]
  )

  # comparisons displayed (as print.netsplit)
  hasDirect   <- !is.na(direct[["TE"]])
  hasIndirect <- !is.na(indirect[["TE"]])
  selected    <- switch(
    options[["nodeSplittingShow"]],
    all        = rep(TRUE, nrow(data)),
    both       = hasDirect & hasIndirect,
    withDirect = hasDirect
  )

  return(data[selected & !is.na(data[["est"]]), , drop = FALSE])
}

.nmaNodeSplittingTable  <- function(jaspResults, options) {

  inconsistencyContainer <- .nmaExtractInconsistencyContainer(jaspResults)

  if (!is.null(inconsistencyContainer[["nodeSplittingTable"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  nodeSplittingTable          <- createJaspTable(gettext("Direct and Indirect Evidence"))
  nodeSplittingTable$position <- 3
  nodeSplittingTable$showSpecifiedColumnsOnly <- TRUE
  nodeSplittingTable$dependOn(c("nodeSplitting", .nmaNodeSplittingMethodDependency(options), "nodeSplittingShow", "confidenceIntervals", "transformEffectSize"))
  inconsistencyContainer[["nodeSplittingTable"]] <- nodeSplittingTable

  nodeSplittingTable$addColumnInfo(name = "comparison", type = "string",  title = gettext("Comparison"))
  .maAddSubgroupColumn(nodeSplittingTable, options)
  nodeSplittingTable$addColumnInfo(name = "k",          type = "integer", title = "k")
  # (the SIDDE method of the Mantel-Haenszel and non-central hypergeometric fits provides no direct evidence proportion: the note says so)
  if (!.nmaIsCountBased(options))
    nodeSplittingTable$addColumnInfo(name = "proportion", type = "number",  title = gettext("Direct proportion"))

  # estimates (with confidence intervals) of the network, direct, indirect evidence and their difference
  groups <- list(
    list(suffix = "",           overtitle = gettext("Network"),    intervals = FALSE),
    list(suffix = "Direct",     overtitle = gettext("Direct"),     intervals = TRUE),
    list(suffix = "Indirect",   overtitle = gettext("Indirect"),   intervals = TRUE),
    list(suffix = "Difference", overtitle = if (options[["transformEffectSize"]] == "exponential") gettext("Ratio") else gettext("Difference"), intervals = TRUE)
  )
  for (group in groups) {
    nodeSplittingTable$addColumnInfo(name = paste0("est", group$suffix), type = "number", title = gettext("Estimate"), overtitle = group$overtitle)
    if (group$intervals && options[["confidenceIntervals"]]) {
      nodeSplittingTable$addColumnInfo(name = paste0("lCi", group$suffix), type = "number", title = gettext("Lower"), overtitle = group$overtitle)
      nodeSplittingTable$addColumnInfo(name = paste0("uCi", group$suffix), type = "number", title = gettext("Upper"), overtitle = group$overtitle)
    }
  }
  nodeSplittingTable$addColumnInfo(name = "z",    type = "number", title = "z")
  nodeSplittingTable$addColumnInfo(name = "pval", type = "pvalue", title = gettext("p"))

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    nodeSplittingTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  splits <- .nmaNetsplitState(jaspResults, options)

  if (.nmaReportFailedResults(nodeSplittingTable, splits))
    return()

  data <- try(.maSafeRbind(lapply(seq_along(splits), function(i) {
    if (jaspBase::isTryError(splits[[i]]))
      return(NULL)
    .nmaNodeSplittingData(splits[[i]], fits[[i]], options)
  })), silent = TRUE)

  if (jaspBase::isTryError(data)) {
    nodeSplittingTable$setError(.maTryCleanErrorMessages(data))
    return()
  }

  anyNA <- FALSE
  if (!is.null(data)) {
    columns     <- grep("^(est|lCi|uCi)", colnames(data), value = TRUE)
    transformed <- .nmaTransformEffects(data, options, columns)
    anyNA       <- sum(is.na(transformed[, columns])) > sum(is.na(data[, columns]))
    data        <- .maSafeOrderAndSimplify(transformed, "comparison", options)
  }

  # footnotes
  methods <- unique(vapply(.nmaSuccessfulResults(splits), function(split) split[["method"]], character(1)))
  nodeSplittingTable$addFootnote(gettextf("The direct and indirect evidence were separated using the %1$s method.", paste(methods, collapse = "/")))
  if (options[["nodeSplittingMethod"]] == "backCalculation" && "SIDDE" %in% methods)
    nodeSplittingTable$addFootnote(gettext("The Mantel-Haenszel and non-central hypergeometric fits use the SIDDE method."))
  # (netsplit() provides the direct evidence proportion of the SIDDE method for the inverse variance fits only)
  .nmaAddFitNote(
    nodeSplittingTable,
    gettext("The SIDDE method of the Mantel-Haenszel and non-central hypergeometric fits provides no direct evidence proportion."),
    fits[vapply(seq_along(splits), function(i) !jaspBase::isTryError(splits[[i]]) && is.null(splits[[i]][[paste0("prop.", .nmaEffectsElement(fits[[i]]))]]), logical(1))],
    fits, options
  )
  # (netsplit() reports z-tests and normal-theory confidence intervals also if the t-distribution is used for the treatment effects)
  if (.nmaOptionsTTest(options)) {
    nodeSplittingTable$addFootnote(gettext("Node splitting uses normal-theory (z) inference."))
    if (options[["nodeSplittingForestPlot"]])
      nodeSplittingTable$addFootnote(gettext("The node-splitting forest plot shows normal-theory (z) intervals, also for the network estimates."))
  }
  if (options[["confidenceIntervals"]])
    nodeSplittingTable$addFootnote(gettextf("Lower and upper limits are the limits of the %1$s%% confidence intervals.", 100 * options[["confidenceIntervalsLevel"]]))
  for (message in .nmaTransformationMessage(options, anyNA))
    nodeSplittingTable$addFootnote(message)
  if (options[["transformEffectSize"]] != "none")
    nodeSplittingTable$addFootnote(gettext("The test of disagreement (z, p) is on the analysis scale."))

  if (!is.null(data))
    nodeSplittingTable$setData(data)

  return()
}

.nmaNodeSplittingForestPlot <- function(jaspResults, options) {

  inconsistencyContainer <- .nmaExtractInconsistencyContainer(jaspResults)

  if (!is.null(inconsistencyContainer[["nodeSplittingForestPlot"]]))
    return()

  fits   <- .nmaExtractFits(jaspResults, options)
  splits <- .nmaNetsplitState(jaspResults, options)

  nodeSplittingForestPlot          <- createJaspPlot(title = gettext("Direct and Indirect Evidence"), width = 600, height = 400)
  nodeSplittingForestPlot$position <- 4
  nodeSplittingForestPlot$dependOn(c("nodeSplittingForestPlot", .nmaNodeSplittingMethodDependency(options), .nmaForestRendererDependencies))
  inconsistencyContainer[["nodeSplittingForestPlot"]] <- nodeSplittingForestPlot

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    nodeSplittingForestPlot$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  if (.nmaReportFailedResults(nodeSplittingForestPlot, splits, footnotes = FALSE))
    return()

  forest <- .nmaForestPrepare(options)
  items  <- list()

  for (i in seq_along(splits)) {
    if (jaspBase::isTryError(splits[[i]]))
      next

    data     <- try(.nmaNodeSplittingData(splits[[i]], fits[[i]], options), silent = TRUE)

    if (jaspBase::isTryError(data)) {
      nodeSplittingForestPlot$setError(.maTryCleanErrorMessages(data))
      return()
    }

    data     <- data[!is.na(data[["estDirect"]]) & !is.na(data[["estIndirect"]]), , drop = FALSE]
    sections <- lapply(seq_len(nrow(data)), function(j) .nmaNodeSplittingForestSection(data[j, ], forest))
    names(sections) <- data[["comparison"]]

    if (length(sections) > 0)
      items[[length(items) + 1]] <- .forestPlotCreateInputItem(
        key           = names(splits)[i],
        index         = length(items) + 1,
        subgroup      = if (options[["subgroup"]] != "") attr(fits[[i]], "subgroup"),
        isFullDataset = FALSE,
        sections      = list(study = NULL, additional = sections)
      )
  }

  .nmaFillForestPlot(nodeSplittingForestPlot, items, forest, gettext("There are no comparisons with both direct and indirect evidence."))

  return()
}

.nmaNodeSplittingForestSection <- function(row, forest) {

  # the direct, indirect, and network estimates of a comparison (the direct and indirect estimates are drawn as diamonds,
  # the additional forest sections provide no labelled point-interval rows)
  transformation <- .nmaForestTransformation(forest)
  state          <- .forestPlotCreateAdditionalSectionState()
  noTest         <- .forestPlotResolveTestPlacement(FALSE, forest[["options"]])
  testPlacement  <- .forestPlotResolveTestPlacement(TRUE, forest[["options"]], allowBelow = FALSE)
  testText       <- .maPrintCoefficientTest(
    list(stat = row[["z"]], pval = row[["pval"]]),
    forest[["options"]][["forestPlotAuxiliaryTestsInformation"]] == "statisticAndPValue"
  )

  evidence <- list(
    list(label = gettext("Direct"),   est = row[["estDirect"]],   lCi = row[["lCiDirect"]],   uCi = row[["uCiDirect"]]),
    list(label = gettext("Indirect"), est = row[["estIndirect"]], lCi = row[["lCiIndirect"]], uCi = row[["uCiIndirect"]]),
    list(label = gettext("Network"),  est = row[["est"]],         lCi = row[["lCi"]],         uCi = row[["uCi"]])
  )

  for (item in evidence) {
    state <- .forestPlotAppendEstimateWithInterval(
      state                   = state,
      label                   = item[["label"]],
      estimate                = list(
        est = transformation(item[["est"]]),
        lCi = transformation(item[["lCi"]]),
        uCi = transformation(item[["uCi"]])
      ),
      testText                = "",
      testPlacement           = noTest,
      showPredictionIntervals = FALSE
    )
  }

  # the test of the disagreement belongs to the comparison of the direct and the indirect estimate (a row of its own)
  state <- .forestPlotAppendAdditionalRow(
    state = state,
    label = .forestPlotApplyLeftTestLabel(gettext("Direct vs. indirect"), testText, testPlacement),
    test  = .forestPlotApplyRightTestLabel(testText, testPlacement)
  )

  finalized <- .forestPlotFinalizeAdditionalSectionState(state)

  return(.forestPlotCreateAdditionalSection(
    heading                = row[["comparison"]],
    information            = finalized[["information"]],
    objects                = finalized[["objects"]],
    includeWithFullDataset = TRUE
  ))
}

# Net heat plot ----

.nmaNetHeatPlot         <- function(jaspResults, options) {

  inconsistencyContainer <- .nmaExtractInconsistencyContainer(jaspResults)

  if (!is.null(inconsistencyContainer[["netHeatPlot"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  if (is.null(fits) || options[["subgroup"]] == "") {

    netHeatPlot          <- createJaspPlot(title = gettext("Net Heat Plot"), width = 600, height = 600)
    netHeatPlot$position <- 5
    netHeatPlot$dependOn("netHeatPlot")
    inconsistencyContainer[["netHeatPlot"]] <- netHeatPlot

    .nmaFillNetHeatPlot(netHeatPlot, fits[[1]])

  } else {

    netHeatContainer          <- createJaspContainer(gettext("Net Heat Plot"))
    netHeatContainer$position <- 5
    netHeatContainer$dependOn("netHeatPlot")
    inconsistencyContainer[["netHeatPlot"]] <- netHeatContainer

    for (i in seq_along(fits)) {
      netHeatPlot          <- createJaspPlot(title = .nmaScopeTitle(fits[[i]]), width = 600, height = 600)
      netHeatPlot$position <- i
      netHeatContainer[[names(fits)[i]]] <- netHeatPlot
      .nmaFillNetHeatPlot(netHeatPlot, fits[[i]])
    }
  }

  return()
}

.nmaFillNetHeatPlot     <- function(netHeatPlot, fit) {

  if (is.null(fit))
    return()

  if (jaspBase::isTryError(fit)) {
    netHeatPlot$setError(.maTryCleanErrorMessages(fit))
    return()
  }

  netHeatRender <- try(.nmaNetHeatRender(.nmaNetHeatData(fit)), silent = TRUE)

  if (jaspBase::isTryError(netHeatRender)) {
    netHeatPlot$setError(.maTryCleanErrorMessages(netHeatRender))
    return()
  }

  netHeatPlot$width      <- netHeatRender[["width"]]
  netHeatPlot$height     <- netHeatRender[["height"]]
  netHeatPlot$plotObject <- netHeatRender[["plot"]]

  return()
}

# Reproduces netmeta::netheat() (netmeta 3.7-0) up to the graphics: the matrix of the change in the
# between-design inconsistency Q of a design (row) after detaching another design (column), the
# hierarchical clustering order, and the hat matrix elements that determine the size of the squares.
# netheat() returns no data, and the helpers nma_krahn() and tau.within() are not exported, so they are
# called with ':::' (netmeta 3.7-0). The matrix, the order, and the sizes of the squares equal the values computed inside
# netheat() (test V7 of the network meta-analysis tests compares them for the common-effect and random-effects models).
.nmaNetHeatData         <- function(fit) {

  if (fit[["d"]] <= 2)
    stop(gettextf("The net heat plot is not available for a network with only %1$s designs.", fit[["d"]]), call. = FALSE)

  # common-effect model, a fixed between-study variance, or the between-study variance estimated within designs
  random    <- isTRUE(fit[["random"]])
  tauPreset <- fit[["tau.preset"]]

  if (!random && is.null(tauPreset)) {
    nmak         <- netmeta:::nma_krahn(fit)
    decomp       <- netmeta::decomp.design(fit)
    residuals    <- decomp[["residuals.inc.detach"]]
    qIncDesign   <- decomp[["Q.inc.design"]]
  } else {
    tau          <- if (!is.null(tauPreset)) tauPreset else netmeta:::tau.within(fit)
    nmak         <- netmeta:::nma_krahn(fit, tau.preset = tau)
    decomp       <- netmeta::decomp.design(fit, tau.preset = tau)
    residuals    <- decomp[["residuals.inc.detach.random.preset"]]
    qIncDesign   <- decomp[["Q.inc.design.random.preset"]]
  }

  if (decomp[["Q.decomp"]]["Between designs", "df"] == 0)
    stop(gettext("The net heat plot is not available because the network does not contain a loop of more than one design."), call. = FALSE)

  hat    <- nmak[["H"]]
  design <- nmak[["design"]]

  qIncDesignTyp <- apply(residuals, 2, function(x) t(x) %*% solve(nmak[["V"]]) * x)
  difference    <- matrix(qIncDesign, nrow = nrow(qIncDesignTyp), ncol = ncol(qIncDesignTyp)) - qIncDesignTyp
  dimnames(difference) <- list(rownames(residuals), colnames(qIncDesignTyp))

  if (all(is.na(difference)))
    stop(gettext("The net heat plot is not available because the network contains no between-design heterogeneity."), call. = FALSE)

  if (length(difference) == 1)
    stop(gettext("The net heat plot is not available because the network contains too few informative designs."), call. = FALSE)

  # designs whose detaching disconnects the network are not shown
  t1 <- -difference
  wi <- which(apply(t1, 2, function(x) sum(is.na(x)) == nrow(t1)))
  if (length(wi) > 0) {
    t1     <- t1[-wi, -wi, drop = FALSE]
    design <- design[-wi, ]
    hatSub <- hat[as.character(nmak[["design"]][["comparison"]])[-wi], -wi]
  } else {
    hatSub <- hat[as.character(design[["comparison"]]), ]
  }

  distances <- stats::dist(t1, method = "manhattan") + stats::dist(t(t1), method = "manhattan")

  if (length(distances) == 0)
    stop(gettext("The net heat plot is not available because the network contains too few informative designs."), call. = FALSE)

  # (netheat() labels a comparison of a multi-arm design as 'comparison_design'; the design is given in parentheses here)
  order  <- stats::hclust(distances)$order
  labels <- ifelse(
    design[["narms"]][order] > 2,
    sprintf("%1$s (%2$s)", as.character(design[["comparison"]])[order], as.character(design[["design"]])[order]),
    as.character(design[["comparison"]])[order]
  )

  return(list(
    t1     = t1[order, order, drop = FALSE],
    size   = abs(hatSub[order, order, drop = FALSE]),
    order  = order,
    labels = labels
  ))
}

# the colors of netheat(): from red (detaching the design in the column reduces the inconsistency of the design in the row) via white to blue
.nmaNetHeatColors       <- function(t1) {

  tn    <- pmin(pmax(t1, -8), 8)
  ncolo <- 40
  sat   <- min(max(abs(tn)) / 8, 1)
  nblue <- ifelse(
    (max(max(tn), 0) - min(min(tn), 0)) != 0,
    round(max(max(tn), 0) / (max(max(tn), 0) - min(min(tn), 0)) * ncolo),
    0
  )
  bf        <- min(max(max(tn), 0) / abs(min(min(tn), 0)), 1) * sat
  clev      <- seq(from = 1, to = 1 - bf, length.out = nblue)
  heatVec   <- grDevices::heat.colors(round((ncolo - nblue - 1) / max(sat, 1e-05)))
  colors    <- c(
    heatVec[(length(heatVec) - (ncolo - nblue - 1) + 1):length(heatVec)],
    grDevices::rgb(1, 1, 1),
    grDevices::rgb(clev, clev, 1)
  )

  return(colors)
}

.nmaNetHeatRender       <- function(heat) {

  n      <- nrow(heat[["t1"]])
  clipped <- pmin(pmax(heat[["t1"]], -8), 8)

  # the plotted value is the reduction in Q of the design in the row when the design in the column is detached
  data <- data.frame(
    row    = rep(seq_len(n), times = n),
    column = rep(seq_len(n), each = n),
    value  = -as.vector(clipped)
  )

  # the size of the squares reflects the contribution of the design in the column to the estimate of the design in the row (as netheat)
  size <- as.vector(heat[["size"]])
  size <- if (max(size) > min(size)) (sqrt((size - min(size)) / (max(size) - min(size))) + 0.1) / 1.4 else rep(0.5, length(size))
  data$size <- size
  data$y    <- n - data$row + 1

  colors <- .nmaNetHeatColors(heat[["t1"]])

  # JASP lays the plot out on another graphics device than it draws it on, where the text is narrower (5.4 instead of 6.1 pixels per character on
  # Windows), which would cut off the start of the long labels of multi-arm designs; the outer margin of the axis texts leaves room for the difference
  labelLength <- max(nchar(heat[["labels"]]))
  labelRoom   <- 0.8 * labelLength

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = column, y = y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), width = 1, height = 1) +
    ggplot2::geom_tile(ggplot2::aes(width = size, height = size), fill = "black", alpha = 0.2) +
    ggplot2::scale_fill_gradientn(colours = rev(colors), limits = range(data$value), name = gettext("Reduction in Q")) +
    ggplot2::scale_x_continuous(breaks = seq_len(n), labels = heat[["labels"]], position = "top", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = n:1, labels = heat[["labels"]], expand = c(0, 0)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL) +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    ggplot2::theme(
      axis.line        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      axis.text.x.top  = ggplot2::element_text(size = .nmaPlotTextSize(), angle = 90, hjust = 0, vjust = 0.5, margin = ggplot2::margin(t = labelRoom, b = 2)),
      axis.text.y      = ggplot2::element_text(size = .nmaPlotTextSize(), margin = ggplot2::margin(l = labelRoom, r = 3)),
      legend.title     = ggplot2::element_text(size = .nmaPlotTextSize()),
      legend.text      = ggplot2::element_text(size = .nmaPlotTextSize()),
      panel.background = ggplot2::element_rect(fill = "grey", color = NA)
    )

  # the size in pixels: 30 pixels per design, the labels (about 6 pixels per character of the text of the forest plot and the room above; the plots
  # are drawn at 72 pixels per inch), and the legend
  labels <- 6 * labelLength + labelRoom + 20
  grid   <- 30 * n

  return(list(plot = plot, width = round(labels + grid + 130), height = round(labels + grid + 20)))
}
