# Classical network meta-analysis plots.
#
# Builds the network plot, the forest plot (with the module forest plot renderer), and the
# small-study effects outputs (comparison-adjusted funnel plot and asymmetry tests) from the netmeta fits.

# Network plot ----

.nmaNetworkPlot         <- function(jaspResults, options) {

  if (!is.null(jaspResults[["networkPlot"]]))
    return()

  # the network of the data: the standard model (also with the component model), or the component model of a disconnected network
  scopes       <- .nmaExtractScopes(jaspResults, options)
  fits         <- if (!is.null(scopes)) lapply(scopes, function(scope) {
    standard <- .nmaStandardFit(scope)
    if (jaspBase::isTryError(standard) && .nmaIsComponentFit(scope[["fit"]])) scope[["fit"]] else standard
  })
  dependencies <- c(
    .nmaModelDependencies, "includeFullDatasetInSubgroupAnalysis", "networkPlot", "networkPlotLayout", "networkPlotEdgeWidth",
    "networkPlotNodeSize", "networkPlotNumberOfStudies", "networkPlotMultiArmStudies", "networkPlotSizeNodes", "networkPlotSizeEdges",
    "networkPlotSizeText"
  )

  if (is.null(fits) || options[["subgroup"]] == "") {

    networkPlot          <- createJaspPlot(title = gettext("Network Plot"), width = 500, height = 500)
    networkPlot$position <- 9
    networkPlot$dependOn(dependencies)
    jaspResults[["networkPlot"]] <- networkPlot

    .nmaFillNetworkPlot(networkPlot, fits[[1]], options)
    .nmaNetworkPlotNote(jaspResults, fits, options, dependencies)

  } else {

    networkContainer          <- createJaspContainer(gettext("Network Plot"))
    networkContainer$position <- 9
    networkContainer$dependOn(dependencies)
    jaspResults[["networkPlot"]] <- networkContainer

    for (i in seq_along(fits)) {
      networkPlot          <- createJaspPlot(title = .nmaScopeTitle(fits[[i]]), width = 500, height = 500)
      networkPlot$position <- i
      networkContainer[[names(fits)[i]]] <- networkPlot
      .nmaFillNetworkPlot(networkPlot, fits[[i]], options)
    }

    .nmaNetworkPlotNote(jaspResults, fits, options, dependencies)
  }

  return()
}

# The precision of the direct estimates is not available for the component model of a disconnected network (discomb() has no direct estimates).
.nmaNetworkNoPrecision  <- function(fit, options) {

  return(options[["networkPlotEdgeWidth"]] == "inverseStandardError" && inherits(fit, "discomb"))
}

# The note above the network plot (the plots have no footnotes): the edge widths of the fits without direct estimates show the number of studies.
.nmaNetworkPlotNote     <- function(jaspResults, fits, options, dependencies) {

  notes <- character(0)

  for (fit in fits) {
    if (jaspBase::isTryError(fit) || !.nmaNetworkNoPrecision(fit, options))
      next

    note  <- gettext("The edge widths by precision are not available for the component model of a disconnected network: the edge widths show the number of studies.")
    notes <- c(notes, if (options[["subgroup"]] != "") gettextf("%1$s: %2$s", .nmaScopeQuoted(fit), note) else note)
  }

  if (length(notes) == 0)
    return()

  networkNote          <- createJaspHtml(text = paste(gettext("Warning:"), .nmaEscapeHtml(unique(notes)), collapse = "<br>"))
  networkNote$position <- 8.5
  networkNote$dependOn(dependencies)
  jaspResults[["networkPlotWarning"]] <- networkNote

  return()
}

.nmaFillNetworkPlot     <- function(networkPlot, fit, options) {

  if (is.null(fit))
    return()

  if (jaspBase::isTryError(fit)) {
    networkPlot$setError(.maTryCleanErrorMessages(fit))
    return()
  }

  render <- try(.nmaNetworkPlotObject(fit, options), silent = TRUE)

  if (jaspBase::isTryError(render)) {
    networkPlot$setError(.maTryCleanErrorMessages(render))
    return()
  }

  networkPlot$width      <- render[["width"]]
  networkPlot$height     <- render[["height"]]
  networkPlot$plotObject <- render[["plot"]]

  return()
}

# the widths of the edges (as the lines of netmeta::netgraph(): the maximum width is 20 for the number of studies and the precision, 10 if equal,
# and netgraph() halves the widths by the number of studies if all comparisons have the same number of studies; without direct estimates, the
# widths by precision are the widths by the number of studies)
.nmaNetworkEdgeWidths   <- function(fit, options) {

  adjacency <- fit[["A.matrix"]]
  maximum   <- 20
  edgeWidth <- if (.nmaNetworkNoPrecision(fit, options)) "numberOfStudies" else options[["networkPlotEdgeWidth"]]

  widths <- switch(
    edgeWidth,
    numberOfStudies      = {
      widths <- maximum * sqrt(adjacency) / sqrt(max(adjacency))
      if (length(unique(adjacency[adjacency != 0])) == 1) widths / 2 else widths
    },
    inverseStandardError = {
      standardErrors <- fit[[paste0("seTE.direct.", .nmaEffectsElement(fit))]]
      maximum * sqrt(min(standardErrors, na.rm = TRUE)) / sqrt(standardErrors)
    },
    equal                = 10 * sign(adjacency)
  )

  # the minimum width is 2
  widths[which(widths < 2 & widths != 0)] <- 2

  return(widths)
}

# The number of studies of each treatment: the studies with a comparison of the fit (k.trts of netmetabin() adds the comparisons of a study with
# more than two arms for each of its treatments)
.nmaNetworkStudiesPerTreatment <- function(fit) {

  arms <- unique(data.frame(
    study     = rep(as.character(fit[["studlab"]]), 2),
    treatment = c(as.character(fit[["treat1"]]), as.character(fit[["treat2"]]))
  ))
  numbers <- table(factor(arms[["treatment"]], levels = fit[["trts"]]))

  return(setNames(as.numeric(numbers), names(numbers)))
}

.nmaNetworkNodeSizes    <- function(fit, options) {

  treatments <- fit[["trts"]]
  size       <- switch(
    options[["networkPlotNodeSize"]],
    equal           = NULL,
    numberOfStudies = .nmaNetworkStudiesPerTreatment(fit),
    # (the sample size is not available for all data; equal sizes are used then)
    sampleSize      = fit[["n.trts"]]
  )

  if (is.null(size) || length(unique(size)) == 1)
    return(setNames(rep(7, length(treatments)), treatments))

  # the area of the nodes reflects the quantity
  return(4 + 8 * (sqrt(size[treatments]) - sqrt(min(size))) / (sqrt(max(size)) - sqrt(min(size))))
}

# The directions of the labels: radially away from the center of the network (the centroid of the nodes). A label that would be placed
# close to another node is rotated in steps of 30 degrees (the smallest rotation first).
.nmaNetworkLabelDirections <- function(nodes, center, span) {

  offsets   <- (0.06 + 0.008 * nodes$size) * span
  clearance <- 0.06 * span
  rotations <- c(0, as.vector(rbind(1:6, -(1:6)))) * pi / 6

  directions <- vapply(seq_len(nrow(nodes)), function(i) {

    change <- c(nodes$x[i], nodes$y[i]) - center
    angle  <- if (sqrt(sum(change^2)) < 1e-6 * span) pi / 2 else atan2(change[2], change[1])

    for (rotation in rotations) {
      direction <- c(cos(angle + rotation), sin(angle + rotation))
      label     <- c(nodes$x[i], nodes$y[i]) + offsets[i] * direction
      distances <- sqrt((nodes$x[-i] - label[1])^2 + (nodes$y[-i] - label[2])^2)
      if (all(distances > clearance))
        return(direction)
    }

    return(c(cos(angle), sin(angle)))
  }, numeric(2))

  return(t(directions))
}

# The network plot and its size in pixels: the network spans about 400 pixels, and the canvas adds the
# room for the labels (estimated from the number of characters of the longest label) and the legend; the canvas is at least 400 pixels high,
# as the other plots (the network of two treatments is a line)
# the size of the axis and legend texts of the network and net heat plots: the size of the text of the forest plot (size 4 of geom_text(), in points)
.nmaPlotTextSize        <- function() {

  return(4 * ggplot2::.pt)
}

.nmaNetworkPlotObject   <- function(fit, options) {

  textScale <- options[["networkPlotSizeText"]]

  # the node coordinates of netmeta::netgraph()
  graph <- NULL
  utils::capture.output(graph <- .nmaRunCall(.nmaCallNetgraph(options, fit), fit))
  treatments <- fit[["trts"]]
  nodes      <- data.frame(
    treatment = treatments,
    x         = graph[["nodes"]][treatments, "xpos"],
    y         = graph[["nodes"]][treatments, "ypos"],
    size      = options[["networkPlotSizeNodes"]] * .nmaNetworkNodeSizes(fit, options)[treatments]
  )
  center     <- c(mean(nodes$x), mean(nodes$y))
  span       <- max(diff(range(nodes$x)), diff(range(nodes$y)))

  # the edges of the direct comparisons
  widths <- .nmaNetworkEdgeWidths(fit, options)
  edges  <- graph[["edges"]]
  edges  <- data.frame(
    x      = nodes$x[match(edges[["treat1"]], treatments)],
    y      = nodes$y[match(edges[["treat1"]], treatments)],
    xend   = nodes$x[match(edges[["treat2"]], treatments)],
    yend   = nodes$y[match(edges[["treat2"]], treatments)],
    # (the line widths of netgraph() are in points of a base R plot; 0.25 maps them to the line widths of the ggplot)
    width  = 0.25 * options[["networkPlotSizeEdges"]] * widths[cbind(as.character(edges[["treat1"]]), as.character(edges[["treat2"]]))],
    studies = edges[["n.stud"]]
  )

  # the labels of the treatments outside of the nodes (radially away from the center of the network)
  direction    <- .nmaNetworkLabelDirections(nodes, center, span)
  offset       <- (0.06 + 0.008 * nodes$size) * span
  nodes$labelX <- nodes$x + offset * direction[, 1]
  nodes$labelY <- nodes$y + offset * direction[, 2]
  nodes$hjust  <- 0.5 - 0.5 * direction[, 1]
  nodes$vjust  <- 0.5 - 0.5 * direction[, 2]

  plot   <- ggplot2::ggplot()
  legend <- 0

  # the treatments of multi-arm studies
  if (options[["networkPlotMultiArmStudies"]]) {
    polygons <- .nmaNetworkMultiArmPolygons(fit, nodes)
    if (!is.null(polygons)) {
      plot <- plot +
        ggplot2::geom_polygon(data = polygons, ggplot2::aes(x = x, y = y, group = study, fill = study), alpha = 0.2) +
        .nmaTreatmentColors(length(unique(polygons$study)), "fill") +
        ggplot2::labs(fill = gettext("Multi-arm study"))
      legend <- 40 + 6 * textScale * max(nchar(c(as.character(polygons$study), gettext("Multi-arm study"))))
    }
  }

  plot <- plot +
    ggplot2::geom_segment(data = edges, ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linewidth = width), lineend = "round", color = "grey40") +
    ggplot2::scale_linewidth_identity() +
    ggplot2::geom_point(data = nodes, ggplot2::aes(x = x, y = y, size = size), shape = 21, fill = "grey85", color = "black", stroke = 0.8) +
    ggplot2::scale_size_identity() +
    ggplot2::geom_text(data = nodes, ggplot2::aes(x = labelX, y = labelY, label = treatment, hjust = hjust, vjust = vjust), size = 4 * textScale)

  # the number of studies of the comparisons
  if (options[["networkPlotNumberOfStudies"]]) {
    plot <- plot +
      ggplot2::geom_point(data = edges, ggplot2::aes(x = (x + xend) / 2, y = (y + yend) / 2), shape = 21, size = 6 * textScale, fill = "white", color = "grey40") +
      ggplot2::geom_text(data = edges, ggplot2::aes(x = (x + xend) / 2, y = (y + yend) / 2, label = studies), size = 3.5 * textScale)
  }

  # the limits of the plot: the nodes and the boxes of the labels (about 5.6 pixels per character of a label of size 4 and 14 pixels of
  # its height; the plots are drawn at 72 pixels per inch), with the network spanning 400 pixels
  pixels  <- 400
  perUnit <- span / pixels
  radius  <- (1.5 * nodes$size + 2) * perUnit
  width   <- (5.6 * textScale * nchar(nodes$treatment) + 4) * perUnit
  height  <- (14 * textScale + 2) * perUnit
  xLimits <- range(nodes$x - radius, nodes$x + radius, nodes$labelX - nodes$hjust * width, nodes$labelX + (1 - nodes$hjust) * width)
  yLimits <- range(nodes$y - radius, nodes$y + radius, nodes$labelY - nodes$vjust * height, nodes$labelY + (1 - nodes$vjust) * height)

  plot <- plot +
    ggplot2::coord_equal(xlim = xLimits, ylim = yLimits, expand = FALSE, clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_blank(),
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks   = ggplot2::element_blank(),
      axis.line    = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = textScale * .nmaPlotTextSize()),
      legend.text  = ggplot2::element_text(size = textScale * .nmaPlotTextSize()),
      plot.margin  = ggplot2::margin(5, 5, 5, 5)
    )

  return(list(
    plot   = plot,
    width  = round(diff(xLimits) / perUnit + legend + 10),
    height = max(400, round(diff(yLimits) / perUnit + 10))
  ))
}

# the area spanned by the treatments of each multi-arm study (the corners sorted by their angle around the center of the corners)
.nmaNetworkMultiArmPolygons <- function(fit, nodes) {

  # (the number of arms is given per study, the study labels per comparison)
  studies <- unique(as.character(fit[["studies"]][fit[["narms"]] > 2]))

  if (length(studies) == 0)
    return(NULL)

  polygons <- lapply(studies, function(study) {

    rows       <- as.character(fit[["studlab"]]) == study
    treatments <- unique(c(fit[["treat1"]][rows], fit[["treat2"]][rows]))
    corners    <- nodes[match(treatments, nodes$treatment), c("x", "y")]
    angles     <- atan2(corners$y - mean(corners$y), corners$x - mean(corners$x))
    corners    <- corners[order(angles), ]

    return(data.frame(study = study, x = corners$x, y = corners$y))
  })

  return(do.call(rbind, polygons))
}

# Forest plot ----

# the options of the forest plot: the options of the analysis and the inputs of the module forest plot renderer that are not options of
# the network meta-analysis (the renderer contract of an additional-sections-only forest plot, like .fpStandaloneDefaults; not GUI defaults)
.nmaForestPlotOptions   <- function(options) {

  options[["forestPlotStudyInformation"]]                       <- FALSE
  options[["forestPlotStudyInformationAggregateBy"]]            <- ""
  options[["forestPlotEstimatedMarginalMeansTermTests"]]        <- FALSE
  options[["forestPlotEstimatedMarginalMeansCoefficientTests"]] <- FALSE
  options[["forestPlotEstimateInformationSelectedVariables"]]   <- list()
  options[["row"]]                                              <- ""

  return(options)
}

# options of the module forest plot renderer read by all network meta-analysis forest plots (the main forest plot, the forest plot of the
# direct and indirect evidence, and the component forest plot); the options of the hidden controls (the mapping of the color and the shape,
# the full dataset sections, and the panels within the subgroups) cannot change and are not dependencies
.nmaForestRendererDependencies <- c(
  "transformEffectSize",
  "forestPlotPredictionIntervals", "forestPlotEstimatesAndConfidenceIntervals", "forestPlotConditionalEstimates",
  "forestPlotTestsInRightPanel", "forestPlotAlignLeftPanel",
  "forestPlotSubgroupShowTitles",
  "forestPlotSizeEstimates", "forestPlotSizeText", "forestPlotSizeAxisLabels", "forestPlotSizeRow",
  "forestPlotSizeLeftPanel", "forestPlotSizePlotArea", "forestPlotSizeRightPanel",
  "forestPlotAuxiliaryDigits", "forestPlotAuxiliaryTestsInformation", "forestPlotAuxiliaryPlotColor",
  "forestPlotAuxiliaryAddVerticalLine", "forestPlotAuxiliaryAddVerticalLineValue",
  "forestPlotAuxiliaryAddVerticalLine2", "forestPlotAuxiliaryAddVerticalLineValue2",
  "forestPlotAuxiliaryEffectLabel", "forestPlotAuxiliarySetXAxisLimit",
  "forestPlotAuxiliarySetXAxisLimitLower", "forestPlotAuxiliarySetXAxisLimitUpper",
  "forestPlotAuxiliarySetXAxisTicks", "forestPlotAuxiliarySetXAxisTicksValues",
  "forestPlotAuxiliaryXAxisTransformLabelsOnly"
)

# the main forest plot adds its own options (the comparisons, the order of the treatments, and the ranking that defines the order)
.nmaForestPlotDependencies <- c(.nmaForestRendererDependencies, "forestPlot", "forestPlotComparisons", "forestPlotOrder", "beneficialEffect")

# prepared options of the renderer and the options of the data (transformed only if the axis labels are not transformed instead)
.nmaForestPrepare       <- function(options) {

  prepared <- .forestPlotPrepareOptions(.nmaForestPlotOptions(options))

  return(list(options = prepared, dataOptions = .forestPlotPrepareDataOptions(prepared)))
}

.nmaForestTransformation <- function(forest) {

  return(.maGetEffectSizeTransformationOptions(forest[["dataOptions"]][["transformEffectSize"]]))
}

.nmaForestRender        <- function(items, forest) {

  plotInput <- .forestPlotCreateInput(items, rowMode = "stacked", sectionGrouping = "byItem")
  plotData  <- .forestPlotBuildPlotData(plotInput, forest[["dataOptions"]])
  rendered  <- .forestPlotRenderPlot(plotData, forest[["options"]])

  # the renderer allots about 10 pixels per row, and the axis (labels and title) is placed in the space of about two rows below the last
  # row: a plot with many rows (e.g., the direct and indirect evidence of 11 comparisons, 66 rows) is cut at the axis title, so the height
  # gives every row at least 22 pixels (the height of the renderer is kept if it is larger)
  rendered[["height"]] <- max(rendered[["height"]], 22 * .forestPlotRowSize(forest[["options"]]) * (plotData[["rowCount"]] + 3))

  return(rendered)
}

.nmaFillForestPlot      <- function(forestPlot, items, forest, emptyMessage) {

  if (length(items) == 0) {
    forestPlot$setError(emptyMessage)
    return()
  }

  forestRender <- try(.nmaForestRender(items, forest), silent = TRUE)

  if (jaspBase::isTryError(forestRender)) {
    forestPlot$setError(.maTryCleanErrorMessages(forestRender))
    return()
  }

  forestPlot$width      <- forestRender[["width"]]
  forestPlot$height     <- forestRender[["height"]]
  forestPlot$plotObject <- forestRender[["plot"]]

  return()
}

.nmaForestPlot          <- function(jaspResults, options) {

  if (!is.null(jaspResults[["forestPlot"]]))
    return()

  fits <- .nmaExtractFits(jaspResults, options)

  forestPlot          <- createJaspPlot(title = gettext("Forest Plot"), width = 600, height = 400)
  forestPlot$position <- 10
  forestPlot$dependOn(c(.nmaModelDependencies, "includeFullDatasetInSubgroupAnalysis", .nmaForestPlotDependencies))
  jaspResults[["forestPlot"]] <- forestPlot

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    forestPlot$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  forest   <- .nmaForestPrepare(options)
  items    <- list()
  warnings <- character(0)

  for (i in seq_along(fits)) {
    if (jaspBase::isTryError(fits[[i]]))
      next

    sections <- try(.nmaForestSections(fits[[i]], options, forest), silent = TRUE)

    if (jaspBase::isTryError(sections)) {
      forestPlot$setError(.maTryCleanErrorMessages(sections))
      return()
    }

    # (the note on an order by the ranking that is not available)
    warning <- attr(sections, "warning")
    if (!is.null(warning))
      warnings <- c(warnings, if (options[["subgroup"]] != "") gettextf("%1$s: %2$s", .nmaScopeQuoted(fits[[i]]), warning) else warning)
    attr(sections, "warning") <- NULL

    items[[length(items) + 1]] <- .forestPlotCreateInputItem(
      key           = names(fits)[i],
      index         = length(items) + 1,
      subgroup      = if (options[["subgroup"]] != "") attr(fits[[i]], "subgroup"),
      isFullDataset = FALSE,
      sections      = list(study = NULL, additional = sections)
    )
  }

  .nmaFillForestPlot(forestPlot, items, forest, .nmaFailureMessage(fits))

  # the plots have no footnotes: the note is shown above the plot
  if (length(warnings) > 0) {
    forestWarning          <- createJaspHtml(text = paste(gettext("Warning:"), .nmaEscapeHtml(unique(warnings)), collapse = "<br>"))
    forestWarning$position <- 9.5
    forestWarning$dependOn(c(.nmaModelDependencies, "includeFullDatasetInSubgroupAnalysis", .nmaForestPlotDependencies))
    jaspResults[["forestPlotWarning"]] <- forestWarning
  }

  return()
}

# the text of a message in an HTML element (the labels of the subgroups and the treatments are shown as they are, also with '&', '<', or '>')
.nmaEscapeHtml <- function(text) {

  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)

  return(gsub(">", "&gt;", text, fixed = TRUE))
}

# the order of the treatments in the forest plot (if the ranking is not available, e.g., for a component fit with contrasts that are not
# estimable, the order is the treatment order, as in the league table: the note is the 'warning' attribute)
.nmaForestTreatmentOrder <- function(fit, options) {

  treatments <- fit[["trts"]]
  ordered    <- switch(
    options[["forestPlotOrder"]],
    treatmentOrder = treatments,
    effectSize     = treatments[order(fit[[paste0("TE.", .nmaEffectsElement(fit))]][treatments, fit[["reference.group"]]])],
    ranking        = .nmaRankedTreatments(fit, options)
  )

  if (jaspBase::isTryError(ordered))
    return(structure(treatments, warning = .nmaRankedOrderMessage(ordered)))

  return(ordered)
}

# the estimates of a treatment versus other treatments as rows of an additional section
.nmaForestEstimatesSection <- function(fit, first, second, labels, forest, heading, showHeading) {

  element        <- .nmaEffectsElement(fit)
  transformation <- .nmaForestTransformation(forest)
  noTest         <- .forestPlotResolveTestPlacement(FALSE, forest[["options"]])
  state          <- .forestPlotCreateAdditionalSectionState()

  for (i in seq_along(first)) {

    estimate <- lapply(
      c(est = paste0("TE.", element), lCi = paste0("lower.", element), uCi = paste0("upper.", element)),
      function(name) transformation(fit[[name]][first[i], second[i]])
    )
    # prediction intervals of the random-effects model
    # (the component model has no prediction intervals: the fit has no such elements)
    predictionIntervals <- forest[["options"]][["forestPlotPredictionIntervals"]] && .nmaHasPredictionIntervals(fit)
    if (predictionIntervals) {
      estimate[["lPi"]]   <- transformation(fit[["lower.predict"]][first[i], second[i]])
      estimate[["uPi"]]   <- transformation(fit[["upper.predict"]][first[i], second[i]])
      predictionIntervals <- !anyNA(c(estimate[["lPi"]], estimate[["uPi"]]))
    }

    state <- .forestPlotAppendEstimateWithInterval(
      state                   = state,
      label                   = labels[i],
      estimate                = estimate,
      testText                = "",
      testPlacement           = noTest,
      showPredictionIntervals = predictionIntervals
    )
  }

  finalized <- .forestPlotFinalizeAdditionalSectionState(state)

  return(.forestPlotCreateAdditionalSection(
    heading                = heading,
    information            = finalized[["information"]],
    objects                = finalized[["objects"]],
    showHeading            = showHeading,
    includeWithFullDataset = TRUE
  ))
}

# the sections of the forest plot of a fit (the 'warning' attribute is the note on the order of the treatments)
.nmaForestSections      <- function(fit, options, forest) {

  treatments <- .nmaForestTreatmentOrder(fit, options)
  reference  <- fit[["reference.group"]]

  if (options[["forestPlotComparisons"]] == "versusReference") {
    # each treatment versus the reference treatment
    others   <- setdiff(treatments, reference)
    sections <- list(treatmentEffects = .nmaForestEstimatesSection(
      fit, others, rep(reference, length(others)), gettextf("%1$s vs. %2$s", others, reference), forest, heading = NULL, showHeading = FALSE
    ))
  } else {
    # each treatment versus all other treatments
    sections <- lapply(treatments, function(treatment) {
      others <- setdiff(treatments, treatment)
      .nmaForestEstimatesSection(fit, rep(treatment, length(others)), others, gettextf("vs. %1$s", others), forest, heading = treatment, showHeading = TRUE)
    })
    names(sections) <- treatments
  }

  attr(sections, "warning") <- attr(treatments, "warning")

  return(sections)
}

# Small-study effects ----

.nmaSmallStudyEffectsOutputs <- function(jaspResults, options) {

  anyTest <- options[["funnelPlotEggerTest"]] || options[["funnelPlotBeggTest"]] || options[["funnelPlotThompsonSharpTest"]]

  if (options[["funnelPlot"]])
    .nmaFunnelPlot(jaspResults, options)

  if (anyTest)
    .nmaFunnelTestsTable(jaspResults, options)

  return()
}

.nmaExtractSmallStudyEffectsContainer <- function(jaspResults) {

  return(.nmaExtractOutputContainer(
    jaspResults, "smallStudyEffectsContainer", gettext("Small-Study Effects"), 11,
    c("funnelPlot", "funnelPlotEggerTest", "funnelPlotBeggTest", "funnelPlotThompsonSharpTest")
  ))
}

# The order of the treatments of a network: the reference treatment first, or the custom order list restricted to the treatments of the
# network (a subgroup network can contain fewer treatments) followed by the treatments that are not listed.
.nmaFunnelOrder         <- function(fit, options) {

  treatments <- fit[["trts"]]

  if (options[["funnelPlotTreatmentOrder"]] == "referenceFirst")
    return(c(fit[["reference.group"]], setdiff(treatments, fit[["reference.group"]])))

  # the treatments of the order list, followed by the treatments that are not listed (in the network order)
  order <- as.character(unlist(options[["funnelPlotTreatmentOrderCustom"]]))
  order <- order[order %in% treatments]

  return(c(order, setdiff(treatments, order)))
}

# the study effects centered at the pooled direct effect of their comparison (as netmeta::funnel())
.nmaFunnelState         <- function(jaspResults, options) {

  fits <- .nmaExtractFits(jaspResults, options)

  if (is.null(fits))
    return(NULL)

  return(.nmaExtractState(jaspResults, "funnelState", c("funnelPlotTreatmentOrder", "funnelPlotTreatmentOrderCustom"), function() {
    .nmaComputeResults(fits, function(fit) {

      order  <- .nmaFunnelOrder(fit, options)

      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off())

      data <- .nmaRunCall(.nmaCallFunnel(fit, order), fit)
      data <- data.frame(
        studlab    = as.character(data[["studlab"]]),
        comparison = .nmaComparisonLabels(as.character(data[["comparison"]]), fit[["trts"]], .nmaTreatmentSeparator(fit)),
        TE         = data[["TE"]],
        TE.adj     = data[["TE.adj"]],
        seTE       = data[["seTE"]]
      )
      attr(data, "order") <- order

      return(data)
    })
  }))
}

.nmaFunnelPlot          <- function(jaspResults, options) {

  smallStudyEffectsContainer <- .nmaExtractSmallStudyEffectsContainer(jaspResults)

  if (!is.null(smallStudyEffectsContainer[["funnelPlot"]]))
    return()

  fits         <- .nmaExtractFits(jaspResults, options)
  funnels      <- .nmaFunnelState(jaspResults, options)
  dependencies <- c("funnelPlot", "funnelPlotTreatmentOrder", "funnelPlotTreatmentOrderCustom", "funnelPlotPseudoConfidenceIntervals", "transformEffectSize")

  if (is.null(fits) || options[["subgroup"]] == "") {

    funnelPlot          <- createJaspPlot(title = gettext("Comparison-Adjusted Funnel Plot"), width = 600, height = 450)
    funnelPlot$position <- 1
    funnelPlot$dependOn(dependencies)
    smallStudyEffectsContainer[["funnelPlot"]] <- funnelPlot

    .nmaFillFunnelPlot(funnelPlot, funnels[[1]], options)

  } else {

    funnelContainer          <- createJaspContainer(gettext("Comparison-Adjusted Funnel Plot"))
    funnelContainer$position <- 1
    funnelContainer$dependOn(dependencies)
    smallStudyEffectsContainer[["funnelPlot"]] <- funnelContainer

    for (i in seq_along(fits)) {
      funnelPlot          <- createJaspPlot(title = .nmaScopeTitle(fits[[i]]), width = 600, height = 450)
      funnelPlot$position <- i
      funnelContainer[[names(fits)[i]]] <- funnelPlot
      .nmaFillFunnelPlot(funnelPlot, funnels[[i]], options)
    }
  }

  return()
}

.nmaFillFunnelPlot      <- function(funnelPlot, funnel, options) {

  if (is.null(funnel))
    return()

  if (jaspBase::isTryError(funnel)) {
    funnelPlot$setError(.maTryCleanErrorMessages(funnel))
    return()
  }

  plot <- try(.nmaFunnelPlotObject(funnel, options), silent = TRUE)

  if (jaspBase::isTryError(plot)) {
    funnelPlot$setError(.maTryCleanErrorMessages(plot))
    return()
  }

  # The legend is on the right and its labels are the comparisons: the width is sized from the longest label, 390 pixels for the axis and
  # the panel, 11 pixels per character for the legend (the Desktop font needs about 8 pixels per character at the default text size) and
  # 1 pixel per character for the right margin of the plot (.nmaFunnelPlotObject(); the Desktop draws the legend text 5-10% wider than
  # the layout of the legend assumes, so the labels reach beyond the legend).
  funnelPlot$width      <- max(600, 390 + 12 * max(nchar(as.character(funnel[["comparison"]]))))
  funnelPlot$height     <- max(450, 120 + 18 * length(unique(funnel[["comparison"]])))
  funnelPlot$plotObject <- plot

  return()
}

.nmaFunnelPlotObject    <- function(funnel, options) {

  # comparisons without an effect size (e.g., a study without events) have no point in the plot
  funnel <- funnel[is.finite(funnel[["TE.adj"]]) & is.finite(funnel[["seTE"]]), , drop = FALSE]

  if (nrow(funnel) == 0)
    stop(gettext("There are no comparisons with an effect size and a standard error."), call. = FALSE)

  # the style of the Funnel Plot analysis: the pseudo confidence regions of its default prediction intervals (90%, 95%, 99%) around
  # no effect as filled grey areas without lines, the estimates colored by the JASP palette
  yTicks       <- jaspGraphs::getPrettyAxisBreaks(range(c(0, funnel[["seTE"]])))
  funnelLevels <- sort((1 - c(0.90, 0.95, 0.99)) / 2)
  contours     <- .fpComputeFunnelDf(range(yTicks), mean = 0, heterogeneity = 0, funnelLevels = funnelLevels)

  xAxis   <- .nmaFunnelXAxis(range(c(funnel[["TE.adj"]], if (options[["funnelPlotPseudoConfidenceIntervals"]]) range(sapply(contours, function(contour) contour$x)))), options)
  xBreaks <- xAxis[["breaks"]]

  comparisons       <- unique(funnel[["comparison"]])
  funnel$comparison <- factor(funnel[["comparison"]], levels = comparisons)

  plot <- ggplot2::ggplot()

  if (options[["funnelPlotPseudoConfidenceIntervals"]])
    plot <- .fpAddFunnelLayers(plot, contours, .fpFunnelColors(funnelLevels), fillColors = TRUE, lineType = "none")

  plot <- plot +
    jaspGraphs::geom_point(data = funnel, mapping = ggplot2::aes(x = TE.adj, y = seTE, fill = comparison)) +
    jaspGraphs::scale_JASPfill_discrete() +
    ggplot2::labs(fill = gettext("Comparison")) +
    jaspGraphs::scale_x_continuous(name = gettext("Comparison-Adjusted Effect Size"), breaks = xBreaks, labels = xAxis[["labels"]], limits = range(xBreaks), oob = scales::oob_keep) +
    ggplot2::scale_y_reverse(name = gettext("Standard Error"), breaks = rev(yTicks), limits = rev(range(yTicks)), oob = scales::oob_keep) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    # (the right margin keeps the legend labels inside the image, 1 pixel per character of the longest label, at least 25 pixels)
    ggplot2::theme(plot.margin = ggplot2::margin(r = max(25, nchar(as.character(comparisons)))))

  return(plot)
}

# The breaks of the x-axis on the scale of the analysis and their labels on the scale shown. Under the exponential transformation the
# pretty breaks of the log scale give crowded labels (0.14, 0.22, 0.37, 0.61, 1.00, 1.65, ...): the breaks are then ratios, the powers of 2
# (0.25, 0.5, 1, 2, 4) that cover the range, with every 2nd, 3rd, ... power if more than 7 powers are needed. A narrow range (less than
# 4 powers) keeps the pretty breaks.
.nmaFunnelXAxis         <- function(xRange, options) {

  transformation <- .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]])
  breaks         <- jaspGraphs::getPrettyAxisBreaks(xRange)
  axis           <- list(breaks = breaks, labels = format(round(transformation(breaks), 2), trim = TRUE))

  if (options[["transformEffectSize"]] != "exponential")
    return(axis)

  powers <- range(xRange) / log(2)
  step   <- 1
  while (ceiling(powers[2] / step) - floor(powers[1] / step) + 1 > 7)
    step <- step + 1
  exponents <- seq(floor(powers[1] / step), ceiling(powers[2] / step)) * step

  if (length(exponents) < 4)
    return(axis)

  return(list(
    breaks = exponents * log(2),
    labels = vapply(2^exponents, format, character(1), digits = 3, scientific = FALSE)
  ))
}

.nmaFunnelTestsTable    <- function(jaspResults, options) {

  smallStudyEffectsContainer <- .nmaExtractSmallStudyEffectsContainer(jaspResults)

  if (!is.null(smallStudyEffectsContainer[["funnelPlotAsymmetryTestsTable"]]))
    return()

  fits    <- .nmaExtractFits(jaspResults, options)
  funnels <- .nmaFunnelState(jaspResults, options)

  testsTable          <- createJaspTable(gettext("Funnel Plot Asymmetry Tests"))
  testsTable$position <- 2
  testsTable$showSpecifiedColumnsOnly <- TRUE
  testsTable$dependOn(c("funnelPlotEggerTest", "funnelPlotBeggTest", "funnelPlotThompsonSharpTest", "funnelPlotTreatmentOrder", "funnelPlotTreatmentOrderCustom"))
  smallStudyEffectsContainer[["funnelPlotAsymmetryTestsTable"]] <- testsTable

  testsTable$addColumnInfo(name = "test", type = "string", title = "")
  .maAddSubgroupColumn(testsTable, options)
  testsTable$addColumnInfo(name = "stat", type = "number",  title = gettext("Statistic"))
  testsTable$addColumnInfo(name = "df",   type = "integer", title = "df")
  testsTable$addColumnInfo(name = "pval", type = "pvalue",  title = gettext("p"))

  if (is.null(fits))
    return()

  if (.nmaIsFailedSingleFit(fits)) {
    testsTable$setError(.maTryCleanErrorMessages(fits[[1]]))
    return()
  }

  if (.nmaReportFailedResults(testsTable, funnels))
    return()

  tests <- lapply(funnels, .nmaFunnelTests, options = options)
  rows  <- .maSafeRbind(lapply(tests, `[[`, "rows"))
  rows  <- .maSafeOrderAndSimplify(rows, "test", options)

  # a test that cannot be computed shows the reason (e.g., the same precision of all comparisons)
  for (i in seq_along(tests)) {
    for (message in unique(tests[[i]][["errors"]])) {
      text <- gettextf("%1$s: %2$s", paste(names(tests[[i]][["errors"]])[tests[[i]][["errors"]] == message], collapse = ", "), message)
      testsTable$addFootnote(if (options[["subgroup"]] != "") gettextf("%1$s: %2$s", .nmaScopeQuoted(funnels[[i]]), text) else text, symbol = gettext("Warning:"))
    }
  }
  # (meta::metabias() returns no statistic for too few comparisons)
  if (any(vapply(tests, `[[`, logical(1), "noStatistic")))
    testsTable$addFootnote(gettext("Asymmetry tests require at least 10 comparisons."))

  if (!is.null(rows))
    testsTable$setData(rows)

  return()
}

# The rows of the tests of one funnel result, the errors of the tests that fail (named by the test), and whether a test returned no statistic
.nmaFunnelTests         <- function(funnel, options) {

  tests <- c(
    Egger    = options[["funnelPlotEggerTest"]],
    Begg     = options[["funnelPlotBeggTest"]],
    Thompson = options[["funnelPlotThompsonSharpTest"]]
  )
  labels <- c(Egger = gettext("Egger (t)"), Begg = gettext("Begg and Mazumdar (z)"), Thompson = gettext("Thompson and Sharp (t)"))
  tests  <- names(tests)[tests]

  # failed results are reported by a footnote
  if (jaspBase::isTryError(funnel))
    return(list(rows = NULL, errors = character(0), noStatistic = FALSE))

  # the tests of netmeta::funnel(): meta::metabias() of the comparison-adjusted effects (the between-study variance by DerSimonian-Laird)
  adjusted <- suppressWarnings(try(.nmaRunCall(.nmaCallMetagenAdjusted(), NULL, list(funnelData = funnel)), silent = TRUE))
  biases   <- lapply(setNames(tests, tests), function(test) {
    if (jaspBase::isTryError(adjusted)) adjusted else suppressWarnings(try(.nmaRunCall(.nmaCallMetabias(test), NULL, list(adjusted = adjusted)), silent = TRUE))
  })
  failed   <- vapply(biases, jaspBase::isTryError, logical(1))
  errors   <- vapply(biases[failed], .maTryCleanErrorMessages, character(1))
  names(errors) <- labels[names(errors)]

  rows <- do.call(rbind, lapply(tests, function(test) {

    # (tests that cannot be computed are shown without a statistic)
    bias <- biases[[test]]
    stat <- if (failed[[test]] || is.null(bias[["statistic"]])) NA_real_ else bias[["statistic"]]
    df   <- if (failed[[test]] || is.null(bias[["df"]]) || length(bias[["df"]]) == 0) NA_real_ else bias[["df"]]
    pval <- if (failed[[test]] || is.null(bias[["p.value"]])) NA_real_ else bias[["p.value"]]

    return(data.frame(test = labels[[test]], subgroup = attr(funnel, "subgroup"), stat = unname(stat), df = unname(df), pval = unname(pval)))
  }))

  return(list(rows = rows, errors = errors, noStatistic = any(!failed & is.na(rows[["stat"]]))))
}
