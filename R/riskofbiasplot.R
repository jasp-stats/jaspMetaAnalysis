# Risk of Bias Plot analysis.
#
# Visualizes risk-of-bias assessments using summary bar plots and
# traffic light plots via the robvis package.

.robCitation <- "McGuinness LA, Higgins JPT (2021). Risk-of-bias VISualization (robvis): An R package and Shiny web app for visualizing risk-of-bias assessments. Research Synthesis Methods, 12(1), 55-61."

.robDomainOptionNames <- c(
  # ROB2
  "rob2D1", "rob2D2", "rob2D3", "rob2D4", "rob2D5",
  # ROBINS-I
  "robinsID1", "robinsID2", "robinsID3", "robinsID4", "robinsID5",
  "robinsID6", "robinsID7",
  # QUADAS-2
  "quadas2D1", "quadas2D2", "quadas2D3", "quadas2D4"
)

.robBaseDeps <- c(
  "robTool", "studyLabel", "overallJudgment",
  .robDomainOptionNames,
  "colorScheme"
)

RiskOfBiasPlot <- function(jaspResults, dataset, options) {

  ready <- length(.robGetDomainColumns(options)) > 0

  if (ready)
    .robValidateData(dataset, options)

  .robSummaryPlot(jaspResults, dataset, options, ready)
  .robTrafficLightPlot(jaspResults, dataset, options, ready)
}


# ── lookup helpers ───────────────────────────────────────────────────────────

.robGetDomainOptionKeys <- function(options) {
  switch(options[["robTool"]],
    "rob2"    = c("rob2D1", "rob2D2", "rob2D3", "rob2D4", "rob2D5"),
    "robinsI" = c("robinsID1", "robinsID2", "robinsID3", "robinsID4",
                  "robinsID5", "robinsID6", "robinsID7"),
    "quadas2" = c("quadas2D1", "quadas2D2", "quadas2D3", "quadas2D4")
  )
}

.robGetDomainColumns <- function(options) {
  keys <- .robGetDomainOptionKeys(options)
  cols <- vapply(keys, function(k) options[[k]], character(1))
  cols[cols != ""]
}

.robGetToolString <- function(options) {
  switch(options[["robTool"]],
    "rob2"    = "ROB2",
    "robinsI" = "ROBINS-I",
    "quadas2" = "QUADAS-2"
  )
}

.robValidLevels <- function(options) {
  switch(options[["robTool"]],
    "rob2"    = c("low", "some concerns", "high"),
    "robinsI" = c("low", "moderate", "serious", "critical"),
    "quadas2" = c("low", "some concerns", "high")
  )
}


# ── validation ─────────────────────────────────────────────────────────────

.robValidateData <- function(dataset, options) {
  validLevels <- .robValidLevels(options)
  domainCols  <- .robGetDomainColumns(options)
  overallCol  <- options[["overallJudgment"]]
  colsToCheck <- c(domainCols, if (overallCol != "") overallCol)

  for (col in colsToCheck) {
    vals    <- tolower(trimws(as.character(dataset[[col]])))
    invalid <- vals[!is.na(vals) & vals != "" & !(vals %in% validLevels)]
    if (length(invalid) > 0)
      .quitAnalysis(gettextf(
        "Column '%1$s' contains invalid values: %2$s. Expected values for %3$s are: %4$s.",
        col,
        paste(unique(invalid), collapse = ", "),
        .robGetToolString(options),
        paste(validLevels, collapse = ", ")
      ))
  }
}


# ── data assembly ────────────────────────────────────────────────────────────

.robAssembleData <- function(dataset, options) {
  nRows <- nrow(dataset)

  # use generated labels if study label not yet assigned
  if (options[["studyLabel"]] != "") {
    robData <- data.frame(Study = as.character(dataset[[options[["studyLabel"]]]]))
  } else {
    robData <- data.frame(Study = gettextf("Study %1$i", seq_len(nRows)))
  }

  # robvis accesses columns by position index, so we must always include
  # ALL expected domain columns in the exact order the tool expects.
  keys <- .robGetDomainOptionKeys(options)
  for (key in keys) {
    col <- options[[key]]
    if (col != "") {
      robData[[col]] <- as.character(dataset[[col]])
    } else {
      robData[[key]] <- rep(NA_character_, nRows)
    }
  }

  if (options[["overallJudgment"]] != "") {
    robData[["Overall"]] <- as.character(dataset[[options[["overallJudgment"]]]])
  }

  return(robData)
}

.robAddSyntheticOverall <- function(robData) {
  # robvis indexes Overall positionally before honoring its plotting options.
  if (!"Overall" %in% names(robData))
    robData[["Overall"]] <- rep(NA_character_, nrow(robData))

  return(robData)
}

.robDropOverallDomain <- function(plotObj) {
  dropOverall <- function(data) {
    data <- data[is.na(data[["domain"]]) | data[["domain"]] != "Overall", , drop = FALSE]
    if (is.factor(data[["domain"]]))
      data[["domain"]] <- droplevels(data[["domain"]])
    return(data)
  }

  if (is.data.frame(plotObj[["data"]]) && "domain" %in% names(plotObj[["data"]]))
    plotObj[["data"]] <- dropOverall(plotObj[["data"]])

  layers <- plotObj[["layers"]]
  for (i in seq_along(layers)) {
    layerData <- layers[[i]][["data"]]
    if (is.data.frame(layerData) && "domain" %in% names(layerData))
      layers[[i]][["data"]] <- dropOverall(layerData)
  }
  plotObj[["layers"]] <- layers

  return(plotObj)
}


# ── summary plot ─────────────────────────────────────────────────────────────

.robSummaryPlot <- function(jaspResults, dataset, options, ready) {

  if (!options[["summaryPlot"]])
    return()
  if (!is.null(jaspResults[["summaryPlot"]]))
    return()

  summaryDeps <- c(.robBaseDeps, "summaryPlot", "summaryPlotWeighted", "studyWeights")

  if (!ready) {
    plot <- createJaspPlot(title = gettext("Summary"), width = 600, height = 350)
    plot$position <- 1
    plot$dependOn(summaryDeps)
    plot$addCitation(.robCitation)
    jaspResults[["summaryPlot"]] <- plot
    return()
  }

  robData    <- .robAssembleData(dataset, options)
  tool       <- .robGetToolString(options)
  overall    <- options[["overallJudgment"]] != ""
  robvisData <- .robAddSyntheticOverall(robData)
  weighted   <- options[["summaryPlotWeighted"]] && options[["studyWeights"]] != ""

  if (weighted) {
    robvisData[["Weight"]] <- dataset[[options[["studyWeights"]]]]
  }

  plotObj <- try(robvis::rob_summary(
    data     = robvisData,
    tool     = tool,
    overall  = overall,
    weighted = weighted,
    colour   = options[["colorScheme"]]
  ))

  if (inherits(plotObj, "try-error")) {
    plot <- createJaspPlot(title = gettext("Summary"), width = 600, height = 350)
    plot$position <- 1
    plot$dependOn(summaryDeps)
    plot$addCitation(.robCitation)
    plot$setError(plotObj)
    jaspResults[["summaryPlot"]] <- plot
    return()
  }

  # dynamic height: one horizontal bar per domain
  nDomains <- ncol(robData) - 1  # all columns except Study
  height   <- 80 + nDomains * 30

  plot <- createJaspPlot(title = gettext("Summary"), width = 600, height = height)
  plot$position <- 1
  plot$dependOn(summaryDeps)
  plot$addCitation(.robCitation)
  plot$plotObject <- plotObj
  jaspResults[["summaryPlot"]] <- plot
}


# ── traffic light plot ───────────────────────────────────────────────────────

.robTrafficLightPlot <- function(jaspResults, dataset, options, ready) {

  if (!options[["trafficLightPlot"]])
    return()
  if (!is.null(jaspResults[["trafficLightPlot"]]))
    return()

  tlDeps <- c(.robBaseDeps, "trafficLightPlot", "trafficLightPlotPointSize")

  if (!ready) {
    plot <- createJaspPlot(title = gettext("Traffic Light"))
    plot$position <- 2
    plot$dependOn(tlDeps)
    plot$addCitation(.robCitation)
    jaspResults[["trafficLightPlot"]] <- plot
    return()
  }

  robData <- .robAssembleData(dataset, options)
  tool    <- .robGetToolString(options)
  overall <- options[["overallJudgment"]] != ""

  plotObj <- try(robvis::rob_traffic_light(
    data   = .robAddSyntheticOverall(robData),
    tool   = tool,
    colour = options[["colorScheme"]],
    psize  = options[["trafficLightPlotPointSize"]]
  ))

  if (inherits(plotObj, "try-error")) {
    plot <- createJaspPlot(title = gettext("Traffic Light"))
    plot$position <- 2
    plot$dependOn(tlDeps)
    plot$addCitation(.robCitation)
    plot$setError(plotObj)
    jaspResults[["trafficLightPlot"]] <- plot
    return()
  }

  if (!overall)
    plotObj <- .robDropOverallDomain(plotObj)

  # dynamic dimensions: scale with studies (height) and domains (width)
  nStudies <- nrow(robData)
  nDomains <- ncol(robData) - 1  # all columns except Study
  height   <- 225 + nStudies * 30
  width    <- 200 + nDomains * 30

  plot <- createJaspPlot(title = gettext("Traffic Light"), width = width, height = height)
  plot$position <- 2
  plot$dependOn(tlDeps)
  plot$addCitation(.robCitation)
  plot$plotObject <- plotObj
  jaspResults[["trafficLightPlot"]] <- plot
}
