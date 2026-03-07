# Forest plot axis scaling and transformation helpers.
#
# This file contains all functions for transforming effect-size values,
# computing axis ranges/breaks/labels, and handling the special tick-planning
# algorithms for ratio (log-spaced), bounded (probability/correlation), and
# standard (pretty) axis styles.
#
# The three tick styles:
#   "pretty"  -- standard R pretty() ticks on the plotted scale
#   "ratio" / "logScale" -- log-spaced grid planner for exponential / log-odds transforms
#   "bounded" -- ticks at nice round values on the transformed scale, scored
#                for count and equidistance on the original (plotted) scale;
#                used by probability and correlation transforms in labels-only mode

# ── Transformation specification ──────────────────────────────────────────────

.forestPlotTransformationInverse       <- function(transformation) {

  return(switch(
    transformation,
    none                          = function(x) x,
    fishersZToCorrelation         = metafor::transf.rtoz,
    exponential                   = log,
    logOddsToProportions          = metafor::transf.logit,
    logOddsToSmdNormal            = metafor::transf.dtolnor.norm,
    logOddsToSmdLogistic          = metafor::transf.dtolnor.logis,
    smdToLogOddsNormal            = metafor::transf.lnortod.norm,
    smdToLogOddsLogistic          = metafor::transf.lnortod.logis,
    hakstianAndWhalenInverseAlpha = metafor::transf.ahw,
    bonettInverseAlpha            = metafor::transf.abt,
    zToR2                         = NULL,
    smdToCohensU1                 = NULL,
    smdToCohensU2                 = function(x) 2 * stats::qnorm(x),
    smdToCohensU3                 = stats::qnorm,
    smdToCles                     = function(x) sqrt(2) * stats::qnorm(x)
  ))
}
.forestPlotTransformationIsInvertible <- function(transformation) {
  return(is.function(.forestPlotTransformationInverse(transformation)))
}
.forestPlotTransformXAxisLabelsOnly    <- function(options) {
  transformation <- options[["transformEffectSize"]]

  return(
    transformation != "none" &&
    isTRUE(options[["forestPlotAuxiliaryXAxisTransformLabelsOnly"]]) &&
    .forestPlotTransformationIsInvertible(transformation)
  )
}
.forestPlotTransformationMetadata      <- function(transformation) {

  probabilityTransformations <- c(
    "logOddsToProportions",
    "hakstianAndWhalenInverseAlpha",
    "bonettInverseAlpha",
    "zToR2",
    "smdToCohensU1",
    "smdToCohensU2",
    "smdToCohensU3",
    "smdToCles"
  )
  unboundedTransformations <- c(
    "logOddsToSmdNormal",
    "logOddsToSmdLogistic",
    "smdToLogOddsNormal",
    "smdToLogOddsLogistic"
  )

  if (transformation == "none") {
    return(list(
      bounds            = c(-Inf, Inf),
      inverseOpenBounds = c(FALSE, FALSE),
      defaultRange      = c(-1, 1),
      family            = "identity",
      tickStyle         = "pretty"
    ))
  }

  if (transformation == "fishersZToCorrelation") {
    return(list(
      bounds            = c(-1, 1),
      inverseOpenBounds = c(TRUE, TRUE),
      defaultRange      = c(-0.9, 0.9),
      family            = "correlation",
      tickStyle         = "bounded"
    ))
  }

  if (transformation == "exponential") {
    return(list(
      bounds            = c(0, Inf),
      inverseOpenBounds = c(TRUE, FALSE),
      defaultRange      = c(0.5, 2),
      family            = "ratio",
      tickStyle         = "ratio"
    ))
  }

  if (transformation %in% unboundedTransformations) {
    return(list(
      bounds            = c(-Inf, Inf),
      inverseOpenBounds = c(FALSE, FALSE),
      defaultRange      = c(-1, 1),
      family            = "unbounded",
      tickStyle         = if (transformation %in% c("smdToLogOddsNormal", "smdToLogOddsLogistic")) "logScale" else "pretty"
    ))
  }

  if (transformation %in% probabilityTransformations) {
    inverseOpenBounds <- c(TRUE, TRUE)
    defaultRange      <- c(0.1, 0.9)

    if (transformation == "hakstianAndWhalenInverseAlpha") {
      inverseOpenBounds <- c(FALSE, FALSE)
      defaultRange      <- c(0, 1)
    } else if (transformation == "bonettInverseAlpha") {
      inverseOpenBounds <- c(FALSE, TRUE)
    } else if (transformation %in% c("zToR2", "smdToCohensU1")) {
      defaultRange      <- c(0, 1)
    }

    return(list(
      bounds            = c(0, 1),
      inverseOpenBounds = inverseOpenBounds,
      defaultRange      = defaultRange,
      family            = "probability",
      tickStyle         = "bounded"
    ))
  }

  stop(paste0("Unknown effect size transformation: ", transformation))
}
.forestPlotTransformationSpec          <- function(options) {

  transformation <- options[["transformEffectSize"]]
  transform      <- .maGetEffectSizeTransformationOptions(transformation)
  inverse        <- .forestPlotTransformationInverse(transformation)
  metadata       <- .forestPlotTransformationMetadata(transformation)

  return(c(list(
    name              = transformation,
    transform         = transform,
    inverse           = inverse,
    invertible        = is.function(inverse)
  ), metadata))
}

# ── Forward / inverse axis transforms ─────────────────────────────────────────

.forestPlotTransformAxisValues        <- function(values, options) {

  transformationSpec <- .forestPlotTransformationSpec(options)

  if (transformationSpec[["name"]] == "none") {
    return(values)
  }

  transformedValues <- do.call(
    transformationSpec[["transform"]],
    list(values)
  )

  return(.forestPlotClampAxisValues(transformedValues, transformationSpec[["bounds"]]))
}

# Nudge values away from open bounds before applying an inverse transform such
# as logit -> log-odds or r -> Fisher's z.
.forestPlotPrepareInverseAxisValues   <- function(values, transformationSpec, tolerance = 1e-6) {

  values           <- .forestPlotClampAxisValues(values, transformationSpec[["bounds"]], tolerance = tolerance)
  inverseOpenBounds <- transformationSpec[["inverseOpenBounds"]]
  bounds           <- transformationSpec[["bounds"]]

  if (isTRUE(inverseOpenBounds[1]) && is.finite(bounds[1])) {
    values[values <= bounds[1]] <- bounds[1] + tolerance
  }
  if (isTRUE(inverseOpenBounds[2]) && is.finite(bounds[2])) {
    values[values >= bounds[2]] <- bounds[2] - tolerance
  }

  return(values)
}
.forestPlotInverseAxisValues          <- function(values, transformationSpec) {

  if (!is.function(transformationSpec[["inverse"]])) {
    return(values)
  }

  inverseValues <- do.call(
    transformationSpec[["inverse"]],
    list(.forestPlotPrepareInverseAxisValues(values, transformationSpec))
  )

  inverseValues[abs(inverseValues) < 1e-10] <- 0

  return(inverseValues)
}

# Clamp display values to the transformed domain and clean up floating-point
# near-misses at the transformation boundaries.
.forestPlotClampAxisValues            <- function(values, bounds, tolerance = 1e-10) {

  if (!is.finite(bounds[1]) && !is.finite(bounds[2])) {
    values[abs(values) < tolerance] <- 0
    return(values)
  }

  if (is.finite(bounds[1])) {
    values[values < bounds[1] & values > bounds[1] - tolerance] <- bounds[1]
    values <- pmax(values, bounds[1])
  }
  if (is.finite(bounds[2])) {
    values[values > bounds[2] & values < bounds[2] + tolerance] <- bounds[2]
    values <- pmin(values, bounds[2])
  }

  values[abs(values) < tolerance] <- 0

  return(values)
}

# ── Manual tick parsing ───────────────────────────────────────────────────────

.forestPlotParseManualXAxisTickToken  <- function(token) {

  token <- trimws(token, which = "both")
  if (token == "") {
    return(NA_real_)
  }

  tokenSplit <- strsplit(token, "/", fixed = TRUE)[[1]]
  if (length(tokenSplit) == 2) {
    numerator   <- suppressWarnings(as.numeric(trimws(tokenSplit[1], which = "both")))
    denominator <- suppressWarnings(as.numeric(trimws(tokenSplit[2], which = "both")))

    if (anyNA(c(numerator, denominator)) || denominator == 0) {
      return(NA_real_)
    }

    return(numerator / denominator)
  }
  if (length(tokenSplit) > 2) {
    return(NA_real_)
  }

  return(suppressWarnings(as.numeric(token)))
}
.forestPlotParseManualXAxisTickValue  <- function(token) {

  value <- .forestPlotParseManualXAxisTickToken(token)
  if (is.na(value)) {
    stop(gettextf("'%1$s' could not be converted into a numeric value for the custom x-axis ticks.", token), call. = FALSE)
  }

  return(value)
}
.forestPlotParseManualXAxisTicks      <- function(x) {

  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")

  tokens <- strsplit(x, ",", fixed = TRUE)[[1]]
  tokens <- trimws(tokens, which = "both")
  tokens <- tokens[tokens != ""]

  if (length(tokens) == 0) {
    stop(gettext("At least one manual x-axis tick must be specified."), call. = FALSE)
  }

  values <- vapply(tokens, .forestPlotParseManualXAxisTickValue, numeric(1))

  return(list(
    values = values,
    labels = tokens
  ))
}
.forestPlotResolveManualXAxisTicks    <- function(displayRange, options, transformationSpec) {

  if (!isTRUE(options[["forestPlotAuxiliarySetXAxisTicks"]])) {
    return(NULL)
  }

  parsedTicks <- .forestPlotParseManualXAxisTicks(options[["forestPlotAuxiliarySetXAxisTicksValues"]])
  tickValues  <- .forestPlotClampAxisValues(parsedTicks[["values"]], transformationSpec[["bounds"]])
  insideRange <- tickValues >= displayRange[1] & tickValues <= displayRange[2]
  tickValues  <- tickValues[insideRange]
  tickLabels  <- parsedTicks[["labels"]][insideRange]

  if (length(tickValues) == 0) {
    stop(gettext("At least one manual x-axis tick must fall within the x-axis limits."), call. = FALSE)
  }

  tickData <- data.frame(
    value = tickValues,
    label = tickLabels,
    order = seq_along(tickValues)
  )
  tickData <- tickData[order(tickData$value, tickData$order), , drop = FALSE]
  tickData <- tickData[!duplicated(tickData$value), , drop = FALSE]

  return(list(
    breaks = tickData$value,
    labels = tickData$label
  ))
}

# ── Standard pretty range / break helpers ─────────────────────────────────────

.forestPlotDisplayRangeStep           <- function(displayRange, transformationSpec) {

  if (diff(displayRange) > 0) {
    return(diff(displayRange) / 4)
  }

  return(switch(
    transformationSpec[["family"]],
    probability = 0.1,
    correlation = 0.1,
    ratio       = max(displayRange[1] * 0.25, 0.5),
    0.5
  ))
}
.forestPlotNormalizeDisplayRange      <- function(displayRange, transformationSpec) {

  displayRange <- sort(.forestPlotClampAxisValues(displayRange, transformationSpec[["bounds"]]))
  if (diff(displayRange) == 0) {
    step         <- .forestPlotDisplayRangeStep(displayRange, transformationSpec)
    displayRange <- displayRange + c(-step, step)
  }

  return(displayRange)
}
.forestPlotStandardPrettyDisplayRange <- function(displayRange, transformationSpec) {

  displayRange <- .forestPlotNormalizeDisplayRange(displayRange, transformationSpec)
  prettyBreaks <- pretty(displayRange, n = 6)
  prettyBreaks <- .forestPlotClampAxisValues(prettyBreaks, transformationSpec[["bounds"]])
  prettyBreaks <- prettyBreaks[is.finite(prettyBreaks)]

  if (length(prettyBreaks) == 0) {
    return(displayRange)
  }

  return(range(prettyBreaks))
}
.forestPlotStandardPrettyDisplayBreaks <- function(displayRange, transformationSpec, includeLimits = FALSE) {

  displayRange  <- .forestPlotNormalizeDisplayRange(displayRange, transformationSpec)
  displayBreaks <- pretty(displayRange, n = 6)
  displayBreaks <- .forestPlotClampAxisValues(displayBreaks, transformationSpec[["bounds"]])
  displayBreaks <- displayBreaks[displayBreaks >= displayRange[1] & displayBreaks <= displayRange[2]]

  if (includeLimits) {
    displayBreaks <- c(displayRange[1], displayBreaks, displayRange[2])
  }

  displayBreaks <- sort(unique(displayBreaks))
  if (length(displayBreaks) == 0) {
    displayBreaks <- displayRange
  }

  return(displayBreaks)
}

# ── Transformation-aware tick grid (ratio / logScale) ─────────────────────────

.forestPlotTransformationTickSteps    <- function(transformationSpec) {

  return(switch(
    transformationSpec[["tickStyle"]],
    ratio       = c(log(2), log(3) / 2, log(2) / 2, log(3), log(10) / 2, log(10)),
    logScale    = c(log(2), log(3) / 2, log(2) / 2, log(3), log(10) / 2, log(10)),
    numeric(0)
  ))
}
.forestPlotTransformationReferenceValue <- function(transformationSpec) {

  return(switch(
    transformationSpec[["tickStyle"]],
    ratio       = 1,
    logScale    = 0,
    NA_real_
  ))
}
.forestPlotTransformationDisplayGrid  <- function(displayRange, transformationSpec, step, maxIterations = 10) {

  transform <- transformationSpec[["transform"]]
  bounds    <- transformationSpec[["bounds"]]

  if (transformationSpec[["tickStyle"]] == "logScale") {
    return(seq(
      floor(displayRange[1] / step) * step,
      ceiling(displayRange[2] / step) * step,
      by = step
    ))
  }

  if (is.function(transformationSpec[["inverse"]])) {
    originalRange <- sort(.forestPlotInverseAxisValues(displayRange, transformationSpec))
    if (!all(is.finite(originalRange))) {
      return(numeric(0))
    }

    originalGrid <- seq(
      floor(originalRange[1] / step) * step,
      ceiling(originalRange[2] / step) * step,
      by = step
    )
  } else {
    gridLimit <- max(abs(step) * 4, 1)

    for (i in seq_len(maxIterations)) {
      originalGrid <- seq(-gridLimit, gridLimit, by = step)
      displayGrid  <- sort(unique(.forestPlotClampAxisValues(
        do.call(transform, list(originalGrid)),
        bounds
      )))
      displayGrid  <- displayGrid[is.finite(displayGrid)]

      if (length(displayGrid) > 0 &&
          min(displayGrid) <= displayRange[1] + 1e-10 &&
          max(displayGrid) >= displayRange[2] - 1e-10) {
        break
      }

      gridLimit <- gridLimit * 2
    }
  }

  displayGrid <- sort(unique(.forestPlotClampAxisValues(
    do.call(transform, list(originalGrid)),
    bounds
  )))

  return(displayGrid[is.finite(displayGrid)])
}
.forestPlotUniqueAxisValues           <- function(values, tolerance = 1e-10) {

  values <- sort(values[is.finite(values)])
  if (length(values) <= 1) {
    return(values)
  }

  keep <- c(TRUE, diff(values) > tolerance)

  return(values[keep])
}
.forestPlotTransformationDisplayWindow <- function(displayGrid, displayRange) {

  lowerIndex <- max(which(displayGrid <= displayRange[1] + 1e-10))
  upperIndex <- min(which(displayGrid >= displayRange[2] - 1e-10))

  if (!is.finite(lowerIndex) || !is.finite(upperIndex) || lowerIndex > upperIndex) {
    return(NULL)
  }

  return(displayGrid[lowerIndex:upperIndex])
}
.forestPlotTransformationBreakScaleValues <- function(displayBreaks, transformationSpec) {

  if (transformationSpec[["tickStyle"]] == "ratio" && is.function(transformationSpec[["inverse"]])) {
    return(.forestPlotInverseAxisValues(displayBreaks, transformationSpec))
  }

  return(displayBreaks)
}
.forestPlotTrimBoundaryBreaks         <- function(displayBreaks, transformationSpec, step, minimumFraction = 0.5) {

  displayBreaks <- .forestPlotUniqueAxisValues(displayBreaks)
  if (length(displayBreaks) <= 2) {
    return(displayBreaks)
  }

  scaleValues <- .forestPlotTransformationBreakScaleValues(displayBreaks, transformationSpec)

  if (length(scaleValues) >= 3 && abs(scaleValues[2] - scaleValues[1]) < step * minimumFraction) {
    displayBreaks <- displayBreaks[-2]
    scaleValues   <- scaleValues[-2]
  }
  if (length(scaleValues) >= 3 && abs(scaleValues[length(scaleValues)] - scaleValues[length(scaleValues) - 1]) < step * minimumFraction) {
    displayBreaks <- displayBreaks[-(length(displayBreaks) - 1)]
  }

  return(displayBreaks)
}
.forestPlotTransformationCandidateBreaks <- function(displayRange, transformationSpec, step, includeLimits = FALSE) {

  displayGrid <- .forestPlotTransformationDisplayGrid(displayRange, transformationSpec, step)
  displayGrid <- displayGrid[displayGrid >= transformationSpec[["bounds"]][1] & displayGrid <= transformationSpec[["bounds"]][2]]

  if (length(displayGrid) == 0) {
    return(NULL)
  }

  if (includeLimits) {
    displayBreaks <- c(
      displayRange[1],
      displayGrid[displayGrid > displayRange[1] & displayGrid < displayRange[2]],
      displayRange[2]
    )
    displayBreaks <- .forestPlotTrimBoundaryBreaks(displayBreaks, transformationSpec, step)

    return(list(
      range  = displayRange,
      breaks = .forestPlotUniqueAxisValues(displayBreaks)
    ))
  }

  displayWindow <- .forestPlotTransformationDisplayWindow(displayGrid, displayRange)
  if (is.null(displayWindow) || length(displayWindow) == 0) {
    return(NULL)
  }

  return(list(
    range  = c(displayWindow[1], displayWindow[length(displayWindow)]),
    breaks = .forestPlotUniqueAxisValues(displayWindow)
  ))
}
.forestPlotTransformationBreakScore   <- function(candidate, displayRange, transformationSpec, includeLimits = FALSE, orderIndex = 1) {

  displayBreaks <- candidate[["breaks"]]
  candidateRange <- candidate[["range"]]
  referenceValue <- .forestPlotTransformationReferenceValue(transformationSpec)
  targetBreaks   <- if (includeLimits) 5 else 6
  rangeSpan      <- max(diff(displayRange), .Machine$double.eps)

  countPenalty <- abs(length(displayBreaks) - targetBreaks)
  if (length(displayBreaks) < 4) {
    countPenalty <- countPenalty + 4
  }
  if (length(displayBreaks) > 7) {
    countPenalty <- countPenalty + 2
  }

  referencePenalty <- 0
  if (is.finite(referenceValue) &&
      referenceValue >= candidateRange[1] &&
      referenceValue <= candidateRange[2] &&
      !any(abs(displayBreaks - referenceValue) < 1e-10)) {
    referencePenalty <- 1
  }

  rangePenalty <- sum(abs(candidateRange - displayRange)) / rangeSpan

  return(countPenalty * 5 + referencePenalty * 2 + rangePenalty + orderIndex * 0.01)
}
.forestPlotTransformationBreakPlan    <- function(displayRange, transformationSpec, includeLimits = FALSE) {

  displayRange <- .forestPlotNormalizeDisplayRange(displayRange, transformationSpec)
  tickSteps    <- .forestPlotTransformationTickSteps(transformationSpec)
  candidates   <- lapply(seq_along(tickSteps), function(i) {
    candidate <- .forestPlotTransformationCandidateBreaks(
      displayRange        = displayRange,
      transformationSpec = transformationSpec,
      step               = tickSteps[i],
      includeLimits      = includeLimits
    )

    if (is.null(candidate)) {
      return(NULL)
    }

    candidate[["score"]] <- .forestPlotTransformationBreakScore(
      candidate          = candidate,
      displayRange       = displayRange,
      transformationSpec = transformationSpec,
      includeLimits      = includeLimits,
      orderIndex         = i
    )

    return(candidate)
  })
  candidates <- Filter(Negate(is.null), candidates)

  if (length(candidates) == 0) {
    return(NULL)
  }

  bestIndex <- which.min(vapply(candidates, function(candidate) candidate[["score"]], numeric(1)))

  return(candidates[[bestIndex]])
}

# ── Display axis resolver ─────────────────────────────────────────────────────

# Single resolver returns list(range, breaks, labels) for the display x-axis.
# Dispatches to transformation-specific break planners or standard pretty.
.forestPlotResolveDisplayXAxis        <- function(displayValues, options, transformationSpec) {

  includeLimits <- options[["forestPlotAuxiliarySetXAxisLimit"]]

  # 1. Base display range
  if (includeLimits) {
    displayRange <- sort(.forestPlotClampAxisValues(
      c(options[["forestPlotAuxiliarySetXAxisLimitLower"]],
        options[["forestPlotAuxiliarySetXAxisLimitUpper"]]),
      transformationSpec[["bounds"]]
    ))
  } else if (length(displayValues) == 0 || !all(is.finite(range(displayValues, na.rm = TRUE)))) {
    displayRange <- transformationSpec[["defaultRange"]]
  } else {
    displayRange <- range(displayValues, na.rm = TRUE)
  }

  # 2. Manual ticks override everything
  manualTicks <- .forestPlotResolveManualXAxisTicks(displayRange, options, transformationSpec)
  if (!is.null(manualTicks)) {
    if (!includeLimits)
      displayRange <- .forestPlotStandardPrettyDisplayRange(displayRange, transformationSpec)
    return(list(range = displayRange, breaks = manualTicks[["breaks"]], labels = manualTicks[["labels"]]))
  }

  if (!all(is.finite(displayRange)))
    return(list(range = displayRange, breaks = displayRange, labels = NULL))

  # 3. Dispatch by tick style
  tickStyle <- transformationSpec[["tickStyle"]]

  if (tickStyle %in% c("ratio", "logScale")) {
    breakPlan <- .forestPlotTransformationBreakPlan(displayRange, transformationSpec, includeLimits)
    if (!is.null(breakPlan)) {
      finalRange <- if (includeLimits) displayRange else breakPlan[["range"]]
      return(list(range = finalRange, breaks = breakPlan[["breaks"]], labels = NULL))
    }
  }

  # 4. Standard pretty fallback
  if (!includeLimits)
    displayRange <- .forestPlotStandardPrettyDisplayRange(displayRange, transformationSpec)
  displayBreaks <- .forestPlotStandardPrettyDisplayBreaks(displayRange, transformationSpec, includeLimits)
  if (length(displayBreaks) == 0)
    displayBreaks <- displayRange

  return(list(range = displayRange, breaks = displayBreaks, labels = NULL))
}

# ── Bounded axis spec (probability / correlation labels-only mode) ────────────

# Bounded transforms (probability, correlation) in labels-only mode.
# Works on the ORIGINAL (plotted) scale: range from data via pretty(),
# ticks at nice transformed-scale values scored for count and equidistance
# on the original scale.  Avoids the inverse-transform inflation that
# occurs when bounded display-scale values near 0/1 map to extreme
# original-scale positions.
.forestPlotBoundedAxisSpec            <- function(xValuesRaw, transformationSpec, options) {

  transform     <- transformationSpec[["transform"]]
  bounds        <- transformationSpec[["bounds"]]
  ref           <- if (transformationSpec[["family"]] == "correlation") 0 else 0.5
  openLower     <- isTRUE(transformationSpec[["inverseOpenBounds"]][1])
  openUpper     <- isTRUE(transformationSpec[["inverseOpenBounds"]][2])
  includeLimits <- options[["forestPlotAuxiliarySetXAxisLimit"]]

  # 1. Axis range on original (plotted) scale
  if (includeLimits) {
    # User limits are on the displayed (transformed) scale; invert to original
    displayLimits <- sort(.forestPlotClampAxisValues(
      c(options[["forestPlotAuxiliarySetXAxisLimitLower"]],
        options[["forestPlotAuxiliarySetXAxisLimitUpper"]]),
      bounds
    ))
    axisRange <- sort(.forestPlotInverseAxisValues(displayLimits, transformationSpec))
  } else {
    rawRange <- range(xValuesRaw[is.finite(xValuesRaw)], na.rm = TRUE)
    if (!all(is.finite(rawRange))) rawRange <- c(-1, 1)
    axisRange <- range(pretty(rawRange, n = 6))
  }

  # 2. Transform range edges to display scale for tick search
  displayBounds <- sort(.forestPlotClampAxisValues(
    do.call(transform, list(axisRange)),
    bounds
  ))

  # 3. Manual ticks
  manualTicks <- .forestPlotResolveManualXAxisTicks(displayBounds, options, transformationSpec)
  if (!is.null(manualTicks)) {
    breaks  <- .forestPlotInverseAxisValues(manualTicks[["breaks"]], transformationSpec)
    inRange <- is.finite(breaks) & breaks >= axisRange[1] & breaks <= axisRange[2]
    return(list(range = axisRange, breaks = breaks[inRange], labels = manualTicks[["labels"]][inRange]))
  }

  # 4. Generate and score candidate tick sets
  stepSizes    <- c(0.1, 0.2, 0.25, 0.05)
  targetBreaks <- if (includeLimits) 5 else 6
  bestResult   <- NULL
  bestScore    <- Inf

  for (step in stepSizes) {
    grid <- seq(
      ceiling((displayBounds[1] - ref) / step) * step + ref,
      floor((displayBounds[2] - ref) / step) * step + ref,
      by = step
    )
    grid <- grid[
      (if (openLower) grid > bounds[1] else grid >= bounds[1]) &
      (if (openUpper) grid < bounds[2] else grid <= bounds[2]) &
      is.finite(grid)
    ]
    if (length(grid) < 2) next

    originalGrid <- .forestPlotInverseAxisValues(grid, transformationSpec)
    valid        <- is.finite(originalGrid) &
                    originalGrid >= axisRange[1] - 1e-10 &
                    originalGrid <= axisRange[2] + 1e-10
    grid         <- grid[valid]
    originalGrid <- originalGrid[valid]
    if (length(grid) < 2) next

    n     <- length(grid)
    score <- abs(n - targetBreaks) * 5
    if (n < 3) score <- score + 20
    if (n > 8) score <- score + 10
    if (!any(abs(grid - ref) < 1e-10) &&
        ref >= displayBounds[1] && ref <= displayBounds[2])
      score <- score + 2

    # Penalise non-uniform spacing on original scale
    if (n >= 3) {
      diffs <- diff(originalGrid)
      if (all(diffs > 0) && mean(diffs) > 0)
        score <- score + (sd(diffs) / mean(diffs)) * 10
    }

    if (score < bestScore) {
      bestScore  <- score
      bestResult <- list(display = grid, original = originalGrid)
    }
  }

  if (is.null(bestResult)) {
    prettyBreaks <- pretty(axisRange, n = 6)
    prettyBreaks <- prettyBreaks[prettyBreaks >= axisRange[1] & prettyBreaks <= axisRange[2]]
    return(list(range = axisRange, breaks = prettyBreaks, labels = ggplot2::waiver()))
  }

  return(list(
    range  = axisRange,
    breaks = bestResult[["original"]],
    labels = .forestPlotFormatAxisValues(bestResult[["display"]], transformationSpec)
  ))
}

# ── Axis label formatting ─────────────────────────────────────────────────────

.forestPlotAxisLabelDigits           <- function(values, transformationSpec) {

  values       <- sort(unique(values[is.finite(values)]))
  differences  <- diff(values)
  differences  <- differences[differences > 0]
  minimumDigits <- switch(
    transformationSpec[["family"]],
    probability = 2,
    correlation = 2,
    ratio       = 2,
    0
  )

  if (length(differences) == 0) {
    return(minimumDigits)
  }

  return(min(
    8,
    max(minimumDigits, ceiling(-log10(min(differences))) + 1)
  ))
}
.forestPlotFormatAxisValues          <- function(values, transformationSpec) {

  if (length(values) == 0) {
    return(character(0))
  }

  values  <- .forestPlotClampAxisValues(values, transformationSpec[["bounds"]])
  digits  <- .forestPlotAxisLabelDigits(values, transformationSpec)
  labels  <- .forestPlotFormatAxisValuesDigits(values, digits)

  while (anyDuplicated(labels[is.finite(values)]) && digits < 8) {
    digits <- digits + 1
    labels <- .forestPlotFormatAxisValuesDigits(values, digits)
  }

  return(labels)
}
.forestPlotFormatAxisValuesDigits    <- function(values, digits) {

  return(vapply(values, function(value) {
    if (!is.finite(value)) {
      return("")
    }

    if (abs(value) >= 10000 || (abs(value) > 0 && abs(value) < 10^(-(digits + 1)))) {
      label <- formatC(value, format = "g", digits = min(8, digits + 2))
    } else {
      label <- formatC(value, format = "f", digits = digits, drop0trailing = TRUE)
    }

    if (grepl("\\.", label)) {
      label <- sub("0+$", "", label)
      label <- sub("\\.$", "", label)
    }

    if (label == "-0") {
      label <- "0"
    }

    return(label)
  }, character(1)))
}
