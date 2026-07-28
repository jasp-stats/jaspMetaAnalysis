# Forest plot study data.
#
# Extracts, transforms, orders, predicts, and aggregates study-level information.

.forestPlotStudyExtractBaseData        <- function(fit, dataset, options) {

  if (.maIsClassical(options)) {

    if (options[["analysis"]] %in% c("generalizedMetaAnalysis", "mantelHaenszelPeto")) {
      studyWeights <- 1 / fit[["vi"]]
    } else if (.maIsMultilevelMultivariate(options)) {
      studyWeights <- stats::weights(fit, type = "diagonal")
    } else {
      studyWeights <- stats::weights(fit)
    }

    return(data.frame(
      effectSize     = fit[["yi"]],
      standardError  = sqrt(fit[["vi"]]),
      weights        = studyWeights,
      id             = seq_along(fit[["yi"]])
    ))
  }

  if (options[["analysis"]] == "BiBMA") {
    tempDf <- metafor::escalc(
      measure = "OR",
      ai      = dataset[[options[["successesGroup1"]]]],
      n1i     = dataset[[options[["sampleSizeGroup1"]]]],
      ci      = dataset[[options[["successesGroup2"]]]],
      n2i     = dataset[[options[["sampleSizeGroup2"]]]]
    )

    return(data.frame(
      effectSize     = tempDf[["yi"]],
      standardError  = sqrt(tempDf[["vi"]]),
      weights        = dataset[[options[["sampleSizeGroup1"]]]] + dataset[[options[["sampleSizeGroup2"]]]],
      id             = seq_len(nrow(dataset))
    ))
  }

  return(data.frame(
    effectSize     = dataset[[options[["effectSize"]]]],
    standardError  = dataset[[options[["effectSizeStandardError"]]]],
    weights        = 1 / dataset[[options[["effectSizeStandardError"]]]]^2,
    id             = seq_len(nrow(dataset))
  ))
}

.forestPlotStudyAddConfidenceIntervals <- function(dfForest, options) {

  ciZValue <- qnorm((1 - options[["confidenceIntervalsLevel"]]) / 2, lower.tail = FALSE)
  dfForest$lCi <- dfForest$effectSize - ciZValue * dfForest$standardError
  dfForest$uCi <- dfForest$effectSize + ciZValue * dfForest$standardError

  if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
    secondaryCiZValue <- qnorm((1 - options[["forestPlotStudyInformationSecondaryConfidenceIntervalLevel"]]) / 2, lower.tail = FALSE)
    dfForest$lCi2 <- dfForest$effectSize - secondaryCiZValue * dfForest$standardError
    dfForest$uCi2 <- dfForest$effectSize + secondaryCiZValue * dfForest$standardError
  }

  return(dfForest)
}

.forestPlotStudyTransformationColumns  <- function(options) {

  transformColumns <- c("effectSize", "lCi", "uCi")
  if (options[["forestPlotStudyInformationSecondaryConfidenceInterval"]]) {
    transformColumns <- c(transformColumns, "lCi2", "uCi2")
  }

  return(transformColumns)
}

.forestPlotStudyTransformEffectSizes   <- function(dfForest, options) {

  if (options[["transformEffectSize"]] == "none") {
    return(dfForest)
  }

  transformColumns <- .forestPlotStudyTransformationColumns(options)
  dfForest[, transformColumns] <- do.call(
    .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
    list(dfForest[, transformColumns])
  )

  return(dfForest)
}

.forestPlotStudyAdditionalVariables    <- function(options) {

  return(c(
    if (length(options[["forestPlotStudyInformationSelectedVariables"]]) > 0) {
      unlist(options[["forestPlotStudyInformationSelectedVariables"]])
    },
    if (options[["forestPlotStudyInformationOrderBy"]] != "") {
      options[["forestPlotStudyInformationOrderBy"]]
    },
    if (options[["forestPlotStudyInformationAggregateBy"]] != "") {
      options[["forestPlotStudyInformationAggregateBy"]]
    },
    if (options[["forestPlotMappingColor"]] != "") {
      options[["forestPlotMappingColor"]]
    },
    if (options[["forestPlotMappingShape"]] != "") {
      options[["forestPlotMappingShape"]]
    }
  ))
}

.forestPlotStudyBindAdditionalVariables <- function(dfForest, dataset, additionalVariables) {

  if (length(additionalVariables) == 0) {
    return(dfForest)
  }

  return(cbind(dfForest, dataset[, additionalVariables, drop = FALSE]))
}

.forestPlotStudyAggregateData          <- function(dfForest, options, additionalVariables) {

  if (options[["forestPlotStudyInformationAggregateBy"]] == "") {
    return(list(
      forest = dfForest,
      geoms  = NULL
    ))
  }

  return(.forestStudyInformationAggregate(dfForest, options, additionalVariables))
}

.forestPlotStudyOrderData              <- function(dfForest, options) {

  if (options[["forestPlotStudyInformationOrderBy"]] == "") {
    return(dfForest)
  }

  # "Ascending" means values increase top-to-bottom (forest plot convention),
  # which corresponds to decreasing = TRUE in R's order() (row 1 = top of plot).
  dfForest <- dfForest[order(
    dfForest[[options[["forestPlotStudyInformationOrderBy"]]]],
    decreasing = options[["forestPlotStudyInformationOrderAscending"]]
  ), ]

  return(dfForest)
}

.forestPlotStudyAssignCoordinates      <- function(dfForest, dfGeoms = NULL) {

  dfForest$y <- seq(nrow(dfForest))
  if (!is.null(dfGeoms)) {
    dfGeoms <- merge(dfGeoms, dfForest[, colnames(dfForest) %in% c("id", "y")], by = "id")
  }

  return(list(
    forest = dfForest,
    geoms  = dfGeoms
  ))
}

.forestPlotStudyPredictionMergeData    <- function(dfForest) {

  return(dfForest[, !colnames(dfForest) %in% c("effectSize", "standardError", "weights", "lCi", "uCi", "y"), drop = FALSE])
}

.forestPlotStudyBuildPredictions       <- function(fit, dfForest, options) {

  if (!options[["forestPlotStudyInformationPredictedEffects"]]) {
    return(NULL)
  }

  dfForestPrediction <- data.frame(predict(fit))

  if (nrow(dfForestPrediction) == 1) {
    dfForestPrediction <- do.call(rbind, replicate(nrow(dfForest), dfForestPrediction, simplify = FALSE))
  }

  dfForestPrediction$id <- dfForest$id
  dfForestPrediction$y  <- dfForest$y

  dfForestPrediction <- do.call(rbind, lapply(seq_len(nrow(dfForestPrediction)), function(i) {
    with(dfForestPrediction[i, ], .maMakeDiamondDataFrame(est = pred, lCi = pi.lb, uCi = pi.ub, row = y, id = id))
  }))
  dfForestPrediction <- merge(dfForestPrediction, .forestPlotStudyPredictionMergeData(dfForest), by = "id")

  if (options[["transformEffectSize"]] != "none") {
    dfForestPrediction[, "x"] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(dfForestPrediction[, "x"])
    )
  }

  return(dfForestPrediction)
}

.forestPlotBuildStudyInformation       <- function(fit, options){

  if (is.null(fit) || jaspBase::isTryError(fit)) {
    return(NULL)
  }

  # The fit carries the filtered dataset that actually entered the analysis, so
  # the forest plot stays aligned with model-side NA handling and subgroup fits.
  dataset <- attr(fit, "dataset")

  additionalVariables <- .forestPlotStudyAdditionalVariables(options)
  dfForest            <- .forestPlotStudyExtractBaseData(fit, dataset, options)
  dfForest            <- .forestPlotStudyAddConfidenceIntervals(dfForest, options)
  dfForest            <- .forestPlotStudyTransformEffectSizes(dfForest, options)
  dfForest            <- .forestPlotStudyBindAdditionalVariables(dfForest, dataset, additionalVariables)

  dfAggregate         <- .forestPlotStudyAggregateData(dfForest, options, additionalVariables)
  dfForest            <- .forestPlotStudyOrderData(dfAggregate[["forest"]], options)
  dfCoordinates       <- .forestPlotStudyAssignCoordinates(dfForest, dfAggregate[["geoms"]])
  dfForest            <- dfCoordinates[["forest"]]
  dfGeoms             <- dfCoordinates[["geoms"]]
  dfForestPrediction  <- .forestPlotStudyBuildPredictions(fit, dfForest, options)

  return(list(
    forest     = dfForest,
    prediction = dfForestPrediction,
    geoms      = dfGeoms
  ))
}

.forestPlotAggregateVariable           <- function(x) {
  if (length(unique(x)) == 1) {
    return(unique(x))
  } else {
    x <- table(x)
    x <- x[x > 0]
    xNames <- names(x)
    xFreqs <- paste0(" (", x, ")")
    xFreqs[xFreqs == " (1)"] <- ""
    return(paste0(xNames, xFreqs, collapse = ", "))
  }
}

.forestPlotColorKeyColumn             <- function() {
  return(".forestPlotColorKey")
}

.forestPlotAggregateColorValue        <- function(x) {

  x <- as.character(x)
  if (any(is.na(x) | x == "")) {
    return(NA_character_)
  }

  x <- unique(x)
  if (length(x) != 1) {
    return(NA_character_)
  }

  return(x)
}

.forestPlotAddAggregateColorKey       <- function(dfOut, df, options, method) {

  colorVar <- options[["forestPlotMappingColor"]]
  if (colorVar == "" || !colorVar %in% colnames(df)) {
    return(dfOut)
  }

  if (method == "boxplot") {
    dfOut[[.forestPlotColorKeyColumn()]] <- .forestPlotAggregateColorValue(df[[colorVar]])
  } else if (method == "bubbles") {
    dfOut[[.forestPlotColorKeyColumn()]] <- as.character(df[[colorVar]])
  }

  return(dfOut)
}

.forestPlotAggregateEffectSizeSummary  <- function(effectSize) {

  effectSize <- effectSize[is.finite(effectSize)]
  if (length(effectSize) == 0) {
    return(list(
      min    = NA_real_,
      lower  = NA_real_,
      middle = NA_real_,
      upper  = NA_real_,
      max    = NA_real_
    ))
  }

  return(list(
    min    = min(effectSize),
    lower  = stats::quantile(effectSize, 0.25, names = FALSE),
    middle = stats::median(effectSize),
    upper  = stats::quantile(effectSize, 0.75, names = FALSE),
    max    = max(effectSize)
  ))
}

.forestStudyInformationAggregate       <- function(dfForest, options, additionalVariables) {

  # split the data set by the grouping variable
  datasetSplit <- split(dfForest, dfForest[[options[["forestPlotStudyInformationAggregateBy"]]]])

  # add id to each split index
  for (i in seq_along(datasetSplit)) {
    datasetSplit[[i]]$id <- i
  }

  if (options[["forestPlotStudyInformationAggregateMethod"]] == "boxplot") {

    datasetAggregated <- do.call(rbind, lapply(datasetSplit, function(df) {

      effectSizeSummary <- .forestPlotAggregateEffectSizeSummary(df$effectSize)

      # create a base of the geom
      tempDf <- data.frame(
        id      = df$id[1],
        weights = .forestPlotAggregateWeights(df$weights[is.finite(df$effectSize)]),
        min     = effectSizeSummary[["min"]],
        lower   = effectSizeSummary[["lower"]],
        middle  = effectSizeSummary[["middle"]],
        upper   = effectSizeSummary[["upper"]],
        max     = effectSizeSummary[["max"]],
        geom    = "boxplot"
      )

      # add the additional variables
      for (var in additionalVariables) {
        tempDf[[var]] <- .forestPlotAggregateVariable(df[[var]])
      }
      tempDf <- .forestPlotAddAggregateColorKey(tempDf, df, options, "boxplot")

      return(tempDf)
    }))

    # split into study information and geoms
    dfGeoms  <- datasetAggregated
    dfForest <- datasetAggregated[
      , !colnames(datasetAggregated) %in% c(
        "min", "lower", "middle", "upper", "max", "geom", .forestPlotColorKeyColumn()
      ),
      drop = FALSE
    ]


  } else if (options[["forestPlotStudyInformationAggregateMethod"]] == "bubbles") {

    dfForest <- do.call(rbind, lapply(datasetSplit, function(df) {

      tempDf <- data.frame(
        id      = df$id[1],
        weights = .forestPlotAggregateWeights(df$weights)
      )

      # add the additional variables
      for (var in additionalVariables) {
        tempDf[[var]] <- .forestPlotAggregateVariable(df[[var]])
      }

      return(tempDf)
    }))
    dfGeoms  <- do.call(rbind, lapply(datasetSplit, function(df) {

      # create a base of the geom
      tempDf <- data.frame(
        id      = df$id[1],
        x       = df$effectSize,
        weight  = df$weights,
        geom    = "bubbles"
      )

      # add the additional variables
      for (var in additionalVariables) {
        tempDf[[var]] <- .forestPlotAggregateVariable(df[[var]])
      }
      tempDf <- .forestPlotAddAggregateColorKey(tempDf, df, options, "bubbles")

      return(tempDf)
    }))

  }

  return(list(
    forest = dfForest,
    geoms  = dfGeoms
  ))
}

.forestPlotAggregateWeights           <- function(weights) {

  weights <- weights[!is.na(weights)]
  if (length(weights) == 0)
    return(NA_real_)

  return(sum(weights))
}
