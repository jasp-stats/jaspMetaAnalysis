# Classical meta-analysis shared helpers.
#
# Formatting, safe result assembly, table columns, transformations, and shared messages.

.maVariableNames                      <- function(varNames, variables) {

  # sometimes interactions are missformated as "."
  varNames <- gsub(".", ":", varNames, fixed = TRUE)

  return(sapply(varNames, function(varName){

    if (varName %in% c("intrcpt", "intercept"))
      return(gettext("Intercept"))

    for (vn in unique(variables)) {
      inf <- regexpr(vn, varName, fixed = TRUE)

      if (inf[1] != -1) {
        varName <- paste0(
          substr(varName, 0, inf[1] - 1),
          substr(varName, inf[1], inf[1] + attr(inf, "match.length") - 1),
          " (",
          substr(varName, inf[1] + attr(inf, "match.length"), nchar(varName))
        )
      }

    }

    varName <- gsub(":", paste0(")", jaspBase::interactionSymbol), varName, fixed = TRUE)
    varName <- paste0(varName, ")")
    varName <- gsub(" ()", "", varName, fixed = TRUE)
    varName <- gsub(" (/", "/", varName, fixed = TRUE)

    return(varName)

  }))
}

.maPrintQTest                         <- function(fit, type = NULL) {

  # rma.glmm uses QE.Wld/QE.LRT instead of QE/QEp
  if (inherits(fit, "rma.glmm")) {

    if (is.null(type) || type == "Wald") {
      heterogeneityName <- if (fit[["p"]] > 1) gettext("Residual heterogeneity (Wald)") else gettext("Heterogeneity (Wald)")
      if (!.maIsFiniteScalar(fit[["QE.Wld"]]))
        return(gettextf("%1$s: not available", heterogeneityName))
      return(sprintf(
        paste0("%1$s: Q\U2091(%2$i) = ", if (fit[["QE.Wld"]] < 1e5) "%3$.2f" else "%3$.3g", ", %4$s"),
        heterogeneityName,
        fit[["QE.df"]],
        fit[["QE.Wld"]],
        .maPrintPValue(fit[["QEp.Wld"]])
      ))
    } else if (type == "LRT") {
      heterogeneityName <- if (fit[["p"]] > 1) gettext("Residual heterogeneity (LRT)") else gettext("Heterogeneity (LRT)")
      if (!.maIsFiniteScalar(fit[["QE.LRT"]]))
        return(gettextf("%1$s: not available", heterogeneityName))
      return(sprintf(
        paste0("%1$s: Q\U2091(%2$i) = ", if (fit[["QE.LRT"]] < 1e5) "%3$.2f" else "%3$.3g", ", %4$s"),
        heterogeneityName,
        fit[["QE.df"]],
        fit[["QE.LRT"]],
        .maPrintPValue(fit[["QEp.LRT"]])
      ))
    }
  }

  if (fit[["p"]] > 1) {
    heterogeneityName <- gettextf("Residual heterogeneity")
  } else {
    heterogeneityName <- gettextf("Heterogeneity")
  }

  return(sprintf(
    paste0("%1$s: Q(%2$i) = ", if (fit[["QE"]] < 1e5) "%3$.2f" else "%3$.3g", ", %4$s"),
    heterogeneityName,
    fit[["k"]] - fit[["p"]],
    fit[["QE"]],
    .maPrintPValue(fit[["QEp"]])
  ))
}

.maPrintModerationTest                <- function(fit, options, parameter) {

  out      <- .maOmnibusTest(fit, options, parameter)
  outPrint <- .maPrintTermTest(out, testStatistic = TRUE)

  if (.maIsMetaregressionHeterogeneity(options)) {
    if (parameter == "effectSize")
      return(gettextf("Moderation (effect size): %1$s", outPrint))
    else if (parameter == "heterogeneity")
      return(gettextf("Moderation (heterogeneity): %1$s", outPrint))
  } else {
    if (parameter == "effectSize")
      return(gettextf("Moderation: %1$s", outPrint))
  }
}

.maPrintHeterogeneityEstimate         <- function(fit, options, digits, parameter) {

  out <- .maComputePooledHeterogeneityPlot(fit, options, parameter)

  if (!options[["confidenceIntervals"]] || is.na(out$lCi) || is.na(out$uCi)) {
    return(sprintf(paste0("%1$s  = ", "%2$.", digits, "f"), out$par, out$est))
  }

  return(sprintf(paste0(
    "%1$s  = ",
    "%2$.", digits, "f",
    " [",
    "%3$.", digits, "f",
    ", ",
    "%4$.", digits, "f",
    "]"
    ), out$par, out$est, out$lCi, out$uCi))
}

.maPrintTermTest                      <- function(out, testStatistic = TRUE) {

  if (testStatistic) {
    if (!is.null(out[["df2"]])) {
      return(sprintf("F(%1$s, %2$s) = %3$.2f, %4$s", .maPrintDf(out[["df1"]]), .maPrintDf(out[["df2"]]), out[["stat"]], .maPrintPValue(out[["pval"]])))
    } else {
      return(sprintf("Q\U2098(%1$s) = %2$.2f, %3$s", .maPrintDf(out[["df1"]]), out[["stat"]], .maPrintPValue(out[["pval"]])))
    }
  } else {
    return(.maPrintPValue(out[["pval"]]))
  }
}

.maPrintCoefficientTest               <- function(out, testStatistic = TRUE) {

  if (testStatistic) {
    if (!is.null(out[["df"]])) {
      return(sprintf("t(%1$s) = %2$.2f, %3$s", .maPrintDf(out[["df"]]), out[["stat"]], .maPrintPValue(out[["pval"]])))
    } else {
      return(sprintf("z = %1$.2f, %2$s", out[["stat"]], .maPrintPValue(out[["pval"]])))
    }
  } else {
    return(.maPrintPValue(out[["pval"]]))
  }
}

.maPrintDf                            <- function(df) {
  if (.maIsWholenumber(df)) {
    return(sprintf("%1$i", round(df)))
  } else {
    return(sprintf("%1$.2f", df))
  }
}

.maPrintPValue                        <- function(pValue) {
  if (pValue < 0.001) {
    return("p < 0.001")
  } else {
    return(sprintf("p = %.3f", pValue))
  }
}

.maIsFiniteScalar                     <- function(x) {
  return(is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x))
}

.maPrintEstimateAndInterval           <- function(est, lCi, uCi, digits) {
  return(sprintf(paste0(
    .maAddSpaceForPositiveValue(est), "%1$.", digits, "f",
    " [",
    .maAddSpaceForPositiveValue(lCi), "%2$.", digits, "f",
    ", ",
    .maAddSpaceForPositiveValue(uCi), "%3$.", digits, "f",
    "]"), est, lCi, uCi))
}

.maPrintPredictionInterval            <- function(est, lCi, uCi, digits) {
  return(sprintf(paste0(
    "   ", "%1$.", digits, "f",
    " [",
    .maAddSpaceForPositiveValue(lCi), "%2$.", digits, "f",
    ", ",
    .maAddSpaceForPositiveValue(uCi), "%3$.", digits, "f",
    "]"), est, lCi, uCi))
}

.maSafeRbind                          <- function(dfs) {

  # this function allows combining data.frames with different columns
  # the main issue is that some models might be missing coefficients/terms,
  # or complete fit failure, as such, simple rbind might misaligned the grouped output
  # importantly, the order of the output data.frame
  # does not matter as order is determined by the table itself

  dfs <- dfs[!sapply(dfs, function(x) is.null(x) || length(x) == 0 || nrow(x) == 0)]
  if (length(dfs) == 0)
    return(NULL)

  # gather all colnames
  colnamesUnique <- unique(unlist(lapply(dfs, colnames)))

  # add missing columns and reorder
  for (i in seq_along(dfs)) {
    colnamesMissing <- setdiff(colnamesUnique, colnames(dfs[[i]]))
    if (length(colnamesMissing) > 0) {
      for (col in colnamesMissing) {
        dfs[[i]][[col]] <- NA
      }
    }
    dfs[[i]] <- dfs[[i]][,colnamesUnique,drop=FALSE]
  }

  df <- do.call(rbind, dfs)
  return(df)
}

.maSafeOrderAndSimplify               <- function(df, columnName, options) {

  # this function allows ordering and simplifying subgroup output tables
  # the main issue is that some models might be missing coefficients/terms etc
  # as such, simple ordering of the output might misaligned the grouped output

  if (is.null(df) ||
      length(df) == 0 ||
      is.null(nrow(df)) ||
      nrow(df) == 0)
    return(df)

  # drop the grouping column if no subgroups requested
  if (options[["subgroup"]] == "") {
    df <- df[,colnames(df) != "subgroup", drop = FALSE]
    return(df)
  }

  if (!columnName %in% colnames(df))
    return(df)

  # remove rows with NA in the grouping column
  if (anyNA(df[[columnName]])) {
    warning(sprintf("The grouping column '%s' contains NA values. These rows will be removed.", columnName))
    df <- df[!is.na(df[[columnName]]),,drop=FALSE]
  }

  # get the grouping order
  groupingOrder <- unique(df[[columnName]])

  # get the order of the grouping
  newDf <- list()
  for (i in seq_along(groupingOrder)) {
    newDf[[i]] <- df[df[[columnName]] == groupingOrder[i],,drop=FALSE]
  }
  newDf <- do.call(rbind, newDf)

  # simplify the grouping column
  newDf[[columnName]][duplicated(newDf[[columnName]])] <- NA

  return(newDf)
}

.maAddCiColumn                  <- function(tempTable, options) {

  if (options[["confidenceIntervals"]]) {
    overtitleCi <- gettextf("%s%% CI", 100 * options[["confidenceIntervalsLevel"]])
    tempTable$addColumnInfo(name = "lCi", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    tempTable$addColumnInfo(name = "uCi", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }

  return(tempTable)
}

.maAddPiColumn                  <- function(tempTable, options) {

  if (options[["predictionIntervals"]]) {
    overtitleCi <- gettextf("%s%% PI", 100 * options[["confidenceIntervalsLevel"]])
    tempTable$addColumnInfo(name = "lPi", title = gettext("Lower"), type = "number", overtitle = overtitleCi)
    tempTable$addColumnInfo(name = "uPi", title = gettext("Upper"), type = "number", overtitle = overtitleCi)
  }

  return(tempTable)
}

.maAddSeColumn                  <- function(tempTable, options, noTransformation = FALSE) {

  if (!options[["standardErrors"]])
    return(tempTable)

  if (noTransformation || options[["transformEffectSize"]] == "none") {
    tempTable$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
  }

  return(tempTable)
}

.maAddSubgroupColumn            <- function(tempTable, options) {

  if (options[["subgroup"]] != "")
    tempTable$addColumnInfo(name = "subgroup", type = "string", title = gettext("Subgroup"))

  return(tempTable)
}

.maAddSpaceForPositiveValue           <- function(value) {
  if (value >= 0)
    return(" ")
  else
    return("")
}

.maGetDigitsBeforeDecimal             <- function(x) {

  dNAs <- is.na(x)
  dPos <- floor(log10(x[!dNAs & x >= 0])) + 1
  dNeg <- floor(log10(-x[!dNAs & x < 0])) + 2

  # account for missing zeros
  dPos[dPos <= 1] <- 1
  dNeg[dNeg <= 1] <- 2 # (+2 because of minus sign)

  nDigits <- rep(NA, length(x))
  nDigits[!dNAs & x >= 0] <- dPos
  nDigits[!dNAs & x < 0]  <- dNeg

  return(nDigits)
}

.maIsWholenumber                      <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

.maFormatDigits                       <- function(x, digits) {

  xOut <- rep("", length(x))
  xNa  <- is.na(x)

  # compute the character width
  nDigits    <- .maGetDigitsBeforeDecimal(x[!xNa])
  nDigitsMax <- max(nDigits, na.rm = TRUE)
  addDigits  <- nDigitsMax - nDigits

  # add the missing widths
  xOut[!xNa] <- sprintf(paste0("%1$s%2$.", digits,"f"), sapply(addDigits, function(i) paste(rep(" ", i), collapse = "")), x[!xNa])
  xOut[ xNa] <- paste(rep(" ", nDigitsMax + 1 + digits), collapse = "")

  return(xOut)
}

.maGetVariableColumnType              <- function(variable, options) {

  if (.maIsMultilevelMultivariate(options)) {
    randomVariables <- .mammExtractRandomVariableNames(options)
  } else {
    randomVariables <- NULL
  }

  if (variable %in% c(options[["effectSize"]], options[["effectSizeStandardError"]], "samplingVariance",
                      options[["predictors"]][options[["predictors.types"]] == "scale"], randomVariables[["scale"]], randomVariables[["ordinal"]])) {
    return("number")
  } else if (variable %in% c(options[["predictors"]][options[["predictors.types"]] == "nominal"], options[["clustering"]], randomVariables[["nominal"]])) {
    return("string")
  } else {
    return("string")
  }
}

.maExtractAndFormatPrediction         <- function(out) {

  # save as a data.frame
  out <- data.frame(out)

  # TODO: decide whether those should be added as NAs or CIs
  # - if NAs, need to be adjusted for in the rest of the code / GUI
  if (!"pi.lb" %in% colnames(out)) {
    out$pi.lb <- NA
    out$pi.ub <- NA
    #out$pi.lb <- out$ci.lb
    #out$pi.ub <- out$ci.ub
  }

  # rename into a consistent format
  out           <- out[,c("pred", "se", "ci.lb", "ci.ub", "pi.lb", "pi.ub")]
  colnames(out) <- c("est", "se", "lCi", "uCi", "lPi", "uPi")

  return(out)
}

.maGetSqrtTransformationSeDeltaMethod <- function(estimate, estimate_se) {
  estimate_se / (2 * sqrt(estimate))
}

.maAddLowDdfWarning                    <- function(table, fit, options) {
  lowDdfWarning <- .maLowDdfWarning(fit, options)

  if (!is.null(lowDdfWarning))
    table$addFootnote(lowDdfWarning, symbol = gettext("Warning:"))
}

.maLowDdfWarning                       <- function(fit, options) {
  if (!.maHasDdfBelow(fit, threshold = 4))
    return(NULL)

  if (.maIsClustered(options) && isTRUE(options[["clusteringUseClubSandwich"]]))
    return(gettext("Cluster-robust inference has very low effective degrees of freedom. When Satterthwaite df < 4, p-values should be interpreted with extreme caution."))

  if (!.maIsClustered(options) && .maGetFixedEffectTestOptions(options) == "knha")
    return(gettext("Knapp-Hartung inference is based on very few residual degrees of freedom; results can be sensitive and should be interpreted cautiously."))

  return(NULL)
}

.maHasDdfBelow                         <- function(fit, threshold) {
  ddf <- unlist(lapply(fit, .maExtractFitDdf), use.names = FALSE)
  ddf <- as.numeric(ddf)
  ddf <- ddf[is.finite(ddf)]

  return(length(ddf) > 0 && any(ddf < threshold))
}

.maExtractFitDdf                       <- function(fit) {
  if (jaspBase::isTryError(fit))
    return(NULL)

  return(c(
    .maExtractDdf(fit),
    fit[["QMdf"]][2],
    fit[["QSdf"]][2]
  ))
}

.maTryCleanErrorMessages               <- function(message) {
  # probably more messages will be gathered over time
  if (grepl("singular matrix", message))
    return(gettextf("The model estimation failed with the following message: %1$s. Please, consider simplifying the model.", message))
  if (grepl("Could not obtain the cluster-robust variance-covariance matrix", message))
    return(gettext("The cluster-robust standard errors could not be computed. Please, consider modifying the clustering settings in the 'Advanced' section."))
  if (grepl("Fewer than two estimates", message))
    return(gettext("Fewer than two estimates."))
  if (grepl("Must specify the 'weights' argument when method=", message))
    return(gettext("The selected 'Method' requires specification of the 'Weights' option in the 'Advanced' section."))
  if (grepl("Cannot fit ML model", message))
    return(gettext("The GLMM could not be estimated for the selected model. This can happen with sparse event counts, risk differences, random study effects, or a moderator structure that is too complex for the data. Try simplifying the moderator model, using the fixed study-effects GLMM, changing the effect-size measure if appropriate, or centering/scaling continuous moderators.\n\nMetafor reported: Cannot fit ML model."))

  return(message)
}
