# Funnel-plot tables and result extraction.
#
# Builds estimates, asymmetry-test, fail-safe-N tables, and extraction helpers.

.fpPlotEstimatesTable           <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["funnelParametersTable"]]) || options[["funnelUnderH1Parameters"]] != "estimated")
    return()

  # estimates table
  funnelParametersTable          <- createJaspTable(gettext("H₁ Funnel Parameter Estimates"))
  funnelParametersTable$position <- 2
  funnelParametersTable$dependOn(c(.fpDependencies, "funnelUnderH1Parameters", "method", "funnelUnderH1EstimatesTable"))
  jaspResults[["funnelParametersTable"]] <- funnelParametersTable

  if (options[["split"]] != "")
    funnelParametersTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
  funnelParametersTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")

  overtitleMu <- gettext("Estimate \U03BC")
  funnelParametersTable$addColumnInfo(name = "muEst", title = gettext("Estimate"),        type = "number", overtitle = overtitleMu)
  funnelParametersTable$addColumnInfo(name = "muLCI", title = gettextf("Lower 95%% CI"),  type = "number", overtitle = overtitleMu)
  funnelParametersTable$addColumnInfo(name = "muUCI", title = gettextf("Upper 95%% CI"),  type = "number", overtitle = overtitleMu)
  funnelParametersTable$addColumnInfo(name = "muP",   title = gettext("p"),               type = "pvalue", overtitle = overtitleMu)

  if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
    overtitleTau <- gettext("Estimate \U1D70F")
    funnelParametersTable$addColumnInfo(name = "tauEst", title = gettext("Estimate"),         type = "number", overtitle = overtitleTau)
    funnelParametersTable$addColumnInfo(name = "tauLCI", title = gettextf("Lower 95%% CI"),   type = "number", overtitle = overtitleTau)
    funnelParametersTable$addColumnInfo(name = "tauUCI", title = gettextf("Upper 95%% CI"),   type = "number", overtitle = overtitleTau)
    funnelParametersTable$addColumnInfo(name = "tauP",   title = gettext("p"),                type = "pvalue", overtitle = overtitleTau)
  }

  if (!.fpReady(options))
    return()

  if (options[["split"]] == "") {

    fit        <- jaspResults[["fitState"]]$object
    fitSummary <- .fpExtractFitEstimates(fit, options)
    if (jaspBase::isTryError(fit))
      funnelParametersTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = gettext("The funnel plot parameter estimation failed with the following error: "))

  } else {

    fits       <- jaspResults[["fitState"]]$object
    fitSummary <- do.call(rbind, lapply(fits, function(fit) {

      tempFitSummary <- .fpExtractFitEstimates(fit, options)
      if (jaspBase::isTryError(fit))
        funnelParametersTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = gettext("The funnel plot parameter estimation failed with the following error: "))

      return(tempFitSummary)
    }))
    fitSummary <- data.frame(split = names(fits), fitSummary)

  }

  funnelParametersTable$setData(fitSummary)

  return()
}

.fpTrimAndFillEstimatesTable    <- function(jaspResults, dataset, options) {

  trimAndFillContainer <- .fpGetTrimAndFillContainer(jaspResults)

  if (!is.null(trimAndFillContainer[["trimAndFillTable"]]))
    return()

  # Trim and Fill Estimates Table
  trimAndFillTable          <- createJaspTable(gettext("Trim and Fill Parameter Estimates"))
  trimAndFillTable$dependOn(c(.fpDependencies, "trimAndFillEstimatesTable"))
  trimAndFillContainer[["trimAndFillTable"]] <- trimAndFillTable

  if (options[["split"]] != "")
    trimAndFillTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
  trimAndFillTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")

  trimAndFillTable$addColumnInfo(name = "missingK", title = gettext("Missing Estimates"), type = "integer", overtitle = gettext("Trim and Fill"))
  if (options[["trimAndFillEstimator"]] == "R0") {
    trimAndFillTable$addColumnInfo(name = "missingP", title = gettext("p"), type = "pvalue",  overtitle = gettext("Trim and Fill"))
  }

  overtitleMu <- gettext("Adjusted Estimate \U03BC")
  trimAndFillTable$addColumnInfo(name = "muEst", title = gettext("Estimate"),        type = "number", overtitle = overtitleMu)
  trimAndFillTable$addColumnInfo(name = "muLCI", title = gettextf("Lower 95%% CI"),  type = "number", overtitle = overtitleMu)
  trimAndFillTable$addColumnInfo(name = "muUCI", title = gettextf("Upper 95%% CI"),  type = "number", overtitle = overtitleMu)
  trimAndFillTable$addColumnInfo(name = "muP",   title = gettext("p"),               type = "pvalue", overtitle = overtitleMu)

  if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
    overtitleTau <- gettext("Adjusted Estimate \U1D70F")
    trimAndFillTable$addColumnInfo(name = "tauEst", title = gettext("Estimate"),         type = "number", overtitle = overtitleTau)
    trimAndFillTable$addColumnInfo(name = "tauLCI", title = gettextf("Lower 95%% CI"),   type = "number", overtitle = overtitleTau)
    trimAndFillTable$addColumnInfo(name = "tauUCI", title = gettextf("Upper 95%% CI"),   type = "number", overtitle = overtitleTau)
    trimAndFillTable$addColumnInfo(name = "tauP",   title = gettext("p"),                type = "pvalue", overtitle = overtitleTau)
  }


  if (!.fpReady(options))
    return()

  if (options[["split"]] == "") {

    fit        <- jaspResults[["trimAndFillState"]]$object
    fitSummary <- .fpExtractTrimAndFillEstimates(fit, options)
    if (jaspBase::isTryError(fit))
      trimAndFillTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = gettext("The funnel plot parameter estimation failed with the following error: "))

  } else {

    fits       <- jaspResults[["trimAndFillState"]]$object
    fitSummary <- do.call(rbind, lapply(fits, function(fit) {

      tempFitSummary <- .fpExtractTrimAndFillEstimates(fit, options)
      if (jaspBase::isTryError(fit))
        trimAndFillTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = gettext("The funnel plot parameter estimation failed with the following error: "))

      return(tempFitSummary)
    }))
    fitSummary <- data.frame(split = names(fits), fitSummary)

  }

  trimAndFillTable$setData(fitSummary)

  return()
}

.fpTestFunnelPlotAsymmetryTests <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["funnelPlotAsymmetryTests"]])) {
    funnelAsymetryTests <- createJaspContainer(title = gettext("Funnel Plot Asymmetry Tests"))
    funnelAsymetryTests$dependOn(c(.fpDependencies, "funnelPlotAsymmetryTests"))
    funnelAsymetryTests$position <- 3
    jaspResults[["funnelAsymetryTests"]] <- funnelAsymetryTests
  } else {
    funnelAsymetryTests <- jaspResults[["funnelAsymetryTests"]]
  }

  ### create table for each test

  # meta-regression
  if (options[["funnelPlotAsymmetryTests"]] && is.null(funnelAsymetryTests[["metaRegressionTable"]])) {

    metaRegressionTable <- createJaspTable(gettext("Meta-Regression Test for Funnel Plot Asymmetry"))
    metaRegressionTable$position <- 1
    metaRegressionTable$dependOn("funnelPlotAsymmetryTestsMetaRegression")
    funnelAsymetryTests[["metaRegressionTable"]] <- metaRegressionTable

    if (options[["split"]] != "")
      metaRegressionTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
    metaRegressionTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")
    metaRegressionTable$addColumnInfo(name = "z", title = gettext("z"), type = "number", overtitle = gettext("Asymmetry Test"))
    metaRegressionTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue", overtitle = gettext("Asymmetry Test"))
    metaRegressionTable$addColumnInfo(name = "est", title = gettext("Estimate"),       type = "number", overtitle = gettext("Limit Estimate \U03BC"))
    metaRegressionTable$addColumnInfo(name = "lCI", title = gettextf("Lower 95%% CI"), type = "number", overtitle = gettext("Limit Estimate \U03BC"))
    metaRegressionTable$addColumnInfo(name = "uCI", title = gettextf("Upper 95%% CI"), type = "number", overtitle = gettext("Limit Estimate \U03BC"))

    if (.fpReady(options)) {
      if (options[["split"]] == "") {

        fit        <- jaspResults[["fitState"]]$object
        fitTest    <- try(metafor::regtest(fit))
        fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "metaRegression")

        if (jaspBase::isTryError(fit))
          metaRegressionTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = .fpAsymmetryTestErrorMessage())
        else if (jaspBase::isTryError(fitTest))
          metaRegressionTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage())

        metaRegressionTable$setData(fitSummary)

      } else {

        fits         <- jaspResults[["fitState"]]$object
        fitSummaries <- do.call(rbind, lapply(seq_along(fits), function(i) {

          fitTest    <- try(metafor::regtest(fits[[i]]))
          fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "metaRegression")
          fitSummary$split <- names(fits)[i]

          if (jaspBase::isTryError(fits[[i]]))
            metaRegressionTable$addFootnote(.fpMetaforTranslateErrorMessage(fits[[i]]), symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))
          else if (jaspBase::isTryError(fitTest))
            metaRegressionTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))

          return(fitSummary)
        }))

        metaRegressionTable$setData(fitSummaries)

      }
    }
  }

  # weighted regression
  if (options[["funnelPlotAsymmetryTestsWeightedRegression"]] && is.null(funnelAsymetryTests[["weightedRegressionTable"]])) {

    weightedRegressionTable <- createJaspTable(gettext("Weighted Regression Test for Funnel Plot Asymmetry"))
    weightedRegressionTable$position <- 2
    weightedRegressionTable$dependOn("funnelPlotAsymmetryTestsWeightedRegression")
    funnelAsymetryTests[["weightedRegressionTable"]] <- weightedRegressionTable

    if (options[["split"]] != "")
      weightedRegressionTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
    weightedRegressionTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")
    weightedRegressionTable$addColumnInfo(name = "t",  title = gettext("t"),  type = "number",  overtitle = gettext("Asymmetry Test"))
    weightedRegressionTable$addColumnInfo(name = "df", title = gettext("df"), type = "integer", overtitle = gettext("Asymmetry Test"))
    weightedRegressionTable$addColumnInfo(name = "p",  title = gettext("p"),  type = "pvalue",  overtitle = gettext("Asymmetry Test"))
    weightedRegressionTable$addColumnInfo(name = "est", title = gettext("Estimate"),       type = "number", overtitle = gettext("Limit Estimate \U03BC"))
    weightedRegressionTable$addColumnInfo(name = "lCI", title = gettextf("Lower 95%% CI"), type = "number", overtitle = gettext("Limit Estimate \U03BC"))
    weightedRegressionTable$addColumnInfo(name = "uCI", title = gettextf("Upper 95%% CI"), type = "number", overtitle = gettext("Limit Estimate \U03BC"))

    if (.fpReady(options)) {
      if (options[["split"]] == "") {

        fit        <- jaspResults[["fitState"]]$object
        fitTest    <- try(metafor::regtest(fit, model = "lm"))
        fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "weightedRegression")

        if (jaspBase::isTryError(fit))
          weightedRegressionTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = .fpAsymmetryTestErrorMessage())
        else if (jaspBase::isTryError(fitTest))
          weightedRegressionTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage())

        weightedRegressionTable$setData(fitSummary)

      } else {

        fits         <- jaspResults[["fitState"]]$object
        fitSummaries <- do.call(rbind, lapply(seq_along(fits), function(i) {

          fitTest    <- try(metafor::regtest(fits[[i]], model = "lm"))
          fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "weightedRegression")
          fitSummary$split <- names(fits)[i]

          if (jaspBase::isTryError(fits[[i]]))
            weightedRegressionTable$addFootnote(.fpMetaforTranslateErrorMessage(fits[[i]]), symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))
          else if (jaspBase::isTryError(fitTest))
            weightedRegressionTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))

          return(fitSummary)
        }))

        weightedRegressionTable$setData(fitSummaries)

      }
    }
  }

  # rank correlation
  if (options[["funnelPlotAsymmetryTestsRankCorrelation"]] && is.null(funnelAsymetryTests[["rankCorrelationTable"]])) {

    rankCorrelationTable <- createJaspTable(gettext("Rank Correlation Test for Funnel Plot Asymmetry"))
    rankCorrelationTable$position <- 3
    rankCorrelationTable$dependOn("funnelPlotAsymmetryTestsRankCorrelation")
    funnelAsymetryTests[["rankCorrelationTable"]] <- rankCorrelationTable

    if (options[["split"]] != "")
      rankCorrelationTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
    rankCorrelationTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")
    rankCorrelationTable$addColumnInfo(name = "tau",   title = gettext("\U1D70F"),   type = "number")
    rankCorrelationTable$addColumnInfo(name = "p",     title = gettext("p"),         type = "pvalue")

    if (.fpReady(options)) {

      if (options[["split"]] == "") {

        fit        <- jaspResults[["fitState"]]$object
        fitTest    <- try(metafor::ranktest(fit))
        fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "rankCorrelation")

        if (jaspBase::isTryError(fit))
          rankCorrelationTable$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = .fpAsymmetryTestErrorMessage())
        else if (jaspBase::isTryError(fitTest))
          rankCorrelationTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage())
        else
          fitSummary$k <- fit$k

        rankCorrelationTable$setData(fitSummary)

      } else {

        fits         <- jaspResults[["fitState"]]$object
        fitSummaries <- do.call(rbind, lapply(seq_along(fits), function(i) {

          fitTest    <- try(metafor::ranktest(fits[[i]]))
          fitSummary <- .fpExtractAsymmetryTest(fitTest, testType = "rankCorrelation")
          fitSummary$split <- names(fits)[i]

          if (jaspBase::isTryError(fits[[i]])) {
            fitSummary$k <- NA
            rankCorrelationTable$addFootnote(.fpMetaforTranslateErrorMessage(fits[[i]]), symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))
          } else if (jaspBase::isTryError(fitTest)) {
            fitSummary$k <- fits[[i]]$k
            rankCorrelationTable$addFootnote(fitTest, symbol = .fpAsymmetryTestErrorMessage(names(fits)[i]))
          } else {
            fitSummary$k <- fits[[i]]$k
          }

          return(fitSummary)
        }))

        rankCorrelationTable$setData(fitSummaries)

      }
    }
  }

  return()
}

.fpFailSafeNTable               <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["failSafeN"]]))
    return()

  if (is.null(jaspResults[["failSafeN"]])) {
    failSafeN <- createJaspContainer(title = gettext("Fail-Safe N"))
    failSafeN$dependOn(c(.fpDependencies, "failSafeN", "failSafeNRosenthal", "failSafeNOrwin", "failSafeNRosenberg", "failSafeNGeneral", "failSafeNGeneralExact", "failSafeNAlpha",  "failSafeNTarget"))
    failSafeN$position <- 5
    jaspResults[["failSafeN"]] <- failSafeN
  }

  # create the output table
  failSafeNTable <- createJaspTable(gettext("Fail-Safe N Summary Table"))
  failSafeNTable$position <- 1
  failSafeN[["failSafeNTable"]] <- failSafeNTable

  if (options[["split"]] != "")
    failSafeNTable$addColumnInfo(name = "split", title = options[["split"]], type = "string")
  failSafeNTable$addColumnInfo(name = "k",     title = gettext("Estimates"), type = "integer")
  overtitle <- gettext("Fail-Safe N")
  if (options[["failSafeNRosenthal"]])
    failSafeNTable$addColumnInfo(name = "nRosenthal",    title = gettext("Rosenthal"), type = "integer", overtitle = overtitle)
  if (options[["failSafeNOrwin"]])
    failSafeNTable$addColumnInfo(name = "nOrwin",        title = gettext("Orwin"),     type = "integer", overtitle = overtitle)
  if (options[["failSafeNRosenberg"]])
    failSafeNTable$addColumnInfo(name = "nRosenberg",    title = gettext("Rosenberg"), type = "integer", overtitle = overtitle)
  if (options[["failSafeNGeneral"]])
    failSafeNTable$addColumnInfo(name = "nGeneral",      title = gettext("General"),   type = "integer", overtitle = overtitle)


  if (.fpReady(options)) {

    if (options[["split"]] == "") {

      out <- data.frame(
        k = nrow(na.omit(dataset[,c(options[["effectSize"]], options[["effectSizeStandardError"]])]))
      )

      if (options[["failSafeNRosenthal"]])
        out$nRosenthal <- .fpTryGetFailSafeN(dataset, "", options, failSafeNTable, "Rosenthal")
      if (options[["failSafeNOrwin"]])
        out$nOrwin     <- .fpTryGetFailSafeN(dataset, "", options, failSafeNTable, "Orwin")
      if (options[["failSafeNRosenberg"]])
        out$nRosenberg <- .fpTryGetFailSafeN(dataset, "", options, failSafeNTable, "Rosenberg")
      if (options[["failSafeNGeneral"]])
        out$nGeneral   <- .fpTryGetFailSafeN(dataset, "", options, failSafeNTable, "General")

      failSafeNTable$setData(out)

    } else {

      splitLevels <- unique(dataset[[options[["split"]]]])
      out <- do.call(rbind, lapply(splitLevels, function(splitLevel) {

        tempOut <- data.frame(
          k     = nrow(na.omit(dataset[dataset[[options[["split"]]]] == splitLevel,c(options[["effectSize"]], options[["effectSizeStandardError"]])])),
          split = splitLevel
        )

        if (options[["failSafeNRosenthal"]])
          tempOut$nRosenthal <- .fpTryGetFailSafeN(dataset, splitLevel, options, failSafeNTable, "Rosenthal")
        if (options[["failSafeNOrwin"]])
          tempOut$nOrwin     <- .fpTryGetFailSafeN(dataset, splitLevel, options, failSafeNTable, "Orwin")
        if (options[["failSafeNRosenberg"]])
          tempOut$nRosenberg <- .fpTryGetFailSafeN(dataset, splitLevel, options, failSafeNTable, "Rosenberg")
        if (options[["failSafeNGeneral"]])
          tempOut$nGeneral   <- .fpTryGetFailSafeN(dataset, splitLevel, options, failSafeNTable, "General")

        return(tempOut)

      }))

      failSafeNTable$setData(out)

    }
  }

  return()
}

.fpComputeFunnelDf              <- function(seSeq, mean, heterogeneity, funnelLevels) {
  dfs <- list()

  # funnels
  for (i in seq_along(funnelLevels)) {
    tempZ <- qnorm(funnelLevels[i], lower.tail = FALSE)
    dfs[[i]] <- data.frame(
      x = c(rev(mean - tempZ * sqrt(heterogeneity^2 + seSeq^2)), mean + tempZ * sqrt(heterogeneity^2 + seSeq^2)),
      y = c(rev(seSeq), seSeq),
      p = 2 * funnelLevels[i],
      lvl = 1 - 2 * funnelLevels[i]
    )
  }

  # add a center line
  dfs[[length(dfs) + 1]] <- data.frame(
    x   = c(mean, mean),
    y   = range(seSeq)
  )

  return(dfs)
}

.fpAsymmetryTestErrorMessage    <- function(level = NULL) {
  if (is.null(level))
    return(gettext("The funnel plot asymmetry test failed with the following error: "))
  else
    return(gettextf("The funnel plot asymmetry test at level %1$s failed with the following error: ", level))
}

.fpMetaforTranslateErrorMessage <- function(fit) {
  if (grepl("did not converge", fit))
    return(gettext("The meta-analytic model did not converge. Try modifying the 'Method' option."))
  else if (grepl("Stopped because k = 1", fit))
    return(gettext("The method is not available with only one observation."))
  else
    return(fit)
}

.fpExtractAsymmetryTest         <- function(fitTest, testType) {
  if (testType == "metaRegression") {
    return(data.frame(
      k   = if (jaspBase::isTryError(fitTest)) NA else fitTest$fit$k, # nobs will be fixed in the next release
      z   = if (jaspBase::isTryError(fitTest)) NA else fitTest$zval,
      p   = if (jaspBase::isTryError(fitTest)) NA else fitTest$pval,
      est = if (jaspBase::isTryError(fitTest)) NA else fitTest$est,
      lCI = if (jaspBase::isTryError(fitTest)) NA else fitTest$ci.lb,
      uCI = if (jaspBase::isTryError(fitTest)) NA else fitTest$ci.ub
    ))
  } else if (testType == "weightedRegression") {
    return(data.frame(
      k   = if (jaspBase::isTryError(fitTest)) NA else nobs(fitTest$fit),
      t   = if (jaspBase::isTryError(fitTest)) NA else fitTest$zval,
      df  = if (jaspBase::isTryError(fitTest)) NA else fitTest$dfs,
      p   = if (jaspBase::isTryError(fitTest)) NA else fitTest$pval,
      est = if (jaspBase::isTryError(fitTest)) NA else fitTest$est,
      lCI = if (jaspBase::isTryError(fitTest)) NA else fitTest$ci.lb,
      uCI = if (jaspBase::isTryError(fitTest)) NA else fitTest$ci.ub
    ))
  } else if (testType == "rankCorrelation") {
    return(data.frame(
      tau = if (jaspBase::isTryError(fitTest)) NA else fitTest$tau,
      p   = if (jaspBase::isTryError(fitTest)) NA else fitTest$pval
    ))
  }
}

.fpExtractFitEstimates          <- function(fit, options) {

  if (jaspBase::isTryError(fit)) {
    fitSummary <- data.frame(k = NA, muEst = NA, muLCI = NA, muUCI = NA, muP = NA)

    if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
      fitSummary$tauEst <- NA
      fitSummary$tauLCI <- NA
      fitSummary$tauUCI <- NA
      fitSummary$tauP   <- NA
    }

    return(fitSummary)
  }

  fitSummary <- data.frame(
    k        = fit$k,
    muEst    = fit$b[1],
    muLCI    = fit$ci.lb,
    muUCI    = fit$ci.ub,
    muP      = fit$pval
  )

  if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
    tempTau <- try(data.frame(confint(fit)$random)[2,])
    if (jaspBase::isTryError(tempTau)) {
      fitSummary$tauEst <- fit[["tau2"]]
      fitSummary$tauLCI <- NA
      fitSummary$tauUCI <- NA
      fitSummary$tauP   <- fit$QEp
    } else {
      fitSummary$tauEst <- tempTau$estimate
      fitSummary$tauLCI <- tempTau$ci.lb
      fitSummary$tauUCI <- tempTau$ci.ub
      fitSummary$tauP   <- fit$QEp
    }
  }

  return(fitSummary)
}

.fpExtractTrimAndFillEstimates  <- function(fit, options) {

  if (jaspBase::isTryError(fit)) {
    fitSummary <- data.frame(k = NA, missingK = NA, muEst = NA, muLCI = NA, muUCI = NA, muP = NA)

    if (options[["trimAndFillEstimator"]] == "R0")
      fitSummary$missingP <- NA

    if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
      fitSummary$tauEst <- NA
      fitSummary$tauLCI <- NA
      fitSummary$tauUCI <- NA
      fitSummary$tauP   <- NA
    }

    return(fitSummary)
  }

  fitSummary <- data.frame(
    k        = fit$k - fit$k0,
    missingK = fit$k0,
    muEst    = fit$b[1],
    muLCI    = fit$ci.lb,
    muUCI    = fit$ci.ub,
    muP      = fit$pval
  )

  if (options[["trimAndFillEstimator"]] == "R0") {
    fitSummary$missingP <- fit$p.k0
  }

  if (!.maGetMethodOptions(options) %in% c("EE", "FE")) {
    tempTau <- data.frame(confint(fit)$random)[2,]
    fitSummary$tauEst <- tempTau$estimate
    fitSummary$tauLCI <- tempTau$ci.lb
    fitSummary$tauUCI <- tempTau$ci.ub
    fitSummary$tauP   <- fit$QEp
  }

  return(fitSummary)
}

.fpGetTrimAndFillContainer      <- function(jaspResults) {

  if (is.null(jaspResults[["trimAndFillContainer"]])) {
    trimAndFillContainer <- createJaspContainer(title = gettext("Trim and Fill"))
    trimAndFillContainer$dependOn(c(
      .fpDependencies, "method", "trimAndFillEstimator", "trimAndFill"
    ))
    trimAndFillContainer$position <- 4
    jaspResults[["trimAndFillContainer"]] <- trimAndFillContainer
  } else {
    trimAndFillContainer <- jaspResults[["trimAndFillContainer"]]
  }

  return(trimAndFillContainer)
}

.fpTryGetFailSafeN              <- function(dataset, split, options, table, type) {

  input <- list(
    x   = if (split == "") dataset[[options[["effectSize"]]]]              else dataset[[options[["effectSize"]]]][dataset[[options[["split"]]]] == split],
    sei = if (split == "") dataset[[options[["effectSizeStandardError"]]]] else dataset[[options[["effectSizeStandardError"]]]][dataset[[options[["split"]]]] == split],
    type   = type,
    method = .maGetMethodOptions(options),
    exact  = options[["failSafeNGeneralExact"]]
  )

  if (type == "Rosenthal") {
    input$alpha <- options[["failSafeNAlpha"]]
  } else if (type == "orwin") {
    input$target <- options[["failSafeNTarget"]]
  } else if (type == "Rosenberg") {
    input$alpha <- options[["failSafeNAlpha"]]
  } else if (type == "General") {
    input$alpha <- options[["failSafeNAlpha"]]
    input$target <- options[["failSafeNTarget"]]
  }

  fit <- try(do.call(metafor::fsn, input))

  if (jaspBase::isTryError(fit)) {
    table$addFootnote(.fpMetaforTranslateErrorMessage(fit), symbol = gettextf(
      "The %1$s fail-safe N calculation %2$sfailed with the following error: ",
      type,
      if (split == "") "" else gettextf("for split %1$s ", split)))
    return(NA)
  } else {
    return(fit$fsnum)
  }
}
