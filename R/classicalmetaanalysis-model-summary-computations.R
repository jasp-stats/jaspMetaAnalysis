# Classical meta-analysis model-summary computations.
#
# Computes pooled effects, heterogeneity, omnibus tests, fit measures, and table rows.

# Pooled effects ----

.maComputePooledEffect             <- function(fit, options, returnRaw = FALSE) {

  # prediction for effect size of a location-scale models without effect size moderator does not work (compute it manually)
  if (!.maIsMetaregressionEffectSize(options) && .maIsMetaregressionHeterogeneity(options)) {

    predictedHeterogeneity <- .maComputePooledHeterogeneity(fit, options)
    predictedEffect        <- data.frame(
      pred  = fit$beta[1],
      se    = fit$se[1],
      ddf   = .maExtractDdf(fit)[1],
      ci.lb = fit$ci.lb[1],
      ci.ub = fit$ci.ub[1],
      pi.lb = fit$beta[1] - 1.96 * sqrt(fit$se[1]^2 + predictedHeterogeneity[["est"]][1]^2),
      pi.ub = fit$beta[1] + 1.96 * sqrt(fit$se[1]^2 + predictedHeterogeneity[["est"]][1]^2)
    )

  } else {

    predictInput <- list(
      object = fit,
      level  = 100 * options[["confidenceIntervalsLevel"]]
    )

    if (.maIsMetaregressionHeterogeneity(options)) {
      predictInput$newmods  <- t(colMeans(model.matrix(fit)$location))
      predictInput$newscale <- t(colMeans(model.matrix(fit)$scale))
    } else if (.maIsMetaregressionEffectSize(options)) {
      predictInput$newmods  <- t(colMeans(model.matrix(fit)))
    }

    if (!is.null(predictInput$newmods) && options[["effectSizeModelIncludeIntercept"]])
      predictInput$newmods <- predictInput$newmods[, -1, drop=FALSE]

    if (!is.null(predictInput$newscale) && options[["heterogeneityModelIncludeIntercept"]])
      predictInput$newscale <- predictInput$newscale[, -1, drop=FALSE]

    if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {
      tauLevelsMatrix <- .mammExtractTauLevels(fit)
      predictInput$tau2.levels   <- tauLevelsMatrix[["tau2.levels"]]
      predictInput$gamma2.levels <- tauLevelsMatrix[["gamma2.levels"]]

      if (.maIsMetaregressionEffectSize(options))
        predictInput$newmods <- do.call(rbind, lapply(1:nrow(tauLevelsMatrix), function(i) predictInput$newmods))
    }

    predictedEffect <- do.call(predict, predictInput)
  }

  # remove the non-requested heterogeneity levels
  if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && !options[["predictionIntervals"]])
    predictedEffect <- predictedEffect[1, , drop = FALSE]

  # keep levels for which the heterogeneity is predicted for complex multivariate models
  if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {
    # if there is only a single level of heterogeneity, the levels are not returned
    # happens with subgroups etc - it needs to be appended from the design matrix
    if (nrow(tauLevelsMatrix) == 1) {
      tauLevels <- list(
        tauLevelsMatrix[["tau2.levels"]],
        tauLevelsMatrix[["gamma2.levels"]]
      )
    } else {
      tauLevels <- list(
        predictedEffect[["tau2.level"]],
        predictedEffect[["gamma2.level"]]
      )
    }
    tauLevels           <- do.call(cbind.data.frame, tauLevels[!sapply(tauLevels, is.null)])
    colnames(tauLevels) <- .mammExtractTauLevelNames(fit)
  }

  # return for the plotting function: requires different post-formatting
  if (returnRaw) {
    return(predictedEffect)
  }

  # to data.frame
  predictedEffect      <- .maExtractAndFormatPrediction(predictedEffect)
  predictedEffect$par  <- gettext("Pooled effect")

  # apply effect size transformation
  if (options[["transformEffectSize"]] != "none")
    predictedEffect[,c("est", "lCi", "uCi", "lPi", "uPi")] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(predictedEffect[,c("est", "lCi", "uCi", "lPi", "uPi")]))

  # return the tau levels
  if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]])
    predictedEffect <- cbind(predictedEffect, tauLevels)

  return(apply(predictedEffect, 1, as.list))
}

.maComputeFixedEffect              <- function(fit, options) {

  # refit the model as a fixed effect model
  data <- attr(fit, "dataset")
  fit <- metafor::rma(
    yi     = data[[options[["effectSize"]]]],
    sei    = data[[options[["effectSizeStandardError"]]]],
    method = "FE",
    test   = options[["fixedEffectTest"]]
  )

  predictedEffect <- data.frame(
    pred  = fit$beta[1],
    se    = fit$se[1],
    ddf   = .maExtractDdf(fit)[1],
    ci.lb = fit$ci.lb[1],
    ci.ub = fit$ci.ub[1],
    pi.lb = NA,
    pi.ub = NA
  )

  return(predictedEffect)
}

.maComputePooledEffectPlot         <- function(fit, options, forceFixedEffect = FALSE) {

  if (forceFixedEffect) {
    predictedEffect <- .maComputeFixedEffect(fit, options)
  } else {
    predictedEffect <- .maComputePooledEffect(fit, options, returnRaw = TRUE)
  }

  # compute test against specified value
  if (.maIsMetaregressionFtest(options)) {

    # to extract the degrees of freedom (rma.glmm predict may not have ddf)
    tempDf <- if (!is.null(predictedEffect$ddf)) predictedEffect$ddf else .maExtractDdf(fit)[1]
    predictedEffect      <- .maExtractAndFormatPrediction(predictedEffect)
    predictedEffect$df   <- tempDf
    predictedEffect$stat <- (predictedEffect$est - 0)  / predictedEffect$se
    predictedEffect$pval <- 2 * pt(abs(predictedEffect$stat), predictedEffect$df, lower.tail = FALSE)

  } else {

    predictedEffect      <- .maExtractAndFormatPrediction(predictedEffect)
    predictedEffect$stat <- (predictedEffect$est - 0)  / predictedEffect$se
    predictedEffect$pval <- 2 * pnorm(abs(predictedEffect$stat), lower.tail = FALSE)

  }

  # fix column names
  predictedEffect$par       <- "Effect Size"

  # apply effect size transformation
  if (options[["transformEffectSize"]] != "none")
    predictedEffect[,c("est", "lCi", "uCi", "lPi", "uPi")] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(predictedEffect[,c("est", "lCi", "uCi", "lPi", "uPi")]))


  return(as.list(predictedEffect))
}

# Pooled heterogeneity ----

.maComputePooledHeterogeneity      <- function(fit, options) {

  if (fit[["tau2.fix"]]) {

    confIntHeterogeneity <- data.frame(
      par = c("\U1D70F", "\U1D70F\U00B2"),
      est = c(sqrt(fit[["tau2"]]), fit[["tau2"]]),
      lCi = c(NA, NA),
      uCi = c(NA, NA)
    )

    # keep only the requested parameters (other than tau and tau^2 are not possible)
    heterogeneityShow <- c(
      if (options[["heterogeneityTau"]])  1,
      if (options[["heterogeneityTau2"]]) 2
    )

    confIntHeterogeneity <- confIntHeterogeneity[heterogeneityShow,,drop = FALSE]

  } else if (.maIsMetaregressionHeterogeneity(options)) {
    # no confint support
    # predict the scale on the average value
    predScale <- predict(
      fit,
      newscale  = matrix(colMeans(model.matrix(fit)$scale), nrow = 1),
      level     = 100 * options[["confidenceIntervalsLevel"]]
    )

    if (options[["heterogeneityModelLink"]] == "log") {
      confIntHeterogeneity <- data.frame(
        par = c("\U1D70F", "\U1D70F\U00B2"),
        est = exp(c(predScale[["pred"]]  / 2, predScale[["pred"]])),
        lCi = exp(c(predScale[["ci.lb"]] / 2, predScale[["ci.lb"]])),
        uCi = exp(c(predScale[["ci.ub"]] / 2, predScale[["ci.ub"]]))
      )
    } else if (options[["heterogeneityModelLink"]] == "identity") {
      confIntHeterogeneity <- data.frame(
        par = c("\U1D70F", "\U1D70F\U00B2"),
        est = c(sqrt(predScale[["pred"]]),  predScale[["pred"]]),
        lCi = c(sqrt(predScale[["ci.lb"]]), predScale[["ci.lb"]]),
        uCi = c(sqrt(predScale[["ci.ub"]]), predScale[["ci.ub"]])
      )
    }

    # keep only the requested parameters (other than tau and tau^2 are not possible)
    heterogeneityShow <- c(
      if (options[["heterogeneityTau"]])  1,
      if (options[["heterogeneityTau2"]]) 2
    )

    confIntHeterogeneity <- confIntHeterogeneity[heterogeneityShow,,drop = FALSE]

  } else {

    confIntHeterogeneity <- confint(fit, level = 100 * options[["confidenceIntervalsLevel"]])
    confIntHeterogeneity <- data.frame(confIntHeterogeneity[["random"]])[c(2,1,3,4),]
    colnames(confIntHeterogeneity) <- c("est", "lCi", "uCi")
    confIntHeterogeneity$par       <- c("\U1D70F", "\U1D70F\U00B2", "I\U00B2", "H\U00B2")

    if (options[["standardErrors"]]){
      confIntHeterogeneity$se <- c(.maGetSqrtTransformationSeDeltaMethod(fit$tau2 ,fit$se.tau2), fit$se.tau2, NA, NA)
    }

    # keep only the requested parameters
    heterogeneityShow <- c(
      if (options[["heterogeneityTau"]])  1,
      if (options[["heterogeneityTau2"]]) 2,
      if (options[["heterogeneityI2"]])   3,
      if (options[["heterogeneityH2"]])   4
    )

    confIntHeterogeneity <- confIntHeterogeneity[heterogeneityShow,,drop = FALSE]

  }

  return(confIntHeterogeneity)
}

.maComputePooledHeterogeneityPlot  <- function(fit, options, parameter = "tau") {

  # dispatch to GLMM-specific function
  if (inherits(fit, "rma.glmm"))
    return(.maglmmComputePooledHeterogeneityPlot(fit, options, parameter))


  # don't use the confint on robust.rma objects (they are not implemented)
  # the clustering works only on the fixed effect estimates
  # -> we can drop the class and compute confint and get the heterogeneity from the original fit
  # (the fit is passed directly from from forest plot function so it is cleaner to dispatch it here)
  if (inherits(fit, "robust.rma"))
    class(fit) <- class(fit)[!class(fit) %in% "robust.rma"]

  # dispatch options to the .maComputePooledHeterogeneity function
  options[["heterogeneityTau"]]  <- parameter == "tau"
  options[["heterogeneityTau2"]] <- parameter == "tau2"
  options[["heterogeneityI2"]]   <- parameter == "I2"
  options[["heterogeneityH2"]]   <- parameter == "H2"

  # compute the heterogeneity
  confIntHeterogeneity <- .maComputePooledHeterogeneity(fit, options)

  return(confIntHeterogeneity)
}

# Omnibus tests ----

.maOmnibusTest                     <- function(fit, options, parameter = "effectSize") {

  if (parameter == "effectSize") {
    row <- list(
      parameter = gettext("Effect Size"),
      stat      = fit[["QM"]],
      df1       = fit[["QMdf"]][1],
      pval      = fit[["QMp"]]
    )
  } else if (parameter == "heterogeneity") {
    row <- list(
      parameter = gettext("Heterogeneity"),
      stat      = fit[["QS"]],
      df1       = fit[["QSdf"]][1],
      pval      = fit[["QSp"]]
    )
  }

  if (.maIsMetaregressionFtest(options)) {
    if (parameter == "effectSize") {
      row$df2 <- fit[["QMdf"]][2]
    } else if (parameter == "heterogeneity") {
      row$df2 <- fit[["QSdf"]][2]
    }
  }


  if (.maIsPermutation(options))
    row$pval2 <- switch(
      parameter,
      "effectSize"    = attr(fit[["QMp"]], "permutation")[1],
      "heterogeneity" = attr(fit[["QSp"]], "permutation")[1]
    )

  return(row)
}

.maOmnibusTestCoefficients         <- function(fit, options, parameter = "effectSize", returnSelCoef = FALSE) {

  maxCoef <- switch(
    parameter,
    "effectSize"    = nrow(fit$beta),
    "heterogeneity" = nrow(fit$alpha)
  )
  selCoef <- .maCleanSelectedIndexesVector(options[[switch(
    parameter,
    "effectSize"    = "addOmnibusModeratorTestEffectSizeCoefficientsValues",
    "heterogeneity" = "addOmnibusModeratorTestHeterogeneityCoefficientsValues"
  )]])

  # additional error catching
  if ((!is.numeric(selCoef) || any(!(abs(selCoef - round(selCoef)) < .Machine$double.eps^0.5))) ||
      (any(selCoef < 1) || any(selCoef > maxCoef))) {

    row <- list(
      stat = NA,
      df1  = NA,
      pval = NA
    )

    if (.maIsMetaregressionFtest(options))
      row$df2 <- NA

    if ((!is.numeric(selCoef) || any(!(abs(selCoef - round(selCoef)) < .Machine$double.eps^0.5))))
      attr(row, "footnote") <- gettextf(
        "Indexes of %1$s moderation coefficients were specified in an incorrect format. Try '(1, 2)' to test the first two coefficients.",
        switch(
          parameter,
          "effectSize"    = "effect size",
          "heterogeneity" = "heterogeneity"
        ))
    else if (any(selCoef < 1) || any(selCoef > maxCoef))
      attr(row, "footnote") <- gettextf(
        "The selected coefficients for %1$s moderation must be between 1 and %2$i (i.e., the number of regression parameters).",
        switch(
          parameter,
          "effectSize"    = "effect size",
          "heterogeneity" = "heterogeneity"
        ),
        maxCoef)

    return(row)
  }

  if (returnSelCoef) {
    return(selCoef)
  }

  if (parameter == "effectSize") {

    if (.maIsGLMM(options)) {
      out <- .maGlmmWaldTest(fit, btt = selCoef)
    } else {
      out <- anova(fit, btt = selCoef)
    }

    row <- list(
      stat = out[["QM"]],
      df1  = out[["QMdf"]][1],
      pval = out[["QMp"]]
    )

    if (.maIsMetaregressionFtest(options))
      row$df2 <- out[["QMdf"]][2]

  } else if (parameter == "heterogeneity") {

    out <- anova(fit, att = selCoef)

    row <- list(
      stat = out[["QS"]],
      df1  = out[["QSdf"]][1],
      pval = out[["QSp"]]
    )

    if (.maIsMetaregressionFtest(options))
      row$df2 <- fit[["QSdf"]][2]
  }

  if (.maIsPermutation(options))
    row$pval2 <- switch(
      parameter,
      "effectSize"    = attr(fit[["QMp"]], "permutation")[2],
      "heterogeneity" = attr(fit[["QSp"]], "permutation")[2]
    )

  if (parameter == "effectSize") {
    row$parameter <- gettextf("Effect size (coef: %1$s)", paste(selCoef, collapse = ", "))
    attr(row, "footnote") <- gettextf(
      "Effect size coefficients %1$s correspond to %2$s.",
      paste(selCoef, collapse = ","),
      paste(sapply(rownames(fit$beta)[selCoef], function(coefName) .maVariableNames(coefName, unlist(options[["effectSizeModelTerms"]]))), collapse = ", "))
  } else if (parameter == "heterogeneity") {
    row$parameter <- gettextf("Heterogeneity (coef: %1$s)", paste(selCoef, collapse = ", "))
    attr(row, "footnote") <- sapply(rownames(fit$alpha)[selCoef], function(coefName) .maVariableNames(coefName, options[["predictors"]]))
    attr(row, "footnote") <- gettextf(
      "Heterogeneity coefficients %1$s correspond to %2$s.",
      paste(selCoef, collapse = ","),
      paste(sapply(rownames(fit$alpha)[selCoef], function(coefName) .maVariableNames(coefName, unlist(options[["heterogeneityModelTerms"]]))), collapse = ", "))
  }

  attr(row, "selCoef") <- selCoef

  return(row)
}

# Table rows and fit measures ----

.maRowHeterogeneityTest               <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit) || (!is.null(fit[["QE"]]) && is.na(fit[["QE"]]))) {
    return(data.frame(
      subgroup = attr(fit, "subgroup"),
      test     = if (.maIsMetaregression(options)) gettext("Residual heterogeneity") else gettext("Heterogeneity")
    ))
  }

  row <- data.frame(
    subgroup = attr(fit, "subgroup"),
    test     = if (.maIsMetaregression(options)) gettext("Residual heterogeneity") else gettext("Heterogeneity"),
    stat     = sprintf(paste0("Q\U2091(%1$i) = ", if (fit[["QE"]] < 1e5) "%2$.2f" else "%2$.3g"), fit[["k"]] - fit[["p"]], fit[["QE"]]),
    pval     = fit[["QEp"]]
  )

  return(row)
}

.maRowEffectSizeTest                  <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(data.frame(
      subgroup = attr(fit, "subgroup"),
      test     = gettext("Pooled effect")
    ))
  }

  # pooled effect size
  predictedEffect <- try(.maComputePooledEffectPlot(fit, options))

  if (jaspBase::isTryError(predictedEffect))
    return(data.frame(
      subgroup = attr(fit, "subgroup"),
      test     = gettext("Pooled effect"),
      stat     = gettext("The pooled effect size could not be calculated.")
    ))

  row <- data.frame(
    subgroup = attr(fit, "subgroup"),
    test     = gettext("Pooled effect"),
    stat     = if (.maIsMetaregressionFtest(options)) sprintf(paste0(
      "t(%1$s) = ", if (predictedEffect[["stat"]][1] < 1e5) "%2$.2f" else "%2$.3g"),
      .maPrintDf(predictedEffect[["df"]][1]),
      predictedEffect[["stat"]][1]
    )
    else sprintf(paste0("z = ", if (predictedEffect[["stat"]][1] < 1e5) "%1$.2f" else "%1$.3g"), predictedEffect[["stat"]][1]),
    pval     = predictedEffect[["pval"]][1]
  )

  return(row)
}

.maRowModerationTest                  <- function(fit, options, parameter = "effectSize", coefficientsTest = FALSE) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    testAdd <- if (coefficientsTest) gettext("(coef ...)") else ""
    row     <- data.frame(
      subgroup = attr(fit, "subgroup"),
      test     = switch(
        parameter,
        "effectSize"    = if (.maIsMetaregressionHeterogeneity(options)) gettextf("Moderation effect size%1$s", testAdd) else gettextf("Moderation%1$s", testAdd),
        "heterogeneity" = gettextf("Moderation heterogeneity%1$s", testAdd)
      )
    )
    return(row)
  }

  # compute the test
  if (coefficientsTest) {
    moderationOut <- .maOmnibusTestCoefficients(fit, options, parameter = parameter)
  } else {
    moderationOut <- .maOmnibusTest(fit, options, parameter = parameter)
  }

  row <- list(
    "subgroup" = attr(fit, "subgroup")
  )

  # add information about the tested coefficients
  if (coefficientsTest) {
    testAdd <- gettextf(" (coef: %1$s)", paste0(attr(moderationOut, "selCoef"), collapse = ", "))
  } else {
    testAdd <- ""
  }

  # test description
  row[["test"]] <- switch(
    parameter,
    "effectSize"    = if (.maIsMetaregressionHeterogeneity(options)) gettextf("Moderation effect size%1$s", testAdd) else gettextf("Moderation%1$s", testAdd),
    "heterogeneity" = gettextf("Moderation heterogeneity%1$s", testAdd)
  )


  # test statistic
  if (!is.na(moderationOut[["pval"]])) {

    if (.maIsMetaregressionFtest(options)) {
      row[["stat"]] <- sprintf(paste0(
        "F\U2098(%1$s, %2$s) = ", if (moderationOut[["stat"]] < 1e5) "%3$.2f" else "%3$.3g"),
        .maPrintDf(moderationOut[["df1"]]),
        .maPrintDf(moderationOut[["df2"]]),
        moderationOut[["stat"]]
      )
    } else {
      row[["stat"]] <- sprintf(paste0(
        "Q\U2098(%1$s) = ", if (moderationOut[["stat"]] < 1e5) "%2$.2f" else "%2$.3g"),
        .maPrintDf(moderationOut[["df1"]]),
        moderationOut[["stat"]]
      )
    }
    row[["pval"]] <- moderationOut[["pval"]]

    # permutation p-value
    if (.maIsPermutation(options)) {
      if (parameter == "effectSize") {
        row[["pval2"]] <- attr(fit[["QMp"]], "permutation")[1]
      } else if (parameter == "heterogeneity") {
        row[["pval2"]] <- attr(fit[["QSp"]], "permutation")[1]
      }
    }
  }

  row <- do.call(cbind.data.frame, row)

  # add footnote message if necessary
  if (coefficientsTest)
    attr(row, "footnote") <- attr(moderationOut, "footnote")

  return(row)
}

.maRowSubgroupTest                    <- function(fit, options) {

  est <- lapply(fit, function(x) {
    if (isTryError(x)) {
      return()
    } else {
      return(data.frame(.maComputePooledEffect(x, options, returnRaw = TRUE)))
    }
  })
  est <- .maSafeRbind(est)

  if (is.null(est) || nrow(est) < 2)
    return()

  subFit <- try(metafor::rma(yi = est[["pred"]], sei = est[["se"]], mods = ~ as.factor(1:nrow(est)), method = "FE"))

  if (jaspBase::isTryError(subFit))
    return()

  row <- data.frame(
    subgroup = "",
    test     = gettext("Subgroup differences"),
    stat     = sprintf(paste0("Q\U2098(%1$i) = ", if (subFit[["QM"]] < 1e5) "%2$.2f" else "%2$.3g"), nrow(est) - 1, subFit[["QM"]]),
    pval     = subFit[["QMp"]]
  )

  return(row)
}

.maRowPooledEffectEstimate            <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(data.frame(
      par     = gettext("Pooled effect"),
      subgroup = attr(fit, "subgroup")
    ))
  }

  # pooled effect size
  row <- try(.maComputePooledEffect(fit, options))

  if (jaspBase::isTryError(row)) {
    return(data.frame(
      par     = gettext("Pooled effect"),
      subgroup = attr(fit, "subgroup")
    ))
  }

  if (options[["predictionIntervals"]] && .mammHasMultipleHeterogeneities(options, canAddOutput = TRUE)) {
    row <- do.call(rbind.data.frame, row)
  } else {
    row <- do.call(cbind.data.frame, row)
  }

  row$subgroup <- attr(fit, "subgroup")
  return(row)
}

.maRowPooledHeterogeneity             <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(data.frame(
      par     = c("\U1D70F", "\U1D70F\U00B2", "I\U00B2", "H\U00B2")[c(
        options[["heterogeneityTau"]], options[["heterogeneityTau2"]], options[["heterogeneityI2"]], options[["heterogeneityH2"]])],
      subgroup = attr(fit, "subgroup")
    ))
  }

  # pooled heterogeneity
  if (options[["analysis"]] == "generalizedMetaAnalysis") {
    row <- .maglmmComputePooledHeterogeneity(fit, options)
  } else if (options[["analysis"]] == "mantelHaenszelPeto") {
    row <- .mamhpComputePooledHeterogeneity(fit, options)
  } else {
    row <- .maComputePooledHeterogeneity(fit, options)
  }
  row$subgroup <- attr(fit, "subgroup")

  return(row)
}

.maRowFitMeasures                     <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    row <- data.frame(
      "subgroup"     = attr(fit, "subgroup"),
      "model"        = NA,
      "observations" = NA,
      "ll"           = NA,
      "dev"          = NA,
      "AIC"          = NA,
      "BIC"          = NA,
      "AICc"         = NA
    )

    if (.maIsMetaregressionEffectSize(options) && !.maIsMultilevelMultivariate(options))
      row$R2 <- NA

    return(row)
  }

  # pooled effect size
  fitStats <- fit[["fit.stats"]]

  # GLMM always uses ML; drop the REML column
  if (inherits(fit, "rma.glmm") && "REML" %in% colnames(fitStats))
    fitStats <- fitStats[, "ML", drop = FALSE]

  if (.maIsUnrestrictedWeightedLeastSquares(options))
    fitStats <- .maComputeUwlsFitMeasures(fitStats, fit)

  row <- cbind.data.frame(
    "subgroup"     = attr(fit, "subgroup"),
    "model"        = colnames(fitStats),
    "observations" = fit[["k"]],
    data.frame(t(fitStats))
  )

  if (!.maIsUnrestrictedWeightedLeastSquares(options) &&
      .maIsMetaregressionEffectSize(options) && !.maIsMultilevelMultivariate(options) && !.maIsGLMM(options))
    row$R2 <- fit[["R2"]]

  return(row)
}

.maComputeUwlsFitMeasures             <- function(fitStats, fit) {

  yi          <- fit[["yi"]]
  vi          <- fit[["vi"]]
  k           <- fit[["k"]]
  nParameters <- fit[["p"]] + 1

  sigma2        <- sum(1 / vi * stats::resid(fit)^2) / k
  logLikelihood <- sum(stats::dnorm(yi, mean = stats::fitted(fit), sd = sqrt(sigma2 * vi), log = TRUE))

  fitStats["ll", ]  <- logLikelihood
  fitStats["AIC", ] <- -2 * logLikelihood + 2 * nParameters
  fitStats["BIC", ] <- -2 * logLikelihood + log(k) * nParameters

  return(fitStats[c("ll", "AIC", "BIC"), , drop = FALSE])
}
