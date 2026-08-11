# Classical meta-regression tables.
#
# Builds terms, coefficient, correlation, and associated warning output.

.maExtractMetaregressionContainer    <- function(jaspResults) {

  if (!is.null(jaspResults[["metaregressionContainer"]]))
    return(jaspResults[["metaregressionContainer"]])

  # create the output container
  metaregressionContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  metaregressionContainer$dependOn(c(.maDependencies))
  metaregressionContainer$position <- 3
  jaspResults[["metaregressionContainer"]] <- metaregressionContainer

  return(metaregressionContainer)
}

.maTermsTable                            <- function(jaspResults, options, parameter = "effectSize") {

  metaregressionContainer <- .maExtractMetaregressionContainer(jaspResults)

  if (!is.null(metaregressionContainer[[paste0(parameter, "TermsTable")]]))
    return()

  if (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  termsTable <- createJaspTable(switch(
    parameter,
    effectSize    = gettext("Effect Size Meta-Regression Terms Tests"),
    heterogeneity = gettext("Heterogeneity Meta-Regression Terms Tests")
  ))
  termsTable$position <- switch(
    parameter,
    effectSize    = 1,
    heterogeneity = 2
  )
  termsTable$dependOn(c("metaregressionTermTests", "includeFullDatasetInSubgroupAnalysis"))
  metaregressionContainer[[paste0(parameter, "TermsTable")]] <- termsTable

  termsTable$addColumnInfo(name = "term",  type = "string",  title = "")
  .maAddSubgroupColumn(termsTable, options)
  termsTable$addColumnInfo(name = "stat",  type = "number",  title = if(.maIsMetaregressionFtest(options)) gettext("F")   else gettext("Q\U2098"))
  termsTable$addColumnInfo(name = "df1",   type = "integer", title = if(.maIsMetaregressionFtest(options)) gettext("df\U2081") else gettext("df"))
  if (.maIsMetaregressionFtest(options)) {
    termsTable$addColumnInfo(name = "df2", type = "number", title = gettext("df\U2082"))
  }
  termsTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))

  if (.maIsPermutation(options)) {
    termsTable$addColumnInfo(name = "pval2",  type = "pvalue",  title = gettext("p (permutation)"))
    termsTable$addFootnote(.maPermutationMessage(options))
  }

  .maAddFixedEffectTestFootnote(termsTable, options)

  if (.maIsGLMM(options))
    termsTable$addFootnote(gettext("Term tests based on Wald-type chi-squared tests."))

  # skip on error
  if ((length(fit) == 1 && jaspBase::isTryError(fit[[1]]))  || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  if ((parameter == "effectSize"    && !.maIsMetaregressionEffectSize(options)) ||
      (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options)))
    return()

  # term tests rows
  termTests <- .maSafeRbind(lapply(fit, .maRowTermTestTable, options = options, parameter = parameter))
  termTests <- .maSafeOrderAndSimplify(termTests, "term", options)

  # add messages
  termTestWarnings <- .maTermsTableWarnings(fit, options, parameter)
  for (i in seq_along(termTestWarnings))
    termsTable$addFootnote(termTestWarnings[i], symbol = gettext("Warning:"))

  termsTable$setData(termTests)

  return()
}

.maCoefficientEstimatesTable             <- function(jaspResults, options, parameter = "effectSize") {

  metaregressionContainer <- .maExtractMetaregressionContainer(jaspResults)

  if (!is.null(metaregressionContainer[[paste0(parameter, "CoefficientTable")]]))
    return()

  if (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  coefficientsTable <- createJaspTable(switch(
    parameter,
    effectSize    = gettext("Effect Size Meta-Regression Coefficients"),
    heterogeneity = gettext("Heterogeneity Meta-Regression Coefficients")
  ))
  coefficientsTable$position <- switch(
    parameter,
    effectSize    = 3,
    heterogeneity = 4
  )
  coefficientsTable$dependOn(c("metaregressionCoefficientEstimates", "confidenceIntervals", "confidenceIntervalsLevel", "standardErrors", "includeFullDatasetInSubgroupAnalysis"))
  metaregressionContainer[[paste0(parameter, "CoefficientTable")]] <- coefficientsTable

  coefficientsTable$addColumnInfo(name = "name",  type = "string", title = "")
  .maAddSubgroupColumn(coefficientsTable, options)
  coefficientsTable$addColumnInfo(name = "est",   type = "number", title = gettext("Estimate"))
  .maAddSeColumn(coefficientsTable, options, noTransformation = TRUE)
  .maAddCiColumn(coefficientsTable, options)
  coefficientsTable$addColumnInfo(name = "stat",  type = "number", title = if(.maIsMetaregressionFtest(options)) gettext("t") else gettext("z"))
  if (.maIsMetaregressionFtest(options))
    coefficientsTable$addColumnInfo(name = "df",  type = "number", title = gettext("df"))
  coefficientsTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))
  if (.maIsPermutation(options)) {
    coefficientsTable$addColumnInfo(name = "pval2",  type = "pvalue",  title = gettext("p (permutation)"))
    coefficientsTable$addFootnote(.maPermutationMessage(options))
  }

  .maAddFixedEffectTestFootnote(coefficientsTable, options)

  # skip on error
  if ((length(fit) == 1 && jaspBase::isTryError(fit[[1]]))  || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  estimates <- .maSafeRbind(lapply(fit, .maRowCoefficientsEstimatesTable, options = options, parameter = parameter))
  estimates <- .maSafeOrderAndSimplify(estimates, "name", options)

  # add messages
  coefficientsTableWarnings <- .maCoefficientsTableWarnings(fit, options, parameter)
  for (i in seq_along(coefficientsTableWarnings))
    coefficientsTable$addFootnote(coefficientsTableWarnings[i], symbol = gettext("Warning:"))
  if (parameter == "heterogeneity")
    coefficientsTable$addFootnote(.meMetaregressionHeterogeneityMessages(options))
  .maAddLowDdfWarning(coefficientsTable, fit, options)

  coefficientsTable$setData(estimates)

  return()
}

.maCoefficientCorrelationMatrixTable     <- function(jaspResults, options, parameter = "effectSize") {

  metaregressionContainer <- .maExtractMetaregressionContainer(jaspResults)

  if (!is.null(metaregressionContainer[[paste0(parameter, "CorrelationTable")]]))
    return()

  if (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # create individual tables for each subgroup
  if (options[["subgroup"]] == "") {

    correlationMatrixTable <- .maCoefficientCorrelationMatrixTableFun(fit[[1]], options, parameter)
    correlationMatrixTable$title <- switch(
      parameter,
      effectSize    = gettext("Effect Size Meta-Regression Correlation Matrix"),
      heterogeneity = gettext("Heterogeneity Meta-Regression Correlation Matrix")
    )
    correlationMatrixTable$dependOn(c("metaregressionCoefficientCorrelationMatrix", "includeFullDatasetInSubgroupAnalysis"))
    correlationMatrixTable$position <- switch(
      parameter,
      effectSize    = 5,
      heterogeneity = 6
    )
    metaregressionContainer[[paste0(parameter, "CorrelationTable")]] <- correlationMatrixTable
    return()

  } else {

    # create the output container
    correlationMatrixTable <- createJaspContainer(switch(
      parameter,
      effectSize    = gettext("Effect Size Meta-Regression Correlation Matrix"),
      heterogeneity = gettext("Heterogeneity Meta-Regression Correlation Matrix")
    ))
    correlationMatrixTable$dependOn(c(.maDependencies, "metaregressionCoefficientCorrelationMatrix", "includeFullDatasetInSubgroupAnalysis"))
    correlationMatrixTable$position <- switch(
      parameter,
      effectSize    = 5,
      heterogeneity = 6
    )
    metaregressionContainer[[paste0(parameter, "CorrelationTable")]] <- correlationMatrixTable

    for (i in seq_along(fit)) {
      correlationMatrixTable[[names(fit)[i]]]          <- .maCoefficientCorrelationMatrixTableFun(fit[[i]], options, parameter)
      correlationMatrixTable[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      correlationMatrixTable[[names(fit)[i]]]$position <- i
    }

  }

  return()
}

.maCoefficientCorrelationMatrixTableFun  <- function(fit, options, parameter) {

  correlationMatrixTable <- createJaspTable()

  if (is.null(fit) || jaspBase::isTryError(fit))
    return(correlationMatrixTable)

  if (parameter == "effectSize")
    correlationMatrix <- data.frame(as.matrix(cov2cor(fit[["vb"]])))
  else if (parameter == "heterogeneity")
    correlationMatrix <- data.frame(as.matrix(cov2cor(fit[["va"]])))

  correlationMatrixNames      <- .maVariableNames(colnames(correlationMatrix), switch(
    parameter,
    effectSize    = unlist(options[["effectSizeModelTerms"]]),
    heterogeneity = unlist(options[["heterogeneityModelTerms"]])
  ))
  colnames(correlationMatrix) <- correlationMatrixNames
  correlationMatrix$name      <- correlationMatrixNames

  correlationMatrixTable$addColumnInfo(name = "name", type = "string", title = "")
  for (correlationMatrixName in correlationMatrixNames)
    correlationMatrixTable$addColumnInfo(name = correlationMatrixName, type = "number")

  correlationMatrixTable$setData(correlationMatrix)

  return(correlationMatrixTable)
}

.maTermTests                       <- function(fit, options, term, parameter = "effectSize") {

  # obtain terms indicies
  if (parameter == "effectSize") {

    terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")     # get terms indices from the model
    termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")  # get coefficient indices from the model matrix
    if (!is.null(fit$coef.na))
      termsIndex <- termsIndex[!fit$coef.na]                                                   # remove dropped coefficients

    # deal with the possibility that all coefficients of the corresponding term were dropped
    if (sum(termsIndex == which(terms == term)) == 0) {

      out <- list(
        term = .maVariableNames(term, unlist(options[["effectSizeModelTerms"]])),
        stat = NA,
        df1  = NA,
        pval = NA
      )

      if (.maIsMetaregressionFtest(options))
        out$df2 <- NA

      if (.maIsPermutation(options))
        out$pval2 <- NA

    } else {

      bttIdx <- seq_along(termsIndex)[termsIndex == which(terms == term)]
      if (.maIsGLMM(options)) {
        termsAnova <- .maGlmmWaldTest(fit, btt = bttIdx)
      } else {
        termsAnova <- anova(fit, btt = bttIdx)
      }

      out <- list(
        term = .maVariableNames(term, unlist(options[["effectSizeModelTerms"]])),
        stat = termsAnova[["QM"]],
        df1  = termsAnova[["QMdf"]][1],
        pval = termsAnova[["QMp"]]
      )

      if (.maIsMetaregressionFtest(options))
        out$df2 <- termsAnova[["QMdf"]][2]

      if (.maIsPermutation(options))
        out$pval2 <- attr(fit[["QMp"]], "permutationTerms")[which(terms == term)]
    }

  } else if (parameter == "heterogeneity") {

    terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")      # get terms indices from the model
    termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")   # get coefficient indices from the model matrix
    if (!is.null(fit$coef.na.Z))
      termsIndex <- termsIndex[!fit$coef.na.Z]                                                   # remove dropped coefficients

    # deal with the possibility that all coefficients of the corresponding term were dropped
    if (sum(termsIndex == which(terms == term)) == 0) {

      out <- list(
        term = .maVariableNames(term, unlist(options[["heterogeneityModelTerms"]])),
        stat = NA,
        df1  = NA,
        pval = NA
      )

      if (.maIsMetaregressionFtest(options))
        out$df2 <- NA

      if (.maIsPermutation(options))
        out$pval2 <- NA

    } else {

      termsAnova <- anova(fit, att = seq_along(termsIndex)[termsIndex == which(terms == term)])

      out <- list(
        term = .maVariableNames(term, unlist(options[["heterogeneityModelTerms"]])),
        stat = termsAnova[["QS"]],
        df1  = termsAnova[["QSdf"]][1],
        pval = termsAnova[["QSp"]]
      )

      if (.maIsMetaregressionFtest(options))
        out$df2 <- termsAnova[["QSdf"]][2]

      if (.maIsPermutation(options))
        out$pval2 <- attr(fit[["QSp"]], "permutationTerms")[which(terms == term)]
    }

  }

  return(out)
}

.maRowTermTestTable                   <- function(fit, options, parameter) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  if (parameter == "effectSize") {
    terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")
    termsTests <- do.call(rbind.data.frame, lapply(terms, function(term)
      .maTermTests(fit, options, term, parameter = "effectSize")
    ))
  } else if (parameter == "heterogeneity") {
    terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")
    termsTests <- do.call(rbind.data.frame, lapply(terms, function(term)
      .maTermTests(fit, options, term, parameter = "heterogeneity")
    ))
  }

  termsTests$subgroup <- attr(fit, "subgroup")

  return(termsTests)
}

.maRowCoefficientsEstimatesTable      <- function(fit, options, parameter) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  if (parameter == "effectSize") {

    estimates <- data.frame(
      name = .maVariableNames(rownames(fit[["beta"]]), unlist(options[["effectSizeModelTerms"]])),
      est  = fit[["beta"]][,1],
      se   = fit[["se"]],
      stat = fit[["zval"]],
      pval = fit[["pval"]]
    )

    if (.maIsPermutation(options))
      estimates$pval2 <- attr(fit[["pval"]], "permutation")

    if (.maIsMetaregressionFtest(options))
      estimates$df <- .maExtractDdf(fit)

    if (options[["confidenceIntervals"]]) {
      estimates$lCi <- fit[["ci.lb"]]
      estimates$uCi <- fit[["ci.ub"]]
    }

  } else if (parameter == "heterogeneity") {

    estimates <- data.frame(
      name = .maVariableNames(rownames(fit[["alpha"]]), unlist(options[["heterogeneityModelTerms"]])),
      est  = fit[["alpha"]][,1],
      se   = fit[["se.alpha"]],
      stat = fit[["zval.alpha"]],
      pval = fit[["pval.alpha"]]
    )

    if (.maIsPermutation(options))
      estimates$pval2 <- attr(fit[["pval.alpha"]], "permutation")

    if (.maIsMetaregressionFtest(options))
      estimates$df <- fit[["ddf.alpha"]]

    if (options[["confidenceIntervals"]]) {
      estimates$lCi <- fit[["ci.lb.alpha"]]
      estimates$uCi <- fit[["ci.ub.alpha"]]
    }

  }

  estimates$subgroup <- attr(fit, "subgroup")

  return(estimates)
}

.maTermsTableWarnings                  <- function(fit, options, parameter) {

  if (options[["subgroup"]] == "") {
    messages <- .maTermsTableWarningsFun(fit[[1]], parameter, prefix = "")
  } else {
    messages <- NULL
    for (i in seq_along(fit)) {
      if (jaspBase::isTryError(fit))
        next
      messages <- c(messages, .maTermsTableWarningsFun(fit[[i]], parameter, prefix = gettextf("Subgroup %1$s: ", attr(fit[[i]], "subgroup"))))
    }
  }

  return(messages)
}

.maTermsTableWarningsFun               <- function(fit, parameter, prefix = "") {

  coefNA <- switch(
    parameter,
    "effectSize"    = fit$coef.na,
    "heterogeneity" = fit$coef.na.Z
  )

  if (any(coefNA)) {

    if (parameter == "effectSize") {
      terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")     # get terms indices from the model
      termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")  # get coefficient indices from the model matrix
    } else if (parameter == "heterogeneity") {
      terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")      # get terms indices from the model
      termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")   # get coefficient indices from the model matrix
    }

    messages <- unlist(sapply(terms, function(term) {

      thisTermsIndex <- termsIndex[termsIndex == which(terms == term)]
      thisNaTerms    <- coefNA[termsIndex == which(terms == term)]

      if (all(thisNaTerms)) {
        return(gettextf(
          "%1$sThe term %2$s was completely removed from the model. Possible causes are missing values, collinear predictors, or missing crossed cells in an interaction term.",
          prefix, term
        ))
      } else if (any(thisNaTerms)) {
        return(gettextf(
          "%1$sThe term %2$s was partilly removed (%3$i/%4$i coefficients) from the model. Possible causes are missing values, collinear predictors, or missing crossed cells in an interaction term.",
           prefix, term, sum(thisNaTerms), length(thisNaTerms)))
      } else {
        return(NULL)
      }
    }))
  } else (
    messages <- NULL
  )

  return(messages)
}

.maCoefficientsTableWarnings           <- function(fit, options, parameter) {

  if (options[["subgroup"]] == "") {
    messages <- .maCoefficientsTableWarningsFun(fit[[1]], parameter, prefix = "")
  } else {
    messages <- NULL
    for (i in seq_along(fit)) {
      if (jaspBase::isTryError(fit))
        next
      messages <- c(messages, .maCoefficientsTableWarningsFun(fit[[i]], parameter, prefix = gettextf("Subgroup %1$s: ", attr(fit[[i]], "subgroup"))))
    }
  }

  return(messages)
}

.maCoefficientsTableWarningsFun        <- function(fit, parameter, prefix = "") {

  coefNA <- switch(
    parameter,
    "effectSize"    = fit$coef.na,
    "heterogeneity" = fit$coef.na.Z
  )

  if (any(coefNA)) {

    missingCoef <- names(coefNA)[coefNA]
    missingCoef <- gsub("^.", "", missingCoef) # remove first letter as metafor adds "X/Z"

    messages <- gettextf(
      "%1$sThe following coefficients were removed from the model: %2$s. Possible causes are missing values, collinear predictors, or missing crossed cells in an interaction term.",
      prefix, paste(missingCoef, collapse = ", "))

  } else {
    messages <- NULL
  }

  return(messages)
}
