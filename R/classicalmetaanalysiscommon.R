#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This analysis runs
# - classical meta-analysis (using rma.uni)
# - classical multilevel/multivariate meta-analysis (using rma.mv; custom function prefix .mamm)
# - classical binimal meta-analysis (using rma.; custom function prefix .mab)


# TODO:
# Forest plot
# - allow aggregation of studies by a factor (then show simple REML aggregation within and overlaying shaded estimates)
# AIC/BIC Model-averaging
# Diagnostics
# - model re-run on presence of influential cases
# - residual
#   - vs predicted
#   - vs outcome
#   - vs covariates
# Generic
# - allow different covariates factoring across all settings
# - confidence interval for heterogeneity in multilevel multivariate

.ClassicalMetaAnalysisCommon <- function(jaspResults, dataset, options, ...) {

  # fit the model
  .maFitModel(jaspResults, dataset, options)
  .maUpdateFitModelDataset(jaspResults, dataset, options)

  # # remove influential observations and refit the model if requested
  # if (options[["diagnosticsCasewiseDiagnostics"]] && options[["diagnosticsCasewiseDiagnosticsRerunWithoutInfluentialCases"]]) {
  #   dataset <- .maRemoveInfluentialObservations(jaspResults, dataset, options)
  #   .maFitModel(jaspResults, dataset, options, objectName = "fitNoInfluence")
  # }

  # model summary
  .maOverallTestsTable(jaspResults, dataset, options)
  .maPooledEstimatesTable(jaspResults, dataset, options)

  # random effects
  if (.maIsMultilevelMultivariate(options))
    .mammRandomEstimatesTable(jaspResults, dataset, options)

  if (options[["fitMeasures"]])
    .maFitMeasuresTable(jaspResults, dataset, options)

  # meta-regression tables
  if (.maIsMetaregression(options)) {
    if (options[["metaregressionTermTests"]]) {
      .maTermsTable(jaspResults, dataset, options, "effectSize")
      .maTermsTable(jaspResults, dataset, options, "heterogeneity")
    }
    if (options[["metaregressionCoefficientEstimates"]]) {
      .maCoefficientEstimatesTable(jaspResults, dataset, options, "effectSize")
      .maCoefficientEstimatesTable(jaspResults, dataset, options, "heterogeneity")
    }
    if (options[["metaregressionCoefficientCorrelationMatrix"]]) {
      .maCoefficientCorrelationMatrixTable(jaspResults, dataset, options, "effectSize")
      .maCoefficientCorrelationMatrixTable(jaspResults, dataset, options, "heterogeneity")
    }
  }

  # estimated marginal means and contrasts (the whole section is created within the dispatch)
  .maEstimatedMarginalMeansAndContrasts(jaspResults, dataset, options)

  # plots
  .maUltimateForestPlot(jaspResults, dataset, options)
  .maBubblePlot(jaspResults, dataset, options)

  # diagnostics
  if (.maIsMetaregression(options) && options[["diagnosticsVarianceInflationFactor"]]) {
    .maVarianceInflationTable(jaspResults, dataset, options, "effectSize")
    .maVarianceInflationTable(jaspResults, dataset, options, "heterogeneity")
  }
  if (options[["diagnosticsCasewiseDiagnostics"]]) {
    .maCasewiseDiagnosticsTable(jaspResults, dataset, options)
    .maCasewiseDiagnosticsExportColumns(jaspResults, dataset, options)
  }
  if (options[["diagnosticsPlotsProfileLikelihood"]])
    .maProfileLikelihoodPlot(jaspResults, dataset, options)
  if (options[["diagnosticsPlotsBaujat"]])
    .maBaujatPlot(jaspResults, dataset, options)
  if (options[["diagnosticsResidualFunnel"]])
    .maResidualFunnelPlot(jaspResults, dataset, options)


  # additional
  if (options[["showMetaforRCode"]])
    .maShowMetaforRCode(jaspResults, options)

  return()
}

# fitting functions
.maGetFormula                    <- function(modelTerms, includeIntercept) {

  predictors <- unlist(lapply(modelTerms, function(x) {
    if (length(x[["components"]]) > 1)
      return(paste(x[["components"]], collapse = ":"))
    else
      return(x[["components"]])
  }))

  if (length(predictors) == 0)
    return(NULL)

  if (includeIntercept)
    formula <- paste("~", paste(predictors, collapse = "+"))
  else
    formula <- paste("~", paste(predictors, collapse = "+"), "-1")

  return(as.formula(formula, env = parent.frame(1)))
}
.maFitModel                      <- function(jaspResults, dataset, options, objectName = "fit") {

  if (!.maReady(options) || !is.null(jaspResults[[objectName]]))
    return()

  # create the output container
  fitContainer <- createJaspState()
  fitContainer$dependOn(.maDependencies)
  jaspResults[[objectName]] <- fitContainer

  fitOutput <- list()

  # full dataset fit
  startProgressbar(expectedTicks = 1, label = gettext("Estimating Meta-Analytic Model"))
  fitOutput[["__fullDataset"]] <- .maFitModelFun(dataset, options, subgroupName = gettext("Full dataset"))
  progressbarTick()

  # add subgroup fits
  if (options[["subgroup"]] != "") {

    subgroupLevels <- unique(dataset[[options[["subgroup"]]]])
    startProgressbar(expectedTicks = length(subgroupLevels), label = gettext("Estimating Subgroup Models"))

    for (i in seq_along(subgroupLevels)) {

      subgroupLevel <- subgroupLevels[i]
      subgroupIndx  <- dataset[[options[["subgroup"]]]] == subgroupLevel
      subgroupData  <- dataset[subgroupIndx, ]

      # forward NAs information
      tempNasIds    <- attr(dataset, "NasIds")[!attr(dataset, "NasIds")]
      attr(subgroupData, "NAs")    <- sum(tempNasIds[subgroupIndx])
      attr(subgroupData, "NasIds") <- tempNasIds[subgroupIndx]

      # fit the model
      fitOutput[[paste0("subgroup", subgroupLevel)]] <- .maFitModelFun(subgroupData, options, subgroupName = as.character(subgroupLevel))
      progressbarTick()
    }
  }


  # add to the output
  jaspResults[[objectName]]$object <- fitOutput

  return()
}
.maFitModelFun                   <- function(dataset, options, subgroupName) {
  # --------------------------------------------------------------------------- #
  # when updating don't forget to update the '.maMakeMetaforCallText' function! #
  # --------------------------------------------------------------------------- #

  # specify the effect size and outcome
  if (options[["module"]] == "metaAnalysis") {
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      sei  = as.name(options[["effectSizeStandardError"]]),
      data = dataset
    )
  } else if (options[["module"]] == "metaAnalysisMultilevelMultivariate") {
    # TODO: extend to covariance matrices
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      V    = as.name("samplingVariance"), # precomputed on data load
      data = dataset
    )
  }

  # add formulas if specified
  rmaInput$mods  <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  rmaInput$scale <- .maGetFormula(options[["heterogeneityModelTerms"]], options[["heterogeneityModelIncludeIntercept"]])

  # add random effects
  if (.maIsMultilevelMultivariate(options)) {
    randomFormulaList <- .mammGetRandomFormulaList(options)
    randomFormulaList <- unname(randomFormulaList) # remove names for some metafor post-processing functions
    if (length(randomFormulaList) != 0) {
      rmaInput$random <- randomFormulaList
      rmaInput$struct <- do.call(c, lapply(randomFormulaList, attr, which = "structure"))

      # spatial-specific settings
      rmaInput$dist   <- unlist(lapply(randomFormulaList, attr, which = "dist"), recursive = FALSE)
      addConstant     <- do.call(c, lapply(randomFormulaList, attr, which = "addConstant"))
      if (length(addConstant) > 0 && any(addConstant))
        rmaInput$data$constant   <- 1
      for (i in seq_along(rmaInput$dist)) {
        if (is.matrix(rmaInput$dist[[i]]) && !all(unique(rmaInput[["data"]][[names(rmaInput$dist)[i]]]) %in% rownames(rmaInput$dist[[names(rmaInput$dist)[i]]])))
          .quitAnalysis(gettextf("The loaded distance matrix for '%1$s' does not match the dataset. The following levels are missing: %2$s.",
                                 names(rmaInput$dist)[i],
                                 paste0(unique(rmaInput[["data"]][[names(rmaInput$dist)[i]]])[!unique(rmaInput[["data"]][[names(rmaInput$dist)[i]]]) %in% rownames(rmaInput$dist)], collapse = ", ")))
      }

      # known correlation-specific settings
      rmaInput$R   <- unlist(lapply(randomFormulaList, attr, which = "R"), recursive = FALSE)
      for (i in seq_along(rmaInput$R)) {
        if (!all(unique(rmaInput[["data"]][[names(rmaInput$R)[i]]]) %in% rownames(rmaInput$R[[names(rmaInput$R)[i]]])))
          .quitAnalysis(gettextf("The loaded correlation matrix for '%1$s' does not match the dataset. The following levels are missing: %2$s.",
                                 names(rmaInput$R)[i],
                                 paste0(unique(rmaInput[["data"]][[names(rmaInput$R)[i]]])[!unique(rmaInput[["data"]][[names(rmaInput$R)[i]]]) %in% rownames(rmaInput$R)], collapse = ", ")))
      }
    }
  }

  # specify method and fixed effect terms test
  rmaInput$method <- .maGetMethodOptions(options)
  rmaInput$test   <- options[["fixedEffectTest"]]

  if (!options[["weightedEstimation"]])
    rmaInput$weighted <- FALSE

  # add fixed parameters if needed
  if (options[["fixParametersWeights"]] && options[["fixParametersWeightsVariable"]] != "")
    rmaInput$weights <- dataset[[options[["fixParametersWeightsVariable"]]]]
  if (options[["fixParametersTau2"]])
    rmaInput$tau2 <- .maGetFixedTau2Options(options) # TODO: add multiple possible fixed taus

  # add link function if needed
  if (.maIsMetaregressionHeterogeneity(options))
    rmaInput$link <- options[["heterogeneityModelLink"]]

  if (.maIsMultilevelMultivariate(options)) {
    rmaInput$sparse <- if (options[["useSparseMatricies"]])       options[["useSparseMatricies"]]
    rmaInput$cvvc   <- if (!options[["computeCovarianceMatrix"]]) !options[["computeCovarianceMatrix"]]
  }

  # add control options if needed
  control <- .maGetControlOptions(options)
  if (length(control) != 0)
    rmaInput$control <- control

  # additional input
  rmaInput$level <- 100 * options[["confidenceIntervalsLevel"]]

  # extend the call by custom commands from R if requested
  if (options[["advancedExtendMetaforCall"]])
    rmaInput <- c(rmaInput, .maExtendMetaforCallFromOptions(options))

  ### fit the model
  if (options[["module"]] == "metaAnalysis") {
    fit <- try(do.call(metafor::rma, rmaInput))
  } else if (options[["module"]] == "metaAnalysisMultilevelMultivariate") {
    fit <- try(do.call(metafor::rma.mv, rmaInput))
  }


  # add clustering if specified
  if (options[["clustering"]] != "") {
    fitClustered <- try(metafor::robust(
      fit,
      cluster      = dataset[[options[["clustering"]]]],
      clubSandwich = options[["clusteringUseClubSandwich"]],
      adjust       = options[["clusteringSmallSampleCorrection"]]
    ))
  } else {
    fitClustered <- NULL
  }


  # add permutation test if requested (only available for non-clustered fits)
  if (.maIsPermutation(options) && !jaspBase::isTryError(fit)) {
    fit <- .maPermutestAndStore(fit, options)
  }


  # add information about dropped levels to the fit
  if (.maIsMultilevelMultivariate(options)) {
    attr(fit, "skipped") <- attr(randomFormulaList, "skipped")
    if (options[["clustering"]] != "") {
      attr(fitClustered, "skipped") <- attr(randomFormulaList, "skipped")
    }
  }

  # add attributes
  attr(fit, "subgroup") <- paste0(subgroupName)
  attr(fit, "dataset")  <- dataset
  if (!is.null(fitClustered)) {
    attr(fitClustered, "subgroup") <- subgroupName
    attr(fitClustered, "dataset")  <- dataset
  }


  # return the results
  return(list(
    fit            = fit,
    fitClustered   = fitClustered
  ))
}
.maUpdateFitModelDataset         <- function(jaspResults, dataset, options, objectName = "fit") {

  # this function updates the data sets stored as attribute of the fit object if any of the additional variables changes
  # this is necessary for simplifying handling dataset in the forest plot etc...

  if (!.maReady(options) || is.null(jaspResults[[objectName]]))
    return()
  if (!is.null(jaspResults[[paste0(objectName, "DataSet")]]))
    return()

  # create a container that works as an indicator for dataset update
  fitContainer <- createJaspState()
  fitContainer$dependOn(.maDataPlottingDependencies)
  jaspResults[[paste0(objectName, "DataSet")]] <- fitContainer

  # extract the fit objects
  fitOutput <- jaspResults[[objectName]]$object

  # full dataset fit
  fitOutput[["__fullDataset"]] <- .maUpdateFitModelDatasetFun(fitOutput[["__fullDataset"]], dataset)

  # add subgroup fits
  if (options[["subgroup"]] != "") {

    subgroupLevels <- unique(dataset[[options[["subgroup"]]]])

    for (i in seq_along(subgroupLevels)) {

      subgroupLevel <- subgroupLevels[i]
      subgroupIndx  <- dataset[[options[["subgroup"]]]] == subgroupLevel
      subgroupData  <- dataset[subgroupIndx, ]

      # forward NAs information
      tempNasIds    <- attr(dataset, "NasIds")[!attr(dataset, "NasIds")]
      attr(subgroupData, "NAs")    <- sum(tempNasIds[subgroupIndx])
      attr(subgroupData, "NasIds") <- tempNasIds[subgroupIndx]

      # fit the model
      fitOutput[[paste0("subgroup", subgroupLevel)]] <- .maUpdateFitModelDatasetFun(fitOutput[[paste0("subgroup", subgroupLevel)]], subgroupData)
    }
  }

  # save the updated fits
  jaspResults[[objectName]]$object  <- fitOutput

  # set the container to non-null
  jaspResults[[paste0(objectName, "DataSet")]]$object <- TRUE

  return()

}
.maUpdateFitModelDatasetFun      <- function(fitOutput, dataset) {

  if (!is.null(fitOutput[["fit"]])) {
    fit <- fitOutput[["fit"]]
    attr(fit, "dataset") <- dataset
  } else {
    fit <- NULL
  }

  if (!is.null(fitOutput[["fitClustered"]])) {
    fitClustered <- fitOutput[["fitClustered"]]
    attr(fitClustered, "dataset") <- dataset
  } else {
    fitClustered <- NULL
  }

  return(list(
    fit            = fit,
    fitClustered   = fitClustered
  ))
}
.maPermutestAndStore             <- function(fit, options) {

  # perform permutation tests for coefficients and terms tests
  # store the permutation p-values in the fit object
  # this simplifies object dispatching later in the code
  # the whole fitPermutation object can be essentially forgotten

  .setSeedJASP(options)

  permtestCall <- list(
    fit,
    exact = options[["permutationTestType"]] == "exact",
    iter  = options[["permutationTestIteration"]],
    code1 = "jaspBase::startProgressbar(X.iter, label = 'Permutation test')",
    code2 = "jaspBase::progressbarTick()"
  )
  fitPermutation <- try(do.call(metafor::permutest, permtestCall))


  if (.maIsMetaregressionEffectSize(options)) {

    QMpPermutation <- fitPermutation[["QMp"]]

    # add permutation test for additional omnibus moderator test
    if (options[["addOmnibusModeratorTestEffectSizeCoefficients"]]) {
      tempBtt          <- .maOmnibusTestCoefficients(fit, options, parameter = "effectSize", returnSelCoef = TRUE)
      tempPermtestCall <- permtestCall
      tempPermtestCall$btt   <- tempBtt
      tempPermtestCall$code1 <- paste0("jaspBase::startProgressbar(X.iter, label = 'Effect Size Omibus Moderator Test: (", paste0(tempBtt, collapse = "," ), "))')")
      tempFitPermutation     <- try(do.call(metafor::permutest, tempPermtestCall))
      QMpPermutation   <- c(QMpPermutation, if (jaspBase::isTryError(tempFitPermutation)) NA else tempFitPermutation[["QMp"]])
    }

    # add permutation tests for moderation term tests
    termsIndicies  <- .maGetTermsIndices(fit, "effectSize")

    pvalPermutation      <- fitPermutation[["pval"]]
    pvalTermsPermutation <- rep(NA, length(termsIndicies))

    for (i in seq_along(termsIndicies)) {
      # single-coefficient term tests: term == coefficient
      if (length(termsIndicies[[i]]) == 1) {
        pvalTermsPermutation[i] <- pvalPermutation[termsIndicies[[i]]]
      } else if (length(termsIndicies[[i]]) > 1) {
        # multiple-coefficient term tests: permutation needs to be re-done
        tempBtt          <- .maOmnibusTestCoefficients(fit, options, parameter = "effectSize", returnSelCoef = TRUE)
        tempPermtestCall <- permtestCall
        tempPermtestCall$btt   <- tempBtt
        tempPermtestCall$code1 <- paste0("jaspBase::startProgressbar(X.iter, label = 'Effect Size Moderator Test: (", names(termsIndicies)[i], "))')")
        tempFitPermutation     <- try(do.call(metafor::permutest, tempPermtestCall))
        pvalTermsPermutation[i] <- if (jaspBase::isTryError(tempFitPermutation)) NA else tempFitPermutation[["QMp"]]
      }
    }

    # store results
    attr(fit[["QMp"]],  "permutation")      <- QMpPermutation
    attr(fit[["QMp"]],  "permutationTerms") <- pvalTermsPermutation
    attr(fit[["pval"]], "permutation")      <- pvalPermutation
  }

  if (.maIsMetaregressionHeterogeneity(options)) {

    QSpPermutation <- fitPermutation[["QSp"]]

    # add permutation test for additional omnibus moderator test
    if (options[["addOmnibusModeratorTestHeterogeneityCoefficients"]]) {
      tempAtt          <- .maOmnibusTestCoefficients(fit, options, parameter = "heterogeneity", returnSelCoef = TRUE)
      tempPermtestCall <- permtestCall
      tempPermtestCall$att   <- tempAtt
      tempPermtestCall$code1 <- paste0("jaspBase::startProgressbar(X.iter, label = 'Heterogeneity Omibus Moderator Test: (", paste0(tempAtt, collapse = "," ), "))')")
      tempFitPermutation     <- try(do.call(metafor::permutest, tempPermtestCall))
      QSpPermutation   <- c(QSpPermutation, if (jaspBase::isTryError(tempFitPermutation)) NA else tempFitPermutation[["QSp"]])
    }

    # add permutation tests for moderation term tests
    termsIndicies  <- .maGetTermsIndices(fit, "heterogeneity")

    pval.alphaPermutation      <- fitPermutation[["pval.alpha"]]
    pval.alphaTermsPermutation <- rep(NA, length(termsIndicies))

    for (i in seq_along(termsIndicies)) {
      # single-coefficient term tests: term == coefficient
      if (length(termsIndicies[[i]]) == 1) {
        pval.alphaTermsPermutation[i] <- pval.alphaPermutation[termsIndicies[[i]]]
      } else if (length(termsIndicies[[i]]) > 1) {
        # multiple-coefficient term tests: permutation needs to be re-done
        tempAtt          <- .maOmnibusTestCoefficients(fit, options, parameter = "heterogeneity", returnSelCoef = TRUE)
        tempPermtestCall <- permtestCall
        tempPermtestCall$att   <- tempAtt
        tempPermtestCall$code1 <- paste0("jaspBase::startProgressbar(X.iter, label = 'Heterogeneity Moderator Test: (", names(termsIndicies)[i], "))')")
        tempFitPermutation     <- try(do.call(metafor::permutest, tempPermtestCall))
        pval.alphaTermsPermutation[i] <- if (jaspBase::isTryError(tempFitPermutation)) NA else tempFitPermutation[["QSp"]]
      }
    }

    # store results
    attr(fit[["QSp"]],  "permutation")       <- QSpPermutation
    attr(fit[["QSp"]],  "permutationTerms")  <- pval.alphaTermsPermutation
    attr(fit[["pval.alpha"]], "permutation") <- pval.alphaPermutation
  }

  return(fit)
}
.maRemoveInfluentialObservations <- function(jaspResults, dataset, options) {

  if (!.maReady(options) || !is.null(jaspResults[["fit"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  if (jaspBase::isTryError(fit))
    return()

  # remove influential observations
  influenceResults       <- influence.rma.uni(fit)
  influentialObservation <- influenceResults$inf$inf == "*"

  dataset <- dataset[!influentialObservation, ]
  attr(dataset, "influentialObservations") <- sum(influentialObservation)

  if (nrow(dataset) == 0)
    return(.quitAnalysis(gettext("All observations were removed as influential.")))

  return(dataset)
}
.maDiagnostics                   <- function(jaspResults, options) {

  # extract precomputed diagnostics if done before:
  if (!is.null(jaspResults[["diagnosticsResults"]])) {

    out <- jaspResults[["diagnosticsResults"]]$object

  } else {

    # the fit diagnostics work only for the non-clustered fit
    fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)

    # create the output container
    diagnosticsResults <- createJaspState()
    diagnosticsResults$dependOn(.maDependencies)
    jaspResults[["diagnosticsResults"]] <- diagnosticsResults

    out <- list()

    for (i in seq_along(fit)) {

      if (jaspBase::isTryError(fit[[i]])) {
        influenceResultsDfbs <- list()
        influenceResultsInf  <- list()
      } else {
        if (.maIsMultilevelMultivariate(options)) {
          # only a subset of diagnostics is available for rma.mv
          influenceResultsDfbs <- data.frame(dfbetas(fit[[i]], code1 = "jaspBase::startProgressbar(x$k, label = 'Casewise diagnostics: DFBETAS')", code2 = "jaspBase::progressbarTick()"))
          influenceResultsInf  <- data.frame(
            rstudent = rstudent(fit[[i]],       code1 = "jaspBase::startProgressbar(x$k, label = 'Casewise diagnostics: Studentized residuals')", code2 = "jaspBase::progressbarTick()")[["resid"]],
            cook.d   = cooks.distance(fit[[i]], code1 = "jaspBase::startProgressbar(x$k, label = 'Casewise diagnostics: Cooks distance')",        code2 = "jaspBase::progressbarTick()"),
            hat      = hatvalues(fit[[i]])
          )
        } else {
          # the complete suite of influence diagnostics is only available for rma.uni
          influenceResults     <- influence(fit[[i]], code1 = "jaspBase::startProgressbar(x$k, label = 'Casewise diagnostics')", code2 = "jaspBase::progressbarTick()")
          influenceResultsDfbs <- data.frame(influenceResults$dfbs)
          influenceResultsInf  <- data.frame(influenceResults$inf)
          influenceResultsInf$tau.del <- sqrt(influenceResultsInf$tau2.del)
          influenceResultsInf$inf[influenceResultsInf$inf == "*"] <- "Yes"
        }
      }

      out[[attr(fit[[i]], "subgroup")]] <- list(
        "influenceResultsDfbs" = influenceResultsDfbs,
        "influenceResultsInf"  = influenceResultsInf
      )
    }

    # store the results
    jaspResults[["diagnosticsResults"]]$object <- out
  }

  return(out)
}
.maProfile                       <- function(jaspResults, options) {

  # extract precomputed profile likelihood if done before:
  if (!is.null(jaspResults[["profileLikelihoodResults"]])) {

    out <- jaspResults[["profileLikelihoodResults"]]$object

  } else {

    # create the output container
    profileLikelihoodResults <- createJaspState()
    profileLikelihoodResults$dependOn(.maDependencies)
    jaspResults[["profileLikelihoodResults"]] <- profileLikelihoodResults


    fit <- .maExtractFit(jaspResults, options)
    out <- list()

    for (i in seq_along(fit)) {

      if (jaspBase::isTryError(fit[[i]])) {

        dfProfile <- list()

      } else if (.maIsMultilevelMultivariate(options)) {

        # use the defaults (too many possible parameter combinations to control)
        dfProfile <- try(metafor::profile.rma.mv(
          fit[[i]],
          plot    = FALSE,
          progbar = FALSE,
          code1   = "jaspBase::startProgressbar(length(vcs), label = 'Profile likelihood')",
          code2   = "jaspBase::progressbarTick()"
        ))

        # deal with a single component (not a list)
        if (dfProfile[["comps"]] == 1) {
          dfProfile <- list(dfProfile)
          dfProfile[["comps"]] <- 1
        }

      } else {

        # proceed with some nice formatting for rma.uni (too difficult to implement for rma.mv)
        xTicks    <- jaspGraphs::getPrettyAxisBreaks(c(0, max(0.1, 2*fit[[i]][["tau2"]])))
        dfProfile <- try(profile(
          fit[[i]],
          xlim    = range(xTicks),
          plot    = FALSE,
          progbar = FALSE,
          code1   = "jaspBase::startProgressbar(length(vcs), label = 'Profile likelihood')",
          code2   = "jaspBase::progressbarTick()"
        ))
        attr(dfProfile, "xTicks")   <- xTicks
      }

      out[[attr(fit[[i]], "subgroup")]] <- dfProfile
    }

    jaspResults[["profileLikelihoodResults"]]$object <- out
  }


  return(out)
}
.maBaujat                        <- function(jaspResults, options) {

  # extract precomputed profile likelihood if done before:
  if (!is.null(jaspResults[["baujatResults"]])) {

    out <- jaspResults[["baujatResults"]]$object

  } else {

    # create the output container
    baujatResults <- createJaspState()
    baujatResults$dependOn(.maDependencies)
    jaspResults[["baujatResults"]] <- baujatResults


    fit <- .maExtractFit(jaspResults, options)
    out <- list()

    for (i in seq_along(fit)) {

      if (jaspBase::isTryError(fit[[i]])) {
        dfBaujat <- list()
      } else {
        dfBaujat <- try(.maSuppressPlot(metafor::baujat(fit[[i]])))
        attr(dfBaujat, "studyLabels") <- attr(fit[[i]], "dataset")[[options[["studyLabels"]]]]
      }

      out[[attr(fit[[i]], "subgroup")]] <- dfBaujat
    }

    jaspResults[["baujatResults"]]$object <- out
  }


  return(out)
}

# output tables
.maOverallTestsTable                     <- function(jaspResults, dataset, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["testsTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # residual heterogeneity table
  testsTable <- createJaspTable(gettext("Meta-Analytic Tests"))
  testsTable$position <- 1
  testsTable$dependOn(c("addOmnibusModeratorTestEffectSizeCoefficients", "addOmnibusModeratorTestEffectSizeCoefficientsValues",
                        "addOmnibusModeratorTestHeterogeneityCoefficients", "addOmnibusModeratorTestHeterogeneityCoefficientsValues",
                        "includeFullDatasetInSubgroupAnalysis"))
  modelSummaryContainer[["testsTable"]] <- testsTable

  testsTable$addColumnInfo(name = "test",  type = "string",  title = "")
  .maAddSubgroupColumn(testsTable, options)
  testsTable$addColumnInfo(name = "stat",  type = "string",  title = gettext("Test"))
  testsTable$addColumnInfo(name = "pval",  type = "pvalue",  title = gettext("p"))

  if (.maIsPermutation(options)) {
    testsTable$addColumnInfo(name = "pval2",  type = "pvalue",  title = gettext("p (permutation)"))
    testsTable$addFootnote(.maPermutationMessage(options))
  }

  # stop and display errors
  if (is.null(fit))
    return()

  if (!is.null(.maCheckIsPossibleOptions(options))) {
    testsTable$setError(.maCheckIsPossibleOptions(options))
    return()
  }

  # stop with error if only single fit requested and failed
  if (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) {
    testsTable$setError(.maTryCleanErrorMessages(fit[[1]]))
    return()
  }

  # add all the overall model test
  tests <- list()
  tests[["heterogeneity"]] <- .maSafeRbind(lapply(fit, .maRowHeterogeneityTest, options = options))
  tests[["effect"]]        <- .maSafeRbind(lapply(fit, .maRowEffectSizeTest,    options = options))

  # effect size moderation
  if (.maIsMetaregressionEffectSize(options)) {
    # omnibus test
    tests[["moderationEffect"]] <- .maSafeRbind(lapply(fit, .maRowModerationTest, options = options, parameter = "effectSize"))

    # additional custom test
    if (options[["addOmnibusModeratorTestEffectSizeCoefficients"]]) {
      tests[["moderationEffect2"]] <- .maSafeRbind(lapply(fit, .maRowModerationTest, options = options, parameter = "effectSize", coefficientsTest = TRUE))
      if (jaspBase::isTryError(tests[["moderationEffect2"]])) {
        testsTable$setError(tests[["moderationEffect2"]])
        return()
      }
      testsTable$addFootnote(attr(tests[["moderationEffect2"]], "footnote"))
    }
  }

  # heterogeneity moderation
  if (.maIsMetaregressionHeterogeneity(options)) {
    # omnibus test
    tests[["moderationHeterogeneity"]] <- .maSafeRbind(lapply(fit, .maRowModerationTest, options = options, parameter = "heterogeneity"))

    # additional custom test
    if (options[["addOmnibusModeratorTestHeterogeneityCoefficients"]]) {
      tests[["moderationHeterogeneity2"]] <- .maSafeRbind(lapply(fit, .maRowModerationTest, options = options, parameter = "heterogeneity", coefficientsTest = TRUE))
      if (jaspBase::isTryError(tests[["moderationHeterogeneity2"]])) {
        testsTable$setError(tests[["moderationHeterogeneity2"]])
        return()
      }
      testsTable$addFootnote(attr(tests[["moderationHeterogeneity2"]], "footnote"))
    }
  }

  # add errors messages for failed fits
  for (i in seq_along(fit)[sapply(fit, jaspBase::isTryError)]) {
    testsTable$addFootnote(
      gettextf("The model for the subgroup '%1$s' failed with the following error: %2$s",
               attr(fit[[i]], "subgroup"),
               .maTryCleanErrorMessages(fit[[i]])),
      symbol = gettext("Error:")
    )
  }

  # bind and clean rows
  tests <- .maSafeRbind(tests)
  tests <- .maSafeOrderAndSimplify(tests, "test", options)

  # add the rows to the table
  testsTable$setData(tests)

  return()
}
.maPooledEstimatesTable                  <- function(jaspResults, dataset, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["pooledEstimatesTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # pooled estimates
  pooledEstimatesTable          <- createJaspTable(gettext("Meta-Analytic Estimates"))
  pooledEstimatesTable$position <- 4
  pooledEstimatesTable$dependOn(c("heterogeneityTau", "heterogeneityTau2", "heterogeneityI2", "heterogeneityH2",
                                  "confidenceIntervals", "confidenceIntervalsLevel", "predictionIntervals", "transformEffectSize",
                                  "includeFullDatasetInSubgroupAnalysis"))
  modelSummaryContainer[["pooledEstimatesTable"]] <- pooledEstimatesTable

  pooledEstimatesTable$addColumnInfo(name = "par",  type = "string", title = "")
  .maAddSubgroupColumn(pooledEstimatesTable, options)
  pooledEstimatesTable$addColumnInfo(name = "est",  type = "number", title = gettext("Estimate"))
  .maAddCiColumn(pooledEstimatesTable, options)
  .maAddPiColumn(pooledEstimatesTable, options)
  if (options[["predictionIntervals"]] && .mammHasMultipleHeterogeneities(options, canAddOutput = TRUE)) {
    for (colName in .mammExtractTauLevelNames(fit)) {
      pooledEstimatesTable$addColumnInfo(name = colName, title = colName, type = .maGetVariableColumnType(colName, options), overtitle = gettext("Heterogeneity Level"))
    }
  }

  # skip on error
  if (length(fit) == 0 || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  estimates <- list()

  # pooled effect size
  estimates[["effect"]] <- .maSafeRbind(lapply(fit, .maRowPooledEffectEstimate, options = options))

  # pooled heterogeneity
  if (!.maGetMethodOptions(options) %in% c("EE", "FE") && !.maIsMultilevelMultivariate(options) &&
      (options[["heterogeneityTau"]] ||options[["heterogeneityTau2"]] || options[["heterogeneityI2"]] || options[["heterogeneityH2"]])) {

    # requires non-clustered fit
    fitNonClustered <- .maExtractFit(jaspResults, options, nonClustered = TRUE)
    estimates[["heterogeneity"]] <- .maSafeRbind(lapply(fitNonClustered, .maRowPooledHeterogeneity, options = options))
  }

  # add messages
  pooledEstimatesMessages <- .maPooledEstimatesMessages(fit, dataset, options, anyNA(estimates[["effect"]]))
  for (i in seq_along(pooledEstimatesMessages))
    pooledEstimatesTable$addFootnote(pooledEstimatesMessages[i])

  # merge and clean estimates
  estimates <- .maSafeRbind(estimates)
  estimates <- .maSafeOrderAndSimplify(estimates, "par", options)

  pooledEstimatesTable$setData(estimates)

  return()
}
.maFitMeasuresTable                      <- function(jaspResults, dataset, options) {

  modelSummaryContainer <- .maExtractModelSummaryContainer(jaspResults)

  if (!is.null(modelSummaryContainer[["fitMeasuresTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # fit measures table
  fitMeasuresTable          <- createJaspTable(gettext("Fit Measures"))
  fitMeasuresTable$position <- 4
  fitMeasuresTable$dependOn(c(.maDependencies, "fitMeasures", "includeFullDatasetInSubgroupAnalysis"))
  modelSummaryContainer[["fitMeasuresTable"]] <- fitMeasuresTable


  fitMeasuresTable$addColumnInfo(name = "model",         title = "",                      type = "string")
  .maAddSubgroupColumn(fitMeasuresTable, options)
  fitMeasuresTable$addColumnInfo(name = "observations",  title = gettext("Observations"), type = "integer")
  fitMeasuresTable$addColumnInfo(name = "ll",            title = gettext("Log Lik."),     type = "number")
  fitMeasuresTable$addColumnInfo(name = "dev",           title = gettext("Deviance"),     type = "number")
  fitMeasuresTable$addColumnInfo(name = "AIC",           title = gettext("AIC"),          type = "number")
  fitMeasuresTable$addColumnInfo(name = "BIC",           title = gettext("BIC"),          type = "number")
  fitMeasuresTable$addColumnInfo(name = "AICc",          title = gettext("AICc"),         type = "number")

  if (.maIsMetaregressionEffectSize(options) && !.maIsMultilevelMultivariate(options))
    fitMeasuresTable$addColumnInfo(name = "R2",  title = gettext("R\U00B2"),   type = "number")

  # skip on error
  if ((length(fit) == 1 && jaspBase::isTryError(fit[[1]]))  || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # fit measures rows
  fitMeasures <- .maSafeRbind(lapply(fit, .maRowFitMeasures, options = options))
  fitMeasures <- .maSafeOrderAndSimplify(fitMeasures, "model", options)

  fitMeasuresTable$setData(fitMeasures)

  return()
}
.maTermsTable                            <- function(jaspResults, dataset, options, parameter = "effectSize") {

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

  termsTable$addFootnote(.maFixedEffectTextMessage(options))

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
  termTestWarnings <- .maTermsTableWarnings(fit, options, terms, parameter)
  for (i in seq_along(termTestWarnings))
    termsTable$addFootnote(termTestWarnings[i], symbol = gettext("Warning:"))

  termsTable$setData(termTests)

  return()
}
.maCoefficientEstimatesTable             <- function(jaspResults, dataset, options, parameter = "effectSize") {

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
  coefficientsTable$dependOn(c("metaregressionCoefficientEstimates", "confidenceIntervals", "includeFullDatasetInSubgroupAnalysis"))
  metaregressionContainer[[paste0(parameter, "CoefficientTable")]] <- coefficientsTable

  coefficientsTable$addColumnInfo(name = "name",  type = "string", title = "")
  .maAddSubgroupColumn(coefficientsTable, options)
  coefficientsTable$addColumnInfo(name = "est",   type = "number", title = gettext("Estimate"))
  coefficientsTable$addColumnInfo(name = "se",    type = "number", title = gettext("Standard Error"))
  .maAddCiColumn(coefficientsTable, options)
  coefficientsTable$addColumnInfo(name = "stat",  type = "number", title = if(.maIsMetaregressionFtest(options)) gettext("t") else gettext("z"))
  if (.maIsMetaregressionFtest(options))
    coefficientsTable$addColumnInfo(name = "df",  type = "number", title = gettext("df"))
  coefficientsTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))
  if (.maIsPermutation(options)) {
    coefficientsTable$addColumnInfo(name = "pval2",  type = "pvalue",  title = gettext("p (permutation)"))
    coefficientsTable$addFootnote(.maPermutationMessage(options))
  }

  coefficientsTable$addFootnote(.maFixedEffectTextMessage(options))

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

  coefficientsTable$setData(estimates)

  return()
}
.maCoefficientCorrelationMatrixTable     <- function(jaspResults, dataset, options, parameter = "effectSize") {

  metaregressionContainer <- .maExtractMetaregressionContainer(jaspResults)

  if (!is.null(metaregressionContainer[[paste0(parameter, "CorrelationTable")]]))
    return()

  if (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # create individual tables for each subgroup
  if (options[["subgroup"]] == "") {

    correlationMatrixTable <- .maCoefficientCorrelationMatrixTableFun(fit[[1]], dataset, options, parameter)
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
      correlationMatrixTable[[names(fit)[i]]]          <- .maCoefficientCorrelationMatrixTableFun(fit[[i]], dataset, options, parameter)
      correlationMatrixTable[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      correlationMatrixTable[[names(fit)[i]]]$position <- i
    }

  }

  return()
}
.maCoefficientCorrelationMatrixTableFun  <- function(fit, dataset, options, parameter) {

  correlationMatrixTable <- createJaspTable()

  if (is.null(fit) || jaspBase::isTryError(fit))
    return()

  if (parameter == "effectSize")
    correlationMatrix <- data.frame(cov2cor(fit[["vb"]]))
  else if (parameter == "heterogeneity")
    correlationMatrix <- data.frame(cov2cor(fit[["va"]]))

  correlationMatrixNames      <- .maVariableNames(colnames(correlationMatrix), options[["predictors"]])
  colnames(correlationMatrix) <- correlationMatrixNames
  correlationMatrix$name      <- correlationMatrixNames

  correlationMatrixTable$addColumnInfo(name = "name", type = "string", title = "")
  for (correlationMatrixName in correlationMatrixNames)
    correlationMatrixTable$addColumnInfo(name = correlationMatrixName, type = "number")

  correlationMatrixTable$setData(correlationMatrix)

  return(correlationMatrixTable)
}
.maEstimatedMarginalMeansAndContrasts    <- function(jaspResults, dataset, options) {

  # so, this section is a bit complicated -- all in order to prevent updating of all subcomponents once a new variable is added/removed
  # the main container contains effect size and heterogeneity subcontainers, which contain variable containers with the actual output tables
  # updating of the subtables is skipped unless one of the options specified here is checked:
  # .maGetEstimatedMarginalMeansAndContrastsOptions()

  # check whether the section should be created at all
  isReadyEffectSize    <- length(options[["estimatedMarginalMeansEffectSizeSelectedVariables"]])    > 0 && (options[["estimatedMarginalMeansEffectSize"]]    || options[["contrastsEffectSize"]])
  isReadyHeterogeneity <- length(options[["estimatedMarginalMeansHeterogeneitySelectedVariables"]]) > 0 && (options[["estimatedMarginalMeansHeterogeneity"]] || options[["contrastsHeterogeneity"]])

  if (!isReadyEffectSize && !isReadyHeterogeneity) {
    # remove section if exists
    if (!is.null(jaspResults[["estimatedMarginalMeansAndContrastsContainer"]]))
      jaspResults[["estimatedMarginalMeansAndContrastsContainer"]] <- NULL

    return()
  }

  # create/extract section otherwise
  if (!is.null(jaspResults[["estimatedMarginalMeansAndContrastsContainer"]])) {
    estimatedMarginalMeansAndContrastsContainer <- jaspResults[["estimatedMarginalMeansAndContrastsContainer"]]
  } else {
    # create the output container
    estimatedMarginalMeansAndContrastsContainer <- createJaspContainer(gettext("Estimated Marginal Means and Contrasts Summary"))
    estimatedMarginalMeansAndContrastsContainer$dependOn(c(.maDependencies, "confidenceIntervals", "confidenceIntervalsLevel", "includeFullDatasetInSubgroupAnalysis"))
    estimatedMarginalMeansAndContrastsContainer$position <- 4
    jaspResults[["estimatedMarginalMeansAndContrastsContainer"]] <- estimatedMarginalMeansAndContrastsContainer
  }

  # fill the section with EMM/C tables for each variables for the effect size / heterogeneity
  if (isReadyEffectSize)
    .maEstimatedMarginalMeansAndContrastsFun(jaspResults, dataset, options, parameter = "effectSize")

  if (isReadyEffectSize)
    .maEstimatedMarginalMeansAndContrastsFun(jaspResults, dataset, options, parameter = "heterogeneity")

  return()
}
.maEstimatedMarginalMeansAndContrastsFun <- function(jaspResults, dataset, options, parameter = "effectSize") {

  # get the corresponding container
  estimatedMarginalMeansAndContrastsContainer <- jaspResults[["estimatedMarginalMeansAndContrastsContainer"]]

  # create/extract subsection container and meta-data
  if (!is.null(estimatedMarginalMeansAndContrastsContainer[[parameter]])) {
    tempContainer <- estimatedMarginalMeansAndContrastsContainer[[parameter]]
    tempMetaData  <- estimatedMarginalMeansAndContrastsContainer[[paste0(parameter, "MetaData")]]$object
  } else {
    # create the output container
    tempContainer <- createJaspContainer()
    tempContainer$position <- switch(
      parameter,
      effectSize    = 1,
      heterogeneity = 2
    )
    estimatedMarginalMeansAndContrastsContainer[[parameter]] <- tempContainer

    # create the container meta-data
    tempMetaDataState <- createJaspState()
    tempMetaDataState$dependOn(c("estimatedMarginalMeansEffectSize", "contrastsEffectSize"))
    estimatedMarginalMeansAndContrastsContainer[[paste0(parameter, "MetaData")]] <- tempMetaDataState
    tempMetaData      <- list()
  }

  fit <- .maExtractFit(jaspResults, options)

  # add an empty null table in case of an error
  if (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) {
    errorTable <- createJaspContainer(title = gettext("Estimated Marginal Means / Contrasts"))
    tempContainer[["errorTable"]] <- errorTable
    return()
  }

  # extract a list of already existing variables / to be created variables
  existingVariables <- tempMetaData[["existingVariables"]]
  selectedVariables <- sapply(switch(
    parameter,
    effectSize    = options[["estimatedMarginalMeansEffectSizeSelectedVariables"]],
    heterogeneity = options[["estimatedMarginalMeansHeterogeneitySelectedVariables"]]
  ), function(x) paste0(x[["variable"]], collapse = ":"))

  removeVariables <- setdiff(existingVariables, selectedVariables)
  addVariables    <- setdiff(selectedVariables, existingVariables)
  keepVariables   <- intersect(selectedVariables, existingVariables)

  # get information about the output type
  makeEstimatedMarginalMeans <- options[[switch(
    parameter,
    effectSize    = "estimatedMarginalMeansEffectSize",
    heterogeneity = "estimatedMarginalMeansHeterogeneity")]]
  makeContrasts <- options[[switch(
    parameter,
    effectSize    = "contrastsEffectSize",
    heterogeneity = "contrastsHeterogeneity")]]

  # remove variables that are not selected anymore
  for (i in seq_along(removeVariables))
    tempContainer[[removeVariables[i]]] <- NULL

  # if no variables needs to be added, there is no need to reshuffle the order
  if ((length(addVariables) == 0 && length(existingVariables) == length(selectedVariables) && all(existingVariables == selectedVariables)) &&
      (!is.null(tempMetaData[["hasEstimatedMarginalMeans"]]) && tempMetaData[["hasEstimatedMarginalMeans"]] == makeEstimatedMarginalMeans) &&
      (!is.null(tempMetaData[["hasContrasts"]])              && tempMetaData[["hasContrasts"]] == makeContrasts) &&
      (!is.null(tempMetaData[["selectedOptions"]]) && identical(tempMetaData[["selectedOptions"]], .maGetEstimatedMarginalMeansAndContrastsOptions(options)))
  )
    return()

  # add adjusted estimate if requested
  adjustedEstimateOption <- switch(
    parameter,
    effectSize    = "estimatedMarginalMeansEffectSizeAddAdjustedEstimate",
    heterogeneity = "estimatedMarginalMeansHeterogeneityAddAdjustedEstimate"
  )
  if (options[[adjustedEstimateOption]] && is.null(tempContainer[["adjustedEstimate"]]) && makeEstimatedMarginalMeans){
    tempVariableContainer <- createJaspContainer(title = sprintf(
      "Adjusted Estimate%1$s",
      if (.maIsMetaregressionHeterogeneity(options)) switch(parameter, effectSize = gettext(" (Effect Size)"), heterogeneity = gettext(" (Heterogeneity)"))
      else ""
    ))
    tempVariableContainer$position <- 0
    tempVariableContainer$dependOn(adjustedEstimateOption)
    tempContainer[["adjustedEstimate"]] <- tempVariableContainer
    .maEstimatedMarginalMeansTable(tempVariableContainer, fit, dataset, options, "", parameter)
  }

  # reorder / add variables
  for (i in seq_along(selectedVariables)) {

    # get the variable container
    if (is.null(tempContainer[[selectedVariables[[i]]]])) {
      tempVariableContainer <- createJaspContainer(title = sprintf(
        "%1$s%2$s",
        gsub(":", jaspBase::interactionSymbol, selectedVariables[[i]]),
        if (.maIsMetaregressionHeterogeneity(options)) switch(parameter, effectSize = gettext(" (Effect Size)"), heterogeneity = gettext(" (Heterogeneity)"))
        else ""
      ))
      tempContainer[[selectedVariables[[i]]]] <- tempVariableContainer
    } else {
      tempVariableContainer <- tempContainer[[selectedVariables[[i]]]]
    }

    # if output was already created, just reorder the position
    tempVariableContainer$position <- i

    # add the missing outputs
    if (makeEstimatedMarginalMeans && is.null(tempVariableContainer[["estimatedMarginalMeansTable"]]))
      .maEstimatedMarginalMeansTable(tempVariableContainer, fit, dataset, options, selectedVariables[[i]], parameter)

    if (makeContrasts && is.null(tempVariableContainer[["contrastsTable"]]))
      .maContrastsTable(tempVariableContainer, fit, dataset, options, selectedVariables[[i]], parameter)
  }

  # re-write information about existing variables
  estimatedMarginalMeansAndContrastsContainer[[paste0(parameter, "MetaData")]]$object <- list(
    existingVariables         = selectedVariables,
    hasEstimatedMarginalMeans = makeEstimatedMarginalMeans,
    hasContrasts              = makeContrasts,
    selectedOptions           = .maGetEstimatedMarginalMeansAndContrastsOptions(options)
  )

  return()
}
.maEstimatedMarginalMeansTable           <- function(variableContainer, fit, dataset, options, selectedVariable, parameter = "effectSize") {

  estimatedMarginalMeansTable <- createJaspTable(if (selectedVariable == "") gettext("Adjusted Estimate") else gettext("Estimated Marginal Means"))
  estimatedMarginalMeansTable$position <- 1
  estimatedMarginalMeansTable$dependOn(c(switch(
    parameter,
    effectSize    = c("estimatedMarginalMeansEffectSize", "estimatedMarginalMeansEffectSizeSdFactorCovariates", "estimatedMarginalMeansEffectSizeTestAgainst",
                      "estimatedMarginalMeansEffectSizeTestAgainstValue", "transformEffectSize", "predictionIntervals"),
    heterogeneity = c("estimatedMarginalMeansHeterogeneity", "estimatedMarginalMeansHeterogeneityTransformation", "estimatedMarginalMeansHeterogeneitySdFactorCovariates")
  ), if (selectedVariable == "") switch(
    parameter,
    effectSize    = "estimatedMarginalMeansEffectSizeAddAdjustedEstimate",
    heterogeneity = "estimatedMarginalMeansHeterogeneityAddAdjustedEstimate"
  )))
  variableContainer[["estimatedMarginalMeansTable"]] <- estimatedMarginalMeansTable

  # prepare table
  if (selectedVariable != "")
    estimatedMarginalMeansTable$addColumnInfo(name = "value",     type = "string", title = gettext("Level"))
  .maAddSubgroupColumn(estimatedMarginalMeansTable, options)
  estimatedMarginalMeansTable$addColumnInfo(name = "est",       type = "number", title = gettext("Estimate"))
  .maAddCiColumn(estimatedMarginalMeansTable, options)
  if (parameter == "effectSize") {
    .maAddPiColumn(estimatedMarginalMeansTable, options)
    if (options[["predictionIntervals"]] && .mammHasMultipleHeterogeneities(options, canAddOutput = TRUE)) {
      for (colName in .mammExtractTauLevelNames(fit)) {
        estimatedMarginalMeansTable$addColumnInfo(name = colName, title = colName, type = .maGetVariableColumnType(colName, options), overtitle = gettext("Heterogeneity Level"))
      }
    }
    if (options[["estimatedMarginalMeansEffectSizeTestAgainst"]]) {
      estimatedMarginalMeansTable$addColumnInfo(name = "stat",  type = "number", title = if(.maIsMetaregressionFtest(options)) gettext("t") else gettext("z"))
      if (.maIsMetaregressionFtest(options))
        estimatedMarginalMeansTable$addColumnInfo(name = "df",  type = "number", title = gettext("df"))
      estimatedMarginalMeansTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))
    }
  }

  # get the estimate
  estimatedMarginalMeans <- .maSafeRbind(lapply(fit, .maComputeMarginalMeansVariable,
    options          = options,
    dataset          = dataset,
    selectedVariable = if (selectedVariable == "") "" else strsplit(selectedVariable, ":")[[1]],
    testAgainst      = options[["estimatedMarginalMeansEffectSizeTestAgainstValue"]],
    parameter        = parameter
  ))

  # reorder by estimated marginal means estimate
  estimatedMarginalMeans <- .maSafeOrderAndSimplify(estimatedMarginalMeans, "value", options)

  # drop non-required columns
  estimatedMarginalMeans <- estimatedMarginalMeans[,!colnames(estimatedMarginalMeans) %in% "variable", drop = FALSE]
  if (parameter == "effectSize" && !options[["estimatedMarginalMeansEffectSizeTestAgainst"]])
    estimatedMarginalMeans <- estimatedMarginalMeans[,!colnames(estimatedMarginalMeans) %in% c("df", "stat", "pval"), drop = FALSE]
  if (parameter == "heterogeneity")
    estimatedMarginalMeans <- estimatedMarginalMeans[,!colnames(estimatedMarginalMeans) %in% c("lPi", "uPi"), drop = FALSE]
  if (selectedVariable == "")
    estimatedMarginalMeans <- estimatedMarginalMeans[,!colnames(estimatedMarginalMeans) %in% c("value"), drop = FALSE]

  # set data
  estimatedMarginalMeansTable$setData(estimatedMarginalMeans)

  # add footnotes
  estimatedMarginalMeansMessages <- .maEstimatedMarginalMeansMessages(options, parameter, anyNA(sapply(estimatedMarginalMeans[,colnames(estimatedMarginalMeans) %in% c("est", "lCi", "uCi", "lPi", "uPi")], anyNA)))
  for (i in seq_along(estimatedMarginalMeansMessages))
    estimatedMarginalMeansTable$addFootnote(estimatedMarginalMeansMessages[i])

  return()
}
.maContrastsTable                        <- function(variableContainer, fit, dataset, options, selectedVariable, parameter = "effectSize") {

  contrastsTable <- createJaspTable(gettext("Contrasts"))
  contrastsTable$position <- 1
  contrastsTable$dependOn(switch(
    parameter,
    effectSize    = c("contrastsEffectSize", "contrastsEffectSizePValueAdjustment", "predictionIntervals", "transformEffectSize"),
    heterogeneity = c("contrastsHeterogeneity", "contrastsHeterogeneityPValueAdjustment", "estimatedMarginalMeansHeterogeneityTransformation")
  ))
  variableContainer[["contrastsTable"]] <- contrastsTable

  # prepare table
  contrastsTable$addColumnInfo(name = "comparison", type = "string", title = gettext("Comparison"))
  .maAddSubgroupColumn(contrastsTable, options)
  contrastsTable$addColumnInfo(name = "est",        type = "number", title = gettext("Estimate"))
  .maAddCiColumn(contrastsTable, options)
  if (parameter == "effectSize") {
    .maAddPiColumn(contrastsTable, options)
    # if (options[["predictionIntervals"]] && .mammHasMultipleHeterogeneities(options, canAddOutput = TRUE)) {
    #   TODO?
    #   for (colName in .mammExtractTauLevelNames(fit)) {
    #   contrastsTable$addColumnInfo(name = colName, title = colName, type = .maGetVariableColumnType(colName, options), overtitle = gettext("Heterogeneity Level"))
    #   }
    # }
  }
  contrastsTable$addColumnInfo(name = "stat",  type = "number", title = if(.maIsMetaregressionFtest(options)) gettext("t") else gettext("z"))
  if (.maIsMetaregressionFtest(options))
    contrastsTable$addColumnInfo(name = "df",  type = "number", title = gettext("df"))
  contrastsTable$addColumnInfo(name = "pval",  type = "pvalue", title = gettext("p"))

  # get the estimate
  contrasts <- .maSafeRbind(lapply(fit, .maComputeContrastVariable,
    options          = options,
    dataset          = dataset,
    selectedVariable = if (selectedVariable == "") "" else strsplit(selectedVariable, ":")[[1]],
    testAgainst      = options[["contrastsEffectSizeTestAgainstValue"]],
    parameter        = parameter
  ))

  # reorder by estimated marginal means estimate
  contrasts <- .maSafeOrderAndSimplify(contrasts, "comparison", options)

  # drop non-required columns
  if (parameter == "heterogeneity")
    contrasts <- contrasts[,!colnames(contrasts) %in% c("lPi", "uPi"), drop = FALSE]

  # set data
  contrastsTable$setData(contrasts)

  # add footnotes
  contrastsMessages <- .macontrastsMessages(options, parameter)
  for (i in seq_along(contrastsMessages))
    contrastsTable$addFootnote(contrastsMessages[i])

  return()
}
.maUltimateForestPlot                    <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["forestPlot"]]))
    return()

  if (!any(c(
    options[["forestPlotStudyInformation"]],
    (options[["forestPlotEstimatedMarginalMeans"]] && (
      length(options[["forestPlotEstimatedMarginalMeansSelectedVariables"]]) > 0 ||
      options[["forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"]]
    )),
    options[["forestPlotModelInformation"]]
  )))
    return()

  # the full data set fit is always needed for subgroup analyses
  # there are forest plot specific settings
  options[["includeFullDatasetInSubgroupAnalysis"]] <- TRUE
  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit)) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # try execute!
  plotOut <- try(.maMakeTheUltimateForestPlot(fit, options))

  if (inherits(plotOut, "try-error")) {
    forestPlot <- createJaspPlot(title = gettext("Forest Plot"))
    forestPlot$position <- 4
    forestPlot$dependOn(.maForestPlotDependencies)
    forestPlot$setError(plotOut)
    jaspResults[["forestPlot"]] <- forestPlot
    return()
  }

  # try adjusting height and width
  height <- 200 + (attr(plotOut, "rows")) * 10
  if (!attr(plotOut, "isPanel"))
    width <- 500
  else
    width <- 500 + 500 * attr(plotOut, "panelRatio")

  forestPlot <- createJaspPlot(
    title  = gettext("Forest Plot"),
    width  = width,
    height = height
  )
  forestPlot$position <- 5
  forestPlot$dependOn(.maForestPlotDependencies)

  if (!attr(plotOut, "isPanel")) {
    forestPlot$plotObject <- plotOut
  } else {
    plotOut <- jaspGraphs:::jaspGraphsPlot$new(
      subplots = plotOut,
      layout   = attr(plotOut, "layout"),
      heights  = 1,
      widths   = attr(plotOut, "widths")
    )
    forestPlot$plotObject <- plotOut
  }

  jaspResults[["forestPlot"]] <- forestPlot


  return()
}
.maBubblePlot                            <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["bubblePlot"]]))
    return()

  if (length(options[["bubblePlotSelectedVariable"]]) == 0)
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    bubblePlot       <- .maBubblePlotFun(fit[[1]], dataset, options)
    bubblePlot$title <- gettext("Bubble Plots")
    bubblePlot$dependOn(c(.maBubblePlotDependencies, "includeFullDatasetInSubgroupAnalysis"))
    bubblePlot$position <- 5
    jaspResults[["bubblePlot"]] <- bubblePlot
    return()

  } else {

    # create the output container
    bubblePlot       <- createJaspContainer()
    bubblePlot$title <- gettext("Bubble Plots")
    bubblePlot$dependOn(c(.maBubblePlotDependencies, "includeFullDatasetInSubgroupAnalysis"))
    bubblePlot$position <- 5
    jaspResults[["bubblePlot"]] <- bubblePlot

    for (i in seq_along(fit)) {
      bubblePlot[[names(fit)[i]]]          <- .maBubblePlotFun(fit[[i]], dataset, options)
      bubblePlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      bubblePlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}
.maBubblePlotFun                         <- function(fit, dataset, options) {

  if (jaspBase::isTryError(fit)) {
    return()
  }

  # set dimensions
  width  <- if (length(options[["bubblePlotSeparateLines"]]) == 0 || options[["bubblePlotLegendPosition"]] == "none") 450 else 550
  height <- 350

  # create containers / figure
  if (length(options[["bubblePlotSeparatePlots"]]) > 0) {
    bubblePlot <- createJaspContainer()
  } else {
    bubblePlot <- createJaspPlot(width = width, height = height)
  }

  # make bubble plots
  dfPlot <- .maMakeBubblePlotDataset(fit, options, dataset)

  if (attr(dfPlot, "separatePlots") == "") {
    tempPlots <- list(.maMakeBubblePlot(fit, options, dfPlot))
  } else {
    tempPlots <- lapply(unique(dfPlot[["separatePlots"]]), function(lvl) {
      .maMakeBubblePlot(fit, options, dfPlot[dfPlot[["separatePlots"]] == lvl,], separatePlotsLvl = lvl)
    })
  }

  # modify all generated plots simultaneously
  yRange <- do.call(rbind, lapply(tempPlots, attr, which = "yRange"))
  yRange <- c(min(yRange[, 1]), max(yRange[, 2]))
  yRange <- range(jaspGraphs::getPrettyAxisBreaks(yRange))

  tempPlots <- lapply(tempPlots, function(plot) {
    .maAddBubblePlotTheme(plot, options, dfPlot, yRange)
  })

  if (length(options[["bubblePlotSeparatePlots"]]) > 0) {
    for (i in seq_along(tempPlots)) {
      tempBubblePlot <- createJaspPlot(title = gettextf("%1$s (%2$s)", attr(dfPlot, "separatePlots"), unique(dfPlot[["separatePlots"]])[i]), width = width, height = height)
      tempBubblePlot$position      <- i
      tempBubblePlot$plotObject    <- tempPlots[[i]]
      bubblePlot[[paste0("plot", i)]] <- tempBubblePlot
    }
  } else {
    bubblePlot$plotObject <- tempPlots[[1]]
  }


  return(bubblePlot)
}
.maShowMetaforRCode                      <- function(jaspResults, options) {

  if (!.maReady(options) || !is.null(jaspResults[["metaforRCode"]]))
    return()

  metaforRCode <- createJaspHtml(title = gettext("Metafor R Code"))
  metaforRCode$dependOn(c(.maDependencies, "showMetaforRCode"))
  metaforRCode$position <- 99

  metaforRCode$text <- .maTransformToHtml(.maMakeMetaforCallText(options))

  jaspResults[['metaforRCode']] <- metaforRCode

  return()
}
.maVarianceInflationTable                <- function(jaspResults, dataset, options, parameter = "effectSize") {

  varianceInflationContainer <- .maExtractVarianceInflationContainer(jaspResults)

  if (!is.null(varianceInflationContainer[[parameter]]))
    return()

  if (parameter == "heterogeneity" && !.maIsMetaregressionHeterogeneity(options))
    return()

  fit <- .maExtractFit(jaspResults, options)

  termsTable <- createJaspTable(switch(
    parameter,
    effectSize    = gettext("Effect Size Meta-Regression Variance Inflation"),
    heterogeneity = gettext("Heterogeneity Meta-Regression Variance Inflation")
  ))
  termsTable$position <- switch(
    parameter,
    effectSize    = 1,
    heterogeneity = 2
  )
  varianceInflationContainer[[parameter]] <- termsTable

  termsTable$addColumnInfo(name = "term",  type = "string",  title = "")
  .maAddSubgroupColumn(termsTable, options)
  if (options[["diagnosticsVarianceInflationFactorAggregate"]])
    termsTable$addColumnInfo(name = "m", type = "integer", title = gettext("Parameters"))

  termsTable$addColumnInfo(name = "vif",  type = "number", title = gettext("VIF"))
  termsTable$addColumnInfo(name = "sif",  type = "number", title = gettext("SIF"))

  if (length(fit) == 1 && jaspBase::isTryError(fit[[1]]))
    return()

  terms <- do.call(rbind, lapply(fit, .maComputeVifSummary, options = options, parameter = parameter))
  terms <- .maSafeOrderAndSimplify(terms, "term", options)

  termsTable$setData(terms)

  return()
}
.maCasewiseDiagnosticsTable              <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["casewiseDiagnosticsTable"]]))
    return()

  # diagnostics are unavaible for location-scale models
  if (.maIsMetaregressionHeterogeneity(options)) {
    # fit measures table
    casewiseDiagnosticsTable          <- createJaspTable(gettext("Casewise Diagnostics Table"))
    casewiseDiagnosticsTable$position <- 7
    casewiseDiagnosticsTable$dependOn(c(.maDependencies, "diagnosticsCasewiseDiagnostics", "diagnosticsCasewiseDiagnosticsShowInfluentialOnly",
                                        "diagnosticsCasewiseDiagnosticsIncludePredictors", "diagnosticsCasewiseDiagnosticsDifferenceInCoefficients",
                                        "studyLabels"))
    casewiseDiagnosticsTable$setError(gettext("Casewise diagnostics are not available for models that contain meta-regression on heterogeneity."))
    jaspResults[["casewiseDiagnosticsTable"]] <- casewiseDiagnosticsTable
    return()
  }

  # the fit diagnostics work only for the non-clustered fit
  fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # fit measures table
  casewiseDiagnosticsTable          <- createJaspTable(gettext("Casewise Diagnostics Table"))
  casewiseDiagnosticsTable$position <- 7
  casewiseDiagnosticsTable$dependOn(c(.maDependencies, "diagnosticsCasewiseDiagnostics", "diagnosticsCasewiseDiagnosticsShowInfluentialOnly",
                                      "diagnosticsCasewiseDiagnosticsIncludePredictors", "diagnosticsCasewiseDiagnosticsDifferenceInCoefficients",
                                      "studyLabels"))
  jaspResults[["casewiseDiagnosticsTable"]] <- casewiseDiagnosticsTable

  ### the computation needs to be done before the table to get all the necessary information on column names
  # export diagnostics
  diagnostics <- .maDiagnostics(jaspResults, options)

  # always drop the full from subgroups
  if (options[["subgroup"]] != "" && options[["includeFullDatasetInSubgroupAnalysis"]]) {
    fit         <- fit[-1]
    diagnostics <- diagnostics[-1]
  }

  diagnosticsTable <- .maSafeRbind(lapply(seq_along(fit), function(i) .maRowDiagnosticsTable(
    fit         = fit[[i]],
    diagnostics = diagnostics[[attr(fit[[i]], "subgroup")]],
    options     = options
  )))

  # table information
  coefDifferenceNames <- setdiff(colnames(diagnosticsTable), c("subgroup", "label", .maCasewiseDiagnosticsNames()))

  # prepare table
  .maAddSubgroupColumn(casewiseDiagnosticsTable, options)
  if (options[["studyLabels"]] != "") {
    casewiseDiagnosticsTable$addColumnInfo(name = "label", type  = "string", title = gettext("Label"))
  }
  if (options[["diagnosticsCasewiseDiagnosticsIncludePredictors"]]) {
    for (var in options[["predictors"]]) {
      casewiseDiagnosticsTable$addColumnInfo(name = paste0("pred", var), type  = .maGetVariableColumnType(var, options), title = var, overtitle = gettext("Predictor"))
    }
  }
  casewiseDiagnosticsTable$addColumnInfo(name = "rstudent",  title = gettext("Standardized Residual"),  type = "number")
  if (!.maIsMultilevelMultivariate(options))
    casewiseDiagnosticsTable$addColumnInfo(name = "dffits",  title = gettext("DFFITS"),                 type = "number")
  casewiseDiagnosticsTable$addColumnInfo(name = "cook.d",    title = gettext("Cook's Distance"),        type = "number")
  if (!.maIsMultilevelMultivariate(options)) {
    casewiseDiagnosticsTable$addColumnInfo(name = "cov.r",   title = gettext("Covariance ratio"),       type = "number")
    casewiseDiagnosticsTable$addColumnInfo(name = "tau.del", title = gettext("\U1D70F"),                type = "number", overtitle = gettext("Leave One Out"))
    casewiseDiagnosticsTable$addColumnInfo(name = "tau2.del",title = gettext("\U1D70F\U00B2"),          type = "number", overtitle = gettext("Leave One Out"))
    casewiseDiagnosticsTable$addColumnInfo(name = "QE.del",  title = gettext("Q\U2091"),                type = "number", overtitle = gettext("Leave One Out"))
  }
  casewiseDiagnosticsTable$addColumnInfo(name = "hat",       title = gettext("Hat"),                    type = "number")
  if (!.maIsMultilevelMultivariate(options))
    casewiseDiagnosticsTable$addColumnInfo(name = "weight",  title = gettext("Weight"),                 type = "number")
  if (options[["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"]]) {
    for (par in coefDifferenceNames) {
      casewiseDiagnosticsTable$addColumnInfo(name = par, title = .maVariableNames(par, options[["predictors"]]), type = "number", overtitle = gettext("Difference in coefficients"))
    }
  }
  if (!.maIsMultilevelMultivariate(options))
    casewiseDiagnosticsTable$addColumnInfo(name = "inf", title = gettext("Influential"), type = "string")



  # keep influential only
  if (options[["diagnosticsCasewiseDiagnosticsShowInfluentialOnly"]]) {

    diagnosticsTable <- diagnosticsTable[!is.na(diagnosticsTable[["inf"]]) ,,drop=FALSE]
    diagnosticsTable <- diagnosticsTable[diagnosticsTable[["inf"]] == "Yes",,drop=FALSE]

    # add note if some results are completly ommited
    if (options[["subgroup"]] == "" && nrow(diagnosticsTable) == 0) {
      casewiseDiagnosticsTable$addFootnote(gettext("No influential cases found."))
    } else if (options[["subgroup"]] != "") {
      influentialTable <- table(diagnosticsTable[["subgroup"]])
      for (i in seq_along(influentialTable)[influentialTable == 0]) {
        casewiseDiagnosticsTable$addFootnote(gettextf(
          "%1$sNo influential cases found.",
          if (options[["subgroup"]] != "") gettextf("Subgroup %1$s: ", attr(fit[[i]], "subgroup")) else ""
        ))
      }
    }
  }

  # simplify and store results
  diagnosticsTable <- .maSafeOrderAndSimplify(diagnosticsTable, "subgroup", options)
  casewiseDiagnosticsTable$setData(diagnosticsTable)

  if (options[["subgroup"]] != "")
    casewiseDiagnosticsTable$addFootnote(gettext("Diagnostics are based on the the subgroup models."))
  if (.maIsClustered(options))
    casewiseDiagnosticsTable$addFootnote(gettext("Diagnostics are based on the non-clustered model."))

  return()
}
.maCasewiseDiagnosticsExportColumns      <- function(jaspResults, dataset, options) {

  if (!options[["diagnosticsCasewiseDiagnosticsExportToDataset"]])
    return()

  if (.maIsMetaregressionHeterogeneity(options))
    return()

  # the fit diagnostics work only for the non-clustered fit
  fit <- .maExtractFit(jaspResults, options, nonClustered = TRUE)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # export diagnostics
  diagnostics <- .maDiagnostics(jaspResults, options)

  # always drop the full from subgroups
  if (options[["subgroup"]] != "" && options[["includeFullDatasetInSubgroupAnalysis"]]) {
    fit         <- fit[-1]
    diagnostics <- diagnostics[-1]
  }

  # return the diagnostics with order variable indicating the order in the full data set
  diagnosticsTable <- .maSafeRbind(lapply(seq_along(fit), function(i) .maRowDiagnosticsTable(
    fit         = fit[[i]],
    diagnostics = diagnostics[[attr(fit[[i]], "subgroup")]],
    options     = options,
    forExport   = TRUE
  )))

  # add empty rows for the missing observations
  diagnosticsTable <- .maSafeRbind(list(
    diagnosticsTable,
    data.frame("datasetOrder" = setdiff(1:length(attr(dataset, "NasIds")), diagnosticsTable[["datasetOrder"]]))
  ))

  # order according to the dataset order
  diagnosticsTable <- diagnosticsTable[order(diagnosticsTable[["datasetOrder"]]),, drop = FALSE]

  # export columns:
  if (options[["diagnosticsCasewiseDiagnosticsExportToDatasetInfluentialIndicatorOnly"]]) {

    # export only the influential indicator
    columnName <- "Diagnostics: Influential"
    if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
      .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))

    jaspResults[[columnName]] <- createJaspColumn(columnName = columnName, dependencies = .maDependencies)
    jaspResults[[columnName]]$setNominal(diagnosticsTable[["inf"]])

  } else {

    # export all diagnostics
    diagnosticsNames <- .maCasewiseDiagnosticsNames()
    diagnosticsNames <- intersect(diagnosticsNames, colnames(diagnosticsTable))

    for (diagnosticName in diagnosticsNames) {

      columnName <- paste0("Diagnostics: ", .maCasewiseDiagnosticsExportColumnsNames(diagnosticName))

      if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
        .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))

      jaspResults[[columnName]] <- createJaspColumn(columnName = columnName, dependencies = .maDependencies)
      if (diagnosticName == "inf") {
        jaspResults[[columnName]]$setNominal(diagnosticsTable[[diagnosticName]])
      } else {
        jaspResults[[columnName]]$setScale(diagnosticsTable[[diagnosticName]])
      }
    }

    # export change in coefficients
    if (options[["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"]]) {

      coefDifferenceNames <- setdiff(colnames(diagnosticsTable), c("subgroup", "label", "datasetOrder", .maCasewiseDiagnosticsNames()))

      for (diagnosticName in coefDifferenceNames) {

        columnName <- decodeColNames(paste0("Difference in coefficients: ", .maVariableNames(diagnosticName, options[["predictors"]])))

        if (jaspBase:::columnExists(columnName) && !jaspBase:::columnIsMine(columnName))
          .quitAnalysis(gettextf("Column name %s already exists in the dataset.", columnName))

        jaspResults[[columnName]] <- createJaspColumn(columnName = columnName, dependencies = .maDependencies)
        jaspResults[[columnName]]$setScale(diagnosticsTable[[diagnosticName]])
      }
    }

  }

  return()
}
.maProfileLikelihoodPlot                 <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["profileLikelihoodPlot"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # extract precomputed profile likelihoods if done before:
  dfProfile <- .maProfile(jaspResults, options)

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    profileLikelihoodPlot       <- .maProfileLikelihoodPlotFun(fit[[1]], dfProfile[[1]], options)
    profileLikelihoodPlot$title <- gettext("Profile Likelihood Plot")
    profileLikelihoodPlot$dependOn(c(.maDependencies, "diagnosticsPlotsProfileLikelihood", "includeFullDatasetInSubgroupAnalysis"))
    profileLikelihoodPlot$position <- 8
    jaspResults[["profileLikelihoodPlot"]] <- profileLikelihoodPlot
    return()

  } else {

    # create the output container
    profileLikelihoodPlot       <- createJaspContainer()
    profileLikelihoodPlot$title <- gettext("Profile Likelihood Plot")
    profileLikelihoodPlot$dependOn(c(.maDependencies, "diagnosticsPlotsProfileLikelihood", "includeFullDatasetInSubgroupAnalysis"))
    profileLikelihoodPlot$position <- 8
    jaspResults[["profileLikelihoodPlot"]] <- profileLikelihoodPlot

    for (i in seq_along(fit)) {
      profileLikelihoodPlot[[names(fit)[i]]]          <- .maProfileLikelihoodPlotFun(fit[[i]], dfProfile[[i]], options)
      profileLikelihoodPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      profileLikelihoodPlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}
.maProfileLikelihoodPlotFun              <- function(fit, dfProfile, options) {

  # create profile likelihood plot / container
  if (.maIsMultilevelMultivariate(options)) {

    # container for multivariate
    profileLikelihoodPlot <- createJaspContainer()

    # error plot
    if (jaspBase::isTryError(dfProfile)) {
      errorPlot <- createJaspPlot(title = gettext("Profile Likelihood Plot"))
      errorPlot$setError(dfProfile)
      profileLikelihoodPlot[["errorPlot"]] <- errorPlot
      return(profileLikelihoodPlot)
    }
    if (length(dfProfile) == 0) {
      return()
    }

    # component specific plots
    for (i in 1:dfProfile[["comps"]]) {
      tempProfilePlot <- createJaspPlot(title = paste0(dfProfile[[i]][["title"]][-1], collapse = " "), width = 400, height = 320)
      tempProfilePlot$position <- i
      profileLikelihoodPlot[[paste0("plot", i)]] <- tempProfilePlot
      tempProfilePlot$plotObject <- .maMakeProfileLikelihoodPlot(dfProfile[[i]])
    }

  } else {

    # plot for univariate
    profileLikelihoodPlot <- createJaspPlot(width = 400, height = 320)

    if (.maIsMetaregressionHeterogeneity(options)) {
      profileLikelihoodPlot$setError(gettext("Profile likelihood is not available for models that contain meta-regression on heterogeneity."))
      return(profileLikelihoodPlot)
    }
    if (jaspBase::isTryError(dfProfile)) {
      profileLikelihoodPlot$setError(dfProfile)
      return(profileLikelihoodPlot)
    }
    if (length(dfProfile) == 0) {
      return()
    }

    profileLikelihoodPlot$plotObject <- .maMakeProfileLikelihoodPlot(dfProfile)
  }

  return(profileLikelihoodPlot)
}
.maBaujatPlot                            <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["baujatPlot"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # extract precomputed baujat plot if done before:
  dfBaujat <- .maBaujat(jaspResults, options)

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    baujatPlot       <- .maBaujatPlotFun(dfBaujat[[1]], options)
    baujatPlot$title <- gettext("Baujat Plot")
    baujatPlot$dependOn(c(.maDependencies, "diagnosticsPlotsBaujat", "includeFullDatasetInSubgroupAnalysis"))
    baujatPlot$position <- 9
    jaspResults[["baujatPlot"]] <- baujatPlot
    return()

  } else {

    # create the output container
    baujatPlot       <- createJaspContainer()
    baujatPlot$title <- gettext("Baujat Plot")
    baujatPlot$dependOn(c(.maDependencies, "diagnosticsPlotsBaujat", "includeFullDatasetInSubgroupAnalysis"))
    baujatPlot$position <- 9
    jaspResults[["baujatPlot"]] <- baujatPlot

    for (i in seq_along(fit)) {
      baujatPlot[[names(fit)[i]]]          <- .maBaujatPlotFun(dfBaujat[[i]], options)
      baujatPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      baujatPlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}
.maBaujatPlotFun                         <- function(dfBaujat, options) {

  baujatPlot <- createJaspPlot(width = 400, height = 320)

  # error handling
  if (.maIsMetaregressionHeterogeneity(options)) {
    baujatPlot$setError(gettext("Baujat plot is not available for models that contain meta-regression on heterogeneity."))
    return(baujatPlot)
  }
  if (.maIsClustered(options)) {
    baujatPlot$setError(gettext("Baujat plot is not available for models with clustering."))
    return(baujatPlot)
  }
  if (jaspBase::isTryError(dfBaujat)) {
    baujatPlot$setError(dfBaujat)
    return(baujatPlot)
  }
  if (length(dfBaujat) == 0) {
    return()
  }

  if (options[["studyLabels"]] != "")
    dfBaujat$label <- attr(dfBaujat, "studyLabels")

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(dfBaujat$x, na.rm = TRUE))
  yTicks <- jaspGraphs::getPrettyAxisBreaks(range(dfBaujat$y, na.rm = TRUE))

  aesCall <- list(
    x     = as.name("x"),
    y     = as.name("y"),
    label = if (options[["studyLabels"]] != "") as.name("label")
  )
  geomCall <- list(
    data    = dfBaujat,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)])
  )

  # create plot
  plotOut <- do.call(ggplot2::ggplot, geomCall) +
    jaspGraphs::geom_point(
      size = if (options[["studyLabels"]] != "") 2 else 3
    )

  if (options[["studyLabels"]] != "")
    plotOut <- plotOut + ggplot2::geom_text(hjust = 0, vjust = 0)

  plotOut <- plotOut +
    ggplot2::labs(x = gettext("Squared Pearson Residual"), y = gettext("Influence on Fitted Value")) +
    jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks)) +
    jaspGraphs::scale_y_continuous(breaks = yTicks, limits = range(yTicks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  baujatPlot$plotObject <- plotOut

  return(baujatPlot)
}
.maResidualFunnelPlot                    <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["residualFunnelPlot"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) || !is.null(.maCheckIsPossibleOptions(options)))
    return()

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    residualFunnelPlot       <- .maResidualFunnelPlotFun(fit[[1]], options)
    residualFunnelPlot$title <- gettext("Residual Funnel Plot")
    residualFunnelPlot$dependOn(c(.maDependencies, "diagnosticsResidualFunnel", "studyLabels", "includeFullDatasetInSubgroupAnalysis"))
    residualFunnelPlot$position <- 10
    jaspResults[["residualFunnelPlot"]] <- residualFunnelPlot
    return()

  } else {

    # create the output container
    residualFunnelPlot       <- createJaspContainer()
    residualFunnelPlot$title <- gettext("Residual Funnel Plot")
    residualFunnelPlot$dependOn(c(.maDependencies, "diagnosticsResidualFunnel", "studyLabels", "includeFullDatasetInSubgroupAnalysis"))
    residualFunnelPlot$position <- 10
    jaspResults[["residualFunnelPlot"]] <- residualFunnelPlot

    for (i in seq_along(fit)) {
      residualFunnelPlot[[names(fit)[i]]]          <- .maResidualFunnelPlotFun(fit[[i]], options)
      residualFunnelPlot[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      residualFunnelPlot[[names(fit)[i]]]$position <- i
    }

  }

  return()
}
.maResidualFunnelPlotFun                 <- function(fit, options) {

  # create plot
  residualFunnelPlot <- createJaspPlot(width = 400, height = 320)

  if (jaspBase::isTryError(fit)) {
    return()
  }

  # obtain residual funnel plot
  residualFunnelPlot$plotObject <- .maMakeResidualFunnelPlot(fit, options, attr(fit, "dataset"))

  return(residualFunnelPlot)
}

# containers/state functions
.maExtractFit                        <- function(jaspResults, options, nonClustered = FALSE) {

  if (is.null(jaspResults[["fit"]]$object))
    return()

  # if (!is.null(jaspResults[["fitNoInfluence"]]$object)) {
  #   # extract clustered model if specified
  #   if (!.maIsClustered(options) || nonClustered) {
  #     return(jaspResults[["fitNoInfluence"]]$object[["fit"]])
  #   } else {
  #     return(jaspResults[["fitNoInfluence"]]$object[["fitClustered"]])
  #   }
  # }

  fitOutput          <- jaspResults[["fit"]]$object

  # remove full fit if requested (in subgroup analysis)
  if (!options[["includeFullDatasetInSubgroupAnalysis"]] && options[["subgroup"]] !=  "") {
    fitOutput <- fitOutput[names(fitOutput) != "__fullDataset"]
  }

  fitOutputExtracted <- lapply(fitOutput, function(output){
    # extract clustered model if specified
    if (!.maIsClustered(options) || nonClustered) {
      return(output[["fit"]])
    } else {
      return((output)[["fitClustered"]])
    }
  })

  return(fitOutputExtracted)
}
.maExtractModelSummaryContainer      <- function(jaspResults) {

  if (!is.null(jaspResults[["modelSummaryContainer"]]))
    return(jaspResults[["modelSummaryContainer"]])

  # create the output container
  modelSummaryContainer <- createJaspContainer(gettext("Model Summary"))
  modelSummaryContainer$dependOn(.maDependencies)
  modelSummaryContainer$position <- 1
  jaspResults[["modelSummaryContainer"]] <- modelSummaryContainer

  return(modelSummaryContainer)
}
.maExtractMetaregressionContainer    <- function(jaspResults) {

  if (!is.null(jaspResults[["metaregressionContainer"]]))
    return(jaspResults[["metaregressionContainer"]])

  # create the output container
  metaregressionContainer <- createJaspContainer(gettext("Meta-Regression Summary"))
  metaregressionContainer$dependOn(c(.maDependencies, "confidenceInterval"))
  metaregressionContainer$position <- 3
  jaspResults[["metaregressionContainer"]] <- metaregressionContainer

  return(metaregressionContainer)
}
.maExtractVarianceInflationContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["varianceInflationContainer"]]))
    return(jaspResults[["varianceInflationContainer"]])

  # create the output container
  varianceInflationContainer <- createJaspContainer(gettext("Variance Inflation Summary"))
  varianceInflationContainer$dependOn(c(.maDependencies, "diagnosticsVarianceInflationFactor", "diagnosticsVarianceInflationFactorAggregate", "includeFullDatasetInSubgroupAnalysis"))
  varianceInflationContainer$position <- 7
  jaspResults[["varianceInflationContainer"]] <- varianceInflationContainer

  return(varianceInflationContainer)
}

# help compute functions
.maComputePooledEffect             <- function(fit, options, returnRaw = FALSE) {

  # prediction for effect size of a location-scale models without effect size moderator does not work (compute it manually)
  if (!.maIsMetaregressionEffectSize(options) && .maIsMetaregressionHeterogeneity(options)) {

    predictedHeterogeneity <- .maComputePooledHeterogeneity(fit, options)
    predictedEffect        <- data.frame(
      pred  = fit$beta[1],
      se    = fit$se[1],
      ddf   = fit$ddf[1],
      ci.lb = fit$ci.lb[1],
      ci.ub = fit$ci.ub[1],
      pi.lb = fit$beta[1] - 1.96 * sqrt(fit$se[1]^2 + predictedHeterogeneity[1, 2]^2),
      pi.ub = fit$beta[1] + 1.96 * sqrt(fit$se[1]^2 + predictedHeterogeneity[1, 2]^2)
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
    tauLevels <- list(
      predictedEffect[["tau2.level"]],
      predictedEffect[["gamma2.level"]]
    )
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

  # remove non-requested columns
  predictedEffect <- predictedEffect[,c(
    "par", "est",
    if (options[["confidenceIntervals"]]) c("lCi", "uCi"),
    if (options[["predictionIntervals"]]) c("lPi", "uPi")
  )]

  # return the tau levels
  if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]])
    predictedEffect <- cbind(predictedEffect, tauLevels)

  return(apply(predictedEffect, 1, as.list))
}
.maComputeFixedEffect              <- function(fit, options) {

  # refit the model as a fixed effect model
  fit <- metafor::rma(
    yi     = fit$data[[options[["effectSize"]]]],
    sei    = fit$data[[options[["effectSizeStandardError"]]]],
    method = "FE",
    test   = options[["fixedEffectTest"]]
  )

  predictedEffect <- data.frame(
    pred  = fit$beta[1],
    se    = fit$se[1],
    ddf   = fit$ddf[1],
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

    # to extract the degrees of freedom
    tempDf <- predictedEffect$ddf
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
    predScale <- predict(fit, newscale = colMeans(model.matrix(fit)$scale)[-1], level = 100 * options[["confidenceIntervalsLevel"]])

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

    # keep only the requested parameters
    heterogeneityShow <- c(
      if (options[["heterogeneityTau"]])  1,
      if (options[["heterogeneityTau2"]]) 2,
      if (options[["heterogeneityI2"]])   3,
      if (options[["heterogeneityH2"]])   4
    )

    confIntHeterogeneity <- confIntHeterogeneity[heterogeneityShow,,drop = FALSE]

  }

  if (!options[["confidenceIntervals"]])
    confIntHeterogeneity <- confIntHeterogeneity[,c("par", "est")]

  return(confIntHeterogeneity)
}
.maComputePooledHeterogeneityPlot  <- function(fit, options, parameter = "tau") {

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
    if (parameter == "effectSize")
      row$df2 <- fit[["QMdf"]][2]
    else if (parameter == "heterogeneity")
      row$df2 <- fit[["QSdf"]][2]
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

    out <- anova(fit, btt = selCoef)

    row <- list(
      stat = out[["QM"]],
      df1  = out[["QMdf"]][1],
      pval = out[["QMp"]]
    )

    if (.maIsMetaregressionFtest(options))
      row$df2 <- fit[["QMdf"]][2]

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
      paste(sapply(rownames(fit$beta)[selCoef], function(coefName) .maVariableNames(coefName, options[["predictors"]])), collapse = ", "))
  } else if (parameter == "heterogeneity") {
    row$parameter <- gettextf("Heterogeneity (coef: %1$s)", paste(selCoef, collapse = ", "))
    attr(row, "footnote") <- sapply(rownames(fit$alpha)[selCoef], function(coefName) .maVariableNames(coefName, options[["predictors"]]))
    attr(row, "footnote") <- gettextf(
      "Heterogeneity coefficients %1$s correspond to %2$s.",
      paste(selCoef, collapse = ","),
      paste(sapply(rownames(fit$alpha)[selCoef], function(coefName) .maVariableNames(coefName, options[["predictors"]])), collapse = ", "))
  }

  attr(row, "selCoef") <- selCoef

  return(row)
}
.maTermTests                       <- function(fit, options, term, parameter = "effectSize") {

  # obtain terms indicies
  if (parameter == "effectSize") {

    terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")     # get terms indices from the model
    termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")  # get coefficient indices from the model matrix
    termsIndex <- termsIndex[!fit$coef.na]                                                   # remove dropped coefficients

    # deal with the possibility that all coefficients of the corresponding term were dropped
    if (sum(termsIndex == which(terms == term)) == 0) {

      out <- list(
        term = .maVariableNames(term, options[["predictors"]]),
        stat = NA,
        df1  = NA,
        pval = NA
      )

      if (.maIsMetaregressionFtest(options))
        out$df2 <- NA

      if (.maIsPermutation(options))
        out$pval2 <- NA

    } else {

      termsAnova <- anova(fit, btt = seq_along(termsIndex)[termsIndex == which(terms == term)])

      out <- list(
        term = .maVariableNames(term, options[["predictors"]]),
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
    termsIndex <- termsIndex[!fit$coef.na.Z]                                                   # remove dropped coefficients

    # deal with the possibility that all coefficients of the corresponding term were dropped
    if (sum(termsIndex == which(terms == term)) == 0) {

      out <- list(
        term = .maVariableNames(term, options[["predictors"]]),
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
        term = .maVariableNames(term, options[["predictors"]]),
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
.maGetMarginalMeansPredictorMatrix <- function(fit, options, dataset, selectedVariables, trendVarible = NULL, trendSequence = NULL, sdFactor, parameter) {

  variablesContinuous <- options[["predictors"]][options[["predictors.types"]] == "scale"]
  variablesFactors    <- options[["predictors"]][options[["predictors.types"]] == "nominal"]

  # extract the corresponding formula
  formula <- switch(
    parameter,
    effectSize    = fit[["formula.mods"]],
    heterogeneity = fit[["formula.scale"]]
  )
  hasIntercept <- switch(
    parameter,
    effectSize    = options[["effectSizeModelIncludeIntercept"]],
    heterogeneity = options[["heterogeneityModelIncludeIntercept"]]
  )

  # extract the used variables
  terms     <- attr(terms(formula, data = fit[["data"]]), "term.labels")
  variables <- terms[!grepl(":", terms)]

  # average across remaining variables
  remainingVariables <- setdiff(variables, c(selectedVariables, trendVarible))

  ### create model matrix for the remaining predictors
  # (use all factors for levels to average out the predictor matrix later)
  predictorsRemaining <- list()
  for (i in seq_along(remainingVariables)) {
    if (remainingVariables[[i]] %in% variablesFactors) {
      predictorsRemaining[[remainingVariables[i]]] <- factor(levels(dataset[[remainingVariables[[i]]]]), levels = levels(dataset[[remainingVariables[[i]]]]))
      contrasts(predictorsRemaining[[remainingVariables[i]]]) <- contrasts(dataset[[remainingVariables[[i]]]])
    } else if (remainingVariables[[i]] %in% variablesContinuous) {
      predictorsRemaining[[remainingVariables[i]]] <- mean(dataset[[remainingVariables[[i]]]])
    }
  }

  # create complete model matrices including the specified variable
  predictorsSelected <- list()
  predictorsSelectedNames <- list()
  if (length(selectedVariables) > 0) {
    for (selectedVariable in selectedVariables) {
      if (selectedVariable %in% variablesFactors) {
        predictorsSelected[[selectedVariable]] <- factor(levels(dataset[[selectedVariable]]), levels = levels(dataset[[selectedVariable]]))
        predictorsSelectedNames[[selectedVariable]] <- levels(dataset[[selectedVariable]])
        contrasts(predictorsSelected[[selectedVariable]]) <- contrasts(dataset[[selectedVariable]])
      } else if (selectedVariable %in% variablesContinuous) {
        predictorsSelected[[selectedVariable]] <- c(
          mean(dataset[[selectedVariable]]) - sdFactor * sd(dataset[[selectedVariable]]),
          mean(dataset[[selectedVariable]]),
          mean(dataset[[selectedVariable]]) + sdFactor * sd(dataset[[selectedVariable]])
        )
        predictorsSelectedNames[[selectedVariable]] <- c(
          gettextf("Mean - %1$sSD", sdFactor),
          gettext("Mean"),
          gettextf("Mean + %1$sSD", sdFactor)
        )
      }
    }
  }


  # create model matrix for the trend variable
  if (length(trendVarible) != 0) {
    predictorsSelected[[trendVarible]] <- trendSequence
  }

  # add the specified variable and pool across the combinations of the remaining values
  if (length(selectedVariables) == 1 && selectedVariables == "") {
    # empty string creates overall adjusted estimate
    outMatrix <- t(colMeans(model.matrix(formula, data = expand.grid(predictorsRemaining))))
    predictorsSelectedGridNames <- matrix("")
  } else {
    predictorsSelectedGrid      <- expand.grid(predictorsSelected)
    predictorsSelectedGridNames <- expand.grid(predictorsSelectedNames)
    outMatrix <- do.call(rbind, lapply(1:nrow(predictorsSelectedGrid), function(i) {
      colMeans(model.matrix(formula, data = expand.grid(c(predictorsRemaining,  predictorsSelectedGrid[i,,drop = FALSE]))))
    }))
  }

  # remove entries corresponding to omitted coefficients
  if (parameter == "effectSize" && any(fit$coef.na)) {
    outMatrix <- outMatrix[, !fit$coef.na, drop=FALSE]
  } else if (parameter == "heterogeneity" && any(fit$coef.na.Z)) {
    outMatrix <- outMatrix[, !fit$coef.na.Z, drop=FALSE]
  }

  if (hasIntercept)
    outMatrix <- outMatrix[, -1, drop=FALSE]

  # keep information about the variable and levels
  if (length(selectedVariables) == 1 && selectedVariables == "") {

    # add intercept
    attr(outMatrix, "variable") <- gettext("Adjusted estimate")
    attr(outMatrix, gettext("Adjusted estimate")) <- ""
    attr(outMatrix, "selectedGridNames") <- predictorsSelectedGridNames

  } else {

    # selected variables grid
    attr(outMatrix, "selectedGrid") <- predictorsSelectedGrid
    attr(outMatrix, "selectedGridNames") <- predictorsSelectedGridNames

    # add remaining variables
    attr(outMatrix, "variable") <- c(selectedVariables, trendVarible)

    for (selectedVariable in selectedVariables) {
      if (selectedVariable %in% variablesFactors) {
        attr(outMatrix, selectedVariable) <- predictorsSelected[[selectedVariable]]
      } else if (selectedVariable %in% variablesContinuous) {
        attr(outMatrix, selectedVariable) <- c(
          gettextf("Mean - %1$sSD", sdFactor),
          gettext("Mean"),
          gettextf("Mean + %1$sSD", sdFactor))
      }
    }
  }

  if (length(trendVarible) != 0) {
    attr(outMatrix, "trend") <- trendVarible
    attr(outMatrix, "trend") <- trendSequence
  }

  return(outMatrix)

}
.maComputeMarginalMeansVariable    <- function(fit, options, dataset, selectedVariable, testAgainst = 0, parameter) {

  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  if (parameter == "effectSize") {

    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
      parameter         = "effectSize"
    )

    if (.maIsMetaregressionHeterogeneity(options)) {

      predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
        fit               = fit,
        options           = options,
        dataset           = dataset,
        selectedVariables = selectedVariable,
        sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
        parameter         = "heterogeneity"
      )
      computedMarginalMeans <- predict(
        fit,
        newmods  = predictorMatrixEffectSize,
        newscale = predictorMatrixHeterogeneity,
        level    = 100 * options[["confidenceIntervalsLevel"]]
      )
    } else {

      if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {
        tauLevelsMatrix            <- .mammExtractTauLevels(fit)
        tempPredictorMatrixRepeats <- rep(1:nrow(predictorMatrixEffectSize), each = nrow(tauLevelsMatrix)) # repeat the predictors for each tau level
        attr(predictorMatrixEffectSize, attr(predictorMatrixEffectSize, "variable")) <- attr(predictorMatrixEffectSize, attr(predictorMatrixEffectSize, "variable"))[tempPredictorMatrixRepeats]
        computedMarginalMeans <- predict(
          fit,
          newmods = predictorMatrixEffectSize[tempPredictorMatrixRepeats,,drop=FALSE],
          level   = 100 * options[["confidenceIntervalsLevel"]],
          tau2.levels   = if (is.null(dim(predictorMatrixEffectSize))) tauLevelsMatrix[["tau2.levels"]]   else do.call(rbind, lapply(1:nrow(predictorMatrixEffectSize), function(i) tauLevelsMatrix))[["tau2.levels"]],
          gamma2.levels = if (is.null(dim(predictorMatrixEffectSize))) tauLevelsMatrix[["gamma2.levels"]] else do.call(rbind, lapply(1:nrow(predictorMatrixEffectSize), function(i) tauLevelsMatrix))[["gamma2.levels"]]
        )
      } else {
        computedMarginalMeans <- predict(
          fit,
          newmods = predictorMatrixEffectSize,
          level   = 100 * options[["confidenceIntervalsLevel"]]
        )
      }
    }

    if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {
      tauLevels <- list(
        computedMarginalMeans[["tau2.level"]],
        computedMarginalMeans[["gamma2.level"]]
      )
      tauLevels           <- do.call(cbind.data.frame, tauLevels[!sapply(tauLevels, is.null)])
      colnames(tauLevels) <- .mammExtractTauLevelNames(fit)
    }


    # compute test against specified value
    if (.maIsMetaregressionFtest(options)) {

      # extract degrees of freedom
      tempDf                     <- computedMarginalMeans$ddf
      computedMarginalMeans      <- .maExtractAndFormatPrediction(computedMarginalMeans)
      computedMarginalMeans$df   <- tempDf
      computedMarginalMeans$stat <- (computedMarginalMeans$est - testAgainst)  / computedMarginalMeans$se
      computedMarginalMeans$pval <- 2 * pt(abs(computedMarginalMeans$stat), computedMarginalMeans$df, lower.tail = FALSE)

    } else {

      computedMarginalMeans      <- .maExtractAndFormatPrediction(computedMarginalMeans)
      computedMarginalMeans$stat <- (computedMarginalMeans$est - testAgainst)  / computedMarginalMeans$se
      computedMarginalMeans$pval <- 2 * pnorm(abs(computedMarginalMeans$stat), lower.tail = FALSE)

    }

    # apply effect size transformation
    if (options[["transformEffectSize"]] != "none")
      computedMarginalMeans[,c("est", "lCi", "uCi", "lPi", "uPi")] <- do.call(
        .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
        list(computedMarginalMeans[,c("est", "lCi", "uCi", "lPi", "uPi")]))

    # create full data frame
    computedMarginalMeans <- data.frame(
      "variable" = paste0(attr(predictorMatrixEffectSize, "variable"), collapse = jaspBase::interactionSymbol),
      "value"    = apply(attr(predictorMatrixEffectSize, "selectedGridNames"), 1, paste0, collapse = ", "),
      computedMarginalMeans
    )

  } else if (parameter == "heterogeneity") {

    predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansHeterogeneitySdFactorCovariates"]],
      parameter         = "heterogeneity"
    )

    computedMarginalMeans <- predict(
      fit,
      newscale = predictorMatrixHeterogeneity,
      level    = 100 * options[["confidenceIntervalsLevel"]]
    )

    computedMarginalMeans <- .maExtractAndFormatPrediction(computedMarginalMeans)


    # apply link transform
    if (options[["heterogeneityModelLink"]] == "log") {
      computedMarginalMeans <- exp(computedMarginalMeans)
    }

    # apply tau / tau2 transform
    if (options[["estimatedMarginalMeansHeterogeneityTransformation"]] == "tau")
      computedMarginalMeans <- sqrt(computedMarginalMeans)

    # create full data frame
    computedMarginalMeans <- data.frame(
      "variable" = paste0(attr(predictorMatrixHeterogeneity, "variable"), collapse = jaspBase::interactionSymbol),
      "value"    = apply(attr(predictorMatrixHeterogeneity, "selectedGridNames"), 1, paste0, collapse = ", "),
      computedMarginalMeans
    )
  }


  # remove unnecessary columns
  computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% "se", drop = FALSE]

  if (!options[["confidenceIntervals"]])
    computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% c("lCi", "uCi"), drop = FALSE]

  if (!options[["predictionIntervals"]])
    computedMarginalMeans <- computedMarginalMeans[,!colnames(computedMarginalMeans) %in% c("lPi", "uPi"), drop = FALSE]

  # return the tau levels
  if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]])
    computedMarginalMeans <- cbind(computedMarginalMeans, tauLevels)

  computedMarginalMeans$subgroup <- attr(fit, "subgroup")

  return(computedMarginalMeans)
}
.maComputeContrastVariable         <- function(fit, options, dataset, selectedVariable, testAgainst = 0, parameter) {

  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  if (parameter == "effectSize") {

    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
      parameter         = "effectSize"
    )

    selectedVariableLevels   <- apply(attr(predictorMatrixEffectSize, "selectedGridNames"), 1, paste0, collapse = ", ")
    contrastMatrixEffectSize <- matrix(NA, nrow = nrow(predictorMatrixEffectSize) * (nrow(predictorMatrixEffectSize) - 1) / 2, ncol = ncol(predictorMatrixEffectSize))
    contrastComparisons      <- character(nrow(contrastMatrixEffectSize))

    thisContrast <- 1
    for (i in 1:length(selectedVariableLevels)){
      for (j in 1:length(selectedVariableLevels)){
        if (j > i) {
          contrastMatrixEffectSize[thisContrast,] <- predictorMatrixEffectSize[i,] - predictorMatrixEffectSize[j,]
          contrastComparisons[thisContrast]       <- paste0(selectedVariableLevels[i], " – ", selectedVariableLevels[j])
          thisContrast <- thisContrast + 1
        }
      }
    }

    if (.maIsMetaregressionHeterogeneity(options)) {

      predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
        fit               = fit,
        options           = options,
        dataset           = dataset,
        selectedVariables = selectedVariable,
        sdFactor          = options[["estimatedMarginalMeansEffectSizeSdFactorCovariates"]],
        parameter         = "heterogeneity"
      )
      contrastMatrixHeterogeneity <- matrix(NA, nrow = nrow(predictorMatrixEffectSize) * (nrow(predictorMatrixEffectSize) - 1) / 2, ncol = ncol(predictorMatrixHeterogeneity))

      thisContrast <- 1
      for (i in 1:length(selectedVariableLevels)){
        for (j in 1:length(selectedVariableLevels)){
          if (j > i) {
            contrastMatrixHeterogeneity[thisContrast,] <- predictorMatrixHeterogeneity[i,] - predictorMatrixHeterogeneity[j,]
            thisContrast <- thisContrast + 1
          }
        }
      }

      computedContrasts <- predict(
        fit,
        newmods  = contrastMatrixEffectSize,
        newscale = contrastMatrixHeterogeneity,
        level    = 100 * options[["confidenceIntervalsLevel"]]
      )
      computedContrastsTests <- anova(
        fit,
        X      = contrastMatrixEffectSize,
        adjust = .maGetPValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]])
      )

    } else {

      if (FALSE){ # .mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {

        # # TODO?
        # tauLevelsMatrix            <- .mammExtractTauLevels(fit)
        # tempPredictorMatrixRepeats <- rep(1:nrow(predictorMatrixEffectSize), each = nrow(tauLevelsMatrix)) # repeat the predictors for each tau level
        # attr(predictorMatrixEffectSize, attr(predictorMatrixEffectSize, "variable")) <- attr(predictorMatrixEffectSize, attr(predictorMatrixEffectSize, "variable"))[tempPredictorMatrixRepeats]
        # computedMarginalMeans <- predict(
        #   fit,
        #   newmods = predictorMatrixEffectSize[tempPredictorMatrixRepeats,,drop=FALSE],
        #   level   = 100 * options[["confidenceIntervalsLevel"]],
        #   tau2.levels   = if (is.null(dim(predictorMatrixEffectSize))) tauLevelsMatrix[["tau2.levels"]]   else do.call(rbind, lapply(1:nrow(predictorMatrixEffectSize), function(i) tauLevelsMatrix))[["tau2.levels"]],
        #   gamma2.levels = if (is.null(dim(predictorMatrixEffectSize))) tauLevelsMatrix[["gamma2.levels"]] else do.call(rbind, lapply(1:nrow(predictorMatrixEffectSize), function(i) tauLevelsMatrix))[["gamma2.levels"]]
        # )


      } else {

        computedContrasts <- predict(
          fit,
          newmods = contrastMatrixEffectSize,
          level   = 100 * options[["confidenceIntervalsLevel"]]
        )
        computedContrastsTests <- anova(
          fit,
          X      = contrastMatrixEffectSize,
          adjust = .maGetPValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]])
        )

      }
    }

    if (FALSE) {#.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]]) {
      # # TODO?
      # tauLevels <- list(
      #   computedMarginalMeans[["tau2.level"]],
      #   computedMarginalMeans[["gamma2.level"]]
      # )
      # tauLevels           <- do.call(cbind.data.frame, tauLevels[!sapply(tauLevels, is.null)])
      # colnames(tauLevels) <- .mammExtractTauLevelNames(fit)
    }

  } else if (parameter == "heterogeneity") {

    predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = selectedVariable,
      sdFactor          = options[["estimatedMarginalMeansHeterogeneitySdFactorCovariates"]],
      parameter         = "heterogeneity"
    )

    selectedVariableLevels      <- apply(attr(predictorMatrixHeterogeneity, "selectedGridNames"), 1, paste0, collapse = ", ")
    contrastMatrixHeterogeneity <- matrix(NA, nrow = nrow(predictorMatrixHeterogeneity) * (nrow(predictorMatrixHeterogeneity) - 1) / 2, ncol = ncol(predictorMatrixHeterogeneity))
    contrastComparisons         <- character(nrow(contrastMatrixHeterogeneity))

    thisContrast <- 1
    for (i in 1:length(selectedVariableLevels)){
      for (j in 1:length(selectedVariableLevels)){
        if (j > i) {
          contrastMatrixHeterogeneity[thisContrast,] <- predictorMatrixHeterogeneity[i,] - predictorMatrixHeterogeneity[j,]
          contrastComparisons[thisContrast]          <- paste0(selectedVariableLevels[i], " – ", selectedVariableLevels[j])
          thisContrast <- thisContrast + 1
        }
      }
    }

    computedContrasts <- predict(
      fit,
      newscale = contrastMatrixHeterogeneity,
      level    = 100 * options[["confidenceIntervalsLevel"]]
    )
    computedContrastsTests <- anova(
      fit,
      Z      = contrastMatrixHeterogeneity,
      adjust = .maGetPValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]])
    )

    # neither link or tau transformation cannot be applied
  }

  # reformat
  computedContrasts <- .maExtractAndFormatPrediction(computedContrasts)

  # remove unnecessary columns
  computedContrasts <- computedContrasts[,!colnames(computedContrasts) %in% "se", drop = FALSE]

  if (!options[["confidenceIntervals"]])
    computedContrasts <- computedContrasts[,!colnames(computedContrasts) %in% c("lCi", "uCi"), drop = FALSE]

  if (!options[["predictionIntervals"]])
    computedContrasts <- computedContrasts[,!colnames(computedContrasts) %in% c("lPi", "uPi"), drop = FALSE]

  # TODO: ? return the tau levels
  # if (.mammHasMultipleHeterogeneities(options, canAddOutput = TRUE) && options[["predictionIntervals"]])
  #   computedMarginalMeans <- cbind(computedMarginalMeans, tauLevels)

  # add test results
  computedContrasts$comparison <- contrastComparisons
  computedContrasts$stat       <- computedContrastsTests$zval
  if (.maIsMetaregressionFtest(options))
    computedContrasts$df <- computedContrastsTests$ddf
  computedContrasts$pval <- computedContrastsTests$pval

  computedContrasts$subgroup <- attr(fit, "subgroup")

  return(computedContrasts)
}
.maMakeBubblePlotDataset           <- function(fit, options, dataset) {

  # extract options
  separateLines        <- unlist(options[["bubblePlotSeparateLines"]])
  separatePlots        <- unlist(options[["bubblePlotSeparatePlots"]])
  selectedVariable     <- options[["bubblePlotSelectedVariable"]][[1]][["variable"]]
  selectedVariableType <- options[["predictors.types"]][options[["predictors"]] == selectedVariable]

  # create a range of values for continuous predictors to plot the trend but use lvls for factors
  if (selectedVariableType == "scale") {

    xRange <- range(jaspGraphs::getPrettyAxisBreaks(range(dataset[[selectedVariable]])))
    trendSequence <- seq(xRange[1], xRange[2], length.out =  101)

    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = c(separateLines, separatePlots),
      sdFactor          = options[["bubblePlotSdFactorCovariates"]],
      trendVarible      = selectedVariable,
      trendSequence     = trendSequence,
      parameter         = "effectSize"
    )

  } else if (selectedVariableType == "nominal") {

    predictorMatrixEffectSize <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = c(selectedVariable, separateLines, separatePlots),
      sdFactor          = options[["bubblePlotSdFactorCovariates"]],
      parameter         = "effectSize"
    )

  }


  if (.maIsMetaregressionHeterogeneity(options)) {

    predictorMatrixHeterogeneity <- .maGetMarginalMeansPredictorMatrix(
      fit               = fit,
      options           = options,
      dataset           = dataset,
      selectedVariables = c(separateLines, separatePlots),
      sdFactor          = options[["bubblePlotSdFactorCovariates"]],
      trendVarible      = selectedVariable,
      trendSequence     = trendSequence,
      parameter         = "heterogeneity"
    )

    computedMarginalMeans <- predict(
      fit,
      newmods  = predictorMatrixEffectSize,
      newscale = predictorMatrixHeterogeneity,
      level    = 100 * options[["confidenceIntervalsLevel"]]
    )
  } else {

    computedMarginalMeans <- predict(
      fit,
      newmods = predictorMatrixEffectSize,
      level   = 100 * options[["confidenceIntervalsLevel"]]
    )
  }

  ### modify and rename selectedGrid
  selectedGrid <- attr(predictorMatrixEffectSize, "selectedGrid")
  selectedGrid$selectedVariable <- selectedGrid[,selectedVariable]
  # deal with continuous variables dichotomization
  selectedGrid     <- .maDichotomizeVariablesLevels(selectedGrid, c(separateLines, separatePlots), options)
  continuousLevels <- attr(selectedGrid, "continuousLevels")
  # collapse factor levels if multiple selected
  selectedGrid <- .maMergeVariablesLevels(selectedGrid, separateLines, "separateLines")
  selectedGrid <- .maMergeVariablesLevels(selectedGrid, separatePlots, "separatePlots")
  # remove original names
  selectedGrid <- selectedGrid[,setdiff(names(selectedGrid), c(selectedVariable, separateLines, separatePlots)),drop = FALSE]

  ### modify marginal means
  computedMarginalMeans <- .maExtractAndFormatPrediction(computedMarginalMeans)

  ### merge and add attributes
  dfPlot <- cbind.data.frame(selectedGrid, computedMarginalMeans)

  attr(dfPlot, "selectedVariable")     <- selectedVariable
  attr(dfPlot, "selectedVariableType") <- selectedVariableType
  attr(dfPlot, "separateLines")    <- paste(separateLines, collapse = " | ")
  attr(dfPlot, "separatePlots")    <- paste(separatePlots, collapse = " | ")
  attr(dfPlot, "variablesLines")   <- separateLines
  attr(dfPlot, "variablesPlots")   <- separatePlots
  attr(dfPlot, "continuousLevels") <- continuousLevels[!sapply(continuousLevels, is.null)]
  attr(dfPlot, "xRange")           <- if (selectedVariableType == "scale") xRange

  return(dfPlot)
}
.maMakeBubblePlot                  <- function(fit, options, dfPlot, separatePlotsLvl = NULL) {

  bubblePlot <- ggplot2::ggplot()
  yRange     <- NULL

  hasSeparateLines <- attr(dfPlot, "separateLines") != ""
  hasSeparatePlots <- attr(dfPlot, "separatePlots") != ""

  ### add prediction bands
  if (options[["bubblePlotPredictionIntervals"]]) {

    geomPi <- .maBubblePlotMakeCiGeom(dfPlot, options, ci = FALSE)

    if (!is.null(geomPi)) {
      bubblePlot <- bubblePlot + do.call(geomPi$what, geomPi$args)
      yRange     <- attr(geomPi, "yRange")
    } else {
      yRange     <- NA
    }

  }

  ### add confidence bands
  if (options[["bubblePlotConfidenceIntervals"]]) {

    geomCi <- .maBubblePlotMakeCiGeom(dfPlot, options, ci = TRUE)

    if (!is.null(geomCi)) {
      bubblePlot <- bubblePlot + do.call(geomCi$what, geomCi$args)
      yRange     <- range(c(yRange, attr(geomCi, "yRange")), na.rm = TRUE)
    }

  }

  ### add prediction line
  if (attr(dfPlot, "selectedVariableType") == "scale") {
    aesCall <- list(
      x     = as.name("selectedVariable"),
      y     = as.name("est"),
      color = if (hasSeparateLines) as.name("separateLines")
    )
    dfPlot[["y"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfPlot[["y"]]))
    geomCall <- list(
      data    = dfPlot,
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)])
    )
    bubblePlot <- bubblePlot + do.call(jaspGraphs::geom_line, geomCall)
    yRange <- range(c(yRange, dfPlot$pred), na.rm = TRUE)
  }

  ### add studies as bubbles
  dfStudies <- data.frame(
    effectSize       = fit[["yi"]],
    inverseVariance  = 1/fit[["vi"]],
    weight           = weights(fit),
    constant         = rep(options[["bubblePlotBubblesRelativeSize"]], nrow(fit[["data"]])),
    selectedVariable = fit[["data"]][[attr(dfPlot, "selectedVariable")]]
  )

  # add separate lines and plots
  if (hasSeparateLines)
    dfStudies[attr(dfPlot, "variablesLines")] <- fit[["data"]][attr(dfPlot, "variablesLines")]
  if (hasSeparatePlots)
    dfStudies[attr(dfPlot, "variablesPlots")] <- fit[["data"]][attr(dfPlot, "variablesPlots")]

  # make same encoding
  dfStudies <- .maDichotomizeVariablesDataset(dfStudies, c(attr(dfPlot, "variablesLines"), attr(dfPlot, "variablesPlots")), attr(dfPlot, "continuousLevels"), options)
  dfStudies <- .maMergeVariablesLevels(dfStudies, variablesLines <- attr(dfPlot, "variablesLines"), "separateLines")
  dfStudies <- .maMergeVariablesLevels(dfStudies, variablesLines <- attr(dfPlot, "variablesPlots"), "separatePlots")
  if (hasSeparateLines)
    levels(dfStudies[,"separateLines"]) <- levels(dfPlot[,"separateLines"])

  # subset original data across plots
  if (!is.null(separatePlotsLvl))
    dfStudies <- dfStudies[dfStudies$separatePlots == separatePlotsLvl,]

  aesCall <- list(
    x     = as.name("selectedVariable"),
    y     = as.name("effectSize"),
    size  = switch(
      options[["bubblePlotBubblesSize"]],
      "weight"          = as.name("weight"),
      "inverseVariance" = as.name("inverseVariance"),
      "equal"           = as.name("constant")
    ),
    color = if (hasSeparateLines) as.name("separateLines"),
    fill  = if (hasSeparateLines) as.name("separateLines"),
    alpha = options[["bubblePlotBubblesTransparency"]]
  )

  dfStudies[["effectSize"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfStudies[["effectSize"]]))

  geomCall <- list(
    data    = dfStudies,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
    show.legend = FALSE
  )
  if (attr(dfPlot, "selectedVariableType") == "nominal" && hasSeparateLines) {
    geomCall$position <- ggplot2::position_jitterdodge(
      jitter.width  = 0.35 * options[["bubblePlotBubblesJitter"]],
      jitter.height = 0,
      dodge.width   = 0.9
    )
  }else if (attr(dfPlot, "selectedVariableType") == "nominal") {
    geomCall$position <- ggplot2::position_jitter(
      width       = 0.35 * options[["bubblePlotBubblesJitter"]],
      height      = 0
    )
  }

  bubblePlot <- bubblePlot + do.call(jaspGraphs::geom_point, geomCall) +
    ggplot2::scale_size(range = c(1.5, 10) * options[["bubblePlotBubblesRelativeSize"]])
  yRange     <- range(c(yRange, dfStudies[["effectSize"]]))

  # add color palette
  bubblePlot <- bubblePlot +
    jaspGraphs::scale_JASPcolor_discrete(options[["colorPalette"]]) +
    jaspGraphs::scale_JASPfill_discrete(options[["colorPalette"]])

  attr(bubblePlot, "yRange") <- yRange
  return(bubblePlot)
}
.maAddBubblePlotTheme              <- function(plot, options, dfPlot, yRange) {


  selectedVariableType <- attr(dfPlot, "selectedVariableType")

  if (selectedVariableType == "scale") {
    plot <- plot +
      jaspGraphs::scale_x_continuous(
        name   = attr(dfPlot, "selectedVariable"),
        breaks = jaspGraphs::getPrettyAxisBreaks(attr(dfPlot, "xRange")),
        limits = attr(dfPlot, "xRange")
      )
  } else if (selectedVariableType == "nominal") {
    plot <- plot +
      ggplot2::scale_x_discrete(
        name   = attr(dfPlot, "selectedVariable")
      )
  }

  plot <- plot +
    jaspGraphs::scale_y_continuous(
      name   = if (options[["transformEffectSize"]] == "none") gettext("Effect Size") else .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]]),
      breaks = jaspGraphs::getPrettyAxisBreaks(yRange),
      limits = yRange
    )

  if (attr(dfPlot, "separateLines") != "")
    plot <- plot + ggplot2::labs(fill = attr(dfPlot, "separateLines"), color = attr(dfPlot, "separateLines"))

  if (options[["bubblePlotTheme"]] == "jasp") {

    plot <- plot +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw(legend.position = if (attr(dfPlot, "separateLines") == "") "none" else options[["bubblePlotLegendPosition"]])

  } else {

    plot <- plot +
      switch(
        options[["bubblePlotTheme"]],
        "whiteBackground" = ggplot2::theme_bw()       + ggplot2::theme(legend.position = "bottom"),
        "light"           = ggplot2::theme_light()    + ggplot2::theme(legend.position = "bottom"),
        "minimal"         = ggplot2::theme_minimal()  + ggplot2::theme(legend.position = "bottom"),
        "pubr"            = jaspGraphs::themePubrRaw(legend = options[["bubblePlotLegendPosition"]]),
        "apa"             = jaspGraphs::themeApaRaw(legend.pos = switch(
          options[["bubblePlotLegendPosition"]],
          "none"   = "none",
          "bottom" = "bottommiddle",
          "right"  = "bottomright",
          "top"    = "topmiddle",
          "left"   = "bottomleft"
        ))
      )

    plot <- plot + ggplot2::theme(
      legend.text  = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      legend.title = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      axis.text    = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      axis.title   = ggplot2::element_text(size = ggplot2::rel(options[["bubblePlotRelativeSizeText"]])),
      legend.position = if (attr(dfPlot, "separateLines") == "") "none" else options[["bubblePlotLegendPosition"]])
  }

  return(plot)
}
.maMakeMetaforCallText             <- function(options) {

  if (options[["module"]] == "metaAnalysis") {
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      sei  = as.name(options[["effectSizeStandardError"]]),
      data = as.name("dataset")
    )
  } else if (options[["module"]] == "metaAnalysisMultilevelMultivariate") {
    # TODO: extend to covariance matrices
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      V    = paste0(options[["effectSizeStandardError"]], "^2"), # precomputed on data load
      data = as.name("dataset")
    )
  }

  # add formulas if specified
  rmaInput$mods  <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  rmaInput$scale <- .maGetFormula(options[["heterogeneityModelTerms"]], options[["heterogeneityModelIncludeIntercept"]])

  # add random effects
  if (.maIsMultilevelMultivariate(options)) {
    randomFormulaList <- .mammGetRandomFormulaList(options)
    if (length(randomFormulaList) != 0) {
      struct      <- do.call(c, lapply(randomFormulaList, attr, "structure"))
      dist        <- unlist(unname(lapply(randomFormulaList, attr, which = "dist")), recursive = FALSE)
      R           <- unlist(unname(lapply(randomFormulaList, attr, which = "R")), recursive = FALSE)
      # change distance matrix into a variable
      for (i in seq_along(dist)) {
        if (is.matrix(dist[[i]]))
          dist[[i]] <- paste0(names(dist)[i], gettext(" Distance Matrix"))
      }
      # change correlation matrix into a variable
      for (i in seq_along(R)) {
        R[[i]] <- paste0(names(R)[i], gettext(" Correlation Matrix"))
      }

      if (length(randomFormulaList) > 1)
        randomFormulaList <- paste0("list(\n\t\t", paste0("'", names(randomFormulaList), "' = ", randomFormulaList, collapse = "\n\t\t"),")")
      rmaInput$random <- randomFormulaList
      if (length(struct) != 0)
        struct <- paste0("c(", paste0("'", names(struct), "' = '", struct, "'", collapse = ", "),")")
      rmaInput$struct <- struct
      if (length(dist) > 0)
        dist <- paste0("list(", paste0(names(dist), ifelse(names(dist) == "", "'", " = '"), dist, "'", collapse = ", "),")")
      rmaInput$dist <- dist
      if (length(R) > 0)
        R <- paste0("list(", paste0(names(R), " = '", R, "'", collapse = ", "),")")
      rmaInput$R <- R
    }
  }

  # specify method and fixed effect terms test
  rmaInput$method <- paste0("'", .maGetMethodOptions(options), "'")
  rmaInput$test   <- paste0("'", options[["fixedEffectTest"]], "'")

  if (!options[["weightedEstimation"]])
    rmaInput$weighted <- FALSE

  # add fixed parameters if needed
  if (options[["fixParametersWeights"]] && options[["fixParametersWeightsVariable"]] != "")
    rmaInput$weights <- as.name(options[["fixParametersWeightsVariable"]])
  if (options[["fixParametersTau2"]])
    rmaInput$tau2 <- .maGetFixedTau2Options(options)

  # add link function if needed
  if (.maIsMetaregressionHeterogeneity(options))
    rmaInput$link <- paste0("'", options[["heterogeneityModelLink"]], "'")

  if (.maIsMultilevelMultivariate(options)) {
    rmaInput$sparse <- if (options[["useSparseMatricies"]])       options[["useSparseMatricies"]]
    rmaInput$cvvc   <- if (!options[["computeCovarianceMatrix"]]) !options[["computeCovarianceMatrix"]]
  }

  # add control options if needed
  control <- .maGetControlOptions(options)
  if (length(control) != 0)
    rmaInput$control <- control

  # additional input
  rmaInput$level <- 100 * options[["confidenceIntervalsLevel"]]

  # add additional options
  if (options[["advancedExtendMetaforCall"]])
    rmaInput <- c(rmaInput, .maExtendMetaforCallFromOptions(options))

  ### fit the model
  if (.maIsMultilevelMultivariate(options)) {
    fit <- paste0("fit <- rma.mv(\n\t", paste(names(rmaInput), "=", rmaInput, collapse = ",\n\t"), "\n)\n")
  } else {
    fit <- paste0("fit <- rma(\n\t", paste(names(rmaInput), "=", rmaInput, collapse = ",\n\t"), "\n)\n")
  }


  # add clustering if specified
  if (options[["clustering"]] != "") {

    robustInput <- list(
      cluster      = as.name(options[["clustering"]]),
      clubSandwich = options[["clusteringUseClubSandwich"]],
      adjust       = options[["clusteringSmallSampleCorrection"]]
    )

    fit <- paste0(
      fit, "\n",
      "fit <- robust(\n",
      "\tfit,\n\t",
      paste(names(robustInput), "=", robustInput, collapse = ",\n\t"), "\n)\n"
    )
  }

  # add permutation if specified
  if (.maIsPermutation(options)) {

    if (options[["setSeed"]])
      fit <- paste0(fit, "\nset.seed(", options[["seed"]], ")\n")

    fit <- paste0(
      fit, "\n",
      "fitPermutation <- permutest(\n",
      "\tfit,\n",
      "\texact = ", options[["permutationTestType"]] == "exact", ",\n",
      "\titer  = ", options[["permutationTestIteration"]], "\n",
      ")\n"
    )
  }

  return(fit)
}
.maComputeVifSummary               <- function(fit, options, parameter = "effectSize") {

  if (jaspBase::isTryError(fit)) {
    return(NULL)
  }

  if (options[["diagnosticsVarianceInflationFactorAggregate"]]) {

    # obtain terms indicies
    if (parameter == "effectSize") {
      terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")
      termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")
      tableVif   <- do.call(rbind, lapply(seq_along(terms), function(i) {
        cbind.data.frame(
          term = terms[i],
          .maExtractVifResults(try(metafor::vif(fit, btt = seq_along(termsIndex)[termsIndex == i])), options, parameter)
        )
      }))
    } else if (parameter == "heterogeneity") {
      terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")
      termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")
      tableVif   <- do.call(rbind, lapply(seq_along(terms), function(i) {
        cbind.data.frame(
          term = terms[i],
          .maExtractVifResults(try(metafor::vif(fit, att = seq_along(termsIndex)[termsIndex == i])), options, parameter)
        )
      }))
    }

  } else {

    tableVif      <- .maExtractVifResults(try(metafor::vif(fit)), options, parameter)
    tableVif$term <- .maVariableNames(rownames(tableVif), options[["predictors"]])
  }

  tableVif$subgroup <- attr(fit, "subgroup")

  return(tableVif)
}
.maBubblePlotMakeCiGeom            <- function(dfPlot, options, ci = TRUE) {

  hasSeparateLines     <- attr(dfPlot, "separateLines") != ""
  hasSeparatePlots     <- attr(dfPlot, "separatePlots") != ""
  selectedVariableType <- attr(dfPlot, "selectedVariableType")

  aesCall <- list(
    x      = as.name("selectedVariable"),
    fill   = if (hasSeparateLines) as.name("separateLines"),
    group  = if (hasSeparateLines && selectedVariableType == "scale") as.name("separateLines")
  )

  if (selectedVariableType == "scale") {
    aesCall$y      <- as.name("y")
  } else if (selectedVariableType == "nominal") {
    aesCall$lower   <- as.name("lower")
    aesCall$upper   <- as.name("upper")
    aesCall$ymin    <- as.name("lower")
    aesCall$ymax    <- as.name("upper")
    aesCall$middle  <- as.name("middle")
  }

  dfBands <-  .maBubblePlotMakeConfidenceBands(
    dfPlot,
    lCi = if (ci) "lCi" else "lPi",
    uCi = if (ci) "uCi" else "uPi"
  )

  if (selectedVariableType == "scale") {
    dfBands[["y"]] <- do.call(.maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]), list(dfBands[["y"]]))
  } else if (selectedVariableType == "nominal") {
    dfBands[,c("lower","middle","upper")]  <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(dfBands[,c("lower","middle","upper")]))
  }

  geomCall <- list(
    data    = dfBands,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
    alpha   = options[["bubblePlotPredictionIntervalsTransparency"]]
  )

  if (selectedVariableType == "nominal") {
    geomCall$stat     <- "identity"
    geomCall$position <- ggplot2::position_dodge2(width = 0.9)
    if (!hasSeparateLines)
      geomCall$fill <- "grey"
  }


  if (selectedVariableType == "scale" && any(!is.na(dfBands[["y"]]))) {
    geom <- list(
      what = ggplot2::geom_polygon,
      args = geomCall
    )
    attr(geom, "yRange") <- range(c(dfBands$y))
  } else if (selectedVariableType == "nominal" && any(!is.na(dfBands[["lower"]]))) {
    geom <- list(
      what = ggplot2::geom_boxplot,
      args = geomCall
    )
    attr(geom, "yRange") <- range(c(dfBands$lower, dfBands$upper))
  } else {
    geom <- NULL
  }

  return(geom)
}
.maMakeResidualFunnelPlot          <- function(fit, options, dataset) {

  residuals <- rstandard(fit)
  dfPlot    <- data.frame(
    x  = residuals[["resid"]],
    y  = residuals[["se"]]
  )

  yTicks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(dfPlot$y)))

  dfFunnel <- data.frame(
    x = c(-max(yTicks), 0, max(yTicks)) * 1.96,
    y = c(max(yTicks),  0, max(yTicks))
  )
  dfFunnelEdge1 <- dfFunnel[1:2,]
  dfFunnelEdge2 <- dfFunnel[2:3,]

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(dfPlot$x, dfFunnel$x)))

  dfBackground <- data.frame(
    x = c(min(xTicks), max(xTicks), max(xTicks), min(xTicks)),
    y = c(min(yTicks), min(yTicks), max(yTicks), max(yTicks))
  )

  out <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data    = dfBackground,
      mapping = ggplot2::aes(x = x, y = y),
      fill    = "grey",
    ) +
    ggplot2::geom_polygon(
      data    = dfFunnel,
      mapping = ggplot2::aes(x = x, y = y),
      fill    = "white",
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = c(0, 0),
        y = range(yTicks)
      ), linetype = "dotted"
    ) +
    ggplot2::geom_line(
      data    = dfFunnelEdge1,
      mapping = ggplot2::aes(x = x, y = y), linetype = "dotted"
    ) +
    ggplot2::geom_line(
      data    = dfFunnelEdge2,
      mapping = ggplot2::aes(x = x, y = y), linetype = "dotted"
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = c(0, 0),
        y = range(yTicks)
      ), linetype = "dotted"
    ) +
    jaspGraphs::geom_point(
      data    = dfPlot,
      mapping = ggplot2::aes(x = x, y = y),
      fill    = "black"
    )

  # add labels if specified
  if (options[["studyLabels"]] != "") {

    dfLabels <- cbind(
      dfPlot,
      label = dataset[[options[["studyLabels"]]]]
    )
    dfLabels <- dfLabels[abs(dfLabels$y * 1.96) < abs(dfLabels$x),]
    dfLabels$position <- ifelse(dfLabels$x < 0, "right", "left")
    dfLabels$nudge_x  <- ifelse(dfLabels$x < 0, -0.1, 0.1)

    out <- out +
      ggplot2::geom_text(
        data    = dfLabels,
        mapping = ggplot2::aes(x = x, y = y, label = label, hjust = position), nudge_x = dfLabels$nudge_x
      )
  }

  out <- out +
    jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks), name = gettext("Residual Value")) +
    ggplot2::scale_y_reverse(breaks = rev(yTicks), limits = rev(range(yTicks)), name = gettext("Standard Error")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(out)
}
.maMakeProfileLikelihoodPlot       <- function(dfPlot) {

  yTicks <- jaspGraphs::getPrettyAxisBreaks(c(min(dfPlot$ll), max(dfPlot$ll)))

  # xTicks and other attributes only passed for rma.uni
  # (there are way too many options to deal with for rma.mv --- using the metafor package defaults)
  if (!is.null(attr(dfPlot, "xTicks")))
    xTicks <- attr(dfPlot, "xTicks")
  else
    xTicks <- jaspGraphs::getPrettyAxisBreaks(c(min(dfPlot[[1]]), max(dfPlot[[1]])))

  # create plot
  plotOut <- ggplot2::ggplot(
    data    = data.frame(x = dfPlot[[1]], y = dfPlot[["ll"]]),
    mapping = ggplot2::aes(x = x, y = y)
  ) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point()

  plotOut <- plotOut +
    ggplot2::geom_line(
      data = data.frame(
        x = rep(dfPlot[["vc"]], 2),
        y = range(yTicks)),
      linetype = "dotted") +
    ggplot2::geom_line(
      data = data.frame(
        x = range(xTicks),
        y = rep(max(dfPlot[["maxll"]]), 2)),
      linetype = "dotted")

  plotOut <- plotOut +
    ggplot2::labs(x = dfPlot[["xlab"]], y = gettext("Profile Likelihood")) +
    jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks)) +
    jaspGraphs::scale_y_continuous(breaks = yTicks, limits = range(yTicks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(plotOut)
}

# check functions
.maIsMetaregression               <- function(options) {
  return(.maIsMetaregressionEffectSize(options) || .maIsMetaregressionHeterogeneity(options))
}
.maIsMetaregressionEffectSize     <- function(options) {
  return(length(options[["effectSizeModelTerms"]]) > 0)
}
.maIsMetaregressionHeterogeneity  <- function(options) {
  return(length(options[["heterogeneityModelTerms"]]) > 0)
}
.maIsClustered                    <- function(options) {
  return(options[["clustering"]] != "")
}
.maIsMetaregressionFtest          <- function(options) {
  return(options[["fixedEffectTest"]] %in% c("knha", "t"))
}
.maIsMultilevelMultivariate       <- function(options) {
  return(options[["module"]] == "metaAnalysisMultilevelMultivariate")
}
.maIsPermutation                  <- function(options) {
  return(!.maIsClustered(options) && options[["permutationTest"]])
}
.maCheckIsPossibleOptions         <- function(options) {

  if (length(options[["heterogeneityModelTerms"]]) > 0 && options[["clustering"]] != "") {
    return(gettext("Clustering is not supported when specifying a heterogeneity meta-regression model."))
  }

  return(NULL)
}

# extract options
.maGetMethodOptions                             <- function(options) {

  switch(
    options[["method"]],
    "equalEffects"       = "EE",
    "fixedEffects"       = "FE",
    "maximumLikelihood"  = "ML",
    "restrictedML"       = "REML",
    "derSimonianLaird"   = "DL",
    "hedges"             = "HE",
    "hunterSchmidt"      = "HS",
    "hunterSchmidtSsc"   = "HSk",
    "sidikJonkman"       = "SJ",
    "empiricalBayes"     = "EB",
    "pauleMandel"        = "PM",
    "pauleMandelMu"      = "PMM",
    "qeneralizedQStat"   = "GENQ",
    "qeneralizedQStatMu" = "GENQM",
    NA
  )
}
.maGetFixedTau2Options                          <- function(options) {

  tau2 <- .parseRCodeInOptions(options[["fixParametersTau2Value"]])

  if (!is.numeric(tau2) || length(tau2) != 1 || tau2 < 0)
    .quitAnalysis(gettext("The fixed value for tau2 must be a positive number."))
  else
    return(tau2)
}
.maGetControlOptions                            <- function(options) {

  if (.maIsMetaregressionHeterogeneity(options)) {
    if (options[["optimizerMethod"]] == "nlminb" && !options[["optimizerMaximumIterations"]] && !options[["optimizerConvergenceRelativeTolerance"]]) {
      # allow an empty list for default settings --- this allows manual modification of the control argument through extra input
      out <- list()
    } else {
      out <- list(
        optimizer = options[["optimizerMethod"]],
        iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
        rel.tol   = if (options[["optimizerConvergenceRelativeTolerance"]]) options[["optimizerConvergenceRelativeToleranceValue"]]
      )
    }
  } else {
    if (.maIsMultilevelMultivariate(options)) {
      if (options[["optimizerMethod"]] == "nlminb" && !options[["optimizerMaximumEvaluations"]] && !options[["optimizerMaximumIterations"]] && !options[["optimizerConvergenceRelativeTolerance"]]) {
        # allow an empty list for default settings --- this allows manual modification of the control argument through extra input
        out <- list()
      } else if (options[["optimizerMethod"]] == "nlminb") {
        out <- list(
          optimizer = options[["optimizerMethod"]],
          eval.max  = if (options[["optimizerMaximumEvaluations"]]) options[["optimizerMaximumEvaluationsValue"]],
          iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
          rel.tol   = if (options[["optimizerConvergenceRelativeTolerance"]]) options[["optimizerConvergenceRelativeToleranceValue"]]
        )
      } else if (options[["optimizerMethod"]] %in% c("Nelder-Mead", "BFGS")){
        out <- list(
          optimizer = options[["optimizerMethod"]],
          maxit     = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
          reltol    = if (options[["optimizerConvergenceRelativeTolerance"]]) options[["optimizerConvergenceRelativeToleranceValue"]]
        )
      } else if (options[["optimizerMethod"]] %in% c("uobyqa", "newuoa", "bobyqa")){
        out <- list(
          optimizer = options[["optimizerMethod"]],
          maxfun   = if (options[["optimizerMaximumEvaluations"]]) options[["optimizerMaximumEvaluationsValue"]],
          rhobeg   = if (options[["optimizerInitialTrustRegionRadius"]]) options[["optimizerInitialTrustRegionRadiusValue"]],
          rhoend   = if (options[["optimizerFinalTrustRegionRadius"]]) options[["optimizerFinalTrustRegionRadiusValue"]]
        )
      } else if (options[["optimizerMethod"]] %in% c("nloptr", "nlm")){
        # could be much more, "nloptr" probably requires choosing a method too
        out <- list(
          optimizer = options[["optimizerMethod"]],
          iterlim   = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]]
        )
      } else if (options[["optimizerMethod"]] %in% c("hjk", "nmk", "mads")){
        out <- list(
          optimizer    = options[["optimizerMethod"]],
          tol          = if (options[["optimizerConvergenceTolerance"]]) options[["optimizerConvergenceToleranceValue"]],
          maxfeval     = if (options[["optimizerMaximumEvaluations"]]) options[["optimizerMaximumEvaluationsValue"]],
          restarts.max = if (options[["optimizerMethod"]] == "mmk" && options[["optimizerMaximumRestarts"]]) options[["optimizerMaximumRestartsValue"]]
        )
      }
    } else {
      if (.maGetMethodOptions(options) %in% c("REML", "ML", "EB")) {
        out <- list(
          tau2.init = if (options[["optimizerInitialTau2"]]) options[["optimizerInitialTau2Value"]],
          iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
          threshold = if (options[["optimizerConvergenceTolerance"]]) options[["optimizerConvergenceToleranceValue"]],
          stepadj   = if (options[["optimizerStepAdjustment"]]) options[["optimizerStepAdjustmentValue"]]
        )
      } else if (.maGetMethodOptions(options) %in% c("PM", "PMM", "GENQM")) {
        out <- list(
          iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
          tol       = if (options[["optimizerConvergenceTolerance"]]) options[["optimizerConvergenceToleranceValue"]],
          tau2.min  = if (options[["optimizerMinimumTau2"]]) options[["optimizerMinimumTau2Value"]],
          tau2.max  = if (options[["optimizerMaximumTau2"]]) options[["optimizerMaximumTau2Value"]]
        )
      } else if (.maGetMethodOptions(options) %in% c("SD")) {
        out <- list(
          tau2.init = if (options[["optimizerInitialTau2"]]) options[["optimizerInitialTau2Value"]]
        )
      } else {
        out <- list()
      }
    }
  }
  return(out[!sapply(out, is.null)])
}
.maGetEffectSizeTransformationOptions           <- function(effectSizeTransformation) {

  switch(
    effectSizeTransformation,
    none                          = function(x) x,
    fishersZToCorrelation         = metafor::transf.ztor,
    exponential                   = exp,
    logOddsToProportions          = Vectorize(metafor::transf.ilogit),
    logOddsToSmdNormal            = metafor::transf.lnortod.norm,
    logOddsToSmdLogistic          = metafor::transf.lnortod.logis,
    smdToLogOddsNormal            = metafor::transf.dtolnor.norm,
    smdToLogOddsLogistic          = metafor::transf.dtolnor.logis,
    hakstianAndWhalenInverseAlpha = Vectorize(metafor::transf.iahw),
    bonettInverseAlpha            = Vectorize(metafor::transf.iabt),
    zToR2                         = metafor::transf.ztor2,
    smdToCohensU1                 = Vectorize(metafor::transf.dtou1),
    smdToCohensU2                 = Vectorize(metafor::transf.dtou2),
    smdToCohensU3                 = Vectorize(metafor::transf.dtou3),
    smdToCles                     = Vectorize(metafor::transf.dtocles),
    stop(paste0("Unknown effect size transformation: ", effectSizeTransformation))
  )
}
.maExtendMetaforCallFromOptions                 <- function(options) {

  optionsCode <- options[["advancedExtendMetaforCallCode"]]
  optionsCode <- trimws(optionsCode, which = "both")
  if (substr(optionsCode, 1, 4) != "list")
    optionsCode <- paste0("list(\n", optionsCode, "\n)")
  optionsCode <- try(eval(parse(text = optionsCode)))

  if (jaspBase::isTryError(optionsCode))
    .quitAnalysis(gettextf("The custom R code for extending the metafor call failed with the following message: %1$s", optionsCode))

  return(optionsCode)
}
.maGetPValueAdjustment                          <- function(pValueAdjustment) {
  return(switch(
    pValueAdjustment,
    "none"               = "none",
    "bonferroni"         = "bonferroni",
    "holm"               = "holm",
    "hochberg"           = "hochberg",
    "hommel"             = "hommel",
    "benjaminiHochberg"  = "BH",
    "benjaminiYekutieli" = "BY"
  ))
}
.maGetEstimatedMarginalMeansAndContrastsOptions <- function(options){

  return(options[c(
    "contrastsHeterogeneityPValueAdjustment",
    "estimatedMarginalMeansHeterogeneityTransformation",
    "estimatedMarginalMeansHeterogeneitySdFactorCovariates",
    "estimatedMarginalMeansHeterogeneityAddAdjustedEstimate",

    "contrastsEffectSizePValueAdjustment",
    "estimatedMarginalMeansEffectSizeTestAgainst",
    "estimatedMarginalMeansEffectSizeTestAgainstValue",
    "estimatedMarginalMeansEffectSizeSdFactorCovariates",
    "estimatedMarginalMeansEffectSizeAddAdjustedEstimate",

    "confidenceIntervals",
    "confidenceIntervalsLevel",
    "predictionIntervals",
    "transformEffectSize"
  )])
}

# options names
.maGetOptionsNameEffectSizeTransformation <- function(effectSizeTransformation) {

  return(switch(
    effectSizeTransformation,
    "none"                           = NULL,
    "fishersZToCorrelation"          = gettext("Fisher's z to r"),
    "exponential"                    = gettext("exponential"),
    "logOddsToProportions"           = gettext("log odds to proportions"),
    "logOddsToSmdNormal"             = gettext("log odds to SMD (normal)"),
    "logOddsToSmdLogistic"           = gettext("log odds to SMD (logistic)"),
    "smdToLogOddsNormal"             = gettext("SMD to log odds (normal)"),
    "smdToLogOddsLogistic"           = gettext("SMD to log odds (logistic)"),
    "hakstianAndWhalenInverseAlpha"  = gettext("Hakstian & Whalen inverse α"),
    "bonettInverseAlpha"             = gettext("Bonett inverse α"),
    "zToR2"                          = gettext("z to R²"),
    "smdToCohensU1"                  = gettext("SMD to Cohen's U₁"),
    "smdToCohensU2"                  = gettext("SMD to Cohen's U₂"),
    "smdToCohensU3"                  = gettext("SMD to Cohen's U₃"),
    "smdToCles"                      = gettext("SMD to CLES, Pr(supperiority)")
  ))
}
.maGetOptionsNamePValueAdjustment         <- function(pValueAdjustment) {

  return(switch(
    pValueAdjustment,
    "none"               = gettext("None"),
    "bonferroni"         = gettext("Bonferroni"),
    "holm"               = gettext("Holm"),
    "hochberg"           = gettext("Hochberg"),
    "hommel"             = gettext("Hommel"),
    "benjaminiHochberg"  = gettext("Benjamini-Hochberg"),
    "benjaminiYekutieli" = gettext("Benjamini-Yekutieli")
  ))
}
.maCasewiseDiagnosticsNames               <- function() {
  return(c(
    "rstudent",
    "dffits",
    "cook.d",
    "cov.r",
    "tau.del",
    "tau2.del",
    "QE.del",
    "hat",
    "weight",
    "inf"
  ))
}
.maCasewiseDiagnosticsExportColumnsNames  <- function(columnName) {

  return(switch(
    columnName,
    "rstudent"  = "Standardized Residual",
    "dffits"    = "DFFITS",
    "cook.d"    = "Cook's Distance",
    "cov.r"     = "Covariance Ratio",
    "tau.del"   = "Tau",
    "tau2.del"  = "Tau2 LOO",
    "QE.del"    = "QE LOO",
    "hat"       = "Hat",
    "weight"    = "Weight",
    "inf"       = "Influential"
  ))
}

# print
.maVariableNames                      <- function(varNames, variables) {

  return(sapply(varNames, function(varName){

    if (varName == "intrcpt")
      return("Intercept")

    for (vn in variables) {
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
.maPrintQTest                         <- function(fit) {

  if (fit[["p"]] > 1) {
    heterogeneityName <- gettextf("Residual heterogeneity")
  } else {
    heterogeneityName <- gettextf("Heterogeneity")
  }

  return(sprintf("%1$s: Q(%2$i) = %3$.2f, %4$s", heterogeneityName, fit[["k"]] - fit[["p"]], fit[["QE"]], .maPrintPValue(fit[["QEp"]])))
}
.maPrintModerationTest                <- function(fit, options, parameter) {

  out      <- .maOmnibusTest(fit, options, parameter)
  outPrint <- .maPrintTermTest(out, testStatistic = TRUE)

  if (.maIsMetaregressionHeterogeneity(options)) {
    if (parameter == "effectSize")
      return(gettextf("Moderation (Effect Size): %1$s", outPrint))
    else if (parameter == "heterogeneity")
      return(gettextf("Moderation (Heterogeneity): %1$s", outPrint))
  } else {
    if (parameter == "effectSize")
      return(gettextf("Moderation: %1$s", outPrint))
  }
}
.maPrintHeterogeneityEstimate         <- function(fit, options, digits, parameter) {

  out <- .maComputePooledHeterogeneityPlot(fit, options, parameter)

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
    return(sprintf("%1$i", df))
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

# table row functions
.maRowHeterogeneityTest               <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(data.frame(
      subgroup = attr(fit, "subgroup"),
      test     = if (.maIsMetaregression(options)) gettext("Residual heterogeneity") else gettext("Heterogeneity")
    ))
  }

  row <- data.frame(
    subgroup = attr(fit, "subgroup"),
    test     = if (.maIsMetaregression(options)) gettext("Residual heterogeneity") else gettext("Heterogeneity"),
    stat     = sprintf("Q\U2091(%1$i) = %2$.2f", fit[["k"]] - fit[["p"]], fit[["QE"]]),
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
  predictedEffect <- .maComputePooledEffectPlot(fit, options)

  row <- data.frame(
    subgroup = attr(fit, "subgroup"),
    test     = gettext("Pooled effect"),
    stat     = if (.maIsMetaregressionFtest(options)) sprintf("t(%1$s) = %2$.2f", .maPrintDf(predictedEffect[["df"]][1]), predictedEffect[["stat"]][1])
    else sprintf("z = %1$.2f",  predictedEffect[["stat"]][1]),
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
      row[["stat"]] <- sprintf("F\U2098(%1$s, %2$s) = %3$.2f", .maPrintDf(moderationOut[["df1"]]), .maPrintDf(moderationOut[["df2"]]), moderationOut[["stat"]])
    } else {
      row[["stat"]] <- sprintf("Q\U2098(%1$s) = %2$.2f", .maPrintDf(moderationOut[["df1"]]),  moderationOut[["stat"]])
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
  if (coefficientsTest) {
    attr(row, "footnote") <- attr(moderationOut, "footnote")
  }

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
  row          <- .maComputePooledEffect(fit, options)
  row$subgroup <- attr(fit, "subgroup")

  row <- do.call(cbind.data.frame, row)
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

  # pooled effect size
  row          <- .maComputePooledHeterogeneity(fit, options)
  row$subgroup <- attr(fit, "subgroup")

  row <- row[,c(
    "par", "est",
    if (options[["confidenceIntervals"]]) c("lCi", "uCi"),
    "subgroup"
  )]

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
  row <- cbind.data.frame(
    "subgroup"     = attr(fit, "subgroup"),
    "model"        = colnames(fit[["fit.stats"]]),
    "observations" = fit[["k"]],
    data.frame(t(fit[["fit.stats"]]))
  )

  if (.maIsMetaregressionEffectSize(options) && !.maIsMultilevelMultivariate(options))
    row$R2 <- fit[["R2"]]

  return(row)
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
      name = .maVariableNames(rownames(fit[["beta"]]), options[["predictors"]]),
      est  = fit[["beta"]][,1],
      se   = fit[["se"]],
      stat = fit[["zval"]],
      pval = fit[["pval"]]
    )

    if (.maIsPermutation(options))
      estimates$pval2 <- attr(fit[["pval"]], "permutation")

    if (.maIsMetaregressionFtest(options))
      estimates$df <- fit[["ddf"]]

    if (options[["confidenceIntervals"]]) {
      estimates$lCi <- fit[["ci.lb"]]
      estimates$uCi <- fit[["ci.ub"]]
    }

  } else if (parameter == "heterogeneity") {

    estimates <- data.frame(
      name = .maVariableNames(rownames(fit[["alpha"]]), options[["predictors"]]),
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
.maRowDiagnosticsTable                <- function(fit, diagnostics, options, forExport = FALSE) {

  # first create the data part of the output
  # (in case the fit failed there are no diagnostics)

  fitData <- attr(fit, "data")
  rows    <- list()

  # add export specific settings (for adding to the dataset)
  if (forExport) {
    rows[["datasetOrder"]] <- as.numeric(names(attr(fitData, "NasIds"))[!attr(fitData, "NasIds")])

    # no variable information necessary
    options[["studyLabels"]] <- ""
    options[["diagnosticsCasewiseDiagnosticsIncludePredictors"]] <- FALSE
  }

  # include study labels
  if (options[["studyLabels"]] != "") {
    rows[["label"]] <- fitData[[options[["studyLabels"]]]]
  }

  # include predictors
  if (options[["diagnosticsCasewiseDiagnosticsIncludePredictors"]]) {
    fitData           <- fitData[, colnames(fitData) %in% options[["predictors"]], drop = FALSE]
    rows[["pred"]]    <- fitData
  }

  # return on error
  if (jaspBase::isTryError(fit)) {
    return(do.call(cbind.data.frame, rows))
  }

  # main diagnostics section
  rows <- do.call(cbind.data.frame, c(
    rows,
    diagnostics[["influenceResultsInf"]],
    if (options[["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"]]) diagnostics[["influenceResultsDfbs"]]
  ))
  rows$subgroup <- attr(fit, "subgroup")

  return(rows)
}

# table row helper functions
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

  # drop the grouping column if no subgroups requested
  if (options[["subgroup"]] == "") {
    df <- df[,colnames(df) != "subgroup", drop = FALSE]
    return(df)
  }

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

# table functions
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
.maAddSubgroupColumn            <- function(tempTable, options) {

  if (options[["subgroup"]] != "")
    tempTable$addColumnInfo(name = "subgroup", type = "string", title = gettext("Subgroup"))

  return(tempTable)
}

# misc
.maAddSpaceForPositiveValue           <- function(value) {
  if (value >= 0)
    return(" ")
  else
    return("")
}
.maMakeDiamondDataFrame               <- function(est, lCi, uCi, row, id, adj = 1/3) {
  return(data.frame(
    id       = id,
    x        = c(lCi,  est,     uCi,  est),
    y        = c(row,  row-adj, row,  row+adj),
    type     = "diamond",
    mapColor = NA
  ))
}
.maMakeRectangleDataFrame             <- function(lCi, uCi, row, id, adj = 1/5) {
  return(data.frame(
    id       = id,
    x        = c(lCi,     uCi,      uCi,      lCi),
    y        = c(row-adj, row-adj,  row+adj,  row+adj),
    type     = "rectangle",
    mapColor = NA
  ))
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
.maBubblePlotMakeConfidenceBands      <- function(dfPlot, lCi = "lCi", uCi = "uCi") {

  if (attr(dfPlot, "selectedVariableType") == "scale") {

    if (!is.null(dfPlot[["separateLines"]])) {

      dfBands <- do.call(rbind, lapply(unique(dfPlot[["separateLines"]]), function(lvl) {
        dfSubset  <- dfPlot[dfPlot[["separateLines"]] == lvl,]
        dfPolygon <- data.frame(
          selectedVariable  = c(dfSubset$selectedVariable, rev(dfSubset$selectedVariable)),
          y                 = c(dfSubset[[lCi]],           rev(dfSubset[[uCi]]))
        )
        dfPolygon$separateLines <- lvl
        return(dfPolygon)
      }))

    } else {

      dfBands <- data.frame(
        selectedVariable = c(dfPlot$selectedVariable, rev(dfPlot$selectedVariable)),
        y                = c(dfPlot[[lCi]],           rev(dfPlot[[uCi]]))
      )

    }

  } else {

    dfBands <- data.frame(
      lower            = dfPlot[[lCi]],
      upper            = dfPlot[[uCi]],
      middle           = dfPlot[["est"]],
      selectedVariable = dfPlot[["selectedVariable"]]
    )

    if (!is.null(dfPlot[["separateLines"]]))
      dfBands$separateLines <- dfPlot[["separateLines"]]

  }

  return(dfBands)
}
.maMergeVariablesLevels               <- function(df, variables, mergedName) {
  if (length(variables) == 1) {
    df[[mergedName]] <- factor(
      df[,variables],
      levels = unique(df[,variables])
    )
  } else if (length(variables) > 1) {
    df[[mergedName]] <- factor(
      apply(df[,variables], 1, function(x) paste(x, collapse = " | ")),
      levels = unique(apply(df[,variables], 1, function(x) paste(x, collapse = " | ")))
    )
  }
  return(df)
}
.maTransformToHtml                    <- function(rCode) {

  # Replace special characters with HTML entities
  htmlCode <- gsub("&", "&amp;", rCode)
  htmlCode <- gsub("<", "&lt;", htmlCode)
  htmlCode <- gsub(">", "&gt;", htmlCode)

  # Wrap the code in <pre> and <code> tags
  htmlCode <- paste0(
    "<pre><code>", htmlCode, "\n</code></pre>"
  )

  return(htmlCode)
}
.maExtractVifResults                  <- function(vifResults, options, parameter) {

  if (jaspBase::isTryError(vifResults)) {
    if (options[["diagnosticsVarianceInflationFactorAggregate"]]) {
      return(data.frame(
        m   = NA,
        vif = NA,
        sif = NA
      ))
    } else {
      return(data.frame(
        vif = NA,
        sif = NA
      ))
    }
  }

  if (.maIsMetaregressionHeterogeneity(options))
    vifResults <- vifResults[[switch(
      parameter,
      "effectSize"    = "beta",
      "heterogeneity" = "alpha"
    )]]

  vifResults <- data.frame(vifResults)

  if (options[["diagnosticsVarianceInflationFactorAggregate"]])
    vifResults <- vifResults[,c("m", "vif", "sif"),drop = FALSE]
  else
    vifResults <- vifResults[,c("vif", "sif"),drop = FALSE]

  return(vifResults)
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
.maSuppressPlot                       <- function(plotExpression) {
  temp <- tempfile()
  pdf(file = temp)
  dfOut <- plotExpression
  dev.off()
  unlink(temp)
  return(dfOut)
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
.maCleanSelectedIndexesVector         <- function(x) {

  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")

  x <- strsplit(x, ",", fixed = TRUE)[[1]]

  x <- trimws(x, which = "both")
  x <- x[x != ""]

  x <- as.numeric(x)

  return(x)
}
.maDichotomizeVariablesLevels         <- function(df, variables, options) {

  variablesContinuous <- variables[variables %in% options[["predictors"]][options[["predictors.types"]] == "scale"]]
  for (i in seq_along(variablesContinuous)){
    tempUnique <- sort(unique(df[[variablesContinuous[i]]]))
    df[[variablesContinuous[i]]] <- as.character(factor(
      df[[variablesContinuous[i]]],
      levels = tempUnique,
      labels = c(paste0("Mean - ", options[["bubblePlotSdFactorCovariates"]], "SD"), "Mean", paste0("Mean + ", options[["bubblePlotSdFactorCovariates"]], "SD"))
    ))
    attr(df, "continuousLevels") <- list(
      attr(df, "continuousLevels"),
      list(
        variable = variablesContinuous[i],
        levels   = tempUnique
      )
    )
  }
  return(df)
}
.maDichotomizeVariablesDataset        <- function(df, variables, variablesInformation, options) {

  variablesContinuous  <- variables[variables %in% options[["predictors"]][options[["predictors.types"]] == "scale"]]

  for (i in seq_along(variablesContinuous)){

    tempUnique <- variablesInformation[[sapply(variablesInformation, function(x) x[["variable"]]) == variablesContinuous[i]]]

    df[[variablesContinuous[i]]] <- cut(
      df[[variablesContinuous[i]]],
      breaks = c(-Inf, mean(tempUnique[["levels"]][1:2]), mean(tempUnique[["levels"]][2:3]), Inf),
      labels = c(paste0("Mean - ", options[["bubblePlotSdFactorCovariates"]], "SD"), "Mean", paste0("Mean + ", options[["bubblePlotSdFactorCovariates"]], "SD"))
    )

  }

  return(df)
}
.maGetTermsIndices                    <- function(fit, parameter) {

  if (parameter == "effectSize") {

    terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")     # get terms indices from the model
    termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")  # get coefficient indices from the model matrix
    termsIndex <- termsIndex[!fit$coef.na]                                                   # remove dropped coefficients

    termsIndicies <- lapply(terms, function(term){
      seq_along(termsIndex)[termsIndex == which(terms == term)]
    })
    names(termsIndicies) <- terms

  } else if (parameter == "heterogeneity") {

    terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")      # get terms indices from the model
    termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")   # get coefficient indices from the model matrix
    termsIndex <- termsIndex[!fit$coef.na.Z]                                                   # remove dropped coefficients

    termsIndicies <- lapply(terms, function(term){
      seq_along(termsIndex)[termsIndex == which(terms == term)]
    })
    names(termsIndicies) <- terms

  }

  return(termsIndicies)
}

# messages
.maFixedEffectTextMessage              <- function(options) {
  return(switch(
    options[["fixedEffectTest"]],
    "z"    = gettext("Fixed effects tested using z-distribution."),
    "t"    = gettext("Fixed effects tested using t-distribution."),
    "knha" = gettext("Fixed effects tested using Knapp and Hartung adjustment."),
    stop(paste0("Unknown fixed effect test.", options[["fixedEffectTest"]]))
  ))
}
.meMetaregressionHeterogeneityMessages <- function(options) {

  if (options[["heterogeneityModelLink"]] == "log")
    return(gettext("The heterogeneity model for \U1D70F\U00B2 is specified on the log scale."))
  else if (options[["heterogeneityModelLink"]] == "identity")
    return(gettext("The heterogeneity model for \U1D70F\U00B2 is specified on the identity scale."))
}
.maPooledEstimatesMessages             <- function(fit, dataset, options, anyNA = FALSE) {

  messages <- NULL

  if (options[["clustering"]] == "") {
    fit <- fit[[1]]
    if (!jaspBase::isTryError(fit) && !is.null(fit)){
      if (all(fit[["tcl"]][1] == fit[["tcl"]]))
        messages <- c(messages, gettextf("%1$i clusters with %2$i estimates each.", fit[["n"]],  fit[["tcl"]][1]))
      else
        messages <- c(messages, gettextf("%1$i clusters with min/median/max %2$i/%3$i/%4$i estimates.", fit[["n"]],  min(fit[["tcl"]]), median(fit[["tcl"]]), max(fit[["tcl"]])))
    }
  } else {
    for (i in seq_along(fit)) {
      tempFit <- fit[[i]]
      if (!jaspBase::isTryError(tempFit) && !is.null(tempFit)) {
        if (options[["clustering"]] != "") {
          if (all(tempFit[["tcl"]][1] == tempFit[["tcl"]]))
            messages <- c(messages, gettextf("%1$s: %2$i clusters with %3$i estimates each.", gettextf("Subgroup %1$s", attr(tempFit, "subgroup")), tempFit[["n"]],  tempFit[["tcl"]][1]))
          else
            messages <- c(messages, gettextf("%1$s: %2$i clusters with min/median/max %3$i/%4$i/%5$i estimates.", gettextf("Subgroup %1$s", attr(tempFit, "subgroup")), tempFit[["n"]],  min(tempFit[["tcl"]]), median(tempFit[["tcl"]]), max(tempFit[["tcl"]])))
        }
      }
    }
  }

  if (options[["transformEffectSize"]] != "none") {
    if (anyNA) {
      messages <- c(messages, gettextf("NAs in the pooled effect size were introduced due to the %1$s transformation. Please verify that you are using the correct effect size transformation.", .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))
    } else {
      messages <- c(messages, gettextf("The pooled effect size is transformed using %1$s transformation.", .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))
    }
  }


  if (.maIsMetaregressionEffectSize(options))
    messages <- c(messages, gettext("The pooled effect size corresponds to the weighted average effect across studies."))

  if (.maIsMetaregressionHeterogeneity(options))
    messages <- c(messages, gettext("The pooled heterogeneity estimate corresponds to the heterogeneity at the average of predictor values."))

  if (.maIsMetaregressionHeterogeneity(options) && (options[["heterogeneityI2"]] || options[["heterogeneityH2"]]))
    messages <- c(messages, gettext("The I² and H² statistics are not available for heterogeneity models."))

  if (attr(dataset, "NAs") > 0)
    messages <- c(messages, gettextf("%1$i observations were ommited due to missing values.", attr(dataset, "NAs")))

  if (!is.null(attr(dataset, "influentialObservations")) && attr(dataset, "influentialObservations") > 0)
    messages <- c(messages, gettextf("%1$i influential observations were detected and removed.", attr(dataset, "influentialObservations")))

  if (.maIsMultilevelMultivariate(options) && any(attr(fit, "skipped")) && !jaspBase::isTryError(fit[[1]]))
    messages <- c(messages, gettextf("The Model Structure %1$s was not completely specified and was skipped.", paste0(which(attr(fit[[1]], "skipped")), collapse = " and ")))

  if (.mammAnyStructureGen(options) && options[["predictionIntervals"]])
    messages <- c(messages, gettext("Prediction interval for the pooled effect size is not available for models with multiple heterogeneity estimates."))

  return(messages)
}
.maEstimatedMarginalMeansMessages      <- function(options, parameter, anyNA = FALSE) {

  messages <- gettext("Each marginal mean estimate is averaged across the levels of the remaining predictors.")

  if (parameter == "effectSize" && options[["transformEffectSize"]] != "none") {
    if (anyNA) {
      messages <- c(messages, gettextf("NAs in the marginal mean estimatess were introduced due to the %1$s transformation. Please verify that you are using the correct effect size transformation.", .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))
    } else {
      messages <- c(messages, gettextf("The estimates and intervals are transformed using %1$s transformation.", .maGetOptionsNameEffectSizeTransformation(options[["transformEffectSize"]])))
    }
  }


  if (parameter == "heterogeneity")
    messages <- c(messages, gettextf("The estimates and intervals correspond to %1$s.", switch(
      options[["estimatedMarginalMeansHeterogeneityTransformation"]],
      "tau"  = gettext("\U1D70F"),
      "tau2" = gettext("\U1D70F\U00B2")
    )))

  return(messages)
}
.macontrastsMessages                   <- function(options, parameter) {

  messages <- gettext("Each contrast is averaged across the levels of the remaining predictors.")

  if (parameter == "effectSize" && options[["transformEffectSize"]] != "none") {
    messages <- c(messages, gettextf("Contrasts of estimates marginal means cannot be transformed via the effect size transformation."))

    if (options[["contrastsEffectSizePValueAdjustment"]] != "none") {
      messages <- c(messages, gettextf("Contrasts of estimated marginal means are adjusted for multiple comparisons using %1$s correction.", .maGetOptionsNamePValueAdjustment(options[["contrastsEffectSizePValueAdjustment"]])))
    }
  }

  if (parameter == "heterogeneity") {

    if (options[["heterogeneityModelLink"]] == "log") {
      messages <- c(messages, gettext("Contrasts of estimated marginal means cannot be transformed via the link function. As such, the contrasts are summarized on the model scale: log(\U1D70F\U00B2)."))
    } else if (options[["estimatedMarginalMeansHeterogeneityTransformation"]] == "tau"){
      messages <- c(messages, gettext("Contrasts of estimated marginal means cannot be transformed via the heterogeneity transformation. As such, the contrasts are summarized as \U1D70F\U00B2."))
    }

    if (options[["contrastsEffectSizePValueAdjustment"]] != "none") {
      messages <- c(messages, gettextf("Contrasts of estimated marginal means are adjusted for multiple comparisons using %1$s correction.", .maGetOptionsNamePValueAdjustment(options[["contrastsHeterogeneityPValueAdjustment"]])))
    }
  }

  return(messages)
}
.maPermutationMessage                  <- function(options) {
  return(gettextf("Permutation p-value is based on %1$s permutations.", switch(
    options[["permutationTestType"]],
    "exact"       = gettext("exact"),
    "approximate" = options[["permutationTestIteration"]]
  )))
}
.maTryCleanErrorMessages               <- function(message) {
  # probably more messages will be gathered over time
  if (grepl("singular matrix", message))
    return(gettextf("The model estimation failed with the following message: %1$s. Please, consider simplifying the model.", message))

  return(message)
}
.maTermsTableWarnings                  <- function(fit, options, terms, parameter) {

  if (options[["subgroup"]] == "") {
    messages <- .maTermsTableWarningsFun(fit[[1]], terms, parameter, prefix = "")
  } else {
    messages <- NULL
    for (i in seq_along(fit)) {
      if (jaspBase::isTryError(fit))
        next
      messages <- c(messages, .maTermsTableWarningsFun(fit[[i]], terms, parameter, prefix = gettextf("Subgroup %1$s: ", attr(fit[[i]], "subgroup"))))
    }
  }

  return(messages)
}
.maTermsTableWarningsFun               <- function(fit, terms, parameter, prefix = "") {

  coefNA <- switch(
    parameter,
    "effectSize"    = fit$coef.na,
    "heterogeneity" = fit$coef.na.Z
  )

  if (any(coefNA)) {

    messages <- unlist(sapply(terms, function(term) {

      if (parameter == "effectSize") {
        terms      <- attr(terms(fit[["formula.mods"]], data = fit[["data"]]),"term.labels")     # get terms indices from the model
        termsIndex <- attr(model.matrix(fit[["formula.mods"]], data = fit[["data"]]), "assign")  # get coefficient indices from the model matrix
      } else if (parameter == "heterogeneity") {
        terms      <- attr(terms(fit[["formula.scale"]], data = fit[["data"]]),"term.labels")      # get terms indices from the model
        termsIndex <- attr(model.matrix(fit[["formula.scale"]], data = fit[["data"]]), "assign")   # get coefficient indices from the model matrix
      }

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
