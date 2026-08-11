# Classical meta-analysis model fitting and cached state.
#
# Fits, updates, and extracts classical, multilevel, GLMM, and RoBMA-compatible model state.

.maGetFormula                    <- function(modelTerms, includeIntercept) {

  predictors <- unlist(lapply(modelTerms, function(x) {
    if (length(x[["components"]]) > 1) {
      return(paste(x[["components"]], collapse = ":"))
    } else {
      return(x[["components"]])
    }
  }))

  if (length(predictors) == 0)
    return(NULL)

  if (includeIntercept) {
    formula <- paste("~", paste(predictors, collapse = "+"))
  } else {
    formula <- paste("~", paste(predictors, collapse = "+"), "-1")
  }

  return(as.formula(formula, env = parent.frame(1)))
}

.maFitModel                      <- function(jaspResults, dataset, options, objectName = "fit") {

  if (!.maReady(options) || !is.null(jaspResults[[objectName]]))
    return()

  # dispatch fitting function & dependencies
  if (.maIsGLMM(options)) {
    fittingFunction <- .maglmmFitModelFun
    dependencies    <- .maDependencies
  } else if (.maIsClassical(options, notMHP = TRUE)) {
    fittingFunction <- .maFitModelFun
    dependencies    <- .maDependencies
  } else if (.maIsClassical(options)) {
    fittingFunction <- .mamhpFitModelFun
    dependencies    <- .maDependencies
  } else {
    fittingFunction <- .robmaFitModelFun
    dependencies    <- .robmaDependencies
  }

  # create the output container
  fitContainer <- createJaspState()
  fitContainer$dependOn(dependencies)
  jaspResults[[objectName]] <- fitContainer

  fitOutput <- list()

  # full dataset fit
  if (.maIsClassical(options)) startProgressbar(expectedTicks = 1, label = gettext("Estimating Meta-Analytic Model"))
  fitOutput[["__fullDataset"]] <- do.call(fittingFunction, list(dataset = dataset, options = options, subgroupName = gettext("Full dataset")))
  if (.maIsClassical(options)) progressbarTick()

  # add subgroup fits
  if (options[["subgroup"]] != "") {

    subgroupLevels <- unique(dataset[[options[["subgroup"]]]])
    if (.maIsClassical(options)) startProgressbar(expectedTicks = length(subgroupLevels), label = gettext("Estimating Subgroup Models"))

    for (i in seq_along(subgroupLevels)) {

      subgroupLevel <- subgroupLevels[i]
      subgroupIndx  <- dataset[[options[["subgroup"]]]] == subgroupLevel
      subgroupData  <- droplevels(dataset[subgroupIndx, ])

      # forward NAs information and additional attributes
      tempNasIds    <- attr(dataset, "NasIds")[!attr(dataset, "NasIds")]
      attr(subgroupData, "NAs")          <- sum(tempNasIds[subgroupIndx])
      attr(subgroupData, "NasIds")       <- tempNasIds[subgroupIndx]
      attr(subgroupData, "subgroupIndx") <- subgroupIndx

      # fit the model
      fitOutput[[paste0("subgroup", subgroupLevel)]] <- do.call(fittingFunction, list(dataset = subgroupData, options = options, subgroupName = as.character(subgroupLevel)))

      if (.maIsClassical(options)) progressbarTick()
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

  if (nrow(dataset) < 2) {
    fit <- try(stop("Fewer than two estimates."))
    attr(fit, "subgroup") <- paste0(subgroupName)
    attr(fit, "dataset")  <- dataset
    return(list(
      fit          = fit,
      fitClustered = if (options[["clustering"]] != "") fit else NULL
    ))
  }

  # specify the effect size and outcome
  if (options[["analysis"]] == "metaAnalysis") {
    # specify the univariate input
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      sei  = as.name(options[["effectSizeStandardError"]]),
      data = dataset
    )
  } else if (options[["analysis"]] == "metaAnalysisMultilevelMultivariate") {
    # specify the multivariate input
    rmaInput <- list(
      yi   = as.name(options[["effectSize"]]),
      V    = if (.mammVarianceCovarianceMatrixReady(options)) .mammGetVarianceCovarianceMatrix(dataset, options) else as.name("samplingVariance"),
      data = dataset
    )
  }

  # add formulas if specified
  rmaInput$mods  <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  rmaInput$scale <- if (!.maIsUnrestrictedWeightedLeastSquares(options)) .maGetFormula(options[["heterogeneityModelTerms"]], options[["heterogeneityModelIncludeIntercept"]])

  # add random effects
  if (.maIsMultilevelMultivariate(options)) {
    randomFormulaList <- .mammGetRandomFormulaList(options)
    randomFormulaList <- unname(randomFormulaList) # remove names for some metafor post-processing functions
    if (length(randomFormulaList) != 0) {

      rmaInput$random <- randomFormulaList
      rmaInput$struct <- do.call(c, lapply(randomFormulaList, attr, which = "structure"))

      # modify hierarchical structure data, so the levels are nested within each other (otherwise level dropping test will fail)
      for (i in seq_along(randomFormulaList)) {
        if (is.null(attr(randomFormulaList[[i]], "structure")) && length(attr(randomFormulaList[[i]], "levels")) > 1) {
          dataset <- .mammEmbedLevelRandom(dataset, attr(randomFormulaList[[i]], "levels"))
        }
      }
      rmaInput$data <- dataset

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
  rmaInput$test   <- .maGetFixedEffectTestOptions(options)

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
  if (options[["analysis"]] == "metaAnalysis") {
    fit <- try(do.call(metafor::rma, rmaInput))
  } else if (options[["analysis"]] == "metaAnalysisMultilevelMultivariate") {
    fit <- try(do.call(metafor::rma.mv, rmaInput))
  }


  # add clustering if specified
  if (options[["clustering"]] != "" && !jaspBase::isTryError(fit)) {
    fitClustered <- try(metafor::robust(
      fit,
      cluster      = dataset[[options[["clustering"]]]],
      clubSandwich = options[["clusteringUseClubSandwich"]],
      adjust       = options[["clusteringSmallSampleCorrection"]]
    ))
  } else if (options[["clustering"]] != "" && jaspBase::isTryError(fit)) {
    fitClustered <- fit
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
  attr(fit, "dataset")  <- .maAlignDatasetToFitRows(fit, dataset, options)
  if (!is.null(fitClustered)) {
    attr(fitClustered, "subgroup") <- subgroupName
    attr(fitClustered, "dataset")  <- .maAlignDatasetToFitRows(fitClustered, dataset, options)
  }


  # return the results
  return(list(
    fit            = fit,
    fitClustered   = fitClustered
  ))
}

.maAlignDatasetToFitRows          <- function(fit, dataset, options) {

  if (!.maIsGLMM(options) || inherits(fit, "try-error") || !inherits(fit, "rma.glmm"))
    return(dataset)

  retainedRows <- fit[["not.na"]]
  if (is.null(retainedRows) ||
      length(retainedRows) != nrow(dataset) ||
      sum(retainedRows, na.rm = TRUE) != length(fit[["yi"]]))
    return(dataset)

  return(dataset[retainedRows, , drop = FALSE])
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
  fitOutput[["__fullDataset"]] <- .maUpdateFitModelDatasetFun(fitOutput[["__fullDataset"]], dataset, options)
  # add subgroup fits
  if (options[["subgroup"]] != "") {

    subgroupLevels <- unique(dataset[[options[["subgroup"]]]])

    for (i in seq_along(subgroupLevels)) {

      subgroupLevel <- subgroupLevels[i]
      subgroupIndx  <- dataset[[options[["subgroup"]]]] == subgroupLevel
      subgroupData  <- droplevels(dataset[subgroupIndx, ])

      # forward NAs information
      tempNasIds    <- attr(dataset, "NasIds")[!attr(dataset, "NasIds")]
      attr(subgroupData, "NAs")          <- sum(tempNasIds[subgroupIndx])
      attr(subgroupData, "NasIds")       <- tempNasIds[subgroupIndx]
      attr(subgroupData, "subgroupIndx") <- subgroupIndx

      # fit the model
      fitOutput[[paste0("subgroup", subgroupLevel)]] <- .maUpdateFitModelDatasetFun(fitOutput[[paste0("subgroup", subgroupLevel)]], subgroupData, options)
    }
  }

  # save the updated fits
  jaspResults[[objectName]]$object  <- fitOutput

  # set the container to non-null
  jaspResults[[paste0(objectName, "DataSet")]]$object <- TRUE

  return()

}

.maUpdateFitModelDatasetFun      <- function(fitOutput, dataset, options) {

  if (!is.null(fitOutput[["fit"]])) {
    fit <- fitOutput[["fit"]]
    attr(fit, "dataset") <- .maAlignDatasetToFitRows(fit, dataset, options)
  } else {
    fit <- NULL
  }

  if (.maIsClassical(options)) {
    if (!is.null(fitOutput[["fitClustered"]])) {
      fitClustered <- fitOutput[["fitClustered"]]
      attr(fitClustered, "dataset") <- .maAlignDatasetToFitRows(fitClustered, dataset, options)
    } else {
      fitClustered <- NULL
    }

    return(list(
      fit            = fit,
      fitClustered   = fitClustered
    ))
  } else {
    return(list(
      fit            = fit
    ))
  }
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

  if (.maIsClassical(options)) {
    fitOutputExtracted <- lapply(fitOutput, function(output){
      if (!.maIsClustered(options) || nonClustered) {
        return(output[["fit"]])
      } else {
        return((output)[["fitClustered"]])
      }
    })
  } else {
    fitOutputExtracted <- lapply(fitOutput, function(output){
      return(output[["fit"]])
    })
  }

  return(fitOutputExtracted)
}



