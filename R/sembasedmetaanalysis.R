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

SemBasedMetaAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  # set OpenMx options
  # the JASP options() cleanup does not properly re-sets OpenMx settings
  # consequently, the fitting function crashes with matrix(byrow) error
  OpenMx::mxSetDefaultOptions()

  # read the data set
  dataset <- .masemReadData(dataset)

  # estimate the models
  .masemFitModels(jaspResults, dataset, options)

  # create summary with model fit statistics (for all models)
  .masemModelFitTable(jaspResults, options)

  if (options[["pairwiseModelComparison"]])
    .masemPairwiseModelComparisonTable(jaspResults, options)

  # create model-level summaries
  if (options[["modelSummary"]])
    .masemModelSummaryTable(jaspResults, options)

  # create path diagrams
  if (options[["pathDiagram"]])
    .masemModelPathDiagram(jaspResults, options)

  return()
}

.semmetaDependencies          <- c("models", "modelSummaryConfidenceIntervalType")
.masemReady                   <- function(options) {
  return(TRUE)
}
.masemReadData                <- function(dataset) {

  if (!is.null(dataset))
    return(dataset)

  dataset <- .readDataSetToEnd(all.columns = TRUE)

  # check that all columns are numeric
  for(i in seq_len(ncol(dataset))) {
    dataset[,i] <- as.numeric(as.character(dataset[,i]))
  }

  return(dataset)
}
.masemGetModelOutputContainer <- function(jaspResults, name, position, MASEM = FALSE) {

  if (!is.null(jaspResults[[paste0("outputContainer", name)]])) {
    outputContainer <- jaspResults[[paste0("outputContainer", name)]]
  } else {
    outputContainer <- createJaspContainer(title = name)
    outputContainer$position <- 2 + position / 10
    outputContainer$dependOn((if (MASEM) .masemDependencies else .semmetaDependencies))
    jaspResults[[paste0("outputContainer", name)]] <- outputContainer
  }

  return(outputContainer)
}
.masemFitModels               <- function(jaspResults, dataset, options, MASEM = FALSE) {

  # since multiple models are fitted, we check for dependencies on model-by-model basis
  # (by comparing to stored options to the current options)
  # obtain the model container
  # TODO: this container is always reset to NULL, no idea what is wrong
  if (is.null(jaspResults[["modelContainer"]])) {
    # create the jaspState to hold the model object
    # (only run on initialization)
    modelContainer <- createJaspState()
    jaspResults[["modelContainer"]] <- modelContainer
    if (MASEM)       modelContainer$dependOn(c("correlationCovarianceMatrix", "sampleSize", "means", "dataInputType", "variableNameSeparator"))
    else if (!MASEM) modelContainer$dependOn(c("modelSummaryConfidenceIntervalType"))
    fits <- list()
  } else {
    modelContainer <- jaspResults[["modelContainer"]]
    fits <- modelContainer$object
  }

  # compute the individual model fits
  for (i in seq_along(options[["models"]])) {

    model       <- options[["models"]][[i]]
    modelSyntax <- model[["syntax"]][["model"]]

    # check if the model is already fitted
    if (model[["value"]] %in% names(fits)) {

      # extract the fit
      tempFit <- fits[[model[["value"]]]]

      # check if the model is still valid (by comparing stored options to the current options)
      if (isTRUE(all.equal(attr(tempFit, "model"), model)) &&
          isTRUE(all.equal(attr(tempFit, "dataset"), dataset)) &&
          if (!MASEM) attr(tempFit, "modelSummaryConfidenceIntervalType") == options[["modelSummaryConfidenceIntervalType"]]  else TRUE &&
          if (MASEM)  attr(tempFit, "dataInputType")                      == options[["dataInputType"]]                       else TRUE)
        next
    } else {
      tempFit <- NULL
    }

    if (trimws(modelSyntax) == "" || (MASEM && is.null(dataset[["data"]]))) {
      # skip if syntax is empty
      tempFit <- NULL

    } else {

      jaspBase::startProgressbar(1, label = gettextf("Estimating: %1$s", model[["value"]]))

      if (MASEM) tempFit <- .masemFitModelsFun(model, dataset, options)
      else       tempFit <- .semmetaFitModelsFun(model, dataset, options)

      jaspBase::progressbarTick()

      # add options to the model fit
      attr(tempFit, "model") <- model
      attr(tempFit, "dataset") <- dataset
      if       (MASEM) attr(tempFit, "dataInputType") <- options[["dataInputType"]]
      else if (!MASEM) attr(tempFit, "modelSummaryConfidenceIntervalType") <- options[["modelSummaryConfidenceIntervalType"]]
    }

    # save output
    fits[[model[["value"]]]] <- tempFit
  }

  modelContainer$object <- fits

  return()
}
.semmetaFitModelsFun          <- function(model, dataset, options) {

  # prepare RAM
  tempRam <- try(metaSEM::lavaan2RAM(
    model         = model[["syntax"]][["model"]],
    obs.variables = .masemGetObservedVariables(model),
    std.lv        = model[["fixLatentVarianceTo1"]]
  ))

  # fit SEM
  if (!jaspBase::isTryError(tempRam)) {
    tempFit <- try(metaSEM::sem(
      RAM                 = tempRam,
      data                = dataset,
      intervals.type      = .masemGetIntervalsType(options),
      replace.constraints = model[["replaceConstraints"]]
    ))
  } else {
    # forward ram errors
    tempFit <- tempRam
  }

  # forward any mxfit errors
  if (!jaspBase::isTryError(tempFit) && inherits(tempFit[["mx.fit"]], "simpleError")){
    tempFit <- try(stop(tempFit[["mx.fit"]]))
  }

  return(tempFit)
}
.masemFitModelsFun            <- function(model, dataset, options) {

  # prepare RAM
  tempRam <- try(metaSEM::lavaan2RAM(
    model         = model[["syntax"]][["modelOriginal"]],
    obs.variables = attr(dataset[["data"]], "variableNames"),
    std.lv        = if (options[["dataInputType"]] == "correlation") TRUE else model[["fixLatentVarianceTo1"]]
  ))

  # prepare data
  dataCall <- list(
    x = dataset[["data"]],
    n = dataset[["n"]],
    cor.analysis = options[["dataInputType"]] == "correlation"
  )
  if (!is.null(dataset[["means"]])) {
    dataCall[["Means"]] <- dataset[["means"]]
  }
  corDataset <- try(do.call(metaSEM::Cor2DataFrame, dataCall))

  # fit SEM
  if (!jaspBase::isTryError(tempRam) && !jaspBase::isTryError(corDataset)) {

    OpenMx::mxOption(NULL, "Calculate Hessian", "Yes")
    OpenMx::mxOption(NULL, "Standard Errors", "Yes")

    tempFit <- try(metaSEM::osmasem2(
      RAM                 = tempRam,
      data                = corDataset,
      cor.analysis        = options[["dataInputType"]] == "correlation",
      mean.analysis       = !is.null(dataset[["means"]]),
      RE.type.Sigma       = .masemGetRandomEffectsType(model[["randomEffectsSigma"]]),
      RE.type.Mu          = .masemGetRandomEffectsType(model[["randomEffectsMu"]]),
      RE.type.SigmaMu     = .masemGetRandomEffectsType(model[["randomEffectsSigmaMu"]]),
      replace.constraints = model[["replaceConstraints"]]
    ))
  } else {
    if (jaspBase::isTryError(tempRam)){
      # forward ram errors
      tempFit <- tempRam
    } else {
      # forward data errors
      tempFit <- corDataset
    }
  }

  # forward any mxfit errors
  if (!jaspBase::isTryError(tempFit) && inherits(tempFit[["mx.fit"]], "simpleError")){
    tempFit <- try(stop(tempFit[["mx.fit"]]))
  }

  return(tempFit)
}
.masemModelFitTable           <- function(jaspResults, options, MASEM = FALSE) {

  if (!is.null(jaspResults[["modelFitTable"]]))
    return()

  # prepare table
  modelFitTable <- createJaspTable(gettext("Model Fit"))
  modelFitTable$position <- 1
  modelFitTable$dependOn(if (MASEM) .masemDependencies else .semmetaDependencies)
  jaspResults[["modelFitTable"]] <- modelFitTable

  # add columns
  modelFitTable$addColumnInfo(name = "name",          type = "string",  title = "")
  modelFitTable$addColumnInfo(name = "logLik",        type = "number",  title = gettext("log Lik."))
  modelFitTable$addColumnInfo(name = "df",            type = "integer", title = gettext("df"))
  modelFitTable$addColumnInfo(name = "aic",           type = "number",  title = gettext("AIC"))
  modelFitTable$addColumnInfo(name = "bic",           type = "number",  title = gettext("BIC"))

  # exit if not ready
  if(!.masemReady(options) || is.null(jaspResults[["modelContainer"]]))
    return()

  # extract fits
  fits <- jaspResults[["modelContainer"]]$object

  # loop over models and store model fit indicies
  out <- list()
  for (model in options[["models"]]) {

    # extract model fit
    tempFit <- fits[[model[["value"]]]]

    if (jaspBase::isTryError(tempFit) || is.null(tempFit)) {
      out[[model[["value"]]]] <- data.frame(
        name = model[["value"]],
        logLik = NA,
        df     = NA,
        aic    = NA,
        bic    = NA
      )
      if (jaspBase::isTryError(tempFit))
        modelFitTable$addFootnote(gettextf("%1$s fit failed with the following message %2$s.", model[["value"]], tempFit))
    } else {
      out[[model[["value"]]]] <- data.frame(
        name   = model[["value"]],
        logLik = as.numeric(logLik(tempFit[["mx.fit"]])),
        df     = attr(logLik(tempFit[["mx.fit"]]), "df"),
        aic    = AIC(tempFit[["mx.fit"]]),
        bic    = BIC(tempFit[["mx.fit"]])
      )

      # check for openMX errors and warngings
      openMxMessages <- .openMxCheckWarnings(summary(tempFit))
      for (i in seq_along(openMxMessages)) {
        modelFitTable$addFootnote(gettextf("%1$s: %2$s", model[["value"]], openMxMessages[i]), symbol = gettext("Warning"))
      }
    }
  }

  # assign output to table
  modelFitTable$setData(do.call(rbind, out))

  return()
}
.masemModelSummaryTable       <- function(jaspResults, options, MASEM = FALSE) {

  # extract fits
  fits <- jaspResults[["modelContainer"]]$object

  # iterate across models and slot the model summary into the corresponding output container
  for (i in seq_along(options[["models"]])) {

    model   <- options[["models"]][[i]]
    tempFit <- fits[[model[["value"]]]]

    # get output container
    tempOutputContainer <- .masemGetModelOutputContainer(jaspResults, model[["value"]], i, MASEM)

    # create the summary table if it does not exists
    if (MASEM) {

      if (is.null(tempOutputContainer[["summaryTableParameters"]]) && options[["modelSummaryRegression"]]) {
        .masemCreateSummaryTable(tempOutputContainer, tempFit, options, output = "regression")
      }

      if (is.null(tempOutputContainer[["summaryTableParameters"]]) && options[["modelSummaryMeansIntercepts"]] && length(options[["means"]] > 0)) {
        .masemCreateSummaryTable(tempOutputContainer, tempFit, options, output = "meansIntercepts")
      }

      if (is.null(tempOutputContainer[["summaryTableCovariances"]]) && options[["modelSummaryCovariances"]]) {
        .masemCreateSummaryTable(tempOutputContainer, tempFit, options, output = "covariances")
      }

      if (is.null(tempOutputContainer[["summaryTableRandomEffects"]]) && options[["modelSummaryRandomEffects"]]) {
        .masemCreateSummaryTable(tempOutputContainer, tempFit, options, output = "randomEffects")
      }

    } else {

      # create the summary table if it does not exists
      if (is.null(tempOutputContainer[["summaryTable"]])) {
        .semmetaCreateSummaryTable(tempOutputContainer, tempFit, options)
      }

      # create the computed estimates table if it does not exists (and mxalgebras are present)
      if (is.null(tempOutputContainer[["computedEstimatesTable"]]) && !jaspBase::isTryError(tempFit) && !is.null(tempFit[["mxalgebras"]])) {
        .semmetaCreateComputedEstimatesTable(tempOutputContainer, fits[[model[["value"]]]], options)
      }
    }

  }

  return()
}
.masemModelPathDiagram        <- function(jaspResults, options, MASEM = FALSE) {

  # extract fits
  fits <- jaspResults[["modelContainer"]]$object

  # iterate across models and slot the model plot into the corresponding output container
  for (i in seq_along(options[["models"]])) {

    model <- options[["models"]][[i]]

    # get output container
    tempOutputContainer <- .masemGetModelOutputContainer(jaspResults, model[["value"]], i)

    # check if the plot already exists
    if (!is.null(tempOutputContainer[["pathDiagram"]]))
      next

    # create plot
    tempPlot <- createJaspPlot(title = gettext("Path Diagram"), width = 600, height = 400)
    tempPlot$position <- 5
    tempPlot$dependOn(c((if (MASEM) .masemDependencies else .semmetaDependencies),
                        "pathDiagram", "pathDiagramShowParameterNames",
                        "pathDiagramLayout",
                        "pathDiagramManifestNodeWidth", "pathDiagramLatentNodeWidth", "pathDiagramUnitVectorNodeWidth",
                        "pathDiagramLabelSize", "pathDiagramEdgeLabelSize", "pathDiagramNumberOfDigits"))
    tempOutputContainer[["pathDiagram"]] <- tempPlot

    # skip if not ready
    if (!.masemReady(options) && !options[["pathDiagramShowParameterNames"]])
      next

    # prepare path based on the syntax / fitted model
    # the MASEM model with parameter names can be plotted based only on the fitted object (use syntax only if that is not available)
    if (options[["pathDiagramShowParameterNames"]] && (!MASEM || (MASEM && (is.null(fits[[model[["value"]]]]) || jaspBase::isTryError(fits[[model[["value"]]]]))))) {

      # extract the sem paths
      tempPaths <- model[["syntax"]][[if (MASEM) "modelOriginal" else "model"]]

      # skip if syntax is empty
      if (trimws(tempPaths) == "")
        next

      # create the path plot object
      tempPaths <- try(semPlot::semPlotModel(tempPaths))

      # deal with possibly incorrect syntax
      if (jaspBase::isTryError(tempPaths)) {
        tempPlot$setError(gettextf("Syntax failed with the following message %1$s.", tempPaths))
        next
      }

    } else {

      # extract model
      tempFit <- fits[[model[["value"]]]]

      if (is.null(tempFit))
        return()

      # check if the model fit failed
      if (jaspBase::isTryError(tempFit)) {
        tempPlot$setError(gettextf("Model fit failed with the following message %1$s.", tempFit))
        next
      }

      # extract the sem paths
      if (MASEM) {
        tempPaths <- try(.masem2SemPlot(tempFit))
      } else {
        tempPaths <- try(.semmeta2SemPlot(tempFit))
      }

      # deal with possible errors
      if (jaspBase::isTryError(tempPaths)) {
        if (grepl("subscript out of bounds", tempPaths) && model[["replaceConstraints"]]) {
          tempPlot$setError(gettextf("Model visualization might not be possible for models with 'Replace constraints' option."))
        } else {
          tempPlot$setError(gettextf("Model visualization failed with the following message %1$s.", tempPaths))
        }
        next
      }
    }

    # encode output nodes
    if (!MASEM) {
      tempPaths <- .metasemDecodeSemPlot(tempPaths, paste0("data.", model$syntax$prefixedColumns$data.))
    }

    # create the plot
    tempOut <- jaspBase::.suppressGrDevice(semPlot::semPaths(
      object         = tempPaths,
      what           = if (options[["pathDiagramShowParameterNames"]]) "path" else "est",
      layout         = options[["pathDiagramLayout"]],

      nCharNodes = 0,
      nCharEdges = 0,
      sizeInt    = options[["pathDiagramManifestNodeWidth"]],
      sizeMan    = options[["pathDiagramLatentNodeWidth"]],
      sizeLat    = options[["pathDiagramUnitVectorNodeWidth"]],
      nDigits    = options[["pathDiagramNumberOfDigits"]],
      weighted   = FALSE,

      edge.color     = "black",
      color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
      title          = FALSE,
      label.cex      = options[["pathDiagramLabelSize"]],
      edge.label.cex = options[["pathDiagramEdgeLabelSize"]],
      ask            = FALSE
    ))

    # add output to container
    tempPlot$plotObject <- tempOut
  }

  return()
}
.semmetaCreateSummaryTable           <- function(tempOutputContainer, tempFit, options) {

  # create summary table
  tempSummaryTable <- createJaspTable(gettext("Coefficient Summary"))
  tempSummaryTable$position <- 1
  tempSummaryTable$dependOn(c(.semmetaDependencies, "modelSummary"))
  tempOutputContainer[["summaryTable"]] <- tempSummaryTable

  # add columns
  tempSummaryTable$addColumnInfo(name = "parameter", type = "string",  title = "")
  tempSummaryTable$addColumnInfo(name = "estimate",  type = "number",  title = gettext("Estimate"))
  if (options[["modelSummaryConfidenceIntervalType"]] == "standardErrors") {
    tempSummaryTable$addColumnInfo(name = "se",        type = "number",  title = gettext("Standard Error"))
  }
  tempSummaryTable$addColumnInfo(name = "lCi",       type = "number",  title = gettext("Lower"), overtitle = gettextf("95%% CI"))
  tempSummaryTable$addColumnInfo(name = "uCi",       type = "number",  title = gettext("Upper"), overtitle = gettextf("95%% CI"))
  if (options[["modelSummaryConfidenceIntervalType"]] == "standardErrors") {
    tempSummaryTable$addColumnInfo(name = "z",         type = "number",  title = gettext("z"))
    tempSummaryTable$addColumnInfo(name = "p",         type = "pvalue",  title = gettext("p"))
  }

  # skip if not ready
  if (!.masemReady(options))
    return()

  if (is.null(tempFit))
    return()

  # check if the model fit failed
  if (jaspBase::isTryError(tempFit)) {
    tempSummaryTable$setError(gettextf("Model fit failed with the following message %1$s.", tempFit))
    return()
  }

  # extract the parameter estimates
  tempOutput <- summary(tempFit)[["coefficients"]]
  colnames(tempOutput) <- c("estimate", "se", "lCi", "uCi", "z", "p")
  if (options[["modelSummaryConfidenceIntervalType"]] == "likelihoodBased") {
    tempOutput <- tempOutput[, c("estimate", "lCi", "uCi")]
  }
  tempOutput$parameter <- rownames(tempOutput)

  # add output to container
  tempSummaryTable$setData(tempOutput)

  return()
}
.semmetaCreateComputedEstimatesTable <- function(tempOutputContainer, tempFit, options) {

  # create summary table
  tempComputedTable <- createJaspTable(gettext("Computed Estimates Summary"))
  tempComputedTable$position <- 2
  tempComputedTable$dependOn(c(.semmetaDependencies, "modelSummary"))
  tempOutputContainer[["computedEstimatesTable"]] <- tempComputedTable

  # add columns
  tempComputedTable$addColumnInfo(name = "parameter", type = "string",  title = "")
  tempComputedTable$addColumnInfo(name = "estimate",  type = "number",  title = gettext("Estimate"))
  tempComputedTable$addColumnInfo(name = "lCi",       type = "number",  title = gettext("Lower"), overtitle = gettextf("95%% CI"))
  tempComputedTable$addColumnInfo(name = "uCi",       type = "number",  title = gettext("Upper"), overtitle = gettextf("95%% CI"))

  # skip if not ready
  if (!.masemReady(options))
    return()

  if (is.null(tempFit))
    return()

  # check if the model fit failed
  if (jaspBase::isTryError(tempFit)) {
    tempComputedTable$setError(gettextf("Model fit failed with the following message %1$s.", tempFit))
    return()
  }

  # extract the parameter estimates
  tempOutput <- data.frame(summary(tempFit)[["mxalgebras"]])
  colnames(tempOutput) <- c("lCi", "estimate", "uCi")
  tempOutput$parameter <- rownames(tempOutput)

  # add output to container
  tempComputedTable$setData(tempOutput)

  return()
}

# helper functions
.semmeta2SemPlot           <- function(x) {
  # based on metaSEM::plot.mxsem
  # Creates the semPlot object from the mxsem object

  A <- x$mx.fit@matrices$Amatrix$values
  S <- x$mx.fit@matrices$Smatrix$values
  F <- x$mx.fit@matrices$Fmatrix$values
  M <- x$mx.fit@matrices$Mmatrix$values
  if (is.null(M))
    M <- matrix(0, nrow = 1, ncol = ncol(A))
  RAM <- x$RAM
  for (i in seq_len(nrow(S))) for (j in seq_len(ncol(S))) {
    if (grepl("data.", RAM$S[i, j])) {
      tmp <- strsplit(RAM$S[i, j], "data.", fixed = TRUE)[[1]][2]
      S[i, j] <- eval(parse(text = paste0("mean(x$data$",
                                          tmp, ", na.rm=TRUE)")))
    }
  }
  for (i in seq_len(nrow(A))) for (j in seq_len(ncol(A))) {
    if (grepl("data.", RAM$A[i, j])) {
      tmp <- strsplit(RAM$A[i, j], "data.", fixed = TRUE)[[1]][2]
      A[i, j] <- eval(parse(text = paste0("mean(x$data$",
                                          tmp, ", na.rm=TRUE)")))
    }
  }
  for (j in seq_len(ncol(M))) {
    if (grepl("data.", RAM$M[1, j])) {
      tmp <- strsplit(RAM$M[1, j], "data.", fixed = TRUE)[[1]][2]
      M[1, j] <- eval(parse(text = paste0("mean(x$data$",
                                          tmp, ", na.rm=TRUE)")))
    }
  }
  index_obs <- (apply(F, 2, sum) == 1)
  allNames <- colnames(A)
  manNames <- allNames[index_obs]
  latNames <- allNames[!index_obs]
  sem.plot <- semPlot::ramModel(A = A, S = S, F = F, M = M, manNames = manNames, latNames = latNames)

  return(sem.plot)
}
.masem2SemPlot             <- function(x) {
  # based on metaSEM::plot.osmasem
  # Creates the semPlot object from the mxsem object
  manNames <- NULL
  latNames <- NULL
  labels   <- c("labels", "RAM")

  if (is.element(class(x), "osmasem2")) {
    A <- x$mx.fit$Amatrix$values
    S <- x$mx.fit$Smatrix$values
  }
  else {
    A <- x$mx.fit$Amatrix$result
    S <- x$mx.fit$Smatrix$result
    if (is.null(S)) {
      S <- x$mx.fit$algebras$Smatrix$result
    }
  }
  F <- x$mx.fit$Fmatrix$values
  Id <- diag(nrow(S))
  if (is.element(class(x), "osmasem2")) {
    ObsCovs <- vec2symMat(apply(x$data$data[, x$data$ylabels,
                                            drop = FALSE], 2, mean, na.rm = TRUE), diag = !x$cor.analysis)
    ImpCovs <- x$mx.fit$expSigma$result
    if (is.null(manNames))
      manNames <- x$data$obslabels
  }
  else {
    ObsCovs <- vec2symMat(apply(x$data$data[, x$labels$ylabels],
                                drop = FALSE, 2, mean, na.rm = TRUE), diag = FALSE)
    ImpCovs <- x$mx.fit$impliedR$result
    if (is.null(manNames))
      manNames <- x$labels$obslabels
  }
  if (is.null(manNames))
    manNames <- paste("X", seq(1, nrow(F), 1), sep = "")
  dimnames(ImpCovs) <- dimnames(ObsCovs) <- list(manNames,
                                                 manNames)
  lat_index <- dimnames(F)[[2]] %in% dimnames(F)[[1]]
  lat_names <- dimnames(F)[[2]][!lat_index]
  if (is.null(latNames)) {
    if (!is.null(lat_names)) {
      latNames <- lat_names
    }
    else {
      no.latent <- ncol(F) - nrow(F)
      if (no.latent > 0)
        latNames <- paste("L", seq(1, no.latent), sep = "")
    }
  }
  if (is.element(class(x), "osmasem2")) {
    if (x$mean.analysis) {
      M <- x$mx.fit$Mmatrix$values
      colnames(M) <- colnames(F)
      out <- semPlot::ramModel(A = A, S = S, F = F, M = M,
                               manNames = manNames, latNames = latNames, ObsCovs = ObsCovs,
                               ImpCovs = ImpCovs)
    }
    else {
      out <- semPlot::ramModel(A = A, S = S, F = F, manNames = manNames,
                               latNames = latNames, ObsCovs = ObsCovs, ImpCovs = ImpCovs)
    }
  }
  else {
    out <- semPlot::ramModel(A = A, S = S, F = F, manNames = manNames,
                             latNames = latNames, ObsCovs = ObsCovs, ImpCovs = ImpCovs)
  }

  if (is.element(class(x), c("osmasem", "osmasem3L"))) {
    labels <- match.arg(labels)
    if (labels == "labels") {
      A.labels <- x$mx.fit$A0$labels
      S.labels <- x$mx.fit$S0$labels
      labels <- c(c(A.labels), c(S.labels))
      row.pars <- as.numeric(row.names(out@Pars))
      out@Pars$label <- labels[row.pars]
      na.index <- is.na(out@Pars$label)
      out@Pars$label[na.index] <- ""
    }
  }

  return(out)
}
.metasemDecodeSemPlot      <- function(x, dataVar) {
  # based on jaspSem::.lavToPlotObj
  # Create semplot model and decode the names of the manifest variables

  decodeColNamesWithData <- function(x){
    return(ifelse(grepl("data.", x), paste0("data.", decodeColNames(gsub("data.", "", x))), decodeColNames(x)))
  }

  manifests <- c(x@Vars$name[x@Vars$manifest], dataVar)

  nameAreManifest <- x@Vars$name %in% manifests
  if (any(nameAreManifest))
    x@Vars$name[nameAreManifest] <- decodeColNamesWithData(x@Vars$name[nameAreManifest])

  lhsAreManifest <- x@Pars$lhs %in% manifests
  if (any(lhsAreManifest))
    x@Pars$lhs[lhsAreManifest] <- decodeColNamesWithData(x@Pars$lhs[lhsAreManifest])

  rhsAreManifest <- x@Pars$rhs %in% manifests
  if (any(rhsAreManifest))
    x@Pars$rhs[rhsAreManifest] <- decodeColNamesWithData(x@Pars$rhs[rhsAreManifest])

  labelsAreManifest <- x@Pars$label %in% manifests
  if (any(labelsAreManifest))
    x@Pars$label[labelsAreManifest] <- decodeColNamesWithData(x@Pars$label[labelsAreManifest])

  if(.hasSlot(x, "Thresholds") && nrow(x@Thresholds) > 0)
    x@Thresholds$lhs <- ifelse(nchar(x@Thresholds$lhs) > 0, decodeColNamesWithData(x@Thresholds$lhs), "")

  return(x)
}
.masemGetIntervalsType     <- function(options) {
  return(switch(
    options[["modelSummaryConfidenceIntervalType"]],
    "standardErrors"  = "z",
    "likelihoodBased" = "LB"
  ))
}
.masemGetObservedVariables <- function(model) {

  observedVariables <- c(
    model$syntax$columns,
    model$syntax$prefixedColumns$data.
  )
  observedVariables <- encodeColNames(observedVariables)
  return()
}
.masemGetRandomEffectsType <- function(type) {
  return(switch(
    type,
    "diagonal"  = "Diag",
    "symmetric" = "Symm",
    "zero"      = "Zero",
    "full"      = "Full"
  ))
}
.openMxCheckWarnings       <- function(x) {

  msgs <- character()

  # Information matrix not positive definite
  if (!is.null(x$infoDefinite) && !is.na(x$infoDefinite) && !x$infoDefinite) {
    msgs <- c(msgs, "Information matrix is not positive definite (not at a candidate optimum). Be suspicious of these results. At minimum, do not trust the standard errors.")
  }

  # Information criteria warning (from fit statistics)
  if (!is.null(x$CFI) && !is.null(x$TLI) && !is.null(x$RMSEA) && !is.null(x$fitUnits)) {
    if (!is.null(x$independence) && !is.na(x$independence) && !is.null(x$likelihood) && x$independence < x$likelihood) {
      msgs <- c(msgs, "Your model may be mis-specified (and fit worse than an independence model), or you may be using the wrong independence model.")
    }
  }

  # Status code messages (from optimizer)
  if (!is.null(x$npsolMessage) && nzchar(x$npsolMessage)) {
    msgs <- c(msgs, x$npsolMessage)
  }

  # Return unique messages
  return(msgs)
}
