# Robust Bayesian meta-analysis diagnostics.
#
# Builds diagnostic tables, plots, waiting states, and plot storage.

.robmaDiagnosticsTable                   <- function(jaspResults, options) {

  # create/extract section otherwise
  diagnosticsContainer <- .robmaExtractDiagnosticsContainer(jaspResults)

  if (!is.null(diagnosticsContainer[["diagnosticsTable"]]))
    return()

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  # create individual plots for each subgroup
  if (options[["subgroup"]] == "") {

    tempTable       <- .robmaDiagnosticsTableFun(fit = fit[[1]], options = options)
    tempTable$title <- gettext("Diagnostics Summary")
    tempTable$dependOn(c(.robmaDependencies, "mcmcDiagnosticsOverviewTable"))
    tempTable$position <- 1
    diagnosticsContainer[["diagnosticsTable"]] <- tempTable

  } else {

    # create the output container
    tempTable       <- createJaspContainer()
    tempTable$title <- gettext("Diagnostics Summary")
    tempTable$dependOn(c(.robmaDependencies, "mcmcDiagnosticsOverviewTable"))
    tempTable$position <- 1
    diagnosticsContainer[["diagnosticsTable"]] <- tempTable

    for (i in seq_along(fit)) {
      tempTable[[names(fit)[i]]]          <- .robmaDiagnosticsTableFun(fit = fit[[i]], options = options)
      tempTable[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
      tempTable[[names(fit)[i]]]$position <- i
    }
  }

  return()
}

.robmaDiagnosticsTableFun                <- function(fit, options) {

  # create table
  tempTable <- createJaspTable()

  tempTable$addColumnInfo(name = "par",            type = "string",   title = "")
  tempTable$addColumnInfo(name = "MCMC_error",     type = "number",   title = gettext("MCMC error"))
  tempTable$addColumnInfo(name = "MCMC_SD_error",  type = "number",   title = gettext("MCMC error/SD"))
  tempTable$addColumnInfo(name = "ESS",            type = "integer",  title = gettext("ESS"))
  tempTable$addColumnInfo(name = "R_hat",          type = "number",   title = gettext("R-hat"))

  # stop on error
  if (jaspBase::isTryError(fit)) {
    return(tempTable)
  }

  diagnosticsSummary     <- data.frame(summary(fit, type = "diagnostics")[["estimates"]])
  diagnosticsSummary$par <- rownames(diagnosticsSummary)
  diagnosticsSummary     <- diagnosticsSummary[,c("par", "MCMC_error", "MCMC_SD_error", "ESS", "R_hat")]
  diagnosticsSummary$ESS <- round(diagnosticsSummary$ESS)

  # obtain residual funnel plot
  tempTable$setData(diagnosticsSummary)

  return(tempTable)
}

.robmaDiagnosticsPlot                    <- function(jaspResults, options, parameter) {

  # section container
  diagnosticsContainer <- .robmaExtractDiagnosticsContainer(jaspResults)

  fit <- .maExtractFit(jaspResults, options)

  # stop on error
  if (is.null(fit) || (length(fit) == 1 && jaspBase::isTryError(fit[[1]])))
    return()

  # dispatch options
  if (parameter == "pooledEffect") {
    tempTitle        <- if (.maIsMetaregression(options)) gettext("Adjusted Effect") else gettext("Pooled Effect")
    tempPosition     <- 2
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotEffectSize"
    parameter        <- "mu"
  } else if (parameter == "heterogeneity") {
    tempTitle        <- gettext("Heterogeneity")
    tempPosition     <- 3
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotHeterogeneity"
    parameter        <- "tau"
  } else if (parameter == "moderation") {
    tempTitle        <- gettext("Moderation")
    tempPosition     <- 4
    tempFunction     <- .robmaDiagnosticsPlotModerationFun
    tempDependencies <- "mcmcDiagnosticsPlotModeration"
    fit              <- list(fit) # trick to forward the fit to a futher dispatch later
  } else if (parameter == "weights") {
    tempTitle        <- gettext("Weights")
    tempPosition     <- 5
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotWeights"
    parameter        <- "omega"
  } else if (parameter == "pet") {
    tempTitle        <- gettext("PET")
    tempPosition     <- 6
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotPet"
    parameter        <- "PET"
  } else if (parameter == "peese") {
    tempTitle        <- gettext("PEESE")
    tempPosition     <- 7
    tempFunction     <- .robmaDiagnosticsPlotFun
    tempDependencies <- "mcmcDiagnosticsPlotPeese"
    parameter        <- "PEESE"
  }


  # create individual plots for each subgroup
  if (options[["subgroup"]] == "" || parameter == "moderation") {

    tempContainer <- .robmaExtractDiagnosticsSubContainer(diagnosticsContainer, parameter, tempTitle, tempPosition, tempDependencies)
    do.call(tempFunction, list(container = tempContainer, options = options, fit = fit[[1]], parameter = parameter))

  } else {

    # create the output container
    tempContainerBlock       <- createJaspContainer()
    tempContainerBlock$title <- tempTitle
    tempContainerBlock$dependOn(tempDependencies)
    tempContainerBlock$position <- tempPosition
    diagnosticsContainer[[parameter]] <- tempContainerBlock

    for (i in seq_along(fit)) {

      tempContainer <- .robmaExtractDiagnosticsSubContainer(tempContainerBlock, names(fit)[i], gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup")), i, "")
      do.call(tempFunction, list(container = tempContainer, options = options, fit = fit[[i]], parameter = parameter))

    }

  }

  return()
}

.robmaDiagnosticsPlotFun                 <- function(container, options, fit, parameter) {

  if (jaspBase::isTryError(fit))
    return()

  # create a waiting plot for at least one type of the plot to be selected
  if (!options[["mcmcDiagnosticsPlotTypeTrace"]] && !options[["mcmcDiagnosticsPlotTypeAutocorrelation"]] && !options[["mcmcDiagnosticsPlotTypePosteriorSamplesDensity"]]) {
    .robmaDiagnosticsStoreWaitingPlot(container)
    return()
  }

  .robmaDiagnosticsStorePlot(container, options, fit, parameter, "chains",          gettext("Trace Plot"),           1, "mcmcDiagnosticsPlotTypeTrace")
  .robmaDiagnosticsStorePlot(container, options, fit, parameter, "autocorrelation", gettext("Autocorrelation Plot"), 2, "mcmcDiagnosticsPlotTypeAutocorrelation")
  .robmaDiagnosticsStorePlot(container, options, fit, parameter, "densityPlot",     gettext("Density Plot"),         3, "mcmcDiagnosticsPlotTypePosteriorSamplesDensity")

  return()
}

.robmaDiagnosticsPlotModerationFun       <- function(container, options, fit, parameter) {

  moderators <- unlist(options[["effectSizeModelTerms"]])

  for (j in seq_along(moderators)) {

    parameter <- moderators[j]

    # create individual plots for each subgroup
    if (options[["subgroup"]] == "" || parameter == "moderation") {

      tempContainer <- .robmaExtractDiagnosticsSubContainer(container, parameter, parameter, 1, "")
      do.call(.robmaDiagnosticsPlotFun, list(container = tempContainer, options = options, fit = fit[[1]], parameter = parameter))

    } else {

      # create the output container
      tempContainerBlock       <- createJaspContainer()
      tempContainerBlock$title <- parameter
      tempContainerBlock$position <- 3
      container[[parameter]] <- tempContainerBlock

      for (i in seq_along(fit)) {

        tempContainer <- .robmaExtractDiagnosticsSubContainer(tempContainerBlock, names(fit)[i], gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup")), i, "")
        do.call(.robmaDiagnosticsPlotFun, list(container = tempContainer, options = options, fit = fit[[i]], parameter = parameter))

      }

    }
  }

  return()
}

.robmaDiagnosticsStorePlot               <- function(container, options, fit, parameter, type, title, position, dependency) {

  if (!(options[[dependency]] && is.null(container[[type]])))
    return()

  tempPlot <- RoBMA::diagnostics(
    fit,
    parameter     = parameter,
    type          = type,
    plot_type     = "ggplot"
  )

  # add them to the container
  if (!ggplot2::is.ggplot(tempPlot)) {

    subcontainer <- createJaspContainer(title = title)
    subcontainer$position <- position
    subcontainer$dependOn(dependency)
    container[[type]] <- subcontainer

    for (i in 1:length(tempPlot)) {

      subplot <- createJaspPlot(width = 320, height = 250)
      subplot$position <- i
      subplot$dependOn(dependency)
      subplot$plotObject <- tempPlot[[i]] + ggplot2::labs(title = "") + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()
      subcontainer[[paste0(type, i)]] <- subplot

    }

  } else {

    subplot <- createJaspPlot(title = title, width = 320, height = 250)
    subplot$position <- position
    subplot$dependOn(dependency)
    subplot$plotObject <- tempPlot + ggplot2::labs(title = "") + jaspGraphs::geom_rangeframe(sides = "bl") + jaspGraphs::themeJaspRaw()
    container[[type]] <- subplot

  }

  return()
}

.robmaDiagnosticsStoreWaitingPlot        <- function(container) {

  if (!is.null(container[["waitingPlot"]]))
    return()

  subplot <- createJaspPlot(title = "", width = 320, height = 250)
  subplot$position <- 1
  subplot$setError(gettext("Please select at least one 'Type' of diagnostics plot (e.g., 'Trace', 'Autocorrelation' or 'Posterior samples density')."))
  subplot$dependOn(c("mcmcDiagnosticsPlotTypeTrace", "mcmcDiagnosticsPlotTypeAutocorrelation", "mcmcDiagnosticsPlotTypePosteriorSamplesDensity"))

  container[["waitingPlot"]] <- subplot

  return()
}
