# Classical meta-analysis forest-plot integration.
#
# Connects fitted models to the forest-plot pipeline and provides shared plot helpers.

.maUltimateForestPlot                    <- function(jaspResults, options) {

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
  if (is.null(fit) || all(vapply(fit, jaspBase::isTryError, logical(1))) || (.maIsClassical(options) && !is.null(.maCheckIsPossibleOptions(options))))
    return()

  # try execute!
  plotRender <- try(.maMakeTheUltimateForestPlot(fit, options))

  if (inherits(plotRender, "try-error")) {
    forestPlot <- createJaspPlot(title = gettext("Forest Plot"))
    forestPlot$position <- 4
    forestPlot$dependOn(.maForestPlotDependencies)
    forestPlot$setError(plotRender)
    jaspResults[["forestPlot"]] <- forestPlot
    return()
  }

  forestPlot <- createJaspPlot(
    title  = gettext("Forest Plot"),
    width  = plotRender[["width"]],
    height = plotRender[["height"]]
  )
  forestPlot$position <- if (.maIsClassical(options)) 5 else 7
  forestPlot$dependOn(c(.maForestPlotDependencies, if (.maIsClassical(options)) .maDependencies else .robmaDependencies))
  forestPlot$plotObject <- plotRender[["plot"]]

  jaspResults[["forestPlot"]] <- forestPlot


  return()
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


.maSuppressPlot                       <- function(plotExpression) {
  temp <- tempfile()
  pdf(file = temp)
  dfOut <- plotExpression
  dev.off()
  unlink(temp)
  return(dfOut)
}
