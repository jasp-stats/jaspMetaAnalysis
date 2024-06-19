if(FALSE){




  ### extract effect sizes and variances from the fitted object
  dfForrest <- data.frame(
    yi      = fit[["yi"]],
    sei     = sqrt(fit[["vi"]]),
    weights = weights(fit)
  )

  # add CI using normal approximation
  dfForrest$lCi <- dfForrest$yi - 1.96 * dfForrest$sei
  dfForrest$uCi <- dfForrest$yi + 1.96 * dfForrest$sei


  ### add variables used for either color, shape, order or Left panel information
  additionalVariables <- c(
    if (length(options[["forestPlotLeftPanelVariablesSelected"]]) > 0) unlist(options[["forestPlotLeftPanelVariablesSelected"]]),
    if (options[["forestPlotMappingColor"]] != "") options[["forestPlotMappingColor"]],
    if (options[["forestPlotMappingShape"]] != "") options[["forestPlotMappingShape"]]
  )
  if (length(additionalVariables) > 0)
    dfForrest <- cbind(dfForrest, dataset[,additionalVariables,drop=FALSE])


  # add y-axis coordinates for plotting
  dfForrest$yPos <- seq(nrow(dfForrest))

  # mapping by
  plotForrest <- ggplot2::ggplot(data = dfForrest) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x     = yi,
        y     = yPos
#        color = if (options[["forestPlotMappingColor"]] != "") !!rlang::sym(options[["forestPlotMappingColor"]]),
#        shape = if (options[["forestPlotMappingShape"]] != "") as.name(options[["forestPlotMappingShape"]]),
      ),
      color   = if (options[["forestPlotMappingColor"]] != "") dfForrest[[options[["forestPlotMappingColor"]]]] else options[["forestPlotAuxiliaryPlotColor"]],
      size    = dfForrest[["weights"]] * options[["forestPlotRelativeSizeEstimates"]],
      shape   = if (options[["forestPlotMappingShape"]] == "") 15
    ) +
    ggplot2::geom_errorbarh(
      mapping = ggplot2::aes(
        xmin = lCi,
        xmax = uCi,
        y    = yPos),
      height  = 0)
  plotForrest

  ggplot2::geom_vline(
    xintercept = 0,
    linetype   = "dashed")

  require(gridExtra)
  plot1 <- qplot(1)
  plot2 <- qplot(1)
  grid.arrange(plot1, plot2, ncol=2)


}
