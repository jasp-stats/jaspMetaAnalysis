if(FALSE){




  ### extract effect sizes and variances from the fitted object
  dfForrest <- data.frame(
    effectSize     = fit[["yi"]],
    standardError  = sqrt(fit[["vi"]]),
    weights        = weights(fit),
    id             = seq_along(fit[["yi"]])
  )

  # add CI using normal approximation
  dfForrest$lCi <- dfForrest$effectSize - 1.96 * dfForrest$standardError
  dfForrest$uCi <- dfForrest$effectSize + 1.96 * dfForrest$standardError

  # transform effect size when requested
  if (options[["transformEffectSize"]] != "none")
    dfForrest[,c("effectSize", "lCi", "uCi")] <- do.call(
      .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
      list(dfForrest[,c("effectSize", "lCi", "uCi")]))

  xRange <- range(dfForrest$lCi, dfForrest$uCi)

  # add y-axis coordinates for plotting
  dfForrest$yPos <- seq(nrow(dfForrest)) * options[["forestPlotRelativeSizeRow"]]

  ### add prediction intervals
  if (options[["forestPlotPredictedEffects"]]) {

    fitPrediction <- data.frame(predict(fit))

    # replicate the prediction for each estimate if the predictions are the same (no moderators)
    if (nrow(fitPrediction) == 1)
      fitPrediction <- do.call(rbind, replicate(nrow(dfForrest), fitPrediction, simplify = FALSE))

    fitPrediction$id   <- dfForrest$id
    fitPrediction$yPos <- dfForrest$yPos

    # create prediction diamond coordinates for each estimate
    fitPrediction <- do.call(rbind, lapply(1:nrow(fitPrediction), function(i) {
      data.frame(
        id          = fitPrediction$id[i],
        xPrediction = c(fitPrediction$pi.lb[i], fitPrediction$pred[i],       fitPrediction$pi.ub[i], fitPrediction$pred[i]),
        yPrediction = c(fitPrediction$yPos[i],  fitPrediction$yPos[i] - 1/3, fitPrediction$yPos[i],  fitPrediction$yPos[i] + 1/3)
      )
    }))

    fitPrediction <- merge(fitPrediction, dfForrest, by = "id")

    # transform effect size when requested
    if (options[["transformEffectSize"]] != "none")
      fitPrediction[,"xPrediction"] <- do.call(
        .maGetEffectSizeTransformationOptions(options[["transformEffectSize"]]),
        list(fitPrediction[,"xPrediction"]))

    xRange <- range(c(xRange, range(fitPrediction$xPrediction)))
  }


  ### add variables used for either color, shape, order or Left panel information
  additionalVariables <- c(
    if (length(options[["forestPlotLeftPanelVariablesSelected"]]) > 0) unlist(options[["forestPlotLeftPanelVariablesSelected"]]),
    if (options[["forestPlotMappingColor"]] != "") options[["forestPlotMappingColor"]],
    if (options[["forestPlotMappingShape"]] != "") options[["forestPlotMappingShape"]]
  )
  if (length(additionalVariables) > 0)
    dfForrest <- cbind(dfForrest, dataset[,additionalVariables,drop=FALSE])



  ### make the forest plot
  plotForrest <- ggplot2::ggplot(data = dfForrest)


  ### add prediction intervals
  # dispatch the aes call based on mapping:
  aesCall <- list(
    x     = as.name("xPrediction"),
    y     = as.name("yPrediction"),
    group = as.name("id"),
    fill  = if (options[["forestPlotMappingColor"]] != "") as.name(options[["forestPlotMappingColor"]])
  )
  geomCall <- list(
    data    = fitPrediction,
    mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
    fill    = if (options[["forestPlotMappingColor"]] == "") "grey20",
    alpha   = 0.8
  )
  plotForrest <- plotForrest + do.call(ggplot2::geom_polygon, geomCall[!sapply(geomCall, is.null)])


  ### add estimates
  # dispatch the aes call based on mapping:
  aesCall <- list(
    x     = as.name("effectSize"),
    y     = as.name("yPos"),
    color = if (options[["forestPlotMappingColor"]] != "") as.name(options[["forestPlotMappingColor"]]),
    shape = if (options[["forestPlotMappingShape"]] != "") as.name(options[["forestPlotMappingShape"]])
  )
  geomCall <- list(
      mapping = do.call(ggplot2::aes, aesCall[!sapply(aesCall, is.null)]),
      color   = if (options[["forestPlotMappingColor"]] == "") options[["forestPlotAuxiliaryPlotColor"]],
      shape   = if (options[["forestPlotMappingShape"]] == "") 15,
      size    = dfForrest[["weights"]] * options[["forestPlotRelativeSizeEstimates"]]
  )
  plotForrest <- plotForrest + do.call(ggplot2::geom_point, geomCall[!sapply(geomCall, is.null)])
  # change scale for shapes to full shapes if used
  if (options[["forestPlotMappingShape"]] != "")
    plotForrest <- plotForrest + ggplot2::scale_shape_manual(values = rep(c(15:18, 21:25), length.out = length(unique(dfForrest[[options[["forestPlotMappingShape"]]]]))))


  ### add CIs
  plotForrest <- plotForrest + ggplot2::geom_errorbarh(
    mapping = ggplot2::aes(
      xmin = lCi,
      xmax = uCi,
      y    = yPos
    ),
    height = 0
  )




  plotForrest
}
