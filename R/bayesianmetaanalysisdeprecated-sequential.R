# Deprecated Bayesian sequential figures.
#
# Preserves legacy sequential Bayes-factor and posterior-model figures.

.bmaSequentialPlot <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  # Create empty plot
  seqContainer <- createJaspContainer(title = gettext("Sequential Analysis"))
  seqContainer$dependOn(.bmaDependencies)
  jaspResults[["seqContainer"]] <- seqContainer
  jaspResults[["seqContainer"]]$position <- 6

  # Check if ready
  if(!ready){
    return()
  }

  # Fill sequential plot BFs effect size
  if(options$bfSequentialPlot){
    seqPlotES <- createJaspPlot(plot = NULL, title = gettext("Bayes factors effect size"), height = 400, width = 580)
    seqPlotES$dependOn(c("bfSequentialPlot", "BF"))
    seqPlotES$position <- 1
    seqContainer[["seqPlotES"]] <- seqPlotES
    .bmaFillSeqPlot(seqPlotES, jaspResults, dataset, options, .bmaDependencies, type = "ES")
    # Fill sequential plot BFs standard error
    if(!options$model == "fixed"){
      seqPlotSE <- createJaspPlot(plot = NULL, title = gettext("Bayes factors heterogeneity"), height = 400, width = 580)
      seqPlotSE$dependOn(c("bfSequentialPlot", "BF"))
      seqPlotSE$position <- 2
      seqContainer[["seqPlotSE"]] <- seqPlotSE
      .bmaFillSeqPlot(seqPlotSE, jaspResults, dataset, options, .bmaDependencies, type = "SE")
    }
  }

  if(options$modelProbabilitySequentialPlot){
    seqPMPlot <- createJaspPlot(plot = NULL, title = gettext("Posterior model probabilities"), height = 400, width = 580)
    seqPMPlot$dependOn("modelProbabilitySequentialPlot")
    seqPMPlot$position <- 3
    .bmaFillSeqPM(seqPMPlot, jaspResults, dataset, options, .bmaDependencies)
    seqContainer[["seqPMPlot"]] <- seqPMPlot
  }

}

.bmaFillSeqPlot <- function(seqPlot, jaspResults, dataset, options, .bmaDependencies, type){

  evidenceR <- gettext("Evidence for random effects")
  evidenceF <- gettext("Evidence for fixed effects")

  rowResults <- .bmaSequentialResults(jaspResults, dataset, options, .bmaDependencies)
  if(type == "ES"){
    BFs <- rowResults$BFs
  } else if(type == "SE"){
    # The BFs for heterogeneity have different labels
    BFs <- rowResults$BFsHeterogeneity
    if(options$model == "averaging"){
      yName         <- "BF[italic(rf)]"
      pizzaTxt      <- c("data | Hf", "data | Hr")
      bfSubscripts  <-  c("BF[italic(rf)]", "BF[italic(fr)]")
    } else if(options$model == "random"){
      yName         <- "BF[italic(r1f1)]"
      pizzaTxt      <- c("data | Hf1", "data | Hr1")
      bfSubscripts  <-  c("BF[italic(r1f1)]", "BF[italic(f1r1)]")
    }
    arrowLabel <- c(evidenceF, evidenceR)
    BF <- BFs[length(BFs)]
    if(BF >= 1){
      modelEvidence <- gettext("random")
    } else {
      modelEvidence <- gettext("fixed")
    }
    allEvidenceLabels <- c(gettext("Anecdotal",domain="R-jaspGraphs"),
                           gettext("Moderate",domain="R-jaspGraphs"),
                           gettext("Strong",domain="R-jaspGraphs"),
                           gettext("Very Strong",domain="R-jaspGraphs"),
                           gettext("Extreme",domain="R-jaspGraphs"))
    if(BF < 1) BF     <- 1/BF
    idx               <- findInterval(BF, c(1, 3, 10, 30, 100), rightmost.closed = FALSE)
    evidenceLevel     <- jaspGraphs:::fixTranslationForExpression(allEvidenceLabels[idx])

    evidenceFor <- gettextf("Evidence for %s:", modelEvidence, domain="R-jaspGraphs")
    evidenceFor <- jaspGraphs:::fixTranslationForExpression(evidenceFor)
    evidenceTxt <- jaspGraphs:::parseThis(c(evidenceLevel, evidenceFor))
  }

  BFs[1] <- 1
  bfType <- "BF10"

  if(options$bayesFactorType == "BF01") {
    BFs    <- 1/BFs
    bfType <- "BF01"
    if(options$model == "averaging") yName <- "BF[italic(fr)]"
    if(options$model == "random")  yName <- "BF[italic(f1r1)]"
  }

  # The BFs for constrained random effects also have different labels
  if(options$model == "constrainedRandom"){
    pizzaTxt <- c("data | Hf1", "data | Ho1")
    bfSubscripts <-  c("BF[italic(o1f1)]", "BF[italic(f1o1)]")
    if(type == "SE") yName <- "BF[italic(o1f1)]"
    if(type == "SE" && options$bayesFactorType == "BF01") yName <- "BF[italic(f1o1)]"
  }

  if(any(is.infinite(BFs))){
    seqPlot$setError(gettext("Plotting failed: The Bayes factors contain infinity."))
    return()
  }

  df <- data.frame(x = 1:nrow(dataset), y = log(BFs))

  if(type == "ES"){
    plot <- jaspGraphs::PlotRobustnessSequential(dfLines = df,
                                                 xName = "Studies",
                                                 BF = BFs[nrow(dataset)],
                                                 bfType = bfType,
                                                 hasRightAxis = FALSE)
  } else if(type == "SE"){
    plot <- jaspGraphs::PlotRobustnessSequential(dfLines = df,
                                                 xName = "Studies",
                                                 BF = BFs[nrow(dataset)],
                                                 bfType = bfType,
                                                 bfSubscripts = bfSubscripts,
                                                 pizzaTxt = pizzaTxt,
                                                 hasRightAxis = FALSE,
                                                 yName = yName,
                                                 evidenceTxt  = evidenceTxt,
                                                 arrowLabel  = arrowLabel
    )
  }


  seqPlot$plotObject <- plot
  return()
}

.bmaFillSeqPM <- function(seqPMPlot, jaspResults, dataset, options, .bmaDependencies){
  n     <- nrow(dataset)
  x     <- 0:n
  x     <- x[-2]
  dfPMP <- data.frame(prob = 0, g = rep(c("FE0", "FE1", "RE0", "RE1"), each = n))
  bmaResults     <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)
  pM    <- bmaResults[["models"]]$prior

  dfPMP[c(1, 1 + n, 1 + 2*n, 1 + 3*n), 1] <- pM

  rowResults <- .bmaSequentialResults(jaspResults, dataset, options, .bmaDependencies)

  for(i in 2:nrow(dataset)){
    posterior_models <- rowResults$posterior_models[[i]]
    dfPMP[c(i, i + n, i + 2*n, i + 3*n), 1] <- posterior_models
  }

  if(options[["model"]] == "averaging" || options[["model"]] == "constrainedRandom"){

    labels <- c(bquote(.(gettext("Fixed H"))[0]),bquote(.(gettext("Fixed H"))[1]),
                bquote(.(gettext("Random H"))[0]), bquote(.(gettext("Random H"))[1]))
    colorValues <- c("#fcae91ff", "#fcae91ff", "#009E73", "#009E73")
    linetypeValues <- rep("solid", 4)
    pointValues <- c(21, 19, 21, 19)
    lineValues <- c("dotted", "solid", "dotted", "solid")

  } else if(options[["model"]] == "fixed"){
    labels <- c(bquote(.(gettext("Fixed H"))[0]), bquote(.(gettext("Fixed H"))[1]))
    colorValues <- c("#fcae91ff", "#fcae91ff")
    linetypeValues <- rep("solid", 2)
    pointValues <- c(21, 19)
    lineValues <- c("dotted", "solid")
    dfPMP <- subset(dfPMP, dfPMP$g == "FE0" | dfPMP$g == "FE1")

  } else if(options[["model"]] == "random"){

    labels <- c(bquote(.(gettext("Random H"))[0]), bquote(.(gettext("Random H"))[1]))
    colorValues <- c("#009E73", "#009E73")
    linetypeValues <- rep("solid", 2)
    pointValues <- c(21, 19)
    lineValues <- c("dotted", "solid")
    dfPMP <- subset(dfPMP, dfPMP$g == "RE0" | dfPMP$g == "RE1")

  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(x)


  gridLines <- ggplot2::geom_segment(
    data        = data.frame(x = xBreaks[1L], y = c(0, 0.25, 0.5, 0.75, 1), xend = xBreaks[length(xBreaks)]),
    mapping     = ggplot2::aes(x = x, y = y, xend = xend, yend = y),
    inherit.aes = FALSE,
    colour      = rep("gray", 5),
    linetype    = rep("dashed", 5),
    size        = 0.85)

  df <- data.frame(x = x, y = dfPMP$prob, g = dfPMP$g)
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = g, linetype = g)) +
    gridLines +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::scale_y_continuous(limits = c(0,1.05), breaks = c(0, .25, .5, .75, 1)) +
    ggplot2::scale_x_continuous(breaks = xBreaks) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::theme(legend.spacing.x = ggplot2::unit(0.35, 'cm')) +
    ggplot2::labs(x = gettext("Studies"), y = gettext("Posterior model \n probability")) +
    ggplot2::scale_colour_manual(name = "",
                                 labels = labels,
                                 values = colorValues) +
    ggplot2::scale_linetype_manual(name = "",
                                   labels = labels,
                                   values = linetypeValues)



  if(nrow(dataset) < 40) {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(shape = dfPMP$g), size = 3, fill = "white") +
      ggplot2::scale_shape_manual(name = "", values = pointValues, labels = labels)
  } else {
    plot <- plot +
      ggplot2::scale_linetype_manual(name = "", values = lineValues, labels = labels)
  }

  plot <- jaspGraphs::themeJasp(plot, legend.position = "top")


  seqPMPlot$plotObject <- plot
  return()
}
