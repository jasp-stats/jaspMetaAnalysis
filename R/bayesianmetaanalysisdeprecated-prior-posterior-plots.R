# Deprecated Bayesian prior and posterior figures.
#
# Preserves legacy prior and posterior plot construction.

.bmaPriorPlot <- function(jaspResults, dataset, options, ready) {
  priorContainer <- createJaspContainer(title = gettext("Prior"))
  priorContainer$dependOn("priorPlot")
  jaspResults[["priorContainer"]] <- priorContainer
  jaspResults[["priorContainer"]]$position <- 4

  # Create empty plot
  priorPlot <- createJaspPlot(plot = NULL, title = gettext("Effect Size"), width = 450, height = 350)
  priorPlot$position <- 1

  # Custom dependencies (only dependent on prior settings)
  priorPlot$dependOn(c("priorEffectSize",
                       "cauchy", "cauchyLocation", "cauchyScale",
                       "truncationLowerBound", "truncationLowerBoundValue",
                       "truncationUpperBound", "truncationUpperBoundValue",
                       "normal", "normalMean", "normalSd",
                       "t", "tLocation", "tScale","tDf"
  ))

  # Fill plot with effect size prior
  .bmaFillPriorPlot(priorPlot, jaspResults, dataset, options, type = "ES")
  priorContainer[["ES"]] <- priorPlot

  # Make plot hetergeneity prior
  if(options[["model"]] != "fixed"){
    priorPlotSE <- createJaspPlot(plot = NULL, title = gettext("Heterogeneity"), width = 350, height = 350)
    priorPlotSE$dependOn(c("priorStandardError", "inverseGamma",
    "inverseGammaShape", "inverseGammaScale",
                           "halfT", "halfTScale", "halfTDf"))
    priorPlotSE$position <- 2
    .bmaFillPriorPlot(priorPlotSE, jaspResults, dataset, options, type = "SE")
    priorContainer[["SE"]] <- priorPlotSE
  }
}

.bmaFillPriorPlot <- function(priorPlot, jaspResults, dataset, options, type){
  # Get priors from jasp state
  if (is.null(jaspResults[["bmaPriors"]]))
    .bmaPriors(jaspResults, options)
  priors <- jaspResults[["bmaPriors"]]$object

  # Get parameters and x limits
  if(type == "ES"){
    prior <- priors$d
    mean <- attr(prior, "param")[1]
    s <- attr(prior, "param")[2]
    xlimLeft <- mean - (s * 5)
    xlab <- bquote(paste(.(gettext("Effect size")), ~mu))
  } else if(type == "SE"){
    prior <- priors$tau
    mean <- attr(prior, "param")[1]
    s <- attr(prior, "param")[2]
    xlimLeft <- 0
    xlab <- bquote(paste(.(gettext("Heterogeneity")), ~tau))
  }

  if(options$model == "constrainedRandom" && options$constrainedRandomDirection == "positive"){
    xlimLeft <- 0
  } else if(options$model == "constrainedRandom" && options$constrainedRandomDirection == "negative"){
    xlimRight <- 0
  }

  xlimRight <- mean + (s * 5)
  xlimLeft <- xlimLeft - 0.05
  xlimRight <- xlimRight + 0.05

  # Create dataframe for ggplot
  x <- c(xlimLeft, xlimRight)
  df <- data.frame(x = x)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(seq(xlimLeft, xlimRight, 0.5))

  # Plot density function
  plot <- ggplot2::ggplot(df, ggplot2::aes(x)) +
    ggplot2::stat_function(fun = prior, n = 1000, size = 1) +
    ggplot2::labs(x = xlab, y = gettext("Density")) +
    ggplot2::xlim(xlimLeft, xlimRight) +
    ggplot2::scale_x_continuous(breaks = xBreaks)
  plot <- jaspGraphs::themeJasp(plot)
  priorPlot$plotObject <- plot
  return()
}

.bmaPriorAndPosteriorPlot <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  postContainer <- createJaspContainer(title = gettext("Prior and Posteriors"))
  postContainer$dependOn(c(
    .bmaDependencies, "priorPosterior",
    "priorPosteriorCi", "priorPosteriorAdditionalInfo", "priorPosteriorFixedAndRandom"
  ))
  jaspResults[["postContainer"]] <- postContainer
  jaspResults[["postContainer"]]$position <- 5

  # Create empty plot
  postPlotES <- createJaspPlot(plot = NULL, title = gettext("Effect size"), width = 500, height = 350)
  postPlotES$position <- 1



  # Check if ready
  if(!ready){
    return()
  }

  # Fill posterior plot effect size
  .bmaFillPostPlot(postPlotES, jaspResults, dataset, options, type = "ES")
  postContainer[["ES"]] <- postPlotES

  # Make posterior plot heterogeneity
  if(options$model != "fixed"){
    postPlotSE <- createJaspPlot(plot = NULL, title = gettext("Heterogeneity"), width = 500, height = 350)
    postPlotSE$position <- 2
    postContainer[["SE"]] <- postPlotSE
    .bmaFillPostPlot(postPlotSE, jaspResults, dataset, options, type = "SE")
  }
}

.bmaFillPostPlot <- function(postPlot, jaspResults, dataset, options, type){
  # Get results from jasp state
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)

  # Get prior and posterior functions, and 95% CI intervals
  alpha <- 0.2
  postName <- "Posterior"
  valuesCol <- c("black", "black")
  valuesLine <- c("solid", "dotted")

  if(type == "ES"){
    xlab <- bquote(paste(.(gettext("Effect size")), ~mu))
    xlim <- c(-4, 4)
    if(options[["model"]] == "averaging"){
      int <- c(bmaResults[["bma"]]$estimates["averaged", "2.5%"], bmaResults[["bma"]]$estimates["averaged", "97.5%"])
      postName <- "Averaged"
      if(options[["priorPosteriorFixedAndRandom"]]){
        labelsModel <- c(bquote(.(gettext("Fixed H"))[1]),
                         bquote(.(gettext("Random H"))[1]),
                         bquote(.(gettext("Averaged H"))[1]),
                         bquote(.(gettext("Prior H"))[1]))
      } else {
        labelsModel <- c(bquote(.(gettext("Averaged H"))[1]), bquote(.(gettext("Prior H"))[1]))
      }
      yPrior <- bmaResults[["bma"]]$yPrior
      xPost <- bmaResults[["bma"]]$xPost
      yPost <- bmaResults[["bma"]]$yPost
      dfPointsY <- bmaResults[["bma"]]$dfPointsY
    } else if(options[["model"]] == "random"){
      int <- c(bmaResults[["bma"]]$estimates["random", "2.5%"], bmaResults[["bma"]]$estimates["random", "97.5%"])
      postName <- "Random"
      labelsModel <- c(bquote(.(gettext("Random H"))[1]), bquote(.(gettext("Prior H"))[1]))
      yPrior <- bmaResults[["random"]]$yPrior
      xPost <- bmaResults[["random"]]$xPost
      yPost <- bmaResults[["random"]]$yPost
      dfPointsY <- bmaResults[["random"]]$dfPointsY
    } else if(options[["model"]] == "fixed"){
      int <- c(bmaResults[["bma"]]$estimates["fixed", "2.5%"], bmaResults[["bma"]]$estimates["fixed", "97.5%"])
      postName <- "Fixed"
      labelsModel <- c(bquote(.(gettext("Fixed H"))[1]), bquote(.(gettext("Prior H"))[1]))
      yPrior <- bmaResults[["fixed"]]$yPrior
      xPost <- bmaResults[["fixed"]]$xPost
      yPost <- bmaResults[["fixed"]]$yPost
      dfPointsY <- bmaResults[["fixed"]]$dfPointsY
    } else if(options[["model"]] == "constrainedRandom"){
      int <- c(bmaResults[["bma"]]$estimates["ordered", "2.5%"], bmaResults[["bma"]]$estimates["ordered", "97.5%"])
      postName <- "Ordered"
      if(options[["priorPosteriorFixedAndRandom"]]){
        labelsModel <- c(bquote(.(gettext("Fixed H"))[1]),
                         bquote(.(gettext("Ordered H"))[1]),
                         bquote(.(gettext("Random H"))[1]),
                         bquote(.(gettext("Prior H"))[1])
        )
      } else {
        labelsModel <- c(bquote(.(gettext("Ordered H"))[1]),
                         bquote(.(gettext("Prior H"))[1]))
      }
      yPrior <- bmaResults[["ordered"]]$yPrior
      xPost <- bmaResults[["ordered"]]$xPost
      yPost <- bmaResults[["ordered"]]$yPost
      dfPointsY <- bmaResults[["ordered"]]$dfPointsY
    }
    # Heterogeneity priors
  } else if(type == "SE"){
    if(options[["model"]] == "averaging" || options[["model"]] == "random"){
      int <- c(bmaResults[["random"]]$estimates["tau", "2.5%"], bmaResults[["random"]]$estimates["tau", "97.5%"])
      postName <- "Random"
      yPrior <- bmaResults[["random"]]$yPriorTau
      xPost <- bmaResults[["random"]]$xPostTau
      yPost <- bmaResults[["random"]]$yPostTau
      dfPointsY <- data.frame(prior = yPrior[which(xPost == 0)], posterior = yPost[which(xPost == 0)])
    } else if (options[["model"]] == "constrainedRandom"){
      int <- c(bmaResults[["ordered"]]$estimates["tau", "2.5%"], bmaResults[["ordered"]]$estimates["tau", "97.5%"])
      postName <- "Ordered"
      yPrior <- bmaResults[["ordered"]]$yPriorTau
      xPost <- bmaResults[["ordered"]]$xPostTau
      yPost <- bmaResults[["ordered"]]$yPostTau
      dfPointsY <- data.frame(prior = yPrior[which(xPost == 0)], posterior = yPost[which(xPost == 0)])
    }

    if(options[["model"]] == "averaging") valuesCol <- c("#009E73", "black")


    xlab <- bquote(paste(.(gettext("Heterogeneity")), ~tau))
    xlim <- c(0, 3)
    alpha <- 0.3

    if(options[["model"]] == "averaging") labelsModel <- c(bquote(.(gettext("Random H"))[1]), bquote(.(gettext("Prior H"))[1]))
    if(options[["model"]] == "constrainedRandom") labelsModel <- c(bquote(.(gettext("Ordered H"))[1]), bquote(.(gettext("Prior H"))[1]))
    if(options[["model"]] == "fixed") labelsModel <- c(bquote(.(gettext("Fixed H"))[1]), bquote(.(gettext("Prior H"))[1]))
    if(options[["model"]] == "random") labelsModel <- c(bquote(.(gettext("Random H"))[1]), bquote(.(gettext("Prior H"))[1]))
  }

  index <- which(yPost > 0.0001)
  xPost <- xPost[index]
  yPost <- yPost[index]
  yPrior <- yPrior[index]

  df <- data.frame(x = c(xPost, xPost), y = c(yPrior, yPost), g = rep(c("Prior", postName), each = length(xPost)))

  if(options$priorPosteriorFixedAndRandom && (options$model == "averaging" || options$model == "constrainedRandom")){
    if(type == "ES"){
      yPostES <- c(bmaResults[["fixed"]]$yPost, bmaResults[["random"]]$yPost)
      xPostES <- c(bmaResults[["fixed"]]$xPost, bmaResults[["random"]]$xPost)
      gPostES <- c(rep("Fixed", length(bmaResults[["fixed"]]$xPost)), rep("Random", length(bmaResults[["random"]]$xPost)))
      dfPost <- data.frame(x = xPostES,  y = yPostES, g = gPostES)
      if(options[["model"]] == "averaging"){
        valuesCol <- c("#fcae91ff", "#009E73", "black", "black")
      } else if(options[["model"]] == "constrainedRandom"){
        valuesCol <- c("#fcae91ff", "black", "#009E73", "black")
      }
      valuesLine <- c("solid", "solid", "solid", "dotted")
    } else if(type == "SE"){
      yPostSE <- bmaResults[["random"]]$yPostTau
      xPostSE <- bmaResults[["random"]]$xPostTau
      gPostSE <- rep("Random", length(bmaResults[["random"]]$xPostTau))
      dfPost <- data.frame(x = xPostSE,  y = yPostSE, g = gPostSE)
      if(options[["model"]] == "constrainedRandom"){
        valuesCol <- c("black", "#009E73", "black")
        valuesLine <- c("solid", "solid", "dotted")
        labelsModel <- c(bquote(.(gettext("Ordered H"))[1]),
                         bquote(.(gettext("Random H"))[1]),
                         bquote(.(gettext("Prior H"))[1]))
      }
    }
    df <- rbind(df, dfPost)
  }

  if(!options$priorPosteriorFixedAndRandom || options$model == "random" || options$model == "fixed"){
    df$g <- factor(df$g, levels = c(postName, "Prior"))
  } else if(options$priorPosteriorFixedAndRandom){
    if(type == "ES"){
      if(options$model == "averaging") df$g <- factor(df$g, levels = c("Fixed", "Random", "Averaged", "Prior"))
      if(options$model == "constrainedRandom") df$g <- factor(df$g, levels = c("Fixed", "Ordered", "Random", "Prior"))
    } else if(type == "SE"){
      if(options$model == "averaging") df$g <- factor(df$g, levels = c("Random", "Prior"))
      if(options$model == "constrainedRandom") df$g <- factor(df$g, levels = c("Ordered", "Random", "Prior"))
    }
  }

  if(type == "ES"){
    if(options$model == "fixed") BF <- bmaResults[["bf"]]$fixedBF["fixed_H1", "fixed_H0"]
    if(options$model == "random") BF <- bmaResults[["bf"]]$randomBF["random_H1", "random_H0"]
    if(options$model == "averaging") BF <- bmaResults[["bf"]]$inclusionBF
    if(options$model == "constrainedRandom") BF <- bmaResults[["bf"]]$BF["ordered", "null"]

    if(options$model == "fixed") CRI <- bmaResults[["bma"]]$estimates["fixed", c("2.5%", "97.5%")]
    if(options$model == "random") CRI <- bmaResults[["bma"]]$estimates["random", c("2.5%", "97.5%")]
    if(options$model == "averaging") CRI <- bmaResults[["bma"]]$estimates["averaged", c("2.5%", "97.5%")]
    if(options$model == "constrainedRandom") CRI <- bmaResults[["ordered"]]$estimates["average_effect", c("2.5%", "97.5%")]

    if(options$model == "fixed") med <- bmaResults[["bma"]]$estimates["fixed", "mean"]
    if(options$model == "random") med <- bmaResults[["bma"]]$estimates["random", "mean"]
    if(options$model == "averaging") med <- bmaResults[["bma"]]$estimates["averaged", "mean"]
    if(options$model == "constrainedRandom") med <- bmaResults[["ordered"]]$estimates["average_effect", "mean"]

  } else if (type == "SE"){
    if(options$model == "random") BF <- bmaResults[["bf"]]$BF["random_H1", "fixed_H1"]
    if(options$model == "averaging") BF <- bmaResults[["bf"]]$BF["random_H1", "fixed_H1"]
    if(options$model == "constrainedRandom") BF <- bmaResults[["bf"]]$BF["ordered", "fixed"]

    if(options$model == "random") CRI <- bmaResults[["random"]]$estimates["tau", c("2.5%", "97.5%")]
    if(options$model == "averaging") CRI <- bmaResults[["random"]]$estimates["tau", c("2.5%", "97.5%")]
    if(options$model == "constrainedRandom") CRI <- bmaResults[["ordered"]]$estimates["tau", c("2.5%", "97.5%")]


    if(options$model == "random" || options$model == "averaging")  med <- bmaResults[["random"]]$estimates["tau", "mean"]
    if(options$model == "constrainedRandom") med <- bmaResults[["ordered"]]$estimates["tau", "mean"]
  }



  if(!options[["priorPosteriorAdditionalInfo"]]){
    BF <- NULL
    CRI <- NULL
    bfType <- NULL
    med <- NULL
  } else {
    if(options[["bayesFactorType"]] == "BF01") {
      BF    <- 1/BF
      bfType <- "BF01"
    } else if(options[["bayesFactorType"]] == "LogBF10") {
      BF <- log(BF)
      bfType <- "LogBF10"
    } else {
      bfType <- "BF10"
    }
  }

  if(options[["priorPosteriorAdditionalInfo"]]){
    BF <- round(BF, 3)
    CRI <- round(CRI, 3)
    med <- round(med, 3)
  }

  pizzaTxt <- c("data | Hf1",
                "data | Hr1")
  bfSubscripts <-  c("BF[italic(r1f1)]", "BF[italic(f1r1)]")

  if(options$model == "constrainedRandom"){
    pizzaTxt <- c("data | Hf1", "data | Ho1")
    bfSubscripts <-  c("BF[italic(o1f1)]", "BF[italic(f1o1)]")
  }

  xr   <- range(df$x)
  idx  <- which.max(df$y)
  xmax <- df$x[idx]
  if (xmax > mean(xr)) {
    legend.position = c(0.2, 0.875)
  } else {
    legend.position = c(0.80, 0.875)
  }

  if(type == "ES"){
    plot <- jaspGraphs::PlotPriorAndPosterior(dfLines = df,
                                              lineColors = valuesCol,
                                              BF = BF,
                                              CRI = CRI,
                                              bfType = bfType,
                                              xName = xlab,
                                              median = med,
                                              medianTxt = "Mean:")
  } else if(type == "SE"){
    plot <- jaspGraphs::PlotPriorAndPosterior(dfLines = df,
                                              lineColors = valuesCol,
                                              BF = BF,
                                              CRI = CRI,
                                              bfType = bfType,
                                              xName = xlab,
                                              bfSubscripts = bfSubscripts,
                                              pizzaTxt = pizzaTxt,
                                              median = med,
                                              medianTxt = "Mean:")
  }

  .extraPost <- function(plot, int, xPost, yPost){


    if(options[["priorPosteriorCi"]]){
      shadeData <- data.frame(x = xPost[xPost < max(int) & xPost > min(int)], y = yPost[xPost < max(int) & xPost > min(int)])
      plot <- plot + ggplot2::geom_area(data = shadeData, mapping = ggplot2::aes(x = x, y = y), fill = "grey", group = 1, linetype = 1, color = NA, alpha = 0.5)
    }

    if(options[["priorPosteriorFixedAndRandom"]] && options[["model"]] == "averaging"){
      plot <- plot + ggplot2::scale_linetype_manual(values = valuesLine)
    }

    plot <- plot +
      ggplot2::scale_linetype_manual("", values = valuesLine, labels = labelsModel) +
      ggplot2::scale_color_manual("", values = valuesCol, labels = labelsModel) +
      ggplot2::theme(legend.text.align = 0,
                     legend.position = legend.position)
    return(plot)
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, xPost))

  if(options[["priorPosteriorAdditionalInfo"]]){
    plot$subplots$mainGraph <- plot$subplots$mainGraph + ggplot2::scale_x_continuous(name = xlab, breaks = xBreaks, limits = c(min(xPost), max(xPost)))
    plot$subplots$mainGraph <- .extraPost(plot$subplots$mainGraph, int, xPost, yPost)
  } else {
    plot <- plot + ggplot2::scale_x_continuous(name = xlab, breaks = xBreaks, limits = c(min(xPost), max(xPost)))
    plot <- .extraPost(plot, int, xPost, yPost)
  }

  postPlot$plotObject <- plot
  return()
}
