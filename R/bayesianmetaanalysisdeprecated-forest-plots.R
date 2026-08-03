# Deprecated Bayesian forest plots.
#
# Preserves legacy forest and cumulative-forest plot construction.

.bmaForestPlot <- function(jaspResults, dataset, options, ready, .bmaDependencies) {
  forestContainer <- createJaspContainer(title = gettext("Forest Plot"))
  forestContainer$dependOn(c(.bmaDependencies, "studyLabel"))
  forestContainer$position <- 6
  jaspResults[["forestContainer"]] <- forestContainer

  # Get studylabels
  if(options[["studyLabel"]] != ""){
    studyLabels <- as.character(dataset[, options[["studyLabel"]]])
  } else {
    studyLabels <- paste(gettext("Study"), 1:nrow(dataset))
  }

  # Check if ready
  if(!ready){
    return()
  }

  # Scale the height and width of the plot
  heightCumulative <- 100 + nrow(dataset) * 30
  width  <- 500 + (nchar(max(studyLabels)) * 5)

  # Create empty plot
  if(is.null(forestContainer[["forestPlot"]]) && options$forestPlot) {

    # title and height of plot based on observed/estimated
    if(options$forestPlotEffect == "observed"){
      title  <- gettext("Observed study effects")
      height <- 100 + nrow(dataset) * 30
    } else if(options$forestPlotEffect == "estimated"){
      title  <- gettext("Estimated study effects")
      height <- 100 + nrow(dataset) * 30
    } else if(options$forestPlotEffect == "both"){
      title  <- gettext("Observed and estimated study effects")
      height <- 150 + 2 * (nrow(dataset) * 30)
    }

    forestPlot <- createJaspPlot(plot = NULL, title = title, height = height, width = width)

    # Fill plot
    forestPlot$dependOn(c("forestPlotEffect", "forestPlot",
                          "forestPlotRowOrder", "forestPlotOrder", "forestPlotLabel"))
    forestPlot$position <- 1
    .bmaFillForestPlot(forestPlot, jaspResults, dataset, options, studyLabels, showLabels = if (!is.null(options[["forestPlotLabel"]])) options[["forestPlotLabel"]] else TRUE)
    # Add plot to container
    forestContainer[["forestPlotEffect"]] <- forestPlot
  }

  if(is.null(forestContainer[["cumForestPlot"]]) && options$cumulativeForestPlot){
    cumForestPlot <- createJaspPlot(plot = NULL, title = gettext("Cumulative forest plot"), height = heightCumulative, width = width)
    cumForestPlot$dependOn(c("cumulativeForestPlot", "cumulativeForestPlotPrior"))
    cumForestPlot$position <- 2
    .bmaFillCumForest(cumForestPlot, jaspResults, dataset, options, studyLabels, .bmaDependencies)
    forestContainer[["cumForestPlot"]] <- cumForestPlot
  }
}

.bmaFillForestPlot <- function(forestPlot, jaspResults, dataset, options, studyLabels, showLabels = TRUE){
  # Get analysis results from jasp state
  bmaResults <- .bmaResultsState(jaspResults, dataset, options, .bmaDependencies)

  # Create effect size and standard error variable and make dataframe
  varES <- dataset[, options[["effectSize"]]]

  if(all(unlist(options[["effectSizeCi"]]) != "") && !is.null(unlist(options[["effectSizeCi"]]))){
    lower <- dataset[, options[["effectSizeCi"]][[1]][[1]]]
    upper <- dataset[, options[["effectSizeCi"]][[1]][[2]]]
    varSE <- (upper - lower)/2/qnorm(0.975)
  }
  if(options[["effectSizeSe"]] != ""){
    varSE <- dataset[, options[["effectSizeSe"]]]
  }

  # Assign weights for the observed point sizes
  weight <- 1/varSE^2
  weight_scaled <- ((4 - 1)*(weight - min(weight))) / (max(weight) - min(weight)) + 2

  # Assign weights for the estimated point sizes
  # Should be different for ordered analysis
  if(options[["model"]] == "constrainedRandom"){
    se_estimated <- bmaResults[["ordered"]]$summary[3:(length(varES) + 2), "se_mean"]
  } else {
    se_estimated <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "se_mean"]
  }

  weight_estimated <- 1 / se_estimated^2
  weight_estimated_scaled <- ((4 - 1) * (weight_estimated - min(weight_estimated))) / (
    max(weight_estimated) - min(weight_estimated)) + 2

  # Create text object for next to the observed points
  ci <- .95
  lower <- varES - qnorm((ci+1)/2) * varSE
  upper <- varES + qnorm((ci+1)/2) * varSE

  text_observed <- paste(sprintf('%.2f', varES),
                         " [",
                         sprintf('%.2f', lower),
                         ", ",
                         sprintf('%.2f', upper),
                         "]",
                         sep = "")

  # Get estimated points and CI's
  if(options$model == "averaging" || options$model == "random" || options$model == "constrainedRandom"){
    mean_estimates <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "mean"]
    lower_estimates <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "2.5%"]
    upper_estimates <- bmaResults[["random"]]$summary[3:(length(varES) + 2), "97.5%"]
  }
  # The estimates for the ordered analysis are not always saved
  if(options$model == "constrainedRandom"){
    mean_estimates <- bmaResults[["ordered"]]$summary[1:length(varES) + 2, "mean"]
    lower_estimates <- bmaResults[["ordered"]]$summary[1:length(varES) + 2, "2.5%"]
    upper_estimates <- bmaResults[["ordered"]]$summary[1:length(varES) + 2, "97.5%"]
  }

  # Create text object for estimated points
  if(options$model != "fixed"){
    text_estimated <- paste(sprintf('%.2f', mean_estimates),
                            " [",
                            sprintf('%.2f', lower_estimates),
                            ", ",
                            sprintf('%.2f', upper_estimates),
                            "]",
                            sep = "")
  }

  # Make index for model diamond
  modelIndex <- .bmaGetModelName(options)


  yDiamond <- -0.5

  if (options$model == "averaging" || options$model == "constrainedRandom") {
    if (options$forestPlotEffect == "both")
      yDiamond <- c(-0.5, -1.1, -1.7)
    else
      yDiamond <- c(-0.5, -1.5, -2.5)
  }

  # Create diamond for averaged or ordered model
  meanMain <- bmaResults[["bma"]]$estimates[modelIndex, "mean"]
  lowerMain <- bmaResults[["bma"]]$estimates[modelIndex, "2.5%"]
  upperMain <- bmaResults[["bma"]]$estimates[modelIndex, "97.5%"]
  if(modelIndex == "ordered"){
    yMain <- yDiamond[2]
  } else if(options$model == "averaging"){
    yMain <- yDiamond[3]
  } else yMain <- -0.5

  d <- data.frame(x = c(lowerMain, meanMain,
                        upperMain, meanMain),
                  y = c(yMain, yMain + 0.25,
                        yMain, yMain - 0.25))

  # Text object for next to model diamond
  textMain <- paste0(sprintf('%.2f', meanMain), " [",
                     sprintf('%.2f', lowerMain), ", ",
                     sprintf('%.2f', upperMain), "]")


  # Create diamond for fixed model
  meanFixed <- bmaResults[["bma"]]$estimates["fixed", "mean"]
  lowerFixed <- bmaResults[["bma"]]$estimates["fixed", "2.5%"]
  upperFixed <- bmaResults[["bma"]]$estimates["fixed", "97.5%"]
  yFixed <- yDiamond[1]

  d.fixed <- data.frame(x = c(lowerFixed, meanFixed,
                              upperFixed, meanFixed),
                        y = c(yFixed, yFixed + 0.25,
                              yFixed, yFixed - 0.25))

  text_fixed <- paste0(sprintf('%.2f', meanFixed), " [",
                       sprintf('%.2f', lowerFixed), ", ",
                       sprintf('%.2f', upperFixed), "]")

  # Create diamond for random model
  meanRandom <- bmaResults[["bma"]]$estimates["random", "mean"]
  lowerRandom <- bmaResults[["bma"]]$estimates["random", "2.5%"]
  upperRandom <- bmaResults[["bma"]]$estimates["random", "97.5%"]
  if(options$model == "random"){
    yRandom <- -0.5
  } else if(options$model == "averaging"){
    yRandom <- yDiamond[2]
  } else if(options$model == "constrainedRandom"){
    yRandom <- yDiamond[3]
  } else yRandom <- 0

  d.random <- data.frame(x = c(lowerRandom, meanRandom,
                               upperRandom, meanRandom),
                         y = c(yRandom, yRandom + 0.25,
                               yRandom, yRandom - 0.25))

  text_random <- paste0(sprintf('%.2f', meanRandom), " [",
                        sprintf('%.2f', lowerRandom), ", ",
                        sprintf('%.2f', upperRandom), "]")

  # Get y coordinates, labels, and text for diamonds
  if(options$model == "averaging"){
    model <- c(gettext("Fixed effects"), gettext("Random effects"), gettext("Averaged"))
    textDiamond <- c(text_fixed, text_random, textMain)
  } else if(options$model == "random"){
    model <- gettext("Random effects")
    textDiamond <- text_random
  } else if(options$model == "fixed"){
    model <- gettext("Fixed effects")
    textDiamond <- text_fixed
  } else if(options$model == "constrainedRandom"){
    model <- c(gettext("Fixed effects"), gettext("Ordered effects"), gettext("Random effects"))
    textDiamond <- c(text_fixed, textMain, text_random)
  }

  # Shape if only observed points
  shape <- 15

  df <- data.frame(effectSize = varES, y = length(varES):1,
                   studyLabels = studyLabels,
                   weight_scaled = weight_scaled,
                   lower = lower, upper = upper,
                   text = text_observed)

  # Change objects if only estimated points
  if(options$forestPlotEffect == "estimated"){
    df <- data.frame(effectSize = mean_estimates, y = length(varES):1,
                     studyLabels = studyLabels,
                     weight_scaled = weight_estimated_scaled,
                     lower = lower_estimates, upper = upper_estimates,
                     text = text_estimated)
    shape <- 19
  }

  # Get y values for the estimated points
  yEst <- rev(seq(.6, length(varES) - .4, 1))

  if (options[["analysis"]] == "bmaDeprecated"){

    ranked <- rank(df$effectSize, ties.method="first")
    if(options$forestPlotRowOrder == "ascending"){
      ord <- (length(varES) + 1) - ranked
      df$y <- ord
      yEst <- yEst[ranked]
    }

    if(options$forestPlotRowOrder == "descending"){
      ord <- ranked
      df$y <- ord
      yEst <- yEst[(length(varES) + 1) - ranked]
    }

  } else {

    if(options[["forestPlotOrder"]] == "yearAscending"){
      ranked <- rank(dataset[,"studyYear"], ties.method="first")
      ord    <- (length(varES) + 1) - ranked
      df$y   <- ord
      yEst   <- yEst[ranked]
    } else if(options[["forestPlotOrder"]] == "yearDescending"){
      ranked <- rank(dataset[,"studyYear"], ties.method="first")
      ord    <- ranked
      df$y   <- ord
      yEst   <- yEst[(length(varES) + 1) - ranked]
    } else if(options[["forestPlotOrder"]] == "effectSizeAscending"){
      ranked <- rank(dataset[,"effectSize"], ties.method="first")
      ord    <- (length(varES) + 1) - ranked
      df$y   <- ord
      yEst   <- yEst[ranked]
    } else if(options[["forestPlotOrder"]] == "effectSizeDescending"){
      ranked <- rank(dataset[,"effectSize"], ties.method="first")
      ord    <- ranked
      df$y   <- ord
      yEst   <- yEst[(length(varES) + 1) - ranked]
    }

  }

  # a sneaky way of coloring user-added estimates for Cochrane
  df$color        <- ifelse(grepl("_add", df$studyLabels), "blue", "black")
  df$studyLabels  <- gsub("_add", "", df$studyLabels)

  if (!showLabels) {
    df$studyLabels   <- ""
    df$text          <- ""
  }

  # Create plot
  plot <-  ggplot2::ggplot(df, ggplot2::aes(x = effectSize, y = y)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = df$lower, xmax = df$upper), colour = df$color, height = .2) +
    ggplot2::geom_point(shape = shape, size = df$weight_scaled, colour = df$color) +
    ggplot2::scale_y_continuous(breaks = c(df$y, yDiamond), labels = c(as.character(df$studyLabels), model),
                                sec.axis = ggplot2::sec_axis(~ ., breaks = c(df$y, yDiamond), labels = c(as.character(df$text), textDiamond)), expand = c(0, 0.5))

  if(options$forestPlotEffect == "both"){
    dfBoth <- data.frame(effectSize = c(varES, mean_estimates),
                         y = c(df$y, yEst),
                         studyLabels = c(studyLabels, studyLabels),
                         weight_scaled = c(weight_scaled, weight_estimated_scaled),
                         lower = c(lower, lower_estimates), upper = c(upper, upper_estimates),
                         text = c(text_observed, text_estimated),
                         g = rep(c("Observed", "Estimated"), each = length(varES)))

    plot <-  ggplot2::ggplot(dfBoth, ggplot2::aes(x = effectSize, y = y)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
      ggplot2::geom_point(ggplot2::aes(shape = as.factor(dfBoth$g), colour = as.factor(dfBoth$g)), size = dfBoth$weight_scaled) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = dfBoth$lower, xmax = dfBoth$upper, colour = as.factor(dfBoth$g)), height = .1, show.legend = FALSE) +
      ggplot2::scale_y_continuous(breaks = c(df$y, yDiamond), labels = c(as.character(df$studyLabels), model),
                                  sec.axis = ggplot2::sec_axis(~ ., breaks = c(df$y, yEst, yDiamond), labels = c(text_observed, text_estimated, textDiamond)), expand = c(0, 0.5)) +
      ggplot2::scale_color_manual("", values = c("slategrey", "black"), labels = c(gettext("Estimated"), gettext("Observed"))) +
      ggplot2::scale_shape_manual("", values = c(16, 15)) +
      ggplot2::guides(shape = ggplot2::guide_legend(reverse=TRUE, override.aes = list(size=3)), colour = ggplot2::guide_legend(reverse=TRUE)) +
      ggplot2::theme(axis.text.y.right = ggplot2::element_text(colour = c(rep(c("black", "slategrey"), each = nrow(df)), rep("black", 3))))
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(range(df$lower, df$upper))
  plot <- plot +
    ggplot2::scale_x_continuous(
      name = bquote(paste(.(gettext("Effect size")), ~mu)),
      breaks = xBreaks,
      limits = range(xBreaks))

  plot <- jaspGraphs::themeJasp(plot, yAxis = FALSE)

  # Add other theme elements (no y axis and aligning y axis labels)
  plot <- plot + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                axis.line.y = ggplot2::element_blank(),
                                axis.ticks.y = ggplot2::element_blank(),
                                axis.text.y = ggplot2::element_text(hjust = 0),
                                axis.text.y.right = ggplot2::element_text(hjust = 1))

  if(options$forestPlotEffect == "both"){
    plot <- plot + ggplot2::theme(
      legend.position = c(1, 1),
      legend.justification=c(0, 0),
      plot.margin = ggplot2::unit(c(5, 1, 0.5, 0.5), "lines"),
      legend.title = ggplot2::element_blank()
    )
  }
  # Add the model diamond
  plot <- plot + ggplot2::geom_polygon(data = d, ggplot2::aes(x = x, y = y))

  # Add the diamonds of the other models for averaging or ordered analysis
  if(options$model == "averaging" || options$model == "constrainedRandom"){
    plot <- plot + ggplot2::geom_polygon(data = d.fixed, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_polygon(data = d.random, ggplot2::aes(x = x, y = y))
  }

  forestPlot$plotObject <- plot
  return()
}

.bmaFillCumForest <- function(cumForestPlot, jaspResults, dataset, options, studyLabels, .bmaDependencies){

  rowResults <- .bmaSequentialResults(jaspResults, dataset, options, .bmaDependencies)

  meanMain   <- rowResults$mean
  if(round(meanMain[1], 2) == 0.00) meanMain[1] <- 0.00
  lowerMain  <- rowResults$lowerMain
  upperMain  <- rowResults$upperMain

  text <- paste(sprintf('%.2f', meanMain),
                " [",
                sprintf('%.2f', lowerMain),
                ", ",
                sprintf('%.2f', upperMain),
                "]",
                sep = "")

  studyLabels[2] <- paste(studyLabels[1], "\n &", studyLabels[2])
  studyLabels    <- paste("+", studyLabels)
  studyLabels[1] <- gettext("Prior")

  df <- data.frame(effectSize = meanMain, studyLabels = studyLabels, y = length(meanMain):1)

  if(!options$cumulativeForestPlotPrior) {
    idx <- which(df$studyLabels == "Prior")
    df <- df[-idx, ]
    text <- text[-idx]
    lowerMain  <- lowerMain[-idx]
    upperMain  <- upperMain[-idx]
  }

  plot <-  ggplot2::ggplot(df, ggplot2::aes(x = effectSize, y = y))+
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted")+
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lowerMain, xmax = upperMain), height = .2) +
    ggplot2::geom_point(shape = 16, size = 4) +
    ggplot2::xlab(bquote(paste(.(gettext("Overall effect size")), ~mu))) +
    ggplot2::scale_y_continuous(breaks = df$y, labels = df$studyLabels, expand = c(0, 0.5),
                                sec.axis = ggplot2::sec_axis(~ ., breaks = df$y, labels = text))

  plot <- jaspGraphs::themeJasp(plot, yAxis = FALSE)

  # Add other theme elements (no y axis and aligning y axis labels)
  plot <- plot + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                axis.line.y = ggplot2::element_blank(),
                                axis.ticks.y = ggplot2::element_blank(),
                                axis.text.y = ggplot2::element_text(hjust = 0),
                                axis.text.y.right = ggplot2::element_text(hjust = 1))

  cumForestPlot$plotObject <- plot
  return()
}
