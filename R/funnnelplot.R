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


FunnelPlot <- function(jaspResults, dataset = NULL, options, ...) {

  saveRDS(options, file = "C:/JASP/options.RDS")
  saveRDS(dataset, file = "C:/JASP/dataset.RDS")

  if (options[["effectSize"]] == "" || options[["effectSizeStandardError"]] == "")
    return()

  if (FALSE) {
    options <- readRDS(file = "C:/JASP/options.RDS")
    dataset <- readRDS(file = "C:/JASP/dataset.RDS")
  }

  # create plot
  funnelPlot <- createJaspPlot(title = gettext("Funnel Plot"), width = 550, height = 480)
  funnelPlot$dependOn(c(
    "effectSize", "effectSizeStandardError",
    "funnelUnderH0", "funnelUnderH0ParametersFixedMu", "funnelUnderH0ParametersFixedTau",
    "funnelUnderH1", "funnelUnderH1Parameters", "funnelUnderH1ParametersFixedMu", "funnelUnderH1ParametersFixedTau", "funnelUnderH1IncludeHeterogeneity",
    "funnelPredictionInterval", "funnelUnderH0LineType", "funnelUnderH0FillColors", "funnelUnderH1LineType", "funnelUnderH1FillColors",
    "invertColors"
  ))
  funnelPlot$position <- 1
  jaspResults[["funnelPlot"]] <- funnelPlot


  # extract the funnel levels
  if (options[["funnelUnderH0"]] || options[["funnelUnderH1"]]) {
    funnelLevels <- .parseRCodeInOptions(options[["funnelPredictionInterval"]])
    if (any(funnelLevels < 0 | funnelLevels > 1) || any(is.na(funnelLevels)))
      .quitAnalysis(gettext("Funnel plot prediction intervals must be between 0 and 1."))
    if (length(funnelLevels) < 1)
      .quitAnalysis(gettext("Funnel plot prediction intervals must be specified."))
    funnelLevels <- (1-funnelLevels)/2
    funnelLevels <- sort(funnelLevels)

    # funnel colors
    funnelColorsSteps <- 2*length(funnelLevels) + 1
    funnelColorsSteps <- seq(0, 1, length.out = funnelColorsSteps)
    funnelColorsSteps <- funnelColorsSteps[-c(1, length(funnelColorsSteps))]
    funnelColors      <- paste0("grey", round(funnelColorsSteps*100))

    if (options[["invertColors"]])
      funnelColors <- rev(funnelColors)
  }

  # data-points
  dfPlot <- data.frame(
    x  = dataset[[options[["effectSize"]]]],
    y  = dataset[[options[["effectSizeStandardError"]]]]
  )

  # y-axis plotting range
  yTicks <- jaspGraphs::getPrettyAxisBreaks(range(dfPlot$y))

  ### specify zero-centered funnels
  if (options[["funnelUnderH0"]]) {
    dfsFunnel0 <- list()
    adjustFunnel0Mean          <- options[["funnelUnderH0ParametersFixedMu"]]
    adjustFunnel0Heterogeneity <- options[["funnelUnderH0ParametersFixedTau"]]
    for (i in seq_along(funnelLevels)) {
      dfsFunnel0[[i]] <- data.frame(
        x   = c(-max(yTicks), 0, 0, max(yTicks)) / qnorm(funnelLevels[i], lower.tail = FALSE),
        y   = c(max(yTicks),  0, 0, max(yTicks)),
        p   = 2*funnelLevels[i],
        lvl = 1-2*funnelLevels[i]
      )
      dfsFunnel0[[i]]$x[1:2] <- dfsFunnel0[[i]]$x[1:2] + adjustFunnel0Mean - adjustFunnel0Heterogeneity
      dfsFunnel0[[i]]$x[3:4] <- dfsFunnel0[[i]]$x[3:4] + adjustFunnel0Mean + adjustFunnel0Heterogeneity
    }
  }

  ### specify meta-analysis centered funnels
  # allow user imputed vs meta-analytic estimated values
  if (options[["funnelUnderH1"]]) {

    if (options[["funnelUnderH1Parameters"]] == "fixed") {
      adjustFunnel1Mean          <- options[["funnelUnderH1ParametersFixedMu"]]
      adjustFunnel1Heterogeneity <- options[["funnelUnderH1ParametersFixedTau"]]
    } else if (options[["funnelUnderH1Parameters"]] == "estimated"){
      fit <- metafor::rma(yi = dfPlot$x, sei = dfPlot$y, method = .maGetMethodOptions(options))
      adjustFunnel1Mean          <- fit$b[1]
      adjustFunnel1Heterogeneity <- if (options[["funnelUnderH1IncludeHeterogeneity"]]) sqrt(fit$se^2 + fit$tau2) else fit$se
    }

    dfsFunnel1 <- list()
    for (i in seq_along(funnelLevels)) {
      dfsFunnel1[[i]] <- data.frame(
        x   = c(-max(yTicks), 0, 0, max(yTicks)) / qnorm(funnelLevels[i], lower.tail = FALSE),
        y   = c(max(yTicks),  0, 0, max(yTicks)),
        p   = (1-funnelLevels[i]/100),
        lvl = funnelLevels[i]
      )
      dfsFunnel1[[i]]$x[1:2] <- dfsFunnel1[[i]]$x[1:2] + adjustFunnel1Mean - adjustFunnel1Heterogeneity
      dfsFunnel1[[i]]$x[3:4] <- dfsFunnel1[[i]]$x[3:4] + adjustFunnel1Mean + adjustFunnel1Heterogeneity
    }
  }

  # get x-axis ticks
  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(
    range(dfPlot$x),
    if (options[["funnelUnderH0"]]) range(sapply(dfsFunnel0, function(x) x$x)),
    if (options[["funnelUnderH1"]]) range(sapply(dfsFunnel1, function(x) x$x))
  )))

  ### specify "background" for the funnel plot
  dfBackground <- data.frame(
    x = c(min(xTicks), max(xTicks), max(xTicks), min(xTicks)),
    y = c(min(yTicks), min(yTicks), max(yTicks), max(yTicks))
  )

  ### plot
  out <- ggplot2::ggplot()

  if (!options[["powerEnhancement"]] && options[["invertColors"]])
    out <- out + ggplot2::geom_polygon(
      data    = dfBackground,
      mapping = ggplot2::aes(x = x, y = y),
      fill    = "black"
    )

  # add H0 funnel
  if (options[["funnelUnderH0"]]) {

    if (options[["funnelUnderH0FillColors"]]) {
      for (i in rev(seq_along(dfsFunnel0))) {
        out <- out + ggplot2::geom_polygon(
          data     = dfsFunnel0[[i]],
          mapping  = ggplot2::aes(x = x, y = y),
          fill     = scales::alpha(funnelColors[i], .25)
        )
      }
    }

    if (options[["funnelUnderH0LineType"]]!= "none") {
      for (i in rev(seq_along(dfsFunnel0))) {
        out <- out + ggplot2::geom_line(
          data     = dfsFunnel0[[i]],
          mapping  = ggplot2::aes(x = x, y = y),
          linetype = options[["funnelUnderH0LineType"]]
        )
      }
    }
  }

  # add H1 funnel
  if (options[["funnelUnderH1"]]) {

    if (options[["funnelUnderH1FillColors"]]) {
      for (i in rev(seq_along(dfsFunnel1))) {
        out <- out + ggplot2::geom_polygon(
          data     = dfsFunnel1[[i]],
          mapping  = ggplot2::aes(x = x, y = y),
          fill     = scales::alpha(funnelColors[i], .25)
        )
      }
    }

    if (options[["funnelUnderH1LineType"]]!= "none") {
      for (i in rev(seq_along(dfsFunnel1))) {
        out <- out + ggplot2::geom_line(
          data     = dfsFunnel1[[i]],
          mapping  = ggplot2::aes(x = x, y = y),
          linetype = options[["funnelUnderH1LineType"]]
        )
      }
    }
  }

  # add estimates
  out <- out + jaspGraphs::geom_point(
      data    = dfPlot,
      mapping = ggplot2::aes(x = x, y = y),
      fill    = "grey"
  )

  # add labels if specified
  # if (options[["studyLabels"]] != "") {
  #
  #   dfLabels <- cbind(
  #     dfPlot,
  #     label = studyLabels
  #   )
  #   dfLabels <- dfLabels[abs(dfLabels$y/1.96) < abs(dfLabels$x),]
  #   dfLabels$position <- ifelse(dfLabels$x < 0, "right", "left")
  #   dfLabels$nudge_x  <- ifelse(dfLabels$x < 0, -0.1, 0.1)
  #
  #   out <- out +
  #     ggplot2::geom_text(
  #       data    = dfLabels,
  #       mapping = ggplot2::aes(x = x, y = y, label = label, hjust = position), nudge_x = dfLabels$nudge_x
  #     )
  # }

  out <- out +
    jaspGraphs::scale_x_continuous(breaks = xTicks, limits = range(xTicks), name = gettext("Effect Size")) +
    ggplot2::scale_y_reverse(breaks = rev(yTicks), limits = rev(range(yTicks)), name = gettext("Standard Error")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  funnelPlot$plotObject <- out

  return()
}
