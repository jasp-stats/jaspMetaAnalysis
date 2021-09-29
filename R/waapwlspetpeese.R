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

### Common functions for WAAP-WLS and PET-PEESE
.wwppDependencies <- c("measures", "muTransform", "inputES", "inputSE", "inputN")
# check and load functions
.wwppCheckReady              <- function(options) {
  if (options[["measures"]] == "general") {
    return(options[["inputES"]] != "" && options[["inputSE"]] != "")
  } else if (options[["measures"]] == "correlation") {
    return(options[["inputES"]] != "" && options[["inputN"]] != "")
  }
}
.wwppGetData                 <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    return(.readDataSetToEnd(columns.as.numeric = c(
      options[["inputES"]],
      if (options[["inputSE"]] != "") options[["inputSE"]],
      if (options[["inputN"]]  != "") options[["inputN"]]
    )))
  }
}
.wwppCheckData               <- function(jaspResults, dataset, options) {

  dataset_old <- dataset
  dataset     <- na.omit(dataset)

  # store the number of missing values
  if (nrow(dataset_old) > nrow(dataset)) {
    if (!is.null(jaspResults[["nOmitted"]])) {
      nOmitted <- jaspResults[["nOmitted"]]
    } else {
      nOmitted <- createJaspState()
      nOmitted$dependOn(.wwppDependencies)
      jaspResults[["nOmitted"]] <- nOmitted
    }
    nOmitted$object <- nrow(dataset_old) - nrow(dataset)
  }

  .hasErrors(dataset               = dataset,
             type                  = c("infinity", "observations"),
             observations.amount   = "< 2",
             exitAnalysisIfErrors  = TRUE)

  if (options[["inputSE"]] != "")
    .hasErrors(dataset               = dataset,
               type                  = c("negativeValues"),
               negativeValues.target = options[["inputSE"]],
               exitAnalysisIfErrors  = TRUE)

  if (options[["inputN"]] != "")
    .hasErrors(dataset               = dataset,
               type                  = c("negativeValues"),
               negativeValues.target = options[["inputN"]],
               exitAnalysisIfErrors  = TRUE)


  if (options[["inputES"]] != "" && options[["measures"]] == "correlation")
    if (any(dataset[, options[["inputES"]]] <= -1) || any(dataset[, options[["inputES"]]] >= 1))
      .quitAnalysis(gettextf("Cannot compute results. All entries of the correlation coefficient variable (%s) must be between -1 and 1.", options[["inputES"]]))

  if (options[["inputN"]] != "" && any(dataset[, options[["inputN"]]] < 4))
    .quitAnalysis(gettextf("Cannot compute results. All entries of the sample size variable (%s) must be greater than four.", options[["inputN"]]))


  return(dataset)
}
# fill tables functions
.wwppFillEstimates           <- function(jaspResults, table, models, options, type) {

  overtitleCI <- gettextf("95%% Confidence Interval")

  table$addColumnInfo(name = "type",     title = "",                        type = "string")
  table$addColumnInfo(name = "est",      title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",       title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat",     title = "t",                       type = "number")
  table$addColumnInfo(name = "df",       title = gettext("df"),             type = "integer")
  table$addColumnInfo(name = "pVal",     title = "p",                       type = "pvalue")
  table$addColumnInfo(name = "lowerCI",  title = gettext("Lower"),          type = "number", overtitle = overtitleCI)
  table$addColumnInfo(name = "upperCI",  title = gettext("Upper"),          type = "number", overtitle = overtitleCI)


  if (is.null(models))
    return(table)


  for (estimator in if(type == "waapWls") c("wls", "waap") else c("pet", "peese")) {

    if (length(models[[estimator]]) == 0)
      table$addRows(list(
        type    = toupper(estimator)
      ))
    else
      table$addRows(list(
        type    = toupper(estimator),
        est     = models[[estimator]]["(Intercept)", "Estimate"],
        se      = models[[estimator]]["(Intercept)", "Std. Error"],
        stat    = models[[estimator]]["(Intercept)", "t value"],
        df      = attr(models[[estimator]], "df"),
        pVal    = models[[estimator]]["(Intercept)", "Pr(>|t|)"],
        lowerCI = models[[estimator]]["(Intercept)", "lCI"],
        upperCI = models[[estimator]]["(Intercept)", "uCI"]
      ))

  }

  .wwppAddWaapFootnote(table, models[["waap"]])

  return(table)
}
.wwppFillPetPeese            <- function(jaspResults, table, models, options) {

  overtitleCI <- gettextf("95%% Confidence Interval")

  table$addColumnInfo(name = "type",     title = "",                        type = "string")
  table$addColumnInfo(name = "est",      title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",       title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat",     title = "t",                       type = "number")
  table$addColumnInfo(name = "df",       title = gettext("df"),             type = "integer")
  table$addColumnInfo(name = "pVal",     title = "p",                       type = "pvalue")
  table$addColumnInfo(name = "lowerCI",  title = gettext("Lower"),          type = "number", overtitle = overtitleCI)
  table$addColumnInfo(name = "upperCI",  title = gettext("Upper"),          type = "number", overtitle = overtitleCI)


  if (is.null(models))
    return(table)


  for (estimator in c("pet", "peese")) {
    table$addRows(list(
      type    = toupper(estimator),
      est     = models[[estimator]][toupper(estimator), "Estimate"],
      se      = models[[estimator]][toupper(estimator), "Std. Error"],
      stat    = models[[estimator]][toupper(estimator), "t value"],
      df      = attr(models[[estimator]], "df"),
      pVal    = models[[estimator]][toupper(estimator), "Pr(>|t|)"],
      lowerCI = models[[estimator]][toupper(estimator), "lCI"],
      upperCI = models[[estimator]][toupper(estimator), "uCI"]
    ))
  }

  return(table)
}
.wwppFillHeterogeneity       <- function(jaspResults, table, models, options, type) {

  overtitleCI <- gettextf("95%% Confidence Interval")

  table$addColumnInfo(name = "type",     title = "",                        type = "string")
  table$addColumnInfo(name = "est",      title = gettext("Estimate"),       type = "number")


  if (is.null(models))
    return(table)


  for (estimator in if(type == "waapWls") c("wls", "waap") else c("pet", "peese")) {

    if (length(models[[estimator]]) == 0 && estimator == "waap") {
      table$addFootnote(symbol = gettextf("Error: There was not enough adequatelly powered studies (k = %1$i)", attr(models[["waap"]], "nPowered")))
    } else {
      table$addRows(list(
        type    = toupper(estimator),
        est     = attr(models[[estimator]], "sigma")
      ))
      if (estimator == "waap")
        table$addFootnote(symbol = gettextf("Note: There were %1$i adequatelly powered studies", attr(models[["waap"]], "nPowered")))
    }
  }

  return(table)
}
# fit models (the fitting functions are adapted from Carter et al., 2019)
.wwppFit                     <- function(jaspResults, dataset, options, type) {

  if (!is.null(jaspResults[["models"]])) {
    return()
  } else {
    models <- createJaspState()
    models$dependOn(c(.wwppDependencies, "tablePVal"))
    jaspResults[["models"]] <- models
  }

  if (type == "waapWls") {

    summaryWls   <- .wwppWls(  .maGetInputEs(dataset, options), .maGetInputSe(dataset, options))
    summaryWaap  <- .wwppWaap( .maGetInputEs(dataset, options), .maGetInputSe(dataset, options))

    # take care of the transformed estimates
    if (options[["measures"]] == "correlation") {
      summaryWls   <- .wwppTransformEstimates(summaryWls,   options)
      summaryWaap  <- .wwppTransformEstimates(summaryWaap,  options)
    }

    models[["object"]] <- list(
      wls   = summaryWls,
      waap  = summaryWaap
    )

  } else if (type == "petPeese") {

    summaryPet   <- .wwppPet(  .maGetInputEs(dataset, options), .maGetInputSe(dataset, options))
    summaryPeese <- .wwppPeese(.maGetInputEs(dataset, options), .maGetInputSe(dataset, options))

    # take care of the transformed estimates
    if (options[["measures"]] == "correlation") {
      summaryPet   <- .wwppTransformEstimates(summaryPet,   options)
      summaryPeese <- .wwppTransformEstimates(summaryPeese, options)
    }

    models[["object"]] <- list(
      pet   = summaryPet,
      peese = summaryPeese
    )

  }

  return()
}
.wwppPet                     <- function(y, se) {

  lmPet      <- lm(y ~ se, weights = 1/se^2)
  summaryPet <- as.data.frame(summary(lmPet)$coefficients)
  summaryPet <- .wwppAddCi(summaryPet)

  row.names(summaryPet)[2]  <- "PET"
  attr(summaryPet, "df")    <- length(y) - 2
  attr(summaryPet, "sigma") <- sigma(lmPet)
  attr(summaryPet, "fit")   <- lmPet
  attr(summaryPet, "y")     <- y
  attr(summaryPet, "se")    <- se

  return(summaryPet)
}
.wwppPeese                   <- function(y, se) {

  lmPeese      <- lm(y ~ I(se^2), weights = 1/se^2)
  summaryPeese <- as.data.frame(summary(lmPeese)$coefficients)
  summaryPeese <- .wwppAddCi(summaryPeese)

  row.names(summaryPeese)[2]  <- "PEESE"
  attr(summaryPeese, "df")    <- length(y) - 2
  attr(summaryPeese, "sigma") <- sigma(lmPeese)
  attr(summaryPeese, "fit")   <- lmPeese
  attr(summaryPeese, "y")     <- y
  attr(summaryPeese, "se")    <- se

  return(summaryPeese)
}
.wwppWls                     <- function(y, se) {

  lmWls      <- lm(y ~ 1, weights = 1/se^2)
  summaryWls <- as.data.frame(summary(lmWls)$coefficients)
  summaryWls <- .wwppAddCi(summaryWls)

  attr(summaryWls, "df")    <- length(y) - 1
  attr(summaryWls, "sigma") <- sigma(lmWls)
  attr(summaryWls, "fit")   <- lmWls
  attr(summaryWls, "y")     <- y
  attr(summaryWls, "se")    <- se

  return(summaryWls)
}
.wwppWaap                    <- function(y, se) {

  summaryWls <- .wwppWls(y, se)
  powered    <- abs(summaryWls["(Intercept)", "Estimate"] / 2.8) >= se

  if(sum(powered) >= 2){
    summaryWaap <- .wwppWls(y[powered], se[powered])
  }else{
    summaryWaap <- list()
  }

  attr(summaryWaap, "nPowered") <- sum(powered)

  return(summaryWaap)
}
# create the tables
.wwppMakeTestsTables         <- function(jaspResults, dataset, options, type) {

  if (!is.null(jaspResults[["fitTests"]])) {
    return()
  } else {
    # create container
    fitTests <- createJaspContainer(title = gettext("Model Tests"))
    fitTests$position <- 1
    fitTests$dependOn(.wwppDependencies)
    jaspResults[["fitTests"]] <- fitTests
  }

  models   <- jaspResults[["models"]]$object

  ### test of effect
  effectTest <- createJaspTable(title = gettext("Test of Effect"))
  effectTest$position <- 1
  fitTests[["effectTest"]] <- effectTest

  effectTest$addColumnInfo(name = "type",  title = "",            type = "string")
  effectTest$addColumnInfo(name = "stat",  title = "t",           type = "number")
  effectTest$addColumnInfo(name = "df",    title = gettext("df"), type = "integer")
  effectTest$addColumnInfo(name = "pVal",  title = "p",           type = "pvalue")

  if (!is.null(models)) {

    # WLS
    if (type == "waapWls") {
      effectTest$addRows(list(
        type = "WLS",
        stat = models[["wls"]]["(Intercept)", "t value"],
        df   = attr(models[["wls"]], "df"),
        pVal = models[["wls"]]["(Intercept)", "Pr(>|t|)"]
      ))

      # WAAP
      .wwppAddWaapFootnote(effectTest, models[["waap"]])
      if (length(models[["waap"]]) == 0)
        effectTest$addRows(list(type = "WAAP"))
      else
        effectTest$addRows(list(
          type = "WAAP",
          stat = models[["waap"]]["(Intercept)", "t value"],
          df   = attr(models[["waap"]], "df"),
          pVal = models[["waap"]]["(Intercept)", "Pr(>|t|)"]
        ))
    } else if (type == "petPeese") {
      # PET
      effectTest$addRows(list(
        type = "PET",
        stat = models[["pet"]]["(Intercept)", "t value"],
        df   = attr(models[["pet"]], "df"),
        pVal = models[["pet"]]["(Intercept)", "Pr(>|t|)"]
      ))
    }

  }


  ### test of bias
  if (type == "petPeese") {
    biasTest <- createJaspTable(title = gettext("Test of Publication Bias"))
    biasTest$position <- 2
    fitTests[["biasTest"]] <- biasTest

    biasTest$addColumnInfo(name = "type",  title = "",            type = "string")
    biasTest$addColumnInfo(name = "stat",  title = "t",           type = "number")
    biasTest$addColumnInfo(name = "df",    title = gettext("df"), type = "integer")
    biasTest$addColumnInfo(name = "pVal",  title = "p",           type = "pvalue")

    if (!is.null(models)) {

      biasTest$addRows(list(
        type = "PET",
        stat = models[["pet"]]["PET", "t value"],
        df   = attr(models[["pet"]], "df"),
        pVal = models[["pet"]]["PET", "Pr(>|t|)"]
      ))

    }
  }

  return()
}
.wwppMakeEstimatesTables     <- function(jaspResults, dataset, options, type) {

  models   <- jaspResults[["models"]]$object

  ### assuming heterogeneity
  if (is.null(jaspResults[["estimates"]])) {
    # create container
    estimates <- createJaspContainer(title = gettext("Estimates"))
    estimates$position <- 2
    estimates$dependOn(c(.wwppDependencies, "estimates"))
    jaspResults[["estimates"]] <- estimates
  } else {
    estimates <- jaspResults[["estimates"]]
  }

  # mean estimates
  if (is.null(estimates[["mean"]]) && options[["estimatesMean"]]) {
    estimatesMean <- createJaspTable(title = gettextf(
      "Mean Estimates (%s)",
      if (options[["measures"]] == "correlation") "\u03C1" else "\u03BC"
    ))
    estimatesMean$position <- 1
    estimates$dependOn("estimatesMean")
    estimates[["estimatesMean"]] <- estimatesMean
    estimatesMean <- .wwppFillEstimates(jaspResults, estimatesMean, models, options, type)
  }

  # PET-PEESE estimates
  if (type == "petPeese") {
    if (is.null(estimates[["petPeese"]]) && options[["estimatesPetPeese"]]) {
      petPeese <- createJaspTable(title = gettext("PET-PEESE Regression Estimates"))
      petPeese$position  <- 2
      petPeese$dependOn("estimatesPetPeese")
      estimates[["petPeese"]] <- petPeese
      petPeese <- .wwppFillPetPeese(jaspResults, petPeese, models, options)
    }
  }

  # heterogeneity estimates
  if (is.null(estimates[["heterogeneity"]]) && options[["estimatesSigma"]]) {
    heterogeneity <- createJaspTable(title = gettext("Multiplicative Heterogeneity Estimates"))
    heterogeneity$position <- 3
    heterogeneity$dependOn("estimatesSigma")
    estimates[["heterogeneity"]] <- heterogeneity
    heterogeneity <- .wwppFillHeterogeneity(jaspResults, heterogeneity, models, options, type)
  }

  return()
}
# create the plots
.wwppRegressionPlot          <- function(jaspResults, dataset, options, type = "pet") {

  if (!is.null(jaspResults[[paste0(type, "Regression")]])) {
    return()
  } else {
    plotRegression <- createJaspPlot(
      title  = gettextf("Estimated %1$s Regression", toupper(type)),
      width  = 500,
      height = 400)
    plotRegression$dependOn(c(.wwppDependencies, switch(type, "pet" = "regressionPet", "peese" = "regressionPeese")))
    plotRegression$position <- switch(type, "pet" = 5, "peese" = 6)
    jaspResults[[paste0(type, "Regression")]] <- plotRegression
  }

  if (!.wwppCheckReady(options))
    return()


  # get the fit
  fit   <- attr(jaspResults[["models"]]$object[[type]], "fit")
  fitEs <- attr(jaspResults[["models"]]$object[[type]], "y")
  fitSe <- attr(jaspResults[["models"]]$object[[type]], "se")

  # get the regression line prediction
  fitSeRange    <- range(jaspGraphs::getPrettyAxisBreaks(c(0, fitSe)))
  fitSeSequence <- seq(fitSeRange[1], fitSeRange[2], length.out = 101)
  fitPrediction <- predict(fit, newdata = data.frame(se = fitSeSequence), interval = "confidence")

  if (options[["measures"]] == "correlation")
    yLabel <- switch(
      options[["muTransform"]],
      "cohensD"  = gettext("Cohen's d"),
      "fishersZ" = gettext("Fisher's z"))
  else
    yLabel <- gettext("Effect Size")

  xTicks <- jaspGraphs::getPrettyAxisBreaks(c(0, fitSe))
  yTicks <- jaspGraphs::getPrettyAxisBreaks(c(fitPrediction[,"fit"], fitEs))

  # make the plot happen
  plot <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      ggplot2::aes(
        x = c(fitSeSequence, rev(fitSeSequence)),
        y = c(fitPrediction[,"lwr"], rev(fitPrediction[,"upr"]))
      ),
      fill = "grey80") +
    ggplot2::geom_path(
      ggplot2::aes(
        x = fitSeSequence,
        y = fitPrediction[,"fit"]
      ),
      size = 1.25) +
    ggplot2::scale_x_continuous(
      gettext("Standard Error"),
      breaks = xTicks,
      limits = range(xTicks),
      oob    = scales::oob_keep) +
    ggplot2::scale_y_continuous(
      yLabel,
      breaks = yTicks,
      limits = range(yTicks),
      oob    = scales::oob_keep) +
    jaspGraphs::geom_point(
      ggplot2::aes(
        x  = fitSe,
        y  = fitEs),
      size  = 2,
      shape = 18) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plotRegression$plotObject <- plot

  return()
}
.wwppEstimatesPlot           <- function(jaspResults, dataset, options, type) {

  if (!is.null(jaspResults[["plotEstimates"]])) {
    return()
  } else {
    plotEstimates <- createJaspPlot(
      title  = gettextf(
        "Mean Model Estimates (%s)",
        if (options[["measures"]] == "correlation") "\u03C1" else "\u03BC"
      ),
      width  = 500,
      height = 200)
    plotEstimates$dependOn(c(.wwppDependencies, "plotModels"))
    plotEstimates$position <- 7
    jaspResults[["plotEstimates"]] <- plotEstimates
  }


  models <- jaspResults[["models"]]$object
  if (is.null(models))
    return()

  # get the estimates
  estimates <- data.frame()
  for (estimator in if(type == "waapWls") c("wls", "waap") else c("pet", "peese")) {
    if (length(models[[estimator]]) != 0)
      estimates <- rbind(estimates, data.frame(
        model   = toupper(estimator),
        mean    = models[[estimator]]["(Intercept)", "Estimate"],
        lowerCI = models[[estimator]]["(Intercept)", "lCI"],
        upperCI = models[[estimator]]["(Intercept)", "uCI"]
      ))
  }
  estimates <- estimates[nrow(estimates):1, ]

  # handle NaN in the estimates
  if (any(c(is.nan(estimates[,"mean"]), is.nan(estimates[,"lowerCI"]), is.nan(estimates[,"upperCI"]))))
    plotEstimates$setError(gettext("The figure could not be created since one of the estimates is not a number."))

  xTicks <- jaspGraphs::getPrettyAxisBreaks(range(c(0, estimates[,"lowerCI"], estimates[,"upperCI"])))

  # make the plot happen
  plot <- ggplot2::ggplot() +
    ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = estimates[,"lowerCI"],
      xmax = estimates[,"upperCI"],
      y    = 1:nrow(estimates)
    ),
    height = 0.3) +
    jaspGraphs::geom_point(
      ggplot2::aes(
        x = estimates[,"mean"],
        y = 1:nrow(estimates)),
      shape = 15) +
    jaspGraphs::geom_line(ggplot2::aes(x = c(0,0), y = c(.5, nrow(estimates) + 0.5)), linetype = "dotted") +
    ggplot2::scale_x_continuous(
      bquote("Mean Estimate"~.(if (options[["measures"]] == "correlation") bquote(rho) else bquote(mu))),
      breaks = xTicks,
      limits = range(xTicks)) +
    ggplot2::scale_y_continuous(
      "",
      breaks = 1:nrow(estimates),
      labels = estimates[,"model"],
      limits = c(0.5, nrow(estimates) + 0.5)) +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank()) +
    jaspGraphs::geom_rangeframe(sides = "b") +
    jaspGraphs::themeJaspRaw()

  plotEstimates$plotObject <- plot

  return()
}
# effect size transformations
.wwppTransformEstimates      <- function(fit, options) {

  if (options[["measures"]] == "general" || length(fit) == 0)
    return(fit)

  # in the case that correlation input was used, this part will transform the results back
  # from the estimation scale to the outcome scale
  for (i in 1:nrow(fit)) {
    fit["(Intercept)", "Std. Error"]                <- .maInvTransformSe(fit["(Intercept)", "Estimate"], fit["(Intercept)", "Std. Error"], options[["muTransform"]])
    fit["(Intercept)", c("Estimate", "lCI", "uCI")] <- .maInvTransformEs(fit["(Intercept)", c("Estimate", "lCI", "uCI")],      options[["muTransform"]])
  }

  return(fit)
}
# some additional helpers
.wwppAddCi                   <- function(summaryFit) {

  summaryFit$lCI <- summaryFit[, "Estimate"] + qnorm(0.025) * summaryFit[, "Std. Error"]
  summaryFit$uCI <- summaryFit[, "Estimate"] + qnorm(0.975) * summaryFit[, "Std. Error"]

  return(summaryFit)
}
.wwppAddWaapFootnote         <- function(table, waap) {

  if (is.null(waap))
    return()
  else if (length(waap) == 0)
    table$addFootnote(symbol = gettext("Warning:") , gettextf("WAAP was not estimated: there were only %1$i adequatelly powered studies.", attr(waap, "nPowered")))
  else
    table$addFootnote(gettextf("There were %1$i adequatelly powered studies", attr(waap, "nPowered")))
}
