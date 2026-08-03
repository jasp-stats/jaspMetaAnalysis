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

# Selection-model entry point, validation, fitting, messages, and transformations.
# Output tables and plots are implemented in selectionmodels-tables.R and
# selectionmodels-plots.R.

SelectionModels <- function(jaspResults, dataset, options, state = NULL) {

  if (.smCheckReady(options)) {
    # check the data
    dataset <- .smCheckData(jaspResults, dataset, options)

    # fit the models
    .smFit(jaspResults, dataset, options)
  }

  # main summary tables
  .smMakeTestsTables(jaspResults, dataset, options)
  .smMakeEstimatesTables(jaspResults, dataset, options)

  # the p-value frequency tables
  if (options[["modelPValueFrequencyTable"]])
    .smPFrequencyTable(jaspResults, dataset, options)

  # figures
  if (options[["plotsWeightFunctionFixedEffectsPlot"]])
    .smWeightsPlot(jaspResults, dataset, options, "FE")
  if (options[["plotsWeightFunctionRandomEffectsPlot"]])
    .smWeightsPlot(jaspResults, dataset, options, "RE")
  if (options[["plotsMeanModelEstimatesPlot"]])
    .smEstimatesPlot(jaspResults, dataset, options)

  return()
}

.smDependencies <- c(
  "modelAutomaticallyJoinPValueIntervals", "modelPValueCutoffs", "modelTwoSidedSelection", "modelExpectedDirectionOfEffectSizes", "measures", "transformCorrelationsTo",
  "effectSize", "effectSizeSe", "sampleSize", "pValue"
)
# check and load functions
.smCheckReady              <- function(options) {
  if (options[["measures"]] == "general") {
    return(options[["effectSize"]] != "" && options[["effectSizeSe"]] != "")
  } else if (options[["measures"]] == "correlation") {
    return(options[["effectSize"]] != "" && options[["sampleSize"]] != "")
  }
}
.smCheckData               <- function(jaspResults, dataset, options) {

  dataset_old <- dataset
  dataset     <- na.omit(dataset)

  # store the number of missing values
  if (nrow(dataset_old) > nrow(dataset)) {
    if (!is.null(jaspResults[["nOmitted"]])) {
      nOmitted <- jaspResults[["nOmitted"]]
    } else {
      nOmitted <- createJaspState()
      nOmitted$dependOn(.smDependencies)
      jaspResults[["nOmitted"]] <- nOmitted
    }
    nOmitted$object <- nrow(dataset_old) - nrow(dataset)
  }

  .hasErrors(dataset               = dataset,
             type                  = c("infinity", "observations"),
             observations.amount   = "< 2",
             exitAnalysisIfErrors  = TRUE)

  if (options[["effectSizeSe"]] != "")
    .hasErrors(dataset              = dataset,
               seCheck.target       = options[["effectSizeSe"]],
               custom               = .maCheckStandardErrors,
               exitAnalysisIfErrors = TRUE)


  if (options[["sampleSize"]] != "")
    .hasErrors(dataset              = dataset,
               seCheck.target       = options[["sampleSize"]],
               custom               = .maCheckStandardErrors,
               exitAnalysisIfErrors = TRUE)


  if (options[["pValue"]] != "")
    if (any(dataset[, options[["pValue"]]] < 0) || any(dataset[, options[["pValue"]]] > 1))
      .quitAnalysis(gettextf("Cannot compute results. All entries of the p-value variable (%s) must be between 0 and 1.", options[["pValue"]]))

  if (options[["effectSize"]] != "" && options[["measures"]] == "correlation")
    if (any(dataset[, options[["effectSize"]]] <= -1) || any(dataset[, options[["effectSize"]]] >= 1))
      .quitAnalysis(gettextf("Cannot compute results. All entries of the correlation coefficient variable (%s) must be between -1 and 1.", options[["effectSize"]]))

  if (options[["sampleSize"]] != "" && any(dataset[, options[["sampleSize"]]] < 4))
    .quitAnalysis(gettextf("Cannot compute results. All entries of the sample size variable (%s) must be greater than four.", options[["sampleSize"]]))


  return(dataset)
}
.smGetCutoffs              <- function(options) {

  x <- options[["modelPValueCutoffs"]]
  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")

  x <- strsplit(x, ",", fixed = TRUE)[[1]]

  x <- trimws(x, which = "both")
  x <- x[x != ""]

  if (anyNA(as.numeric(x)))
    .quitAnalysis(gettext("The p-value cutoffs were set incorectly."))

  if (length(x) == 0)
    .quitAnalysis(gettext("At least one p-value cutoff needs to be set."))

  x <- as.numeric(x)
  if (options[["modelTwoSidedSelection"]])
    x <- c(x/2, 1-x/2)


  if (any(x <= 0 | x >= 1))
    .quitAnalysis(gettext("All p-value cutoffs needs to be between 0 and 1."))


  x <- c(sort(x), 1)

  return(x)
}
.smJoinCutoffs             <- function(steps, pVal) {

  ### this is probably the most complex part of the analysis
  # the fit function protests when there is any p-value interval with lower than 4 p-values
  # the autoreduce should combine all p-value intervals that have too few p-values

  # create p-value table
  cutoffsTable <- table(cut(pVal, breaks = c(0, steps)))

  # start removing from the end, but never 1 at the end
  while (cutoffsTable[length(cutoffsTable)] < 4) {

    # remove the one before the last step
    steps <- steps[-(length(steps) - 1)]

    # create p-value table
    cutoffsTable <- table(cut(pVal, breaks = c(0, steps)))
  }

  # and then go from the start(go one by one - two neiboring intervals with 2 p-values will become one with 4 instead of removing all of them)
  while (any(cutoffsTable < 4)) {

    # remove the first step that has less than 4 p-values
    steps <- steps[-which.max(cutoffsTable < 4)]

    # create p-value table
    cutoffsTable <- table(cut(pVal, breaks = c(0, steps)))
  }

  # do not fit the models if there is only one p-value interval
  if (length(steps) <= 1)
    stop("No steps")


  return(steps)
}
# fill tables functions
.smFillEstimates           <- function(jaspResults, table, fit, options) {

  overtitleCI <- gettextf("95%% Confidence Interval")

  table$addColumnInfo(name = "type",     title = "",                        type = "string")
  table$addColumnInfo(name = "est",      title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",       title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat",     title = "z",                       type = "number")
  table$addColumnInfo(name = "pVal",     title = "p",                       type = "pvalue")
  table$addColumnInfo(name = "lowerCI",  title = gettext("Lower"),          type = "number", overtitle = overtitleCI)
  table$addColumnInfo(name = "upperCI",  title = gettext("Upper"),          type = "number", overtitle = overtitleCI)

  rowUnadjusted <- list(type = "Unadjusted")
  rowAdjusted   <- list(type = "Adjusted")

  if (!is.null(fit)) {
    if (!jaspBase::isTryError(fit)) {

      if (fit[["fe"]]) {
        posMean <- 1
      } else {
        posMean <- 2
      }

      rowUnadjusted    <- c(rowUnadjusted, list(
        est  = fit[["unadj_est"]][posMean, 1],
        se   = fit[["unadj_se"]][posMean, 1],
        stat = fit[["z_unadj"]][posMean, 1],
        pVal = fit[["p_unadj"]][posMean, 1],
        lowerCI  = fit[["ci.lb_unadj"]][posMean, 1],
        upperCI  = fit[["ci.ub_unadj"]][posMean, 1]
      ))
      rowAdjusted    <- c(rowAdjusted, list(
        est  = fit[["adj_est"]][posMean, 1],
        se   = fit[["adj_se"]][posMean, 1],
        stat = fit[["z_adj"]][posMean, 1],
        pVal = fit[["p_adj"]][posMean, 1],
        lowerCI  = fit[["ci.lb_adj"]][posMean, 1],
        upperCI  = fit[["ci.ub_adj"]][posMean, 1]
      ))

      noteMessages    <- .smSetNoteMessages(jaspResults, fit, options)
      warningMessages <- .smSetWarningMessages(fit)
      for(i in seq_along(noteMessages)) {
        table$addFootnote(symbol = gettext("Note:"),    noteMessages[i])
      }
      for(i in seq_along(warningMessages)) {
        table$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
      }

    } else {
      errorMessage <- .smSetErrorMessage(fit)
      if (!is.null(errorMessage))
        table$setError(errorMessage)
    }
  }

  table$addRows(rowUnadjusted)
  table$addRows(rowAdjusted)

  return(table)
}
.smFillHeterogeneity       <- function(jaspResults, table, fit, options) {

  overtitleCI <- gettextf("95%% Confidence Interval")

  table$addColumnInfo(name = "type",     title = "",                    type = "string")
  table$addColumnInfo(name = "est",      title = gettext("Estimate"),   type = "number")
  table$addColumnInfo(name = "stat",     title = "z",                   type = "number")
  table$addColumnInfo(name = "pVal",     title = "p",                   type = "pvalue")
  table$addColumnInfo(name = "lowerCI",  title = gettext("Lower"),      type = "number", overtitle = overtitleCI)
  table$addColumnInfo(name = "upperCI",  title = gettext("Upper"),      type = "number", overtitle = overtitleCI)

  rowREUnadjustedTau <- list(type = "Unadjusted")
  rowREAdjustedTau   <- list(type = "Adjusted")

  if (!is.null(fit)) {
    if (!jaspBase::isTryError(fit)) {
      rowREUnadjustedTau  <- c(rowREUnadjustedTau, list(
        est  = sqrt(fit[["unadj_est"]][1, 1]),
        stat = fit[["z_unadj"]][1, 1],
        pVal = fit[["p_unadj"]][1, 1],
        lowerCI  = sqrt(ifelse(fit[["ci.lb_unadj"]][1, 1] < 0, 0, fit[["ci.lb_unadj"]][1, 1])),
        upperCI  = sqrt(fit[["ci.ub_unadj"]][1, 1])
      ))
      rowREAdjustedTau    <- c(rowREAdjustedTau, list(
        est  = sqrt(fit[["adj_est"]][1, 1]),
        stat = fit[["z_adj"]][1, 1],
        pVal = fit[["p_adj"]][1, 1],
        lowerCI  = sqrt(ifelse(fit[["ci.lb_adj"]][1, 1] < 0, 0, fit[["ci.lb_adj"]][1, 1])),
        upperCI  = sqrt(fit[["ci.ub_adj"]][1, 1])
      ))

      # add info that tau is on different scale if correlations were used
      if (options[["measures"]] == "correlation")
        table$addFootnote(symbol = gettext("Note:"), gettextf(
          "%1$s is on %2$s scale.",
          "\u03C4",
          if (options[["transformCorrelationsTo"]] == "cohensD") gettext("Cohen's <em>d</em>") else gettext("Fisher's <em>z</em>")
        ))

      noteMessages    <- .smSetNoteMessages(jaspResults, fit, options)
      warningMessages <- .smSetWarningMessages(fit)
      for(i in seq_along(noteMessages)) {
        table$addFootnote(symbol = gettext("Note:"),    noteMessages[i])
      }
      for(i in seq_along(warningMessages)) {
        table$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
      }

    } else {
      errorMessage <- .smSetErrorMessage(fit)
      if (!is.null(errorMessage))
        table$setError(errorMessage)
    }
  }

  table$addRows(rowREUnadjustedTau)
  table$addRows(rowREAdjustedTau)

  return(table)
}
.smFillWeights             <- function(jaspResults, table, fit, options) {

  overtitleCI <- gettextf("95%% Confidence Interval")
  overtitleP  <- gettext("<em>p</em>-values interval(one-sided)")

  table$addColumnInfo(name = "lr",       title = gettext("Lower"),          type = "number", overtitle = overtitleP)
  table$addColumnInfo(name = "ur",       title = gettext("Upper"),          type = "number", overtitle = overtitleP)
  table$addColumnInfo(name = "est",      title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",       title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat",     title = "z",                       type = "number")
  table$addColumnInfo(name = "pVal",     title = "p",                       type = "pvalue")
  table$addColumnInfo(name = "lowerCI",  title = gettext("Lower"),          type = "number", overtitle = overtitleCI)
  table$addColumnInfo(name = "upperCI",  title = gettext("Upper"),          type = "number", overtitle = overtitleCI)

  if (!is.null(fit)) {
    if (!jaspBase::isTryError(fit)) {

      if (fit[["fe"]]) {
        weightsAdd <- 0
      } else {
        weightsAdd <- 1
      }

      for(i in 1:length(fit[["steps"]])) {
        if (i == 1) {
          tempRow <- list(
            lr   = 0,
            ur   = fit[["steps"]][1],
            est  = 1,
            se   = 0,
            lowerCI  = 1,
            upperCI  = 1
          )
        } else {
          tempRow <- list(
            lr   = fit[["steps"]][i-1],
            ur   = fit[["steps"]][i],
            est  = fit[["adj_est"]][i+weightsAdd, 1],
            se   = fit[["adj_se"]][i+weightsAdd, 1],
            stat = fit[["z_adj"]][i+weightsAdd, 1],
            pVal = fit[["p_adj"]][i+weightsAdd, 1],
            lowerCI  = ifelse(fit[["ci.lb_adj"]][i+weightsAdd, 1] < 0, 0, fit[["ci.lb_adj"]][i+weightsAdd, 1]),
            upperCI  = fit[["ci.ub_adj"]][i+weightsAdd, 1]
          )
        }
        table$addRows(tempRow)
      }

      noteMessages    <- .smSetNoteMessages(jaspResults, fit, options)
      warningMessages <- .smSetWarningMessages(fit)
      for(i in seq_along(noteMessages)) {
        table$addFootnote(symbol = gettext("Note:"),    noteMessages[i])
      }
      for(i in seq_along(warningMessages)) {
        table$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
      }

    } else {
      errorMessage <- .smSetErrorMessage(fit)
      if (!is.null(errorMessage))
        table$setError(errorMessage)
    }
  }

  return(table)
}
# fit models
.smFit                     <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["models"]])) {
    return()
  } else {
    models <- createJaspState()
    models$dependOn(c(.smDependencies, "modelPValueFrequencyTable"))
    jaspResults[["models"]] <- models
  }

  # get the p-value steps
  steps <- .smGetCutoffs(options)

  # get the p-values
  pVal  <- .maGetInputPVal(dataset, options)

  # remove intervals that do not contain enought(3) p-values
  if (options[["modelAutomaticallyJoinPValueIntervals"]]) {
    steps <- try(.smJoinCutoffs(steps, pVal))

    if (jaspBase::isTryError(steps)) {
      models[["object"]] <- list(
        FE = steps,
        RE = steps
      )
      return()
    }
  }


  # fit the models
  fitFE <- try(weightr::weightfunct(
    effect = .maGetInputEs(dataset, options),
    v      = .maGetInputSe(dataset, options)^2,
    pval   = pVal,
    steps  = steps,
    fe     = TRUE
  ))

  fitRE <- try(weightr::weightfunct(
    effect = .maGetInputEs(dataset, options),
    v      = .maGetInputSe(dataset, options)^2,
    pval   = pVal,
    steps  = steps,
    fe     = FALSE
  ))

  # take care of the possibly turned estimates
  if (options[["modelExpectedDirectionOfEffectSizes"]] == "negative") {
    fitFE <- .smTurnEstimatesDirection(fitFE)
    fitRE <- .smTurnEstimatesDirection(fitRE)
  }

  # take care of the transformed estimates
  if (options[["measures"]] == "correlation") {
    fitFE <- .smTransformEstimates(fitFE, options)
    fitRE <- .smTransformEstimates(fitRE, options)
  }

  models[["object"]] <- list(
    FE = fitFE,
    RE = fitRE
  )

  return()
}
# some additional helpers
.smSetErrorMessage         <- function(fit, type = NULL) {

  if (jaspBase::isTryError(fit)) {
    if (!is.null(type)) {
      model_type <- switch(
        type,
        "FE" = gettext("Fixed effects model: "),
        "RE" = gettext("Random effects model: ")
      )
    } else {
      model_type <- ""
    }

    # Extend with targeted optimizer messages as additional failure modes are identified.
    if (grepl("non-finite value supplied by optim", fit) || grepl("singular", fit)) {
      message <- gettextf("%sThe optimizer failed to find a solution. Consider re-specifying the model or the p-value cutoffs.", model_type)
    } else if (grepl("No steps", fit)) {
      if (model_type != "")
        message <- gettextf("%sThe automatic cutoffs selection did not find viable p-value cutoffs. Please, specify them manually.", model_type)
      else
        message <- NULL
    } else {
      message <- paste0(model_type, fit)
    }
  } else {
    message <- NULL
  }

  return(message)
}
.smSetWarningMessages      <- function(fit) {

  messages   <- NULL

  if (!jaspBase::isTryError(fit)) {

    ### check for no p-value in cutoffs
    steps <- c(0, fit[["steps"]])
    pVal  <- fit[["p"]]

    cutoffsTable <- table(cut(pVal, breaks = steps))
    if (any(cutoffsTable == 0)) {
      messages <- c(messages, gettext(
        "At least one of the p-value intervals contains no effect sizes, leading to estimation problems. Consider re-specifying the cutoffs."
      ))
    } else if (any(cutoffsTable <= 3)) {
      messages <- c(messages, gettext(
        "At least one of the p-value intervals contains three or fewer effect sizes, which may lead to estimation problems. Consider re-specifying the cutoffs."
      ))
    }

    ### check whether the unadjusted estimates is negative - the weightr assumes that the effect sizes are in expected direction
    if (fit[["unadj_est"]][ifelse(fit[["fe"]], 1, 2), 1] < 0 && is.null(fit[["estimates_turned"]])) {
      messages <- c(messages, gettext(
        "The unadjusted estimate is negative. The selection model default specification expects that the expected direction of the effect size is positive. Please, check that you specified the effect sizes correctly or change the 'Expected effect size direction' option in the 'Model' tab."
      ))
    }

  }

  return(messages)
}
.smSetNoteMessages         <- function(jaspResults, fit, options) {

  messages <- NULL

  if (!.smCheckReady(options) && options[["pValue"]] != "") {

    messages <- gettext("The analysis requires both 'Effect Size' and 'Effect Size Standard Error' to be specified.")

  } else if (!jaspBase::isTryError(fit)) {

    # add note about the ommited steps
    steps <- fit[["steps"]]
    steps <- steps

    stepsSettings <- .smGetCutoffs(options)

    if (!all(stepsSettings %in% steps) && options[["modelAutomaticallyJoinPValueIntervals"]]) {
      messages      <- c(messages, gettextf(
        "Only the following one-sided p-value cutoffs were used: %s.",
        paste(steps[steps != 1], collapse = ", ")
      ))
    }

    if (!is.null(jaspResults[["nOmitted"]])) {
      messages      <- c(messages, sprintf(
        ngettext(
          jaspResults[["nOmitted"]]$object,
          "%i observation was removed due to missing values.",
          "%i observations were removed due to missing values."
        ),
        jaspResults[["nOmitted"]]$object
      ))
    }

  }


  return(messages)
}
# effect size transformations
.smTurnEstimatesDirection  <- function(fit) {

  # in the case that the expected direction was negative, the estimated effect sizes will be in the opposite direction
  # this function turns them around

  if (!jaspBase::isTryError(fit)) {

    if (fit[["fe"]]) {
      posMean <- 1
    } else {
      posMean <- 2
    }

    fitOld <- fit

    fit[["unadj_est"]][posMean, 1] <- fitOld[["unadj_est"]][posMean, 1] * -1
    fit[["z_unadj"]][posMean, 1]   <- fitOld[["z_unadj"]][posMean, 1]   * -1
    fit[["ci.lb_adj"]][posMean, 1] <- fitOld[["ci.ub_adj"]][posMean, 1] * -1
    fit[["ci.ub_adj"]][posMean, 1] <- fitOld[["ci.lb_adj"]][posMean, 1] * -1

    fit[["adj_est"]][posMean, 1]     <- fitOld[["adj_est"]][posMean, 1]     * -1
    fit[["z_adj"]][posMean, 1]       <- fitOld[["z_adj"]][posMean, 1]       * -1
    fit[["ci.lb_unadj"]][posMean, 1] <- fitOld[["ci.ub_unadj"]][posMean, 1] * -1
    fit[["ci.ub_unadj"]][posMean, 1] <- fitOld[["ci.lb_unadj"]][posMean, 1] * -1

    fit[["output_unadj"]][["par"]][posMean] <- fitOld[["output_unadj"]][["par"]][posMean] * -1
    fit[["output_adj"]][["par"]][posMean]   <- fitOld[["output_adj"]][["par"]][posMean]   * -1

    fit[["estimates_turned"]] <- TRUE
  }

  return(fit)
}
.smTransformEstimates      <- function(fit, options) {

  if (options[["measures"]] == "general")
    return(fit)

  # in the case that correlation input was used, this part will transform the results back
  # from the estimation scale to the outcome scale

  if (!jaspBase::isTryError(fit)) {

    if (fit[["fe"]]) {
      posMean <- 1
    } else {
      posMean <- 2
    }

    fitOld <- fit

    fit[["unadj_est"]][posMean, 1]   <- .maInvTransformEs(fitOld[["unadj_est"]][posMean, 1],   options[["transformCorrelationsTo"]])
    fit[["unadj_se"]][posMean, 1]    <- .maInvTransformSe(fitOld[["unadj_est"]][posMean, 1],   fitOld[["unadj_se"]][posMean, 1], options[["transformCorrelationsTo"]])
    fit[["ci.lb_unadj"]][posMean, 1] <- .maInvTransformEs(fitOld[["ci.lb_unadj"]][posMean, 1], options[["transformCorrelationsTo"]])
    fit[["ci.ub_unadj"]][posMean, 1] <- .maInvTransformEs(fitOld[["ci.ub_unadj"]][posMean, 1], options[["transformCorrelationsTo"]])

    fit[["adj_est"]][posMean, 1]     <- .maInvTransformEs(fitOld[["adj_est"]][posMean, 1],   options[["transformCorrelationsTo"]])
    fit[["adj_se"]][posMean, 1]      <- .maInvTransformSe(fitOld[["adj_est"]][posMean, 1],   fitOld[["adj_se"]][posMean, 1], options[["transformCorrelationsTo"]])
    fit[["ci.lb_adj"]][posMean, 1]   <- .maInvTransformEs(fitOld[["ci.lb_adj"]][posMean, 1], options[["transformCorrelationsTo"]])
    fit[["ci.ub_adj"]][posMean, 1]   <- .maInvTransformEs(fitOld[["ci.ub_adj"]][posMean, 1], options[["transformCorrelationsTo"]])

    fit[["estimates_transformed"]] <- options[["transformCorrelationsTo"]]
  }

  return(fit)
}
