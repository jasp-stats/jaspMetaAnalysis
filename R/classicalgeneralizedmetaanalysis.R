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


ClassicalGeneralizedMetaAnalysis <- function(jaspResults, dataset = NULL, options, ...) {

  options[["analysis"]] <- "generalizedMetaAnalysis"

  if (.maReady(options)) {
    dataset <- .maglmmCheckData(dataset, options)
    .maglmmCheckErrors(dataset, options)
  }

  ClassicalMetaAnalysisCommon(jaspResults, dataset, options)

  return()
}

# predicates
.maIsGLMM <- function(options) {
  options[["analysis"]] == "generalizedMetaAnalysis"
}

.maglmmGetMeasureCategory <- function(options) {
  switch(
    options[["effectSizeMeasure"]],
    "OR" = , "RR" = , "RD" = "twoByTwo",
    "IRR"                  = "events"
  )
}

# data loading and validation
.maglmmCheckData <- function(dataset, options) {

  measureCategory <- .maglmmGetMeasureCategory(options)

  if (measureCategory == "twoByTwo") {
    omitOnVariables <- c(
      options[["successesGroup1"]],
      options[["successesGroup2"]],
      options[["sampleSizeGroup1"]],
      options[["sampleSizeGroup2"]]
    )
  } else if (measureCategory == "events") {
    omitOnVariables <- c(
      options[["eventsGroup1"]],
      options[["eventsGroup2"]],
      options[["personTimeGroup1"]],
      options[["personTimeGroup2"]]
    )
  }

  # add subgroup and predictors
  if (options[["subgroup"]] != "")
    omitOnVariables <- c(omitOnVariables, options[["subgroup"]])

  predictorsNominal <- options[["predictors"]][options[["predictors.types"]] == "nominal"]
  predictorsScale   <- options[["predictors"]][options[["predictors.types"]] == "scale"]
  if (length(predictorsNominal) > 0) omitOnVariables <- c(omitOnVariables, predictorsNominal)
  if (length(predictorsScale)   > 0) omitOnVariables <- c(omitOnVariables, predictorsScale)

  # omit NAs
  anyNaByRows <- apply(dataset[, omitOnVariables, drop = FALSE], 1, function(x) anyNA(x))
  dataset     <- dataset[!anyNaByRows, ]
  attr(dataset, "NAs")    <- sum(anyNaByRows)
  attr(dataset, "NasIds") <- anyNaByRows

  # drop empty factor levels
  dataset <- droplevels(dataset)

  return(dataset)
}

.maglmmCheckErrors <- function(dataset, options) {

  measureCategory <- .maglmmGetMeasureCategory(options)

  if (measureCategory == "twoByTwo") {
    variables <- c(
      options[["successesGroup1"]],
      options[["successesGroup2"]],
      options[["sampleSizeGroup1"]],
      options[["sampleSizeGroup2"]]
    )
  } else if (measureCategory == "events") {
    variables <- c(
      options[["eventsGroup1"]],
      options[["eventsGroup2"]],
      options[["personTimeGroup1"]],
      options[["personTimeGroup2"]]
    )
  }

  .hasErrors(
    dataset              = dataset,
    type                 = c("infinity", "observations"),
    all.target           = variables,
    observations.amount  = "< 2",
    custom               = list(nonNegativeCheck = function(dataset, target) {
      nonNegative <- !all(dataset[, target] >= 0, na.rm = TRUE)
      if (nonNegative)
        return(gettext("All observations must be non-negative."))
    }),
    exitAnalysisIfErrors = TRUE)

  # check predictors
  otherVariable <- options[["predictors"]]
  if (length(otherVariable) > 0) {
    .hasErrors(
      dataset              = dataset,
      type                 = c("infinity", "observations", "variance", "factorLevels"),
      all.target           = otherVariable,
      observations.amount  = "< 2",
      factorLevels.amount  = "< 2",
      exitAnalysisIfErrors = TRUE)
  }
}

# fitting function
.maglmmFitModelFun <- function(dataset, options, subgroupName) {

  rmaInput <- list(data = dataset)

  # data arguments based on measure category
  measureCategory <- .maglmmGetMeasureCategory(options)
  if (measureCategory == "twoByTwo") {
    rmaInput$ai  <- as.name(options[["successesGroup1"]])
    rmaInput$ci  <- as.name(options[["successesGroup2"]])
    rmaInput$n1i <- as.name(options[["sampleSizeGroup1"]])
    rmaInput$n2i <- as.name(options[["sampleSizeGroup2"]])
  } else if (measureCategory == "events") {
    rmaInput$x1i <- as.name(options[["eventsGroup1"]])
    rmaInput$x2i <- as.name(options[["eventsGroup2"]])
    rmaInput$t1i <- as.name(options[["personTimeGroup1"]])
    rmaInput$t2i <- as.name(options[["personTimeGroup2"]])
  }

  rmaInput$measure <- options[["effectSizeMeasure"]]
  rmaInput$method  <- .maGetMethodOptions(options)
  rmaInput$test    <- options[["fixedEffectTest"]]
  rmaInput$level   <- 100 * options[["confidenceIntervalsLevel"]]

  # model type
  rmaInput$model <- options[["glmmModel"]]

  # moderators
  rmaInput$mods <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])

  # model-specific parameters
  if (options[["glmmModel"]] %in% c("UM.FS", "UM.RS"))
    rmaInput$coding <- as.numeric(options[["glmmCoding"]])

  if (options[["glmmModel"]] == "UM.RS")
    rmaInput$cor <- options[["glmmCorrelatedEffects"]]

  rmaInput$nAGQ <- options[["glmmQuadraturePoints"]]

  # zero-cell handling
  rmaInput$add    <- options[["advancedAdd"]]
  rmaInput$to     <- switch(
    options[["advancedTo"]],
    "all"       = "all",
    "onlyZero"  = "only0",
    "ifAnyZero" = "if0all",
    "none"      = "none"
  )
  rmaInput$drop00 <- options[["advancedDropStudiesWithNoCasesOrEvents"]] == "yes"

  # extend call
  if (options[["advancedExtendMetaforCall"]])
    rmaInput <- c(rmaInput, .maExtendMetaforCallFromOptions(options))

  # fit
  if (nrow(dataset) < 2) {
    fit <- try(stop("Fewer than two estimates."))
  } else {
    fit <- try(do.call(metafor::rma.glmm, rmaInput))
  }

  attr(fit, "subgroup") <- paste0(subgroupName)
  attr(fit, "dataset")  <- dataset

  return(list(
    fit          = fit,
    fitClustered = NULL
  ))
}

# heterogeneity
.maglmmComputePooledHeterogeneity <- function(fit, options) {

  heterogeneity <- data.frame(
    par = c("\U1D70F", "\U1D70F\U00B2", "I\U00B2", "H\U00B2"),
    est = c(sqrt(fit[["tau2"]]), fit[["tau2"]], fit[["I2"]], fit[["H2"]]),
    lCi = c(sqrt(fit[["ci.lb.tau2"]]), fit[["ci.lb.tau2"]], NA, NA),
    uCi = c(sqrt(fit[["ci.ub.tau2"]]), fit[["ci.ub.tau2"]], NA, NA)
  )

  if (options[["standardErrors"]] && !is.null(fit[["se.tau2"]]))
    heterogeneity$se <- c(.maGetSqrtTransformationSeDeltaMethod(fit[["tau2"]], fit[["se.tau2"]]), fit[["se.tau2"]], NA, NA)

  # keep only the requested parameters
  heterogeneityShow <- c(
    if (options[["heterogeneityTau"]])  1,
    if (options[["heterogeneityTau2"]]) 2,
    if (options[["heterogeneityI2"]])   3,
    if (options[["heterogeneityH2"]])   4
  )

  heterogeneity <- heterogeneity[heterogeneityShow, , drop = FALSE]

  if (!options[["confidenceIntervals"]])
    heterogeneity <- heterogeneity[, c("par", "est")]

  return(heterogeneity)
}

.maglmmComputePooledHeterogeneityPlot <- function(fit, options, parameter = "tau") {

  options[["heterogeneityTau"]]  <- parameter == "tau"
  options[["heterogeneityTau2"]] <- parameter == "tau2"
  options[["heterogeneityI2"]]   <- parameter == "I2"
  options[["heterogeneityH2"]]   <- parameter == "H2"

  confIntHeterogeneity <- .maglmmComputePooledHeterogeneity(fit, options)

  return(confIntHeterogeneity)
}

.maglmmPrintHeterogeneityEstimate <- function(fit, options, digits, parameter) {

  out <- .maglmmComputePooledHeterogeneityPlot(fit, options, parameter)

  return(sprintf(paste0(
    "%1$s  = ",
    "%2$.", digits, "f"
  ), out$par, out$est))
}

# heterogeneity test rows (Wald + LRT)
.maglmmRowHeterogeneityTest <- function(fit, options) {

  # handle missing subfits
  if (jaspBase::isTryError(fit)) {
    return(data.frame(
      subgroup = rep(attr(fit, "subgroup"), 2),
      test     = c(gettext("Heterogeneity (Wald)"), gettext("Heterogeneity (LRT)"))
    ))
  }

  rows <- data.frame(
    subgroup = rep(attr(fit, "subgroup"), 2),
    test     = c(
      if (.maIsMetaregression(options)) gettext("Residual heterogeneity (Wald)") else gettext("Heterogeneity (Wald)"),
      if (.maIsMetaregression(options)) gettext("Residual heterogeneity (LRT)")  else gettext("Heterogeneity (LRT)")
    ),
    stat = c(
      sprintf(paste0("Q_W(%1$i) = ", if (fit[["QE.Wld"]] < 1e5) "%2$.2f" else "%2$.3g"), fit[["QE.df"]], fit[["QE.Wld"]]),
      sprintf(paste0("Q_LR(%1$i) = ", if (fit[["QE.LRT"]] < 1e5) "%2$.2f" else "%2$.3g"), fit[["QE.df"]], fit[["QE.LRT"]])
    ),
    pval = c(fit[["QEp.Wld"]], fit[["QEp.LRT"]])
  )

  return(rows)
}

# metafor R code display
.maglmmShowMetaforRCode <- function(jaspResults, options) {

  if (!.maReady(options) || !is.null(jaspResults[["metaforRCode"]]))
    return()

  metaforRCode <- createJaspHtml(title = gettext("Metafor R Code"))
  metaforRCode$dependOn(c(.maDependencies, "showMetaforRCode"))
  metaforRCode$position <- 99

  metaforRCode$text <- .maTransformToHtml(.maglmmMakeMetaforCallText(options))

  jaspResults[["metaforRCode"]] <- metaforRCode

  return()
}

.maglmmMakeMetaforCallText <- function(options) {

  measureCategory <- .maglmmGetMeasureCategory(options)

  rmaInput <- list()

  if (measureCategory == "twoByTwo") {
    rmaInput$ai  <- as.name(options[["successesGroup1"]])
    rmaInput$ci  <- as.name(options[["successesGroup2"]])
    rmaInput$n1i <- as.name(options[["sampleSizeGroup1"]])
    rmaInput$n2i <- as.name(options[["sampleSizeGroup2"]])
  } else if (measureCategory == "events") {
    rmaInput$x1i <- as.name(options[["eventsGroup1"]])
    rmaInput$x2i <- as.name(options[["eventsGroup2"]])
    rmaInput$t1i <- as.name(options[["personTimeGroup1"]])
    rmaInput$t2i <- as.name(options[["personTimeGroup2"]])
  }

  rmaInput$data    <- as.name("dataset")
  rmaInput$measure <- paste0("'", options[["effectSizeMeasure"]], "'")
  rmaInput$method  <- paste0("'", .maGetMethodOptions(options), "'")
  rmaInput$test    <- paste0("'", options[["fixedEffectTest"]], "'")
  rmaInput$level   <- 100 * options[["confidenceIntervalsLevel"]]

  rmaInput$model <- paste0("'", options[["glmmModel"]], "'")

  # moderators
  mods <- .maGetFormula(options[["effectSizeModelTerms"]], options[["effectSizeModelIncludeIntercept"]])
  if (!is.null(mods))
    rmaInput$mods <- paste0("'", deparse(mods), "'")

  rmaInput$nAGQ <- options[["glmmQuadraturePoints"]]

  fit <- paste0("fit <- rma.glmm(\n\t", paste(names(rmaInput), "=", rmaInput, collapse = ",\n\t"), "\n)\n")

  return(fit)
}

# compute observed effect sizes from raw data via escalc
.maglmmEscalc <- function(dataset, options) {

  measureCategory <- .maglmmGetMeasureCategory(options)
  escalcArgs      <- list(measure = options[["effectSizeMeasure"]])

  if (measureCategory == "twoByTwo") {
    escalcArgs$ai  <- dataset[[options[["successesGroup1"]]]]
    escalcArgs$ci  <- dataset[[options[["successesGroup2"]]]]
    escalcArgs$n1i <- dataset[[options[["sampleSizeGroup1"]]]]
    escalcArgs$n2i <- dataset[[options[["sampleSizeGroup2"]]]]
  } else if (measureCategory == "events") {
    escalcArgs$x1i <- dataset[[options[["eventsGroup1"]]]]
    escalcArgs$x2i <- dataset[[options[["eventsGroup2"]]]]
    escalcArgs$t1i <- dataset[[options[["personTimeGroup1"]]]]
    escalcArgs$t2i <- dataset[[options[["personTimeGroup2"]]]]
  }

  # zero-cell handling
  escalcArgs$add    <- options[["advancedAdd"]]
  escalcArgs$to     <- switch(
    options[["advancedTo"]],
    "all"       = "all",
    "onlyZero"  = "only0",
    "ifAnyZero" = "if0all",
    "none"      = "none"
  )
  escalcArgs$drop00 <- options[["advancedDropStudiesWithNoCasesOrEvents"]] == "yes"

  return(do.call(metafor::escalc, escalcArgs))
}

.maglmmGetSampleSize <- function(dataset, options) {

  measureCategory <- .maglmmGetMeasureCategory(options)

  if (measureCategory == "twoByTwo") {
    return(dataset[[options[["sampleSizeGroup1"]]]] + dataset[[options[["sampleSizeGroup2"]]]])
  } else if (measureCategory == "events") {
    return(dataset[[options[["personTimeGroup1"]]]] + dataset[[options[["personTimeGroup2"]]]])
  }
}
