# Classical meta-analysis option helpers.
#
# Contains analysis predicates, option extraction, validation, and display-name mapping.

.maIsGLMM                         <- function(options) {
  options[["analysis"]] == "generalizedMetaAnalysis"
}

.maIsMetaregression               <- function(options) {
  return(.maIsMetaregressionEffectSize(options) || .maIsMetaregressionHeterogeneity(options))
}

.maIsMetaregressionEffectSize     <- function(options) {
  return(!is.null(options[["effectSizeModelTerms"]]) && length(options[["effectSizeModelTerms"]]) > 0)
}

.maIsMetaregressionHeterogeneity  <- function(options) {
  return(!.maIsUnrestrictedWeightedLeastSquares(options) && !is.null(options[["heterogeneityModelTerms"]]) && length(options[["heterogeneityModelTerms"]]) > 0)
}

.maIsClustered                    <- function(options) {
  return(!is.null(options[["clustering"]]) && options[["clustering"]] != "")
}

.maIsMetaregressionFtest          <- function(options) {
  test <- .maGetFixedEffectTestOptions(options)
  return(test %in% c("knha", "t"))
}

.maIsUnrestrictedWeightedLeastSquares <- function(options) {
  return(!is.null(options[["method"]]) && options[["method"]] == "unrestrictedWeightedLeastSquares")
}

.maExtractDdf                     <- function(object) {
  # rma.glmm does not store ddf; compute as k - p per metafor docs
  if (!is.null(object[["ddf"]]))
    return(object[["ddf"]])
  if (!is.null(object[["k"]]) && !is.null(object[["p"]]))
    return(rep(object[["k"]] - object[["p"]], object[["p"]]))
  return(NULL)
}

.maIsMultilevelMultivariate       <- function(options) {
  return(options[["analysis"]] == "metaAnalysisMultilevelMultivariate")
}

.maIsPermutation                  <- function(options) {
  return(.maIsClassical(options, notMHP = TRUE) && !.maIsClustered(options) && options[["permutationTest"]])
}

.maCheckIsPossibleOptions         <- function(options) {

  if (length(options[["heterogeneityModelTerms"]]) > 0 && options[["clustering"]] != "") {
    return(gettext("Clustering is not supported when specifying a heterogeneity meta-regression model."))
  }

  return(NULL)
}

.maGetMethodOptions                             <- function(options) {

  # dummy return for Bayesian methods
  # simplifies further plotting dispatch in forest plot
  if (options[["analysis"]] %in% c("RoBMA", "NoBMA", "BiBMA")) {
    if (.robmaHasHeterogeneity(options))
      return("REML")
    else
      return("FE")
  }

  # GLMM only supports ML/FE (not EE)
  if (.maIsGLMM(options)) {
    return(switch(
      options[["method"]],
      "maximumLikelihood" = "ML",
      "equalEffects"      = "FE"
    ))
  }

  switch(
    options[["method"]],
    "equalEffects"       = "EE",
    "fixedEffects"       = "FE",
    "unrestrictedWeightedLeastSquares" = "FE",
    "maximumLikelihood"  = "ML",
    "restrictedML"       = "REML",
    "derSimonianLaird"   = "DL",
    "hedges"             = "HE",
    "hunterSchmidt"      = "HS",
    "hunterSchmidtSsc"   = "HSk",
    "sidikJonkman"       = "SJ",
    "empiricalBayes"     = "EB",
    "pauleMandel"        = "PM",
    "pauleMandelMu"      = "PMM",
    "qeneralizedQStat"   = "GENQ",
    "qeneralizedQStatMu" = "GENQM",
    "mantelHaenszelFrequencies" = "MH",
    "mantelHaenszelEvents"      = "MH",
    "peto"                      = "PETO",
    NA
  )
}

.maGetFixedEffectTestOptions                    <- function(options) {
  if (.maIsUnrestrictedWeightedLeastSquares(options))
    return("knha")

  if (identical(options[["analysis"]], "mantelHaenszelPeto"))
    return("z")

  return(options[["fixedEffectTest"]])
}

.maGetFixedTau2Options                          <- function(options) {

  tau2 <- .parseRCodeInOptions(options[["fixParametersTau2Value"]])

  if (!is.numeric(tau2) || length(tau2) != 1 || tau2 < 0)
    .quitAnalysis(gettext("The fixed value for tau2 must be a positive number."))
  else
    return(tau2)
}

.maGetControlOptions                            <- function(options) {

  if (.maIsMetaregressionHeterogeneity(options)) {
    if (options[["optimizerMethod"]] == "nlminb" && !options[["optimizerMaximumIterations"]] && !options[["optimizerConvergenceRelativeTolerance"]]) {
      # allow an empty list for default settings --- this allows manual modification of the control argument through extra input
      out <- list()
    } else {
      out <- list(
        optimizer = options[["optimizerMethod"]],
        iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
        rel.tol   = if (options[["optimizerConvergenceRelativeTolerance"]]) options[["optimizerConvergenceRelativeToleranceValue"]]
      )
    }
  } else {
    if (.maIsMultilevelMultivariate(options)) {
      if (options[["optimizerMethod"]] == "nlminb" && !options[["optimizerMaximumEvaluations"]] && !options[["optimizerMaximumIterations"]] && !options[["optimizerConvergenceRelativeTolerance"]]) {
        # allow an empty list for default settings --- this allows manual modification of the control argument through extra input
        out <- list()
      } else if (options[["optimizerMethod"]] == "nlminb") {
        out <- list(
          optimizer = options[["optimizerMethod"]],
          eval.max  = if (options[["optimizerMaximumEvaluations"]]) options[["optimizerMaximumEvaluationsValue"]],
          iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
          rel.tol   = if (options[["optimizerConvergenceRelativeTolerance"]]) options[["optimizerConvergenceRelativeToleranceValue"]]
        )
      } else if (options[["optimizerMethod"]] %in% c("Nelder-Mead", "BFGS")){
        out <- list(
          optimizer = options[["optimizerMethod"]],
          maxit     = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
          reltol    = if (options[["optimizerConvergenceRelativeTolerance"]]) options[["optimizerConvergenceRelativeToleranceValue"]]
        )
      } else if (options[["optimizerMethod"]] %in% c("uobyqa", "newuoa", "bobyqa")){
        out <- list(
          optimizer = options[["optimizerMethod"]],
          maxfun   = if (options[["optimizerMaximumEvaluations"]]) options[["optimizerMaximumEvaluationsValue"]],
          rhobeg   = if (options[["optimizerInitialTrustRegionRadius"]]) options[["optimizerInitialTrustRegionRadiusValue"]],
          rhoend   = if (options[["optimizerFinalTrustRegionRadius"]]) options[["optimizerFinalTrustRegionRadiusValue"]]
        )
      } else if (options[["optimizerMethod"]] %in% c("nloptr", "nlm")){
        # could be much more, "nloptr" probably requires choosing a method too
        out <- list(
          optimizer = options[["optimizerMethod"]],
          iterlim   = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]]
        )
      } else if (options[["optimizerMethod"]] %in% c("hjk", "nmk", "mads")){
        out <- list(
          optimizer    = options[["optimizerMethod"]],
          tol          = if (options[["optimizerConvergenceTolerance"]]) options[["optimizerConvergenceToleranceValue"]],
          maxfeval     = if (options[["optimizerMaximumEvaluations"]]) options[["optimizerMaximumEvaluationsValue"]],
          restarts.max = if (options[["optimizerMethod"]] == "mmk" && options[["optimizerMaximumRestarts"]]) options[["optimizerMaximumRestartsValue"]]
        )
      }
    } else {
      if (.maGetMethodOptions(options) %in% c("REML", "ML", "EB")) {
        out <- list(
          tau2.init = if (options[["optimizerInitialTau2"]]) options[["optimizerInitialTau2Value"]],
          iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
          threshold = if (options[["optimizerConvergenceTolerance"]]) options[["optimizerConvergenceToleranceValue"]],
          stepadj   = if (options[["optimizerStepAdjustment"]]) options[["optimizerStepAdjustmentValue"]]
        )
      } else if (.maGetMethodOptions(options) %in% c("PM", "PMM", "GENQM")) {
        out <- list(
          iter.max  = if (options[["optimizerMaximumIterations"]]) options[["optimizerMaximumIterationsValue"]],
          tol       = if (options[["optimizerConvergenceTolerance"]]) options[["optimizerConvergenceToleranceValue"]],
          tau2.min  = if (options[["optimizerMinimumTau2"]]) options[["optimizerMinimumTau2Value"]],
          tau2.max  = if (options[["optimizerMaximumTau2"]]) options[["optimizerMaximumTau2Value"]]
        )
      } else if (.maGetMethodOptions(options) %in% c("SD")) {
        out <- list(
          tau2.init = if (options[["optimizerInitialTau2"]]) options[["optimizerInitialTau2Value"]]
        )
      } else {
        out <- list()
      }
    }
  }
  return(out[!sapply(out, is.null)])
}

.maGetEffectSizeTransformationOptions           <- function(effectSizeTransformation) {

  switch(
    effectSizeTransformation,
    none                          = function(x) x,
    fishersZToCorrelation         = metafor::transf.ztor,
    exponential                   = exp,
    logOddsToProportions          = Vectorize(metafor::transf.ilogit),
    logOddsToSmdNormal            = metafor::transf.lnortod.norm,
    logOddsToSmdLogistic          = metafor::transf.lnortod.logis,
    smdToLogOddsNormal            = metafor::transf.dtolnor.norm,
    smdToLogOddsLogistic          = metafor::transf.dtolnor.logis,
    hakstianAndWhalenInverseAlpha = Vectorize(metafor::transf.iahw),
    bonettInverseAlpha            = Vectorize(metafor::transf.iabt),
    zToR2                         = metafor::transf.ztor2,
    smdToCohensU1                 = Vectorize(metafor::transf.dtou1),
    smdToCohensU2                 = Vectorize(metafor::transf.dtou2),
    smdToCohensU3                 = Vectorize(metafor::transf.dtou3),
    smdToCles                     = Vectorize(metafor::transf.dtocles),
    stop(paste0("Unknown effect size transformation: ", effectSizeTransformation))
  )
}

.maExtendMetaforCallFromOptions                 <- function(options) {

  optionsCode <- options[["advancedExtendMetaforCallCode"]]
  optionsCode <- trimws(optionsCode, which = "both")
  if (substr(optionsCode, 1, 4) != "list")
    optionsCode <- paste0("list(\n", optionsCode, "\n)")
  optionsCode <- try(eval(parse(text = optionsCode)))

  if (jaspBase::isTryError(optionsCode))
    .quitAnalysis(gettextf("The custom R code for extending the metafor call failed with the following message: %1$s", optionsCode))

  return(optionsCode)
}

.maGetPValueAdjustment                          <- function(pValueAdjustment) {
  return(switch(
    pValueAdjustment,
    "none"               = "none",
    "bonferroni"         = "bonferroni",
    "holm"               = "holm",
    "hochberg"           = "hochberg",
    "hommel"             = "hommel",
    "benjaminiHochberg"  = "BH",
    "benjaminiYekutieli" = "BY"
  ))
}

.maGetEstimatedMarginalMeansAndContrastsOptions <- function(options){

  return(options[c(
    "contrastsHeterogeneityPValueAdjustment",
    "estimatedMarginalMeansHeterogeneityTransformation",
    "estimatedMarginalMeansHeterogeneitySdFactorCovariates",
    "estimatedMarginalMeansHeterogeneityAddAdjustedEstimate",

    "contrastsEffectSizePValueAdjustment",
    "estimatedMarginalMeansEffectSizeTestAgainst",
    "estimatedMarginalMeansEffectSizeTestAgainstValue",
    "estimatedMarginalMeansEffectSizeSdFactorCovariates",
    "estimatedMarginalMeansEffectSizeAddAdjustedEstimate",

    "standardErrors",
    "confidenceIntervals",
    "confidenceIntervalsLevel",
    "predictionIntervals",
    "transformEffectSize"
  )])
}

.maGetOptionsNameEffectSizeTransformation <- function(effectSizeTransformation) {

  return(switch(
    effectSizeTransformation,
    "none"                           = NULL,
    "fishersZToCorrelation"          = gettext("Fisher's z to r"),
    "exponential"                    = gettext("exponential"),
    "logOddsToProportions"           = gettext("log odds to proportions"),
    "logOddsToSmdNormal"             = gettext("log odds to SMD (normal)"),
    "logOddsToSmdLogistic"           = gettext("log odds to SMD (logistic)"),
    "smdToLogOddsNormal"             = gettext("SMD to log odds (normal)"),
    "smdToLogOddsLogistic"           = gettext("SMD to log odds (logistic)"),
    "hakstianAndWhalenInverseAlpha"  = gettext("Hakstian & Whalen inverse α"),
    "bonettInverseAlpha"             = gettext("Bonett inverse α"),
    "zToR2"                          = gettext("z to R²"),
    "smdToCohensU1"                  = gettext("SMD to Cohen's U₁"),
    "smdToCohensU2"                  = gettext("SMD to Cohen's U₂"),
    "smdToCohensU3"                  = gettext("SMD to Cohen's U₃"),
    "smdToCles"                      = gettext("SMD to CLES, Pr(superiority)")
  ))
}

.maGetOptionsNamePValueAdjustment         <- function(pValueAdjustment) {

  return(switch(
    pValueAdjustment,
    "none"               = gettext("None"),
    "bonferroni"         = gettext("Bonferroni"),
    "holm"               = gettext("Holm"),
    "hochberg"           = gettext("Hochberg"),
    "hommel"             = gettext("Hommel"),
    "benjaminiHochberg"  = gettext("Benjamini-Hochberg"),
    "benjaminiYekutieli" = gettext("Benjamini-Yekutieli")
  ))
}

.maCasewiseDiagnosticsNames               <- function() {
  return(c(
    "rstudent",
    "dffits",
    "cook.d",
    "cov.r",
    "tau.del",
    "tau2.del",
    "QE.del",
    "hat",
    "weight",
    "inf"
  ))
}
