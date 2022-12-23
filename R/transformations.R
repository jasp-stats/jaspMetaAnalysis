# common effect size transformations for using correlations input in selection models and PET-PEESE
.maGetInputEs              <- function(dataset, options) {

  if (!is.null(options[["modelExpectedDirectionOfEffectSizes"]]) && options[["modelExpectedDirectionOfEffectSizes"]] == "negative")
    dataset[, options[["effectSize"]]] <- dataset[, options[["effectSize"]]] * - 1

  return(switch(
    options[["measures"]],
    "general"     = dataset[, options[["effectSize"]]],
    "correlation" = .maTransformEs(dataset[, options[["effectSize"]]], options[["transformCorrelationsTo"]])
  ))
}
.maGetInputSe              <- function(dataset, options) {
  return(switch(
    options[["measures"]],
    "general"     = dataset[, options[["effectSizeSe"]]],
    "correlation" = .maComputeSe(dataset[, options[["effectSize"]]], dataset[, options[["sampleSize"]]], options[["transformCorrelationsTo"]])
  ))
}
.maGetInputPVal            <- function(dataset, options) {
  # weightfunc uses one-sided p-values as input!
  if (options[["pValue"]] == "")
    return(pnorm(.maGetInputEs(dataset, options) /.maGetInputSe(dataset, options), lower.tail = FALSE))
  else
    return(dataset[, options[["pValue"]]])
}
.maTransformEs             <- function(es, transformation) {
  switch(
    transformation,
    "cohensD"  = RoBMA::r2d(r = es),
    "fishersZ" = RoBMA::r2z(r = es)
  )
}
.maTransformSe             <- function(es, se, transformation) {
  switch(
    transformation,
    "cohensD"  = RoBMA::se_r2se_d(se_r = se, r = es),
    "fishersZ" = RoBMA::se_r2se_z(se_r = se, r = es)
  )
}
.maComputeSe               <- function(es, n, transformation) {
  switch(
    transformation,
    "cohensD"  = RoBMA::se_d(d = RoBMA::r2d(es), n = n),
    "fishersZ" = RoBMA::se_z(n = n)
  )
}
.maInvTransformEs          <- function(es, transformation) {
  switch(
    transformation,
    "cohensD"  = RoBMA::d2r(d = es),
    "fishersZ" = RoBMA::z2r(z = es)
  )
}
.maInvTransformSe          <- function(es, se, transformation) {
  switch(
    transformation,
    "cohensD"  = RoBMA::se_d2se_r(se_d = se, d = es),
    "fishersZ" = RoBMA::se_z2se_r(se_z = se, z = es)
  )
}
