# common effect size transformations for using correlations input in selection models and PET-PEESE
.maGetInputEs              <- function(dataset, options) {

  if (!is.null(options[["effectDirection"]]) && options[["effectDirection"]] == "negative")
    dataset[, options[["inputES"]]] <- dataset[, options[["inputES"]]] * - 1

  return(switch(
    options[["measures"]],
    "general"     = dataset[, options[["inputES"]]],
    "correlation" = .maTransformEs(dataset[, options[["inputES"]]], options[["muTransform"]])
  ))
}
.maGetInputSe              <- function(dataset, options) {
  return(switch(
    options[["measures"]],
    "general"     = dataset[, options[["inputSE"]]],
    "correlation" = .maComputeSe(dataset[, options[["inputES"]]], dataset[, options[["inputN"]]], options[["muTransform"]])
  ))
}
.maGetInputPVal            <- function(dataset, options) {
  # weightfunc uses one-sided p-values as input!
  if (options[["inputPVal"]] == "")
    return(pnorm(.maGetInputEs(dataset, options) /.maGetInputSe(dataset, options), lower.tail = FALSE))
  else
    return(dataset[, .v(options[["inputPVal"]])])
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
