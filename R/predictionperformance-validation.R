# Prediction-performance validation and option helpers.
#
# Checks readiness and data and maps JASP measure options to metamisc.

.metamiscReady               <- function(options) {

  if (options[["effectSize"]] != "" && options[["effectSizeSe"]] != "")
    return(TRUE)

  if (options[["effectSize"]] != "" && sum(unlist(options[["effectSizeCi"]]) != "") == 2)
    return(TRUE)

  if (options[["measure"]] == "cStatistic" && options[["effectSize"]] != "" && options[["numberOfParticipants"]] != "" && options[["numberOfObservedEvents"]] != "")
    return(TRUE)

  if (options[["measure"]] == "oeRatio" && options[["numberOfExpectedEvents"]] != "" && options[["numberOfObservedEvents"]] != "")
    return(TRUE)

  return(FALSE)
}

.metamiscCheckData           <- function(options, dataset) {

  varNames <- c(options[["effectSize"]], options[["effectSizeSe"]], unlist(options[["effectSizeCi"]]),
                options[["numberOfParticipants"]], options[["numberOfObservedEvents"]], options[["numberOfExpectedEvents"]])
  varNames <- varNames[varNames != ""]

  .hasErrors(dataset               = dataset[,varNames],
             type                  = c("infinity", "observations", "negativeValues"),
             observations.amount   = "< 2",
             exitAnalysisIfErrors  = TRUE)

  .hasErrors(dataset              = dataset,
             seCheck.target       = varNames[varNames %in% c(options[["effectSizeSe"]],options[["numberOfParticipants"]])],
             custom               = .maCheckStandardErrors,
             exitAnalysisIfErrors = TRUE)

  return()
}

.metamiscGetMeasureOption <- function(options){
  return(switch(
    options[["measure"]],
    "oeRatio"    = "OE",
    "cStatistic" = "cstat"
  ))
}
