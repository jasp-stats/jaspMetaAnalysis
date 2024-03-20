
initOpts <- function(analysisName) {
  options <- jaspTools::analysisOptions(analysisName)
  options <- addCommonQMLoptions(options)
  return(options)
}

addCommonQMLoptions <- function(options) {
  # jaspTools doesn't recognize common QML elements so this function adds the defaults manually
  root <- testthat::test_path(file.path("..", "..", "inst", "qml", "qml_components"))
  c(
    options,
    jaspTools:::readQML(file.path(root, "RobustBayesianMetaAnalysisDiagnostics.qml")),
    jaspTools:::readQML(file.path(root, "RobustBayesianMetaAnalysisInference.qml")),
    jaspTools:::readQML(file.path(root, "RobustBayesianMetaAnalysisPlots.qml"))
  )
}
