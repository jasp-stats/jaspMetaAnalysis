context("Multilevel/Multivariate Meta-Analysis - variance-covariance matrix files")

# The multilevel example data (moderator deltype) with estimates nested in studies and sampling errors
# correlated .6 within studies. The references are direct metafor fits with the same matrix.
jaspFile <- testthat::test_path("jaspfiles", "other", "Multivariate and Multilevel Meta-Analysis - dat.assink2016.jasp")
dataset  <- jaspTools::extractDatasetFromJASPFile(jaspFile)
V        <- metafor::vcalc(vi = sei^2, cluster = study, obs = esid, rho = 0.6, data = dataset)

.mammVcovReference <- function(V, rows = rep(TRUE, nrow(dataset))) {
  fit <- metafor::rma.mv(yi, V[rows, rows], mods = ~ deltype, random = ~ 1 | study / esid,
                         data = droplevels(dataset[rows, ]), test = "t")
  data.frame(est = unname(fit$beta[, 1]), se = unname(fit$se))
}

.mammVcovRun <- function(...) {
  options <- jaspTools::analysisOptions(jaspFile)[[1]]
  options[["randomEffectsSpecification"]][[1]][c("level1", "level2")]       <- list("study", "esid")
  options[["randomEffectsSpecification.types"]][[1]][c("level1", "level2")] <- list("nominal", "nominal")
  changes <- list(...)
  options[names(changes)] <- changes
  encoded <- jaspTools:::encodeOptionsAndDataset(options, dataset)
  jaspTools::runAnalysis("ClassicalMetaAnalysisMultilevelMultivariate", encoded$dataset, encoded$options, encodedDataset = TRUE)
}

.mammVcovCoefficients <- function(results, subgroup = NULL) {
  rows <- results[["results"]][["metaregressionContainer"]][["collection"]][["metaregressionContainer_effectSizeCoefficientTable"]][["data"]]
  if (!is.null(subgroup))
    rows <- Filter(function(row) identical(row[["subgroup"]], subgroup), rows)
  data.frame(est = vapply(rows, `[[`, numeric(1), "est"), se = vapply(rows, `[[`, numeric(1), "se"))
}

# the matrix computed from the within-study correlation of the estimates
constructOptions <- list(
  varianceCovarianceMatrixType                            = "constructsGroupsTimes",
  varianceCovarianceMatrixCluster                         = "study",
  varianceCovarianceMatrixCluster.types                   = "nominal",
  varianceCovarianceMatrixConstruct                       = "esid",
  varianceCovarianceMatrixConstruct.types                 = "nominal",
  varianceCovarianceMatrixConstructCorrelationMatrixValue = 0.6
)

fileComma     <- tempfile(fileext = ".csv")
fileSemicolon <- tempfile(fileext = ".csv")
utils::write.table(V, fileComma,     sep = ",",             row.names = FALSE, col.names = FALSE)
utils::write.table(V, fileSemicolon, sep = ";", dec = ",", row.names = FALSE, col.names = FALSE)

test_that("Precomputed matrix from a comma-separated file matches metafor", {
  results <- .mammVcovRun(varianceCovarianceMatrixType = "precomputed", varianceCovarianceMatrixFile = fileComma)
  expect_equal(.mammVcovCoefficients(results), .mammVcovReference(V), tolerance = 1e-6)
})

test_that("Precomputed matrix from a semicolon-separated file with decimal commas matches metafor", {
  results <- .mammVcovRun(varianceCovarianceMatrixType = "precomputed", varianceCovarianceMatrixFile = fileSemicolon)
  expect_equal(.mammVcovCoefficients(results), .mammVcovReference(V), tolerance = 1e-6)
})

test_that("Subgroup analyses use the matching block of the precomputed matrix", {
  results <- .mammVcovRun(varianceCovarianceMatrixType = "precomputed", varianceCovarianceMatrixFile = fileComma,
                          subgroup = "pubstatus", subgroup.types = "nominal")
  for (level in c("0", "1"))
    expect_equal(.mammVcovCoefficients(results, level), .mammVcovReference(V, dataset$pubstatus == as.numeric(level)), tolerance = 1e-6)
})

test_that("Computed matrix is saved to the selected file", {
  fileSaved <- tempfile(fileext = ".csv")
  results   <- do.call(.mammVcovRun, c(constructOptions, varianceCovarianceMatrixSaveComputedVarianceCovarianceMatrix = fileSaved))
  expect_equal(.mammVcovCoefficients(results), .mammVcovReference(V), tolerance = 1e-6)
  expect_equal(unname(as.matrix(utils::read.csv(fileSaved, header = FALSE))), matrix(as.numeric(V), nrow(V)), tolerance = 1e-10)
})

test_that("Saving the computed matrix to an unwritable location reports an error", {
  results <- do.call(.mammVcovRun, c(constructOptions, varianceCovarianceMatrixSaveComputedVarianceCovarianceMatrix = file.path(tempfile(), "V.csv")))
  expect_identical(results[["status"]], "validationError")
  expect_match(results[["results"]][["errorMessage"]], "Error writing the variance-covariance matrix file", fixed = TRUE)
})

test_that("Construct-type correlation matrix from a semicolon-separated file matches metafor", {
  # correlations between the three delinquency types, with decimal commas
  rho  <- matrix(c(1, .5, .3, .5, 1, .4, .3, .4, 1), 3, dimnames = rep(list(c("covert", "general", "overt")), 2))
  file <- tempfile(fileext = ".csv")
  utils::write.csv2(rho, file)
  results <- do.call(.mammVcovRun, c(constructOptions, list(
    varianceCovarianceMatrixConstructType                         = "deltype",
    varianceCovarianceMatrixConstructType.types                   = "nominal",
    varianceCovarianceMatrixConstructTypeCorrelationMatrix        = "correlationMatrix",
    varianceCovarianceMatrixConstructTypeCorrelationMatrixFilePath = file
  )))
  VType <- metafor::vcalc(vi = sei^2, cluster = study, obs = esid, type = deltype, rho = list(0.6, rho), data = dataset)
  expect_equal(.mammVcovCoefficients(results), .mammVcovReference(VType), tolerance = 1e-6)
})
