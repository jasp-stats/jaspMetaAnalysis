# Focused review regressions, run separately from the human-owned output tests.
# Run from the repository root after loading the module with pkgload::load_all().

selectionFunction <- function(name) get(name, asNamespace("jaspMetaAnalysis"))
selectionFile <- "tests/testthat/jaspfiles/other/Selection Models - bcg.jasp"
selectionOptions <- jaspTools::analysisOptions(selectionFile)[[1]]
selectionData <- jaspTools::extractDatasetFromJASPFile(selectionFile)
selectionOptions[["analysis"]] <- "selectionModels"
selectionElement <- list(
  name = "model1", type = "stepfun", steps = "(.025, .50)", delta = "1, .5, .2",
  sidedness = "oneSided", prec = "none", sampleSize = "", scaleprec = TRUE,
  decreasing = FALSE
)

testthat::test_that("NULL exports are skipped and warnings remain captured", {
  capture <- selectionFunction(".smCapture")
  testthat::expect_null(capture(NULL))
  testthat::expect_null(capture({ warning("empty export"); NULL }))
  value <- capture({ warning("fit warning"); 1 })
  testthat::expect_identical(attr(value, "selectionWarnings"), "fit warning")
  testthat::expect_s3_class(capture(stop("fit error")), "try-error")
})

testthat::test_that("supplied p-values reproduce computed fits and affect selection", {
  for (sidedness in c("oneSided", "twoSided")) {
    for (direction in c("positive", "negative")) {
      opts <- selectionOptions
      opts[["modelExpectedDirectionOfTheEffect"]] <- direction
      opts[["pValue"]] <- ""
      element <- selectionElement
      element[["sidedness"]] <- sidedness
      spec <- selectionFunction(".smSpecification")(element)
      reference <- do.call(metafor::rma.uni, selectionFunction(".smBaseArguments")(selectionData, opts))
      computed <- selectionFunction(".smFitOne")(reference, spec, selectionData, opts)
      testthat::expect_s3_class(computed, "rma.uni.selmodel")

      data <- selectionData
      z <- data[[opts[["effectSize"]]]] / data[[opts[["effectSizeStandardError"]]]]
      data[["reviewPValue"]] <- if (sidedness == "twoSided") {
        2 * stats::pnorm(-abs(z))
      } else {
        stats::pnorm(if (direction == "positive") -z else z)
      }
      opts[["pValue"]] <- "reviewPValue"
      supplied <- selectionFunction(".smFitOne")(reference, spec, data, opts)
      testthat::expect_s3_class(supplied, "rma.uni.selmodel")
      for (field in c("beta", "tau2", "delta", "ll", "ptable"))
        testthat::expect_equal(supplied[[field]], computed[[field]], tolerance = 1e-7)

      # Equality alone would also pass if metafor silently ignored pval.
      data[["reviewPValue"]] <- rep(.01, nrow(data))
      changed <- selectionFunction(".smFitOne")(reference, spec, data, opts)
      testthat::expect_s3_class(changed, "rma.uni.selmodel")
      testthat::expect_false(isTRUE(all.equal(changed$ll, computed$ll, tolerance = 1e-7)))
    }
  }
})

testthat::test_that("frequency counts survive failed fits", {
  opts <- selectionOptions
  opts[["pValue"]] <- "reviewPValue"
  opts[["modelExpectedDirectionOfTheEffect"]] <- "positive"
  data <- selectionData
  data[["reviewPValue"]] <- rep(.7, nrow(data))
  element <- selectionElement
  element[["delta"]] <- ""
  spec <- selectionFunction(".smSpecification")(element)
  cache <- selectionFunction(".smFitCandidates")(NULL, data, opts, list(model1 = spec), "Full dataset")
  fit <- cache$models$model1$fit
  testthat::expect_s3_class(fit, "try-error")
  testthat::expect_equal(as.numeric(attr(fit, "selectionFrequencies")$k), c(0, 0, nrow(data)))

  # Zero fixed weights induce an optimizer error without invalidating cutoffs.
  element[["delta"]] <- "1, 0, 0"
  spec <- selectionFunction(".smSpecification")(element)
  data[["reviewPValue"]] <- rep(c(.01, .1, .7), length.out = nrow(data))
  cache <- selectionFunction(".smFitCandidates")(NULL, data, opts, list(model1 = spec), "Full dataset")
  fit <- cache$models$model1$fit
  testthat::expect_s3_class(fit, "try-error")
  testthat::expect_equal(sum(attr(fit, "selectionFrequencies")$k), nrow(data))
})

testthat::test_that("failed models display their frequency counts", {
  opts <- selectionOptions
  opts[["publicationBiasAdjustment"]] <- "custom"
  opts[["weightFunctionPValueFrequencyTable"]] <- TRUE
  opts[["pValue"]] <- "reviewPValue"
  opts[["pValue.types"]] <- "scale"
  opts[[".meta"]][["pValue"]] <- list(shouldEncode = TRUE)
  opts[["confidenceIntervals"]] <- FALSE
  opts[["profileLikelihood"]] <- FALSE
  opts[["profileLikelihoodSelection"]] <- FALSE
  opts[["profileLikelihoodHeterogeneity"]] <- FALSE
  data <- selectionData
  data[["reviewPValue"]] <- rep(.7, nrow(data))
  for (parameters in c("", "1, 0, 0")) {
    element <- selectionElement
    element[["delta"]] <- parameters
    opts[["selectionModels"]] <- list(element)
    encoded <- jaspTools:::encodeOptionsAndDataset(opts, data)
    set.seed(1)
    result <- jaspTools::runAnalysis("SelectionModels", encoded$dataset, encoded$options,
                                    encodedDataset = TRUE, view = FALSE)
    testthat::expect_identical(result$status, "complete")
    frequency <- result$results$weightFunctionSummary$collection$weightFunctionSummary_selectionFrequencies
    testthat::expect_equal(vapply(frequency$data, `[[`, numeric(1), "count"), c(0, 0, nrow(data)))
  }
})

testthat::test_that("missing sample-size columns have an actionable error", {
  element <- selectionElement
  element[["type"]] <- "halfnorm"
  element[["steps"]] <- ""
  element[["delta"]] <- ""
  element[["prec"]] <- "ninv"
  element[["sampleSize"]] <- "deletedVariable"
  spec <- selectionFunction(".smSpecification")(element)
  testthat::expect_error(
    selectionFunction(".smCheckData")(selectionData, selectionOptions, list(model1 = spec)),
    "Sample-size variable not found in the data", fixed = TRUE
  )
})

testthat::test_that("RoBMA printing survives a conflicting prior method", {
  prior <- BayesTools::prior("normal", list(mean = 0, sd = 1))
  printer <- utils::getS3method("print", "prior", envir = asNamespace("BayesTools"))
  expected <- lapply(c(FALSE, TRUE), function(shortName) printer(prior, short_name = shortName, silent = TRUE))
  previous <- utils::getS3method("print", "prior")
  tryCatch({
    registerS3method("print", "prior", function(...) stop("conflicting prior printer"))
    for (i in seq_along(expected))
      testthat::expect_identical(selectionFunction(".robmaPrintPrior")(prior, c(FALSE, TRUE)[i]), expected[[i]])
  }, finally = registerS3method("print", "prior", previous))
})
