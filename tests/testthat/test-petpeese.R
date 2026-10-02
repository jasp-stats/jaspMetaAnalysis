test_that("PET-PEESE Fisher-z means are back-transformed once", {
  dataset <- data.frame(
    r = c(.45, .55, .65, .50, .60),
    n = c(20, 35, 60, 90, 150)
  )
  opts <- jaspTools::analysisOptions("PetPeese")
  opts$effectSize <- "r"
  opts$sampleSize <- "n"
  opts$effectSizeSe <- ""
  opts$measures <- "correlation"
  opts$transformCorrelationsTo <- "fishersZ"
  opts$inferenceMeanEstimatesTable <- TRUE

  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  result <- jaspTools::runAnalysis(
    "PetPeese", encoded$dataset, encoded$options,
    encodedDataset = TRUE, view = FALSE
  )

  expect_equal(result$status, "complete")
  rows <- result$results$estimates$collection$estimates_mean$data
  expect_equal(vapply(rows, function(row) row$type, character(1)), c("PET", "PEESE"))

  z <- atanh(dataset$r)
  se <- 1 / sqrt(dataset$n - 3)
  refs <- list(
    lm(z ~ se, weights = dataset$n - 3),
    lm(z ~ I(se^2), weights = dataset$n - 3)
  )

  for (i in seq_along(refs)) {
    intercept <- unname(coef(refs[[i]])[1])
    expect_equal(rows[[i]]$est, tanh(intercept), tolerance = 1e-6)

    interceptSE <- summary(refs[[i]])$coefficients[1, 2]
    ci <- tanh(intercept + qnorm(c(.025, .975)) * interceptSE)
    expect_equal(rows[[i]]$lowerCI, unname(ci[1]), tolerance = 1e-6)
    expect_equal(rows[[i]]$upperCI, unname(ci[2]), tolerance = 1e-6)
  }
})
