context("Classical Meta-Analysis - location-scale prediction intervals")

.maLocationScaleOptions <- function(level = 0.95, tau = TRUE, tau2 = TRUE) {
  list(
    analysis                           = "metaAnalysis",
    method                             = "restrictedML",
    effectSizeModelTerms               = list(),
    heterogeneityModelTerms            = list(list(components = "ablat")),
    heterogeneityModelIncludeIntercept = TRUE,
    heterogeneityModelLink             = "log",
    confidenceIntervalsLevel           = level,
    predictionIntervals                = TRUE,
    heterogeneityTau                   = tau,
    heterogeneityTau2                  = tau2,
    transformEffectSize                = "none"
  )
}

testthat::test_that("location-scale pooled PI does not depend on heterogeneity display checkboxes", {
  dat <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = metadat::dat.bcg)
  fit <- metafor::rma(yi, vi, scale = ~ ablat, data = dat, level = 95)

  tau2Mean <- exp(stats::predict(fit, newscale = matrix(colMeans(stats::model.matrix(fit)$scale), nrow = 1), level = 95)$pred)
  crit     <- stats::qnorm(0.975)
  expected <- fit$beta[1] + c(-1, 1) * crit * sqrt(fit$se[1]^2 + tau2Mean)

  for (flags in list(c(TRUE, TRUE), c(FALSE, TRUE), c(FALSE, FALSE))) {
    opts <- .maLocationScaleOptions(level = 0.95, tau = flags[1], tau2 = flags[2])
    got  <- jaspMetaAnalysis:::.maComputePooledEffect(fit, opts, returnRaw = TRUE)
    testthat::expect_equal(c(got$pi.lb, got$pi.ub), expected, tolerance = 1e-4)
  }
})

testthat::test_that("location-scale pooled PI respects confidenceIntervalsLevel and t/knha critical values", {
  dat <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = metadat::dat.bcg)

  for (testType in c("z", "t", "knha")) {
    for (level in c(0.90, 0.95, 0.99)) {
      fit      <- metafor::rma(yi, vi, scale = ~ ablat, data = dat, level = 100 * level, test = testType)
      tau2Mean <- exp(stats::predict(fit, newscale = matrix(colMeans(stats::model.matrix(fit)$scale), nrow = 1), level = 100 * level)$pred)
      alpha    <- 1 - level
      crit     <- if (testType == "z") stats::qnorm(alpha / 2, lower.tail = FALSE) else stats::qt(alpha / 2, df = fit$ddf[1], lower.tail = FALSE)
      expected <- fit$beta[1] + c(-1, 1) * crit * sqrt(fit$se[1]^2 + tau2Mean)

      opts <- .maLocationScaleOptions(level = level, tau = TRUE, tau2 = TRUE)
      got  <- jaspMetaAnalysis:::.maComputePooledEffect(fit, opts, returnRaw = TRUE)
      testthat::expect_equal(c(got$pi.lb, got$pi.ub), expected, tolerance = 1e-4)
    }
  }
})
