# Selection-model plots.
#
# Defines selection-function geometry and overall diagnostics. Study-level
# forest and bubble plots use the classical meta-analysis pipeline.

# Weight-function output ----

.smModelPlots                           <- function(target, options) {

  if (!options[["weightFunctionPlot"]])
    return()

  fits <- .maExtractFit(target, options)
  container <- .smExtractWeightFunctionContainer(target, options)

  for (scope in names(fits)) {
    fit <- fits[[scope]]

    if (inherits(fit, "try-error"))
      next

    if (options[["subgroup"]] == "") {

      .smWeightsPlot(container, fit, options)

    } else {

      if (is.null(container[["selectionWeightFunctions"]])) {
        plots <- createJaspContainer(gettext("Weight Function"))
        plots$dependOn("weightFunctionPlot")
        plots$position <- 2
        container[["selectionWeightFunctions"]] <- plots
      }

      .smWeightsPlot(
        container[["selectionWeightFunctions"]],
        fit,
        options,
        scope,
        gettextf("Subgroup: %1$s", attr(fit, "subgroup"))
      )
    }
  }
}

.smPrintBiasTest                        <- function(fit) {

  if (!inherits(fit, "rma.uni.selmodel"))
    return(gettext("Publication bias test: not available"))

  comparison <- .smPublicationBiasComparison(fit)

  if (!is.finite(comparison$stat))
    return(gettext("Publication bias test: not available"))

  paste0(
    gettext("Publication bias"), ": ", .smPrintLikelihoodRatio(comparison),
    if (is.finite(comparison$pval)) paste0(", ", .maPrintPValue(comparison$pval))
  )
}

.smPlotTheme                            <- function() {

  ggplot2::theme(
    axis.title  = ggplot2::element_text(size = 12),
    axis.text   = ggplot2::element_text(size = 11),
    plot.margin = ggplot2::margin(10, 14, 10, 10)
  )
}

.smWeightData                           <- function(fit) {

  if (!inherits(fit, "rma.uni.selmodel"))
    return(.smWeightFrame(c(0, 1), c(1, 1), matrix(1, nrow = 2, ncol = 2)))

  # Truncation is a step on the effect-size scale, rather than the p-value scale.
  if (fit$type %in% c("trunc", "truncest")) {
    cutoff  <- if (fit$type == "trunc") fit$steps else fit$delta[2]
    range   <- range(c(fit$yi, cutoff))
    padding <- max(diff(range) * .1, .1)
    if (fit$type == "trunc") {
      x <- c(range[1] - padding, cutoff, cutoff, range[2] + padding)
      selected <- if (fit$alternative == "greater") c(TRUE, TRUE, FALSE, FALSE) else c(FALSE, FALSE, TRUE, TRUE)
      weights <- function(delta) ifelse(selected, delta[1], 1)
    } else {
      # Include cutoff uncertainty for estimated truncation.
      x <- sort(unique(c(seq(range[1] - padding, range[2] + padding, length.out = 501), cutoff)))
      weights <- function(delta) {
        selected <- if (fit$alternative == "greater") x <= delta[2] else x >= delta[2]
        ifelse(selected, delta[1], 1)
      }
    }

    return(.smWeightFrame(x, weights(fit$delta), .smWeightBounds(fit, weights)))
  }

  if (fit$type == "stepfun") {
    x <- as.vector(rbind(head(c(0, fit$steps), -1), fit$steps))

    weights <- function(delta) rep(delta, each = 2)
    return(.smWeightFrame(x, weights(fit$delta), .smWeightBounds(fit, weights)))
  }

  # Use the fitted weight function and its own precision scale. Show the
  # observed precision extremes when the function depends on precision.
  cutoffs   <- fit$steps[is.finite(fit$steps) & fit$steps > 0 & fit$steps < 1]
  x         <- sort(unique(c(seq(.0001, .9999, length.out = 501), cutoffs)))
  precision <- if (fit$precspec) unique(c(fit$precis["min"], fit$precis["max"])) else 1

  frames <- lapply(precision, function(prec) {

    weights <- function(delta) fit$wi.fun(
      x, delta, yi = 0, vi = 1, preci = prec,
      alternative = fit$alternative, steps = fit$steps
    )
    y <- weights(fit$delta)

    bounds <- .smWeightBounds(fit, weights)

    .smWeightFrame(
      x, y, cbind(pmax(0, bounds[, 1]), pmax(0, bounds[, 2])),
      if (fit$precspec) gettextf("Precision index: %1$.3f", prec) else ""
    )
  })

  do.call(rbind, frames)
}

.smWeightFrame                          <- function(x, y, bounds, precision = "") {

  data.frame(x = x, y = y, lower = bounds[, 1], upper = bounds[, 2], precision = precision)
}

.smWeightBounds                         <- function(fit, weights) {

  if (all(fit$delta.fix))
    return(cbind(weights(fit$delta), weights(fit$delta)))

  # Step-function bands use each interval's Wald CI. Single-parameter smooth
  # functions transform the parameter CI; other functions use joint draws.
  if (fit$type == "stepfun" || (sum(!fit$delta.fix) == 1 && fit$type != "truncest")) {
    lower <- weights(ifelse(fit$delta.fix, fit$delta, fit$ci.lb.delta))
    upper <- weights(ifelse(fit$delta.fix, fit$delta, fit$ci.ub.delta))
    return(cbind(pmin(lower, upper), pmax(lower, upper)))
  }

  .smWeightBootstrap(fit, weights)
}

# Pointwise parametric bands, as in metafor's selection-function plot. Preserve
# the analysis RNG state so requesting a plot does not change later computations.
.smWeightBootstrap                      <- function(fit, weights) {

  free <- which(!fit$delta.fix)
  out  <- matrix(NA_real_, nrow = length(weights(fit$delta)), ncol = 2)

  if (!length(free))
    return(cbind(weights(fit$delta), weights(fit$delta)))

  covariance <- fit$vd[free, free, drop = FALSE]
  if (any(!is.finite(covariance)))
    return(out)

  seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", seed, envir = .GlobalEnv)
    }
  })
  set.seed(1)

  samples <- MASS::mvrnorm(1000, mu = fit$delta[free], Sigma = covariance)
  samples <- matrix(samples, ncol = length(free))
  curves  <- vapply(seq_len(nrow(samples)), function(i) {
    delta <- fit$delta
    delta[free] <- pmin(fit$delta.max[free], pmax(fit$delta.min[free], samples[i, ]))
    weights(delta)
  }, numeric(nrow(out)))

  t(apply(curves, 1, stats::quantile, probs = c(fit$level / 2, 1 - fit$level / 2), na.rm = TRUE))
}

.smWeightsPlot                          <- function(container, fit, options,
                                                    key = "weightFunction", title = gettext("Weight Function")) {

  if (!options[["weightFunctionPlot"]] ||
      !is.null(container[[key]]) ||
      inherits(fit, "try-error"))
    return()

  plot          <- createJaspPlot(title = title, width = 500, height = 350)
  plot$position <- 2
  plot$dependOn(c(.smDependencies, "weightFunctionPlot"))
  container[[key]] <- plot

  data <- .smCapture(.smWeightData(fit))

  if (inherits(data, "try-error")) {
    plot$setError(as.character(data))
    return()
  }

  label <- if (!inherits(fit, "rma.uni.selmodel")) {
    gettext("P-value")
  } else if (fit$type %in% c("trunc", "truncest")) {
    gettext("Effect size")
  } else if (fit$alternative == "two.sided") {
    gettext("P-value (two-sided)")
  } else {
    gettext("P-value (one-sided)")
  }
  precisionDependent <- inherits(fit, "rma.uni.selmodel") && fit$precspec
  data$interval <- if (inherits(fit, "rma.uni.selmodel") && fit$type == "stepfun") {
    rep(seq_along(fit$delta), each = 2)
  } else if (inherits(fit, "rma.uni.selmodel") && fit$type == "trunc") {
    rep(1:2, each = 2)
  } else {
    1L
  }

  plot$plotObject <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = x, y = y, group = interaction(precision, interval), linetype = precision)
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = .15, colour = NA, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(group = precision)) +
    ggplot2::labs(x = label, y = gettext("Relative selection likelihood"), linetype = NULL) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw() +
    .smPlotTheme() +
    ggplot2::theme(legend.position = if (precisionDependent) "bottom" else "none")
}

# Overall profile-likelihood diagnostics ----

.smDiagnostics                          <- function(jaspResults, fits, options, specifications) {

  if (!options[["diagnosticsPlotsProfileLikelihood"]] ||
      is.null(fits) ||
      !is.null(jaspResults[["selectionDiagnostics"]]))
    return()

  diagnostics          <- createJaspContainer(gettext("Diagnostics"))
  diagnostics$position <- 8
  diagnostics$dependOn(c(
    .smDependencies, "diagnosticsPlotsProfileLikelihood", "showSelectionModels",
    "includeFullDatasetInSubgroupAnalysis"
  ))
  jaspResults[["selectionDiagnostics"]] <- diagnostics

  scopes   <- .smVisibleScopes(fits, options)
  modelIds <- .smDisplayedModelIds(scopes)

  # Keep diagnostics in one analysis-level section, grouped by displayed model.
  for (i in seq_along(specifications)) {

    id <- names(specifications)[i]

    if (!id %in% modelIds)
      next

    target <- .smModelContainer(diagnostics, id, i, options)
    modelFits <- .smModelFits(scopes, id)

    for (scope in names(modelFits)) {
      fit <- modelFits[[scope]]

      if (options[["subgroup"]] == "") {
        .smProfilePlots(jaspResults, target, fit, id, scope, options)
      } else {
        .smProfilePlots(
          jaspResults,
          target,
          fit,
          id,
          scope,
          options,
          scope,
          gettextf("Subgroup: %1$s", scopes[[scope]]$label)
        )
      }
    }
  }

  if (!length(modelIds)) {
    message <- createJaspHtml(
      text = gettext("No fitted model is available for profile likelihood diagnostics.")
    )
    diagnostics[["unavailable"]] <- message
  }
}

.smComputeProfiles                      <- function(fit) {

  parameters <- if (.smHasEstimatedHeterogeneity(fit)) {
    list(tau2 = list(tau2 = 1))
  } else {
    list()
  }

  if (inherits(fit, "rma.uni.selmodel")) {
    for (i in which(!fit$delta.fix))
      parameters[[paste0("delta", i)]] <- list(delta = i)
  }

  out <- list()

  for (parameter in names(parameters)) {
    args <- c(
      list(fitted = fit, plot = FALSE, progbar = FALSE),
      parameters[[parameter]]
    )

    # The unadjusted rma.uni method profiles heterogeneity without tau2 = 1.
    if (!inherits(fit, "rma.uni.selmodel"))
      args$tau2 <- NULL

    out[[parameter]] <- .smCapture(do.call(stats::profile, args))
  }

  out
}

.smProfilePlots                         <- function(jaspResults, container, fit, id, scope, options,
                                                    key = "selectionProfiles", title = gettext("Profile Likelihood")) {

  if (!options[["diagnosticsPlotsProfileLikelihood"]] || !is.null(container[[key]]))
    return()

  profiles          <- createJaspContainer(title)
  profiles$position <- 8
  profiles$dependOn(c(.smDependencies, "diagnosticsPlotsProfileLikelihood"))
  container[[key]] <- profiles

  if (inherits(fit, "try-error")) {
    output                    <- createJaspPlot(title = title)
    profiles[["unavailable"]] <- output
    output$setError(gettextf("The model could not be fitted: %1$s", as.character(fit)))
    return()
  }

  if (inherits(fit, "rma.uni.selmodel") && fit$decreasing) {
    output                    <- createJaspPlot(title = title)
    profiles[["unavailable"]] <- output
    output$setError(gettext("Profile likelihood diagnostics are not available for ordinal selection models."))
    return()
  }

  profileResults <- .smCachedResult(jaspResults, id, scope, "profiles", function() .smComputeProfiles(fit))

  if (!length(profileResults)) {
    profiles[["unavailable"]] <- createJaspHtml(
      text = gettext("There are no free heterogeneity or selection parameters to profile.")
    )
    return()
  }

  for (parameter in names(profileResults)) {

    title <- if (parameter == "tau2") {
      "\U1D70F\u00b2"
    } else {
      gettextf("Selection Parameter %1$i", as.integer(sub("delta", "", parameter)))
    }
    output                <- createJaspPlot(title = title, width = 400, height = 320)
    output$position       <- 2 * match(parameter, names(profileResults))
    profiles[[parameter]] <- output

    result <- .smMakeProfilePlot(profileResults[[parameter]], parameter)

    if (inherits(result, "try-error")) {
      output$setError(as.character(result))
    } else {
      output$plotObject <- result
    }
  }
}

.smMakeProfilePlot                      <- function(profile, parameter) {

  if (inherits(profile, "try-error"))
    return(profile)

  finite <- is.finite(profile[[1]]) & is.finite(profile$ll)

  if (sum(finite) < 2)
    return(.smError(gettext("Fewer than two finite profile likelihood values are available.")))

  profile[[1]] <- profile[[1]][finite]
  profile$ll   <- profile$ll[finite]
  profile$xlab <- if (parameter == "tau2") "\U1D70F\u00b2" else paste0("\u03b4", sub("delta", "", parameter))

  plot <- .smCapture(.maMakeProfileLikelihoodPlot(profile))

  if (inherits(plot, "try-error"))
    return(plot)

  plot + .smPlotTheme()
}
