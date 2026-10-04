# Selection-model specifications and output selection.

.smHasMultipleModels                    <- function(options) {

  options[["publicationBiasAdjustment"]] == "custom" && length(options[["selectionModels"]]) > 1
}

# Candidate specifications ----

.smParseNumbers                         <- function(text, allowNA = FALSE) {

  text <- trimws(text)

  if (!nzchar(text))
    return(numeric())

  text <- sub("^c\\s*\\(", "(", text)
  if (startsWith(text, "(") && endsWith(text, ")"))
    text <- substr(text, 2, nchar(text) - 1)

  # Parse literal numbers only; the extra token preserves an empty trailing field.
  tokens <- trimws(head(strsplit(paste0(text, ",END"), ",", fixed = TRUE)[[1]], -1))
  valid  <- grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", tokens) |
            (allowNA & tokens == "NA")

  if (!length(tokens) || any(!valid))
    stop(gettext("Enter comma-separated numbers, optionally enclosed in parentheses. Use NA for parameters to estimate."))

  values <- suppressWarnings(as.numeric(tokens))

  if (any(!is.finite(values) & !(allowNA & is.na(values))))
    stop(gettext("All specified numbers must be finite."))

  return(values)
}

.smSpecification                        <- function(element) {

  type <- element[["type"]]

  if (type == "none")
    return(list(type = "none"))

  steps <- .smParseNumbers(element[["steps"]])
  delta <- .smParseNumbers(element[["delta"]], allowNA = TRUE)

  # Cutoffs refer to p-values, except for effect-size truncation.
  if (type == "stepfun") {

    if (!length(steps) || any(steps <= 0 | steps > 1) || is.unsorted(steps, strictly = TRUE))
      stop(gettext("Step-function cutoffs must be strictly increasing and greater than 0, with a maximum of 1."))

    if (tail(steps, 1) != 1)
      steps <- c(steps, 1)

    if (length(steps) < 2)
      stop(gettext("Specify at least one cutoff below 1."))

  } else if (type == "beta") {

    if (length(steps) &&
        (length(steps) != 2 || any(steps <= 0 | steps >= 1) || steps[1] >= steps[2]))
      stop(gettext("Beta truncation requires two increasing p-value cutoffs strictly between 0 and 1."))

  } else if (type == "trunc") {

    if (length(steps) != 1)
      stop(gettext("Specify one effect-size cutoff for the truncated model."))

  } else if (type == "truncest") {

    steps <- numeric()

  } else if (length(steps) && (length(steps) != 1 || steps <= 0 || steps >= 1)) {

    stop(gettext("Specify a single p-value threshold strictly between 0 and 1, or leave the threshold empty."))
  }

  parameters <- switch(
    type,
    stepfun   = length(steps),
    beta      = 2L,
    negexppow = 2L,
    truncest  = 2L,
    1L
  )

  if (length(delta) && length(delta) != parameters)
    stop(gettextf(
      "This model requires %1$i selection parameters. Use NA for each parameter to estimate, or leave the field empty to estimate all parameters.",
      parameters
    ))

  if (length(delta)) {

    # The second truncest parameter is an unrestricted effect-size cutoff.
    bounded <- if (type == "truncest") delta[1] else delta

    if (any(bounded < 0, na.rm = TRUE) || (type == "beta" && any(delta <= 0, na.rm = TRUE)))
      stop(gettext("Selection parameters must be non-negative; beta parameters must be strictly positive."))

    if (type == "stepfun" && (is.na(delta[1]) || delta[1] != 1))
      stop(gettext("The first step-function weight must equal 1 (the reference interval)."))
  }

  decreasing <- type == "stepfun" && element[["decreasing"]]

  if (decreasing && length(delta) && any(!is.na(delta[-1])))
    stop(gettext("Ordinality requires estimated non-reference weights. Clear the parameters or turn off ordinality."))

  precision <- if (type %in% c("halfnorm", "negexp", "logistic", "power", "negexppow")) element[["prec"]] else "none"

  return(list(
    type       = type,
    steps      = steps,
    delta      = delta,
    sidedness  = element[["sidedness"]],
    prec       = precision,
    sampleSize = if (precision %in% c("ninv", "sqrtninv")) element[["sampleSize"]] else "",
    scaleprec  = element[["scaleprec"]],
    decreasing = decreasing
  ))
}

.smSpecifications                       <- function(options) {

  if (options[["publicationBiasAdjustment"]] != "custom") {
    return(list(
      preset = list(
        type       = "stepfun",
        steps      = if (options[["publicationBiasAdjustment"]] == "4PSM") c(.025, .5, 1) else c(.025, 1),
        delta      = numeric(),
        sidedness  = "oneSided",
        prec       = "none",
        decreasing = options[["selectionForceOrdinality"]]
      )
    ))
  }

  # A malformed candidate must not stop the remaining candidates.
  elements <- options[["selectionModels"]]
  out      <- lapply(elements, function(element) {
    tryCatch(
      .smSpecification(element),
      error = function(e) .smError(conditionMessage(e))
    )
  })
  names(out) <- vapply(elements, function(element) element[["name"]], character(1))

  return(out)
}

.smNeedsSampleSize                      <- function(spec) {

  !inherits(spec, "try-error") && any(spec[["prec"]] %in% c("ninv", "sqrtninv"))
}

.smSampleSizeVariables                  <- function(specifications) {

  variables <- vapply(Filter(.smNeedsSampleSize, specifications), `[[`, character(1), "sampleSize")
  unique(variables[variables != ""])
}

# Candidate labels ----

.smSpecificationLabel                   <- function(spec) {

  if (inherits(spec, "try-error"))
    return(gettext("Invalid specification"))

  if (spec[["type"]] == "none")
    return(gettext("None (unadjusted)"))

  name <- switch(
    spec[["type"]],
    stepfun   = gettext("Step function"),
    beta      = gettext("Beta"),
    halfnorm  = gettext("Half-normal"),
    negexp    = gettext("Negative exponential"),
    logistic  = gettext("Logistic"),
    power     = gettext("Power"),
    negexppow = gettext("Negative exponential power"),
    trunc     = gettext("Truncation"),
    truncest  = gettext("Estimated truncation")
  )

  precision <- switch(
    spec[["prec"]],
    sei      = gettext("Standard error"),
    vi       = gettext("Variance"),
    ninv     = gettext("Inverse sample size"),
    sqrtninv = gettext("Inverse square root sample size")
  )

  paste0(
    name,
    if (length(spec[["steps"]])) paste0(" (", paste(spec[["steps"]], collapse = ", "), ")"),
    if (spec[["decreasing"]]) gettext("; ordinal"),
    if (spec[["sidedness"]] == "twoSided") gettext("; two-sided") else gettext("; one-sided"),
    if (spec[["prec"]] != "none") paste0("; ", precision),
    if (.smNeedsSampleSize(spec) && spec[["sampleSize"]] != "")
      gettextf("; sample size: %1$s", jaspBase::decodeColNames(spec[["sampleSize"]])),
    if (spec[["prec"]] != "none" && !spec[["scaleprec"]]) gettext("; unscaled precision"),
    if (length(spec[["delta"]])) paste0("; delta = (", paste(spec[["delta"]], collapse = ", "), ")")
  )
}

# Displayed candidates and subgroups ----

.smModelFits                            <- function(scopes, id) {

  fits <- lapply(scopes, function(scope) scope$models[[id]])
  Filter(Negate(is.null), fits)
}

.smDisplayedModelIds                    <- function(scopes) {

  unique(unlist(lapply(scopes, function(scope) names(scope$models))))
}

.smVisibleScopes                        <- function(fits, options) {

  if (options[["subgroup"]] != "" && !options[["includeFullDatasetInSubgroupAnalysis"]])
    fits <- fits[names(fits) != "full"]

  return(fits)
}

.smSelectModels                         <- function(fits, options) {

  if (is.null(fits))
    return(NULL)

  # Results and diagnostics share the same per-subgroup display selection.
  # Scope visibility is applied by the output builders.
  lapply(fits, function(scope) {
    scope$models <- scope$models[.smSelectedModels(scope$models, options)]
    scope
  })
}

.smSelectedModels                       <- function(models, options) {

  if (!.smHasMultipleModels(options))
    return(names(models))

  selected <- options[["showSelectionModels"]]

  if (selected == "all")
    return(names(models))

  if (selected %in% c("bestBIC", "bestAIC")) {

    criterion <- if (selected == "bestBIC") "BIC" else "AIC"
    values    <- vapply(models, function(fit) .smFitStatistics(fit)[[criterion]], numeric(1))
    eligible  <- which(is.finite(values))

    if (!length(eligible))
      return(character())

    return(names(models)[eligible[which.min(values[eligible])]])
  }

  index <- match(selected, paste0("model", seq_along(models)))

  return(if (is.na(index)) character() else names(models)[index])
}
