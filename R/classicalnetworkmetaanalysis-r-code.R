# Classical network meta-analysis R code.
#
# Prints the R code of the model fitting, as the other meta-analyses show their model: the complete cases of the analysis, meta::pairwise()
# (arm-based data), the netmeta fit, the component model, and the network meta-regression. The calls are formatted from the same call
# specifications that fit the models, so the code and the models cannot differ.

.nmaShowRCode           <- function(jaspResults, options) {

  if (!.nmaReady(options) || !is.null(jaspResults[["netmetaRCode"]]))
    return()

  html          <- createJaspHtml(title = gettext("netmeta R Code"))
  html$position <- 99
  html$dependOn(c(.nmaModelDependencies, "showNetmetaRCode", "includeFullDatasetInSubgroupAnalysis", "metaregressionAssumption"))
  jaspResults[["netmetaRCode"]] <- html

  scopes <- .nmaExtractScopes(jaspResults, options)

  if (is.null(scopes))
    return()

  # (the results of the network meta-regression of the tables: a regression that failed is commented out)
  metaregressions <- if (options[["predictor"]] != "" && !.nmaIsComponentModel(options)) .nmaMetaregressionState(jaspResults, options)

  isSubgroup <- options[["subgroup"]] != ""
  code       <- vapply(names(scopes), function(scope) {
    paste0(
      if (isSubgroup) {
        paste0("# ", gsub("[\r\n]", " ", .nmaScopeTitle(scopes[[scope]][["fit"]])), "\n")
      },
      .nmaCode(scopes[[scope]], options, subgroup = scope != "__fullDataset", metaregression = metaregressions[[scope]])
    )
  }, character(1))

  setup <- c(.nmaCompleteCasesCode(options), if (isSubgroup) "subgroupFits <- list()\n")

  html$text <- .maTransformToHtml(paste(c(setup, code), collapse = "\n"))

  return()
}

.nmaCompleteCasesCode   <- function(options) {

  # the module analyses the complete cases of the variables used by the analysis (including the predictor and the subgroup)
  variables <- paste(vapply(.nmaDataVariables(options), .metaQuoteRString, character(1)), collapse = ", ")

  return(paste0(
    "# ", gettext("Complete cases of the variables used by the analysis"), "\n",
    "dataset <- dataset[stats::complete.cases(dataset[, c(", variables, ")]), ]\n",
    "dataset <- droplevels(dataset)\n"
  ))
}

# 'metaregression': the result of the network meta-regression of the scope (a try-error if it failed)
.nmaCode                <- function(scope, options, subgroup, metaregression = NULL) {

  # with the component model, the standard model is 'nma' and the component model is 'fit' (the fit of the follow-up calls)
  fit       <- scope[["fit"]]
  standard  <- .nmaStandardFit(scope)
  component <- .nmaIsComponentModel(options)

  # subgroup models are fitted to the corresponding rows of the dataset
  dataName <- if (subgroup) "subgroupData" else "dataset"
  dataCode <- if (subgroup) {
    paste0(
      "subgroupRows <- dataset$", .metaAsRName(options[["subgroup"]]), " == ", .metaQuoteRString(attr(fit, "subgroup")), "\n",
      "subgroupData <- droplevels(dataset[subgroupRows, ])\n"
    )
  }

  # (if the reference treatment is missing in the fitted network, the module fitted the network with the first treatment of the network as the
  # reference treatment: the printed call has that reference.group)
  specification <- .nmaCallSpecification(scope[["data"]], options, scope[["pairwise"]], reference = if (!is.null(scope[["fallbackReference"]])) scope[["fallbackReference"]] else NA)

  # the contrasts of arm-based data could not be computed: the call is a comment with the error, and the calls that need the
  # contrasts (all other calls) are not printed
  if (!is.null(scope[["pairwise"]]) && jaspBase::isTryError(scope[["pairwise"]])) {
    return(paste0(
      dataCode,
      "# ", gettext("The contrasts could not be computed:"), "\n",
      .nmaCommentCode(.nmaCodeCall(specification[["pairwise"]], dataName, "pw")),
      "# ", gsub("[\r\n]+", " ", gettextf("The module reported: %1$s", .maTryCleanErrorMessages(scope[["pairwise"]]))), "\n"
    ))
  }

  fitCode <- .nmaCodeCall(specification[["fit"]], dataName, if (component) "nma" else "fit")

  # a fit that failed (e.g., the standard network meta-analysis of a disconnected network, which is followed by the component model, or
  # multi-arm studies with missing comparisons) is commented out: the script continues with the other scopes
  if (jaspBase::isTryError(standard))
    fitCode <- paste0(
      "# ", if (.nmaIsDisconnectedScope(scope)) {
        gettext("The standard network meta-analysis is not available for a disconnected network:")
      } else {
        gettext("The network meta-analysis could not be fitted:")
      }, "\n",
      .nmaCommentCode(fitCode)
    )

  code <- paste0(
    dataCode,
    if (!is.null(specification[["pairwise"]])) .nmaCodeCall(specification[["pairwise"]], dataName, "pw"),
    fitCode
  )

  # (the error of the standard model is reported below unless the component model of a disconnected network follows)
  if (component && (!jaspBase::isTryError(standard) || .nmaIsDisconnectedScope(scope)))
    code <- paste0(code, .nmaComponentCode(scope, options, dataName))

  if (jaspBase::isTryError(fit)) {
    # (a message in a comment must not contain line breaks)
    code <- paste0(code, "# ", gsub("[\r\n]+", " ", gettextf("The module reported: %1$s", .maTryCleanErrorMessages(fit))), "\n")
  } else {
    if (options[["predictor"]] != "" && !component)
      code <- paste0(code, .nmaMetaregressionCode(scope, options, dataName, metaregression))
    # (the models of the subgroups are collected, and the model of the full dataset of a subgroup analysis is kept: the next scope assigns 'fit')
    if (subgroup) {
      code <- paste0(code, "subgroupFits[[", .metaQuoteRString(attr(fit, "subgroup")), "]] <- fit\n")
    } else if (options[["subgroup"]] != "") {
      code <- paste0(code, "fullDatasetFit <- fit\n")
    }
  }

  return(code)
}

# Printing helpers ----

# the lines of the code as comments
.nmaCommentCode         <- function(code) {

  return(paste0("# ", strsplit(code, "\n", fixed = TRUE)[[1]], "\n", collapse = ""))
}

# Network meta-regression ----

.nmaMetaregressionCode  <- function(scope, options, dataName, result = NULL) {

  fit     <- scope[["fit"]]
  problem <- .nmaMetaregressionProblem(fit, options)

  if (!is.null(problem))
    return(paste0("# ", gettext("Network meta-regression"), ": ", problem, "\n"))

  call <- .nmaCodeCall(.nmaCallMetaregression(scope[["data"]], fit, options, dataName), dataName, "metareg")

  # a regression that failed (e.g., the predictor does not vary among the studies that include the reference treatment) is commented out with
  # the message of the module: the script continues with the other scopes
  if (jaspBase::isTryError(result))
    call <- paste0(
      .nmaCommentCode(call),
      "# ", gsub("[\r\n]+", " ", gettextf("The module reported: %1$s", .maTryCleanErrorMessages(result))), "\n"
    )

  return(paste0(
    "# ", gettext("Network meta-regression"), "\n",
    # (the levels of a factor predictor are renamed because netmetareg() fails if a level has the name of a treatment and removes columns by the label of the first level)
    # (the mapping of the labels to the levels of the data is a comment because the results of netmetareg() contain the labels)
    if (is.factor(scope[["data"]][[options[["predictor"]]]])) {
      levels <- .nmaMetaregressionPredictorLevels(scope[["data"]], fit, options)
      paste0(
        "# ", gettext("The levels of the predictor are renamed (see the factor() call): netmetareg() fails if a level has the name of a treatment."), "\n",
        "# ", gettext("Labels of the levels in metareg$results (columns cov_lvl and cov_ref; the first level is the reference level):"), " ",
        paste0(.nmaMetaregressionLevelLabels(levels, fit[["trts"]]), " = ", .metaQuoteRString(gsub("[[:cntrl:]]", " ", levels)), collapse = ", "), "\n"
      )
    },
    call
  ))
}

# Component model ----

# the C matrix and netcomb() (or discomb() for a disconnected network); the component model is the fit of the outputs
.nmaComponentCode       <- function(scope, options, dataName) {

  # (the reason, e.g., the pooling method, is reported by the error of the fit)
  if (.nmaIsCountBased(options))
    return("")

  matrix <- try(.nmaComponentMatrix(.nmaComponentTreatments(scope, options), scope[["data"]], options), silent = TRUE)

  if (jaspBase::isTryError(matrix))
    return("")

  reference <- .nmaComponentReference(scope, .nmaComponentTreatments(scope, options), matrix[["inactive"]], options)

  return(paste0(
    "# ", gettext("Component model (additive component network meta-analysis)"), "\n",
    .nmaCodeMatrix(matrix[["matrix"]], "C"),
    .nmaCodeCall(.nmaCallComponentModel(scope, options, reference), dataName, "fit"),
    # (netcomb() and discomb() return values for the contrasts that the components do not identify; the module reports them as NA)
    if (.nmaHasNotEstimableContrasts(scope[["fit"]]))
      paste0("# ", gsub("[\r\n]+", " ", gettextf("Not estimable (reported as NA by the module): %1$s.", paste(.nmaNotEstimablePairs(scope[["fit"]]), collapse = ", "))), "\n")
  ))
}

# the contrasts of a component fit that the components do not identify, each pair once as "A vs. B" in the order of the treatments
.nmaNotEstimablePairs   <- function(fit) {

  estimable <- attr(fit, "components")[["estimable"]]
  pairs     <- which(!estimable & upper.tri(estimable), arr.ind = TRUE)
  pairs     <- pairs[order(pairs[, "row"], pairs[, "col"]), , drop = FALSE]

  return(gettextf("%1$s vs. %2$s", rownames(estimable)[pairs[, "row"]], colnames(estimable)[pairs[, "col"]]))
}

# a matrix with 0/1 entries (the C matrix) as R code
.nmaCodeMatrix          <- function(matrix, name) {

  rows <- apply(matrix, 1, function(row) paste(format(row, trim = TRUE), collapse = ", "))

  return(paste0(
    name, " <- matrix(\n",
    "\tc(\n\t\t", paste(rows, collapse = ",\n\t\t"), "\n\t),\n",
    "\tnrow = ", nrow(matrix), ", ncol = ", ncol(matrix), ", byrow = TRUE,\n",
    "\tdimnames = list(\n",
    "\t\t", .nmaCodeValue(rownames(matrix), NULL), ",\n",
    "\t\t", .nmaCodeValue(colnames(matrix), NULL), "\n",
    "\t)\n)\n"
  ))
}

# Printing the calls ----

.nmaCodeCall            <- function(call, dataName, result) {

  # the rows of the data of the fit that are kept and the columns that are defined by code
  rows        <- if (!is.null(call[["fitRows"]])) paste0("fit$data <- fit$data[", call[["fitRows"]], ", ]\n")
  definitions <- vapply(names(call[["fitData"]]), function(name) paste0("fit$data$", name, " <- ", call[["fitData"]][[name]], "\n"), character(1))
  arguments   <- vapply(call[["arguments"]], .nmaCodeValue, character(1), dataName = dataName)

  return(paste0(rows, paste(definitions, collapse = ""), .nmaFormatRCall(call[["fun"]], arguments, result)))
}

.nmaCodeValue           <- function(argument, dataName) {

  if (inherits(argument, "nmaColumn"))
    return(paste0(dataName, "$", .metaAsRName(argument[["name"]])))

  if (inherits(argument, "nmaObject"))
    return(argument[["name"]])

  if (inherits(argument, "nmaSymbol"))
    return(argument[["name"]])

  if (is.character(argument))
    return(if (length(argument) == 1) .metaQuoteRString(argument) else paste0("c(", paste(.metaQuoteRString(argument), collapse = ", "), ")"))

  return(paste(deparse(argument), collapse = ""))
}

.nmaFormatRCall         <- function(functionName, arguments, result = "fit") {

  # as .maFormatRCall, but the first argument (the pairwise object) is not named
  argumentNames <- if (is.null(names(arguments))) rep("", length(arguments)) else names(arguments)
  lines         <- ifelse(argumentNames == "", arguments, paste(argumentNames, "=", arguments))

  return(paste0(result, " <- ", functionName, "(\n\t", paste(lines, collapse = ",\n\t"), "\n)\n"))
}
