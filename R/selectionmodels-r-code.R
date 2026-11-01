# Selection-model R code.
#
# Reproducible calls share the argument builders used by fitting.

# Reproducible metafor calls ----

.smCodeString                           <- function(text, quote = '"') {

  # deparse() replaces non-ASCII text with <U+...> in the C locale. Explicit
  # Unicode escapes preserve factor labels in copied code on every platform.
  codePoints <- utf8ToInt(enc2utf8(text))
  escaped    <- vapply(codePoints, function(point) {

    if (point > 127) {
      if (point <= 65535)
        return(sprintf("\\u%04x", point))

      return(sprintf("\\U%08x", point))
    }

    quoted <- encodeString(intToUtf8(point), quote = quote)
    substr(quoted, 2, nchar(quoted) - 1)

  }, character(1))

  paste0(quote, paste(escaped, collapse = ""), quote)
}

.smCodeValue                            <- function(value) {

  if (is.character(value) && length(value) == 1)
    return(.smCodeString(value, quote = "'"))

  if (is.list(value))
    return(paste0("list(", paste(names(value), "=", vapply(value, .smCodeValue, character(1)), collapse = ", "), ")"))

  paste(deparse(value, width.cutoff = 500, backtick = TRUE), collapse = " ")
}

.smCodeComment                          <- function(text) {

  paste0("# ", paste(strsplit(text, "\n", fixed = TRUE)[[1]], collapse = "\n# "), "\n")
}

.smCode                                 <- function(fit, options) {

  dataset       <- attr(fit, "dataset")
  failed        <- inherits(fit, "try-error")
  specification <- attr(fit, "selectionSpecification")

  if (failed && inherits(specification, "try-error"))
    return(.smCodeComment(gettextf("R code unavailable until the specification is valid: %1$s", as.character(specification))))

  # Use original column names in the emitted code, quoting moderator components.
  readable <- options

  for (key in c("effectSize", "effectSizeStandardError", "pValue", "noSelection", "predictors"))
    readable[[key]] <- jaspBase::decodeColNames(options[[key]])

  readable[["effectSizeModelTerms"]] <- lapply(options[["effectSizeModelTerms"]], function(term) {
    term[["components"]] <- vapply(
      jaspBase::decodeColNames(term[["components"]]),
      function(name) deparse(as.name(name), backtick = TRUE),
      character(1)
    )
    term
  })

  sampleSize      <- if (.smNeedsSampleSize(specification)) jaspBase::decodeColNames(specification[["sampleSize"]]) else ""
  rows            <- unname(attr(dataset, "selectionRows"))
  needsModelData  <- !identical(rows, seq_len(attr(dataset, "selectionTotalRows")))
  dataName        <- if (needsModelData) "modelData" else "dataset"
  args            <- .smBaseArguments(dataset, readable, sampleSize, dataName)

  if (args$intercept)
    args$intercept <- NULL

  if (!is.null(args$mods))
    args$mods <- stats::formula(paste(deparse(args$mods), collapse = " "))

  # Failed fits still provide reproducible calls when their specification is valid.
  code <- if (failed) {
    .smCodeComment(gettextf("The module reported: %1$s", as.character(fit)))
  } else {
    ""
  }

  if (needsModelData) {
    code <- paste0(code, "modelData <- dataset[", .smCodeValue(rows), ", , drop = FALSE]\n")
  }

  selectionModel <- inherits(fit, "rma.uni.selmodel") || (failed && specification[["type"]] != "none")
  code <- paste0(code, .maFormatRCall(
    "metafor::rma", vapply(args, .smCodeValue, character(1)),
    if (selectionModel) "reference" else "fit"
  ))

  if (selectionModel) {

    selectionArgs <- if (failed) {
      .smSelectionArguments(specification, dataset, options)
    } else {
      attr(fit, "selectionArguments")
    }

    if (options[["pValue"]] != "") {
      selectionArgs$pval <- substitute(
        data[[variable]],
        list(data = as.name(dataName), variable = readable[["pValue"]])
      )
    }

    if (options[["noSelection"]] != "") {
      code <- paste0(
        code,
        "noSelectionColumn <- ", .smCodeString(readable[["noSelection"]]), "\n",
        "unaffectedLevel <- ", .smCodeString(options[["noSelectionLevel"]]), "\n"
      )
      selectionArgs$subset <- substitute(
        as.character(data[[noSelectionColumn]]) != unaffectedLevel,
        list(data = as.name(dataName))
      )

      # Evaluate these inputs before selmodel evaluates arguments in the study
      # data. A data column can otherwise shadow the generated local variables.
      code <- paste0(code, .maFormatRCall(
        "list", vapply(c(list(x = quote(reference)), selectionArgs), .smCodeValue, character(1)),
        "selectionArguments"
      ), "fit <- do.call(metafor::selmodel, selectionArguments)\n")

    } else {
      code <- paste0(code, .maFormatRCall(
        "metafor::selmodel", vapply(c(list(x = quote(reference)), selectionArgs), .smCodeValue, character(1))
      ))
    }
  }

  code
}

.smShowRCode                            <- function(container, options) {

  if (!options[["showMetaforRCode"]] || !is.null(container[["rCode"]]))
    return()

  html          <- createJaspHtml(title = gettext("R Code"))
  html$position <- 99
  html$dependOn(c(
    .smDependencies, "showMetaforRCode", "includeFullDatasetInSubgroupAnalysis"
  ))
  container[["rCode"]] <- html

  fits <- .maExtractFit(container, options)
  code <- vapply(
    fits,
    function(fit) {
      paste0(
        if (options[["subgroup"]] != "") {
          paste0("# ", gsub("[\r\n]", " ", attr(fit, "subgroup")), "\n")
        },
        .smCode(fit, options)
      )
    },
    character(1)
  )

  html$text <- .maTransformToHtml(paste(code, collapse = "\n"))
}
