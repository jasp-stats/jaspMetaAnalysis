---
applyTo: "**/R/*.R"
description: "R function structure, validation, jaspResults API, output components, style conventions"
---

# R Instructions

## 1) Core Basics

- **Main entry point (name matters):**
  - The R function name **must match** the case-sensitive `"function"` field in `Description.qml`.
  - Signature is always:
    ```r
    AnalysisName <- function(jaspResults, dataset, options) { ... }
    ```
  - `jaspResults` is a container that stores all of the analysis output and byproducts (if they are supposed to be kept for later use).
  - `dataset` is the loaded dataset in JASP
  - `options` are the UI choices from QML; **do not rename** option keys (they’re your API).

- **Recommended structure (3 roles):**
  1) **Main function** orchestrates and wires output elements.
  2) **create* functions** declare output markup (tables/plots/text).
  3) **fill* (or compute*) functions** compute results and fill outputs.

- **Dependencies (cache & reuse):**
  Add `$dependOn()` to every output (table/plot/text/container/state) so JASP knows when to reuse or drop it.
  Outputs nested within containers inherit all dependencies from the container.

- **Errors:**
  - Catch run-time errors with `try(...)` and report via `$setError()`.
  - Wrap user-visible text with `gettext()` / `gettextf()` for translation.

---

## 2) Input Validation

Only validate the `dataset`. `options` input is validated in the QML automatically.

Common checks (prefix arguments with the check name):
```r
.hasErrors(
  dataset, type = c("factorLevels", "observations", "variance", "infinity", "missingValues"),
  factorLevels.target = options$variables,
  factorLevels.amount = "< 1",
  observations.target = options$variables,
  observations.amount = "< 1"
)
```
Other useful checks:
- `limits.min/max` (inclusive bounds),
- `varCovData.target/corFun` (positive-definiteness),
- `modelInteractions` (ensure lower-order terms exist).

---

## 3) Output Components

### Tables — `createJaspTable()`
**Key methods/properties:**
- `$dependOn(<optionNames>)`
- `$addCitation("<text>")`
- `$addColumnInfo(name, title, type = "string|number|integer|pvalue", format = "sf:4;dp:3", combine = FALSE, overtitle = NULL)`
- `$showSpecifiedColumnsOnly <- TRUE` (hide unspecified stats you happen to compute)
- `$setExpectedSize(nRows)` (for long computations)
- `$addFootnote(message, colNames = NULL, rowNames = NULL)`
- `$addRows(list(...))` or `$setData(df)`
- `$setError("<message>")`

**Skeleton:**
```r
.createMyTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["mainTable"]])) return()
  tab <- createJaspTable(title = gettext("My Table"))
  tab$dependOn(c("variables", "alpha", "showCI"))
  tab$addColumnInfo("variable", gettext("Variable"), "string", combine = TRUE)
  tab$addColumnInfo("estimate", gettext("Estimate"), "number")
  if (options$showCI) {
    over <- gettextf("%f%% CI", 100 * options[["alpha"]])
    tab$addColumnInfo("lcl", gettext("Lower"), "number", overtitle = over)
    tab$addColumnInfo("ucl", gettext("Upper"), "number", overtitle = over)
  }
  tab$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["mainTable"]] <- tab
  if (!ready) return()
  .fillMyTable(tab, dataset, options)
}
```

### Plots — `createJaspPlot()`
**Key methods/properties:**
- `$dependOn(<optionNames>)`, `$addCitation()`
- Set `plotObject <- ggplot2::ggplot(...)`
- `$setError("<message>")`

**Skeleton:**
```r
.createMyPlot <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["descPlot"]])) return()
  plt <- createJaspPlot(title = gettext("My Plot"), width = 400, height = 300)
  plt$dependOn(c("variables", "alpha"))
  jaspResults[["descPlot"]] <- plt
  if (!ready) return()
  .fillMyPlot(plt, dataset, options)
}
```

### Text blocks — `createJaspHtml()`
Display formatted messages; can depend on options like other outputs.
```r
if (!is.null(jaspResults[["note"]])) return()
msg <- createJaspHtml(text = gettextf("The variable <b>%s</b> was omitted.", options[["variable"]]))
msg$dependOn(c("variable"))
jaspResults[["note"]] <- msg
```

### Containers — `createJaspContainer()`
Group related outputs; container dependencies propagate to children. Useful for “one-per-variable” sections.
- `$dependOn(...)`, `$setError("<message>")`, `$getError()`
- Nest containers freely.

```r
if (is.null(jaspResults[["descGroup"]])) {
  grp <- createJaspContainer(title = gettext("Descriptive Plots"))
  grp$dependOn(c("variables", "alpha"))
  jaspResults[["descGroup"]] <- grp
} else {
  grp <- jaspResults[["descGroup"]]
}
for (v in options[["variables"]]) {
  if (!is.null(grp[[v]])) next
  p <- createJaspPlot(title = v, width = 480, height = 320)
  p$dependOn(optionContainsValue = list(variables = v))
  grp[[v]] <- p
}
```

### State (cache) — `createJaspState()`
Cache computed results across reruns (while dependencies hold).
- `$dependOn(...)`
- `$object <- results` (store) / `results <- state$object` (retrieve)

```r
.stateCompute <- function(jaspResults, dataset, options) {
  st <- createJaspState()
  st$dependOn(c("variables", "alpha"))
  jaspResults[["internalResults"]] <- st
  res <- colMeans(dataset[options[["variables"]]], na.rm = TRUE)
  st$object <- res
}
```

---

## 4) Style & Conventions

- **Follow the project R style guide.** Keep functions short; prefer pure helpers; avoid global state; no I/O or printing in analyses.
- **Naming:**
  - Helpers start with a dot, e.g., `.computeFoo()`, `.fillBarTable()`, `.plotBaz()`.
  - Stable keys in `jaspResults[["..."]]` (don’t rename them later).
- **Internationalization:** All visible text via `gettext()`/`gettextf()`.
- **Performance:** Read only needed columns; postpone decoding; reuse `createJaspState()` when multiple outputs share results.
- **Robustness:** Validate early; guard long loops with `if (!ready) return()`; wrap risky code in `try()` and call `$setError()`.
- **Reproducibility:** Set column formats explicitly in tables; document assumptions in footnotes/citations.
- **Assignment alignment:** 
For related assignments allign them at the arrow `<-`, i.e.,
```
variableOne  <- foo()
variableFive <- foo()
```
and allign function arguments in the similar way for function whose call is too long to be on a single line:
```
out <- foo(
	argumentOne  = variableOne,
	argumentFive = variableFive,
	... 
)

---

## 5) Minimal main() template (copy/paste)

```r
MyAnalysis <- function(jaspResults, dataset, options) {

  ready <- length(options[["variables"]]) > 0

  .createMyTable(jaspResults, dataset, options, ready)
  .createMyPlot(jaspResults, dataset, options, ready)
}
