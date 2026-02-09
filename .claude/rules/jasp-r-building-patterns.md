# JASP R Output Building Patterns

Extracted from well-structured analyses (e.g., `classicalmetaanalysiscommon.R`).
For the serialized output format (what tests see), see [jasp-output-structure.md](jasp-output-structure.md).

---

## Architecture Overview

A JASP analysis follows a **dispatch + build** pattern:

```
EntryPoint(jaspResults, dataset, options)    # thin wrapper: sets options, checks data
  └─> Common(jaspResults, dataset, options)  # orchestrator: calls builders in order
        ├─ .fitModel()                       # createJaspState -- cached computation
        ├─ .tableA()                         # createJaspTable -- output table
        ├─ .tableB()                         # createJaspTable -- output table
        ├─ .plotA()                          # createJaspPlot  -- output plot
        └─ .htmlA()                          # createJaspHtml  -- raw HTML output
```

**Key principle:** Each builder function is **idempotent** -- it checks `if (!is.null(jaspResults[["key"]])) return()` at the top and only creates output when missing. jaspResults caching + `$dependOn()` handles invalidation automatically.

---

## 1. Containers

Containers group related output elements under a collapsible section.

### Get-or-create pattern (reusable across multiple builder functions)

```r
.myExtractContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["myContainer"]]))
    return(jaspResults[["myContainer"]])

  container <- createJaspContainer(gettext("My Section Title"))
  container$dependOn(.myBaseDependencies)
  container$position <- 1
  jaspResults[["myContainer"]] <- container

  return(container)
}
```

- Use a dedicated extractor when **multiple builder functions** write to the same container
- `$position` controls display order (lower = higher on page)
- The `$dependOn()` on the container invalidates **all children** when base options change

### Direct creation (when only one function writes to it)

```r
if (is.null(jaspResults[["sectionContainer"]])) {
  container <- createJaspContainer(gettext("Section Title"))
  container$dependOn(c(.baseDependencies, "specificOption"))
  container$position <- 4
  jaspResults[["sectionContainer"]] <- container
}
```

### Nested containers

For deeply hierarchical output (e.g., per-variable EMM tables):
```r
outerContainer <- jaspResults[["outer"]]
innerContainer <- createJaspContainer(title = "Variable X")
innerContainer$position <- i
outerContainer[["variableX"]] <- innerContainer
# then add tables/plots to innerContainer
```

### Dynamic container management

When the set of children depends on user-selected variables:
```r
# Track existing vs selected variables via metadata state
existingVariables <- metaData[["existingVariables"]]
selectedVariables <- getSelectedVariables(options)

# Remove deselected
for (v in setdiff(existingVariables, selectedVariables))
  container[[v]] <- NULL

# Add new
for (v in setdiff(selectedVariables, existingVariables)) {
  childContainer <- createJaspContainer(title = v)
  container[[v]] <- childContainer
  .buildChildTable(childContainer, fit, options, v)
}

# Update metadata
metaDataState$object <- list(existingVariables = selectedVariables)
```

---

## 2. Tables

### Complete table lifecycle

```r
.myTable <- function(jaspResults, options) {

  container <- .myExtractContainer(jaspResults)

  # 1. SKIP if already created (idempotency)
  if (!is.null(container[["myTable"]]))
    return()

  fit <- .extractFit(jaspResults, options)

  # 2. CREATE table and attach to parent BEFORE filling data
  myTable <- createJaspTable(gettext("My Table Title"))
  myTable$position <- 1
  myTable$dependOn(c("optionA", "optionB"))
  container[["myTable"]] <- myTable

  # 3. DEFINE columns
  myTable$addColumnInfo(name = "term", type = "string",  title = "")
  myTable$addColumnInfo(name = "est",  type = "number",  title = gettext("Estimate"))
  myTable$addColumnInfo(name = "se",   type = "number",  title = gettext("Standard Error"))
  myTable$addColumnInfo(name = "pval", type = "pvalue",  title = gettext("p"))

  # 4. EARLY RETURN on error (table shows as empty with error)
  if (is.null(fit))
    return()
  if (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) {
    myTable$setError(.cleanErrorMessage(fit[[1]]))
    return()
  }

  # 5. BUILD row data (list of data.frames → rbind)
  rows <- .safeRbind(lapply(fit, .myRowBuilder, options = options))
  rows <- .safeOrderAndSimplify(rows, "term", options)

  # 6. ADD footnotes
  myTable$addFootnote(gettext("Some methodological note."))

  # 7. SET data
  myTable$setData(rows)
}
```

### Column types

| Type | Use for | Format examples |
|------|---------|-----------------|
| `"string"` | Labels, names, formatted test stats | -- |
| `"number"` | Numeric values | `"sf:4;dp:3"` (4 sig figs, 3 decimal places) |
| `"integer"` | Counts, df | -- |
| `"pvalue"` | p-values | `"dp:3;p:.001"` (3 dp, threshold at .001) |

### Column modifiers

```r
# Grouped column header (e.g., "95% CI" spanning Lower/Upper)
table$addColumnInfo(name = "lCi", type = "number", title = gettext("Lower"),
                    overtitle = gettextf("%s%% CI", 100 * options[["ciLevel"]]))

# Show only explicitly added columns (hide data columns not in schema)
table$showSpecifiedColumnsOnly <- TRUE
```

### Reusable column helpers

Factor out repeated column patterns into helper functions:
```r
.addCiColumns <- function(table, options) {
  if (options[["confidenceIntervals"]]) {
    overtitle <- gettextf("%s%% CI", 100 * options[["confidenceIntervalsLevel"]])
    table$addColumnInfo(name = "lCi", title = gettext("Lower"), type = "number", overtitle = overtitle)
    table$addColumnInfo(name = "uCi", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }
}

.addSeColumn <- function(table, options) {
  if (options[["standardErrors"]])
    table$addColumnInfo(name = "se", title = gettext("Standard Error"), type = "number")
}
```

### Parameterized tables

When the same table structure serves multiple purposes (e.g., effect size vs heterogeneity):
```r
.myTable <- function(jaspResults, options, parameter = "effectSize") {

  container <- .extractContainer(jaspResults)
  tableKey  <- paste0(parameter, "Table")

  if (!is.null(container[[tableKey]]))
    return()

  table <- createJaspTable(switch(parameter,
    effectSize    = gettext("Effect Size Results"),
    heterogeneity = gettext("Heterogeneity Results")
  ))
  table$position <- switch(parameter, effectSize = 1, heterogeneity = 2)
  container[[tableKey]] <- table
  # ... columns and data
}
```

### Row builder functions

Each `Row` function takes a **single fit** and returns a **data.frame** (one or more rows):

```r
.myRowBuilder <- function(fit, options) {

  # Handle failed fits gracefully (return skeleton with NAs)
  if (jaspBase::isTryError(fit)) {
    return(data.frame(
      term     = gettext("My term"),
      subgroup = attr(fit, "subgroup")
    ))
  }

  row <- data.frame(
    term     = gettext("My term"),
    subgroup = attr(fit, "subgroup"),
    est      = fit$beta[1],
    se       = fit$se[1],
    pval     = fit$pval[1]
  )

  return(row)
}
```

**Key conventions:**
- Always include `subgroup = attr(fit, "subgroup")` for subgroup support
- On error, return data.frame with labels but missing numeric columns (renders as empty cells)
- Use `gettext()` / `gettextf()` for all user-visible strings

### Safe data aggregation

```r
# Combine data.frames with potentially different columns (failed fits may be missing columns)
.safeRbind <- function(dfs) {
  dfs <- dfs[!sapply(dfs, function(x) is.null(x) || length(x) == 0 || nrow(x) == 0)]
  if (length(dfs) == 0) return(NULL)
  allCols <- unique(unlist(lapply(dfs, colnames)))
  for (i in seq_along(dfs)) {
    missing <- setdiff(allCols, colnames(dfs[[i]]))
    for (col in missing) dfs[[i]][[col]] <- NA
    dfs[[i]] <- dfs[[i]][, allCols, drop = FALSE]
  }
  do.call(rbind, dfs)
}

# Order rows by grouping variable and drop subgroup column if no subgroups
.safeOrderAndSimplify <- function(df, labelColumn, options) {
  if (options[["subgroup"]] == "") {
    df <- df[, colnames(df) != "subgroup", drop = FALSE]
    return(df)
  }
  # ... reorder by grouping within each label value
}
```

### Footnotes

```r
# Simple footnote (appears at bottom)
table$addFootnote(gettext("Fixed effects tested using Knapp and Hartung adjustment."))

# Warning-style footnote
table$addFootnote(warningMsg, symbol = gettext("Warning:"))

# Per-subgroup error footnotes
for (i in which(sapply(fit, jaspBase::isTryError))) {
  table$addFootnote(
    gettextf("The model for subgroup '%1$s' failed: %2$s",
             attr(fit[[i]], "subgroup"), .cleanError(fit[[i]])),
    symbol = gettext("Error:")
  )
}

# Cell-specific footnote
table$addFootnote(message, colNames = "est", rowNames = "rowLabel")
```

### Error display on tables

```r
# Error message replaces entire table content
table$setError(gettext("Feature not available for this model type."))

# Error from a try-error object
table$setError(.cleanErrorMessage(tryResult))
```

---

## 3. Plots

### Simple plot

```r
.myPlot <- function(jaspResults, options) {

  if (!is.null(jaspResults[["myPlot"]]))
    return()

  fit <- .extractFit(jaspResults, options)
  if (is.null(fit) || jaspBase::isTryError(fit[[1]]))
    return()

  myPlot <- createJaspPlot(
    title  = gettext("My Plot"),
    width  = 400,
    height = 320
  )
  myPlot$position <- 5
  myPlot$dependOn(c(.baseDependencies, "plotSpecificOption"))
  jaspResults[["myPlot"]] <- myPlot

  # Build ggplot
  plotObj <- ggplot2::ggplot(...) + ...
  myPlot$plotObject <- plotObj
}
```

### Plot with error handling

```r
plotOut <- try(.makePlot(fit, options))

if (inherits(plotOut, "try-error")) {
  myPlot <- createJaspPlot(title = gettext("My Plot"))
  myPlot$dependOn(dependencies)
  myPlot$setError(plotOut)
  jaspResults[["myPlot"]] <- myPlot
  return()
}

myPlot <- createJaspPlot(title = gettext("My Plot"), width = w, height = h)
myPlot$plotObject <- plotObj
jaspResults[["myPlot"]] <- myPlot
```

### Composite plot (jaspGraphsPlot)

For plots with multiple panels (e.g., forest plot with left panel + right panel):
```r
plotObj <- jaspGraphs:::jaspGraphsPlot$new(
  subplots = list(leftPanel, rightPanel),
  layout   = matrix(1:2, ncol = 2),
  heights  = 1,
  widths   = c(0.4, 0.6)
)
forestPlot$plotObject <- plotObj
```

In tests, each subplot gets its own SVG snapshot: `"name-subplot-1"`, `"name-subplot-2"`.

### Subgroup plot pattern

When a single fit → single plot, but multiple subgroups → container of plots:
```r
if (options[["subgroup"]] == "") {
  # Single plot, attach directly
  plot <- .makePlotFun(fit[[1]], options)
  plot$title <- gettext("My Plot")
  plot$dependOn(dependencies)
  jaspResults[["myPlot"]] <- plot

} else {
  # Container with one plot per subgroup
  container <- createJaspContainer()
  container$title <- gettext("My Plot")
  container$dependOn(dependencies)
  jaspResults[["myPlot"]] <- container

  for (i in seq_along(fit)) {
    container[[names(fit)[i]]]          <- .makePlotFun(fit[[i]], options)
    container[[names(fit)[i]]]$title    <- gettextf("Subgroup: %1$s", attr(fit[[i]], "subgroup"))
    container[[names(fit)[i]]]$position <- i
  }
}
```

### Separate-plots-by-variable pattern

When a variable creates multiple faceted plots:
```r
if (length(options[["separatePlots"]]) > 0) {
  container <- createJaspContainer()
  for (i in seq_along(levels)) {
    tempPlot <- createJaspPlot(title = levels[i], width = w, height = h)
    tempPlot$position <- i
    tempPlot$plotObject <- makePlot(data[data$facet == levels[i], ])
    container[[paste0("plot", i)]] <- tempPlot
  }
} else {
  plot <- createJaspPlot(width = w, height = h)
  plot$plotObject <- makePlot(data)
}
```

---

## 4. State Objects (Caching)

### Model fit caching

The most critical use -- expensive model fitting cached in a state object:
```r
.fitModel <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["fit"]]))
    return()

  fitState <- createJaspState()
  fitState$dependOn(.baseDependencies)
  jaspResults[["fit"]] <- fitState

  result <- list()
  result[["__fullDataset"]] <- .fitFun(dataset, options)

  # Subgroup fits
  if (options[["subgroup"]] != "") {
    for (level in unique(dataset[[options[["subgroup"]]]])) {
      subData <- dataset[dataset[[options[["subgroup"]]]] == level, ]
      result[[paste0("subgroup", level)]] <- .fitFun(subData, options)
    }
  }

  jaspResults[["fit"]]$object <- result
}
```

### Extracting cached state

```r
.extractFit <- function(jaspResults, options) {
  fitOutput <- jaspResults[["fit"]]$object
  if (is.null(fitOutput)) return(NULL)
  lapply(fitOutput, function(x) x[["fit"]])
}
```

### Expensive computation caching

For results reused by multiple output elements (e.g., diagnostics used by both table and plot):
```r
.computeDiagnostics <- function(jaspResults, options) {

  if (!is.null(jaspResults[["diagnosticsResults"]]))
    return(jaspResults[["diagnosticsResults"]]$object)

  state <- createJaspState()
  state$dependOn(.baseDependencies)
  jaspResults[["diagnosticsResults"]] <- state

  results <- expensiveComputation(...)
  jaspResults[["diagnosticsResults"]]$object <- results

  return(results)
}
```

### Dataset update state (dependency sentinel)

When a state object needs partial re-computation on a different set of dependencies:
```r
# The fit depends on model options, but the dataset attached to the fit
# also needs updating when plotting variables change
sentinel <- createJaspState()
sentinel$dependOn(.plottingDependencies)  # different from fit dependencies
jaspResults[["fitDataSet"]] <- sentinel

# When sentinel is invalidated, re-attach updated dataset to existing fit
```

---

## 5. HTML Output

For raw HTML content (e.g., displaying R code):
```r
htmlOutput <- createJaspHtml(title = gettext("R Code"))
htmlOutput$dependOn(c(.baseDependencies, "showCode"))
htmlOutput$position <- 99
htmlOutput$text <- "<pre><code>rma(yi = ..., sei = ...)</code></pre>"
jaspResults[["rCode"]] <- htmlOutput
```

---

## 6. Dependencies

### Dependency vectors as module constants

Define at the top of the file, not inline:
```r
.myBaseDependencies <- c("effectSize", "standardError", "method", ...)
.myPlotDependencies <- c("plotOption1", "plotOption2", ...)
```

### Dependency inheritance

- Container `$dependOn()` → invalidates ALL children when triggered
- Child `$dependOn()` → additional option-specific dependencies
- Combine: `container$dependOn(.baseDeps)` + `table$dependOn("tableSpecificOption")`

### Common dependency pattern

```r
# Container holds base model dependencies
container$dependOn(.baseDependencies)

# Each table adds its own specific dependencies
tableA$dependOn(c("showTableA", "tableAOption"))
tableB$dependOn(c("showTableB"))
```

---

## 7. Orchestrator Pattern

The main `Common()` function is a flat sequence of conditional builder calls:
```r
AnalysisCommon <- function(jaspResults, dataset, options) {

  # 1. Fit model (cached via state)
  .fitModel(jaspResults, dataset, options)

  # 2. Summary tables (always shown)
  .overallTestsTable(jaspResults, options)
  .pooledEstimatesTable(jaspResults, options)

  # 3. Conditional sections
  if (options[["showRegression"]]) {
    .termsTable(jaspResults, options, "effectSize")
    .coefficientsTable(jaspResults, options, "effectSize")
  }

  # 4. Conditional plots
  .forestPlot(jaspResults, options)
  .bubblePlot(jaspResults, options)

  # 5. Diagnostics
  if (options[["showDiagnostics"]])
    .diagnosticsTable(jaspResults, options)
}
```

**Rules:**
- Builder functions handle their own visibility checks (return early if option is off)
- The orchestrator doesn't need to know about internal output structure
- Order of calls = default display order (but `$position` overrides when needed)

---

## 8. Error Handling Patterns

### Graceful degradation with subgroups

When some subgroup fits fail but others succeed:
```r
# Row builders return skeleton data.frames on error
# Tables show partial results with error footnotes per failed subgroup
for (i in which(sapply(fit, jaspBase::isTryError))) {
  table$addFootnote(
    gettextf("Subgroup '%1$s' failed: %2$s", attr(fit[[i]], "subgroup"), .cleanError(fit[[i]])),
    symbol = gettext("Error:")
  )
}
```

### Total failure

When the entire fit fails:
```r
if (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) {
  table$setError(.cleanErrorMessage(fit[[1]]))
  return()
}
```

### Create-then-error pattern

Always **attach the table to jaspResults before checking errors**. This ensures the empty table (with error message) is displayed rather than nothing:
```r
table <- createJaspTable(gettext("Title"))
container[["table"]] <- table  # attach FIRST

# THEN check for errors
if (someError) {
  table$setError(errorMessage)
  return()
}
```
