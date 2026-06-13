---
applyTo: "**/R/*.R"
description: "Table lifecycle, columns, rows, footnotes, error display in jaspResults"
---

# JASP Table Building Patterns

How to create, configure, and populate tables in jaspResults.

For dependency mechanics see [jasp-dependency-management.md](jasp-dependency-management.md).
For state/caching see [jasp-state-management.md](jasp-state-management.md).
For containers and error handling see [jasp-containers-and-errors.md](jasp-containers-and-errors.md).

---

## 1) Complete Table Lifecycle

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
  rows <- do.call(rbind, lapply(fit, .myRowBuilder, options = options))

  # 6. ADD footnotes
  myTable$addFootnote(gettext("Some methodological note."))

  # 7. SET data
  myTable$setData(rows)
}
```

**Key**: Always attach the table to jaspResults (step 2) **before** checking errors (step 4). This ensures the empty table with error message displays rather than nothing. See [jasp-containers-and-errors.md](jasp-containers-and-errors.md) for the create-then-error pattern.

---

## 2) Column Types

| Type | Use for | Format examples |
|------|---------|-----------------|
| `"string"` | Labels, names, formatted test stats | -- |
| `"number"` | Numeric values | `"sf:4;dp:3"` (4 sig figs, 3 decimal places) |
| `"integer"` | Counts, df | -- |
| `"pvalue"` | p-values | `"dp:3;p:.001"` (3 dp, threshold at .001) |

---

## 3) Column Modifiers

```r
# Grouped column header (e.g., "95% CI" spanning Lower/Upper)
table$addColumnInfo(name = "lCi", type = "number", title = gettext("Lower"),
					overtitle = gettextf("%s%% CI", 100 * options[["ciLevel"]]))

# Show only explicitly added columns (hide data columns not in schema)
table$showSpecifiedColumnsOnly <- TRUE
```

---

## 4) DRY Pattern: Reusable Column Helpers

When multiple tables share the same column groups (e.g., CI columns, SE columns, test statistics), factor out repeated `addColumnInfo()` calls into shared helper functions. For example, a helper that conditionally adds a CI lower/upper pair with a dynamic overtitle avoids duplicating those 3-4 lines across every table builder.

Apply the same pattern for any column group that appears in more than one table — each helper takes the table and relevant options, and adds the columns conditionally.

---

## 5) Parameterized Tables

When the same table structure serves multiple purposes, parametrize the builder:

```r
.myTable <- function(jaspResults, options, parameter = "main") {

  container <- .extractContainer(jaspResults)
  tableKey  <- paste0(parameter, "Table")

  if (!is.null(container[[tableKey]]))
	return()

  table <- createJaspTable(switch(parameter,
	main    = gettext("Main Results"),
	summary = gettext("Summary Results")
  ))
  table$position <- switch(parameter, main = 1, summary = 2)
  container[[tableKey]] <- table
  # ... columns and data
}
```

---

## 6) Row Builder Pattern

Each row builder takes a **single fit** and returns a **data.frame** (one or more rows):

```r
.myRowBuilder <- function(fit, options) {

  # Handle failed fits gracefully (return skeleton with NAs)
  if (jaspBase::isTryError(fit)) {
	return(data.frame(
	  term  = gettext("My term"),
	  group = attr(fit, "group")
	))
  }

  row <- data.frame(
	term  = gettext("My term"),
	group = attr(fit, "group"),
	est   = fit$beta[1],
	se    = fit$se[1],
	pval  = fit$pval[1]
  )

  return(row)
}
```

**Key conventions:**
- Include `group = attr(fit, "group")` for per-group support
- On error, return data.frame with labels but missing numeric columns (renders as empty cells)
- Use `gettext()` / `gettextf()` for all user-visible strings

---

## 7) DRY Pattern: Safe Data Aggregation

When combining data.frames from multiple fits — especially when some fits may fail and return fewer columns — create a helper that:

1. Filters out NULL/empty data.frames
2. Computes the union of all column names
3. Pads each data.frame with NA for missing columns
4. Calls `do.call(rbind, ...)` on the aligned data.frames

This avoids `rbind()` failures when partial errors produce data.frames with heterogeneous columns. Apply the same helper pattern for ordering rows by grouping variable and simplifying output (e.g., dropping a grouping column when no groups are selected).

---

## 8) Footnotes

```r
# Simple footnote (appears at bottom)
table$addFootnote(gettext("Fixed effects tested using Knapp and Hartung adjustment."))

# Warning-style footnote
table$addFootnote(warningMsg, symbol = gettext("Warning:"))

# Per-group error footnotes
for (i in which(sapply(fit, jaspBase::isTryError))) {
  table$addFootnote(
	gettextf("The model for group '%1$s' failed: %2$s",
			 attr(fit[[i]], "group"), .cleanError(fit[[i]])),
	symbol = gettext("Error:")
  )
}

# Cell-specific footnote
table$addFootnote(message, colNames = "est", rowNames = "rowLabel")
```

---

## 9) Error Display on Tables

```r
# Error message replaces entire table content
table$setError(gettext("Feature not available for this model type."))

# Error from a try-error object
table$setError(.cleanErrorMessage(tryResult))
```

See [jasp-containers-and-errors.md](jasp-containers-and-errors.md) for the full create-then-error and graceful degradation patterns.
