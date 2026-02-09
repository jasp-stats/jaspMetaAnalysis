---
applyTo:
  - "**/tests/testthat/*.R"
  - "**/R/*.R"
---

# JASP Analysis Output Structure

Reading and testing the serialized output from `jaspTools::runAnalysis()`.
For building tables see [jasp-tables.md](jasp-tables.md). For plots see [jasp-plots.md](jasp-plots.md).

## Top-Level `results` Object

After `jaspTools::runAnalysis()`, the returned list has 5 keys:
- `status` -- `"complete"` or `"fatalError"`
- `results` -- nested list of all output elements (containers, tables, plots)
- `state` -- cached figures and computed objects
- `progress` -- progress info (usually empty after completion)
- `typeRequest` -- internal type info

## `results$results` Structure

Contains:
- `.meta` -- recursive metadata describing the tree (type, name, title for each element)
- `name` -- analysis name
- Named elements for each output component (containers, tables, plots)

### Element Types

| Type | Key fields | How to identify |
|------|-----------|-----------------|
| **Container** | `collection`, `name`, `title`, `initCollapsed` | Has `$collection` (named list of children) |
| **Table** | `data`, `schema`, `name`, `title`, `status`, `footnotes`, `casesAcrossColumns` | Has `$schema` with `$fields` |
| **Plot/Image** | `data` (string path), `name`, `title`, `width`, `height`, `status`, `convertible` | Has `$data` as character string (e.g., `"plots/1.png"`) |

## Containers

Containers group related output elements. Structure:
```
container$collection  -- named list of child elements (containers, tables, or plots)
container$name        -- unique identifier (underscore-separated path)
container$title       -- display title (can be "")
container$initCollapsed -- whether collapsed by default
```

**Naming convention:** Child names are parent name + `_` + child suffix. This creates a hierarchical path:
```
modelSummaryContainer
  modelSummaryContainer_testsTable
  modelSummaryContainer_pooledEstimatesTable
```

Containers can nest arbitrarily deep:
```
estimatedMarginalMeansAndContrastsContainer
  estimatedMarginalMeansAndContrastsContainer_effectSize
	estimatedMarginalMeansAndContrastsContainer_effectSize_adjustedEstimate
	  ..._adjustedEstimate_estimatedMarginalMeansTable
```

**Accessing deeply nested elements:** Chain `$collection` at each container level:
```r
results[["results"]][["containerName"]][["collection"]][["containerName_child"]][["collection"]][["containerName_child_table"]][["data"]]
```

## Tables

### Schema (`table$schema$fields`)
List of column definitions, each with:
- `name` -- field identifier (used as key in data rows)
- `title` -- display column header
- `type` -- `"string"`, `"number"`, `"integer"`, `"pvalue"`
- `format` (optional) -- formatting spec, e.g., `"sf:4;dp:3"`, `"dp:3;p:.001"`
- `overTitle` (optional) -- grouped column header (e.g., `"95% CI"` spanning Lower/Upper)

### Data (`table$data`)
List of rows. Each row is a named list with field names as keys:
```r
table$data[[1]]  # first row
# $est, $se, $lCi, $uCi, $pval, ...
```

**Key:** Fields within each row are **alphabetically sorted by name** (from JSON deserialization).

### Footnotes (`table$footnotes`)
List of footnote objects:
```r
footnote$text   -- footnote text
footnote$symbol -- HTML symbol (e.g., "<em>Note.</em>")
footnote$cols   -- columns it applies to (NULL = all)
footnote$rows   -- rows it applies to (NULL = all)
```

### Special Row Fields
- `.isNewGroup` -- boolean, marks visual row separator in JASP GUI
- These appear in `expect_equal_tables` flattened output

## Plots

### In `results$results`
Plot entries store metadata only:
```r
plot$data    -- string key into state$figures (e.g., "plots/1.png")
plot$name    -- identifier
plot$title   -- display title
plot$width   -- pixel width
plot$height  -- pixel height
plot$status  -- "complete"
```

### In `results$state$figures`
Actual plot objects stored here, keyed by the `data` path:
```r
results$state$figures[["plots/1.png"]]$obj     -- the plot object
results$state$figures[["plots/1.png"]]$width
results$state$figures[["plots/1.png"]]$height
```

### Plot Object Types
- **`jaspGraphsPlot`** (R6 class) -- composite plot with `$subplots` list of ggplot objects
- **Plain `ggplot`** -- single ggplot object (no subplots)

### Retrieving Plot for Testing
```r
plotName <- results[["results"]][["plotElement"]][["data"]]
testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
jaspTools::expect_equal_plots(testPlot, "snapshot-name")
```

## State Object (`results$state`)

- `state$figures` -- named list of plot objects (keyed by "plots/N.png")
- `state$other` -- named list of cached R objects (keyed by "state_N")
  - Used by `createJaspState()` for caching expensive computations between output elements

## Testing Utilities

### `expect_equal_tables(table_data, reference_list)`
1. Takes `table$data` (list of row-lists)
2. Flattens via `unname(unlist(rows))` -- row-by-row, fields in alphabetical order within each row
3. Converts numeric strings back to numbers via `charVec2MixedList`
4. Replaces unicode characters with `<unicode>` placeholder
5. Compares element-by-element against flat reference list

**Reference list format:** Single flat `list(...)` with all values row-by-row, fields alphabetically sorted:
```r
# For a table with fields: df, est, name, pval (alphabetical)
# Row 1: df=9, est=-0.69, name="Intercept", pval=0.50
# Row 2: df=9, est=0.29, name="Slope", pval=0.01
jaspTools::expect_equal_tables(table_data,
  list(9, -0.69, "Intercept", 0.50,   # row 1
	   9, 0.29, "Slope", 0.01))       # row 2
```

### `expect_equal_plots(plot_obj, snapshot_name)`
- If `jaspGraphsPlot`: splits into subplots, each compared via `vdiffr::expect_doppelganger` with name `"snapshot-name-subplot-N"`
- If plain `ggplot`: compared directly via `vdiffr::expect_doppelganger`
- SVG snapshots stored in `tests/testthat/_snaps/`

## Quick Reference: Navigating Results

```r
# Run analysis
results <- jaspTools::runAnalysis("AnalysisName", dataset, options)

# Check status
results$status  # "complete" or "fatalError"
results$results$errorMessage  # if fatalError

# Get table data (for expect_equal_tables)
results[["results"]][["containerName"]][["collection"]][["containerName_tableName"]][["data"]]

# Get plot object (for expect_equal_plots)
plotKey <- results[["results"]][["plotName"]][["data"]]
plotObj <- results[["state"]][["figures"]][[plotKey]][["obj"]]

# Inspect table schema
table$schema$fields  # list of {name, title, type, format, overTitle}

# Map entire tree (debug helper)
mapResults <- function(x, depth = 0) {
  indent <- paste(rep("  ", depth), collapse = "")
  if (is.list(x) && !is.null(x$collection)) {
	cat(sprintf("%s[container] %s: '%s'\n", indent, x$name, x$title))
	for (child in x$collection) mapResults(child, depth + 1)
  } else if (is.list(x) && !is.null(x$schema)) {
	cat(sprintf("%s[table] %s: '%s' (%d rows x %d cols)\n",
		indent, x$name, x$title, length(x$data), length(x$schema$fields)))
  } else if (is.list(x) && !is.null(x$data) && is.character(x$data)) {
	cat(sprintf("%s[plot] %s: '%s'\n", indent, x$name, x$title))
  }
}
for (item in results$results[setdiff(names(results$results), c(".meta", "name"))]) {
  mapResults(item)
}
```
