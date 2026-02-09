---
applyTo: "**/R/*.R"
---

# JASP State Management (createJaspState)

How to cache expensive computations and track dynamic output state.

For the reactive loop context see [jasp-module-architecture.md](jasp-module-architecture.md).
For dependency mechanics see [jasp-dependency-management.md](jasp-dependency-management.md).

Note that you cannot test this by running analysis via `runAnalysis()` because you only generate one state at a time
(with no initial elements - ask the human maintainer to validate the dependencies manually if you suspect an issue!).

---

## Why State Objects Exist

Model fitting is expensive. Without caching, every option change (even toggling a checkbox for an unrelated table) would re-run the computation. State objects solve this by caching results that persist across R invocations as long as their dependencies hold.

```r
.computeModel <- function(jaspResults, dataset, options) {
	if (!is.null(jaspResults[["modelFit"]]))
		return()                                    # cached → skip

	fitState <- createJaspState()
	fitState$dependOn(.modelDeps)                    # only model options
	jaspResults[["modelFit"]] <- fitState

	result <- try(expensiveFit(dataset, options))
	fitState$object <- result                        # cache
}
```

Now when the user toggles "Show CI" (a table option, not a model option), `jaspResults[["modelFit"]]` survives. Only when a model option changes does the fit get invalidated and recomputed.

---

## The $object Property

`createJaspState()` stores arbitrary R objects via `$object`:

```r
# Store anything: model fits, lists, data.frames
jaspResults[["modelFit"]]$object <- list(model = fitResult, residuals = resid)

# Retrieve in another builder function
cached <- jaspResults[["modelFit"]]$object
if (is.null(cached)) return()    # not yet computed
model <- cached$model
```

---

## State vs Output Elements

| | State | Table/Plot/Html |
|---|---|---|
| Visible to user | No | Yes |
| Has `$object` | Yes | No (use `$setData()`, `$plotObject`) |
| Purpose | Cache computations | Display results |
| `$dependOn()` | Yes | Yes |
| Can nest in container | Yes | Yes |

---

## Pattern: Model Fit Caching

The most common pattern -- fit a model once, reuse across multiple tables and plots:

```r
.computeModel <- function(jaspResults, dataset, options) {
	if (!is.null(jaspResults[["modelFit"]]))
		return()

	fitState <- createJaspState()
	fitState$dependOn(.modelDeps)
	jaspResults[["modelFit"]] <- fitState

	fit <- try(myPackage::fitModel(
		formula = .buildFormula(options),
		data    = dataset
	))

	fitState$object <- fit
}

# Used by multiple builders:
.extractFit <- function(jaspResults) {
	cached <- jaspResults[["modelFit"]]$object
	if (is.null(cached)) return(NULL)
	return(cached)
}
```

---

## Pattern: Multiple Fits (Per Group / Per Variable)

When the analysis computes separate fits for groups or variables, store them as a named list:

```r
.computeModel <- function(jaspResults, dataset, options) {
	if (!is.null(jaspResults[["modelFit"]]))
		return()

	fitState <- createJaspState()
	fitState$dependOn(.modelDeps)
	jaspResults[["modelFit"]] <- fitState

	results <- list()

	# Overall fit
	results[["overall"]] <- try(fitFun(dataset, options))

	# Per-group fits (if grouping variable selected)
	if (options[["groupingVariable"]] != "") {
		groups <- unique(dataset[[options[["groupingVariable"]]]])
		for (g in groups) {
			subData <- dataset[dataset[[options[["groupingVariable"]]]] == g, ]
			fit <- try(fitFun(subData, options))
			attr(fit, "group") <- as.character(g)    # preserve metadata even on error
			results[[paste0("group_", g)]] <- fit
		}
	}

	fitState$object <- results
}
```

**Key conventions:**
- Use `attr(fit, "group")` to tag each fit with its group label (survives `try()` errors)
- Extractors can filter: include/exclude overall, handle errors per group
- Row builders iterate over fits via `lapply()`, returning skeleton data.frames on error

### Extractor with filtering

```r
.extractFit <- function(jaspResults, options) {
	results <- jaspResults[["modelFit"]]$object
	if (is.null(results)) return(NULL)

	# Optionally exclude overall fit
	if (options[["groupingVariable"]] != "" && !options[["includeOverall"]])
		results <- results[names(results) != "overall"]

	return(results)
}
```

---

## Pattern: Shared Computation Cache

When multiple output elements (table + plot) need the same intermediate result:

```r
.computeDiagnostics <- function(jaspResults, options) {
	if (!is.null(jaspResults[["diagnosticsCache"]]))
		return(jaspResults[["diagnosticsCache"]]$object)

	state <- createJaspState()
	state$dependOn(.diagnosticsDeps)
	jaspResults[["diagnosticsCache"]] <- state

	results <- expensiveComputation(...)
	state$object <- results
	return(results)
}
```

Both `.diagnosticsTable()` and `.diagnosticsPlot()` call `.computeDiagnostics()` -- the second call returns the cached result immediately.

---

## Pattern: Metadata State for Dynamic Containers

When the set of output children depends on user-selected variables, track what's currently rendered:

```r
.buildVariableOutputs <- function(jaspResults, options) {

	container <- .extractContainer(jaspResults)

	# Get or create metadata state
	if (!is.null(container[["metaData"]])) {
		meta <- container[["metaData"]]$object
	} else {
		metaState <- createJaspState()
		metaState$dependOn(c("selectedVariables"))
		container[["metaData"]] <- metaState
		meta <- list(existing = character(0))
	}

	selected <- options[["selectedVariables"]]
	existing <- meta$existing

	# Remove deselected
	for (v in setdiff(existing, selected))
		container[[v]] <- NULL

	# Add new
	for (v in setdiff(selected, existing)) {
		child <- createJaspContainer(title = v)
		child$position <- which(selected == v)
		container[[v]] <- child
		.buildTableForVariable(child, jaspResults, options, v)
	}

	# Update tracking
	container[["metaData"]]$object <- list(existing = selected)
}
```

This avoids rebuilding the entire container when the user adds or removes a single variable.

---

## Pattern: Dataset Update Sentinel

When an expensive fit should NOT be re-run for visualization-only option changes, but auxiliary data attached to the fit needs updating:

```r
.updateFitData <- function(jaspResults, dataset, options) {
	if (is.null(jaspResults[["modelFit"]]))
		return()
	if (!is.null(jaspResults[["fitDataUpdate"]]))
		return()

	# Create sentinel with narrow deps
	sentinel <- createJaspState()
	sentinel$dependOn(.plottingVariableDeps)
	jaspResults[["fitDataUpdate"]] <- sentinel

	# Update auxiliary data on the existing (cached) fit
	fit <- jaspResults[["modelFit"]]$object
	fit$plotData <- .prepPlotData(fit, dataset, options)
	jaspResults[["modelFit"]]$object <- fit

	sentinel$object <- TRUE    # mark as done
}
```

When a plotting variable changes: sentinel is NULLed, data is re-attached. The model fit itself survives.

---

## Common Pitfalls

**Forgetting to store:** Creating a state but never assigning `$object` -- extractors see NULL.

**Circular extraction:** An extractor that calls the compute function which calls the extractor. Use the `if (!is.null(...)) return()` guard pattern consistently.

**Overwriting state from extractors:** Extractors should be read-only. Only the compute function should write to `$object`.

**State without dependencies:** A state with no `$dependOn()` is never invalidated -- it persists forever with potentially stale data.
