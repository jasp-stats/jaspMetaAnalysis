---
paths:
  - "**/R/*.R"
---

# JASP Dependency Management ($dependOn)

How `$dependOn()` controls caching and invalidation of output elements in jaspResults.

For the reactive loop context see [jasp-module-architecture.md](jasp-module-architecture.md).

Note that you cannot test this by running analysis via `runAnalysis()` because you only generate one state at a time
(with no initial elements - ask the human maintainer to validate the dependencies manually if you suspect an issue!).

---

## 1) What $dependOn Does

When you write:
```r
table$dependOn(c("method", "ciLevel"))
```

You tell JASP Desktop: "If `options[["method"]]` or `options[["ciLevel"]]` changes, set this element to NULL before calling R." On the next R invocation, the builder's `if (!is.null(...))` guard sees NULL and recreates the element.

Elements whose dependencies are NOT hit survive across invocations -- the builder returns early and the existing output stays on screen.

---

## 2) Dependency Inheritance

Container dependencies propagate to ALL children:

```r
container$dependOn(c("dependentVariable", "method"))     # base deps
table$dependOn(c("showCI"))                               # additional dep
container[["myTable"]] <- table
```

The table is invalidated if `dependentVariable`, `method`, OR `showCI` changes. Never repeat parent deps on children.

This means you can put shared model-level dependencies on the container and only add output-specific deps to individual tables/plots.

---

## 3) Dependency Vectors as Constants

Define at file top for reuse across builders:
```r
.baseDeps <- c("dependentVariable", "covariates", "method", "ciLevel")
.plotDeps <- c("plotColor", "plotSize", "plotTheme")
```

Use in builders:
```r
container$dependOn(.baseDeps)                          # container holds base deps
table$dependOn(c("showResiduals"))                     # child adds specific dep
plot$dependOn(c(.baseDeps, .plotDeps))                 # or combine for standalone elements
```

Keep dependency vectors comprehensive -- missing a dependency means stale output when that option changes.

---

## 4) Conditional / Dynamic Dependencies

When different analysis modes need different dependency sets:
```r
if (options[["variant"]] == "classical") {
    fitState$dependOn(.classicalDeps)
} else {
    fitState$dependOn(.bayesianDeps)
}
```

Or combine dynamically:
```r
plot$dependOn(c(.plotDeps,
    if (options[["variant"]] == "classical") .classicalDeps else .bayesianDeps
))
```

---

## 5) Per-Value Dependencies (optionContainsValue)

For containers with one child per user-selected variable, invalidate only when that specific variable is removed:

```r
for (v in options[["variables"]]) {
    if (!is.null(container[[v]])) next
    plot <- createJaspPlot(title = v)
    plot$dependOn(optionContainsValue = list(variables = v))
    container[[v]] <- plot
    # ... fill plot ...
}
```

If the user removes variable `"x"` from the list, only `container[["x"]]` is NULLed. Other children survive.

---

## 6) Sentinel Pattern (Narrow Dependencies)

When an expensive computation (e.g., model fit) should NOT be invalidated by visualization-only options, but the visualization data still needs updating:

```r
# Broad deps: model options → invalidate and re-fit
fitState <- createJaspState()
fitState$dependOn(.modelDeps)
jaspResults[["fit"]] <- fitState

# Narrow deps: plotting options → update auxiliary data without re-fitting
sentinel <- createJaspState()
sentinel$dependOn(.plottingDeps)
jaspResults[["fitDataUpdate"]] <- sentinel
```

When a plotting option changes:
- `jaspResults[["fit"]]` survives (model deps not hit)
- `jaspResults[["fitDataUpdate"]]` is NULLed (plotting deps hit)
- The update function sees the NULL sentinel, re-attaches updated auxiliary data to the existing fit

This avoids expensive re-computation when only display options change.

---

## 7) Common Pitfalls

**Missing dependency:** If you forget to list an option in `$dependOn()`, changing that option won't invalidate the element. The user sees stale output.

**Over-broad dependencies:** Putting ALL options on every element means everything gets recomputed on any change. Split into base deps (container) + specific deps (children).

**Duplicate dependencies:** Listing a parent container's dep on a child is harmless but redundant. Keep it clean.

**Forgetting $dependOn entirely:** The element will never be invalidated -- it's created once and persists forever, even when relevant options change.
