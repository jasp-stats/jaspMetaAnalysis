---
applyTo: "**/R/*.R"
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
  - `options` are the UI choices from QML; **do not rename** option keys (they're your API).

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

For detailed patterns, examples, and lifecycle guides:

- Tables: see [jasp-tables.md](jasp-tables.md)
- Plots: see [jasp-plots.md](jasp-plots.md)
- Containers, HTML, errors: see [jasp-containers-and-errors.md](jasp-containers-and-errors.md)
- State/caching: see [jasp-state-management.md](jasp-state-management.md)

**Quick API reference:**

| Element | Create | Key properties |
|---------|--------|----------------|
| Table | `createJaspTable(title)` | `$addColumnInfo()`, `$setData(df)`, `$addFootnote()`, `$setError()`, `$showSpecifiedColumnsOnly` |
| Plot | `createJaspPlot(title, width, height)` | `$plotObject <- ggplot(...)`, `$setError()` |
| HTML | `createJaspHtml(text)` | `$text`, `$dependOn()` |
| Container | `createJaspContainer(title)` | `$dependOn()` (propagates to children), nest freely |
| State | `createJaspState()` | `$object` (store/retrieve), `$dependOn()` |

All elements support `$dependOn()`, `$position`, and `$addCitation()`.

---

## 4) Style & Conventions

- **Follow the project R style guide.** Keep functions short; prefer pure helpers; avoid global state; no I/O or printing in analyses.
- **Naming:**
  - Helpers start with a dot, e.g., `.computeFoo()`, `.fillBarTable()`, `.plotBaz()`.
  - Stable keys in `jaspResults[["..."]]` (don't rename them later).
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
```
