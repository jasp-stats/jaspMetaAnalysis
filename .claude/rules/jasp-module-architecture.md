# JASP Module Architecture

How QML, JASP Desktop, and R interact. This explains *why* the patterns in the other rule files exist.

For dependency details see [jasp-dependency-management.md](jasp-dependency-management.md).
For state/caching see [jasp-state-management.md](jasp-state-management.md).
For R coding patterns see [jasp-tables.md](jasp-tables.md), [jasp-plots.md](jasp-plots.md), [jasp-containers-and-errors.md](jasp-containers-and-errors.md).
For serialized output format see [jasp-output-structure.md](jasp-output-structure.md).

---

## 1) The Reactive Loop

```
User changes option in QML GUI
        │
        ▼
JASP Desktop collects ALL current option values into a flat named list
        │
        ▼
Desktop calls: AnalysisName(jaspResults, dataset, options)
        │                      │          │         │
        │                      │          │         └─ named list of ALL QML option values
        │                      │          └─ data.frame loaded from the active dataset
        │                      └─ PERSISTENT container surviving across invocations
        │
        ▼
R function builds/updates output in jaspResults
        │
        ▼
Desktop reads jaspResults and renders tables/plots/text in the GUI
```

**Key insight:** Every time the user changes *anything* in the QML interface, Desktop calls the R analysis function again with a fresh `options` list but the **same** `jaspResults` object. This is why:

1. Every builder checks `if (!is.null(jaspResults[["key"]])) return()` -- skip if output already exists and dependencies haven't changed.
2. `$dependOn()` tells Desktop which option changes should invalidate (NULL out) an element. See [jasp-dependency-management.md](jasp-dependency-management.md).
3. `createJaspState()` caches expensive computations so they survive across invocations. See [jasp-state-management.md](jasp-state-management.md).

---

## 2) jaspResults: The Persistent Bridge

`jaspResults` is an R5 reference class that persists between R invocations for the same analysis instance. It is NOT recreated each time.

### Element lifecycle

```
1. Element does not exist          → builder creates it, attaches to jaspResults
2. Options change, deps NOT hit    → element survives, builder returns early
3. Options change, deps ARE hit    → Desktop NULLs the element before calling R
                                     → builder sees NULL, recreates it
4. User removes the analysis       → jaspResults is destroyed entirely
```

### What can live in jaspResults

| Create function | Purpose | Displayed? |
|----------------|---------|------------|
| `createJaspTable()` | Tabular output | Yes |
| `createJaspPlot()` | Plot output | Yes |
| `createJaspHtml()` | Raw HTML/text | Yes |
| `createJaspContainer()` | Groups children | Yes (collapsible section) |
| `createJaspState()` | Cache arbitrary R objects | **No** (invisible to user) |

All five support `$dependOn()`. All five can be stored in jaspResults or nested inside a container.

### Display ordering

Every element has `$position` (integer). Lower = higher on page. Children within a container also have positions.

---

## 3) Options: The Flat Named List

### QML name → R options key

Every QML control has a `name:` property. Desktop flattens ALL controls into a single named list regardless of QML nesting:

```qml
CheckBox {
    name: "showCI"                          // options[["showCI"]] = TRUE/FALSE
    DoubleField {
        name: "ciLevel"                     // options[["ciLevel"]] = 0.95
        defaultValue: 0.95
    }
}
```

Both `showCI` and `ciLevel` appear at the top level of `options`. QML nesting controls UI visibility/enabling but does NOT create nested R structures.

### QML control → R value type

| QML control | R type | Example value |
|-------------|--------|---------------|
| `CheckBox` | logical | `TRUE` / `FALSE` |
| `DropDown` | character | `"restrictedML"` |
| `RadioButtonGroup` | character | `"estimated"` (selected button's `value:`) |
| `AssignedVariablesList` | character | `"myColumn"` (single) or `c("a","b")` (multi) |
| `DoubleField` | numeric | `0.95` |
| `IntegerField` | integer | `1000L` |
| `TextField` | character | `"user text"` |
| `CIField` | numeric | `0.95` (0-1 scale) |
| `PercentField` | numeric | `95` (0-100 scale) |

### Empty/unset variable slots

When no variable is assigned to an `AssignedVariablesList`, the value is `""` (empty string):

```r
if (options[["dependentVariable"]] != "") { ... }
```

For multi-variable lists, check `length(options[["variables"]]) > 0`.

### Column encoding

JASP internally encodes column names. In R analysis code, the encoding is transparent -- `dataset` columns are already encoded. Use `jaspBase::decodeColNames()` when displaying names in plot axes/labels. In tests, use `jaspTools:::encodeOptionsAndDataset()` when loading from .jasp files.

---

## 4) Data Flow (Generic)

```
QML assigns variable names → options[["dependentVariable"]] = "score"
        │
        ▼
Desktop loads dataset with requested columns → dataset (data.frame)
        │
        ▼
Entry point: readiness check + data validation
    - Are required variables assigned?
    - .hasErrors(): infinity, observations, variance, etc.
        │
        ▼
Compute function: expensive model fitting, cached in state
    - Wrap in try() for error handling
    - Store result via createJaspState()
        │
        ▼
Builder functions: extract cached results, build output
    - Tables: define columns, build rows, setData()
    - Plots: build ggplot, assign to plotObject
    - Errors: attach element FIRST, then setError()
```

Builders should handle the "not ready" case gracefully -- create empty tables (column headers but no data) so the user sees the output structure before assigning variables.

---

## 5) The Entry Point → Common → Builder Pattern

### Three-layer architecture

```
Layer 1: Entry point (thin wrapper per analysis)
    MyAnalysis(jaspResults, dataset, options)
        - Sets dispatch flags if sharing code with other analyses
        - Validates data
        - Delegates to orchestrator

Layer 2: Orchestrator (flat sequence of builder calls)
    MyAnalysisCommon(jaspResults, dataset, options)
        - Calls .computeModel()                  # state
        - Calls .summaryTable()                  # table
        - Calls .coefficientsTable()             # table
        - Calls .mainPlot()                      # plot
        - Conditional sections based on options

Layer 3: Builders (idempotent, self-contained)
    .summaryTable(jaspResults, options)
        - Checks if output exists (return early if so)
        - Gets/creates container
        - Creates table, defines columns
        - Extracts cached results
        - Builds rows, sets data
```

### Multiple entry points sharing one orchestrator

When related analyses share logic, they set a dispatch flag and delegate:

```r
AnalysisVariantA <- function(jaspResults, dataset, options) {
    options[["variant"]] <- "A"
    if (.isReady(options)) {
        dataset <- .checkData(dataset, options)
        .checkErrors(dataset, options)
    }
    AnalysisCommon(jaspResults, dataset, options)
}

AnalysisVariantB <- function(jaspResults, dataset, options) {
    options[["variant"]] <- "B"
    # ... same pattern ...
    AnalysisCommon(jaspResults, dataset, options)
}
```

Builders branch on the flag:
```r
if (options[["variant"]] == "B")
    .additionalTable(jaspResults, options)
```

### The readiness check

Before model fitting, verify required inputs exist:

```r
.isReady <- function(options) {
    options[["dependentVariable"]] != "" && length(options[["covariates"]]) > 0
}
```

In the entry point:
```r
if (.isReady(options)) {
    dataset <- .checkData(dataset, options)
    .checkErrors(dataset, options)
}
AnalysisCommon(jaspResults, dataset, options)
```

---

## 6) Registration & Backward Compatibility

### Description.qml

Registers analyses with their R function names:
```qml
Analysis {
    title: qsTr("My Analysis")
    func:  "MyAnalysis"               // must match R function name exactly (case-sensitive)
}
```

### NAMESPACE

Every analysis entry point must be exported:
```r
export(MyAnalysis)
```

### Upgrades.qml

When renaming QML option names, add a migration so old .jasp files load correctly:
```qml
Upgrade {
    functionName: "MyAnalysis"
    fromVersion:  "0.17.2"
    toVersion:    "0.17.3"

    ChangeRename { from: "oldOptionName"; to: "newOptionName" }

    ChangeJS {
        name: "transformedOption"
        jsFunction: function(options) {
            switch(options["transformedOption"]) {
                case "oldValue": return "newValue";
                default:         return options["transformedOption"];
            }
        }
    }
}
```
