# Fix & Debug JASP Analysis (MCP Session)

Quick reference for debugging JASP analysis functions through MCP sessions.

**Note**: `browser()` and `recover()` require interactive R console and **do not work** through MCP's `btw_tool_run_r`.

---

## 1) Debugging Approaches

There are two approaches, in order of preference:

### Approach A: Code Inspection (try first)

Many bugs — especially logic errors, missing branches, wrong conditions — are solvable by reading the code and tracing the control flow. This is faster and doesn't require instrumenting code.

1. **Reproduce**: Bootstrap a `runAnalysis()` call (Step 0) and confirm the issue
2. **Read**: Trace the code path from the entry-point function through the relevant helpers
3. **Identify**: Look for logic errors — wrong conditions, missing option checks, incorrect branching
4. **Fix**: Edit the source, hot-reload, and verify

**Use this when**: Output is missing, wrong options are checked, a feature works in one analysis type but not another, UI options don't match R-side logic.

### Approach B: saveRDS State Capture (escalation)

When the bug depends on runtime values that can't be deduced from code reading alone.

1. **Instrument**: Add saveRDS() before the error location
2. **Capture**: Hot-reload and run analysis, copy debug path from console
3. **Inspect**: Load saved state and examine values via MCP
4. **Fix**: Develop and test fix using captured state
5. **Verify**: Remove debug code, hot-reload, confirm fix works

**Use this when**: Error depends on specific data values, unexpected NULL/type, dimension mismatches, or the code path is too complex to trace by reading.

---

## 2) Reproducing the Issue

### Step 0: Bootstrap a Reproducible Analysis Run

Before debugging, you need a working `runAnalysis()` call that reproduces the error. Choose the first applicable source:

#### Option A: User provides a .jasp file

```r
jaspFile <- "path/to/file.jasp"
opts     <- jaspTools::analysisOptions(jaspFile)
dataset  <- jaspTools::extractDatasetFromJASPFile(jaspFile)
encoded  <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
set.seed(1)
results  <- jaspTools::runAnalysis("AnalysisName", encoded$dataset, encoded$options, encodedDataset = TRUE)
```

If the .jasp file contains multiple analyses, `analysisOptions()` returns a list — index with `[[1]]`, `[[2]]`, etc. Pick the analysis that matches the error context.

#### Option B: Extract from existing unit tests (most common fallback)

When no .jasp file is provided, **search test files first**. Test files contain pre-configured options and dataset references that are known to produce complete output.

1. **Find the test file** for the analysis in `tests/testthat/`:
   ```
   grep -r "AnalysisName" tests/testthat/
   ```

2. **Determine the input pattern** used in the test. Tests use one of two patterns:

   **Pattern 1 — .jasp example file** (look for `analysisOptions(jaspFile)` or `extractDatasetFromJASPFile`):
   ```r
   # Copy the loading code from the test, adjusting the path for non-test context
   jaspFile <- file.path("examples", "Example Name.jasp")
   opts     <- jaspTools::analysisOptions(jaspFile)[[1]]  # note: may need [[1]] for multi-analysis files
   dataset  <- jaspTools::extractDatasetFromJASPFile(jaspFile)
   encoded  <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
   set.seed(1)
   results  <- jaspTools::runAnalysis("AnalysisName", encoded$dataset, encoded$options, encodedDataset = TRUE)
   ```

   **Pattern 2 — inline options** (look for `analysisOptions("AnalysisName")` with explicit option assignments):
   ```r
   # Copy the options setup from the test verbatim
   options <- jaspTools::analysisOptions("AnalysisName")
   options$dependent  <- "contNormal"    # copy from test
   options$group      <- "contBinom"     # copy from test
   # ... copy ALL option assignments from the test ...
   set.seed(1)
   results <- jaspTools::runAnalysis("AnalysisName", "debug.csv", options)
   ```

3. **Modify options** to match the bug-triggering scenario (e.g., enable/disable specific checkboxes).

4. **Verify reproduction**: Check that the issue is reproduced — this could be a `"fatalError"` status, an error message in a specific output element, incorrect values, missing output, etc., depending on what the user reported.

#### Option C: Build options from scratch (last resort)

Only when no tests or examples exist:

```r
options <- jaspTools::analysisOptions("AnalysisName")
# Set required inputs — check .robttCheckReady() or equivalent readiness function
# to discover which options must be non-empty
options$dependent <- "contNormal"
options$group     <- "contBinom"
set.seed(1)
results <- jaspTools::runAnalysis("AnalysisName", "debug.csv", options)
```

**Tip**: `jaspTools::analysisOptions("AnalysisName")` returns all options with their QML defaults. Inspect it with `str(options)` to understand available options and their types.

---

## 3) saveRDS Workflow (Approach B)

Use these steps when code inspection alone is insufficient and you need to examine runtime values.

### Step 1: Identify Error Location

From the error message and stack trace, locate the function and approximate line where the error occurs.

**Example**: Stack trace shows `.buildTable()` → `table$addFootnote()` → error

### Step 2: Instrument Code

Add saveRDS() just **before** the line that's failing:

```r
.buildTable <- function(jaspResults, options) {
    # ... existing code ...

    someVariable <- computeSomething(data, options)

    # DEBUG: REMOVE - save state before error
    debug_dir <- tempdir()
    saveRDS(list(
        someVariable = someVariable,
        relatedData = relatedData,
        fit = fit,
        options = options
        # Include ALL relevant variables
    ), file.path(debug_dir, "debug_state.rds"))
    message("DEBUG: Saved to ", file.path(debug_dir, "debug_state.rds"))

    # The line that's failing
    processData(someVariable)
}
```

**Critical rules**:
- Always use marker comment `# DEBUG: REMOVE`
- **Never save `jaspResults`** (crashes R)
- Save to `tempdir()` (auto-cleanup)
- Include `message()` to print path to console
- Save ALL variables that might be relevant

### Step 3: Hot-Reload and Capture

```r
# Via btw_tool_run_r in MCP
devtools::load_all()

# Re-run the analysis (use same code that triggered original error)
set.seed(1)
results <- jaspTools::runAnalysis("AnalysisName", encoded$dataset, encoded$options, encodedDataset = TRUE)

# Console output will show:
# DEBUG: Saved to C:/Users/.../Temp/RtmpXXX/debug_state.rds
```

Copy the debug path from the console output.

### Step 4: Inspect Captured State

```r
# Via btw_tool_run_r in MCP
debug_path <- "C:/Users/.../Temp/RtmpXXX/debug_state.rds"
debug_data <- readRDS(debug_path)

# Examine structure
str(debug_data)

# Inspect specific variables
print(debug_data$someVariable)
sapply(debug_data$someVariable, class)
any(sapply(debug_data$someVariable, is.null))

# Check attributes
for (i in seq_along(debug_data$relatedData)) {
    cat("Item", i, "attribute:", attr(debug_data$relatedData[[i]], "someAttr"), "\n")
}
```

**Goal**: Identify the exact values causing the error.

### Step 5: Develop Fix

Based on inspection, develop fix logic using the saved objects:

```r
# Via btw_tool_run_r in MCP
# Test the fix logic interactively using saved state

# Example: Filter out invalid values
someVariable_clean <- Filter(function(x) !is.null(x) && is.finite(x), debug_data$someVariable)
print(someVariable_clean)  # Verify it works

# Try the fix
for (i in seq_along(someVariable_clean)) {
    cat("Would process item:", someVariable_clean[[i]], "\n")
}
```

Once fix logic works, implement it in the source file.

### Step 6: Clean Up and Verify

1. Apply fix to source file
2. **Remove all debug code** (saveRDS(), message(), and "# DEBUG: REMOVE" markers)
3. Hot-reload and verify:

```r
# Via btw_tool_run_r in MCP
devtools::load_all()

set.seed(1)
results <- jaspTools::runAnalysis("AnalysisName", encoded$dataset, encoded$options, encodedDataset = TRUE)

# Check overall status
cat("Status:", results$status, "\n")
```

4. **Verify the specific issue is resolved** — don't just check `results$status`:
   - If the bug was missing output: confirm the output element now exists in `results$results`
   - If the bug was wrong values: check the specific table/cell values
   - If the bug was an error in a subcomponent: navigate to that component and verify no error
   - If the bug was a crash: confirm status is `"complete"`

5. Search for any remaining debug code before committing:

```bash
grep -r "DEBUG: REMOVE" R/
grep -r "saveRDS.*tempdir" R/
```

---

## 4) What to Save

| Location | Objects to save | DON'T save |
|----------|----------------|------------|
| **Model fitting** | `dataset`, `options`, function args, intermediate values | `jaspResults`, `...` (ellipsis args) |
| **Row building** | `fit`, `attr(fit, "group")`, computed rows, `options` | Parent containers, environments |
| **Table assembly** | `rows` list, intermediate data.frames | Full fit objects if not needed |
| **Error handling** | Error object, variables being processed when error occurred | Large intermediate objects |

**Golden rule**: When unsure, save it. Missing a variable means re-running the entire capture process.

---

## 5) Real-World Example

**Error**: `jaspTable$addFootnote expects 'message' to be a string!`

**Workflow**:

1. **Loaded .jasp file and reproduced error**:
   ```r
   jaspFile <- "path/to/file.jasp"
   opts <- jaspTools::analysisOptions(jaspFile)
   dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)
   encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
   set.seed(1)
   results <- jaspTools::runAnalysis("AnalysisName", encoded$dataset, encoded$options, encodedDataset = TRUE)
   # → Status: fatalError
   ```

2. **Identified error location**: Stack trace → `.buildTable()` at specific line

3. **Instrumented code**:
   ```r
   footnotes <- unique(lapply(dataList, attr, which = "footnote"))

   # DEBUG: REMOVE
   saveRDS(list(
       footnotes = footnotes,
       dataList = dataList
   ), file.path(tempdir(), "footnote_debug.rds"))
   message("DEBUG: Saved to ", file.path(tempdir(), "footnote_debug.rds"))

   for (i in seq_along(footnotes))
       table$addFootnote(footnotes[[i]])
   ```

4. **Captured state**:
   ```r
   devtools::load_all()
   results <- jaspTools::runAnalysis(...)
   # Console: DEBUG: Saved to C:/Users/.../RtmpXXX/footnote_debug.rds
   ```

5. **Inspected**:
   ```r
   debug_data <- readRDS("C:/Users/.../RtmpXXX/footnote_debug.rds")
   str(debug_data$footnotes)
   # List of 2
   #  $ : chr "Some footnote text..."
   #  $ : NULL  ← THE PROBLEM
   ```

6. **Root cause**: `unique()` preserves NULL values → loop called `addFootnote(NULL)` → error

7. **Implemented fix**:
   ```r
   footnotes <- unique(lapply(dataList, attr, which = "footnote"))
   footnotes <- Filter(Negate(is.null), footnotes)  # Filter NULLs
   for (i in seq_along(footnotes))
       table$addFootnote(footnotes[[i]])
   ```

8. **Verified**:
   ```r
   devtools::load_all()
   results <- jaspTools::runAnalysis(...)
   # → Status: complete ✓
   ```

**Time**: ~5 minutes from error to verified fix.

---

## 6) Advanced Techniques

### Conditional Saving

For errors in specific iterations/groups:

```r
# Only save when condition is met
for (i in seq_along(items)) {
    if (i == 47) {  # Error only in iteration 47
        saveRDS(list(item = items[[i]], i = i), file.path(tempdir(), "debug_iter47.rds"))
        message("DEBUG: Saved iteration 47")
    }
    result <- process(items[[i]])
}
```

### Multiple Checkpoints

Narrow down error location by saving at multiple points:

```r
# Checkpoint 1
saveRDS(list(step = "before_transform", data = data),
        file.path(tempdir(), "checkpoint1.rds"))

data_transformed <- transform(data)

# Checkpoint 2
saveRDS(list(step = "after_transform", data_transformed = data_transformed),
        file.path(tempdir(), "checkpoint2.rds"))
```

### Save with Timestamp

For multiple runs:

```r
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
saveRDS(list(...), file.path(tempdir(), paste0("debug_", timestamp, ".rds")))
```

---

## 7) Common Error Patterns

### Pattern 1: Unexpected NULL

**Symptom**: "argument is NULL" or "expects X to be a Y"

**Debugging**:
```r
saveRDS(list(suspect_var = suspect_var, related_vars = list(...)), ...)
# Inspect: is.null(debug_data$suspect_var)
```

### Pattern 2: Wrong Type/Class

**Symptom**: "cannot coerce X to Y" or "is not a valid type"

**Debugging**:
```r
saveRDS(list(var = var, class = class(var), str = capture.output(str(var))), ...)
# Inspect: class(debug_data$var), attributes(debug_data$var)
```

### Pattern 3: Dimension Mismatch

**Symptom**: "dims [product X] do not match length of object [Y]"

**Debugging**:
```r
saveRDS(list(obj = obj, dims = dim(obj), length = length(obj)), ...)
# Inspect: dim(debug_data$obj), length(debug_data$obj)
```

### Pattern 4: Index Out of Bounds

**Symptom**: "subscript out of bounds" or "undefined columns selected"

**Debugging**:
```r
saveRDS(list(container = container, index = i, length = length(container)), ...)
# Inspect: i vs length(debug_data$container), names(debug_data$container)
```

---

## 8) Safety Checklist

Before committing code:

- [ ] All `# DEBUG: REMOVE` markers removed
- [ ] All `saveRDS()` calls removed
- [ ] All debug `message()` calls removed
- [ ] Verified with: `grep -r "DEBUG: REMOVE" R/`
- [ ] Verified with: `grep -r "saveRDS.*tempdir" R/`
- [ ] Hot-reloaded and tested: analysis completes successfully
