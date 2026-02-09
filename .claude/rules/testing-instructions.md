---
paths:
  - "**/tests/testthat/*.R"
---

# JASP Testing Instructions

## 1) Test Framework

This module uses the `jaspTools` testing framework. Tests are **critical** and must always pass before committing code.

## 2) Running Tests

Run via `btw_tool_run_r` in the persistent R session:

```r
# Full test suite (300+ sec, NEVER CANCEL)
testAll()

# Specific analysis tests (for quick iteration)
testAnalysis("AnalysisName")
```

**Critical rules:**

- Tests take 300+ seconds to complete
- **NEVER CANCEL** tests -- always let them run to completion
- Some deprecation warnings are expected and can be ignored
- ALL tests must pass before proceeding
- Some tests skip on certain platforms (e.g., Windows) -- this is expected

## 3) Test File Structure

Each test file in `tests/testthat/` corresponds to an R analysis file:

- `test-penalizedmetaanalysis.R` -> `R/penalizedmetaanalysis.R`
- Test file name pattern: `test-<analysisname>.R`
- Analysis names for `testAnalysis()` come from NAMESPACE exports (PascalCase)

## 4) Writing Tests

### Basic test structure

```r
# 1. Set up analysis options
options <- jaspTools::analysisOptions("AnalysisName")
options$variables <- "contGamma"
options$descriptives <- TRUE

# 2. Set seed for reproducibility
set.seed(1)

# 3. Run the analysis
results <- jaspTools::runAnalysis("AnalysisName", "debug.csv", options)

# 4. Test tables
test_that("Table name matches", {
  table <- results[["results"]][["tableName"]][["data"]]
  jaspTools::expect_equal_tables(table, list(...expected values...))
})

# 5. Test plots
test_that("Plot name matches", {
  plotName <- results[["results"]][["containerName"]][["collection"]][["plotId"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotname", dir = "AnalysisName")
})
```

### Loading from .jasp example files

```r
jaspFile <- testthat::test_path("..", "..", "examples", "Example Name.jasp")
opts     <- jaspTools::analysisOptions(jaspFile)
dataset  <- jaspTools::extractDatasetFromJASPFile(jaspFile)
encoded  <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
set.seed(1)
results  <- jaspTools::runAnalysis("AnalysisName", encoded$dataset, encoded$options, encodedDataset = TRUE)
```

### Key testing functions

- `jaspTools::analysisOptions(name)` -- Get default options for an analysis
- `jaspTools::runAnalysis(name, dataset, options)` -- Run analysis with options
- `jaspTools::expect_equal_tables(actual, expected)` -- Compare table output
- `jaspTools::expect_equal_plots(plot, name, dir)` -- Compare plot output (snapshot-based)

## 5) Test Data

- `"debug.csv"` is a built-in jaspTools dataset containing most data types
- Use `set.seed()` before running analyses for reproducibility
- Example .jasp files in `examples/` provide pre-configured options and datasets

## 6) Test Snapshots

- Snapshots stored in `tests/testthat/_snaps/`
- **NEVER automatically accept snapshot changes** -- always notify user for manual inspection
- When a new snapshot is created, inform the user so they can verify it

## 7) When to Update Tests

### Always update tests when

1. Adding new analysis outputs (tables, plots, text)
2. Modifying existing output structure or values
3. Adding new QML options that affect results
4. Changing analysis calculations

### How to update test expectations

1. Run tests and capture new output
2. Verify the new output is correct
3. Update expected values in test file
4. Re-run tests to confirm they pass

## 8) Test Workflow

### Before making code changes

Run `testAll()` via `btw_tool_run_r` to establish baseline -- all tests should pass.

### After making code changes

1. Run `devtools::load_all()` to hot-reload R changes
2. Run `testAnalysis("AnalysisName")` for quick iteration on the affected analysis
3. Once the specific tests pass, run `testAll()` to check for regressions

### If tests fail

1. Review the failure messages carefully
2. Check if failure is expected (due to your intentional changes)
3. If expected: update test expectations and notify user about snapshot changes
4. If unexpected: fix your code
5. Re-run tests until all pass

## 9) Adding New Tests

When adding a new analysis:

1. Create test file: `tests/testthat/test-<analysisname>.R`
2. Set up options with all default values explicitly set
3. Test all output tables and plots
4. Test edge cases and error conditions
5. Use meaningful variable names and test data

## 10) Best Practices

- **One test per output element** -- separate `test_that()` blocks for each table/plot
- **Descriptive test names** -- clearly state what is being tested
- **Reproducible** -- always use `set.seed()` for analyses with randomness
- **Complete option coverage** -- test with various option combinations
- **Keep tests focused** -- each test should verify one specific aspect
