# JASP Module

ALWAYS follow these instructions first and fallback to additional search and context gathering ONLY if the information in these instructions is incomplete or found to be in error.
This is a JASP module. It contains QML user-facing interfaces and R backend computations.
In all interactions and commit messages, be extremely concise and sacrifgice grammar for the sake of concisionn.

## Working Effectively

### Initial Setup and Build
- R is and the JASP R packages are pre-installed in GitHub Actions runners
- All tests should pass. Note that there might be many tests and the runtime might be ~ 5 minutes.

### Running Tests
- `Rscript -e "library(jaspTools); testAll()"` -- runs full test suite, takes 70+ seconds. NEVER CANCEL.
- Tests are located in `tests/testthat/test-*.R` files
- Each test file corresponds to an R analysis file in the `R/` directory
- Test snapshots are stored in `tests/testthat/_snaps/`

### Repository Structure
```
/
├── R/                           # Backend R analysis functions
├── inst/
│   ├── qml/                     # QML interface definitions
│   ├── Descriptions/            # Analysis descriptions (Description.qml)
│   ├── help/                    # Markdown help files
│   └── Upgrades.qml             # Version upgrade mappings
├── tests/testthat/              # Unit tests using jaspTools
├── .github/workflows/           # CI/CD automation
├── DESCRIPTION                  # R package metadata
├── renv.lock                    # R dependency lockfile
└── jaspSummaryStatistics.Rproj  # RStudio project
```

### Key Files to Check After Changes
- Always check corresponding test file in `tests/testthat/` when modifying R functions
- Update `inst/Upgrades.qml` when renaming QML options to maintain backward compatibility

## Building and Testing Code Changes

### Before Making Changes
- Run full test suite to establish baseline: `Rscript -e "library(jaspTools); testAll()"`
- NEVER CANCEL: Tests can take 300+ seconds, set timeout to 300 seconds

### After Making Changes
- Run tests again to verify your changes: `Rscript -e "library(jaspTools); testAll()"`
- NEVER CANCEL: Build and test can take up to 5 minutes total
- All tests must pass - do not proceed if tests fail
- Some deprecation warnings are expected and can be ignored

### Manual Validation Scenarios
Since this module runs within JASP desktop application, manual testing requires:
- Testing via jaspTools test framework (covered above)
- Individual analysis validation can be done through R console using jaspTools::runAnalysis()
- CANNOT run standalone - module only functions within JASP ecosystem

## Development Rules

### QML Interface Rules
- QML interfaces in `inst/qml/` define user-facing options passed to R functions
- Each analysis links: `inst/Description.qml/` → `inst/qml/` → `R/` functions
- QML elements use `name` (camelCase internal) and `title`/`label` (user-facing)
- Document QML elements using `info` property for help generation
- Use existing QML files as examples for structure and style
- Add default values to unit tests when adding new QML options

### R Backend Rules  
- R functions in `R/` directory called by analyses in `inst/Descriptions/`
- Use camelCase for all function and variable names
- NEVER use `library()` or `require()` - use `package::function()` syntax
- Avoid new dependencies - re-implement simple functions instead
- Access `options` list via `options[["name"]]` notation to avoid partial matching
- Follow CRAN guidelines for code structure and documentation

### Input Validation and Error Handling
- **TARGETED VALIDATION ONLY**: Since `options` are validated in the GUI, R functions should NOT check user input validity except for specific cases
- **VALIDATE ONLY**: `dataset` object (data.frame from GUI), `TextField` options, and `FormulaField` options (arbitrary text input)
- Use `gettext()` and `gettextf()` for all user-visible messages (internationalization)
- For `dataset` validation, check: missing values, infinity, negative values, insufficient observations, factor levels, variance
- Example: `.hasErrors(dataset, type = c('observations', 'variance', 'infinity'), all.target = options$variables, observations.amount = '< 3', exitAnalysisIfErrors = TRUE)`
- Validate dataset assumptions automatically when required for analysis validity
- Use footnotes for assumption violations that affect specific cells/values
- Place critical errors that invalidate entire analysis over the results table

### Error Message Guidelines (from jasp-human-guide.md)
- Write clear, actionable error messages that prevent user confusion
- Use `gettextf()` with placeholders for dynamic content: `gettextf("Number of factor levels is %1$s in %2$s", levels, variable)`
- For multiple arguments, use `%1$s`, `%2$s` format for translator clarity
- Use `ngettext()` for singular/plural forms
- Never mark empty strings for translation
- Use UTF-8 encoding for non-ASCII characters: `\u03B2` for β
- Double `%` characters in format strings: `gettextf("%s%% CI for Mean")`

### Testing Requirements
- Unit tests in `tests/testthat/` use jaspTools framework
- Tests run via `jaspTools::testAll()` - takes 300+ seconds, NEVER CANCEL
- Test files correspond to R analysis files (test-*.R matches *.R)
- Update test expected values when changing analysis outputs

## CI/CD Pipeline
- GitHub Actions in `.github/workflows/unittests.yml` runs on every push
- Triggers on changes to R, test, or package files
- Uses jasp-stats/jasp-actions reusable workflow
- No external dependencies (JAGS, igraph) required for this module

## Common Tasks

### Adding New Analysis
1. Create R function in `R/` directory following camelCase naming
2. Add QML interface in `inst/qml/`
3. Define analysis in `inst/Description.qml`
4. Add unit tests in `tests/testthat/`
5. Run `jaspTools::testAll()` to validate (300+ seconds, NEVER CANCEL)

### Modifying Existing Analysis
1. Update R function maintaining existing interface
2. Update QML if adding/changing options
3. Update unit tests and expected results
4. Add upgrade mapping to `inst/Upgrades.qml` if renaming options
5. Run tests: `jaspTools::testAll()` (NEVER CANCEL, 300+ seconds)

### Detailed Development Process
- **Step 1**: Create main analysis function with `jaspResults`, `dataset`, `options` arguments
- **Step 2**: **CRITICAL** - Use `.quitAnalysis()` for `dataset`, `TextField`, `FormulaField` validation only
- **Step 3**: Create output tables/plots with proper dependencies, citations, column specs
- Use `createJaspTable()`, `createJaspPlot()`, `createJaspHtml()` for output elements
- Always set `$dependOn()` for proper caching and state management
- Use containers for grouping related elements, state objects for reusing computed results
