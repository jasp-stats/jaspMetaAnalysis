# JASP Module

ALWAYS follow these instructions first and fallback to additional search and context gathering ONLY if the information in these instructions is incomplete or found to be in error.

This is a JASP module. It contains QML user-facing interfaces and R backend computations.

In all interactions and commit messages, be extremely concise and sacrifice grammar for the sake of concision.

## Detailed Instructions

For comprehensive guidance on specific topics, read the corresponding rule file **before working on matching file types**:

- **[Module Architecture](.codex/rules/jasp-module-architecture.md)** - **Start here.** QML-Desktop-R reactive loop, jaspResults persistence, options mapping, data flow. Read when working on `R/*.R`, `inst/qml/*.qml`, or `tests/testthat/*.R`.
- **[Dependency Management](.codex/rules/jasp-dependency-management.md)** - $dependOn mechanics, inheritance, vectors, per-value deps, sentinel pattern. Read when working on `R/*.R`.
- **[State Management](.codex/rules/jasp-state-management.md)** - createJaspState caching, model fit patterns, metadata state, dynamic containers. Read when working on `R/*.R`.
- **[R Backend Development](.codex/rules/r-instructions.md)** - R function structure, validation, style conventions. Read when working on `R/*.R`.
- **[Tables](.codex/rules/jasp-tables.md)** - Table lifecycle, columns, rows, footnotes, error display. Read when working on `R/*.R`.
- **[Plots](.codex/rules/jasp-plots.md)** - Plot lifecycle, composite plots, subgroup/facet patterns. Read when working on `R/*.R`.
- **[Containers & Errors](.codex/rules/jasp-containers-and-errors.md)** - Container patterns, HTML output, error handling. Read when working on `R/*.R`.
- **[QML Interface Development](.codex/rules/qml-instructions.md)** - QML controls, validation, bindings, and UI patterns. Read when working on `inst/qml/*.qml`.
- **[Testing & Test Writing](.codex/rules/testing-instructions.md)** - Test framework, snapshots, and test workflow. Read when working on `tests/testthat/*.R`.
- **[Translation (i18n)](.codex/rules/translation-instructions.md)** - gettext/gettextf/qsTr usage, formatting, plurals. Read when working on `R/*.R`, `inst/qml/*.qml`, or `po/`.
- **[Output Structure](.codex/rules/jasp-output-structure.md)** - Reading/testing serialized output (containers, tables, plots, state). Read when working on `tests/testthat/*.R` or `R/*.R`.
- **[Git Workflow](.codex/rules/git-workflow.md)** - Commit conventions, branch strategy, PR guidelines. Read before any git operations.

## R Session via MCP

This project uses the `btw` MCP server (`.claude/mcp-server.R`) to provide a persistent R session via `btw_tool_run_r`. The MCP server config is in `.codex/config.toml` (project-scoped) and should NOT be committed to git.

**Session handoff:** The user sets up their R session (RStudio/Positron/radian), runs `btw::btw_mcp_session()`, and hands it over. Connect via `list_r_sessions` / `select_r_session`. All `btw_tool_run_r` calls then execute in the user's session with full access to loaded packages and objects. The following R packages are required for the mcp server: `btw`, `mcptools`.

### Available MCP Tools

Use these R-specific tools instead of shell commands when possible:

| Tool | Use for |
|------|---------|
| `btw_tool_run_r` | Execute R code in persistent session (variables persist between calls) |
| `btw_tool_docs_help_page` | Look up R function documentation |
| `btw_tool_docs_package_news` | Check package changelogs |
| `btw_tool_docs_available_vignettes` | Find package vignettes |
| `btw_tool_env_describe_environment` | Inspect objects in the R session |
| `btw_tool_env_describe_data_frame` | Inspect data frame structure |
| `btw_tool_search_packages` | Search CRAN for packages |
| `btw_tool_session_platform_info` | Check R version and platform |
| `btw_tool_session_check_package_installed` | Verify package availability |

**Use Codex native tools** (shell, file read/write, apply_patch, search) for file editing, git operations, and file search -- they are faster than MCP equivalents.

## Working Effectively

### Session Setup (done by user)

At the start of a session, check for a connected R session via `list_r_sessions`. If none is available, **prompt the user** to run in their interactive R console:

```r
source(".claude/session_startup.R")
```

This restores dependencies, installs the module, configures jaspTools, and registers the session. Then connect via `list_r_sessions` / `select_r_session`.

### Hot-Reload After Code Changes

- **R code only changed:** `devtools::load_all()` via `btw_tool_run_r`
- **QML, dependencies, or imports changed:** `renv::install(".", prompt = FALSE)`

### Running Tests

Run via `btw_tool_run_r` in the persistent session:

**Agent-optimized** (preferred -- compact output, returns queryable result object):

```r
# Full test suite -- returns rich S3 result object
x <- agentTestAll()

# Specific analysis tests
x <- agentTestAnalysis("AnalysisName")
```

These return a `jaspAgentTestResults` object. Console output is a compact one-line summary:
```
== Test Results == FAIL: 0 | WARN: 0 | SKIP: 2 | PASS: 72 | Time: 3.6s
```

Query the result object directly:
```r
x$status        # 0 = all passed, 1 = failures
x$summary       # list(fail, warn, skip, pass, time)
x$failures      # data.frame: module | file | test | message
x$warnings      # data.frame: module | file | test | message
x$skips         # data.frame: module | file | test | reason
x$tests         # data.frame: all tests with module | file | context | test | passed | failed | ...
x$errorModules  # named character vector of module-level errors
x$logFile       # path to detailed JSON log (with backtraces)
```

**Human-oriented** (verbose output, for interactive use):
```r
testAll()
testAnalysis("AnalysisName")
```

**Rules:**
- Tests take 300+ seconds to complete -- **NEVER CANCEL**
- Run `agentTestAll()` at session start to verify baseline, and after all fixes
- Use `agentTestAnalysis("Name")` for quick iteration on specific analyses
- Analysis names are PascalCase exports from NAMESPACE
- Some tests skip on certain platforms (e.g., Windows) -- expected
- Some stderr noise (ggplot messages, tryCatch errors) may leak through -- expected and minor
- **MCP timeout:** If `btw_tool_run_r` times out on `agentTestAll()`, do NOT retry -- use the Bash fallback in [testing-instructions.md](.codex/rules/testing-instructions.md)

**See [testing-instructions.md](.codex/rules/testing-instructions.md) for detailed test writing guidelines, snapshots, and workflows.**

### Running a Specific Analysis

**With built-in debug dataset:**
```r
options <- jaspTools::analysisOptions("AnalysisName")
options$someOption <- value
set.seed(1)
results <- jaspTools::runAnalysis("AnalysisName", "debug.csv", options)
```

**From a .jasp example file:**
```r
jaspFile <- file.path("examples", "Example Name.jasp")
opts     <- jaspTools::analysisOptions(jaspFile)
dataset  <- jaspTools::extractDatasetFromJASPFile(jaspFile)
encoded  <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
set.seed(1)
results  <- jaspTools::runAnalysis("AnalysisName", encoded$dataset, encoded$options, encodedDataset = TRUE)
```

The encoding step is required because JASP internally encodes variable names and options to resolve ambiguities (e.g., same variable used with different types).

**From a user-provided .jasp file:** Use the same pattern above. This is the primary way to reproduce bugs reported by users.

**NEVER instantiate jaspResults C++ objects directly** (e.g., `jaspResultsClass$new()`, `create_cpp_jaspResults()`, `jaspBase:::initJaspResults()`). These require JASP Desktop C++ initialization unavailable in headless R sessions. They crash with `Rcpp::not_initialized` or `Expecting an external pointer`. Always use `jaspTools::runAnalysis()` or `agentTestAll()` which handle initialization internally.

### Inspecting Results

After `runAnalysis()`, check:
- `results$status` -- `"complete"` or `"fatalError"`
- `results$results` -- nested list of output containers, tables, plots
- `results$results$errorMessage` -- if status is fatalError

### Finding Analysis Names

1. Check roxygen documentation in R files (if available)
2. Parse `NAMESPACE` for `export()` directives

### Test Snapshots

- Snapshots stored in `tests/testthat/_snaps/`
- **NEVER automatically accept snapshot changes** -- always notify user for manual inspection
- When a snapshot is newly created, inform the user

### Repository Structure
```
/
├── R/                           # Backend R analysis functions
├── inst/
│   ├── qml/                     # QML interface definitions
│   ├── Descriptions/            # Analysis descriptions (Description.qml)
│   ├── help/                    # Markdown help files
│   └── Upgrades.qml             # Version upgrade mappings
├── examples/                    # Example .jasp files for testing
├── tests/testthat/              # Unit tests using jaspTools
├── .codex/                      # Codex CLI instructions and config
│   ├── config.toml              # MCP servers, sandbox, approval settings
│   ├── rules/                   # Rule files (referenced from AGENTS.md)
│   └── README.md                # Codex CLI setup documentation
├── .agents/                     # Cross-platform agent skills
│   └── skills/                  # Skills (debugging, etc.)
├── .claude/                     # Claude Code instructions and MCP server
│   ├── mcp-server.R             # MCP server startup script (shared)
│   └── session_startup.R        # R session bootstrap (shared)
├── .github/workflows/           # CI/CD automation
├── DESCRIPTION                  # R package metadata
├── NAMESPACE                    # Exported analysis names
└── renv.lock                    # R dependency lockfile
```

### Key Files to Check After Changes
- Always check corresponding test file in `tests/testthat/` when modifying R functions
- Update `inst/Upgrades.qml` when renaming QML options to maintain backward compatibility

## Critical Safety Rules

- **NEVER directly edit test files** under `tests/`. Test files are human-owned. Fix source code instead.
- **NEVER automatically accept snapshot changes** -- always notify user for manual inspection.
- **NEVER push/create PRs/merge without explicit human approval.**
- **NEVER use `library()` or `require()`** in R code -- use `package::function()` syntax.
- **NEVER cancel running tests** -- they take 300+ seconds, always let them complete.

## Development Rules

### Dependencies
- Avoid new dependencies -- re-implement simple functions instead of importing a whole package
- If a new dependency is truly needed, add it to DESCRIPTION and update renv.lock

### QML Interface Rules
- QML interfaces in `inst/qml/` define user-facing options passed to R functions
- Each analysis links: `inst/Description.qml/` -> `inst/qml/` -> `R/` functions
- QML elements use `name` (camelCase internal) and `title`/`label` (user-facing)
- Document QML elements using `info` property for help generation
- Use existing QML files as examples for structure and style
- Add default values to unit tests when adding new QML options

**See [qml-instructions.md](.codex/rules/qml-instructions.md) for comprehensive QML controls reference, validation patterns, and UI conventions.**

### R Backend Rules
- R functions in `R/` directory called by analyses in `inst/Descriptions/`
- Use camelCase for all function and variable names
- NEVER use `library()` or `require()` - use `package::function()` syntax
- Access `options` list via `options[["name"]]` notation to avoid partial matching
- Follow CRAN guidelines for code structure and documentation

**See [r-instructions.md](.codex/rules/r-instructions.md) for complete R function structure, jaspResults API, output components (tables/plots/containers/state), and coding conventions.**

### Input Validation and Error Handling
- **TARGETED VALIDATION ONLY**: Since `options` are validated in the GUI, R functions should NOT check user input validity except for specific cases
- **VALIDATE ONLY**: `dataset` object (data.frame from GUI), `TextField` options, and `FormulaField` options (arbitrary text input)
- Use `gettext()` and `gettextf()` for all user-visible messages (internationalization)
- For `dataset` validation, check: missing values, infinity, negative values, insufficient observations, factor levels, variance
- Example: `.hasErrors(dataset, type = c('observations', 'variance', 'infinity'), all.target = options$variables, observations.amount = '< 3', exitAnalysisIfErrors = TRUE)`
- Validate dataset assumptions automatically when required for analysis validity
- Use footnotes for assumption violations that affect specific cells/values
- Place critical errors that invalidate entire analysis over the results table

### Error Message Guidelines
- Write clear, actionable error messages that prevent user confusion
- Use `gettextf()` with placeholders for dynamic content: `gettextf("Number of factor levels is %1$s in %2$s", levels, variable)`
- For multiple arguments, use `%1$s`, `%2$s` format for translator clarity
- Use `ngettext()` for singular/plural forms
- Never mark empty strings for translation
- Use UTF-8 encoding for non-ASCII characters: `\u03B2` for beta
- Double `%` characters in format strings: `gettextf("%s%% CI for Mean")`

**See [translation-instructions.md](.codex/rules/translation-instructions.md) for comprehensive i18n guidelines including QML qsTr(), R gettext/gettextf/ngettext, formatting rules, and Weblate workflow.**

## CI/CD Pipeline
- GitHub Actions in `.github/workflows/unittests.yml` runs on every push
- Triggers on changes to R, test, or package files
- Uses jasp-stats/jasp-actions reusable workflow

## Git Workflow

- **ALWAYS work on feature branches** -- never commit directly to `master`
- **NEVER push/create PRs/merge without explicit human approval**
- Commit locally freely, but wait for approval before pushing to remote

## Common Tasks

### Adding New Analysis

1. Create R function in `R/` directory following camelCase naming
2. Add QML interface in `inst/qml/`
3. Define analysis in `inst/Description.qml`
4. Add unit tests in `tests/testthat/`
5. Run `agentTestAll()` to validate (300+ seconds, NEVER CANCEL)

### Modifying Existing Analysis

1. Update R function maintaining existing interface
2. Update QML if adding/changing options
3. Update unit tests and expected results
4. Add upgrade mapping to `inst/Upgrades.qml` if renaming options
5. Run tests: `agentTestAll()` (NEVER CANCEL, 300+ seconds)

### Detailed Development Process
- **Step 1**: Create main analysis function with `jaspResults`, `dataset`, `options` arguments
- **Step 2**: **CRITICAL** - Use `.quitAnalysis()` for `dataset`, `TextField`, `FormulaField` validation only
- **Step 3**: Create output tables/plots with proper dependencies, citations, column specs
- Use `createJaspTable()`, `createJaspPlot()`, `createJaspHtml()` for output elements
- Always set `$dependOn()` for proper caching and state management
- Use containers for grouping related elements, state objects for reusing computed results
