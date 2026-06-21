---
applyTo: "**/R/*.R,**/inst/qml/*.qml,**/po/**"
description: "gettext/gettextf/qsTr usage, formatting, plurals, Weblate workflow"
---

# Translation (i18n) Instructions

## 1) Core Principle

**ALL user-visible text must be wrapped for translation.**

This module is translated into multiple languages via Weblate integration.

## 2) R Code Translation

### Use `gettext()` for static strings:
```r
# Single string
message <- gettext("Analysis complete")

# Table titles
tab <- createJaspTable(title = gettext("Descriptive Statistics"))

# Error messages
tab$setError(gettext("Insufficient observations"))
```

### Use `gettextf()` for dynamic strings:
```r
# Single placeholder
msg <- gettextf("Variable %s has insufficient data", varName)

# Multiple placeholders - use numbered format for translators
msg <- gettextf("Number of factor levels is %1$s in %2$s", nLevels, varName)

# Percentage signs must be doubled
label <- gettextf("%s%% CI for Mean Difference", 100 * alpha)
```

### Use `ngettext()` for plurals:
```r
msg <- ngettext(n,
                "One observation removed",
                "%d observations removed",
                domain = "R-jaspEquivalenceTTests")
```

### Column overtitles with dynamic content:
```r
if (options$confidenceInterval) {
  ciLabel <- gettextf("%s%% CI", 100 * options$confidenceIntervalLevel)
  tab$addColumnInfo("lower", gettext("Lower"), overtitle = ciLabel)
  tab$addColumnInfo("upper", gettext("Upper"), overtitle = ciLabel)
}
```

## 3) QML Translation

### Wrap all visible strings with `qsTr()`:
```qml
CheckBox
{
  name:  "descriptives"
  label: qsTr("Descriptive statistics")

  CheckBox
  {
    name:  "confidenceInterval"
    label: qsTr("Confidence interval")
    info:  qsTr("Display confidence intervals for effect sizes")
  }
}
```

### For groups and sections:
```qml
Group
{
  title: qsTr("Additional Statistics")

  CheckBox
  {
    label: qsTr("Effect size")
  }
}

Section
{
  title: qsTr("Advanced Options")

  DoubleField
  {
    label: qsTr("Prior scale")
  }
}
```

### Radio buttons and dropdowns:
```qml
RadioButtonGroup
{
  name:  "hypothesis"
  title: qsTr("Alternative Hypothesis")

  RadioButton
  {
    value: "twoSided"
    label: qsTr("Two-sided")
  }

  RadioButton
  {
    value: "greater"
    label: qsTr("Greater than")
  }
}

DropDown
{
  name:   "effectSize"
  label:  qsTr("Effect Size")
  values: [
    { label: qsTr("Cohen's d"),     value: "cohen" },
    { label: qsTr("Glass' delta"),  value: "glass" }
  ]
}
```

## 4) Translation Rules

### DO wrap for translation:
- ✅ Table/plot/container titles
- ✅ Column names and overtitles
- ✅ Error messages and warnings
- ✅ Footnotes and citations
- ✅ All QML labels, titles, and info text
- ✅ Help text and descriptions
- ✅ Button labels and tooltips

### DON'T wrap for translation:
- ❌ Empty strings: `""` (NEVER mark for translation)
- ❌ Variable names (internal identifiers)
- ❌ Statistical symbols: `"β"`, `"p"`, `"t"`, `"df"`
- ❌ Mathematical expressions
- ❌ Code or syntax
- ❌ File paths

### Format specifications:
```r
# CORRECT - use numbered placeholders for clarity
gettextf("Mean difference is %1$s with SE = %2$s", mean, se)

# AVOID - unnamed placeholders are harder for translators
gettextf("Mean difference is %s with SE = %s", mean, se)
```

### Special characters:
```r
# Use UTF-8 escape sequences for non-ASCII
label <- gettext("Cram\u00E9r's V")  # Cramér's V
symbol <- gettext("\u03B2")          # β (beta)
```

### Percentage signs in format strings:
```r
# WRONG - single % will cause format error
label <- gettextf("%s% CI", 95)

# CORRECT - double %% in format string
label <- gettextf("%s%% CI", 95)
```

## 5) Translation Workflow

### Automated process:
1. Developers write code with `gettext()`/`gettextf()`/`qsTr()`
2. Translation extraction happens automatically
3. Weblate platform provides translation interface
4. Translators work on Weblate
5. Translation files synced back to repository automatically
6. `.github/workflows/translations.yml` handles automation

### Translation files location:
```
po/                    # R translation files
inst/qml/translations/ # QML translation files (if exists)
```

### Manual updates (rare):
Usually handled automatically, but if needed:
```bash
# Update R translations (done by translation workflow)
# Don't manually edit .po files unless absolutely necessary
```

## 6) Testing Translations

While we can't easily test all languages locally, ensure:
1. All user-visible strings are wrapped
2. Format strings use numbered placeholders
3. Percentage signs are doubled in format strings
4. No empty strings marked for translation
5. Context provided for ambiguous terms

## 7) Common Mistakes to Avoid

### ❌ WRONG:
```r
# Missing translation
tab <- createJaspTable(title = "Descriptive Statistics")

# Empty string marked for translation
label <- gettext("")

# Unnamed placeholders
msg <- gettextf("Found %s issues in %s", count, name)

# Single % for percentage
label <- gettextf("%s% Confidence Interval", 95)
```

### ✅ CORRECT:
```r
# Proper translation
tab <- createJaspTable(title = gettext("Descriptive Statistics"))

# No translation for empty string
label <- ""

# Numbered placeholders for translators
msg <- gettextf("Found %1$s issues in %2$s", count, name)

# Doubled %% for percentage
label <- gettextf("%s%% Confidence Interval", 95)
```

## 8) Translation Context

For ambiguous terms, consider adding comments:
```r
# "Mean" as in average (not "mean" as in unkind)
columnTitle <- gettext("Mean")

# "Scale" as in measurement scale (not fish scales)
fieldLabel <- qsTr("Scale variable")
```

## 9) Weblate Integration

- Weblate repo: `jaspequivalencettests-qml` and `jaspequivalencettests-r`
- Automated workflow: `.github/workflows/translations.yml`
- Scheduled runs: Weekly on Saturday at 2:45 AM
- Manual trigger: `workflow_dispatch` available
- Translation updates automatically create commits/PRs
