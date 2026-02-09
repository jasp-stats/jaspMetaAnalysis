---
applyTo: "**/R/*.R"
---

# JASP Containers, HTML Output & Error Handling

Patterns for grouping output elements and handling errors in jaspResults.

For tables see [jasp-tables.md](jasp-tables.md).
For plots see [jasp-plots.md](jasp-plots.md).
For state/caching see [jasp-state-management.md](jasp-state-management.md).

---

## 1. Containers

Containers group related output elements under a collapsible section.

### Get-or-create pattern (reusable across multiple builder functions)

```r
.myExtractContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["myContainer"]]))
	return(jaspResults[["myContainer"]])

  container <- createJaspContainer(gettext("My Section Title"))
  container$dependOn(.myBaseDependencies)
  container$position <- 1
  jaspResults[["myContainer"]] <- container

  return(container)
}
```

- Use a dedicated extractor when **multiple builder functions** write to the same container
- `$position` controls display order (lower = higher on page)
- `$dependOn()` on the container invalidates **all children** when base options change

### Direct creation (when only one function writes to it)

```r
if (is.null(jaspResults[["sectionContainer"]])) {
  container <- createJaspContainer(gettext("Section Title"))
  container$dependOn(c(.baseDependencies, "specificOption"))
  container$position <- 4
  jaspResults[["sectionContainer"]] <- container
}
```

### Nested containers

For deeply hierarchical output (e.g., per-variable tables):

```r
outerContainer <- jaspResults[["outer"]]
innerContainer <- createJaspContainer(title = "Variable X")
innerContainer$position <- i
outerContainer[["variableX"]] <- innerContainer
# then add tables/plots to innerContainer
```

### Dynamic container management

When the set of children depends on user-selected variables:

```r
# Track existing vs selected variables via metadata state
existingVariables <- metaData[["existingVariables"]]
selectedVariables <- getSelectedVariables(options)

# Remove deselected
for (v in setdiff(existingVariables, selectedVariables))
  container[[v]] <- NULL

# Add new
for (v in setdiff(selectedVariables, existingVariables)) {
  childContainer <- createJaspContainer(title = v)
  container[[v]] <- childContainer
  .buildChildTable(childContainer, fit, options, v)
}

# Update metadata
metaDataState$object <- list(existingVariables = selectedVariables)
```

See [jasp-state-management.md](jasp-state-management.md) for the metadata state pattern that powers this.

---

## 2. HTML Output

For raw HTML content (e.g., displaying R code or formatted messages):

```r
htmlOutput <- createJaspHtml(title = gettext("R Code"))
htmlOutput$dependOn(c(.baseDependencies, "showCode"))
htmlOutput$position <- 99
htmlOutput$text <- "<pre><code>myFunction(yi = ..., sei = ...)</code></pre>"
jaspResults[["rCode"]] <- htmlOutput
```

---

## 3. Error Handling Patterns

### Create-then-error

Always **attach the element to jaspResults before checking errors**. This ensures the empty table (with error message) is displayed rather than nothing:

```r
table <- createJaspTable(gettext("Title"))
container[["table"]] <- table  # attach FIRST

# THEN check for errors
if (someError) {
  table$setError(errorMessage)
  return()
}
```

### Graceful degradation with groups

When some per-group fits fail but others succeed, show partial results with per-group error footnotes:

```r
# Row builders return skeleton data.frames on error (labels only, NAs for numeric columns)
# Tables show partial results with error footnotes per failed group
for (i in which(sapply(fit, jaspBase::isTryError))) {
  table$addFootnote(
	gettextf("Group '%1$s' failed: %2$s", attr(fit[[i]], "group"), .cleanError(fit[[i]])),
	symbol = gettext("Error:")
  )
}
```

### Total failure

When the entire fit fails:

```r
if (length(fit) == 1 && jaspBase::isTryError(fit[[1]])) {
  table$setError(.cleanErrorMessage(fit[[1]]))
  return()
}
```
