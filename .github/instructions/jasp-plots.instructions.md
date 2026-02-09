---
applyTo: "**/R/*.R"
---

# JASP Plot Building Patterns

How to create and configure plots in jaspResults.

For dependency mechanics see [jasp-dependency-management.md](jasp-dependency-management.md).
For testing plots see [testing-instructions.md](testing-instructions.md) (`expect_equal_plots`).

---

## 1) Simple Plot

```r
.myPlot <- function(jaspResults, options) {

  if (!is.null(jaspResults[["myPlot"]]))
	return()

  fit <- .extractFit(jaspResults, options)
  if (is.null(fit) || jaspBase::isTryError(fit[[1]]))
	return()

  myPlot <- createJaspPlot(
	title  = gettext("My Plot"),
	width  = 400,
	height = 320
  )
  myPlot$position <- 5
  myPlot$dependOn(c(.baseDependencies, "plotSpecificOption"))
  jaspResults[["myPlot"]] <- myPlot

  # Build ggplot
  plotObj <- ggplot2::ggplot(...) + ...

  # Add JASP theme and (plot frame b = bottom, r = right, t = top, l = left)
  plotObj <- plotObj + 
	jaspGraphs::geom_rangeframe(sides = "bl") +
	jaspGraphs::themeJaspRaw()

  myPlot$plotObject <- plotObj
}
```

---

## 2) Plot with Error Handling

Wrap plot construction in `try()` and display the error on the plot element:

```r
plotOut <- try(.makePlot(fit, options))

if (inherits(plotOut, "try-error")) {
  myPlot <- createJaspPlot(title = gettext("My Plot"))
  myPlot$dependOn(dependencies)
  myPlot$setError(plotOut)
  jaspResults[["myPlot"]] <- myPlot
  return()
}

myPlot <- createJaspPlot(title = gettext("My Plot"), width = w, height = h)
myPlot$plotObject <- plotOut
jaspResults[["myPlot"]] <- myPlot
```

---

## 3) Composite Plot (jaspGraphsPlot)

For plots with multiple panels (e.g., a left annotation panel + right data panel):

```r
plotObj <- jaspGraphs:::jaspGraphsPlot$new(
  subplots = list(leftPanel, rightPanel),
  layout   = matrix(1:2, ncol = 2),
  heights  = 1,
  widths   = c(0.4, 0.6)
)
myPlot$plotObject <- plotObj
```

In tests, each subplot gets its own SVG snapshot: `"name-subplot-1"`, `"name-subplot-2"`.

---

## 4) Per-Group Plot Pattern

When a single fit produces a single plot, but multiple groups produce a container of plots:

```r
if (options[["groupingVariable"]] == "") {
  # Single plot, attach directly
  plot <- .makePlotFun(fit[[1]], options)
  plot$title <- gettext("My Plot")
  plot$dependOn(dependencies)
  jaspResults[["myPlot"]] <- plot

} else {
  # Container with one plot per group
  container <- createJaspContainer()
  container$title <- gettext("My Plot")
  container$dependOn(dependencies)
  jaspResults[["myPlot"]] <- container

  for (i in seq_along(fit)) {
	container[[names(fit)[i]]]          <- .makePlotFun(fit[[i]], options)
	container[[names(fit)[i]]]$title    <- gettextf("Group: %1$s", attr(fit[[i]], "group"))
	container[[names(fit)[i]]]$position <- i
  }
}
```

---

## 5) Separate-Plots-by-Variable Pattern

When a variable creates multiple faceted plots:

```r
if (length(options[["separatePlots"]]) > 0) {
  container <- createJaspContainer()
  for (i in seq_along(levels)) {
	tempPlot <- createJaspPlot(title = levels[i], width = w, height = h)
	tempPlot$position <- i
	tempPlot$plotObject <- makePlot(data[data$facet == levels[i], ])
	container[[paste0("plot", i)]] <- tempPlot
  }
} else {
  plot <- createJaspPlot(width = w, height = h)
  plot$plotObject <- makePlot(data)
}
```
