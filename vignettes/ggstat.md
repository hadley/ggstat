---
title: "ggcomp"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

(This vignette is currently largely aspirational. It describes ggcomp as it will be when complete.)

The ggcomp package does three things:

* Converts large datasets into smaller summaries.
* Provides a standard way of representing geometric primitives.
* Manipulated geometric primitives.

Compared to ggplot2, ggcomp covers similar functionality to the `stat_` and `position_` functions. But each function in ggcomp tends to be simpler, and there's a standard data representation that makes richer composition easier. Additionally, the majority of functions in ggcomp have been written in C++, allowing you to work with much larger datasets.

## Statistical summaries

ggcomp provides six functions designed to work with smaller (<1e6 obs) datasets:

* `compute_density()`
* `compute_density_2d()`
* `compute_smooth()`
* `compute_model()`
* `compute_ecdf()`
* `compute_qq()`

And two flexible functions designed to be used with larger datasets:

* `compute_bin()`
* `compute_summary()`

These functions have a consistent interface:

* The first argument is `data`, and that object is used for method dispatch. 
  All functions currently implemented provide methods for data frames and
  `grouped_df`s from dplyr. In the future we will add summaries for other 
  types of object.

* The subsequent arguments either refer to variables (as formulas) or 
  specify parameters of the transformation. `compute_*` functions never do
  NSE.

* The result the transformation is always a data frame. The variables should
  be consistent (i.e. always use the same names) and be "type-stable".
  The result should always has the same columns with consistent types.
  All result columns should end with `_` - this avoids conflicts with variables
  carried through the grouping process.

These properties make the summaries easy to program with, at the slight expense of more typing when using interactively. This is a good trade-off because most people will use these functions through a ggvis `layer` function.

Typically each `compute_x()` function will also be accompanied by a `compute_x_vec()` function. This will usually be implemented in C++, and works with vectors, rather than data frames. Summaries that require non-trivial computation to determine parameters should separate these out into their own function, `param_x()`. This makes it easier to test, and to consistently pick parameters across groups.

### Missing values

All summary functions should have an `na.rm` argument which defaults to FALSE.

* `FALSE`: Where possible, preserves missing values in output. If not possible 
  to preserve, needs to print warning message describing how many missing
  values were removed.

* `TRUE`: silently drop missing values.

### Variable types

* Continuous: integer, numeric, POSIXct, Date, difftime.
* Categorical: factor, ordered factor

See `restore()` for a convenient approach to solving this problem.
