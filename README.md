# ggstat

[![Travis-CI Build Status](https://travis-ci.org/hadley/ggstat.svg?branch=master)](https://travis-ci.org/hadley/ggstat)
[![Coverage Status](https://img.shields.io/codecov/c/github/hadley/ggstat/master.svg)](https://codecov.io/github/hadley/ggstat?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggstat)](https://cran.r-project.org/package=ggstat)

The goal of ggstat is to extract out useful statistical transformations needed by visualisation tools and make them as fast as possible.

ggstat will provides two families of functions:

1.  Functions that work directly on the raw data, and work with hundreds of
    thousands of observations.
   
1.  Functions follow the [bigvis](http://vita.had.co.nz/papers/bigvis.html)
    philosophy of bin-summarise-smooth, that scale to millions of observations.

ggstat provides a low-level vector-based interface. Functions have at most three vector inputs (`x`, `y`, and `wt`), as well as parameters that control the operation of the function. Each summary function is paired with a parameter function that will guess reasonable defaults from data. All functions return a data frame with consistent variable nams and types. Most people will use ggstat via a visualisation package like [ggplot2](http://ggplot2.org) or [ggvis](http://ggvis.rstudio.com): using ggstat directly will general be frustrating because you have to call multiple functions to get the job done.

## Installation

You can install ggstat from github with:

```R
# install.packages("devtools")
devtools::install_github("hadley/ggstat")
```
