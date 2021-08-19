
# STAT302package

<!-- badges: start -->
[![R-CMD-check](https://github.com/kelsiosorio/STAT302package/workflows/R-CMD-check/badge.svg)](https://github.com/kelsiosorio/STAT302package/actions)
[![codecov](https://codecov.io/gh/kelsiosorio/STAT302package/branch/master/graph/badge.svg?token=5JVJ1O1A3P)](https://codecov.io/gh/kelsiosorio/STAT302package)
<!-- badges: end -->

The goal of STAT302package is to build a package.

## Installation

You can install the released version of STAT302package from GitHub using: 

``` r
devtools::install_github("kelsiosorio/STAT302package")
```

## Use

To view vignettes, run the following code:

```{r}
devtools::install_github("kelsiosorio/STAT302package", build_vignette = TRUE, build_opts = c())
library(STAT302package)
# Use this to view the vignette in the Demo HTML help
help(package = "STAT302package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302package")
```
