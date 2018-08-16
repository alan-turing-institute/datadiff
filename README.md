## Overview

Tabular data sets are common, and many data processing tasks must be repeated on multiple similar data samples. In practice, however, there may be unexpected changes in structure across different batches of data, which are likely to break the analytical pipeline.

Datadiff identifies structural differences between pairs of (related) tabular
data sets and returns an executable summary (or "patch") which is both a description of the differences and a corrective transformation.

In making comparisons, datadiff considers the following (composable) patch types:

 - column permutation
 - column insertion
 - column deletion
 - column replacement
 - recoding of categorical data
 - linear transformation of numerical data

## Installation

``` r
# Install the most recent release from GitHub:
# install.packages("devtools")
devtools::install_github("alan-turing-institute/datadiff")
```

## Usage

Diff two data frames with `ddiff(df1, df2)`.

For more information and examples, see the package vignette:
``` r
# Build the vignette on package installation:
devtools::install_github("alan-turing-institute/datadiff", build_vignettes = TRUE)
vignette("datadiff")
```
