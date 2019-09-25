---
title: "Datadiff"
author: "Timothy Hobson"
date: "2019-09-24"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



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


```r
# Install the most recent release from GitHub:
# install.packages("devtools")
devtools::install_github("datadiff", ref = "v0.2.2")
```

## Usage

Diff two data frames with `ddiff(df1, df2)`.

## Example application

For demonstration purposes we consider a data set published by the UK's 
communications regulator Ofcom containing the results of an annual survey of
fixed-line broadband performance. The source is: https://data.gov.uk/dataset/uk-fixed-line-broadband-performance.

The `datadiff` package includes these data sets for the years 2013 and 2014. The 
content is consistent (although expanding) over time, but the table structure is 
not consistent.


```r
head(as.tibble(broadband2013))
#> Warning: `as.tibble()` is deprecated, use `as_tibble()` (but mind the new semantics).
#> This warning is displayed once per session.
#> # A tibble: 6 x 29
#>   ID    Distance.band Urban.rural Market ISP   Headline.speed Technology
#>   <chr> <chr>         <chr>        <int> <chr> <chr>          <chr>     
#> 1 1302  2563-5000     Urban            3 Virg~ 60             Cable     
#> 2 1318  1367-1635     Urban            3 Talk~ 20             ADSL2+    
#> 3 1319  1992-2563     Urban            3 Sky   20             ADSL2+    
#> 4 1320  1367-1635     Urban            3 Virg~ 60             Cable     
#> 5 1321  763-946       Urban            3 Virg~ 30             Cable     
#> 6 1325  1367-1635     Rural            2 Plus~ 20             DSL       
#> # ... with 22 more variables: Download.speed..Mbit.s..24.hour <dbl>,
#> #   Download.speed..Mbit.s..Max <dbl>,
#> #   Download.speed..Mbit.s..8.10pm.weekday <dbl>,
#> #   Upload.speed..Mbit.s.24.hour <dbl>, Upload.speed..Mbit.s.Max <dbl>,
#> #   Upload.speed..Mbit.s.8.10pm.weekday <dbl>,
#> #   DNS.resolution..ms.24.hour <dbl>, DNS.failure....24.hour <dbl>,
#> #   DNS.resolution..ms.8.10pm.weekday <dbl>,
#> #   DNS.failure....8.10pm.weekday <dbl>, Jitter.up..ms.24.hour <dbl>,
#> #   Jitter.down..ms.24.hour <dbl>, Jitter.up..ms.8.10pm.weekday <dbl>,
#> #   Jitter.down..ms.8.10pm.weekday <dbl>, Latency..ms.24.hour <dbl>,
#> #   Packet.loss....24.hour <chr>, Latency..ms.8.10pm.weekday <dbl>,
#> #   Packet.loss....8.10pm.weekday <chr>, Web.page..ms.24.hour <dbl>,
#> #   Web.page..ms.8.10pm.weekday <dbl>, ISP.weights <dbl>,
#> #   Nat.weights <dbl>
```


```r
head(as.tibble(broadband2014))
#> # A tibble: 6 x 31
#>      Id Distance.band Distance.band.u~ Urban.rural Market ISP   Technology
#>   <int> <chr>         <chr>            <chr>        <int> <chr> <chr>     
#> 1  1302 2563-5000     ""               Urban            3 Virg~ Cable     
#> 2  1308 0-385         ""               Urban            3 BT    FTTC      
#> 3  1318 1367-1635     1299-1680        Urban            3 Talk~ ADSL      
#> 4  1319 1992-2563     1680-2274        Urban            3 Sky   ADSL      
#> 5  1321 763-946       ""               Urban            3 BT    FTTC      
#> 6  1322 582-763       ""               Urban            2 Sky   FTTC      
#> # ... with 24 more variables: LLU <chr>, Headline.speed <int>,
#> #   Download.speed..Mbit.s..24.hrs <dbl>,
#> #   Download.speed..Mbit.s..Max <dbl>,
#> #   Download.speed..Mbit.s..8.10pm.weekday <dbl>,
#> #   Upload.speed..Mbit.s.24.hour <dbl>, Upload.speed..Mbit.s.Max <dbl>,
#> #   Upload.speed..Mbit.s.8.10pm.weekday <dbl>,
#> #   DNS.resolution..ms.24.hour <dbl>, DNS.failure....24.hour <dbl>,
#> #   DNS.resolution..ms.8.10pm.weekday <dbl>,
#> #   DNS.failure....8.10pm.weekday <dbl>, Jitter.up..ms.24.hour <dbl>,
#> #   Jitter.down..ms.24.hour <dbl>, Jitter.up..ms.8.10pm.weekday <dbl>,
#> #   Jitter.down..ms.8.10pm.weekday <dbl>, Latency..ms.24.hour <dbl>,
#> #   Packet.loss....24.hour <dbl>, Latency..ms.8.10pm.weekday <dbl>,
#> #   Packet.loss....8.10pm.weekday <dbl>, Web.page..ms.24.hour <dbl>,
#> #   Web.page..ms.8.10pm.weekday <dbl>, isp.weights <dbl>,
#> #   nat.weights <dbl>
```

We see that new columns were introduced in 2014, the original column order is 
not preserved, column names also change over time, as do the encodings in 
certain categorical columns.

### Preprocessing

This prototype release of `datadiff` does not include automatic data type inference, so a little preprocessing is necessary.


```r
broadband2013[["ID"]][broadband2013[["ID"]] == "FTTC"] <- NA
broadband2013$ID <- as.integer(broadband2013$ID)

strip_non_numeric <- function(x) { gsub("[^0-9\\.]", "", x) }
broadband2013[["Headline.speed"]] <- as.integer(strip_non_numeric(broadband2013[["Headline.speed"]]))
broadband2013[["Packet.loss....24.hour"]] <- as.numeric(strip_non_numeric(broadband2013[["Packet.loss....24.hour"]]))
broadband2013[["Packet.loss....8.10pm.weekday"]] <- as.numeric(strip_non_numeric(broadband2013[["Packet.loss....8.10pm.weekday"]]))
```

## Detecting structural changes

Apply the `ddiff` function to the two samples:


```r
result <- ddiff(broadband2014, broadband2013)
```

The result is a function which transforms the structure of the corrupted dataset to match that of the original. We call this a patch.


```r
print(result, broadband2014)
#> Composed patch with elementary constituents:
#> Recode patch.
#>   cols: Technology
#>   encoding: ADSL -> DSL, Cable -> ADSL2+, FTTC -> FTTx, FTTP -> 
#>   one_to_one: TRUE
#> Rescale patch.
#>   cols: Packet.loss....24.hour
#>   shift: -0.17
#>   scale_factor: 280.505
#> Rescale patch.
#>   cols: Packet.loss....8.10pm.weekday
#>   shift: 0.014
#>   scale_factor: 113.909
#> Delete patch.
#>   cols: LLU
#> Delete patch.
#>   cols: Distance.band.used.for.weighting
#> Permute patch.
#>   Technology -> Headline.speed
#>   Headline.speed -> Technology
```

The proposed patch involves:

 - recoding of the categories in the _Technology_ column
 - rescaling of columns _Packet loss 24-hour_ and _Packet loss 8-10pm weekday_
 - deletion of columns _LLU_ and _Distance.band.used.for.weighting_
 - transposition of columns _Headline.speed_ and _Technology_.

Executing the patch on the 2014 data we obtain a table consistent with the 2013 format:


```r
head(as.tibble(result(broadband2014)))
#> # A tibble: 6 x 29
#>      Id Distance.band Urban.rural Market ISP   Headline.speed Technology
#>   <int> <chr>         <chr>        <int> <chr>          <int> <chr>     
#> 1  1302 2563-5000     Urban            3 Virg~             60 ADSL2+    
#> 2  1308 0-385         Urban            3 BT                80 FTTx      
#> 3  1318 1367-1635     Urban            3 Talk~             20 DSL       
#> 4  1319 1992-2563     Urban            3 Sky               20 DSL       
#> 5  1321 763-946       Urban            3 BT                40 FTTx      
#> 6  1322 582-763       Urban            2 Sky               40 FTTx      
#> # ... with 22 more variables: Download.speed..Mbit.s..24.hrs <dbl>,
#> #   Download.speed..Mbit.s..Max <dbl>,
#> #   Download.speed..Mbit.s..8.10pm.weekday <dbl>,
#> #   Upload.speed..Mbit.s.24.hour <dbl>, Upload.speed..Mbit.s.Max <dbl>,
#> #   Upload.speed..Mbit.s.8.10pm.weekday <dbl>,
#> #   DNS.resolution..ms.24.hour <dbl>, DNS.failure....24.hour <dbl>,
#> #   DNS.resolution..ms.8.10pm.weekday <dbl>,
#> #   DNS.failure....8.10pm.weekday <dbl>, Jitter.up..ms.24.hour <dbl>,
#> #   Jitter.down..ms.24.hour <dbl>, Jitter.up..ms.8.10pm.weekday <dbl>,
#> #   Jitter.down..ms.8.10pm.weekday <dbl>, Latency..ms.24.hour <dbl>,
#> #   Packet.loss....24.hour <dbl>, Latency..ms.8.10pm.weekday <dbl>,
#> #   Packet.loss....8.10pm.weekday <dbl>, Web.page..ms.24.hour <dbl>,
#> #   Web.page..ms.8.10pm.weekday <dbl>, isp.weights <dbl>,
#> #   nat.weights <dbl>
```

## Calibration

The `ddiff` function takes a collection of "penalty" parameters, one for each patch type. These penalties influence the way datadiff selects from the space of possible patches to identify the most promising candidate. The greater the penalty, the less likely a patch of that type will be included in the result.

By default, datadiff uses a set of penalty parameters that have been found to perform well on a collection of training datasets. However, optimal parameters are data-dependent and therefore it may be necessary to calibrate the tool for best performance.

To calibrate datadiff, execute the `iterative_calibration` function, passing the name of one or more target data frames. The return value is a named vector of penalty parameter values optimised for the given data.


