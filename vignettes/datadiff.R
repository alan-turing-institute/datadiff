## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(datadiff)
library(tibble)
# set.seed(1014)

## ------------------------------------------------------------------------
head(as.tibble(broadband2013))

## ------------------------------------------------------------------------
head(as.tibble(broadband2014))

## ------------------------------------------------------------------------
ddiff(broadband2014, broadband2013)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

