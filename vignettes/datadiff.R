## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(datadiff)
library(tibble)
# set.seed(1014)

## ---- eval=FALSE---------------------------------------------------------
#  # Install the most recent release from GitHub:
#  # install.packages("devtools")
#  devtools::install_github("datadiff", ref = "v0.2.0")

## ------------------------------------------------------------------------
head(as.tibble(broadband2013))

## ------------------------------------------------------------------------
head(as.tibble(broadband2014))

## ------------------------------------------------------------------------
broadband2013[["ID"]][broadband2013[["ID"]] == "FTTC"] <- NA
broadband2013$ID <- as.integer(broadband2013$ID)

strip_non_numeric <- function(x) { gsub("[^0-9\\.]", "", x) }
broadband2013[["Headline.speed"]] <- as.integer(strip_non_numeric(broadband2013[["Headline.speed"]]))
broadband2013[["Packet.loss....24.hour"]] <- as.numeric(strip_non_numeric(broadband2013[["Packet.loss....24.hour"]]))
broadband2013[["Packet.loss....8.10pm.weekday"]] <- as.numeric(strip_non_numeric(broadband2013[["Packet.loss....8.10pm.weekday"]]))

## ------------------------------------------------------------------------
result <- ddiff(broadband2014, broadband2013)

## ------------------------------------------------------------------------
print(result, broadband2014)

## ------------------------------------------------------------------------
head(as.tibble(result(broadband2014)))

