# For the CSV breach data from privacyrights.org.
# https://www.privacyrights.org/data-breaches

library(dplyr)
library(stringr)

prep.data <- function(fnam="select-all.csv") {
    d <- read.csv(fnam, as.is=TRUE)
    orig <- d
    d$Original.Row <- 1:nrow(d)
    d <- d[-grep("^unknown", d$Total.Records, ignore.case=T),]

    x <- d$Total.Records
    x <- gsub("(\\d),(\\d)", "\\1\\2", x, perl=T)
    d$Total.Records <- x

    list(prepped=d, original=orig)
}
