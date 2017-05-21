# For the CSV breach data from privacyrights.org.
# https://www.privacyrights.org/data-breaches

library(dplyr)
library(stringr)

prep.rb <- function(fnam="select-all.csv") {
    d <- read.csv(fnam, as.is=TRUE)
    orig <- d
    d$Original.Row <- 1:nrow(d)
    omit <- rep(FALSE, nrow(orig))

    x <- d$Records.Breached

    do.not.know <- c(
        "^unknown",
        "^hundreds",
        "^\"hundreds\"",
        "^thousands",
        "^at least",
        "^more than",
        "^over",
        "^considerably more than",
        "^up to ",
        "^fewer than ",
        "^between ",
        "^not disclosed"
    )        
    for (re in do.not.know) {
        omit[grep(re, x, ignore.case=T)] <- TRUE
    }

    omit[x == "hundreds of patients"] <- TRUE
    omit[grep("revised to 0", x, ignore.case=T)] <- TRUE

    for (i in c("about", "approximately", "approx\\.", "potentially")) {
        re <- paste("^", i, "\\s*", sep="")
        omit[grep(re, x, ignore.case=T, perl=T)] <- TRUE
    }

    x <- gsub("([0-9]),([0-9])", "\\1\\2", x)
    x <- gsub("&nbsp;", " ", x, ignore.case=T)
    x <- gsub("\\(not .*ssn.*\\)", "", x, ignore.case=T)
    x <- gsub("\\(no .*ssn.*\\)", "", x, ignore.case=T)
    x <- gsub("\\(not .*total.*\\)", "", x, ignore.case=T)
    x <- gsub("\\(no .*total.*\\)", "", x, ignore.case=T)
    x <- gsub("\\(no incidents reported\\)", "", x, ignore.case=T)
    x <- gsub("\\(not added to total\\)", "", x, ignore.case=T)
    x <- gsub("not added to total", "", x, ignore.case=T)
    x <- gsub("[0-9]+,*\\supdated amount ([0-9,]+)", "\\1", x, ignore.case=T, perl=T)
    x <- gsub("(\\d+)\\s+not.*total", "\\1", perl=T, ignore.case=T, x)
    x <- str_trim(x)
    x <- gsub("^(\\d+)\\s+because it's included.*", "\\1", x, perl=T, ignore.case=T)
    x <- gsub("^(\\d+)\\s+.*SSN.*", "\\1", x, perl=T)

    # "5247. It does not appear that SSNs or financial account                numbers were exposed."
    # "280000. It is not clear that SSNs or financial account                numbers were exposed." 
    x <- gsub("^([0-9]+)\\..*", "\\1", x)

    for (i in c(
        "households", "students", "patients", "current", "children",
        "people", "past", "financial", "taxpayers", "employees",
        "customers", "veterans", "villanova", "high school",
        "north carolina", "former", "victims", "magazine",
        "RNC", "records", "upward bound students", "fund shareholders",
        "assembly members", "policyholders", "individuals",
        "in Hawaii", "Con Edison"
    )) {
        re <- paste("^([0-9]+)\\s", i, ".*", sep="")
        x <- gsub(re, "\\1", x, ignore.case=T)
    }

    x <- gsub(".* raised to ", "", x, ignore.case=T)
    x <- gsub("^([0-9]+)\\s*\\n.*", "\\1", x)

    x <- gsub("^\\s*(\\d+)\\s+million", "\\1000000", x, ignore.case=T, perl=T)

    omit[grep("^\\d+\\s*-\\s*\\d+$", x, perl=T)] <- TRUE
    omit[x == ""] <- TRUE

    d$Records.Breached <- x
    list(prepped=d[!omit,], original=orig)
}
