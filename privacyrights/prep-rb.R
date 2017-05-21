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
        "^among the",
        "^unknown",
        "^unkown",                      # I guess I shouldn't be surprised.
        "^unknowm",
        "^hundreds",
        "^\"hundreds\"",
        "^tens\\s+",
        "^thousands",
        "^at least",
        "^more than",
        "^over",
        "^considerably more than",
        "^up to ",
        "^fewer than ",
        "^nearly ",
        "^between ",
        "^not disclosed",
        "^Very few of",
        "^(\\d+)\\s*-\\s*(\\d+)"
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
    x <- gsub("[ -]*not .*total.*", "", x, ignore.case=T)
    x <- gsub("[ -]*not .*number.*", "", x, ignore.case=T)
    x <- gsub("\\(no .*total.*\\)", "", x, ignore.case=T)
    x <- gsub("\\(no incidents reported\\)", "", x, ignore.case=T)
    x <- gsub("\\(not added to total\\)", "", x, ignore.case=T)
    x <- gsub("not added to total", "", x, ignore.case=T)
    x <- gsub("[0-9]+,*\\supdated amount ([0-9,]+)", "\\1", x, ignore.case=T, perl=T)
    x <- gsub("([0-9]+)\\(updated as of.*", "\\1", x, ignore.case=T, perl=T)
    x <- gsub("(\\d+)\\s+not.*total.*", "\\1", perl=T, ignore.case=T, x)
    x <- gsub("(\\d+)\\s+\\(not.*total.*", "\\1", perl=T, ignore.case=T, x)
    x <- gsub("(\\d+)\\s+\\(not.*included.*", "\\1", perl=T, ignore.case=T, x)
    x <- gsub("(\\d+)\\s+not.*included.*", "\\1", perl=T, ignore.case=T, x)
    x <- gsub("^\\s*(\\d+) \\(.*credit card.*", "\\1", x, perl=T, ignore.case=T)
    x <- str_trim(x)
    x <- gsub("^(\\d+)\\s+because it's included.*", "\\1", x, perl=T, ignore.case=T)
    x <- gsub("^(\\d+)\\s+.*SSN.*", "\\1", x, perl=T)
    x <- gsub("^(\\d+)\\s+\\(at least \\1\\s+.*", "\\1", x, perl=T, ignore.case=T)
    x <- gsub("^1800 [+] 23$", "1823", x)
    x <- gsub("\\s*15806, 25000 more later discovered\\s*", "40806", x)

    # "5247. It does not appear that SSNs or financial account                numbers were exposed."
    # "280000. It is not clear that SSNs or financial account                numbers were exposed." 
    x <- gsub("^([0-9]+)\\..*", "\\1", x)

    x <- gsub(".* raised to ", "", x, ignore.case=T)
    x <- gsub("^\\s*(\\d+)\\s+million", "\\1000000", x, ignore.case=T, perl=T)
    x <- gsub("^\\s+12\\.5 million", "12500000", x, ignore.case=T, perl=T)

    for (i in c(
        "households", "students", "patients", "current", "children",
        "people", "past", "financial", "taxpayers", "employees",
        "customers", "veterans", "villanova", "high school",
        "north carolina", "former", "victims", "magazine",
        "RNC", "records", "upward bound students", "fund shareholders",
        "assembly members", "policyholders", "individuals",
        "in Hawaii", "Con Edison", "[[]Updated", "clients",
        "Oregon high school", "New Yorkers", "\\(\\d+ cases of",
        "\\(\\d+ employees", "\\(\\d+ confirmed", "business emails",
        "\\(unknown number", "residents", "\\(between", "\\(two social",
        "\\(\\d+ people", "\\(\\d+ checks", "as communicated",
        "NYSEG customers", "\\(\\d+ encrypted", "american accounts",
        "\\(\\d+ individuals", "\\(\\d*\\s*partial", "\\(nine social",
        "\\(three social", "\\(107\\)", "\\.\\s+updated as of",
        "\\(no social security", "\\(last four", "\\(\\d+ customers"
    )) {
        re <- paste("^([0-9]+)\\s", i, ".*", sep="")
        x <- gsub(re, "\\1", x, ignore.case=T, perl=T)
    }

    x <- gsub("^(\\d+); \\d+ social security.*", "\\1", x, ignore.case=T, perl=T)
    x <- gsub("^(\\d+) \\(\\d+ social security.*", "\\1", x, ignore.case=T, perl=T)
    x <- gsub("^([0-9]+)\\s*\\n.*", "\\1", x)
    x <- gsub("[[](\\d+)[]]", "\\1", x)
    x <- gsub("^(\\d+), although.*", "\\1", x, ignore.case=T, perl=T)
    x <- gsub("^(\\d+) \\(number includes.*", "\\1", x, ignore.case=T, perl=T)
    x <- gsub("^individuals in (\\d+) households.*", "\\1", x, ignore.case=T, perl=T)

    omit[grep("^\\d+\\s*-\\s*\\d+$", x, perl=T)] <- TRUE
    omit[x == ""] <- TRUE

    d$Records.Breached <- x
    list(prepped=d[!omit,], original=orig)
}
