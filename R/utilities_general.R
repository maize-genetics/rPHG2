## ----
# @title Create mock config file
#
# @description Creates a temporary PHG configuration file to access the
#    provided example database. Mainly for debugging and educational
#    purposes.
#
# @param file User defined output file
# @param host Host service for database
# @param user User ID for database access
# @param password Password for database access
# @param dbType Database architecture used
# @param dbPath Path to DB
createConfigFile <- function(
    file,
    host = "localhost",
    user = "user",
    password = "sqlite",
    dbType = "sqlite",
    dbPath = NULL
) {
    myFile <- file(file, "w")

    if (is.null(dbPath)) {
        dbPath <- system.file(
            "extdata",
            "phg_smallseq_test.db",
            package = "rPHG"
        )
    }

    writeLines(sprintf("host=%s", host), myFile, sep = "\n")
    writeLines(sprintf("user=%s", user), myFile, sep = "\n")
    writeLines(sprintf("password=%s", password), myFile, sep = "\n")
    writeLines(sprintf("DB=%s", dbPath), myFile, sep = "\n")
    writeLines(sprintf("DBtype=%s", dbType), myFile, sep = "\n")

    close(myFile)
}


## ----
# @title Logic support for config files
#
# @description Provides logic checking for config files used in PHG creation.
#
# @param configFile Path to a configuration file for your graph database.
configCatcher <- function(configFile) {

    if (!file.exists(configFile)) {
        stop ("Path to config file does not exist.", call. = FALSE)
    }

    configLines <- readLines(configFile)

    # Check for fields
    mandatoryFields <- c("DB", "DBtype", "host", "password", "user")
    dbTypes         <- c("sqlite", "postgres")
    fieldPatterns   <- paste0("^", mandatoryFields, "=")

    # Create logical matrix for given lines in file (i) and fields (j)
    fcMatrix <- vapply(fieldPatterns, grepl, logical(length(configLines)), configLines)

    # Check for presence of each field
    presentChecks <- apply(fcMatrix, 2, any)

    # Check for duplicates of each field
    dupChecks <- apply(fcMatrix, 2, function(x) {
        ifelse(sum(x, na.rm = TRUE) > 1, TRUE, FALSE)
    })

    names(presentChecks) <- mandatoryFields
    names(dupChecks)     <- mandatoryFields

    if (!all(presentChecks)) {
        stop(
            "Some mandatory connection fields are missing. Missing fields:\n",
            paste0("  * ", names(presentChecks[!presentChecks]), collapse = "\n"),
            call. = FALSE
        )
    }

    if (any(dupChecks)) {
        stop(
            "Some mandatory connection fields are duplicated. Duplicated fields:\n",
            paste0("  * ", names(dupChecks[dupChecks]), collapse = "\n"),
            call. = FALSE
        )
    }

    dbParam     <- trimws(gsub("^DB=|#.*$", "", configLines[grepl("^DB=", configLines)]))
    dbTypeParam <- trimws(gsub("^DBtype=|#.*$", "", configLines[grepl("^DBtype=", configLines)]))

    if (!dbTypeParam %in% dbTypes) {
        stop("Only PostgreSQL (DBtype=postgres) or SQLite (DBtype=sqlite) database types are allowed.", call. = FALSE)
    }

    if (!file.exists(dbParam) && dbTypeParam == "sqlite") {
        stop("Path to database (DB=) in SQLite config file does not exist.", call. = FALSE)
    }
}


## ----
# Parse components of config file into a list object
#
# @param file Path to a configuration file for database
parseConfigFile <- function(file) {
    FIELDS <- c("host", "DB", "DBtype")
    conLines <- readLines(file)

    properties <- vapply(FIELDS, \(x) getProperty(conLines, x), character(1))

    return(stats::setNames(as.list(properties), FIELDS))
}


## ----
# Get property from config file field
#
# @param configLines A character vector of config lines
# @param x A field value
getProperty <- function(configLines, x) {
    regexField <- paste0("^", x, "=")

    matchingLines <- configLines[grepl(regexField, configLines)]

    property <- gsub("^.*=", "", x = matchingLines)

    return(property)
}


## ----
# Convert TASSEL TableReport objects to native `data.frame` objects
#
# @param x A TASSEL `TableReport` object
tableReportToDF <- function(x) {
    rJC <- rJava::J("net/maizegenetics/plugindef/GenerateRCode")
    tabRep <- rJC$tableReportToVectors(x)

    tabRepCols <- lapply(tabRep$dataVector, rJava::.jevalArray)

    tabRepCols <- do.call("data.frame", c(tabRepCols, stringsAsFactors = FALSE))
    colnames(tabRepCols) <- tabRep$columnNames
    colnames(tabRepCols) <- gsub(" ", "_", colnames(tabRepCols))

    return(tibble::as_tibble(tabRepCols))
}


## ----
# Convert method description field string to list from local PHG method call
#
# @param df A PHG method table
descriptionStringToList <- function(s) {
    sList <- lapply(
        X = strsplit(unlist(strsplit(s, "\",\"")), "\":\""),
        FUN = function(i) gsub("\"}|\\{\"", "", x = i)
    )

    names(sList) <- unlist(lapply(sList, function(i) i[1]))
    sList <- lapply(sList, function(i) i[2])

    return(sList)
}


