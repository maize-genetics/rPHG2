## ----
# Evaluate to R environment
#
# NOTE:
#   This currently does not work with hVCF variants arrays - 'ASCII' is not
#   a supported domain type...
tileToR <- function(dbUri) {

    dbUri <- normalizePath(dbUri, mustWork = TRUE)

    altHead <- tiledb::tiledb_array(
        uri       = dbUri,
        return_as = "tibble"
    )

    return(aHead[])
}


