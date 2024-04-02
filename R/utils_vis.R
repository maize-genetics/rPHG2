convertHapIdToNumber <- function(m) {
    rowNames <- rownames(m)
    m <- apply(m, 2, \(it) {
        it |>
            as.factor() |>
            as.numeric()
    })

    rownames(m) <- rowNames

    return(m)
}

convertMatrixToDf <- function(m) {
    # Get row and column names
    row_names <- rownames(m)
    col_names <- colnames(m)

    # Create a dataframe to store melted data
    meltedData <- data.frame(
        row_name = character(0),
        column_name = character(0),
        value = numeric(0)
    )

    # Loop through each cell in the matrix and append to meltedData
    for (i in 1:nrow(m)) {
        for (j in 1:ncol(m)) {
            meltedData <- rbind(
                meltedData,
                data.frame(
                    y = row_names[i],
                    x = col_names[j],
                    value = m[i, j]
                )
            )
        }
    }

    return(meltedData)
}










