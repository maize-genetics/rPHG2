## ----
# Get haplotype IDs from JVM graph object
#
# @param jvmGraph A JVM graph object
hapIdsFromJvmGraph <- function(jvmGraph) {
    interface <- createRMethodInterface()
    jm <- jvmGraph |> interface$getHapIdMatrixFromGraph()
    rm <- jm |> kotlinMatToRMatrix()

    return(rm)
}


