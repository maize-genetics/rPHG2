## ----
# Get haplotype IDs from JVM graph object
#
# @param jvmGraph A JVM graph object
# @param nThreads Number of threads to use during evaluation
#
# @return A matrix of character elements
hapIdsFromJvmGraph <- function(jvmGraph, nThreads) {
    # Ensure integer type
    nThreads <- as.integer(nThreads)

    interface <- createRMethodInterface()
    jm <- interface$getHapIdMatrixFromGraph(jvmGraph, nThreads)
    rm <- jm |> kotlin2DArrayToRMatrix()

    return(rm)
}


