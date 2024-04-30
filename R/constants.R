## ----
# Specified BrAPI endpoints
BRAPI_ENDPOINTS <- list(
    "METHOD_TABLE"   = "allelematrix",
    "SAMPLES"        = "samples",
    "SERVER_INFO"    = "serverinfo",
    "VARIANT_TABLES" = "variantTables",
    "VARIANTS"       = "variants"
)


## ----
# Commonly used BrAPI parameters
BRAPI_PARAMS <- list(
    "DEMO_N_RR_SIZE"     = 5,
    "DEMO_N_RR_TOTAL"    = 25,
    "DEMO_N_SAMPLES"     = 5,
    "MAX_N_RR_SIZE"      = 5000,
    "MAX_N_RR_TOTAL"     = 150000,
    "MAX_N_SAMPLES"      = 10000,
    "METHOD_ID_KEY"      = "variantSetDbId=%s",
    "METHOD_RR_SIZE"     = "dimensionCallSetPageSize=%i",
    "METHOD_RR_PAGE"     = "dimensionCallSetPage=%i",
    "METHOD_SAMPLE_SIZE" = "dimensionVariantPageSize=%i",
    "METHOD_SAMPLE_PAGE" = "dimensionVariantPagePage=%i",
    "PAGE_SIZE"          = "pageSize=%i",
    "REST_QUERY"         = "?",
    "REST_KV_SEP"        = "&",
    "VALID_VERSIONS"     = c("v1", "v2"),
    "VALID_PROTOCOLS"    = c("http", "https")
)


## ----
# Commonly used JVM and PHG API classes
PHG_JVM <- list(
    "ARRAY_LIST" = "java.util.ArrayList",
    "HAP_GRAPH"  = "net.maizegenetics.phgv2.api.HaplotypeGraph",
    "LIST"       = "java/util/List",
    "R_METHODS"  = "net.maizegenetics.phgv2.rphg.RMethods"
)


## ----
PHG_METRICS <- list(
    "VALID_GVCF_HEADERS" = c(
        "taxa",
        "chrom",
        "refLength",
        "numSNPs",
        "numIns",
        "numDel",
        "numNs",
        "numBasesInserted",
        "numBasesDeleted",
        "percentIdentityWithRef",
        "percentMappedToRef",
        "meanInsertionSize",
        "medianInsertionSize",
        "largestInsertion",
        "meanDeletionSize",
        "medianDeletionSize",
        "largestDeletion",
        "refRangesWithHaplotype",
        "haplotypesIdenticalToRef"
    )
)


