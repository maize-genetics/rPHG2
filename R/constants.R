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
# Reference map for gVCF metrics
GVCF_MAP <- data.frame(
    id = c(
        "ref_length",
        "num_snps",
        "num_ins",
        "num_del",
        "num_ns",
        "num_bases_inserted",
        "num_bases_deleted",
        "percent_identity_with_ref",
        "percent_mapped_to_ref",
        "mean_insertion_size",
        "median_insertion_size",
        "largest_insertion",
        "mean_deletion_size",
        "median_deletion_size",
        "largest_deletion"
    ),
    plt = c(
        "Length of ref. seq.",
        "# of SNPs",
        "# of insertions",
        "# of deletions",
        "# of Ns",
        "# of bases inserted",
        "# of bases deleted",
        "% identity",
        "% mapped to ref",
        "Mean insertion size",
        "Mdn. insertion size",
        "Largest insertion",
        "Mean deletion size",
        "Mdn. deletion size",
        "Largest deletion"
    ),
    axs = c(
        "bp", "N", "N", "N", "N", "N", "N", "%", "%",
        "bp", "bp", "bp", "bp", "bp", "bp"
    ),
    typ = c(
        "n", "n", "n", "n", "n", "n", "n", "p", "p",
        "n", "n", "n", "n", "n", "n"
    )
)


## ----
# Commonly used JVM and PHG API classes
PHG_JVM <- list(
    "ARRAY_LIST" = "java.util.ArrayList",
    "GEN_UTILS"  = "net.maizegenetics.phgv2.utils.GeneralUtilitiesKt",
    "HAP_GRAPH"  = "net.maizegenetics.phgv2.api.HaplotypeGraph",
    "LIST"       = "java/util/List",
    "R_METHODS"  = "net.maizegenetics.phgv2.rphg.RMethods"
)


## ----
# PHG metric headers and IDs
PHG_METRICS <- list(
    "VALID_ANCHOR_HEADERS" = c(
        "refChr",
        "referenceStart",
        "referenceEnd",
        "queryChr",
        "queryStart",
        "queryEnd",
        "strand",
        "gene",
        "blockIndex",
        "score"
    ),
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
    ),
    "VALID_METRICS_IDS" = c(
        "gvcf",
        "align"
    )
)


