## rPHG2 0.2
* Added JVM connections via `rJava` interface
* Added new class, `HaplotypeGraph`
  + Wrapper for PHGv2 API Java graph object
* Added new class, `PHGLocalCon`
  + Interface to local hVCF database and file connections
* Added new function `initPHG()`
  + Initializes JVM and adds PHGv2 Java JARs to classpath
* Added new function `readHapIds()`
  + Reads haplotype IDs as a `character` matrix from a connection object
* Added new function `readHapIdMetaData()`
  + Reads ALT header metadata for each haplotype ID from a connection object
* Added new function `readHapIdPosMetaData()`
  + Reads ALT header positional metadata for each haplotype ID from a 
    connection object
* Added new function `readSamples()`
  + Reads sample IDs from a connection object
* Added new function `readRefRanges()`
  + Read reference range positional information as a `GRanges` object from a
    connection object

