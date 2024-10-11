## rPHG2 0.7
* Added new parameter to `plotHaploCounts()`:
  + `geom`
  + allows user to specify a given geometry (line, bar, or point)
* Added new conditionals for to `buildHaplotypeGraph()`:
  + Checks to see if the JVM is initialized. If not, it will cause an exception 
    and direct user to run `initPhg()`
* Added new class `JvmStats`:
  + Returns internal Java and PHG versions and memory information for JVM
    session
  + Can be instantiated using the `jvmStats()` constructor function
* Added new checks for `initPhg()`
* Added new parameters to `plotGvcf()` function:
  + `colorOverride`: Overrides bar color based on given user defined color
  + `sampleOrder`: Overrides default bar ordering based on user defined
    `character` vector


## rPHG2 0.6
* Added new parameters to `plogtGvcf()`:
  + `mData` and `mVar`
  + Allows user to override default sample color option with categorical data
* Added new generic, `numberOfHaplotypes()`:
  + Returns the number of unique haplotypes found within each reference range or
    the total value found within a `PHGDataSet` object
* Added getters to `PHGDataSet` objects:
  + `numberOfChromosomes()`
  + `numberOfSamples()`
  + `numberOfRefRanges()`
* Changed functions and variables containing `taxa` to `samples`
  + e.g., `numberOfTaxa()` is now `numberOfSamples()`
* Format tweaks to `HaplotypeGraph` objects' `show()` method
* Added new function `plotHaploCount()`:
  + Plots unique haplotype counts for all or given reference ranges
* Added new function `plotHaploDist()`:
  + Plots unique haplotype distribution for entire `PHGDataSet` object


## rPHG2 0.5
* Added new visualization method, `plotGvcf()`:
  + Auto plotting various gVCF metrics
  + Granular metric support through formula subsetting
* Added new accessor and setting method, `seqnames()`
  + Returns all contig IDs found in a `PHGMetrics` object
  + Setter version (`seqnames()<-`) will update old IDs found within a
    `data.frame` object
* Added new default methods to `plotGvcf()` and `plotDot()` for `PHGMetrics`
  objects that only contain one alignment or gVCF dataset


## rPHG2 0.4
* Added vignettes and README updates
  + `vignette("rPHG2")`
* Created build system for website generation
  + [rphg2.maizegenetics.net](https://rphg2.maizegenetics.net)
* Added new class `PHGMetrics`
  + Primary container for PHGv2 metrics data
* Added accessor methods for `PHGMetrics` objects:
  + `metricsIds()`
  + `metricsTable()`
  + `metricsMetaData()`
* Prior methods are also updatable
* Added visualization methods:
  + `plotDot()` - collinearity plotter for alignment data


## rPHG2 0.3
* Added new class, `PHGDataSet`
  + In memory representation of hVCF data sets
* Added new function, `filterRefRanges()`
  + Filters relevant rPHG2 data objects by reference range
  + Requires a `GRanges` object as input
* Added new function, `filterSamples()`
  + Filters relevant rPHG2 data objects by sample ID
  + Input is a basic `character` vector of sample identifiers


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
  + Reads reference range positional information as a `GRanges` object from a 
    connection object


