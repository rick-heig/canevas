# Experimental Tools

The experimental tools are not directly available from the executable. They must be run through an interactive Scala console.

## Launching the scala interactive console

```
cd canevas/canevas
export SBT_OPTS="-Xmx8G" # Provide 8 GB or RAM to SBT
sbt
```

This will launch SBT with 8 GB or RAM

```
sbt:Canevas> console
```

This will launch the console

```
scala> 
```

From the console everything can be loaded and accessed, every function can be directly run and experimented with.

## Tools

### Checking variants in IGV

With IGV open and the desired files loaded in it (bam, vcf, bedgraph etc.) it is possible to control it from the console with the following commands :

```Scala
scala> cnv.CheckingVariantsInIGV.interactiveViewSession("path/to/vcf/file.vcf")
```

If for some reason the network port of IGV was changed it can be specified as an argument

```Scala
scala> cnv.CheckingVariantsInIGV.interactiveViewSession("path/to/vcf/file.vcf", 60151)
```

The user will be shown the first variant extracted from the VCF and by pressing enter IGV will show the variant. Entering a number will skip that number of variants. Entering a string in the form of `chrN` will advance to the first variant of chromosome `N`. Entering anything else will stop the session.

It is also possible to provide a scaling (unzoom) factor with :

```Scala
scala> cnv.CheckingVariantsInIGV.scaledInteractiveViewSession("path/to/vcf/file.vcf", 60151 /* IGV Port */, 5.0 /* Scaling factor */)
```

#### Advanced

Or check any location with IGV based on any collection with :

```Scala
scala> cnv.CheckingVariantsInIGV.genericInteractiveViewSession(it: Iterator[T], conv: T => cnv.CommonGenomics.GenomicWindow, port: Int)
```

Were a function must be provided to convert the iterator on the collection of generic type `T` to `cnv.CommonGenomics.GenomicWindow`. This is helpful for developers that want to check a collection of e.g., variants, bed entries, loci, etc. interactively through IGV, the only requirement is a conversion function which can be given inline in the call.

### Comparing VCF files

VCF files can be compared with the function available in `cnv.BenchMarkGraph.compareTwoVcfs`. This function does not care of the type of call in the VCF so we only want to compare for example "SVTYPE=DEL" entries, the VCF files should be filtered first.

```Scala
scala> import cnv.BenchMarkGraph._
scala> compareTwoVcfs("path/to/truth/set/VCF.vcf", "path/to/candidate/VCF.vcf", "region", ComparisonParameters(extractIntervalFromPosAndSVLEN, extractIntervalFromPosAndEND, defaultAcceptance))
```

This will give the results in number of true events (all truth set), true positives (both sets), false negatives (only in truth set), false positives (only  in candidate set).

Specific comparison parameters ares passed through the last argument to the function which is of type `ComparisonParameters`.

```Scala
case class ComparisonParameters(
  getBoundariesFromTruthEntry: (VcfEntry) => Interval,
  getBoundariesFromCandidateEntry: (VcfEntry) => Interval,
  acceptanceFunction: (Interval, Interval) => Boolean = defaultAcceptance,
  generateFiles: Boolean = false)
```

This allows to specify functions to get the boundaries of the entry (some VCF use position + SVLEN, others use position + END, sometimes both). For deletions and inversions this does not really matter, however for insertions the insertion site may be very narrow (position + END) but the SVLEN may represent the length of the inserted sequence.

The acceptance function takes two intervals (given by the functions above) and returns a boolean if they are similar enough (by default based on reciprocal overlap)

The last option allows to generate the "found" "missed" "false" VCF callsets, which are the true positives, false negatives, and false positives calls respectively.

### Predictors

The experimental predictors or callers can be called from the console.

There are several in `Predictors.scala` and `ImprovedPredictors.scala`

### VCF File related

#### Filter VCF File

It is possible to filter a VCF file on any predicate with the following function

```Scala
cnv.VcfToolBox.filterVcfFile(vcfInputFile: String, vcfOutputFile: String, line: String => filter)
```

Example (get only deletions)

```Scala
cnv.VcfToolBox.filterVcfFile("path/to/input/file.vcf", "path/to/output/file.vcf", (line: String) => line.contains("SVTYPE=DEL"))
```

### Assembly 

Assembly can be played with through the console.

```Scala
import cnv.GraphG._
import cnv.AssemblyGraph._
val readsList = cnv.SamToolBox.getIteratorOnSubRegion("path/to/bamfile.bam", "chr8" /* Contig, region */, 897554 /* Start */, 898434 /* Stop */).toList
// Check the number of reads
readsList.size
// Create the de Bruijn Graph
val dbg = createDBGGraphFromReads(readsList)
// Check the adjacency map size
dbg.adjacencyMap.size
// Check the max weight
dbg.edgeProperties.maxBy(_._2)
import cnv.MyMath._
// Check the mean weight
mean(dbg.edgeProperties.values map {_.weight})
// Check the number of edges
dbg.adjacencyMap.values.map(_.toList).flatten.size
// Check the edge properties (meta data) table size (is the same at creation)
dbg.edgeProperties.size
// Do some pruning
val pruned = pruneBasedOnPredicate(dbg, (prop: SimpleWeight) => prop.weight < 10)
// Check the number of edges
pruned.adjacencyMap.values.map(_.toList).flatten.size
// Find a path
val path = test.GraphG.singleStrandPathes(findStartingPoints(pruned), pruned).head
// Show the path
path map {pruned.vertexProperties.get(_).get}
// Generate path contig
val contig = (path map {pruned.vertexProperties.get(_).get.kmer}).foldLeft(pruned.vertexProperties.get(path.head).get.kmer)((str, kmer) => str + kmer.last)

// Connect to gephi
val gephiConnection = cnv.Things.connectToGephi(60150)
// Send the graph to gephi - The workspace name must be the same as in gephi
cnv.GraphToGephi.sendGraphToGephiG(dbg, gephiConnection, "workspace1")
cnv.GraphToGephi.sendGraphToGephiG(pruned, gephiConnection, "workspace2")
val pruned3 = pruneBasedOnPredicate(dbg, (prop: SimpleWeight) => prop.weight < 3)
cnv.GraphToGephi.sendGraphToGephiG(pruned3, gephiConnection, "workspace3")
```

