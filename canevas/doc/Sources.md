# Introduction

This project was implemented in the Scala programming language. "Scala combines object-oriented and functional programming in one concise, high-level language. Scala's static types help avoid bugs in complex applications, and its  JVM and JavaScript runtimes let you build high-performance systems with easy access to huge ecosystems of libraries" (from https://www.scala-lang.org/). 

https://www.scala-lang.org/

The possibility to execute Scala code in a interactive environment is also a very strong development and debugging tool. The JVM runtime also provides the possibility to access all of the Java libraries making it a perfect tool for bioinformatics. Since Scala runs in the JVM it is possible to port to any system, and is also able to scale in distributed computing platforms, e.g., with the Apache Spark lightning-fast unified analytics engine https://spark.apache.org/. It is also possible to call native libraries (written in C or C++ for example) through the Java Native Interface (JNI) as well as to call other software such as Samtools, GATK, Picard, written in any language to execute a specific task.

The version of Scala used to develop the software for this project is : 2.13.0

# Libraries

This section introduces the external libraries used within the project.

## HTSJDK

HTSJDK is a Java API for high-throughput sequencing data (HTS) formats. It was developed at the Broad Institute and is used extensively in GATK and IGV proving it's maturity and usefulness.

The library is open-source and most of its source code is under MIT license. It offers a unified Java library to access common file formats, such as SAM and VCF, there are also a number of utilities for manipulating HTS data. The source code is available at :

https://github.com/samtools/htsjdk

Integration in Java/Scala projects can be done through the Maven repository available at :

https://mvnrepository.com/artifact/com.github.samtools/htsjdk

This library (version 2.21.1) was used to read files in SAM/BAM format and access or extract all the data related to the alignment records. It was also used to generate output alignments in SAM/BAM format. It was also used to handle reference FASTA files.

## BioJava

BioJava is an open-source project licensed under LGPL 2.1 that provides a Java library for processing biological data. The project aims to simplify bioinformatic analyses by implementing parsers, data structures, and algorithms for common tasks in genomics.

The library website is : 

https://biojava.org/

The source code is available at :

https://github.com/biojava/biojava

Integration in Java/Scala projects can be done through the Maven repository available at :

https://mvnrepository.com/artifact/org.biojava/biojava-core

This library was solely used for the Needleman-Wunsch global alignment algorithm and scoring matrix implementations.

## Apache Commons

Apache Commons is an Apache project focused on all aspects of reusable Java components. Commons is dedicated to one principal goal: creating and maintaining reusable Java components. This is a place for collaboration and sharing, where developers from throughout the Apache community can work together on projects to be shared by the Apache projects and Apache users.

https://commons.apache.org/}

It includes several packages for math, text, and system utilities.

The Statistics package was used to handle statistic computations such as extraction of the read length distribution percentiles. This is the same package that is used in IGV for statistic computations. It is released under the Apache 2.0 license as are the other packages in the Apache Commons project.

# Software implementation of this project

The project is structured into packages, which are a collection of tools and functions. These packages are implemented in the form of Scala objects for the scope of this application but could be implemented as a package (in the Scala/Java sense) together if to be used by another software or directly used as-is by including the source code.

## Software packages developed during this project

This section gives an overview of the software packages developed for this project. They are contained in Scala objects and provide functions to do or implement specific tasks.

### General packages

- Basic
	
  The basic package provides basic interaction with files and general input output. It provides functions to generate a stream that allows to read files line by line from either compressed (gzip) or uncompressed files. It also provides an output stream to write output files either uncompressed or compressed. This makes the handling of input and output files transparent to the developer, the correct processing will be applied based on the file type chosen, e.g., if the user provides a path to a compressed input file, the decompression will be applied to the stream.
	
  This package also provides a function that allows to count the time used by any portion of code, this should be moved to a benchmarking package but was implemented here because it was a very basic and useful function used throughout developments to profile the time required by the execution of any code block.

- BedSignalToolBox
	
  This provides functions to read (extract information) and write BED and bedgraph format files. It also provides basic algorithms on the BED data such as filtering or extracting data based on given properties, as well as, conversion algorithms to convert BED encoded data to other formats and the other way around.
	
- CommonGenomics
	
  This package provides data structures to hold commonly used genomic data (e.g., loci or regions) and basic algorithms to deal with these data, mostly genomic region related, intersecting, merging, sorting algorithms.
	
- RegionSamToolBox & SamToolBox
	
  These two packages provide functions to handle SAM/BAM input files and extract records based on reference location. They also provide filtering capabilities to handle large collections of records.
	
  The packages also provides useful extraction functions, such as extracting the names of the regions of the alignment file, extracting statistics such as the distribution of inferred fragment sizes, extracting the average read length and its uniformity.
  The packages also provide functions to create SAM/BAM record files, for example in order to write alignments of contigs generated by local re-assembly.
	
  The two packages rely heavily on the htsjdk library that provides the low-level functions to parse, read, and write files in the SAM/BAM format.
	
- VcfToolBox
	
  The VCF toolbox is collection of utilities to handle VCF files and variants, both reading and writing VCF files as well as providing a data structures to hold VCF entries, generate them, filter and convert them. 
	
  This package also provides generic transformation algorithms on VCF files such as filtering them given a predicate, this allows for example to quickly generate a new VCF file with only the entries with the "SVTYPE=DEL" parameter or filter VCF files so that only the variants with the "PASS" entry remain. Other functions include re-identifying variants (giving them a new ID) or sorting them by any property (e.g, size).
	
- EndToEnd
	
  Implements some end-to-end functionality such as calling variants from input files and generating the final output variant call file.
	
### Prediction and Calling packages

- Predictors
	
  This package includes functions to help create predictors as well as predictors to predict breakpoints, regions, and variants given the input signals.
	
- ImprovedPredictors
	
  The improved predictor package holds more advanced predictors such as insertion predictors, copy number predictors and interchromosomal breakpoint predictors. This package also includes variant calling algorithms. These include deletion, insertion, inversion, and duplication callers.
	
### Assembly packages

- Alignment
	
  The alignment package provides functions to align sequences such as the Needleman-Wunsch algorithm, helper functions to generate the CIGAR string for alignments, as well as, functions for generating SAM records of aligned sequences. It also provides functions to directly re-assemble and re-align mapped reads from regions of the BAM file and produce alignments for the assembled contigs.
	
- Assembly & AssemblyGraph
	
  These two packages provides the functions to create the de Bruijn Graphs from sequencing reads as well as update and prune assembly graphs, they also provide more advanced functions such as generating contigs, paths, possible mapping positions and other properties from the assembly graphs.
	
- Graph & GraphG
	
  This package (GraphG, Graph is the non generic version) contains abstract graph related data structures and algorithms. This package implements graphs in a generic manner. It implements graphs as generic adjacency lists and is based on the same ideas behind the C++ Boost graph library. As with the C++ version with templates this implementation allows for the nodes, edges and other properties to be of any desired type. Every algorithm is generic and will work with any data type. This package allows for example to create graphs were the nodes are integers, k-mers, SAM alignments or any other type. This not only allows to create any graph but also allows for performance optimizations such as creating a graph with a very simple data type such as an integer that serves as an unique identifier and having a table that links that unique identifier to any other property. This allows to apply all graph related algorithms that do not require the extra properties on a very simple graph and achieve good performance results. The choice is left to the user based on the specific needs.
	
  The augmented de Bruijn graphs for assembly were created this way with graphs that had nodes either being k-mers represented by unique identifiers and augmented with tables (hashtables) of node and edge properties. This allowed to run all the graph algorithms on a simple integer graph and still be able to run the more advanced algorithms such as generating contigs, extracting histograms of possible positions of provenance for the k-mers, querying the reads of origin using the tables and this in an efficient manner.
	
  General graph topology algorithms are also provided here, e.g., find nodes that have only incoming or outgoing edges, find non-looping paths, extract the longest paths from a graph to name a few.
	
  More abstract algorithms are also given such as pruning nodes based on a predicate, which will apply a predicate function to all nodes, remove them if they fulfill it and finally remove unconnected edges.
	
  This package was designed to be compatible in data structure with the C++ Boost graph library and this allows to call the algorithms implemented in that library through the Java Native Interface (JNI). This gives direct access to validated and very optimized implementations of graph algorithms such as Dijkstra's Shortest Paths, Connected Components, Kruskal's Minimum Spanning Tree algorithms, and others, if required https://www.boost.org/doc/libs/1_72_0/libs/graph/doc/index.html

### Visualization packages

- GraphToGephi
	
  This package allows to send graphs created with the graph package introduced above to the program [Gephi](https://gephi.org/) over the network. This interaction makes it possible to dynamically visualize graphs and display their properties as well as change their layout. Gephi was chosen because it is open source and very effective at displaying graphs of a very high number of nodes and edges. The assembly package also provided some basic functions to export graphs as .dot files which could then be printed with [graphviz](https://www.graphviz.org/), however graphviz does not handle big graphs or dense graphs very well.
	
- CheckingVariantsInIGV
	
  This package provides the functionality of reading VCF files or any collection that can be converted to a genomic region and interacting with the user in order to send commands to IGV to display the variants or regions. Functionality includes zoom factor around the event, interactive search for events in a specific region (chromosome), being able to skip events etc.
	
- Things
	
  This package ought to be renamed but for now handles everything related with the network connection to Gephi and IGV, it provide functions to connect to these programs at the socket level and send data through these sockets.

- Graphml
	
  Contains a single function to generate a graphml format representation of a DNA sequence from a string in order to generate diagrams with tools such as Dia or Yed.

### Benchmarking packages

- Basic
	
  The basic package, introduced above, did provide the time related profiling function that can evaluate the time required by any code block execution.

- BenchMarkGraph
	
  This is a benchmarking package that provides tools to evaluate results from this project. It can check reciprocal overlap between variant calls, evaluate the statistics of relationships between variant call files. These functions, given a truth set of variant calls, can extract all the statistics in terms of true/false-positives/negatives. It also provides functions to analyze the distances between calls that may be related in order to quantify the differences between callers.
	
  This package also provides functions to generate data for graphs and tables.

### Main program

The software App resides in the "CanevasApp" package (CanevasApp.scala source file) but the main program it calls resides in the "Canevas" package (Canevas.scala source file). This is a placeholder name that was chosen because canevas has the meaning of "rough sketch" or "outline" in French but also is a play on the acronym CNV and is very similar to the name chosen by the CNV caller "Canvas" by Illumina.

The functions of the main program are :

- To implement the signal generator to generate signals from the input whole-genome alignment BAM file.
- To do predictions given the signals (breakpoints, regions of interest).
- To call variants (run callers created using the framework).

Since this is meant as a software framework separate "programs" can be created from this collection of tools, functions, and algorithms. For example [VCFotographer](https://github.com/rick-heig/vcfotographer) a variant photography tool.

# Implementation choices

This section goes over some of the important implementation choices made during this project.

## Reproducibility and Portability

With scientific computations, reproducibility is important, the computations should rely as little as possible on random parameters. In this project this is not a problem since we only extract information from a fixed input file. However, another source of problems with reproducibility is library dependency and system specific parameters. In order to be able to run the program under known conditions it is packaged into a Docker image making it possible to run on any system that supports Docker.

As for imported libraries the version of the imported library is fixed so that unless the developer willingly changes this, the program will always be run with the same version of the libraries. All the external libraries are imported in the Docker image with a fixed version number. This removes the burden from the user to install libraries and dependencies on which the program relies as well as makes sure the same coherent system is used every time.

## Compression

Whole Genome Sequencing data is big, often in the hundreds of gigabytes, therefore compression is key. Having to decompress compressed files for processing can be a major pain and may take up unnecessary disk space. Therefore this project can take "gzipped" (GZIP compressed) files as well as uncompressed files as input. This is implemented in a transparent manner for the user and all output files can be generated GZIP compressed as well. This is implemented as a compression stream, therefore the whole decompressed file never needs to be entirely loaded in memory or on disk.

For example a per base read count in BED format for human chromosome 8 takes 1.7~GB while the compressed file takes less than 400~MB.

The handling of compressed files requires a small amount of extra processing power, however this is counterbalanced by the fact that the compressed files are lighter and therefore require less data transfers from disk which is often one of the major bottlenecks, especially with non solid-state (mechanical) drives.

Building the program as an entity that directly reads and outputs compressed files not only reduces the disk usage footprint but can also improve performance because of this. Therefore, by default it handles all input and output files with compression, the program is however compatible with uncompressed input files and the user can always decompress the output files at his leisure.

## Parallelism

Parallelism was implemented for the most time consuming tasks inside the project. Naturally, the CNV assessment can be run for different samples in parallel by executing multiple instances of the program. Inside the program, the tasks are separated by chromosome (more generally by reference contig). The chromosomes are sorted by size and the tasks for the biggest chromosomes are launched first to optimize the overall runtime. Several sub-tasks can be run in parallel and some tasks have further parallel implementations. This is mostly used for testing tasks rapidly during development, during normal execution, the tasks are only parallelized by chromosomes. The level of parallelism can be set by changing the parameters of the task.

Parallelism was implemented using *Futures*. From the Scala documentation, "Futures provide a way to reason about performing many operations in parallel – in an efficient and non-blocking way. A Future is a placeholder object for a value that may not yet exist. Generally, the value of the Future is supplied concurrently and can subsequently be used. Composing concurrent tasks in this way tends to result in faster, asynchronous, non-blocking parallel code." Futures are a description of a task which will provide a result sometime in the future. Futures can be launched at any time and the result can be waited upon at a point where it is needed. This is similar to *threads* in parallel programming where the function launched by the thread would be the body of the *Future* and waiting on the result is analog to joining the thread.

*Futures* are complemented by *ExecutionContexts* which will handle the creation of threads, joining, and set the levels of parallelism, changing the *ExecutionContext* allows to adapt the global execution of concurrent code if needed. An more in-depth overview is available from the Scala documentation at :

https://docsscala-lang.org/overviews/core/futures.html}

## Unit Testing

Some of the developed packages (scala objects) are accompanied by unit tests created to validate their correct behavior. These are specifications and tests that check if these specifications are met. Having unit tests allows for a higher level of confidence when making changes to existing functions.

Unit tests are also a way to check that important algorithms and computations exhibit the correct behavior. It is common for libraries as well as software to come with a collection of unit tests to validate their behavior. The unit test suite used in this project is ScalaTest, available at :

http://www.scalatest.org/

It provides a Domain Specific Language (DSL) to generate unit tests in an intelligible way. For example :

```scala
class ExampleSpec extends FlatSpec with Matchers {

    "A Stack" should "pop values in last-in-first-out order" in {
        val stack = new Stack[Int]
        stack.push(1)
        stack.push(2)
        stack.pop() should be (2)
        stack.pop() should be (1)
    }

    it should "throw NoSuchElementException if an empty stack is popped" in {
        val emptyStack = new Stack[Int]
        a [NoSuchElementException] should be thrownBy {
            emptyStack.pop()
        }
    }
}
```

And generates readable output messages when the test suite is run, when tests fail, they will also output readable (English) messages.

```shell
$ scala -cp scalatest_2.13-3.1.0.jar org.scalatest.run ExampleSpec
Discovery starting.
Discovery completed in 21 milliseconds.
Run starting. Expected test count is: 2
ExampleSpec:
A Stack
- should pop values in last-in-first-out order
- should throw NoSuchElementException if an empty stack is popped
Run completed in 76 milliseconds.
Total number of tests run: 2
Suites: completed 1, aborted 0
Tests: succeeded 2, failed 0, canceled 0, ignored 0, pending 0
All tests passed.
```