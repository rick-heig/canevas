package cnv

import org.apache.commons.math3.stat.StatUtils

import htsjdk.samtools.SamReaderFactory
import htsjdk.samtools.SAMFileWriterFactory
import htsjdk.samtools.SAMRecord

import java.io.File

//import collection.JavaConverters._ // Deprecated
import scala.jdk.CollectionConverters._

import cnv.CommonGenomics._
import scala.collection.immutable.SortedMap
import cnv.CommonGenomics.Position
import scala.annotation.tailrec
import scala.collection.mutable

object SamToolBox {

  def readerFromSamFile(fileName: String) = {
    SamReaderFactory.makeDefault().open(new File(fileName))
  }

  /** Get Scala SAM records iterator of BAM/SAM file */
  def getScalaIterator(fileName: String) = {
    val it = readerFromSamFile(fileName).iterator().asScala
    it
  }

  /** This is a test function */
  def printReadBases(fileName: String) = {
    val it = getScalaIterator(fileName)
    it foreach {
      rec => {
        println(new String(rec.getReadBases()))
      }
    }
  }

  // Check doc : https://www.javadoc.io/doc/com.github.samtools/htsjdk/2.20.3/htsjdk/samtools/SAMRecord.html
  def testFF(fileName: String) = {
    val it = getScalaIterator(fileName)
    it foreach {
      rec => {
        val alignementStart = rec.getStart()

        // It is possible to see if the mate is unmapped
        val mateUnmapped = rec.getMateUnmappedFlag()
        if (mateUnmapped) {
          println(rec)
          println("Mate unmapped !")
        }

        // It is possible to get the mapping quality
        println("Rec : " + rec + " Mapping quality : " + rec.getMappingQuality())

        // It is possible to see where the mate is mapped
        // rec.getMateAlignmentStart()

        // It is possible to see if the mate is on the negative strand
        // rec.getMateNegativeStrandFlag()

        // It is possible to get the unclipped start/end (whatever that means)
        if ((rec.getStart() != rec.getUnclippedStart()) || (rec.getEnd() != rec.getUnclippedEnd())) {
          println(rec)
          println("Unclipped start : " + rec.getUnclippedStart() + " unclipped end : " + rec.getUnclippedEnd())
        }

        // It is possible to
        if (rec.getProperPairFlag() == false) {
          println("Rec: " + rec + " is not properly paired")
        }

        // It is possible to
        println("Rec : " + rec + " has an inferred insert size of : " + rec.getInferredInsertSize())

        // It is possible to see if an alignment is clipped
        if (rec.getCigar().isClipped()) {
          println("Rec " + rec + " is clipped")
        }
      }
    }
  }

  def getNumberOfRecords(fileName: String) = {
    val it = getScalaIterator(fileName)
    val length = it.length
    length
  }

  def getNumberOfUnmappedMates(fileName: String) = {
    val it = getScalaIterator(fileName)
    val length = (it filter (rec => rec.getMateUnmappedFlag())).toList.length
    length
  }

  // Extract reads according to predicate can be done with filter on getScalaIterator()
  // The readUnmappedFlag must be checked to determine wether or not a read is mapped (if not the alignment data should not be interpreted)
  // Also the presence of mate name/index and alignment start does not mean the mate is mapped (check mateUnmappedFLag flag)
  case class IncrementalModifier(positive: Int, negative: Int)
  
  /** Updates a map with a record, value of update is given by a function, does not check if record is for mapped read */
  def updateMapWithRecord(sm: scala.collection.immutable.SortedMap[Position, IncrementalModifier], record: SAMRecord, valueFromRecord: (SAMRecord) => Int) = {
    /** Note : The performance could be greatly improved if we know that all the positions refer to the same region, the Map will not have to check
     * the region (ref) for each entry. This would allow to change the map key from Position(String, Int) to Int */

    // Get old entries (if none create default ones) and udpate them
    val start  = record.getStart()
    val stop   = record.getEnd()
    val region = record.getReferenceName()
    val startPosition = Position(region, start)
    val stopPosition  = Position(region, stop)

    val oldStartEntry = sm.getOrElse(startPosition, IncrementalModifier(0,0))
    val oldStopEntry  = sm.getOrElse(stopPosition, IncrementalModifier(0,0))

    val updateValue = valueFromRecord(record)

    val updatedStartEntry = IncrementalModifier(oldStartEntry.positive + updateValue, oldStartEntry.negative)
    val updatedStopEntry  = IncrementalModifier(oldStopEntry.positive, oldStopEntry.negative + updateValue)

    // Update the map itself
    (sm - startPosition - stopPosition) + (startPosition -> updatedStartEntry) + (stopPosition -> updatedStopEntry)
  }

  /** Updates a map with a record, does not check if record is for mapped read */
  def updatedMapWithRecord(sm: scala.collection.immutable.SortedMap[Position, IncrementalModifier], record: SAMRecord) = {
    // TODO : Compare performance of this, relative to not using the lambda
    updateMapWithRecord(sm, record, (_: SAMRecord) => {1})
  }

  def updateMutableMapWithRecord(sm: scala.collection.mutable.SortedMap[Position, IncrementalModifier], record: SAMRecord) = {
        // Get old entries (if none create default ones) and udpate them
        val start  = record.getStart()
        val stop   = record.getEnd()
        val region = record.getReferenceName()
        val startPosition = Position(region, start)
        val stopPosition  = Position(region, stop)
    
        val oldStartEntry = sm.getOrElse(startPosition, IncrementalModifier(0,0))
        val oldStopEntry  = sm.getOrElse(stopPosition, IncrementalModifier(0,0))
    
        val updatedStartEntry = IncrementalModifier(oldStartEntry.positive + 1, oldStartEntry.negative)
        val updatedStopEntry  = IncrementalModifier(oldStopEntry.positive, oldStopEntry.negative + 1)
    
        // Update the map itself
        sm.update(startPosition, updatedStartEntry)
        sm.update(stopPosition, updatedStopEntry)
  }

  /** Creates an immutable sorted map from reads given a predicate */
  def createSortedMapFromReadsGivenPredicateFromIt(it: Iterator[SAMRecord], pred: (SAMRecord) => Boolean) = {
    val emptyMap: scala.collection.immutable.SortedMap[Position, IncrementalModifier] = SortedMap()(Ordering.by(e => (e.region, e.pos)))
    (it filter pred).foldLeft(emptyMap){(map, record) => updatedMapWithRecord(map, record)}
  }

  /** Creates an immutable sorted map from reads given a predicate */
  def createSortedMapFromReadsGivenPredicate(fileName: String, pred: (SAMRecord) => Boolean, region: String = "") = {
    val it = if (region == "") {
      getScalaIterator(fileName)
    } else {
      getIteratorOnRegion(fileName, region)
    }

    createSortedMapFromReadsGivenPredicateFromIt(it, pred)
  }

  /** Creates a mutable sorted map from reads given a predicate */
  @deprecated
  def createMutableSortedMapFromReadsGivenPredicate(fileName: String, pred: (SAMRecord) => Boolean) = {
    val it = getScalaIterator(fileName)

    val map: scala.collection.mutable.SortedMap[Position, IncrementalModifier] = scala.collection.mutable.SortedMap()(Ordering.by(e => (e.region, e.pos)))
    (it filter pred) foreach {record => updateMutableMapWithRecord(map, record)}
    map
  }

  /* This is actually much slower than the foldLeft version and uses much more CPU 
   * Probably because it handles the iterator element by element with the .nextOption()
   * while foldLeft() is optimized */
  @deprecated
  def createSortedMapFromReadsGivenPredicateTR(fileName: String, pred: (SAMRecord) => Boolean) = {
    val it = getScalaIterator(fileName)

    val emptyMap: scala.collection.immutable.SortedMap[Position, IncrementalModifier] = SortedMap()(Ordering.by(e => (e.region, e.pos)))

    @tailrec
    def doFoldLeft(it: Iterator[SAMRecord], acc: scala.collection.immutable.SortedMap[Position, IncrementalModifier]): scala.collection.immutable.SortedMap[Position, IncrementalModifier] = {
      it.nextOption() match {
        case None => acc
        case Some(record) => doFoldLeft(it, updatedMapWithRecord(acc, record))
      }
    }

    doFoldLeft(it, emptyMap)
  }

  /** Returns true if the edit distance (SAM extra tag NM) is above a threhsold */
  def editDistanceAboveThreshold(rec: SAMRecord, threshold: Int): Boolean = {
    rec.getAttribute("NM") match {
      case null => false
      case attr: Object => attr.toString.toInt > threshold
    }
  }

  /* Example :
    val pred = editDistanceAboveThreshold(_, 10)
    val sm = createSortedMapFromAlignedReadsGivenPredicate("chr8.bam", pred)
    val it = getRangeIteratorOfIncrementalMap(sm)
    Entries(it.map(e => BedEntry(BedSpan(e.range.region, e.range.start-1, e.range.stop-1), e.value)).iterator, "./testbg_NM_chr8.bedgraph")
    // load it with IGV, then check variants
    interactiveViewSession("NA12878.sorted.vcf", 60151) // or maybe better :
    scaledInteractiveViewSession("NA12878.sorted.vcf", 60151, 10.0)
  */

  /** Creates an immutable sorted map from aligned reads given a predicate */
  def createSortedMapFromAlignedReadsGivenPredicate(fileName: String, pred: (SAMRecord) => Boolean, region: String = "") = {
    // Examples :
    // val sm = createSortedMapFromAlignedReadsGivenPredicate("input.bam", (rec) => { rec.getCigar().isClipped() })
    // val sm = createSortedMapFromAlignedReadsGivenPredicate("input.bam", (rec) => { rec.getMappingQuality() <= 0 })
    // val sm = createSortedMapFromAlignedReadsGivenPredicate("input.bam", (rec) => { rec.getMappingQuality() <= 0 })
    createSortedMapFromReadsGivenPredicate(fileName, (rec: SAMRecord) => {pred(rec) && !rec.getReadUnmappedFlag()}, region)
  }

  /** Get iterator on incremental map with summed values */
  def getRangeIteratorOfIncrementalMap(sm: scala.collection.immutable.SortedMap[Position, IncrementalModifier]) = {
    if (!sm.isEmpty) {
      // TODO : Maybe change this to the start of the region
      val start = Range(sm.head._1.region, sm.head._1.pos - 1, sm.head._1.pos)
      val startValue = IntValueOfRange(start, 0)
      //
      //// This does not work well when ranges cross regions (e.g., 2 separate chromosomes, they will be merged on the first)
      //sm.scanLeft(startValue)((rangeValue, entry) => {
      //  IntValueOfRange(Range(rangeValue.range.region, rangeValue.range.stop, entry._1.pos), rangeValue.value + entry._2.positive - entry._2.negative)
      //})

      sm.toIterator.sliding(2).scanLeft(startValue)((value, entryPair) => {
        entryPair match {
          case Seq(e1, e2) => {
            // TODO handle different regions
            IntValueOfRange(Range(e1._1.region, e1._1.pos, e2._1.pos), value.value + e1._2.positive - e1._2.negative)
          }
        }
      })
    } else {
      List[IntValueOfRange]().iterator
    }
  }

  /** Get iterator on incremental map with summed values */
  def getRangeIteratorOfIncrementalMap(sm: scala.collection.immutable.SortedMap[Int, IncrementalModifier], region: String) = {
    if (!sm.isEmpty) {
      // TODO : Maybe change this to the start of the region
      val start = Range(region, sm.head._1 - 1, sm.head._1)
      val startValue = IntValueOfRange(start, 0)
      //
      //// This does not work well when ranges cross regions (e.g., 2 separate chromosomes, they will be merged on the first)
      //sm.scanLeft(startValue)((rangeValue, entry) => {
      //  IntValueOfRange(Range(rangeValue.range.region, rangeValue.range.stop, entry._1.pos), rangeValue.value + entry._2.positive - entry._2.negative)
      //})

      sm.toIterator.sliding(2).scanLeft(startValue)((value, entryPair) => {
        entryPair match {
          case Seq(e1, e2) => {
            // TODO handle different regions
            IntValueOfRange(Range(region, e1._1, e2._1), value.value + e1._2.positive - e1._2.negative)
          }
        }
      })
    } else {
      List[IntValueOfRange]().iterator
    }
  }
  
  // Notes :
  // import htsjdk.samtools.reference._
  // val refFa = new IndexedFastaSequenceFile(new File("hg19.fa.gz")) // require the .fa.fai
  // val chr20Test = reffa.getSubsequenceAt("chr20", 10019407, 10028665)

  // val it = getScalaIterator("/file.bam")
  // val rec = it.next
  // rec.getAttributes.asScala foreach {e => println("Tag : " + e.tag + " value : " + e.value)}
  // Tag : MC value : 151M
  // Tag : MD value : 22T26T13T36A32T2
  // Tag : PG value : MarkDuplicates
  // Tag : RG value : H0164.2
  // Tag : NM value : 7
  // Tag : MQ value : 60
  // Tag : OQ value : AFF-FFAJ<AAF-J-AJ-F<F7-JJ-AFAJJJJJFJAA7-JF-JA-<FA-<JJJF<----<<J<F<A-<-<F--FFFJJJJJ<JF<F----JAJFJ7AJ-FJ-AJA--7--F-<FJF-AJFJ<J--7AJFA####################
  // Tag : UQ value : 65
  // Tag : AS value : 106

  private val HUMAN_CONTIG_MAX_SIZE = 250_000_000

  def getArrayOfModifiers(fileName: String, updateWithSamRecord: (SAMRecord, scala.collection.mutable.ArrayBuffer[Int]) => Unit, arraySize: Int = HUMAN_CONTIG_MAX_SIZE) = {
    val array = mutable.ArrayBuffer.fill(arraySize)(0)

    getScalaIterator(fileName) foreach {updateWithSamRecord(_, array)}

    array
  }

  def getArrayOfModifiersGivenPredicate(fileName: String, pred: (SAMRecord) => Boolean, arraySize: Int = HUMAN_CONTIG_MAX_SIZE) = {
    val fun = (rec: SAMRecord, array: scala.collection.mutable.ArrayBuffer[Int]) => {
      if (pred(rec)) {
        val start = rec.getAlignmentStart()
        val end = rec.getAlignmentEnd()

        array.update(start, array(start) + 1)
        array.update(end, array(end) - 1)
      }
    }
    getArrayOfModifiers(fileName, fun, arraySize)
  }

  // SAM / BAM global functions
  def getIteratorOnRegion(inBamFile: String, region: String) = {
    val samReader = readerFromSamFile(inBamFile)
    val samIt = samReader.query(region, 0, 0, false).asScala
    samIt
  }

  def getIteratorOnSubRegion(inBamFile: String, region: String, start: Int, stop: Int) = {
    val samReader = readerFromSamFile(inBamFile)
    val samit = samReader.queryOverlapping(region, start, stop).asScala
    samit
  }

  def getIteratorOnSubRegionNonOverlapping(inBamFile: String, region: String, start: Int, stop: Int) = {
    val samReader = readerFromSamFile(inBamFile)
    val samit = samReader.queryContained(region, start, stop).asScala
    samit
  }

  /** Does the same as '$ samtools view -b inBamFile.bam "region" > outBamFile.bam' at similar speed */
  def extractRegion(inBamFile: String, outBamFile: String, region: String) = {
    val it = getIteratorOnRegion(inBamFile, region)
    
    // Reader is to get the header of the source file
    val reader = SamReaderFactory.makeDefault().open(new File(inBamFile))

    // Write BAM file
    val samFileWriter = new SAMFileWriterFactory().makeSAMOrBAMWriter(reader.getFileHeader, true, new File(outBamFile))
    it foreach {samFileWriter.addAlignment(_)}

    samFileWriter.close()
    reader.close()
  }

  /** Extract reads overlapping the region given */
  def extractSubRegionOverlapping(inBamFile: String, outBamFile: String, region: String, start: Int, stop: Int) = {
    val it = getIteratorOnSubRegion(inBamFile, region, start, stop)

    // Reader is to get the header of the source file
    val reader = SamReaderFactory.makeDefault().open(new File(inBamFile))

    // Write BAM file
    val samFileWriter = new SAMFileWriterFactory().makeSAMOrBAMWriter(reader.getFileHeader, true, new File(outBamFile))
    it foreach {samFileWriter.addAlignment(_)}

    samFileWriter.close()
    reader.close()
  }

  /** Write SAM records */
  def writeSAMRecords(samRecords: Iterator[SAMRecord], inBamFileForHeader: String, outBamFile: String) = {
    // Reader is to get the header of the source file
    val reader = SamReaderFactory.makeDefault().open(new File(inBamFileForHeader))

    // Write BAM file
    val samFileWriter = new SAMFileWriterFactory().makeSAMOrBAMWriter(reader.getFileHeader, true, new File(outBamFile))
    samRecords foreach {samFileWriter.addAlignment(_)}

    samFileWriter.close()
    reader.close()
  }

  /** Get regions as map (Name -> Length), filtered (only chromosomes) */
  def getRegions(inBamFile: String) = {
    // Reader is to get the header of the source file
    val reader = SamReaderFactory.makeDefault().open(new File(inBamFile))

    val dict = reader.getFileHeader.getSequenceDictionary.getSequences.asScala
    val list = (dict map {e => (e.getSequenceName, e.getSequenceLength)} filter {_._1.matches("(chr)?([0-9]+|[XxYy])")}).toList
    reader.close()
    Map(list: _*)
  }

  case class PairedEndStats(averageInsertSize: Double, medianInsertSize: Double, insertSizeStdev: Double, madInsertSize: Double, secondPercentileSize: Double, maxPercentileInsertSize: Double)
  /** This is basically the same as is done in IGV, the code is sa reinterpretation of the IGV code found here : https://github.com/igvteam/igv/blob/master/src/main/java/org/broad/igv/tools/PairedEndStats.java */
  def getFragmentSizeThresholds(inBamFile: String, numberOfReads: Int = 10000, minPercentile: Double = 0.5, maxPercentile: Double = 99.5) = {

    val DEFAULT_MINIMUM = 50
    val DEFAULT_MAXIMUM = 1000

    val it = getScalaIterator(inBamFile)

    val insertSizes = (it filter 
    {rec => !rec.getReadUnmappedFlag && !rec.getMateUnmappedFlag() && rec.getReferenceName() == rec.getMateReferenceName() && rec.getMappingQuality > 0 && rec.getInferredInsertSize != 0} map 
    {rec => Math.abs(rec.getInferredInsertSize()).toDouble}).take(numberOfReads).toArray

    val nPairs = insertSizes.length
    val mean   = StatUtils.mean(insertSizes, 0, nPairs);
    val median = StatUtils.percentile(insertSizes, 0, nPairs, 50);
    val stdDev = Math.sqrt(StatUtils.variance(insertSizes, 0, nPairs));

    val deviations = insertSizes map {s => Math.abs(s - median)}

    // MAD, as defined at http://stat.ethz.ch/R-manual/R-devel/library/stats/html/mad.html
    val mad = 1.4826 * StatUtils.percentile(deviations, 50);

    val sec = StatUtils.percentile(insertSizes, 0, nPairs, minPercentile);
    val max = StatUtils.percentile(insertSizes, 0, nPairs, maxPercentile);

    PairedEndStats(mean, median, stdDev, mad, sec, max);
  }

  case class ReadLength(length: Int, homogeneous: Boolean)
  def getReadLength(inBamFile: String, numberOfReads: Int = 10000) = {
    val it = getScalaIterator(inBamFile)

    val readSizes = (it filter 
    {rec => !rec.getReadUnmappedFlag() && !rec.getMateUnmappedFlag() && rec.getMappingQuality() > 0 && !rec.getReadFailsVendorQualityCheckFlag()} map 
    {rec => rec.getReadLength}).take(numberOfReads).distinct.toList

    if (readSizes.length == 1) {
      ReadLength(readSizes.head, true)
    } else {
      ReadLength(readSizes.reduce(_+_) / readSizes.length, false)
    }
  }
}