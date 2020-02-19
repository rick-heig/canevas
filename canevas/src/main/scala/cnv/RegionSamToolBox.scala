package cnv

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

import java.io.File

import cnv.BasicToolBox._
import cnv.CommonGenomics._
import cnv.SamToolBox._
import cnv.BedSignalToolBox._

import htsjdk.samtools._

/** This is similar to SamToolBox but optimized to region specific alignments */
object RegionSamToolBox {

   /** Updates a map with a record, value of update is given by a function, does not check if record is for mapped read */
   def updateMapWithRecord(sm: SortedMap[Int, IncrementalModifier], record: SAMRecord, valueFromRecord: (SAMRecord) => Int) = {
    /** Note : The performance could be greatly improved if we know that all the positions refer to the same region, the Map will not have to check
     * the region (ref) for each entry. This would allow to change the map key from Position(String, Int) to Int */

    // Get old entries (if none create default ones) and udpate them
    val start  = record.getStart()
    val stop   = record.getEnd()

    val oldStartEntry = sm.getOrElse(start, IncrementalModifier(0,0))
    val oldStopEntry  = sm.getOrElse(stop, IncrementalModifier(0,0))

    val updateValue = valueFromRecord(record)

    val updatedStartEntry = IncrementalModifier(oldStartEntry.positive + updateValue, oldStartEntry.negative)
    val updatedStopEntry  = IncrementalModifier(oldStopEntry.positive, oldStopEntry.negative + updateValue)

    // Update the map itself
    (sm - start - stop) + (start -> updatedStartEntry) + (stop -> updatedStopEntry)
  } 

  /** Updates a map with a record, does not check if record is for mapped read */
  def updatedMapWithRecord(sm: SortedMap[Int, IncrementalModifier], record: SAMRecord) = {
    // TODO : Compare performance of this, relative to not using the lambda
    updateMapWithRecord(sm, record, (_: SAMRecord) => {1})
  }

  def createSortedMapFromReadsGivenPredicate(fileName: String, region: String, pred: (SAMRecord) => Boolean) = {
    val it = getIteratorOnRegion(fileName, region)
    val emptyMap: SortedMap[Int, IncrementalModifier] = SortedMap()

    (it filter pred).foldLeft(emptyMap){(map, record) => updatedMapWithRecord(map, record)}
  }

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

  case class RegionSize(region: String, size: Int)

  /** Returns a list of regions (only chr) in bamfile with their sizes */
  def getSizesOfRegions(inBamFile: String) = {
    (getRegions(inBamFile) map {e => RegionSize(e._1, e._2)}).toList
  }

  //def getSizesOfRegions(referenceRegionsFile: String) = {
  //  val it = iteratorOnLineFromFile(referenceRegionsFile)
  //  (it map {
  //    line => {
  //      val split = line.split("\\s+")
  //      RegionSize(split(0), split(1).toInt)
  //    }
  //  }).toList
  //}

  case class SignalExtractor(pred: (SAMRecord) => Boolean, score: (SAMRecord) => Int = (_) => 1, name: String = "No name")
  case class ExtendedSignalExtractor(signalExtractor: SignalExtractor, fileName: String)
  case class ExtractionParameters(split: Int = 1000, showProgress: Boolean = false)
  val DEFAULT_EXTRACTION_PARAMETERS = ExtractionParameters()

  /** Generates multiple signals from a region */
  def extractSignalsFromRegion(inBamFile: String, directory: String, signals: List[ExtendedSignalExtractor], region: String, params: ExtractionParameters = DEFAULT_EXTRACTION_PARAMETERS) = {
    // This function does the same as the one below, except it will use the same iterators for the extraction, requiring to traverse the file only once while generating multiple signals
    val (split, showProgress) = ExtractionParameters.unapply(params).get

    getSizesOfRegions(inBamFile).find(_.region == region) match {
      case None => println("Region '" + region + "' not found in region size file !")
      case Some(value) => {
        val regionSize = value.size
        val splitSize = regionSize / split

        // Split into sub regions
        for (i <- 1 to split) {
          val splitStart = (i-1)*splitSize+1
          val splitEnd = if (i == split) {0} else {i*splitSize}
          // Create a reader and iterator for each subregion
          val reader = SamReaderFactory.makeDefault().open(new File(inBamFile))
          val it = reader.queryOverlapping(region, splitStart, splitEnd).asScala

          val emptyMap: SortedMap[Int, IncrementalModifier] = SortedMap()
          case class AccMap(map: SortedMap[Int, IncrementalModifier] = emptyMap, signal: ExtendedSignalExtractor)
          val emptyMaps = signals map {AccMap(emptyMap, _)}

          // Create maps for all signals at the same time
          val sms = (it filterNot {_.getReadUnmappedFlag()}).foldLeft(emptyMaps)((maps, samRecord) => {
            maps map {
              case AccMap(map, signal) => {
                if (signal.signalExtractor.pred(samRecord)) {
                  AccMap(updateMapWithRecord(map, samRecord, signal.signalExtractor.score), signal)
                } else {
                  AccMap(map, signal)
                }
              }
            }
          })

          val rangeIts = sms map {am => (getRangeIteratorOfIncrementalMap(am.map, region), am.signal)}
          val asBedEntries = rangeIts map {ri => (cnv.EndToEnd.asBedEntries(ri._1), ri._2)}

          // Write the files
          asBedEntries foreach {
            case (asBedEntriesIt, signal) => {
              val outBedGraph = directory + "/" + signal.fileName
              // Filter artefacts from splitting (entries from overlapping reads)
              val filteredBedEntriesIt = asBedEntriesIt.filter(e => (e.span.stop >= splitStart) && (e.span.start <= splitEnd))
              // Adjust the remaining overlapping entries (if the entries are sorted this can be optimized by only changing the head and tail)
              val finalBedEntriesIt = filteredBedEntriesIt map {
                e => e match {
                  case BedEntry(span, _) => {
                    span match {
                      case BedSpan(_, start, stop) if ((start < splitStart) && (stop > splitEnd)) => e.copy(span = e.span.copy(start = splitStart, stop = splitEnd))
                      case BedSpan(_, start, stop) if (start < splitStart) => e.copy(span = e.span.copy(start = splitStart))
                      case BedSpan(_, start, stop) if (stop > splitEnd) => e.copy(span = e.span.copy(stop = splitEnd))
                      case _ => e
                    }
                  }
                }
              }
              if (i == 1) {
                // Create a new bedGraph file
                bedGraphFromEntries(finalBedEntriesIt, outBedGraph, region + " " + signal.signalExtractor.name)
              } else {
                // Append to existing bedGraph file
                bedGraphFromEntriesAppend(finalBedEntriesIt, outBedGraph)
              }
            }
          }

          // Close the reader
          reader.close()
          // Show progress
          if (showProgress) {
            val progress = (i.toDouble/split)*100
            print("\r" + f"$progress%.1f" +"%")
          }
        }
      }
    }
  }

  /** Generates a signal from a region */
  @deprecated // Has not the corrections of the above function applied (filtering and adjuting the entries)
  def extractSignalFromRegion(inBamFile: String, outBedGraph: String, referenceRegionsFile: String, region: String, signalExtractor: SignalExtractor, params: ExtractionParameters = DEFAULT_EXTRACTION_PARAMETERS) = {
    val (pred, score, signalName) = SignalExtractor.unapply(signalExtractor).get
    val (split, showProgress) = ExtractionParameters.unapply(params).get
    
    getSizesOfRegions(inBamFile).find(_.region == region) match {
      case None => println("Region not found in region size file !")
      case Some(value) => {
        val regionSize = value.size
        val splitSize = regionSize / split

        // Split into sub regions
        for (i <- 1 to split) {
          // Create a reader and iterator for each subregion
          val reader = SamReaderFactory.makeDefault().open(new File(inBamFile))
          val it = reader.queryOverlapping(region, (i-1)*splitSize+1, if (i == split) {0} else {i*splitSize}).asScala

          val emptyMap: SortedMap[Int, IncrementalModifier] = SortedMap()
          // Create sorted map for the subregion (position -> incremental modifier), this could be made more generic by having the "score" function return an incremental modifier instead of an Int
          val sm = (it filter {!_.getReadUnmappedFlag()} filter pred).foldLeft(emptyMap)((map, samRecord) => updateMapWithRecord(map, samRecord, score))
          // Transform the map into an iterator on valued ranges i.e., (start, stop, value)
          val rangeIt = getRangeIteratorOfIncrementalMap(sm, region)
          val asBedEntries = cnv.EndToEnd.asBedEntries(rangeIt)

          if (i == 1) {
            // Create a new bedGraph file
            bedGraphFromEntries(asBedEntries, outBedGraph, region + " " + signalName)
          } else {
            // Append to existing bedGraph file
            bedGraphFromEntriesAppend(asBedEntries, outBedGraph)
          }

          // Close the reader
          reader.close()
          // Show progress
          if (showProgress) {
            val progress = (i.toDouble/split)*100
            print("\r" + f"$progress%.1f" +"%")
          }
        }
      }
    }
  }

  /** Extracts the coverage of a region given a predicate */
  def getCoverageOfRegionGivenPred(inBamFile: String, outBedGraph: String, referenceRegions: String, region: String, pred: (SAMRecord) => Boolean, showProgress: Boolean = false) = {
    extractSignalFromRegion(inBamFile, outBedGraph, referenceRegions, region, SignalExtractor(pred = pred, name = "Coverage"), DEFAULT_EXTRACTION_PARAMETERS.copy(showProgress = showProgress))
  }
}