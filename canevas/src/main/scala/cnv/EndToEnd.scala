package cnv

import cnv.BedSignalToolBox._
import cnv.SamToolBox._
import cnv.Predictors._
import cnv.VcfToolBox._

import htsjdk.samtools.SAMRecord
import htsjdk.samtools.SamPairUtil._

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

import java.io.File
import cnv.CommonGenomics.IntValueOfRange
import cnv.CommonGenomics.Range
import org.apache.logging.log4j.core.jackson.ListOfMapEntrySerializer

// TODO : Write unit test that computes the different files and checksums them (checksum example : https://www.michaelpollmeier.com/2018/12/10/checksum-files-scala)

object EndToEnd {

  /** Generates coverage of a given predicate as a SortedMap */
  def generateCoverageByPredicate(inBamFile: String, region: String, pred: (SAMRecord) => Boolean) = {
    // TODO : fill empty ranges
    // TODO : change ranges to new data type
    val sm = createSortedMapFromAlignedReadsGivenPredicate(inBamFile, pred, region)
    val it = getRangeIteratorOfIncrementalMap(sm)

    it
  }

  def sumOfAlignedReadsByPredicateAsSmIt(in: String, pred: (SAMRecord) => Boolean) = {
    val sm = createSortedMapFromAlignedReadsGivenPredicate(in, pred)
    val it = getRangeIteratorOfIncrementalMap(sm)

    it
  }

  def asBedEntries(it: Iterator[IntValueOfRange]) = {
    (it map {e => BedEntry(BedSpan(e.range.region, e.range.start-1, e.range.stop-1), e.value)}).iterator
  }

  def sumOfAlignedReadsByPredicateAsBedGraph(in: String, out: String, pred: (SAMRecord) => Boolean, trackName: String = "track", description: String = "no description") = {
    val it = sumOfAlignedReadsByPredicateAsSmIt(in, pred)
    bedGraphFromEntries(asBedEntries(it), out, trackName, description)
  }

  /** Does what mosdepth did in fast per base mode, however this takes ages */
  def generateCoverage(inBamFile: String, outBedgraphFile: String, region: String) = {
    /* TODO : Write a custom function instead of relying on the functions below, there is
      a call to record.getReferenceName() in the map crate which is horrible for performance
      since this requires to generate a new string each time, since we do already filter
      by region (reference name) this is superflous and unnecessary */
    val it = generateCoverageByPredicate(inBamFile, region, (_) => true)
    bedGraphFromEntries(asBedEntries(it), outBedgraphFile, "coverage", "plain coverage")
  }

  def jumps(in: String, out: String, threshold: Int) = {
    extractJumpsToFile(in, out, threshold)
  }

  /** Writes regions with coverage below threhold in bed format from bed count input (mosdepth)
   * 
   * @param in the input bed file with the counts as generate from mosdepth per base
   * @param out the output file name, a bed file
   * @param threshold the threshold below which the region is output
   */
  def coverageBelow(in: String, out: String, threshold: Int) = {
    bedGraphFromEntries(findLowCoverage(in, threshold) map {e => BedEntry(e.span, 1)}, out, "coverage below " + threshold)
  }

  /** Gives the number of clipped reads aligned to a position */
  def clippedReads(in: String, out: String) = {
    val pred = {(rec: SAMRecord) => rec.getCigar().isClipped()}
    sumOfAlignedReadsByPredicateAsBedGraph(in, out, pred, "Clipped")
  }

  /** Delta of the above for delta x of 1 */
  def derivativeClippedRead(in: String, out: String) = {
    val pred = {(rec: SAMRecord) => rec.getCigar().isClipped()}
    val it = sumOfAlignedReadsByPredicateAsSmIt(in, pred)
    val itd = (it sliding 2) map {
      e => {
        e match {
          case Seq(e1, e2) => {
            IntValueOfRange(Range(e2.range.region, e2.range.start, e2.range.start), e2.value - e1.value)
          }
        }
      }
    }
    bedGraphFromEntries(asBedEntries(itd), out, "derivative clip")
  }

  /** Generates a bedgraph file which will be 1 if the clipped reads coverage is above the ratio times coverage, else 0 */
  def clippedReadsAboveRatioOfCoverage(in: String, out: String, coverage: Int, ratio: Double) = {
    val pred = {(rec: SAMRecord) => rec.getCigar().isClipped()}
    val it = sumOfAlignedReadsByPredicateAsSmIt(in, pred)
    val itf = it map {e => IntValueOfRange(e.range, if (e.value > coverage * ratio) {1} else {0})}
    bedGraphFromEntries(itf.map(e => BedEntry(BedSpan(e.range.region, e.range.start-1, e.range.stop-1), e.value)).iterator, out, "Clipped reads above ratio")    
  }

  def mapq0(in: String, out: String) = {
    val pred = {(rec: SAMRecord) => rec.getMappingQuality() == 0}
    sumOfAlignedReadsByPredicateAsBedGraph(in, out, pred, "MAPQ0")
  }

  def coverage(in: String, out: String) = {
    sumOfAlignedReadsByPredicateAsBedGraph(in, out, _ => true, "Coverage")
  }

  def editDistanceAbove(in: String, out: String, threshold: Int) = {
    val pred = editDistanceAboveThreshold(_, threshold)
    sumOfAlignedReadsByPredicateAsBedGraph(in, out, pred, "Sum of (NM > " + threshold + ")")
  }

  def tandemOrientation(in: String, out: String) = {
    // Note, it would be nice to also check the mate mapping quality
    val pred = {(rec: SAMRecord) => !rec.getReadUnmappedFlag() && !rec.getMateUnmappedFlag() && (getPairOrientation(rec) != PairOrientation.FR)}
    sumOfAlignedReadsByPredicateAsBedGraph(in, out, pred, "Tandem Orientation")
  }

  def tandemOrientationNoMAPQ0(in: String, out: String) = {
    val pred = {(rec: SAMRecord) => !rec.getReadUnmappedFlag() && (rec.getMappingQuality() != 0) && !rec.getMateUnmappedFlag() && (getPairOrientation(rec) != PairOrientation.FR)}
    sumOfAlignedReadsByPredicateAsBedGraph(in, out, pred, "Tandem Orientation w/o MAPQ0")
  }

  def generateSignals(inBam: String, inCount: String, outFolder: String) = {
    val jumpTreshold = 7
    val covThreshold = 7
    val editThreshold = 10
    val coverage = 30
    val ratio = 2.0/3.0

    val directory = new File(outFolder)
    if (!directory.exists()) {
      directory.mkdir()
    }

    val futures = List(
      Future{jumps(inCount, outFolder + "/jumps.bed", jumpTreshold)},
      Future{coverageBelow(inCount, outFolder + "/coverage_below.bedgraph", covThreshold)},
      Future{clippedReads(inBam, outFolder + "/clipped.bedgraph")},
      Future{clippedReadsAboveRatioOfCoverage(inBam, outFolder + "/clippedAboveThr.bedgraph", coverage, ratio)},
      Future{mapq0(inBam, outFolder + "/mapq0.bedgraph")},
      Future{editDistanceAbove(inBam, outFolder + "/sum_of_nm_above_th.bedgraph", editThreshold)}
    )

    println("Generating files in " + outFolder + " ...")
    futures foreach {Await.result(_, Duration.Inf)}
    println("Done !")
  }

  /** Sorts a list of regions by size (descending) given a file of region sizes */
  def sortRegionsBySize(listOfRegions: List[String], inBamFile: String) = {
    (cnv.RegionSamToolBox.getSizesOfRegions(inBamFile) filter {rs => listOfRegions.contains(rs.region)} sortBy {_.size} map {_.region}).reverse
  }

  def generateVcfsFromBamAndExtractedCounts(inBamFile: String, outFolder: String, listOfRegions: List[String]) = {
    val showProgress = true

    // Example to generate a list of regions
    // val listOfRegions = ((1 to 22).toList map {_.toString}) ++ List("X", "Y") map {"chr" ++ _}
    val effectiveRegions = listOfRegions filter {_.startsWith("chr")}

    def vcfFileName(outFolder: String, region: String, predictor: String) = {
      outFolder + "/" + region + "_" + predictor + ".vcf"
    }

    val futures = (effectiveRegions map {
      region => {
        Future {
          //val preds = predictFromHardIncreasesInCoverageBridgedByClippedReads(outFolder + "/" + region + "_count.bedgraph.gz", inBamFile, region)
          //val preds = predictFromHardDropsInCoverageBridgedByLackOfClippedReadsMinMAPQ0(outFolder + "/" + region + "_count.bedgraph.gz", inBamFile, region)
          val preds = predictFromRelativeHardDropsInCoverageBridgedByLackOfClippedReadsMinMAPQ0(outFolder + "/" + region + "_count.bedgraph.gz", inBamFile, region)
          val mergedPreds = cnv.CommonGenomics.mergeOverlappingIntervals(preds.iterator)
          //writeCalls(mergedPreds.iterator.map(e => cnv.CommonGenomics.Range(region, e.start, e.stop)), vcfFileName(outFolder, region, "phbbclip"))
          //writeCalls(mergedPreds.iterator.map(e => cnv.CommonGenomics.Range(region, e.start, e.stop)), vcfFileName(outFolder, region, "phibclip"))
          writeCalls(mergedPreds.iterator.map(e => cnv.CommonGenomics.Range(region, e.start, e.stop)), vcfFileName(outFolder, region, "predict_relative_hard_drops_bridged_no_clipped_new"))
        }
      }
    })
    //val moreFutures = (effectiveRegions map {})
    futures foreach {Await.result(_, Duration.Inf)}
    if (showProgress) {println("Generating VCFs done !")}
  }

  def generateCounts(inBamFile: String, outFolder: String, regionSizesFile: String, listOfRegions: List[String]) = {
    val showProgress = true
    // This is sorted by size for more optimal execution (biggest first)
    val effectiveRegions = sortRegionsBySize(listOfRegions, inBamFile)

    val directory = new File(outFolder)
    if (!directory.exists()) {
      directory.mkdir()
    }

    // Generate the coverage (takes ages for chrM for some reason)
    val futures = (effectiveRegions map {
      region => {
        Future {
          val pred = (rec: SAMRecord) => {rec.getMappingQuality() != 0}
          cnv.RegionSamToolBox.getCoverageOfRegionGivenPred(inBamFile, outFolder + "/" + region + "_count.bedgraph.gz", regionSizesFile, region, pred, false)
          val message = region + " done !"
          if (showProgress) {println(message)}
          message // So that the Future has a message
        }
      }
    })

    futures foreach {Await.result(_, Duration.Inf)}
    if (showProgress) {println("Coverage gneration done !")}
  }
}