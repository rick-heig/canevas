package cnv

import cnv.RegionSamToolBox._
import cnv.SamToolBox._
import cnv.Basic.printWriter
import cnv.Predictors._
import cnv.VcfToolBox._

import htsjdk.samtools._
import htsjdk.samtools.SamPairUtil._

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Canevas {
  // Theses could be computed from mean values e.g., as is done in IGV
  // They definitely should in order to work with different templates/sequencing parameters
  val MIN_INSERT_SIZE = 250
  val MAX_INSERT_SIZE = 750
  val MIN_LENGTH_ON_REF_FOR_CLIP = 222 // arbitrary and should be made dependent on the read length

  val COVERAGE_SIGNAL_NAME = "Coverage.bedgraph.gz"
  val CLIPPED_SIGNAL_NAME = "Clipped.bedgraph.gz"
  val MAPQ0_SIGNAL_NAME = "MAPQ0.bedgraph.gz"
  val INTERCHR_SIGNAL_NAME = "InterChr.bedgraph.gz"
  val DISCORDANT_TANDEM_SIGNAL_NAME = "DiscordantTandemOrientation.bedgraph.gz"
  val MAPPING_ERRORS_SIGNAL_NAME = "MappingErrors.bedgraph.gz"
  val INSERT_SIZE_TOO_SMALL_SIGNAL_NAME = "InsertSizeTooSmall.bedgraph.gz"
  val INSERT_SIZE_TOO_BIG_SIGNAL_NAME = "InsertSizeTooBig.bedgraph.gz"
  val CIGAR_INDELS_SIGNAL_NAME = "CigarIndels.bedgraph.gz"
  val MATE_UNMAPPED_SIGNAL_NAME = "MateUnmapped.bedgraph.gz"
  val FORWARD_INSERT_SIZE_TOO_BIG_SIGNAL_NAME = "ForwardInsertSizeTooBig.bedgraph.gz"

  // This list should be filled based on requirements
  def signalExtractors(MIN_INSERT_SIZE: Int = MIN_INSERT_SIZE, MAX_INSERT_SIZE: Int = MAX_INSERT_SIZE) = List(
      ExtendedSignalExtractor(fileName = COVERAGE_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0, _ => 1, "Coverage")),
      // TODO : Set a threshold for clipped reads (some alignments have many 1-2 soft clipped bases ! therefore it makes this signal quasi the same as coverage)
      //ExtendedSignalExtractor(fileName = "Clipped.bedgraph.gz", signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && rec.getCigar().isClipped(), _ => 1, "Clipped")),
      // Below is a custom clipped signal where the read length on reference is taken into account
      ExtendedSignalExtractor(fileName = CLIPPED_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && rec.getLengthOnReference() < MIN_LENGTH_ON_REF_FOR_CLIP && rec.getCigar().isClipped(), _ => 1, "Clipped")),
      ExtendedSignalExtractor(fileName = MAPQ0_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() == 0, _ => 1, "MAPQ0")),
      ExtendedSignalExtractor(fileName = INTERCHR_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && !rec.getMateUnmappedFlag() && rec.getMateReferenceName() != rec.getReferenceName(), _ => 1, "InterChr")),
      ExtendedSignalExtractor(fileName = DISCORDANT_TANDEM_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && !rec.getMateUnmappedFlag() && (rec.getReferenceName == rec.getMateReferenceName) && (getPairOrientation(rec) != PairOrientation.FR) && (getPairOrientation(rec) != PairOrientation.RF), _ => 1, "DiscordantTandemOrientation")),
      ExtendedSignalExtractor(fileName = MAPPING_ERRORS_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && rec.getAttribute("NM") != null && rec.getAttribute("NM").toString.toInt != 0, rec => rec.getAttribute("NM").toString().toInt, "Mapping Errors without MAPQ0")),
      // This seems to be wrong somehow vvv (too many) - This is because it is a fixed threshold and not based on the actual distribution AND becauses insert size of mate iss negative !
      ExtendedSignalExtractor(fileName = INSERT_SIZE_TOO_SMALL_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && !rec.getMateUnmappedFlag() && Math.abs(rec.getInferredInsertSize()) < MIN_INSERT_SIZE, _ => 1, "Insert size too small")),
      // Warning ! The insert size is negative for the second read !
      ExtendedSignalExtractor(fileName = INSERT_SIZE_TOO_BIG_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && !rec.getMateUnmappedFlag() && (getPairOrientation(rec) == PairOrientation.FR) && Math.abs(rec.getInferredInsertSize()) > MAX_INSERT_SIZE, _ => 1, "Insert size too big")),
      // Cigar has more than two elements, normal read is like 150M, clipped Read may be 140M10S, read with Indel will be 50M50D50M so setting the threshold above 2 will ignore reads that are only clipped
      ExtendedSignalExtractor(fileName = CIGAR_INDELS_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && rec.getCigar().numCigarElements > 2, _ => 1, "Cigar has more than 2 elements")),
      // Unmapped mates
      ExtendedSignalExtractor(fileName = MATE_UNMAPPED_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && rec.getReadUnmappedFlag() == false && rec.getMateUnmappedFlag() == true, _ => 1, "Mate is unmapped")),
      ExtendedSignalExtractor(fileName = FORWARD_INSERT_SIZE_TOO_BIG_SIGNAL_NAME, signalExtractor = SignalExtractor(rec => rec.getMappingQuality() != 0 && !rec.getMateUnmappedFlag() && (getPairOrientation(rec) == PairOrientation.FR) && !rec.getReadNegativeStrandFlag() && Math.abs(rec.getInferredInsertSize()) > MAX_INSERT_SIZE, _ => 1, "Forward Insert size too big")),

  )

  def signalGeneration(inBamFile: String, directory: String, signalExtractors: List[RegionSamToolBox.ExtendedSignalExtractor]) = {
    // Get regions (chromosomes) sorted by their size (biggest first)
    val listOfRegions = getRegions(inBamFile).toList.sortBy(_._2).reverse map {_._1}

    // Generate all signals in parallel
    listOfRegions map {
      region => {
        Future{cnv.RegionSamToolBox.extractSignalsFromRegion(inBamFile, directory, signalExtractors map {e => e.copy(fileName = region + "_" + e.fileName)}, region)}
      }
    } foreach {Await.result(_, Duration.Inf)}
  }

  case class VCFGenerator(fun: (/* inBamFile */ String, /* signalDir */ String, /* region */ String, /* outputDir */ String) => Unit)

  // This is a possible list of first stage VCF generators (only require signals and bamfile, not other VCF files)
  val vcfGenerators = List(
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.callInterChrBreakPoints(signalDirectory, directory, inBamFile, region /*, todo sample */)),
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.predictDeletionsFromInsertSizes(signalDirectory, inBamFile, directory, region)),
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.predictFromRegionsThatHaveNoCoverage(signalDirectory, directory, region)),
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.predictFromHighEditDistance(signalDirectory, directory, region)),
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.predictInversionBreakPoints(signalDirectory, directory, region)),
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.predictInterChrBreakPoints(signalDirectory, directory, region)),
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.predictCopyNumber(signalDirectory, directory, region, 30)),
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.predictFromHardIncreasesInCoverageBridgedByClippedReads(signalDirectory, directory, region)) //,
  )

  // This generates predictors that have been tuned a bit more, at least some more processing is done to filter and determine the type of SV
  def niceVcfGenerators(coverage: Int) = List(
    // Deletions - Nice
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.predictDeletionsFromInsertSizesFine(signalDirectory, inBamFile, directory, region, cnv.ImprovedPredictors.PRED_DEL_INS_SIZE_BIG_FINE_DEFAULT_PARAMS.copy(coverage = coverage))),
    // Duplications - Nice
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.predictCopyNumberNew(inBamFile, signalDirectory, directory, coverage, region)),
    // Inversions - Nice
    VCFGenerator((inBamFile: String, signalDirectory: String, region: String, directory: String) => cnv.ImprovedPredictors.callInversions(inBamFile, signalDirectory, directory, region))
    // Insertions ... TODO, for the moment I mostly have breakpoints (maybe just evaluate breakpoints)
  )

  // This is a possible list of second stage VCF generators (require some first stage signals)
  // TODO

  def vcfGeneration(inBamFile: String, signalDirectory: String, directory: String, vcfGenerators: List[VCFGenerator]) = {
    // Get regions (chromosomes) sorted by their size (biggest first)
    val listOfRegions = getRegions(inBamFile).toList.sortBy(_._2).reverse map {_._1}

    // Generate all VCFs in parallel
    listOfRegions map {
      region => {
        Future{
          vcfGenerators foreach {
            gen => gen.fun(inBamFile, signalDirectory, region, directory)
          }
        }
      }
    } foreach {Await.result(_, Duration.Inf)}
  }

  /** Generate the signals for the whole genome */
  def allSignalGeneration(inBamFile: String, directory: String) = {
    val fragmentStats = getFragmentSizeThresholds(inBamFile)
    val MIN_INSERT_SIZE = fragmentStats.secondPercentileSize.toInt
    val MAX_INSERT_SIZE = fragmentStats.maxPercentileInsertSize.toInt
    println("Min insert size : " + MIN_INSERT_SIZE + " Max insert size : " + MAX_INSERT_SIZE)
    signalGeneration(inBamFile, directory, signalExtractors(MIN_INSERT_SIZE, MAX_INSERT_SIZE))
  }

  /** Generate the signals for the whole chromosome (region) */
  def allSignalGenerationChr(inBamFile: String, directory: String, region: String) = {
    val rlt = getFragmentSizeThresholds(inBamFile)
    val MIN_INSERT_SIZE = rlt.secondPercentileSize.toInt
    val MAX_INSERT_SIZE = rlt.maxPercentileInsertSize.toInt
    println("Min insert size : " + MIN_INSERT_SIZE + " Max insert size : " + MAX_INSERT_SIZE)
    getRegions(inBamFile).get(region) match {
      case Some(value) => extractSignalsFromRegion(inBamFile, directory, signalExtractors(MIN_INSERT_SIZE, MAX_INSERT_SIZE) map {e => e.copy(fileName = region + "_" + e.fileName)}, region)
      case None => {
        println("Region " + region + " is not in regions of bam file. You can check with $ samtools view -H bamfile.bam")
        println("Regions are : " + getRegions(inBamFile).keySet.toList.sorted.mkString(" "))
      }
    }
  }

  /** Generate all first stage VCFs (signals need to be generated) */
  def allFirstStageVCFGeneration(inBamFile: String, signalDirectory: String, outputDirectory: String) = {
    vcfGeneration(inBamFile, signalDirectory, outputDirectory, vcfGenerators)
  }

  /** Generate all nice VCFs (signals need to be generated) */
  def allNiceVCFGeneration(inBamFile: String, signalDirectory: String, outputDirectory: String) = {
    val coverage = 30
    vcfGeneration(inBamFile, signalDirectory, outputDirectory, niceVcfGenerators(coverage))
  }

  def allFirstStageVCFGenerationChr(inBamFile: String, signalDirectory: String, outputDirectory: String, region: String) = {
    getRegions(inBamFile).get(region) match {
      case Some(value) => vcfGenerators map {
        gen => Future{
          gen.fun(inBamFile, signalDirectory, region, outputDirectory)
        }
      } foreach {Await.result(_, Duration.Inf)}
      case None => {
        println("Region " + region + " is not in regions of bam file. You can check with $ samtools view -H bamfile.bam")
        println("Regions are : " + getRegions(inBamFile).keySet.toList.sorted.mkString(" "))
      }
    }
  }

  /** Generates a file with chromosome contig names and their sizes */
  def generateChrSizeFile(inBamFile: String, directory: String) = {
    val writer = printWriter(directory + "/chrom.sizes")

    getRegions(inBamFile) foreach {
      region => writer.println(region._1 + "\t" + region._2)
    }

    writer.close()
  }

  /** Generate the VCFs */
  def generateVcfsFromBamAndExtractedCounts(inBamFile: String, countFolder: String, outFolder: String, listOfRegions: List[String]) = {
    val showProgress = true

    // Example to generate a list of regions
    // val listOfRegions = ((1 to 22).toList map {_.toString}) ++ List("X", "Y") map {"chr" ++ _}
    val effectiveRegions = listOfRegions filter {_.matches("(chr)?([0-9]+|[XxYy])")}

    def vcfFileName(outFolder: String, region: String, predictor: String) = {
      outFolder + "/" + region + "_" + predictor + ".vcf"
    }

    val futures = (effectiveRegions map {
      region => {
        List(
          Future {
            val preds = predictFromHardIncreasesInCoverageBridgedByClippedReads(countFolder + "/" + region + "_Coverage.bedgraph.gz", inBamFile, region)
            val mergedPreds = cnv.CommonGenomics.mergeOverlappingIntervals(preds.iterator)
            writeCalls(mergedPreds.iterator.map(e => cnv.CommonGenomics.Range(region, e.start, e.stop)), vcfFileName(outFolder, region, "IncreasesInCoverageBridgedByClipped"))
          },
          //val preds = predictFromHardDropsInCoverageBridgedByLackOfClippedReadsMinMAPQ0(outFolder + "/" + region + "_count.bedgraph.gz", inBamFile, region)
          //val preds = predictFromRelativeHardDropsInCoverageBridgedByLackOfClippedReadsMinMAPQ0(outFolder + "/" + region + "_count.bedgraph.gz", inBamFile, region)
          //val mergedPreds = cnv.CommonGenomics.mergeOverlappingIntervals(preds.iterator)
          //writeCalls(mergedPreds.iterator.map(e => cnv.CommonGenomics.Range(region, e.start, e.stop)), vcfFileName(outFolder, region, "phbbclip"))
          //writeCalls(mergedPreds.iterator.map(e => cnv.CommonGenomics.Range(region, e.start, e.stop)), vcfFileName(outFolder, region, "phibclip"))
          //writeCalls(mergedPreds.iterator.map(e => cnv.CommonGenomics.Range(region, e.start, e.stop)), vcfFileName(outFolder, region, "predict_relative_hard_drops_bridged_no_clipped_new"))
          Future {
            val preds = predictFromHardDropsInCoverageBridgedByLackOfClippedReadsMinMAPQ0(countFolder + "/" + region + "_Coverage.bedgraph.gz", inBamFile, region)
            val mergedPreds = cnv.CommonGenomics.mergeOverlappingIntervals(preds.iterator)
            writeCalls(mergedPreds.iterator.map(e => cnv.CommonGenomics.Range(region, e.start, e.stop)), vcfFileName(outFolder, region, "DropsInCoverageBridgeByLackOfClipped"))
          }
          //Future {
          //
          //}
        )
      }
    }).flatten
    
    futures foreach {Await.result(_, Duration.Inf)}
    if (showProgress) {println("Generating VCFs done !")}
  }

  /** Simple generator from threshold */
  def generateVcfFromThreshold(inBedGraphFile: String, outVcfFile: String, threshold: Int => Boolean, region: String) = {
    val entries = cnv.BedSignalToolBox.readBedEntriesFromFile(inBedGraphFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    val preds = entries filter {e => threshold(e.value)} map {e => cnv.CommonGenomics.Interval(e.span.start, e.span.stop)}
    val mergedPreds = cnv.CommonGenomics.mergeOverlappingIntervals(preds)
    writeCalls(mergedPreds.iterator.map(e => cnv.CommonGenomics.Range(region, e.start, e.stop)), outVcfFile)
  }

}