package cnv

import cnv.Predictors._
import cnv.Canevas._
import cnv.BedSignalToolBox._
import cnv.VcfToolBox._
import cnv.CommonGenomics._
import cnv.SamToolBox._

import htsjdk.samtools.SAMRecord
import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

import htsjdk.samtools.SamPairUtil._

// TODO : All the thresholds are constants here, they could be moved to parameters

/** These predictors are improved versions of the Predictors object, mainly they use the pregenerated signals */
object ImprovedPredictors {

  def getBedEntriesFromSignal(signalDirectory: String, signalName: String, region: String) = {
    val counts = signalDirectory + "/" + region + "_" + signalName
    val bedEntries = cnv.BedSignalToolBox.readBedEntriesFromFile(counts, cnv.BedSignalToolBox.intBedEntryFromLine)
    bedEntries
  }

  /** Extract regions with high discordance - This gives a ton of regions that should be solved (e.g., by de-novo assembly */
  def predictFromHighEditDistance(signalDirectory: String, outDirectory: String, region: String) = {
    val THRESHOLD = 400 // This should be function of coverage, at least function of average coverage, this is arbitrary for now

    val editDistanceBedCountFile = signalDirectory + "/" + region + "_" + MAPPING_ERRORS_SIGNAL_NAME
    val editDistanceEntries = cnv.BedSignalToolBox.readBedEntriesFromFile(editDistanceBedCountFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    val filteredEntries = editDistanceEntries filter {_.value > THRESHOLD} map {e => Interval(e.span.start, e.span.stop)}
    val mergedEntries = mergeOverlappingIntervals(filteredEntries) map {e => Range(region, e.start, e.stop)}

    cnv.VcfToolBox.writeCalls(mergedEntries.iterator, cnv.VcfToolBox.vcfFileName(outDirectory, region, "edit_distance_too_high"))
  }

  /** Extract from regions with reads that have a cigar of more than 2 elements */
  def predictFromMultipleElementCigar(signalDirectory: String, outDirectory: String, region: String) = {
    val THRESHOLD = 12 // This should be function of coverage, at least function of average coverage, this is arbitrary for now

    val cigarBedCountFile = signalDirectory + "/" + region + "_" + CIGAR_INDELS_SIGNAL_NAME
    val entries = cnv.BedSignalToolBox.readBedEntriesFromFile(cigarBedCountFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    val filteredEntries = entries filter {_.value > THRESHOLD} map {e => Interval(e.span.start, e.span.stop)}

    // These should be merged if close (or do a low pass filter on input signal)
    val mergedEntries = mergeOverlappingIntervals(filteredEntries) map {e => Range(region, e.start, e.stop)}

    cnv.VcfToolBox.writeCalls(mergedEntries.iterator, cnv.VcfToolBox.vcfFileName(outDirectory, region, "cigar_elements_too_high"))
  }

  /** Extract regions that are not convered */
  def predictFromRegionsThatHaveNoCoverage(signalDirectory: String, outDirectory: String, region: String) = {
    val THRESHOLD = 0

    val bedCountFile = signalDirectory + "/" + region + "_" + COVERAGE_SIGNAL_NAME
    val entries = cnv.BedSignalToolBox.readBedEntriesFromFile(bedCountFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    val filteredEntries = entries filter {_.value <= THRESHOLD} map {e => Interval(e.span.start, e.span.stop)}

    val mergedEntries = mergeOverlappingIntervals(filteredEntries) map {e => Range(region, e.start, e.stop)}

    cnv.VcfToolBox.writeCalls(mergedEntries.iterator, cnv.VcfToolBox.vcfFileName(outDirectory, region, "coverage_too_low"))
  }

  /** Predict from drops in clipped reads */
  /** Predict from increases in clipped reads */

  /** This tries to predict deletions from insert sizes, however it also predicts translocations and inversions (this can be checked when reads are in the same direction) */
  def predictDeletionsFromInsertSizes(signalDirectory: String, inBamFile: String, outDirectory: String, region: String) = {
    // Note : The runtime of this predictor can be improved by restricting the insert size too big signal to the leftmost reads in pair

    val THRESHOLD = 2

    // Get Local maximas based on insert sizes too long
    val insertSizesTooBigInBedCountFile = signalDirectory + "/" + region + "_" + INSERT_SIZE_TOO_BIG_SIGNAL_NAME
    val insertSizesTooBigbedEntries = cnv.BedSignalToolBox.readBedEntriesFromFile(insertSizesTooBigInBedCountFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    val localMaximas = insertSizesTooBigbedEntries.sliding(3) filter {_ match { case Seq(e1, e2, e3) => e1.value < e2.value && e2.value > e3.value}} map {_(1)}

    // For each maxima query the local reads (overlapping) and filter them by insert size then filter them by primary (first of pair)
    val it = localMaximas filter {_.value > THRESHOLD} map {
      max => {
        val readsIt = cnv.SamToolBox.getIteratorOnSubRegion(inBamFile, region, max.span.start, max.span.stop)
        val reads = (readsIt filter {r => r.getMappingQuality != 0 && !r.getReadUnmappedFlag && !r.getMateUnmappedFlag && r.getInferredInsertSize() > cnv.Canevas.MAX_INSERT_SIZE && getPairOrientation(r) == PairOrientation.FR}).toList

        reads match {
          case List() => {
            // Empty list means secondary read in pair
            // Do nothing
            Range(region, 0, -1) // Not the most elegant
          }
          case list if list.length < max.value => Range(region, 0, -1)
          case _ => {
            val start = (reads map {_.getAlignmentEnd()}).max
            // Get the mate coordinates of all these reads and generate the endpoint 
            val end = (reads map {_.getMateAlignmentStart()}).min // This will not be correct in cases were the mates are not clustered but dispersed (more complex event)
            
            // Check if overlapping (not done)
            // Generate the "midpoint" of mates
            Range(region, start, end)
          }
        }
      }
    } filter {range => range.start < range.stop} // Remove secondaries or misformed ranges


    cnv.VcfToolBox.writeCalls(it, cnv.VcfToolBox.vcfFileName(outDirectory, region, "predict_from_insert_sizes_too_big"))

    // Get the mate coordinates of all these reads
    // Check if overlapping
    // Generate the "midpoint" of mates

    // Output the call from local maxima to midpoint of mates
  }

  case class PredictDeletionFromInsertSizesFineParameters(coverage: Int = 30, threshold: Int = 3, onlyDel: Boolean = true)
  val PRED_DEL_INS_SIZE_BIG_FINE_DEFAULT_PARAMS = PredictDeletionFromInsertSizesFineParameters()
  def predictDeletionsFromInsertSizesFine(signalDirectory: String, inBamFile: String, outDirectory: String, region: String, parameters: PredictDeletionFromInsertSizesFineParameters = PRED_DEL_INS_SIZE_BIG_FINE_DEFAULT_PARAMS, sample: String = "SAMPLE") = {
    val (coverage, threshold, onlyDel) = PredictDeletionFromInsertSizesFineParameters.unapply(parameters).get
    val fragmentStats = cnv.SamToolBox.getFragmentSizeThresholds(inBamFile)
    val MAX_EVENT_SIZE = 1000000
    val MAX_INSERT_SIZE = fragmentStats.maxPercentileInsertSize.toInt
    val READ_LENGTH = getReadLength(inBamFile).length // Here a check could be added to emit a warning if the read size is not uniform in the bamfile

    val forwardInsertSizesTooBigFile = signalDirectory + "/" + region + "_" + FORWARD_INSERT_SIZE_TOO_BIG_SIGNAL_NAME
    val forwardInsertSizesTooBigbedEntries = cnv.BedSignalToolBox.readBedEntriesFromFile(forwardInsertSizesTooBigFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    // Filter entries so that the remaining are above the threshold
    val filteredIntervals = forwardInsertSizesTooBigbedEntries filter {_.value > threshold} map {e => cnv.CommonGenomics.Interval(e.span.start, e.span.stop)}
    
    // Join close intervals
    val intervalsOfInterest = cnv.CommonGenomics.joinMonotonicIntervalsIfClose(filteredIntervals.toList, 2*READ_LENGTH)

    val results = intervalsOfInterest map {
      interval => {
        val start = interval.start
        val stop = interval.stop

        //println("Interval analysis : " + start + " - " + stop)

        val readsIt = cnv.SamToolBox.getIteratorOnSubRegion(inBamFile, region, start, stop)
        val reads = (readsIt filter {r => r.getMappingQuality != 0 && !r.getReadUnmappedFlag && !r.getMateUnmappedFlag && !r.getReadNegativeStrandFlag && getPairOrientation(r) == PairOrientation.FR && r.getInferredInsertSize > MAX_INSERT_SIZE}).toList

        val eventStart = (reads map {_.getAlignmentEnd()}).max

        (reads map {_.getMateAlignmentStart / (2 * MAX_INSERT_SIZE)})

        // Generate the starting positions of the aligments of the mates
        val mateAlignmentStartPositions = reads map {r => r.getMateAlignmentStart}
        val eventStop = mateAlignmentStartPositions.min
        val templateVcfRecord = VcfEntry(region, eventStart, "MISSING", "N", "<SV>", ".", "None", Map(("END" -> eventStop.toString)), List("GT", "0/1"))

        if ((mateAlignmentStartPositions.max - mateAlignmentStartPositions.min) < (MAX_INSERT_SIZE * 3)) {
          // If the mates are clustered close enough to each other
          val eventStop = mateAlignmentStartPositions.min
          val record = templateVcfRecord

          if (eventStop < eventStart) {
            // Sometimes happens when everything is clustered, check Sim-A 19:33,234,934-33,239,276
            record
          } else if (eventStop - eventStart < MAX_EVENT_SIZE) {
            // Evaluate the coverage between the two ends of the event
            val numReads = (getIteratorOnSubRegionNonOverlapping(inBamFile, region, eventStart, eventStop) filter {rec => !rec.getReadUnmappedFlag() && rec.getMappingQuality() != 0}).length
            val approxAverageCoverage = (numReads * READ_LENGTH) / (eventStop - eventStart + 1)

            approxAverageCoverage match {
              case value if (value < (coverage / 2)) => {
                // "DEL"
                record.copy(alt = "<DEL>", info = record.info ++ Map(("SVTYPE" -> "DEL")))
              }
              case value if (value < (coverage * 3) / 2) => {
                // "LOH"
                //println(s"Event : $eventStart, $eventStop, approxCov : $approxAverageCoverage")
                record.copy(alt = "<DEL>", info = record.info ++ Map(("SVTYPE" -> "LOH")))
              }
              case _ => {
                // "CPX"
                record
              }
            }
          } else {
            // Too big
            //println("too big")
            record
          }
        } else {
          // TODO : Compute distribution and check std dev etc.
          //println(s"Mate distribution fuzzy, mates min $eventStop mate max" + mateAlignmentStartPositions.max)
          //println(mateAlignmentStartPositions)
          // Here we may need to split the analysis
          templateVcfRecord.copy(info = templateVcfRecord.info ++ Map(("IMPRECISE" -> "IMPRECISE")))
        }
      }
    }

    //if (onlyDel) {
      writeVcfEntries(results.iterator filter {_.alt != "<SV>"}, vcfFileName(outDirectory, region, "deletions_fine"), sample)
    //} else {
      writeVcfEntries(results.iterator, vcfFileName(outDirectory, region, "from_insert_sizes_too_big_fine"), sample)
    //}
    
  }

  /** Predict Inversions from pair orientation (crude) */
  def predictInversionBreakPoints(signalDirectory: String, outDirectory: String /*, inBamFile: String */, region: String) = {
    val THRESHOLD = 12

    val orientationCountFile = signalDirectory + "/" + region + "_" + DISCORDANT_TANDEM_SIGNAL_NAME
    val entries = cnv.BedSignalToolBox.readBedEntriesFromFile(orientationCountFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    val filteredEntries = entries filter {_.value >= THRESHOLD} map {e => Interval(e.span.start, e.span.stop)}
    val mergedEntries = mergeOverlappingIntervals(filteredEntries) map {e => Range(region, e.start, e.stop)}

    cnv.VcfToolBox.writeCalls(mergedEntries.iterator, cnv.VcfToolBox.vcfFileName(outDirectory, region, "invBreakPoints"))
  }

  /** Call inversions from pair orientation (fine) */
  def callInversions(inBamFile: String, signalDirectory: String, outDirectory: String, region: String, sample: String = "SAMPLE") = {
    // This will try to give an estimation of the inversions if possible, it may be possible to solve some cases
    val threshold = 3
    val readLength = getReadLength(inBamFile).length // Here a check could be added to emit a warning if the read size is not uniform in the bamfile
    val maxIntervalSizeToCheck = 10000
    val minLinks = 10 // Minimum links between islands

    // First, create breakpoint islands
    val discordantTandemOrientationFile = signalDirectory + "/" + region + "_" + DISCORDANT_TANDEM_SIGNAL_NAME
    val entries = cnv.BedSignalToolBox.readBedEntriesFromFile(discordantTandemOrientationFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    // Filter entries so that the remaining are above the threshold
    val filteredIntervals = entries filter {_.value > threshold} map {e => cnv.CommonGenomics.Interval(e.span.start, e.span.stop)}
    
    // Join close intervals
    val intervalsOfInterest = cnv.CommonGenomics.joinMonotonicIntervalsIfClose(filteredIntervals.toList, 2*readLength) filter {i => i.stop - i.start > readLength}
    val islands = scala.collection.immutable.SortedMap(intervalsOfInterest map {i => (i.start, i.stop)}: _*) // (Start -> Stop) sorted map, allows to get an iterator for a range

    // Second, search for links between islands (links will actually be approx (because mate maybe MAPQ0) double the expected value since we check both ways)
    val vcfCalls = (intervalsOfInterest map {
      case Interval(start, stop) => {
        if ((stop - start) <= maxIntervalSizeToCheck) {
          // Extract reads with FF or RR orientations
          val readsIt = cnv.SamToolBox.getIteratorOnSubRegion(inBamFile, region, start, stop)
          val reads = (readsIt filter {r => r.getMappingQuality != 0 && !r.getReadUnmappedFlag && !r.getMateUnmappedFlag && (getPairOrientation(r) != PairOrientation.FR) && (getPairOrientation(r) != PairOrientation.RF)}).toList
          // Create histogram of island to island relations based on these reads (islands are indexed by their start position, they are non overlapping)
          reads.foldLeft(List(): List[(Int, Int)])((list, read) => {
            islands.maxBefore(read.getMateAlignmentStart + 1) match {
              case Some(value) if (value._2 >= read.getMateAlignmentStart) => (start, value._1) :: list
              case _ => list
            } // Rewrite the relations as first island - second island by position (because they can be swapped and still equivalent)
          } map {case (start1, start2) => (Math.min(start1, start2), Math.max(start1, start2))}).toList
        } else {
          List()
        }
      } // Third, make the calls
    }).flatten.groupBy(i => i).mapValues(v => v.length).toList filter {_._2 > minLinks} filter {case (islands, v) => islands._1 != islands._2} map { // Filter if not enough evidence (links) and if self reference
      case (couple, links) => {
        // They are already sorted at this point (island 1 is before island 2, self references are filtered out above)
        val island1Start = couple._1
        val island2Start = couple._2

        // The event should start and end approx in the middle of the islands
        val eventStart = (island1Start + islands.get(island1Start).get) / 2
        val eventStop = (island2Start + islands.get(island2Start).get) / 2

        // Create the VCF entries
        // TODO : ID, Ref, Phasing etc
        VcfEntry(region, eventStart, "MISSING", "N", "<INV>", ".", "None", Map(("END" -> eventStop.toString), ("SVTYPE" -> "INV"), ("SCORE" -> links.toString)), List("GT", "0/1"))
      }
    }

    writeVcfEntries(vcfCalls.iterator, vcfFileName(outDirectory, region, "inversions_fine"), sample)
  }

  /** Predict Interchromosomal breakpoints (crude) */
  def predictInterChrBreakPoints(signalDirectory: String, outDirectory: String /*, inBamFile: String */, region: String) = {
    val THRESHOLD = 12

    val interChrCountFile = signalDirectory + "/" + region + "_" + INTERCHR_SIGNAL_NAME
    val entries = cnv.BedSignalToolBox.readBedEntriesFromFile(interChrCountFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    val filteredEntries = entries filter {_.value >= THRESHOLD} map {e => Interval(e.span.start, e.span.stop)}

    val mergedEntries = mergeOverlappingIntervals(filteredEntries) map {e => Range(region, e.start, e.stop)}

    cnv.VcfToolBox.writeCalls(mergedEntries.iterator, cnv.VcfToolBox.vcfFileName(outDirectory, region, "interChrBreakPoints"))
  }

  /** Call Interchromosomal breakpoints (fine) */
  def callInterChrBreakPoints(signalDirectory: String, outDirectory: String, inBamFile: String, region: String, sample: String = "NAxxxxx") = {
    // The first part is the same as the above function and could be grouped if needed.
    val THRESHOLD = 12

    val interChrCountFile = signalDirectory + "/" + region + "_" + INTERCHR_SIGNAL_NAME
    val entries = cnv.BedSignalToolBox.readBedEntriesFromFile(interChrCountFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    val filteredEntries = entries filter {_.value >= THRESHOLD} map {e => Interval(e.span.start, e.span.stop)}

    val mergedEntries = mergeOverlappingIntervals(filteredEntries) map {e => Range(region, e.start, e.stop)}

    val vcfEntries = mergedEntries.zipWithIndex map {
      case (e, index) => {
        // Query the bam file
        val InterChrReads = (cnv.SamToolBox.getIteratorOnSubRegion(inBamFile, region, e.start, e.stop) filter {
          // Filter entries so that both the read and its mate are mapped and have them be mapped on different references (contigs)
          rec => !rec.getReadUnmappedFlag() && !rec.getMateUnmappedFlag() && rec.getReferenceName() != rec.getMateReferenceName()
        }).toList

        // TODO : Check if mates are not MAPQ0

        // Create the histogram of interchromosomal relations
        val chrHist = InterChrReads.foldLeft(Map(): Map[String, Int])((map, rec) => {
          val chr = rec.getMateReferenceName() match {
            case region if (region.matches("[0-9]+|[XxYy]")) => "chr" + region
            case region => region
          }
          map.get(chr) match {
            case Some(value) => (map - chr) + (chr -> (value + 1))
            case None => map + (chr -> 1)
          }
        })

        // Create a VCF entry based on this information
        val pos  = InterChrReads.minBy(_.getAlignmentStart()).getAlignmentStart()
        val end  = InterChrReads.maxBy(_.getAlignmentEnd()).getAlignmentEnd()
        val id   = region + ":" + "BND" + ":" + index
        val ref  = "N" // TODO
        val alt  = "<BND>"
        val qual = "."
        val filter = "None"
        val info = Map(
          ("SVTYPE" -> "BND"),
          ("END" -> end.toString),
        ) ++ chrHist.mapValues(_.toString)
        val opt  = List("GT", "0/1") // TODO

        cnv.VcfToolBox.VcfEntry(region, pos, id, ref, alt, qual, filter, info, opt)
      }
    }

    writeVcfEntries(vcfEntries.iterator, vcfFileName(outDirectory, region, "inter_chromosomal_hist"), sample)
  }

  def estimateAverageCoverageOnRegion(signalDirectory: String, region: String) = {
    case class Acc(sum: Long, covered: Long)
    val res = getBedEntriesFromSignal(signalDirectory, COVERAGE_SIGNAL_NAME, region).foldLeft(Acc(0,0))((acc, entry) => {
      val coverage = entry.value
      val length = entry.span.stop - entry.span.start
      if (coverage > 0) {
        Acc(acc.sum + coverage * length, acc.covered + length)
      } else {
        acc
      }
    })

    res.sum / res.covered
  }

  /** Predict copy number */
  def predictCopyNumber(signalDirectory: String, outDirectory: String /*, inBamFile: String */, region: String, coverage: Int) = {
    val entries = getBedEntriesFromSignal(signalDirectory, COVERAGE_SIGNAL_NAME, region)

    val binnedEntries = entries map {
      e => IntValueOfRange(Range(region, e.span.start, e.span.stop), e.value)
    } map {
      i => i.copy(value = Math.round((i.value.toDouble / (coverage / 1.0)).toInt)) // Binned into copy number
    }

    val FILTER_THREHSOLD = 50 // Arbitrary
    val cnvIt = fillMissingRangesWithMostProbableValue(joinContiguousSameValuedRanges(binnedEntries) filter {irange => (irange.range.stop - irange.range.start) > FILTER_THREHSOLD})

    {
      val dupsIt = cnvIt filter {_.value > 3}

      val vcfEntries = dupsIt.zipWithIndex map {
        case (d, index) => {
          // Create a VCF entry based on this information
          val pos  = d.range.start
          val end  = d.range.stop
          val id   = region + ":" + "CNV" + ":" + index
          val ref  = "N" // TODO
          val alt  = "<CNV>"
          val qual = "."
          val filter = "None"
          val info = Map(
            ("SVTYPE" -> "CNV"),
            ("END" -> end.toString),
            ("CN" -> d.value.toString)
          )
          val opt  = List("GT", "0/1") // TODO

          cnv.VcfToolBox.VcfEntry(region, pos, id, ref, alt, qual, filter, info, opt)
        }
      }

      writeVcfEntries(vcfEntries.iterator, vcfFileName(outDirectory, region, "test_cnv"), "SAMPLE")
    }
    //cnv.BedSignalToolBox.bedGraphFromEntries(cnvIt map {i => cnv.BedSignalToolBox.BedEntry(cnv.BedSignalToolBox.BedSpan(region, i.range.start, i.range.stop), i.value)}, "testFileName.bedgraph.gz", "copy number")
  }

  /** WIP */ // TODO Remove coverage
  def predictCopyNumberNew(inBamFile: String, signalDirectory: String, outDirectory: String, coverage: Int, region: String) = {
    val readLength = getReadLength(inBamFile).length // Here a check could be added to emit a warning if the read size is not uniform in the bamfile
    val threshold = 6
    val minDistance = 500 // TODO
    val saveSecondOrderMorphologySignal = true

    val coverage = estimateAverageCoverageOnRegion(signalDirectory, region)

    val bedCountFile = signalDirectory + "/" + region + "_" + COVERAGE_SIGNAL_NAME
    val entries = cnv.BedSignalToolBox.readBedEntriesFromFile(bedCountFile, cnv.BedSignalToolBox.intBedEntryFromLine)

    case class Accumulator(it: Iterator[BedEntry[Int]] = Nil.iterator, lastEntry: Option[BedEntry[Int]] = None, sum: Int = 0, start: Int = 0)

    // Morphological non-linear transformation
    // Mathematical morphology is the analysis of signals in terms of shape. Much is based on simple Boolean functions that are used to produce filters for signals (often non linear)

    // This is actually a second order signal (as are the edges)
    // This basically does signal segmentation based on edge detection and zero coverage then applies a low pass filter (average) on the segments
    val cnvIt = entries.foldLeft(Accumulator())((acc, entry) => {
      acc match {
        case Accumulator(it, None, _, _) => {
          // First entry
          Accumulator(it, Some(entry), entry.value * BedEntry.length(entry), entry.span.start)
        }
        case Accumulator(it, Some(lastEntry), sum, start) => {
          val end = lastEntry.span.stop

          // If there is a breakpoint (edge) // TODO : Drop edges too close to each other
          if ((lastEntry.span.stop != entry.span.start) || (entry.value == 0) || (lastEntry.value == 0) || (Math.abs(entry.value - lastEntry.value) > threshold)) {
            Accumulator(it ++ Iterator.single(BedEntry(BedSpan(region, start, end), sum / /* div by zero on simA chr 6 TODO check */Math.max(1, (end - start)))), Some(entry), entry.value * BedEntry.length(entry), entry.span.start)
          } else {
            Accumulator(it, Some(entry), sum + entry.value * BedEntry.length(entry), start)
          }
        }
      }
    }).it

    val it = if (saveSecondOrderMorphologySignal) {
      val (fileIt, it) = cnvIt.duplicate // Duplicate the iterator, hope this works as intended
      cnv.BedSignalToolBox.bedGraphFromEntries(fileIt, outDirectory + "/" + region + "_testFileName_copy_number.bedgraph.gz", "copy number")
      it
    } else {
      cnvIt
    }

    // Above we have generated the segmented averaged signal

    // Below we interpret it for duplications, since everything is merged below if duplications from different lengths are present on each allele, only the combination of both will be detected for the moment

    val highCopyNumber = (it filter {_.value > coverage + (coverage * 0.5).toInt}).toList
    val highCopyNumberAsIntervals = highCopyNumber map {e => cnv.CommonGenomics.Interval(e.span.start, e.span.stop)}
    // Join intervals that are close and remove those that are small than a read length
    val vcfEntries = joinMonotonicIntervalsIfClose(highCopyNumberAsIntervals, 2*readLength) filter {i => i.stop - i.start > 4*readLength} map {
      case Interval(start, stop) => {
        // Generate the VCF calls
        VcfEntry(region, start, "MISSING", "N", "<DUP>", ".", "None", Map(("END" -> stop.toString), ("SVTYPE" -> "DUP")), List("GT", "0/1")) // TODO Copy number and phasing
      }
    }
    
    writeVcfEntries(vcfEntries.iterator, vcfFileName(outDirectory, region, "test_cnv_fine"), "SAMPLE")
  }

  /** Also work well */
  def predictFromHardIncreasesInCoverageBridgedByClippedReads(signalDirectory: String, outDirectory: String, region: String, useFutures: Boolean = false) = {
    import scala.collection.immutable.SortedMap
    // TODO : Factor in with the predictors above, they share a lot of code
    val minClippedThreshold = 0
    val jumpThreshold = 9 // As parameter and is function of the coverage

    val inBedCountFile = signalDirectory + "/" + region + "_" + COVERAGE_SIGNAL_NAME
    val inClippedCountFile = signalDirectory + "/" + region + "_" + CLIPPED_SIGNAL_NAME

    def genJumps = {cnv.BedSignalToolBox.generateJumpsFromFile(inBedCountFile) partition {e => e.value >= 0}}
    def genBridges = {
      val entries = cnv.BedSignalToolBox.readBedEntriesFromFile(inClippedCountFile, cnv.BedSignalToolBox.intBedEntryFromLine)
      joinContiguousSameValuedRanges(entries filter {_.value > 0} map {e => IntValueOfRange(Range(region, e.span.start, e.span.stop), e.value)}).iterator
    }

    val (jumps, falls, it) = if (useFutures) {
      // Futures for local concurrent execution
      val future1 = Future{genJumps}
      val future2 = Future{genBridges}

      // Generate falls and jumps
      val (jumps, falls) = Await.result(future1, Duration.Inf)
      // Generate clipped read counts
      val it = Await.result(future2, Duration.Inf)
      (jumps, falls, it)
    } else {
      val (jumps, falls) = genJumps
      val it = genBridges
      (jumps, falls, it)
    }

    val fallMap = SortedMap((falls map {e => (e.span.start, e.value)}): _*)
    val jumpMap = SortedMap((jumps map {e => (e.span.start, e.value)}): _*)

    val clippedIntervalsList = joinIntervals(it filter {e => e.value > minClippedThreshold} map {e => Interval(e.range.start, e.range.stop)}) map {e => (e.start, e.stop)}
    val clippedIntervals = scala.collection.immutable.SortedMap(clippedIntervalsList: _*)

    // Predict SVs
    val predictions = intervalsStartedBridgedEnded(jumpMap, fallMap, clippedIntervals)
    // Merge overlapping and contiguous calls
    val callIt = mergeOverlappingIntervals(predictions.iterator).iterator

    writeCalls(callIt map {i => cnv.CommonGenomics.Range(region, i.start, i.stop)}, outDirectory + "/" + region + "_testPred.vcf")
  }
}