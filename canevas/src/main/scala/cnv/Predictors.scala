package cnv
import scala.collection.immutable.SortedMap
import cnv.CommonGenomics._
import cnv.BedSignalToolBox._
import cnv.SamToolBox._
import cnv.EndToEnd._
import htsjdk.samtools.SAMRecord
import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Predictors {

  def intervalAroundPosition(pos: Int, increment: Int) = {
    Interval(pos - increment, pos + increment)
  }

  def inInterval(i: Int, int: Interval) = {
    (i >= int.start) && (i <= int.stop)
  }

  def doIntersect(i: Interval, j: Interval) = {
    inInterval(i.start, j) || inInterval(i.stop, j) || inInterval(j.start, i) || inInterval(j.stop, i)
  }

  def intersection(i: Interval, j: Interval) = {
    if (doIntersect(i,j)) {
      val l = List(i.start, i.stop, j.start, j.stop).sorted
      Some(Interval(l(1), l(2)))
    } else {
      None
    }
  }

  def intersectingRange(i: Interval, ranges: SortedMap[Int, Int]) = {
    ranges.minAfter(i.start) match {
      case Some(value) if value._1 <= i.stop => Some(Interval(value._1, value._2))
      case _ => { // Added this case for completion, maybe refactor this function, this is a bit dodgy
        ranges.maxBefore(i.start) match {
          case Some(value) if value._2 >= i.start => Some(Interval(i.start + (i.stop - i.start)/2, value._2))
          case _ => None
        }
      }
    }
  }

  def biggestInInterval(jumps: SortedMap[Int, Int], int: Interval) = {
    jumps.rangeImpl(Some(int.start), Some(int.stop + 1)) maxByOption {_._2}
  }

  /** Joins contiguous ordered intervals */
  def joinIntervals(intervals: Iterator[Interval]) = {
    // Example :
    // val il = List(Interval(1,10), Interval(10, 16), Interval(16,17), Interval(18,19), Interval(30,40), Interval(40,42))
    // joinIntervals(il.iterator)
    // List[cnv.CommonGenomics.Interval] = List(Interval(1,17), Interval(18,19), Interval(30,42))

    // Note this function seems to do the same job as mergeOverlapping and Contiguous intervals.

    @tailrec
    def trf(it: Iterator[Interval], acc: List[Interval], workingInterval: Option[Interval]): List[Interval] = {
      it.nextOption() match {
        // Recursion
        case Some(interval) => {
          workingInterval match {
            case Some(wi) => {
              if (wi.stop == interval.start) {
                // join
                trf(it, acc, Some(Interval(wi.start, interval.stop)))
              } else {
                // accumulate
                trf(it, wi :: acc, Some(interval))
              }
            }
            case None => trf(it, acc, Some(interval))
          }
        }
        // Termination
        case None => {
          workingInterval match {
            case Some(wi) => wi :: acc
            case None => acc
          }
        }.reverse
      }
    }

    trf(intervals, List(), None)
  }

  /** Predict from hard drops in coverage */
  def predictFromHardDropsInCoverage(falls: SortedMap[Int, Int], jumps: SortedMap[Int, Int], ranges: SortedMap[Int, Int]) = {
    /*
     * Traverse all drops in coverage above a threshold
     * Unite with a low coverage level in a range around the drops
     * Extend a bit in the direction of the drop
     * Intersect with increase in coverage if not null then call SV
     */

    // Note : val jumpMap = scala.collection.immutable.SortedMap(jumps: _*) where jumps is a list of tuples

    // Parameters, move to function parameters if needed
    val around = 5

    // Map to (pos, interval around) Map to (pos, Optional range) Filter None ranges Map
    val fallRanges = falls map {e => (e._1, intervalAroundPosition(e._1, around))}
    val intersectingRanges = ((fallRanges map {e => (e._1, intersectingRange(e._2, ranges))}) filter {_._2.isDefined}) map {e => (e._1, e._2.get)}
    val expandedIntersectingRanges = intersectingRanges map {e => (e._1, Interval(e._2.start, e._2.stop + around))}
    val fallAndJumps = expandedIntersectingRanges map {e => (e._1, biggestInInterval(jumps, e._2))} filter {_._2.isDefined} map {e => (e._1, e._2.get._1)}

    // TODO : if the region is smaller than say 50 bases it probably is an insertion, this is rather easy to verify with short reads
    // E.g., a 3bp region is indicative of an insertion, especially if count come from non CIGAR coverage

    fallAndJumps
  }

  def predictFromHardDropsInCoverageFromFile(inBedCountFile: String, region: String) = {
    // Parameters, move to functoin parameters if needed
    val lcThreshold = 7

    // Generate low coverage ranges
    val lowCov = (findLowCoverage(inBedCountFile, lcThreshold) filter {e => e.span.chr == region}) map {e => Interval(e.span.start, e.span.stop)}
    val ranges = SortedMap((joinIntervals(lowCov) map {i => (i.start, i.stop)}): _ *)

    // Generate falls and jumps
    val (jumps, falls) = generateJumpsFromFile(inBedCountFile) partition {e => e.value >= 0}
    val fallMap = SortedMap((falls map {e => (e.span.start, e.value)}): _*)
    val jumpMap = SortedMap((jumps map {e => (e.span.start, e.value)}): _*)

    // Predict SVs
    predictFromHardDropsInCoverage(fallMap, jumpMap, ranges)
  }

  /** Very precise predictor but not does not find much */ // over 98% precision, < 3% sensitivity on chr8 with GC short reads
  def predictFromHardIncreasesInCoverageBridgedByClippedReads(falls: SortedMap[Int, Int], jumps: SortedMap[Int, Int], ranges: SortedMap[Int, Int]) = {
    // Parameters. move to function parameters if needed
    val around = 5

    // TODO: refactor both functions and make this more abstract
    val jumpRanges = jumps map {e => (e._1, intervalAroundPosition(e._1, around))}
    val intersectingRanges = ((jumpRanges map {e => (e._1, intersectingRange(e._2, ranges))}) filter {_._2.isDefined}) map {e => (e._1, e._2.get)}
    val expandedIntersectingRanges = intersectingRanges map {e => (e._1, Interval(e._2.start, e._2.stop + around))}
    val intervals = expandedIntersectingRanges map {e => (e._1, biggestInInterval(falls, e._2))} filter {_._2.isDefined} map {e => Interval(e._1, e._2.get._1)}

    intervals
  }

  /** Generates intervals that have start and end events (positions) as well as a bridge event in between */
  def intervalsStartedBridgedEnded(starters: SortedMap[Int, Int], enders: SortedMap[Int, Int], bridges: SortedMap[Int, Int]) = {
    val around = 5

    val startIntervals = starters map {e => (e._1, intervalAroundPosition(e._1, around))}
    val intersectingBridges = (startIntervals map {e => (e._1, intersectingRange(e._2, bridges))}) filter {_._2.isDefined} map {e => (e._1, e._2.get)} // This is not the only way ! The intersection may be bad
    val expandedIntersectingBridges = intersectingBridges map {e => (e._1, Interval(e._2.start, e._2.stop + around))}
    val endedIntervals = expandedIntersectingBridges map {e => (e._1, biggestInInterval(enders, e._2))} filter {_._2.isDefined} map {e => Interval(e._1, e._2.get._1)}

    endedIntervals
  }

  /** A nice predictor */
  @deprecated
  def predictFromHardDropsInCoverageBridgedByLackOfClippedReads(inBedCountFile: String, inBamFile: String, region: String) = {
    val lackOfClippedReadsThreshold = 2

    // Futures for concurrent execution
    val future1 = Future{generateJumpsFromFile(inBedCountFile) partition {e => e.value >= 0}}
    val future2 = Future{
      val pred = {(rec: SAMRecord) => rec.getCigar().isClipped()}
      val sm = createSortedMapFromAlignedReadsGivenPredicate(inBamFile, pred)
      val it = getRangeIteratorOfIncrementalMap(sm)
      it
    }

    // Generate falls and jumps
    val (jumps, falls) = Await.result(future1, Duration.Inf)
    val fallMap = SortedMap((falls map {e => (e.span.start, e.value)}): _*)
    val jumpMap = SortedMap((jumps map {e => (e.span.start, e.value)}): _*)
    
    // Generate clipped read counts
    val it = Await.result(future2, Duration.Inf)

    val notClippedIntervalsList = joinIntervals(it filter {e => e.value < lackOfClippedReadsThreshold} map {e => Interval(e.range.start, e.range.stop)}) map {e => (e.start, e.stop)}
    val notClippedIntervalsMap = SortedMap(notClippedIntervalsList: _*)

    // Predict SVs
    intervalsStartedBridgedEnded(fallMap, jumpMap, notClippedIntervalsMap)
  }

  /** A nicer predictor */
  def predictFromHardDropsInCoverageBridgedByLackOfClippedReadsMinMAPQ0(inBedCountFile: String, inBamFile: String, region: String, useFutures: Boolean = false) = {
    val lackOfClippedReadsThreshold = 2 // TODO set the threshold a bit higher
    val jumpThreshold = 9 // As parameter and is function of the coverage

    // Definitions of how to generate the signals
    def genJumps = {generateJumpsFromFile(inBedCountFile, jumpThreshold) partition {e => e.value >= 0}}
    def genBridges = {
      val pred = {(rec: SAMRecord) => rec.getCigar().isClipped() && (rec.getMappingQuality() != 0)}
      //val sm = createSortedMapFromAlignedReadsGivenPredicate(inBamFile, pred)
      val sm = cnv.RegionSamToolBox.createSortedMapFromReadsGivenPredicate(inBamFile, region, pred)
      val it = getRangeIteratorOfIncrementalMap(sm, region)
      it
    }

    // This is mostly to save time during testing, not to be used in production
    // Parallelism is handled at a higher level for production
    val (jumps, falls, it) = if (useFutures) {
      // Futures for concurrent execution
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

    // Generate starters / stoppers
    val fallMap = SortedMap((falls map {e => (e.span.start, e.value)}): _*)
    val jumpMap = SortedMap((jumps map {e => (e.span.start, e.value)}): _*)
    
    // Generate bridges
    val notClippedIntervalsList = joinIntervals(it filter {e => e.value < lackOfClippedReadsThreshold} map {e => Interval(e.range.start, e.range.stop)})
    val notClippedIntervalsMap = SortedMap((notClippedIntervalsList map {e => (e.start, e.stop)}): _*)

    // Predict SVs
    val predictions = intervalsStartedBridgedEnded(fallMap, jumpMap, notClippedIntervalsMap)
    // Merge overlapping and contiguous calls
    mergeOverlappingIntervals(predictions.iterator)
  }

  /** Also work well */
  def predictFromHardIncreasesInCoverageBridgedByClippedReads(inBedCountFile: String, inBamFile: String, region: String, useFutures: Boolean = false) = {
    // TODO : Factor in with the predictors above, they share a lot of code
    val minClippedThreshold = 0
    val jumpThreshold = 9 // As parameter and is function of the coverage

    def genJumps = {generateJumpsFromFile(inBedCountFile) partition {e => e.value >= 0}}
    def genBridges = {
      val pred = {(rec: SAMRecord) => rec.getCigar().isClipped()}
      //val sm = createSortedMapFromAlignedReadsGivenPredicate(inBamFile, pred)
      // This one should be more effective when running on really big BAMs
      val sm = cnv.RegionSamToolBox.createSortedMapFromReadsGivenPredicate(inBamFile, region, pred)
      val it = getRangeIteratorOfIncrementalMap(sm, region)
      it
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
    mergeOverlappingIntervals(predictions.iterator)
  }

  /** New predictor relative to coverage */
  def predictFromRelativeHardDropsInCoverageBridgedByLackOfClippedReadsMinMAPQ0(inBedCountFile: String, inBamFile: String, region: String, useFutures: Boolean = false) = {
    val lackOfClippedReadsThreshold = 2 // TODO set the threshold a bit higher
    val jumpThreshold = 9
    val jumpThresholdRelative = 0.33 // As parameter and is function of the coverage

    // Definitions of how to generate the signals
    def genJumps = {generateJumpsFromFile(inBedCountFile, jumpThresholdRelative, jumpThreshold) partition {e => e.value >= 0}}
    def genBridges = {
      val pred = {(rec: SAMRecord) => rec.getCigar().isClipped() && (rec.getMappingQuality() != 0)}
      //val sm = createSortedMapFromAlignedReadsGivenPredicate(inBamFile, pred)
      val sm = cnv.RegionSamToolBox.createSortedMapFromReadsGivenPredicate(inBamFile, region, pred)
      val it = getRangeIteratorOfIncrementalMap(sm, region)
      it
    }

    // This is mostly to save time during testing, not to be used in production
    // Parallelism is handled at a higher level for production
    val (jumps, falls, it) = if (useFutures) {
      // Futures for concurrent execution
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

    // Generate starters / stoppers
    val fallMap = SortedMap((falls map {e => (e.span.start, e.value)}): _*)
    val jumpMap = SortedMap((jumps map {e => (e.span.start, e.value)}): _*)
    
    // Generate bridges
    val notClippedIntervalsList = joinIntervals(it filter {e => e.value < lackOfClippedReadsThreshold} map {e => Interval(e.range.start, e.range.stop)})
    val notClippedIntervalsMap = SortedMap((notClippedIntervalsList map {e => (e.start, e.stop)}): _*)

    // Predict SVs
    val predictions = intervalsStartedBridgedEnded(fallMap, jumpMap, notClippedIntervalsMap)
    // Merge overlapping and contiguous calls
    mergeOverlappingIntervals(predictions.iterator)
  }


  // Notes : val (small50Preds, big50Preds) = predsFut2M.partition(int => int.stop - int.start < 50)
}