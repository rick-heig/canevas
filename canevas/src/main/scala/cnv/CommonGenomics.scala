package cnv
import scala.annotation.tailrec

object CommonGenomics {
  // A region is a chromosome or scaffold (contig)
  case class Position(region: String, pos: Int)
  // TODO replace Position and Range by their Genomic equivalents for clarity
  case class GenomicPosition(region: String, pos: Int)
  case class Range(region: String, start: Int, stop: Int)
  case class GenomicWindow(region: String, start: Int, stop: Int)

  def stringFromGenomicWindow(gw: GenomicWindow) = {
    gw match {
      case GenomicWindow(region, start, stop) => region + ":" + start + "-" + stop
    }
  }

  /** Scales a genomic window by a factor */
  def scaleGenomicWindow(gw: GenomicWindow, factor: Double) = {
    val length = gw.stop - gw.start
    val newLength = length * factor
    val start = gw.start - ((newLength.toInt - length) / 2)
    val stop = gw.stop + ((newLength.toInt - length) / 2)

    val effectiveStart = if (start < 1) {1} else {start}

    GenomicWindow(gw.region, effectiveStart, stop)
  }

  case class IntValueOfRange(range: Range, value: Int)

  case class Interval(start: Int, stop: Int)

  /** This supposes the interval collections are ordered and merged (no overlaps or disjoint contiguous intervals) */
  // TODO : Make private
  def intersectWithOutMergingAndSorting(intervals1: Iterator[Interval], intervals2: Iterator[Interval]) = {
    // Accumulator for recursive inner function
    case class Accumulator(startList: List[Int], stopList: List[Int], coverage: Int, pendingInterval1: (Option[Int], Option[Int]), pendingInterval2: (Option[Int], Option[Int]))

    @tailrec
    def intersect(acc: Accumulator, intervals1: Iterator[Interval], intervals2: Iterator[Interval]): List[Interval] = {
      acc match {
        // Extractor pattern
        case Accumulator(startList, stopList, coverage, pendingInterval1, pendingInterval2) => {
          // Update intervals if needed
          val interval1 = pendingInterval1 match {
            case (None, None) => intervals1.nextOption() match {
              case Some(value) => (Some(value.start), Some(value.stop))
              case None => (None, None)
            }
            case i => i
          }
          val interval2 = pendingInterval2 match {
            case (None, None) => intervals2.nextOption() match {
              case Some(value) => (Some(value.start), Some(value.stop))
              case None => (None, None)
            }
            case i => i
          }

          // Detect overlaps
          (interval1, interval2) match {
            case ((None, None), (None, None)) => {
              // Termination (nothing more to do)
              (startList.reverse) zip (stopList.reverse) map {e => Interval(e._1, e._2)}
            }
            case ((start1, stop1), (start2, stop2)) => {
              // Find the earliest event (lowest starting point then lowest stopping point, there is at least one point because of previous pattern match)
              val event = ((List(start1, start2, stop1, stop2).zipWithIndex.filter(_._1.isDefined)) map {e => (e._1.get, e._2)}).min._2

              // If coverage is 1 and we have a starting point on the position, it means we overlap (intersect)
              // If coverage is 2 and we have a stopping point on the position it means we stop to overlap (intersect)
              event match {
                // Start 1
                case 0 => intersect(Accumulator(if (coverage == 1) start1.get :: startList else startList, stopList, coverage + 1, (None, stop1), (start2, stop2)), intervals1, intervals2)
                // Start 2
                case 1 => intersect(Accumulator(if (coverage == 1) start2.get :: startList else startList, stopList, coverage + 1, (start1, stop1), (None, stop2)), intervals1, intervals2)
                // Stop 1
                case 2 => intersect(Accumulator(startList, if (coverage == 2) stop1.get :: stopList else stopList, coverage - 1, (start1, None), (start2, stop2)), intervals1, intervals2)
                // Stop 2 
                case _ => intersect(Accumulator(startList, if (coverage == 2) stop2.get :: stopList else stopList, coverage - 1, (start1, stop1), (start2, None)), intervals1, intervals2)
              }
            }
          }
        }
      }
    }

    // Merge the two collections separately first so that they have a coverage of 0 or 1 at most, then intersections are discovered by a total coverage of 2 (one from each collection)
    intersect(Accumulator(List(), List(), 0, (None, None), (None, None)), intervals1, intervals2)
  }

  /** Intersections of two interval collections */
  def intersect(intervals1: Iterator[Interval], intervals2: Iterator[Interval]) = {
    intersectWithOutMergingAndSorting(mergeOverlappingIntervals(intervals1).iterator, mergeOverlappingIntervals(intervals2).iterator)
  }

  /** Merges overlapping or contiguous intervals (TODO: Check how the ranges are defined, if [start,stop] or [start, stop[ i.e., stop not included) */
  def mergeOverlappingIntervals(it: Iterator[Interval]) = {
    // Extract starting and stopping positions in two lists from an interator of intervals
    val (startingPoints, stoppingPoints) = it.foldLeft((List[Int](), List[Int]()))((acc, interval) => {(interval.start :: acc._1, interval.stop :: acc._2)})
    // Here we sort them because the intervals could have been in any order in the initial collection
    val startIt = startingPoints.sorted.iterator
    val stopIt = stoppingPoints.sorted.iterator

    // Private internal accumulator
    case class Accumulator(startList: List[Int], stopList: List[Int], coverage: Int, pendingStart: Option[Int], pendingStop: Option[Int])

    // Recursive function (tailrec, i.e., linear) to overlap
    @tailrec
    def mergeOverlappingIntervals(acc: Accumulator, startIt: Iterator[Int], stopIt: Iterator[Int]): (List[Int], List[Int]) = {
      acc match {
        case Accumulator(startList, stopList, coverage, pendingStart, pendingStop) => {
          // Update start and stop from streams if needed
          val start = if (pendingStart.isEmpty) {
            startIt.nextOption()
          } else {
            pendingStart
          }
          val stop = if (pendingStop.isEmpty) {
            stopIt.nextOption()
          } else {
            pendingStop
          }

          // Handle every case
          (start, stop) match {
            case (Some(start), Some(stop)) if (start <= stop) => {
              // If we start a new interval
              val newStartList = if (coverage == 0) {
                start :: startList
              } else { // We are currently overlapping
                startList
              }
              mergeOverlappingIntervals(Accumulator(newStartList, stopList, coverage + 1, None, Some(stop)), startIt, stopIt)
            }
            case (Some(start), Some(stop)) if (start > stop) => {
              // If we close an interval
              val newStopList = if (coverage == 1) {
                stop :: stopList
              } else {
                stopList
              }
              mergeOverlappingIntervals(Accumulator(startList, newStopList, coverage - 1, Some(start), None), startIt, stopIt)
            }
            case (Some(start), None) => {
              // Should not happen ! (this meanse there were more starting points than stopping points)
              (startList, stopList)
            }
            case (None, Some(stop)) => {
              // End (coverage would go from 1 to 0)
              if (coverage == 1) {
                (startList, stop :: stopList)
              } else {
                mergeOverlappingIntervals(Accumulator(startList, stopList, coverage - 1, None, None), startIt, stopIt)
              }
            }
            // End edge case
            case (None, None) => {
              (startList, stopList)
            }
          }
        }
      }
    }

    val (startList, stopList) = mergeOverlappingIntervals(Accumulator(List(), List(), 0, None, None), startIt, stopIt)
    (startList.reverse zip stopList.reverse) map {e => Interval(e._1, e._2)}
  }

  /** Joins Intvalues of ranges when they are contiguouss and have the same value */
  def joinContiguousSameValuedRanges(it: Iterator[IntValueOfRange]) = {

    @tailrec
    def tr(acc: Iterator[IntValueOfRange], currentRange: Option[IntValueOfRange], it: Iterator[IntValueOfRange]): Iterator[IntValueOfRange] = {
      (currentRange, it.nextOption) match {
        case (None, None) => Nil.iterator // Nothing to do
        case (None, Some(irange)) => tr(acc, Some(irange), it) // First element
        case (Some(irange), None) => (acc ++ Iterator.single(irange)) // Termination
        case (Some(irange), Some(nextiRange)) => {
          if (irange.value == nextiRange.value && irange.range.stop == nextiRange.range.start) {
            tr(acc, Some(irange.copy(range = irange.range.copy(stop = nextiRange.range.stop))), it) // Concatenate ranges
          } else {
            tr(acc ++ Iterator.single(irange), Some(nextiRange), it) // Continue
          }
        }
      }
    }

    tr(Nil.iterator, None, it)
  }

  /** Joins intervals if they are close */
  def joinMonotonicIntervalsIfClose(intervalList: List[Interval], maxDistance: Int = 10) = {
    case class Acc(list: List[Interval], currentInterval: Interval)

    val res = intervalList.tail.foldLeft(Acc(List(), intervalList.head))((acc, interval) => {
      if ((interval.start - acc.currentInterval.stop) < maxDistance) {
        // Merge
        acc.copy(currentInterval = acc.currentInterval.copy(stop = interval.stop))
      } else {
        // Add to list
        acc.copy(list = acc.currentInterval :: acc.list, currentInterval = interval)
      }
    })

    // Return the joined (merged) intervals
    (res.currentInterval :: res.list).reverse
  }

  /** Fills missing ranges with Most probable value (check ranges to left and right and fill with the value of the biggest) */
  def fillMissingRangesWithMostProbableValue(it: Iterator[IntValueOfRange]) = {
        @tailrec
    def tr(acc: Iterator[IntValueOfRange], currentRange: Option[IntValueOfRange], it: Iterator[IntValueOfRange]): Iterator[IntValueOfRange] = {
      (currentRange, it.nextOption) match {
        case (None, None) => Nil.iterator // Nothing to do
        case (None, Some(irange)) => tr(acc, Some(irange), it) // First element
        case (Some(irange), None) => (acc ++ Iterator.single(irange)) // Termination
        case (Some(irange), Some(nextiRange)) => {
          if (irange.range.stop != nextiRange.range.start) { // Missing range
            if (irange.value == nextiRange.value) { // If both sides have the same value
              // Fill and join
              tr(acc, Some(irange.copy(range = irange.range.copy(stop = nextiRange.range.stop))), it) // Concatenate ranges
            } else { // Else fill with most probable value (biggest side range)
              if ((irange.range.stop - irange.range.start) >= (nextiRange.range.stop - nextiRange.range.start)) {
                // Fill and join to previous range
                tr(acc ++ Iterator.single(irange.copy(range = irange.range.copy(stop = nextiRange.range.start))), Some(nextiRange), it) // Concatenate with previous range
              } else {
                // Fill and join to next range
                tr(acc ++ Iterator.single(irange), Some(nextiRange.copy(range = nextiRange.range.copy(start = irange.range.stop))), it) // Concatenate with next range
              }
            }
          } else {
            tr(acc ++ Iterator.single(irange), Some(nextiRange), it) // Continue
          }
        }
      }
    }

    tr(Nil.iterator, None, it)
  }
}