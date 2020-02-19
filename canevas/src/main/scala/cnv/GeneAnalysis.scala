package cnv

import cnv.Basic.getLines
import cnv.BedSignalToolBox._
import cnv.CommonGenomics._
import scala.collection.immutable.SortedMap

/** REMOVE THIS FILE */
object GeneAnalysis {
  
  def mosdepthToSortedMapStartValue(fileName: String) = {
    val it = readBedEntriesFromFile(fileName, intBedEntryFromLine)
    val emptyMap: SortedMap[Position, Int] = SortedMap()(Ordering.by(e => (e.region, e.pos)))
    val noOldEntry: Option[BedEntry[Int]] = None

    it.foldLeft((emptyMap, noOldEntry))(
      (acc, entry) => {
        // extract
        acc match {
          case (sm, oldEntry) => {
            val pos = Position(entry.span.chr, entry.span.start)
            val updatedSm = sm + (pos -> entry.value)

            // if there is an old entry, also check if disjoint, in order to add a 0 valued entry if needed for completion
            (oldEntry match {
              case Some(oldEntry) if ((oldEntry.span.chr == entry.span.chr) && (oldEntry.span.stop != entry.span.start)) => {
                updatedSm + (Position(oldEntry.span.chr, oldEntry.span.stop) -> 0)
              }
              case _ => updatedSm
            }, Some(entry))
          }
        }
      }
    )._1 // Return the SortedMap
  }

}