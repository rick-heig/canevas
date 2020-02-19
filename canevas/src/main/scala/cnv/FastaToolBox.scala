package cnv

import java.io._
import java.util._;

import org.biojava._
import org.biojava.nbio.core.sequence.io.FastaReader
import org.biojava.nbio.core.sequence.io.FastaReaderHelper

import scala.collection.JavaConverters._

/** REMOVE THIS FILE */
object FastaToolBox {

  def test() = {
    val file = new File("./test.fasta")
    
    mapAsScalaMap(FastaReaderHelper.readFastaDNASequence(file)) foreach {
      entry =>
      entry match {
        case (s, dna) => {
          println("Sequence description line : " + s)
          println("Sequence : " + dna)
        }
      }
    }
  }

  def readDnaFasta(fileName: String) = {
    val file = new File(fileName)

    mapAsScalaMap(FastaReaderHelper.readFastaDNASequence(file))
  }

  def readHugeDnaFasta(fileName: String) = {
    val file = new File(fileName)

    mapAsScalaMap(FastaReaderHelper.readFastaDNASequence(file, true)) // Lazy sequenced load
    // TODO : Check if this is still lazy after the mapAsScalaMap
  }
}