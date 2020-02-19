package cnv

import cnv.Basic._

import org.scalatest._
import java.io.File
import java.io.FileWriter
import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream
import java.io.OutputStreamWriter

class BasicSpec extends FlatSpec with Matchers {

  /** Test if cnv.Basic.getLines is able to read files */
  "Lines" should "be extracted correctly from file" in {
    val file = File.createTempFile("tmp", ".txt")
    val expectedValue = """Hello
                           |  $
                           |World !""".stripMargin
    val writer = new FileWriter(file)

    writer.write(expectedValue)
    writer.close()

    val testValue = getLines(file.getAbsolutePath()) reduce {_+"\n"+_}

    testValue should be (expectedValue)
  }



  /** Test if cnv.Basic.getLines is able to read gzipped files */
  "Lines" should "be extracted correctly from gzipped file" in {
    val compressedFile = File.createTempFile("tmp", ".gz")
    val expectedValue = """Hello
                           |  $
                           |World !""".stripMargin

    
    val fos = new FileOutputStream(compressedFile)
    val gzos = new GZIPOutputStream(fos)
    val writer = new OutputStreamWriter(gzos)

    writer.write(expectedValue)
    writer.close()

    // Test the getLines, it should work with gzipped files
    val testValue = getLines(compressedFile.getAbsolutePath()) reduce {_+"\n"+_}

    testValue should be (expectedValue)
  }
}