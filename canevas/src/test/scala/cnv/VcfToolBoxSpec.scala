package cnv

import org.scalatest._
import VcfToolBox._
import java.io.FileWriter
import java.io.File._
import java.io.File

class VcfToolBoxSpec extends FlatSpec with Matchers {
  "The annotations" should "be extracted correctly" in {
    val expectedMap = Map(("IMPRECISE","IMPRECISE"),
                          ("SVTYPE","INS"),
                          ("END","10743"),
                          ("CIPOS","-50,50"),
                          ("NS","0100000"),
                          ("ZU","ZU"))
    val testString = "IMPRECISE;SVTYPE=INS;END=10743;CIPOS=-50,50;NS=0100000;ZU"
    extractAnnotations(testString) should be (expectedMap)
  }

  "6 entries" should "be extracted correctly from a file" in {
    val file = File.createTempFile("tmp", ".vcf")
    val writer = new FileWriter(file)

    try {
      writer.write(
        """##fileformat=VCFv4.1
          |##fileDate=20150403
          |##reference=http://hgdownload.cse.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz
          |#contig=<ID=chr1,length=249250621,assembly=hg19,md5=65f842b98f4298437d3e80f5979dc53b,species="Homo sapiens",taxonomy=x>
          |##contig=<ID=chr10,length=135534747,assembly=hg19,md5=170d2e58df7cdd1b7b4f1f155b97341e,species="Homo sapiens",taxonomy=x>
          |##contig=<ID=chr11,length=135006516,assembly=hg19,md5=47208c3afca76415cb7651128c55b9a4,species="Homo sapiens",taxonomy=x>
          |##FORMAT=<ID=GT,Number=1,Type=String,Description="Genotype">
          |#CHROM	POS	ID	REF	ALT	QUAL	FILTER	INFO	FORMAT	NA12878
          |chr1	10632	.	G	<INS>	.	lt3	IMPRECISE;SVTYPE=INS;END=10743;CIPOS=-50,50;NS=0100000;ZU	GT	0/1
          |chr1	10851	.	G	<INS>	.	lt3	IMPRECISE;SVTYPE=INS;END=10942;CIPOS=-50,50;NS=1100000;ZU	GT	0/1
          |chr1	66191	.	T	<INS>	.	lt3	IMPRECISE;SVTYPE=INS;END=66192;SVLEN=+261;CIPOS=-10,10;NS=0000100;ZU	GT	0/1
          |chr1	66493	.	A	<INS>	.	lt3	IMPRECISE;SVTYPE=INS;END=66494;SVLEN=+62;CIPOS=-10,10;NS=0000100;ZU	GT	0/1
          |chr1	67811	.	T	<DEL>	.	PASS	IMPRECISE;SVTYPE=DEL;END=68252;SVLEN=-441;CIPOS=-10,10;CIEND=-10,10;NS=1100111;BL;ZU	GT	0/1
          |chr1	149392	.	T	<DEL>	.	lt3	IMPRECISE;SVTYPE=DEL;END=149706;SVLEN=False;CIPOS=-50,50;CIEND=-50,50;NS=0100000;ZU	GT	0/1""".stripMargin)
    }
    finally {
      writer.flush()
      writer.close()
    }

    val entries = extractEntriesFromFile(file.getAbsolutePath())

    entries.length should be (6)
  }

  "correct coordinates" should "be extracted from entry" in {
    val entry = extractEntry("chr1	10632	.	G	<INS>	.	lt3	IMPRECISE;SVTYPE=INS;END=10743;CIPOS=-50,50;NS=0100000;ZU	GT	0/1")
    val expectedCoordinates = EventCoordinates("chr1", 10632, 10743)

    extractEventCoordinates(entry) should be (expectedCoordinates)
  }

  "correct coordinates" should "be extracted from entry without annotation" in {
    val entry = extractEntry("chr1	67811	.	T	<DEL>	.	PASS	IMPRECISE;SVTYPE=DEL	GT	0/1")
    val expectedCoordinates = EventCoordinates("chr1", 67811, 67812)

    extractEventCoordinates(entry) should be (expectedCoordinates)
  }

  /** Scaling is approximate */
  "coordinates" should "scale x0.5" in {
    val baseCoordinates = EventCoordinates("chr1", 100, 105)
    val expectedCoordinates = EventCoordinates("chr1", 101, 104)

    scaleCoordinates(baseCoordinates, 0.5) should be (expectedCoordinates)
  }

  "coordinates" should "scale x1.0" in {
    val baseCoordinates = EventCoordinates("chr1", 100, 105)
    val expectedCoordinates = EventCoordinates("chr1", 100, 105)

    scaleCoordinates(baseCoordinates, 1.0) should be (expectedCoordinates)
  }

  "coordinates" should "scale x3.0" in {
    val baseCoordinates = EventCoordinates("chr1", 100, 105)
    val expectedCoordinates = EventCoordinates("chr1", 95, 110)

    scaleCoordinates(baseCoordinates, 3.0) should be (expectedCoordinates)
  }
}