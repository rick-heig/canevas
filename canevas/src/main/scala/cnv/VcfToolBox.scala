package cnv

import cnv.Basic.getLines
import cnv.Basic.printWriter

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter

import scala.util.Try
import cnv.BedSignalToolBox.BedEntry
import cnv.CommonGenomics.Position
import cnv.CommonGenomics.Range
import java.time.format.DateTimeFormatter
import scala.collection.immutable.Nil

object VcfToolBox {

    /** Constants */
    val ID         = "ID"
    val TYPE       = "TYPE"
    val NUMBER     = "Number"
    val FILE_NAME  = "file_name"
    val TEXT_VALUE = "text_value"
    val VALUE      = "value"

    /** Entry case class
     *
     * The VCF File format specifies 8 mandatory fixed columns :
     * 1. #CHROM
     * 2. POS
     * 3. ID
     * 4. REF
     * 5. ALT
     * 6. QUAL
     * 7. FILTER
     * 8. INFO
     *
     * Optional fields are grouped in the "opt" attribute
     * */
    case class VcfEntry(chr: String, pos: Int, id: String, ref: String,
                        alt: String, qual: String, filter: String,
                        info: Map[String, String],
                        opt: List[String])

    /** Helper function to extract extra annotations (INFO field) */
    def extractAnnotations(annotation: String): Map[String, String] = {
        (annotation.split(";") map (_.split("=")) collect {
            case keyValue if (keyValue.length == 1) => (keyValue(0), keyValue(0))
            case keyValue if (keyValue.length  > 1) => (keyValue(0), keyValue(1))
        }).toMap
    }

    /** Extract entry from string line, there are no extra checks here so filter first */
    def extractEntry(line: String) = {
        val split = line.split("\t")

        // This may not be best to do here
        val chr = {
            lazy val chrS = split(0)
            if (chrS.startsWith("chr")) {
                chrS
            } else {
                "chr" + chrS
            }
        }
        val pos = split(1).toInt
        // val start = pos
        val id = split(2)
        val reference = split(3)
        val alternate = split(4)
        val quality = split(5)
        val filter = split(6)
        // val stop = start + reference.length + 1
        val info = extractAnnotations(split(7))

        val opt = split.drop(8).toList

        VcfEntry(chr, pos, id, reference, alternate, quality, filter, info, opt)
    }

    def extractEntryOpt(line: String): Option[VcfEntry] = {
        Try {extractEntry(line)}.toOption
    }

    /** Extract entries from vcf file */
    def extractEntriesFromFile(fileName: String) = {
        getLines(fileName) filterNot {_.startsWith("#")} map extractEntry
    }

    case class EventCoordinates(chr: String, start: Int, stop: Int, optEventLength: Option[Int] = None)

    /** Print coordinates */
    val locale = new java.util.Locale("en", "US")
    val formatter = java.text.NumberFormat.getIntegerInstance(locale)
    def stringFromCoordinates(coord: EventCoordinates) = {
        coord.chr + ":" + formatter.format(coord.start) + "-" + formatter.format(coord.stop) + "\n"
    }

    /** Extract coordinates of events */
    def extractEventCoordinates(entry: VcfEntry) = {
        val start = entry.pos
        val stop = entry.info.get("END") match {
            // If the info is in the annotation (INFO field)
            case Some(v) => v.toInt
            // Otherwise take the info from the reference string
            case None    => start + entry.ref.length
        }
        val optEventLength = entry.info.get("SVLEN") match {
            case Some(v) => {
                v.toIntOption match {
                    case Some(v) => Some(Math.abs(v))
                    case None => None
                }
            }
            case None    => None
        }
        EventCoordinates(entry.chr, start, stop, optEventLength)
    }

    /** Scale coordinates
     *
     *  Warning : This could give stop coordinates outside of the chromosome */
    def scaleCoordinates(coord: EventCoordinates, scalingFactor: Double) = {
        // TODO : length should be changed to SVLEN if the field exists in the annotations
        val length = {
            coord.optEventLength match {
                case Some(len) => len
                case None => coord.stop - coord.start
            }
        }
        val scaledLength = scalingFactor * length
        val start = coord.start - ((scaledLength.toInt - length) / 2)
        val stop  = coord.stop  + ((scaledLength.toInt - length) / 2)

        val effectiveStart = if (start < 1) {1} else {start}

        EventCoordinates(coord.chr, effectiveStart, stop)
    }

    /** Generates a file with positions of all extracted events in VCF file */
    def writePositionFileFromVcfFile(vcfFile: String, outputFile: String) = {
        val file = new File(outputFile)
        val bw = new BufferedWriter(new FileWriter(file))

        ((extractEntriesFromFile(vcfFile) map extractEventCoordinates) map stringFromCoordinates) foreach {
            bw.write(_)
        }
        bw.close()
    }

    /** Filters a VCF file given a predicate */
    def filterVcfFile(vcfFile: String, outputFile: String, pred: (String) => Boolean) = {
        val it = getLines(vcfFile)
        val writer = printWriter(outputFile)

        // Compose a new predicate that keeps comment lines (header etc) since we only want to filter entries
        val predicate = (line: String) => {line.startsWith("#") || pred(line)} // Short circuit

        (it filter {predicate}) foreach {writer.println}

        writer.close()
    }

    /** Re-Identify entries VCF File */
    def reIdVcfFile(vcfFile: String, outputFile: String, idPrefix: String = "") = {
        val it = getLines(vcfFile)
        val writer = printWriter(outputFile)
        val firstIndex = 0

        it.foldLeft(firstIndex)({
            case (index, line) if (line.startsWith("#")) => {
                writer.println(line)
                index
            }
            case (index, line) => {
                // Change the ID
                writer.println(line.split("\t").updated(2, idPrefix + index.toString).mkString("\t"))
                index + 1
            }
        })

        writer.close()
    }

    /** Fill Ref with 'N' */
    def fillRefNVcfFile(vcfFile: String, outputFile: String) = {
        val writer = printWriter(outputFile)

        getLines(vcfFile) foreach {
            case line if (line.startsWith("#")) => writer.println(line)
            case line => writer.println(line.split("\t").updated(3, "N").mkString("\t"))
        }

        writer.close()
    }

    /** Generates a header for VCF files */
    def generateVcfHeader(sample: String, optLines: List[String] = List()) = {
        // This is a temporary header, it is used for testing purposes
        val header =
        ("""##fileformat=VCFv4.1
        |##fileDate=$$TODAY$$
        |##reference=http://hgdownload.cse.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz
        |##phasing=None
        |##REF is not used here, so filled with 'N', check reference at given position
        |""".stripMargin +
        optLines.mkString("\n") +
        """|##FORMAT=<ID=GT,Number=1,Type=String,Description="Genotype">
        |""".stripMargin).replace("$$TODAY$$", DateTimeFormatter.ofPattern("YYYYMMdd").format(java.time.LocalDate.now()))

        header + "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\t" + sample + "\n"
    }

    /** Write calls */
    def writeCalls(it: Iterator[Range], fileName: String, alt: String = "<SV>", sample: String = "NA12878") = {
        val writer = printWriter(fileName)

        // Write header
        writer.print(generateVcfHeader(sample))

        val id = "."
        val ref = "N"
        val qual = "."
        val filter = "None"
        val format = "GT"
        val na = "0/1"

        it foreach {
            range => {
                val info = "TESTING;" + "END=" + range.stop.toString() + ";" + "SVLEN=" + (range.stop - range.start).toString()
                // Chrom Pos ID REF ALT QUAL FILTER INFO FORMAT NA12878
                writer.println(range.region + "\t" + range.start + "\t" + id + "\t" + ref + "\t" + alt + "\t" + qual + "\t" + filter + "\t" + info + "\t" + format + "\t" + na)
            }
        }

        writer.close()
    }

    /** Writes VcfEntries to a file */
    def writeVcfEntries(it: Iterator[VcfEntry], fileName: String, sample: String, optLines: List[String] = List()) = {
        val writer = printWriter(fileName)

        // TODO : Adapt the header
        writer.print(generateVcfHeader(sample, optLines))

        it foreach {
            entry => {
                val info = (entry.info.toList map {
                    case (k,v) if (k == v) => k
                    case (k,v) => k + "=" + v}).mkString(";")
                val opt  = entry.opt match {
                    case Nil => ""
                    case list => "\t" + list.mkString("\t")
                }
                writer.println(entry.chr + "\t" + entry.pos + "\t" + entry.id + "\t" + entry.ref + "\t" + entry.alt + "\t" + entry.qual + "\t" + entry.filter + "\t" + info + opt)
            }
        }

        writer.close()
    }

    /** Add anotation */
    def addAnnotationToLine(line: String) = {
        line.split("\t")
    }

    /** Helper function to create fileName as String */
    def vcfFileName(outFolder: String, region: String, predictor: String) = {
      outFolder + "/" + region + "_" + predictor + ".vcf"
    }
}