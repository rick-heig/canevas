package cnv

import cnv.VcfToolBox._
import cnv.SamToolBox._
import cnv.BedSignalToolBox._
import cnv.Canevas._
import cnv.CommonGenomics._
import cnv.Basic.printWriter

import scala.util.Try

import htsjdk.samtools._

/** This is helper code to generate benchmarks, graphs, etc. for my thesis */
object BenchMarkGraph {

  val THRESHOLD = 10
  val POS_BIN_WIDTH = 2000000

  def interChrGraphData(inBamFile: String, vcfFileDirectory: String, outputDirectory: String) = {
    val chrList = getRegions(inBamFile).keySet.toList
    interChrGraphDataFromList(inBamFile, vcfFileDirectory, outputDirectory, chrList)
  }

  def interChrGraphDataSeparate(inBamFile: String, vcfFileDirectory: String, outputDirectory: String) = {
    val chrListList = getRegions(inBamFile).keySet.toList map {chr => List(chr)}

    chrListList foreach {
      chrAsList => interChrGraphDataFromList(inBamFile, vcfFileDirectory, outputDirectory, chrAsList)
    }
  }

  def interChrGraphDataFromList(inBamFile: String, vcfFileDirectory: String, outputDirectory: String, chromosomes: List[String]) = {
    val vcfFiles = chromosomes map {region => vcfFileDirectory + "/" + region + "_" + "inter_chromosomal_hist.vcf"}

    val fromTos = vcfFiles map {file => interChrGraphDataFromVCF(inBamFile, file)}
    val (from, to) = fromTos.reduceLeft((t1, t2) => (t1._1 ++ t2._1, t1._2 ++ t2._2))

    generateFileFromBedEntries(from, outputDirectory + "/" + chromosomes.mkString("_") + "_from.bed")
    generateFileFromBedEntries(to, outputDirectory + "/" + chromosomes.mkString("_") + "_to.bed")
  }

  def interChrGraphDataFromVCF(inBamFile: String, vcfFile: String) = {
    val it = extractEntriesFromFile(vcfFile)

    val interChr = (it map {
      entry => {
        // For each entry extract the reads
        val end = entry.info.get("END") match {
          case Some(value) => value.toInt
          case None => entry.pos + 1
        }
        
        val reads = (getIteratorOnSubRegion(inBamFile, entry.chr.replaceFirst("chr", ""), entry.pos, end) filter {
          // Filter entries so that both the read and its mate are mapped and have them be mapped on different references (contigs)
          rec => !rec.getReadUnmappedFlag() && !rec.getMateUnmappedFlag() && rec.getReferenceName() != rec.getMateReferenceName() && rec.getMappingQuality() != 0
        }).toList

        val groupedReads = reads.groupBy(_.getMateReferenceName())

        val relations = (groupedReads map {
          case (chr, reads) => {
            val start = (entry.pos / POS_BIN_WIDTH) * POS_BIN_WIDTH
            val end = start + POS_BIN_WIDTH

            (reads.groupBy(_.getMateAlignmentStart() / POS_BIN_WIDTH) map {
              case (index, subreads) => {
                val otherStart = index * POS_BIN_WIDTH
                val otherEnd = otherStart + POS_BIN_WIDTH
                (BedEntry(BedSpan(entry.chr, start, end), 1), BedEntry(BedSpan("chr" + chr, otherStart, otherEnd), subreads.length), subreads.length)
              }
            }).toList
          }
        }).toList

        relations.flatten
      }
    }).toList.flatten filter {case (_, _, qty) => qty > THRESHOLD} filter {case (_, to, _) => to.span.chr.matches("chr([0-9]+|[XxYy])")}

    val bedList1 = interChr map {_._1}
    val bedList2 = interChr map {_._2}

    (bedList1, bedList2)
  }

  /** Gives the minimum reciprocal overlap */
  def checkReciprocalOverlap(a: Interval, b: Interval) = {
    if (a.start <= b.start) {
      checkReciprocalOverlapOrdered(a, b)
    } else {
      checkReciprocalOverlapOrdered(b, a)
    }
  }

  /** Gives the overlap ratio of two intervals, must be ordered */
  private def checkReciprocalOverlapOrdered(first: Interval, second: Interval) = {
    val lengthFirst = Math.max(first.stop - first.start, 1)
    val lengthSecond = Math.max(second.stop - second.start, 1)

    val overlapLength = if (second.start > first.stop) {
      0 // No overlap
    } else {
      Math.min(first.stop, second.stop) - second.start
    }

    val overlapWorstRatio = overlapLength.toDouble / Math.max(lengthFirst, lengthSecond)

    if (overlapWorstRatio < 0) {
      println("First interval :")
      println(first)
      println("Second interval :")
      println(second)
      throw new Exception("Overlap ratio negative, this should not happen")
    }

    overlapWorstRatio
  }

  // True negatives cannot be estimated since it means all positions in the genome where there is not en event ... (infinity)
  case class Statistics(trueEvents: Int, truePositives: Int, falseNegatives: Int, falsePositives: Int)
  case class Check(found: Boolean, deltaStart: Int, deltaStop: Int)
  def compareTwoDelVcfsSimA(truthVCF: String, myVCF: String, region: String, verbose: Boolean = false) = {
    // This can be slow don't worry about optimization here
    val truthEntries = (extractEntriesFromFile(truthVCF) filter {_.chr == region}).toList
    val myEntries = (extractEntriesFromFile(myVCF) filter {_.chr == region}).toList

    if (verbose) println("My entries size : " + myEntries.length)

    if (truthEntries.length == 0 || myEntries.length == 0) println("Warning check region, one of the sets is empty")

    val myCalls = scala.collection.immutable.SortedMap(myEntries map {e => (e.pos, e.info.get("END").get.toInt)}: _*)

    val results = truthEntries map {
      e => {
        if (verbose) println("TruthEntry " + e)
        val (start, stop) = (e.pos, e.pos + Math.abs(e.info.get("SVLEN").get.toInt))
        val length = (stop - start)
        val AROUND_PERCENT = if (length <= 1000) {0.4} else {0.2} // TODO : Check reciprocal overlap, add 40% for <= 1kb
        val around = (length * AROUND_PERCENT).toInt
        val (checkFrom, checkTo) = (start - around, start + around)
        //val (goalFrom, goalTo) = (stop - around, stop + around)
        (e, myCalls.range(checkFrom, checkTo) map {
          case (myStart, myStop) => {
            val target = if (length <= 1000) {0.6} else {0.8}
            val overlapRatio = checkReciprocalOverlap(Interval(start, stop), Interval(myStart, myStop))
            if (verbose) println(s"$start-$stop vs $myStart-$myStop : overlap ratio $overlapRatio" + (if (length <= 1000) {" Small"} else {""}))
            if (overlapRatio < target) {
              Check(false, Math.abs(start-myStart), Math.abs(stop-myStop))
            } else {
              if (verbose) println("Entry found !")
              Check(true, Math.abs(start-myStart), Math.abs(stop-myStop))
            }
            //if ((myStop >= goalFrom) && (myStop <= goalTo)) {
            //  Check(true, Math.abs(start-myStart), Math.abs(stop-myStop))
            //} else {
            //  Check(false, Math.abs(start-myStart), Math.abs(stop-myStop))
            //}
          }
        } filter {_.found})
      }
    }

    // Length of truthset
    val trueEvents = truthEntries.length

    // Check found results
    val truePositives = (results filter {_._2 != Nil}).length

    // Missed calls
    val falseNegatives = trueEvents - truePositives

    // Extra calls
    val falsePositives = myEntries.length - truePositives

    (Statistics(trueEvents, truePositives, falseNegatives, falsePositives), results)
  }

  /** For real data set ! (lax reciprocal overlap of 50%, however this does not change much) */
  def evaluateDELVCFsOnAllChr(inBamFile: String, vcfDirectory: String, referenceVCFFile: String) = {
    val listOfRegions = getRegions(inBamFile).toList.sortBy(_._2).reverse map {_._1}

    val parameters = ComparisonParameters(extractIntervalFromPosAndSVLEN, extractIntervalFromPosAndEND, /*laxAcceptance,*/ generateFiles = false)

    val results = listOfRegions map {
      region => {
        val delStats = compareTwoVcfs(referenceVCFFile, vcfDirectory + "/" + region + "_deletions_fine.vcf", "chr" + region, parameters)
        //val delStats = compareTwoDelVcfsSimA(referenceVCFFile, vcfDirectory + "/" + region + "_deletions_fine.vcf", "chr" + region)._1
        println(s"$region : " + delStats)
        println(s"$region : " + extractSecondOrderStats(delStats))
        val (all, tp, fn, fp) = Statistics.unapply(delStats).get
        val precision = extractSecondOrderStats(delStats).precision * 100.0
        val recall = extractSecondOrderStats(delStats).recall * 100.0
        println(s"$region & $all & $tp & $fn & $fp & " + f"$precision%2.1f" + "\\% & " + f"$recall%2.1f" + "\\% \\\\")
        delStats
      }
    }

    val prints = listOfRegions map {
      region => {
        val delStats = compareTwoVcfs(referenceVCFFile, vcfDirectory + "/" + region + "_deletions_fine.vcf", "chr" + region, parameters)
        //val delStats = compareTwoDelVcfsSimA(referenceVCFFile, vcfDirectory + "/" + region + "_deletions_fine.vcf", "chr" + region)._1
        val (all, tp, fn, fp) = Statistics.unapply(delStats).get
        val precision = extractSecondOrderStats(delStats).precision * 100.0
        val recall = extractSecondOrderStats(delStats).recall * 100.0
        val print = s"$region & $all & $tp & $fn & $fp & " + f"$precision%2.1f" + "\\% & " + f"$recall%2.1f" + "\\% \\\\"
        print
      }
    }

    prints.sortBy(p => {
      Try(p.split("&")(0).toInt).toOption match {
        case None => 100
        case Some(value) => value
      }
    }) foreach println

    val globalDelResults = results.foldLeft(Statistics(0,0,0,0))((stats, s) => {
      Statistics(stats.trueEvents + s.trueEvents, stats.truePositives + s.truePositives, stats.falseNegatives + s.falseNegatives, stats.falsePositives + s.falsePositives)
    })

    println("Global Del Results :")
    println(globalDelResults)
    println(extractSecondOrderStats(globalDelResults))
  }

  case class ChrStats(chr: String, delStats: Statistics, dupStats: Statistics, invStats: Statistics)

  def statToLine(stat: Statistics, chr: String) = {
    val all = stat.trueEvents
    val tp = stat.truePositives
    val fn = stat.falseNegatives
    val fp = stat.falsePositives
    val precision = extractSecondOrderStats(stat).precision * 100.0
    val recall = extractSecondOrderStats(stat).recall * 100.0
    s"$chr & $all & $tp & $fn & $fp & " + f"$precision%2.1f" + "\\% & " + f"$recall%2.1f" + "\\% \\\\"
  }

  def generateLatexTable(stats: List[ChrStats]) = {
    val xStats = stats.find(c => c.chr.matches(".*[Xx]")).get
    val yStats = stats.find(c => c.chr.matches(".*[Yy]")).get

    def statToTriple(stat: ChrStats) = {
      (statToLine(stat.delStats, stat.chr), statToLine(stat.dupStats, stat.chr), statToLine(stat.invStats, stat.chr))
    }

    val lines = ((stats filter {!_.chr.matches(".*[XxYy]")}).sortBy(c => c.chr.replace("chr", "").toInt) map statToTriple) ++ List(statToTriple(xStats),statToTriple(yStats))

    val delTable = lines map {_._1}
    val dupTable = lines map {_._2}
    val invTable = lines map {_._3}

    // Excuse the crudeness of these strings
    """\begin{tabular}{ccccccc}""" +
    """Chr          & \begin{tabular}[c]{@{}c@{}}Number\\ of events\\ in truth set\end{tabular} & \begin{tabular}[c]{@{}c@{}}Correctly\\ Predicted\\ (TP)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Missed\\ (FN)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Incorrectly\\ Predicted\\ (FP)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Precision\\ TP / (TP + FP)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Recall\\ TP / (TP + FN)\end{tabular} \\ \hline""" + "\n" +
    delTable.mkString("\n") + "\n" +
    """\end{tabular}""" + "\n" +
    """Chr          & \begin{tabular}[c]{@{}c@{}}Number\\ of events\\ in truth set\end{tabular} & \begin{tabular}[c]{@{}c@{}}Correctly\\ Predicted\\ (TP)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Missed\\ (FN)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Incorrectly\\ Predicted\\ (FP)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Precision\\ TP / (TP + FP)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Recall\\ TP / (TP + FN)\end{tabular} \\ \hline""" + "\n" +
    dupTable.mkString("\n") + "\n" + 
    """\end{tabular}""" + "\n" +
    """Chr          & \begin{tabular}[c]{@{}c@{}}Number\\ of events\\ in truth set\end{tabular} & \begin{tabular}[c]{@{}c@{}}Correctly\\ Predicted\\ (TP)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Missed\\ (FN)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Incorrectly\\ Predicted\\ (FP)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Precision\\ TP / (TP + FP)\end{tabular} & \begin{tabular}[c]{@{}c@{}}Recall\\ TP / (TP + FN)\end{tabular} \\ \hline""" + "\n" +
    invTable.mkString("\n") +
    """\end{tabular}""" + "\n"
  }

  def evaluateAllVCFsOnAllChr(inBamFile: String, vcfDirectory: String, referenceVCFDir: String) = {
    val listOfRegions = getRegions(inBamFile).toList.sortBy(_._2).reverse map {_._1}

    val results = listOfRegions map {
      region => {
        //val delStats = compareTwoDelVcfsSimA(referenceVCFDir + "/Sim-A2.DEL.vcf", vcfDirectory + "/" + region + "_deletions_fine.vcf", "chr" + region)._1
        //val dupStats = compareTwoDelVcfsSimA(referenceVCFDir + "/Sim-A2.DUP.vcf", vcfDirectory + "/" + region + "_test_cnv_fine.vcf", "chr" + region)._1
        //val invStats = compareTwoDelVcfsSimA(referenceVCFDir + "/Sim-A2.INV.vcf", vcfDirectory + "/" + region + "_inversions_fine.vcf", "chr" + region)._1
        val parameters = ComparisonParameters(extractIntervalFromPosAndSVLEN, extractIntervalFromPosAndEND, /*laxAcceptance, */ generateFiles = false)
        val delStats = compareTwoVcfs(referenceVCFDir + "/Sim-A2.DEL.vcf", vcfDirectory + "/" + region + "_deletions_fine.vcf", "chr" + region, parameters)
        val dupStats = compareTwoVcfs(referenceVCFDir + "/Sim-A2.DUP.vcf", vcfDirectory + "/" + region + "_test_cnv_fine.vcf", "chr" + region, parameters)
        val invStats = compareTwoVcfs(referenceVCFDir + "/Sim-A2.INV.vcf", vcfDirectory + "/" + region + "_inversions_fine.vcf", "chr" + region, parameters)        
        ChrStats(region, delStats, dupStats, invStats)
      }
    }

    // Print per chromosome results
    results foreach {
      result => {
        println("Chromosome / Region : " + result.chr)
        println("Deletions : ")
        println(result.delStats)
        println(extractSecondOrderStats(result.delStats))
        println("Duplications : ")
        println(result.dupStats)
        println(extractSecondOrderStats(result.dupStats))
        println("Inversions : ")
        println(result.invStats)
        println(extractSecondOrderStats(result.invStats))
        println("- - - ")
      }
    }

    // Print the LaTeX tables
    print(generateLatexTable(results))

    val delStats = results map {_.delStats}
    val dupStats = results map {_.dupStats}
    val invStats = results map {_.invStats}

    val globalDelResults = delStats.foldLeft(Statistics(0,0,0,0))((stats, s) => {
      Statistics(stats.trueEvents + s.trueEvents, stats.truePositives + s.truePositives, stats.falseNegatives + s.falseNegatives, stats.falsePositives + s.falsePositives)
    })
    val globalDupResults = dupStats.foldLeft(Statistics(0,0,0,0))((stats, s) => {
      Statistics(stats.trueEvents + s.trueEvents, stats.truePositives + s.truePositives, stats.falseNegatives + s.falseNegatives, stats.falsePositives + s.falsePositives)
    })
    val globalInvResults = invStats.foldLeft(Statistics(0,0,0,0))((stats, s) => {
      Statistics(stats.trueEvents + s.trueEvents, stats.truePositives + s.truePositives, stats.falseNegatives + s.falseNegatives, stats.falsePositives + s.falsePositives)
    })

    println("Global Del Results :")
    println(extractSecondOrderStats(globalDelResults))
    println(statToLine(globalDelResults, ""))
    println("Global Dup Results :")
    println(extractSecondOrderStats(globalDupResults))
    println(statToLine(globalDupResults, ""))
    println("Global Inv Results :")
    println(extractSecondOrderStats(globalInvResults))
    println(statToLine(globalInvResults, ""))

    results
  }

  case class SecondOrderStats(precision: Double, fdr: Double, recall: Double, fnr: Double)
  def extractSecondOrderStats(stats: Statistics) = {
    stats match {
      case Statistics(trueEvents, truePositives, falseNegatives, falsePositives) => {
        val precision = truePositives / (truePositives + falsePositives).toDouble
        val fdr = falsePositives / (truePositives + falsePositives).toDouble // Complementary (1.0 - x)
        val recall = truePositives / (truePositives + falseNegatives).toDouble
        val fnr = falseNegatives / (truePositives + falseNegatives).toDouble // Complementary (1.0 - x)
        SecondOrderStats(precision, fdr, recall, fnr)
      }
    }
  }



  case class VcfEntryWithLineOfOrigin(entry: VcfEntry, lineOfOrigin: String)
  case class VcfEntryWithPossibleMates(entry: VcfEntryWithLineOfOrigin, mates: List[VcfEntryWithLineOfOrigin])

  /** Default acceptance function based on reciprocal overlap */
  def defaultAcceptance(truthInterval: Interval, candidateInterval: Interval) = {
    val truthLength = truthInterval.stop - truthInterval.start

    val overlap = checkReciprocalOverlap(truthInterval, candidateInterval)
    val targetOverlap = if (truthLength <= 1000) {0.6} else {0.8}

    if (overlap < targetOverlap) {
      false
    } else {
      true
    }
  }

  /** Lax acceptance function based on reciprocal overlap for real data sets */
  def laxAcceptance(truthInterval: Interval, candidateInterval: Interval) = {
    val truthLength = truthInterval.stop - truthInterval.start

    val overlap = checkReciprocalOverlap(truthInterval, candidateInterval)

    // For real datasets only half is required
    if (overlap < 0.5) {
      false
    } else {
      true
    }
  }

  /** Extract an interval from a VCF entry "POS" and "SVLEN" fields */
  def extractIntervalFromPosAndSVLEN(e: VcfEntry) = {
    Interval(e.pos, e.pos + Math.abs(e.info.get("SVLEN").get.toInt))
  }

  /** Extract an interval from a VCF entry "POS" and "END" fields */
  def extractIntervalFromPosAndEND(e: VcfEntry) = {
    Interval(e.pos, e.info.get("END").get.toInt)
  }

  case class ComparisonParameters(
    getBoundariesFromTruthEntry: (VcfEntry) => Interval,
    getBoundariesFromCandidateEntry: (VcfEntry) => Interval,
    acceptanceFunction: (Interval, Interval) => Boolean = defaultAcceptance,
    generateFiles: Boolean = false,
    svType: String = ""
  )
  def compareTwoVcfs(truthSetFile: String, candidateSetFile: String, region: String, parameters: ComparisonParameters) = {

    val filterRegion = region.replace("chrchr", "chr")

    // Extract the entries from the VCF with their lines of origin
    val truthSet = (cnv.Basic.getLines(truthSetFile) filterNot {_.startsWith("#")} filter {_.contains(parameters.svType)} map {line => VcfEntryWithLineOfOrigin(extractEntry(line), line)}).toList filter {_.entry.chr == filterRegion}
    val candidateSet = (cnv.Basic.getLines(candidateSetFile) filterNot {_.startsWith("#")} filter {_.contains(parameters.svType)} map {line => VcfEntryWithLineOfOrigin(extractEntry(line), line)}).toList filter {_.entry.chr == filterRegion}

    if (truthSet.length == 0) println("Warning check region, truth set seems empty")
    if (candidateSet.length == 0) println("Warning check region, candidate set seems empty")

    // Sort the candidate entries by their position for quick look-up
    val candidateEntriesStartMap = scala.collection.immutable.SortedMap(candidateSet map {e => (e.entry.pos, e)}: _*)

    // For every entry in the truth set search for a corresponding entries in the candidate set
    val results = truthSet map {
      e => {
        val truthInterval = parameters.getBoundariesFromTruthEntry(e.entry)
        // Compute the length of the truth interval
        val length = truthInterval.stop - truthInterval.start
        // Compute coordinates to look around the truth interval (arbitrary choice, could be adapted, could be changed to simply find the closests)
        val searchFrom = Math.max(0, truthInterval.start - length)
        val searchTo = truthInterval.stop + length

        // Get all candidates in the vicinity
        val goodCalls = (candidateEntriesStartMap.range(searchFrom, searchTo) map {
          e => {
            val candidateInteval = parameters.getBoundariesFromCandidateEntry(e._2.entry)

            // Check if the candidate interval is good enough
            if (parameters.acceptanceFunction(truthInterval, candidateInteval)) {
              Some(e._2)
            } else {
              None
            }
          }
        }).flatten.toList

        VcfEntryWithPossibleMates(e, goodCalls)
      }
    }

    // Get the calls
    val (missedCalls, foundCalls) = results partition {_.mates.isEmpty}
    val truePositiveCalls = (results.foldLeft(List(): List[VcfEntryWithLineOfOrigin])((list, result) => list ++ result.mates)).toSet.toList // toSet ToList removes duplicates
    val falsePositiveCalls = candidateSet.toSet -- truePositiveCalls

    // Get the statistics
    val stats = Statistics(truthSet.length, /*truePositiveCalls.length*/ foundCalls.size, /*truthSet.length - truePositiveCalls.length*/ missedCalls.size, falsePositiveCalls.size)
    val stats2 = extractSecondOrderStats(stats)

    println(stats)
    println(stats2)
    //println("Missed calls : " + missedCalls.size)

    // Write VCF files in order to check them later
    val missedCallsFileName = truthSetFile.replace(".vcf", "_" + region + "_missed_calls.vcf")
    val truePositiveCallsFileName = candidateSetFile.replace(".vcf", "_" + region + "_true_calls.vcf")
    val falsePositiveCallsFileName = candidateSetFile.replace(".vcf", "_" + region + "_false_calls.vcf")

    def writeCalls(fileName: String,  calls: Iterator[VcfEntryWithLineOfOrigin], commentLines: Iterator[String]) = {
      val writer = printWriter(fileName)

      commentLines foreach writer.println
      calls foreach {c => writer.println(c.lineOfOrigin)}

      writer.close
    }

    if (parameters.generateFiles) {
      writeCalls(missedCallsFileName, (missedCalls map {_.entry}).iterator, cnv.Basic.getLines(truthSetFile) filter {_.startsWith("#")})
      writeCalls(truePositiveCallsFileName, truePositiveCalls.iterator, cnv.Basic.getLines(candidateSetFile) filter {_.startsWith("#")})
      writeCalls(falsePositiveCallsFileName, falsePositiveCalls.iterator, cnv.Basic.getLines(candidateSetFile) filter {_.startsWith("#")})
    }

    stats
  }


  def generateFiguresFromListOfFiles(listFile: String) = {
    val list = cnv.Basic.getLines(listFile).toList filter {_.endsWith(".png")}

    println("\\begin{figure}")
    println("\t\\caption{\\label{...}}")


	// \begin{subfigure}[t]{0.47\textwidth}
	// 	\includegraphics[width=\linewidth]{img/results/sim_dup/dup_false_positive_1}
	// 	\caption{\label{fig:sim_dup_fp1}Duplication false positive}
	// \end{subfigure}
	// \hspace{1mm}
	// \begin{subfigure}[t]{0.47\textwidth}
	// 	\includegraphics[width=\linewidth]{img/results/sim_dup/dup_false_positive_2}
	// 	\caption{\label{fig:sim_dup_fp2}Duplication false positive}
	// \end{subfigure}
	
	// \vspace{2mm}

    list.zipWithIndex foreach {
      case (file, index) => {
        println("""\begin{subfigure}[t]{0.47\textwidth}""")
        println(s"\\includegraphics[width=\\linewidth]{img/results/insertion_subset/$file}")
        println("""\end{subfigure}"""")
        if ((index % 3) < 2) println("\\hspace{1mm}")
        else if ((index % 12) == 11) {
          println("""\end{figure}""")
          println
          println("\\begin{figure}")
          println("\t\\caption{\\label{...}}")
        }
        else println("""\vspace{2mm}""")
      }
    }

    println("\\end{figure}")
  }
}