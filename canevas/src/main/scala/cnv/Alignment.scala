package cnv

import scala.jdk.CollectionConverters._
import scala.annotation.tailrec

import htsjdk.samtools._
import org.biojava.nbio.core.sequence._
import org.biojava.nbio.core.sequence.compound._
import org.biojava.nbio.core.alignment.template._
import org.biojava.nbio.alignment.Alignments._

import cnv.Assembly._

object Alignment {

  def alignSequences(sequence: String, reference: String) = {
    val target = new DNASequence(reference, org.biojava.nbio.core.sequence.compound.AmbiguityDNACompoundSet.getDNACompoundSet)
    val query = new DNASequence(sequence, org.biojava.nbio.core.sequence.compound.AmbiguityDNACompoundSet.getDNACompoundSet)

    val matrix = org.biojava.nbio.core.alignment.matrices.SubstitutionMatrixHelper.getNuc4_4 // Scoring Matrix
    val gapP = new org.biojava.nbio.alignment.SimpleGapPenalty(4, 1); // Gap opening, continuation penalties
    
    val aligner = org.biojava.nbio.alignment.Alignments.getPairwiseAligner(query, target, PairwiseSequenceAlignerType.GLOBAL, gapP, matrix)

    aligner.getPair.getAlignedSequences.asScala
  }

  /** Generates the cigar string of an alignment between a sequence and a reference */
  def generateCigar(sequence: String, reference: String) = {
    val alignment = alignSequences(sequence, reference)
    val referenceAlignment = alignment(1).toString()
    val sequenceAlignment = alignment(0).toString()

    val zippedAlignment = referenceAlignment zip sequenceAlignment

    case class Acc(cigar: List[CigarElement], currentState: CigarOperator, count: Int)

    @tailrec
    def generateCigarTR(it: Iterator[(Char, Char)], acc: Acc): Cigar = {
      acc match {
        case Acc(cigar, currentState, count) => {
          it.nextOption match {
            case None => new Cigar((if(count != 0) {new CigarElement(count, currentState) :: cigar} else {cigar}).reverse.asJava)
            case Some(pair) => {
              val state = pair match {
                case ('-', _) => htsjdk.samtools.CigarOperator.I
                case (_, '-') => htsjdk.samtools.CigarOperator.D
                case (_, _) => htsjdk.samtools.CigarOperator.M
              }
              if (state != currentState) {
                generateCigarTR(it, Acc(if(count != 0) {new CigarElement(count, currentState) :: cigar} else {cigar}, state, 1))
              } else {
                generateCigarTR(it, Acc(cigar, state, count + 1))
              }
            }
          }
        }
      }
    }

    generateCigarTR(zippedAlignment.iterator, Acc(List(), currentState = CigarOperator.N, 0))
  }

  def generateCigarString(sequence: String, reference: String): String = {
    generateCigar(sequence, reference).toString
  }

  case class ReassemblyParameters(k: Int = 51, mappingQual: Int = 60, phredQual: String = "*", pruneUntil: Int = 30, pruneLessThan: Int = 4)
  def DEFAULT_PARAMETERS = ReassemblyParameters()

  def generateSAMRecord(inBamFile: String, refFaFile: String, region: String, start: Int, stop: Int, params: ReassemblyParameters = DEFAULT_PARAMETERS) = {
    val regionNumber = region.replace("chr", "")
    val readsList = cnv.SamToolBox.getIteratorOnSubRegion(inBamFile, region, start, stop).toList
    val k = params.k
    val phredQual = params.phredQual
    val mappingQual = params.mappingQual

    val dbg = updateWeightedGraphWithkplus1mers(Map(), kplus1mersFromReads(readsList, k).iterator)
    val tryPrune = pruneUntilSingleThreadsOrLimitReached(dbg, params.pruneUntil /* This parameters should be depedent on the local coverage */)
    val pruned = tryPrune.get
    //if (isSingleThread(pruned)) {
    if (hasOnlySingleThreads(pruned)) {
      val posMap = kmersWithPosFromReads(readsList.filter(_.getMappingQuality != 0).iterator, k)
      //val hap = getASequenceFromGraph(pruned)
      val hap = getSequencesFromGraph(pruned).maxBy(_.length()) // Get the longest contig
      val mappedHap = mapSequence(hap, posMap).toList
      val reffa = new htsjdk.samtools.reference.IndexedFastaSequenceFile(new java.io.File(refFaFile))
      val hapStart = mappedHap.head._2.get
      val hapStop = mappedHap.last._2.get + (k - 1)
      val ref = reffa.asInstanceOf[htsjdk.samtools.reference.ReferenceSequenceFile].getSubsequenceAt(regionNumber /* depends on reference used */, hapStart, hapStop).getBaseString
      val header = cnv.SamToolBox.readerFromSamFile(inBamFile).getFileHeader

      val rec = samAlignment(hap, ref, hapStart, hapStop, region, header)

      Some(rec)
    } else {
      None
    }
  }

  case class DBGraphParameters(k: Int = 51, minMAPQ: Int = 0, includeMates: Boolean = false)
  val DBG_DEFAULT_PARAMETERS = DBGraphParameters()

  /** Generates a De Bruijn Graph from a given region */
  def dbgFromRegion(inBamFile: String, region: String, start: Int, stop: Int, params: DBGraphParameters = DBG_DEFAULT_PARAMETERS) = {
    params match {
      case DBGraphParameters(k, minMAPQ, includeMates) => {
        val reads = cnv.SamToolBox.getIteratorOnSubRegion(inBamFile, region, start, stop).filter(_.getMappingQuality() >= minMAPQ).toSet
        val readsList = (if (includeMates) { // This may be slow because mate querying is random access, especially if mates are unmapped
          val samReader = cnv.SamToolBox.readerFromSamFile(inBamFile)
          reads.map(r => samReader.queryMate(r)).filter(r => r != null) union reads // Put reads and their mates together
        } else {reads}).toList
        // Create the De Bruijn Graph from the reads
        val dbg = cnv.AssemblyGraph.createDBGGraphFromReads(readsList)
        dbg
      }
    }
  }

  /** Generates SAM Records of possible haplotypes over a region (can be empty) */
  def generateSAMRecordsGraphG(inBamFile: String, refFaFile: String, region: String, start: Int, stop: Int, params: ReassemblyParameters = DEFAULT_PARAMETERS) = {
    params match { // Extract parameters
      case ReassemblyParameters(k, mappingQual, phredQual, pruneUntil, pruneLessThan) => {
        val MAXHAPS = 4

        val regionNumber = region.replace("chr", "")
        val halfWindowSize = (stop - start) / 2

        // Create and prune graph
        val dbg = dbgFromRegion(inBamFile, region, start, stop, DBG_DEFAULT_PARAMETERS.copy(k = k))
        // Prune edges that have a low coverage
        val pruned = cnv.GraphG.pruneBasedOnPredicate(dbg, (prop: cnv.GraphG.SimpleWeight) => prop.weight < pruneLessThan)
        // Prune kmers that come solely from MAPQ0
        val prunedMore = cnv.GraphG.pruneVerticesBasedOnPredicate(pruned, (vp: cnv.AssemblyGraph.VertexProperties) => (vp.sourceReads map {read => read.getMappingQuality}) == Set(0))

        // The haps are coming from non looping pathes
        val haps = cnv.GraphG.nonLoopingPathes(prunedMore) filter {p => cnv.AssemblyGraph.anchoredHap(p, prunedMore)} // Only keep the well anchored haps

        // Helper class
        case class HapInfo(sequence: String, start: Int, stop: Int)
        // Remove small haplotypes, sort by average edge weight and take only the max number
        val hapsWithInfo = ((haps filter {_.size > halfWindowSize/2}).sortBy(h => (cnv.MyMath.mean((h.sliding(2) map {case Seq(e1, e2) => prunedMore.edgeProperties.get(cnv.GraphG.Edge(e1,e2)).get.weight}).toList)))).take(MAXHAPS) map {
          h => HapInfo(cnv.AssemblyGraph.pathToSequence(h, prunedMore), cnv.AssemblyGraph.getPosition(h.head, prunedMore).get, cnv.AssemblyGraph.getPosition(h.last, prunedMore).get + (prunedMore.graphProperties.k - 1))
        }
      
        // Reference
        val reffa = new htsjdk.samtools.reference.IndexedFastaSequenceFile(new java.io.File(refFaFile))
        val header = cnv.SamToolBox.readerFromSamFile(inBamFile).getFileHeader
      
        // Align the haplotypes and create sam records
        hapsWithInfo filter {h => h.start < h.stop} map {
          case HapInfo(sequence, hapStart, hapStop) => {
            val ref = reffa.asInstanceOf[htsjdk.samtools.reference.ReferenceSequenceFile].getSubsequenceAt(regionNumber /* depends on reference used */, hapStart, hapStop).getBaseString
          
            samAlignment(sequence, ref, hapStart, hapStop, region, header)
          }
        }
      }
    }
  }

  /** Creates a new alignment as a SAM Record */
  def samAlignment(hap: String, ref: String, start: Int, stop: Int, region: String, header: htsjdk.samtools.SAMFileHeader, name: String = ""): htsjdk.samtools.SAMRecord = {
    val phredQual = "*"
    val mappingQual = 60

    val alignment = alignSequences(hap, ref)
    val rec = new SAMRecord(header)
    rec.setAlignmentStart(start)
    rec.setReadName(if (name.isEmpty) { "Haplotype_" + region + ":" + start + "-" + stop + "_" + java.util.UUID.randomUUID()} else {name})
    rec.setReadString(hap)
    rec.setReferenceName(region)

    rec.setCigar(generateCigar(hap, ref))
    rec.setBaseQualityString(phredQual * hap.length) // No qualities
    rec.setFlags(0x000) // check with rec.isValid
    rec.setMappingQuality(mappingQual)

    rec
  }

}