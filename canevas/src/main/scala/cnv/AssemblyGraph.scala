package cnv

import cnv.GraphG._

object AssemblyGraph {

  case class VertexProperties(kmer: String, sourceReads: Set[htsjdk.samtools.SAMRecord], positions: Map[cnv.Assembly.Position, Int])
  case class GraphProperties(name: String, kmerToVertex: Map[String, Int], k: Int)

  /** Creates a De Bruijn Graph from reads */
  def createDBGGraphFromReads(reads: List[htsjdk.samtools.SAMRecord], k: Int = 51) = {
    // This is not at all optimized !
    // This is experimental (as to which properties are used or useful)
    // Once we know what works best, this can be reworked to be more efficient (less duplication of actions)
    // TODO

    val kplus1mers = cnv.Assembly.kplus1mersFromReads(reads, k)
    val kmers = cnv.Assembly.kmersFromReads(reads, k).flatten.toSet.toList
    //val kmers = reads zip (reads map {_.getReadBases} map {_.sliding(k)} map {_ map {new String(_)}})
    val kmersWithPos = cnv.Assembly.kmersWithPosFromReads(reads.iterator, k)
    val kmerToVertex = Map(kmers.zipWithIndex: _*)
    val kmersWithReadsOfOrigin = cnv.Assembly.kmersWithReadsOfOrigin(reads.iterator, k)
    val vertexProperties = kmersWithPos map {kv => (kmerToVertex.get(kv._1).get, VertexProperties(kv._1, kmersWithReadsOfOrigin.getOrElse(kv._1, Set()), kv._2))}

    val baseGraph = Graph[Int, VertexProperties, SimpleWeight, GraphProperties](Map(), vertexProperties, Map(), GraphProperties("DBG", kmerToVertex, k = k))

    kplus1mers.foldLeft(baseGraph)((graph, kplus1mer) => {
      val from = kmerToVertex.get(kplus1mer.dropRight(1)).get
      val to  = kmerToVertex.get(kplus1mer.drop(1)).get
      addWeightedEdge(Edge(from, to), graph)
    })
  }

  def pathToSequence(path: List[Int], g: Graph[Int, VertexProperties, _, GraphProperties]): String = {
    // Path to kmers
    (path map {vertex => g.vertexProperties.get(vertex).get.kmer}).foldLeft(g.vertexProperties.get(path.head).get.kmer)((seq, kmer) => seq + kmer.last.toString)
  }

  /** Returns the position to which a vertex (kmer) maps to if likely unique, None if unsure */
  def getPosition(vertex: Int, g: Graph[Int, VertexProperties, _, GraphProperties]): Option[Int] = {
    g.vertexProperties.get(vertex) match {
      case None => None
      case Some(value) => {
        // If the kmer only maps to a single position return it
        if (value.positions.size == 1) {
          value.positions.keySet.head
        } else {
          // Sort the positions the kmers maps to
          val sortedPositions = value.positions.toList.sortBy(kv => kv._2).reverse
          val first = sortedPositions(0)
          val second = sortedPositions(1)

          // Return the most likely position only if it is at least twice as likely as the next possibility
          if (first._2 > (second._2 * 2)) {
            first._1
          } else {
            None
          }
        }
      }
    }
  }

  /** Returns true if the haplotype is well anchored at both ends, meaning the kmers are nicely mapped */
  def anchoredHap(hapPath: Path[Int], g: Graph[Int, VertexProperties, _, GraphProperties]): Boolean = {
    val anchorSize = 3

    val startAnchor = hapPath.take(anchorSize) map {vertex => getPosition(vertex, g)}
    val endAnchor = hapPath.reverse.take(anchorSize).reverse map {vertex => getPosition(vertex, g)}

    val startAnchored = startAnchor.sliding(2).foldLeft(true)((cond, pair) => cond && ((pair(0).getOrElse(-1) + 1) == pair(1).getOrElse(-1)))
    val endAnchored = endAnchor.sliding(2).foldLeft(true)((cond, pair) => cond && ((pair(0).getOrElse(-1) + 1) == pair(1).getOrElse(-1)))

    startAnchored && endAnchored
  }

}