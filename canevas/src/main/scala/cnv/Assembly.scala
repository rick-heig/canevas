package cnv

//import com.liangdp.graphviz4s._

package object Assembly {
import scala.annotation.tailrec

  // Below are generic types not yet used

/*   case class Node[T](value: T)

  trait Directional[T] {
    def from: Node[T]
    def to: Node[T]
  }

  trait Weighted[T] {
    def weight: T
  }

  case class Edge[T](from: Node[T], to: Node[T]) extends Directional[T]
  case class WeightedEdge[T](from: Node[T], to: Node[T], weight: Int) extends Directional[T] with Weighted[Int]

  case class Graph[T](nodes: Iterable[Node[T]], edges: Iterable[Node[T]]) */

  // Below are algorithms based on an adjacency list representation (node, edges) equiv to (node, list of nodes the outgoing edges go to)

  type Node = String
  type EdgeTo = String
  type Weight = Int
  type Position = Option[Int]
  type WeightedEdgeTo = (Node, Weight)
  type WeightedAdjacencyMap = Map[Node, Weight]
  def WeightedAdjacencyMap(xs: WeightedEdgeTo*) = Map(xs: _*)
  type AdjacencyList = List[Node]
  def AdjacencyList(xs: Node*) = List(xs: _*)
  type GraphAsAdjacencyMap = Map[Node, AdjacencyList]
  def GraphAsAdjacencyMap(xs: (Node, AdjacencyList)*) = Map(xs: _*)
  type GraphWeAsAdjacencyMap = Map[Node, WeightedAdjacencyMap]
  def GraphWeAsAdjacencyMap(xs: (Node, WeightedAdjacencyMap)*) = Map(xs: _*)

  // This could be done way better with OOP
  type WeightedEdgeToInteger = (Int, Weight)
  type WeightedIntegerAdjacencyMap = Map[Int, Weight]
  def WeightedIntegerAdjacencyMap(xs: WeightedEdgeToInteger*) = Map(xs: _*)
  type GraphWeAsIntegerAdjacencyMap = Map[Int, WeightedIntegerAdjacencyMap]
  def GraphWeAsIntegerAdjacencyMap(xs: (Int, WeightedIntegerAdjacencyMap)*) = Map(xs: _*)

  case class IntegerGraphWithTranslation(graph: GraphWeAsIntegerAdjacencyMap, translation: Map[Node, Int])

  // Note : The implementations below are not made to be the most efficient, e.g., Strings are used for k-mers and two Strings are
  // used even if they overlap by k-1 bases, this could be improved by using immutable arrays of bytes because this will be optimized
  // by construction (since the structures are immutable, like creating a new immutable list by adding an element) to be more memory
  // efficient a prefix/suffix tree could be used since the k-mers inherently share suffixes and prefixes. However since these tools
  // are meant to solve "small" cases after thorough filtering (i.e., ~thousands of cases per chromosome, tens of thousand WGS) with
  // a limited number of reads, this should not be a problem.
  // However in order to do assembly with the unmapped reads it may be necessary to rework this package or write an optimized version.

  // Probably better for performance to rewrite this package with arrays of bytes instead of Strings and better data structures
  // Better also to create graphs with nodes that are indexed by an integer (or long) instead of a full String, comparisons will be
  // much cheaper, then have a bidirectional map to do k-mer <-> integer to do the translation.

  /** Adds a node to a graph */
  def addNode(node: Node, g: GraphAsAdjacencyMap) = {
    val adjacencyList = g.getOrElse(node, AdjacencyList())
    g - node + (node -> adjacencyList)
  }

  /** Adds a node to as weighted edge graph */
  def addNodeWeGraph(node: Node, g: GraphWeAsAdjacencyMap) = {
    val adjacencyList = g.getOrElse(node, WeightedAdjacencyMap())
    g - node + (node -> adjacencyList)
  }

  /** Updates a graph with a from-to relation (overlapping k-mers) */
  def updateGraphWithKmers(from: Node, to: Node, g: GraphAsAdjacencyMap) = {
    val edges = g.getOrElse(from, AdjacencyList())
    val newEdges = edges ++ AdjacencyList(to)

    addNode(to, g) - from + (from -> newEdges)
  }

  /** Updates weighted "to" edges with a new "to" relation */ 
  def updateWeightedEdges(to: Node, weightedEdges: WeightedAdjacencyMap) = {
    val weight = weightedEdges.getOrElse(to, 0)
    val newWeight = weight + 1

    weightedEdges - to + (to -> newWeight)
  }

  /** Updates a graph with a from-to realtion (overlapping k-mers) */
  def updateWeightedGraphWithKmers(from: Node, to: Node, g: GraphWeAsAdjacencyMap) = {
    val edges = g.getOrElse(from, WeightedAdjacencyMap())
    val newEdges = updateWeightedEdges(to, edges)

    addNodeWeGraph(to, g) - from + (from -> newEdges)
  }

  /** Get k-mers of reads as list collections of k-mers as string */
  def kmersFromReads(reads: List[htsjdk.samtools.SAMRecord], k: Int) = {
    reads map {_.getReadBases} map {_.sliding(k)} map {_ map {new String(_)}}
  }

  /** Get positions of unclipped bases from read */
  def getUnclippedPositionsFromRead(read: htsjdk.samtools.SAMRecord) = {
    val start = read.getStart()
    val end = read.getEnd()
    val unclippedStart = read.getUnclippedStart() // Possibly further left
    val unclippedEnd = read.getUnclippedEnd() // Possibly further right

    // Ranges (may be empty)
    val startClippedRange = unclippedStart until start
    val unClippedRange = start to end
    val endClippedRange = (end + 1) to unclippedEnd

    startClippedRange.map(_ => None) concat unClippedRange.map(Some(_)) concat endClippedRange.map(_ => None)
  }

  /** Get positions for the k-mers that have at most a certain percentage of their bases clipped */
  def getKmerPositionsFromRead(read: htsjdk.samtools.SAMRecord, k: Int, acceptableClipFactor: Double = 0.5) = {
    val acceptableClip: Int = (acceptableClipFactor * k).toInt - 1 // TODO : Check this

    val start = read.getStart()
    val end = read.getEnd()
    val unclippedStart = read.getUnclippedStart() // Possibly further left
    val unclippedEnd = read.getUnclippedEnd() // Possibly further right
    val kmerend = unclippedEnd - (k-1)
    val endClippedBases = unclippedEnd - end
    val startClippedBases = start - unclippedStart

    // Ranges (may be empty) positions are not given for k-mers that are clipped by more than 50%
    val tooMuchClippedRange = unclippedStart until (start - acceptableClip)
    val tooMuchClippedEndRange = (kmerend + 1 - (endClippedBases - acceptableClip)) to kmerend
    val goodEnoughPositions = Math.max(unclippedStart, tooMuchClippedRange.end) to Math.min(kmerend, tooMuchClippedEndRange.start)

    tooMuchClippedRange.map(_ => None) concat goodEnoughPositions.map(Some(_)) concat tooMuchClippedEndRange.map(_ => None)
  }

  /** This creates a Map with k-mers as keys and histograms of positions as values */
  def kmersWithPosFromReads(reads: Iterator[htsjdk.samtools.SAMRecord], k: Int): Map[String,Map[Position,Int]] = {
    ((reads map {
      // Extract k-mers
      rec => (rec.getReadBases.sliding(k) map {
        new String(_)
      // Zip them with their index (relative position)
      }).zip(getKmerPositionsFromRead(rec, k, 0.3))
    // flatten (group all entries) and regroup them by kmer
    }).flatten.toList groupBy {
      case (kmer, _) => kmer
    // Remove redundancy from Map (this will create an k-mer to histogram of positions map)
    }).view.mapValues(_.map {case (kmer, pos) => pos}).mapValues(list => list.groupBy(pos => pos).view.mapValues(_.size).toMap).toMap
    // This is convoluted because Map.mapValues(f) is deprecated in scala 2.13.0 and requires to do Map.view.mapValues(f).toMap for now...
  }

  /** This creates a Map with k-mers as keys and a set of reads they originate from as values */
  def kmersWithReadsOfOrigin(reads: Iterator[htsjdk.samtools.SAMRecord], k: Int): Map[String, Set[htsjdk.samtools.SAMRecord]] = {
    ((reads map {
      rec => rec.getReadBases.sliding(k) map {
        kmer => (new String(kmer), rec)
      }
    }).flatten.toList groupBy {
      case (kmer, _) => kmer
    }).view.mapValues(_.map {case (kmer, read) => read}).mapValues(reads => reads.toSet).toMap
  }

  /** Map sequence with most likely positions */
  def mapSequence(seq: String, posMap: Map[String,Map[Position,Int]]) = {
    // Get k from the position map, all the k-mers should have the same k
    val k = posMap.head._1.length()

    // Map the sequence
    //seq.sliding(k) map {kmer => (kmer, posMap.get(kmer) match { // method sliding in class StringOps is deprecated (since 2.13.0): Use `s.toSeq.sliding(...).map(_.unwrap)` instead of `s.sliding(...)`
    seq.toSeq.sliding(k).map(_.unwrap) map {kmer => (kmer, posMap.get(kmer) match {
      case None => None // No position
      case Some(histogram) => histogram.maxBy(_._2)._1 // Most likely position
    })}
  }

  /** Align sequence */
  def AlignSequence(mappedSeq: List[(String, Position)]) = {
    val positionList = mappedSeq map {case (kmer, possiblePosition) => possiblePosition}
    val start = positionList.dropWhile(possiblePosition => possiblePosition.isEmpty).head.get

    // TODO

    // Only breaks in positions should indicate some variation, e.g., "None" position can be viewed as novel insertion
    // If the positions are not monotonically increasing it may be sign of an inversion or translocation etc.
    // Deletions are the easiest because they show up as a jump in positions

    // Check if monotonically increasing
    val monotonicallyIncreasing = (positionList.flatten.sliding(2) dropWhile {case Seq(p1, p2) => p1 < p2}).isEmpty
  }

  /** Get (k+1)-mers from reads as list of (k+1)-mers as string */
  def kplus1mersFromReads(reads: List[htsjdk.samtools.SAMRecord], k: Int) = {
    kmersFromReads(reads, k+1).flatten
  }

  /** Updates a graph with (k+1)-mers (representing two overlapping k-mers) */
  def updateGraphWithkplus1mers(g: GraphAsAdjacencyMap, kplus1mers: Iterator[String]) = {
    kplus1mers.foldLeft(g)((acc, kplus1mer) => {
      val from: Node = kplus1mer.dropRight(1)
      val to: Node = kplus1mer.drop(1)

      updateGraphWithKmers(from, to, acc)
    })
  }

  /** Updates a weighted edge graph with (k+1)-mers (representing two overlapping k-mers) */
  def updateWeightedGraphWithkplus1mers(g: GraphWeAsAdjacencyMap, kplus1mers: Iterator[String]) = {
    kplus1mers.foldLeft(g)((acc, kplus1mer) => {
      val from: Node = kplus1mer.dropRight(1)
      val to: Node = kplus1mer.drop(1)

      updateWeightedGraphWithKmers(from, to, acc)
    })
  }

  /** Creates a weighted edge graph from reads */
  def createWeGraphFromReads(reads: List[htsjdk.samtools.SAMRecord], k: Int) = {
    updateWeightedGraphWithkplus1mers(GraphWeAsAdjacencyMap(), kplus1mersFromReads(reads, k).iterator)
  }

  /** Gives an integer number to each node */
  def annotationOfGraph[T](g: Map[Node, T]) = {
    Map((g.zipWithIndex map {case ((s, _), v) => (s, v)}).toSeq: _*)
  }

  /** Associates the reads with a list of integer numbers for each of it's the k-mers corresponding to the nodes of the graph
   *  It will be helpful in order to remap the reads on the graph.
   */
  def annotateReads(reads: List[htsjdk.samtools.SAMRecord], dbg: Map[Node, _]) = {
    val k = dbg.head._1.length() // The graph should have only k-mers with a fixed k value
    val annotatedDbg = annotationOfGraph(dbg)
    reads zip (kmersFromReads(reads, k) map {list => list map {kmer => annotatedDbg.get(kmer)}} map {it => it.toList})
  }

  /** Removes unconnected nodes */
  def removeUnconnectedNodes(dbg: GraphAsAdjacencyMap) = {
    // Nodes that edges point to
    val destinationNodes = dbg.values.foldLeft(Set[Node]())((acc, e) => acc.union(e.toSet))
    // Nodes that are in the graph and have neither outgoing edges nor are destinations
    val lonelyNodes = (dbg filter {_._2.isEmpty}).keySet -- destinationNodes
    // Remove the unconnected nodes
    dbg -- lonelyNodes
  }

  /** Removes unconnected nodes */
  def removeUnconnectedNodesWe(dbgw: GraphWeAsAdjacencyMap) = {
    // Nodes that edges point to
    val destinationNodes = dbgw.values.foldLeft(Set[Node]())((acc, e) => acc.union(e.keySet))
    // Nodes that are in the graph and have neither outgoing edges nor are destinations
    val lonelyNodes = (dbgw filter {_._2.isEmpty}).keySet -- destinationNodes
    // Remove the unconnected nodes
    dbgw -- lonelyNodes
  }

  /** Removes edges of weight less than or equal to threhsold and unconnected nodes */
  def pruneGraph(dbgw: GraphWeAsAdjacencyMap, threshold: Int = 1): GraphWeAsAdjacencyMap = {
    removeUnconnectedNodesWe(dbgw map {case (node, edges) => (node, edges filterNot {_._2 <= threshold})})
  }

  /** Returns true if each node has at most one outgoing edge */
  def hasAtMostOneEdgeGoingOutOfEachNode(dbg: GraphWeAsAdjacencyMap) = {
    (dbg.values filter {_.size > 1}).isEmpty
  }

  /** Returns true if a node has at most one ingoing edge */
  def hasAtMostOneEdgeGoingInOfEachNode(dbg: GraphWeAsAdjacencyMap) = {
    // Take all "to" nodes from edges as sets and take the union, if the |union(sets)| = sum |sets| then each node has at most a single ingoing edge
    ((dbg.values map {_.keySet}).foldLeft(Set[String]())(_.union(_))).size == dbg.values.foldLeft(0)((acc, map) => acc + map.size)
  }

  /** True if only single thread graphs (no bubbles) */
  def hasOnlySingleThreads(dbgw: GraphWeAsAdjacencyMap) = {
    hasAtMostOneEdgeGoingInOfEachNode(dbgw) && hasAtMostOneEdgeGoingOutOfEachNode(dbgw)
  }

  /** Returns true if a graph is a single connected thread of nodes (k-mers) 
   * @TODO Check if this is OK, it seems to fail sometimes with big graphs, e.g.,
   * val dbg = cnv.Assembly.graphFromRegion("/media/rick/c1887699-787a-456e-a50d-ec3844b33c1e/master/genome_center_data/Guipponi.NA12878-B.mdup.sorted.bam.bqsr.bam", "chr8", 895363, 901041, 51)
   * val pruned = cnv.Assembly.pruneUntilSingleThreadOrLimitReached(dbg, 200).get
   * cnv.Assembly.isSingleThread(pruned)
   * But when sent to gephi it seems wrong (probably because there are circular islands)
  */
  def isSingleThread(dbgw: GraphWeAsAdjacencyMap) = {
    // These two conditions are not enough, because threded islands will still make this true (hence the two second conditions)
    hasOnlySingleThreads(dbgw) && findStartingPointsWe(dbgw) == 0 && findStoppingPointsWe(dbgw) == 0
  }

  /** Find nodes that have no edges going out of them */
  def findStoppingPoints(dbg: Map[Node, Iterable[_]]) = {
    // If a node has no outgoing (to) edges it is a stopping point
    (dbg filter {_._2.isEmpty}).keySet
  }

  /** Finds nodes that have no edges going into them */
  def findStartingPoints(dbg: GraphAsAdjacencyMap) = {
    dbg.keySet -- (dbg.values.foldLeft(Set[Node]())((acc, list) => acc.union(list.toSet)))
  }

  /** Find nodes that have no edges going out of them */
  def findStoppingPointsWe(dbgw: GraphWeAsAdjacencyMap) = {
    // If a node has no outgoing (to) edges it is a stopping point
    (dbgw filter {_._2.isEmpty}).keySet
  }

  /** Finds nodes that have no edges going into them */
  def findStartingPointsWe(dbgw: GraphWeAsAdjacencyMap) = {
    dbgw.keySet -- (dbgw.values map {_.keySet}).foldLeft(Set[Node]())(_.union(_))
  }

  /** Gets a sequence from graph, starting from a starting point */
  def getASequenceFromGraph(dbgw: GraphWeAsAdjacencyMap) = {
    def threadGraph(node: Node) = {
      @tailrec
      def threadGraph(node: Node, acc: List[Char]): String = {
        dbgw.get(node) match {
          case None => "" // Should not happen, it means the node is not part of the graph
          case Some(edge) => if (edge.isEmpty) {
            acc.reverse.mkString // Stops when no outgoing edges
          } else {
            threadGraph(edge.head._1 /* follow an edge */, edge.head._1.last :: acc)
          }
        }
      }
      threadGraph(node, node.reverse.toList)
    }

    val startingPoints = findStartingPointsWe(dbgw)
    val stoppingPoints = findStoppingPoints(dbgw)

    // If the graph is circular at some point E.g., O shape or ->->->->O shape or O->->->, O denoting a loop
    if (startingPoints.isEmpty || stoppingPoints.isEmpty) {
      "" // Return an empty string
    } else {
      val start = startingPoints.head // Take a starting point
      threadGraph(start) // Generate a sequence
    }
  }

  /** Gets sequences from graph with only single threads (no bubbles)
   * @TODO This is not right, it only follows the head edge
   */
  def getSequencesFromGraph(dbgw: GraphWeAsAdjacencyMap) = {
    def threadGraph(node: Node) = {
      @tailrec
      def threadGraph(node: Node, acc: List[Char]): String = {
        dbgw.get(node) match {
          case None => "" // Should not happen, it means the node is not part of the graph
          case Some(edge) => if (edge.isEmpty) {
            acc.reverse.mkString // Stops when no outgoing edges
          } else {
            threadGraph(edge.head._1 /* follow an edge */, edge.head._1.last :: acc)
          }
        }
      }
      threadGraph(node, node.reverse.toList)
    }

    val startingPoints = findStartingPointsWe(dbgw)
    val stoppingPoints = findStoppingPoints(dbgw)

    // If the graph is circular at some point E.g., O shape or ->->->->O shape or O->->->, O denoting a loop
    if (startingPoints.isEmpty || stoppingPoints.isEmpty) {
      List[String]() // Return an empty string
    } else {
      startingPoints.toList map {threadGraph(_)} // Generate sequences
    }
  }

  /** Creates a map with weights as keys and the number of time they appear as values, i.e., a histogram of weights */
  def getWeightDistribution(dbgw: GraphWeAsAdjacencyMap) = {
    // Get the weighted edges, groupBy weight, count the number of occurrences, sort by weight
    dbgw.values.map(_.values).flatten.groupBy(w => w).view.mapValues(l => l.size).toList.sortBy(_._1)
  }

  /** Tries to prune the graph by removing edges of low weight until it is a single thread or a limit has been reached */
  def pruneUntilSingleThreadsOrLimitReached(dbgw: GraphWeAsAdjacencyMap, limit: Weight) = {
    @tailrec
    def tr(g: GraphWeAsAdjacencyMap, w: Weight): Option[GraphWeAsAdjacencyMap] = {
      if (w > limit) {
        None
      } else {
        pruneGraph(g, w) match {
          case pruned if (hasOnlySingleThreads(pruned)) => Some(pruned)
          case pruned => tr(pruned, w+1)
        }
      }
    }

    tr(dbgw, 1)
  }

  /** Generate a de weighted De Bruijn Graph on a region */
  def graphFromRegion(inBamFile: String, region: String, start: Int, stop: Int, k: Int) = {
    val readsList = cnv.SamToolBox.getIteratorOnSubRegion(inBamFile, region, start, stop).toList.filter(_.getMappingQuality() != 0)

    val dbg = updateWeightedGraphWithkplus1mers(Map(), kplus1mersFromReads(readsList, k).iterator)
    dbg
  }

  /** This is an alternate representation for faster graph processing (e.g., full traversal) */
  def integerGraphFromGraph(g: GraphWeAsAdjacencyMap) = {
    // Create transaltion table
    val translationTable = Map[Node, Int](g.keys.zipWithIndex.toSeq: _*)
    // Translate the graph
    IntegerGraphWithTranslation(g map {case (node -> adj) => (translationTable.get(node).get -> (adj map {case (to -> weight) => (translationTable.get(to).get -> weight)}))}, translationTable)
  }

  def integerGraphFromGraph2(g: GraphWeAsAdjacencyMap) = {
    val nodes = (g.keySet ++ g.values.foldLeft(Set[Node]())((set, map) => set ++ map.keySet)).toList
    val nodeProperties = Map((nodes.indices zip nodes): _*)
    val nodeToInt = Map(nodes.zipWithIndex: _*)
    val emptyGraph = cnv.Graph.createEmptyWeightedEdgeGraph("Name").copy(vertexProperties = nodeProperties)

    g.foldLeft(emptyGraph)((newGraph, adjacencyList) => {
      adjacencyList match {
        case (node, wadjmap) if (wadjmap.isEmpty) => cnv.Graph.addVertex(nodeToInt.get(node).get, newGraph)
        case (node, wadjmap) => {
          wadjmap.foldLeft(newGraph)((graph, wto) => {
            val edge = cnv.Graph.Edge(nodeToInt.get(node).get, nodeToInt.get(wto._1).get)
            cnv.Graph.addEdge(edge, cnv.Graph.SimpleWeight(wto._2), graph)
          })
        }
      }
    })
  }

  // Graphvis4s related functions
  //def printGraph(dbg: GraphAsAdjacencyMap, fileName: String, graphName: String = "dbg") = {
  //  val g = new Digraph(graphName)
  //  dbg foreach {
  //    case (sourceNode, setOfNodes) => setOfNodes foreach {
  //      case (destNode: String) => g.edge(sourceNode, destNode)
  //    }
  //  }
  //  g.view(fileName = fileName, directory = "sandbox")
  //}
  //
  //def printGraphWe(dbgw: GraphWeAsAdjacencyMap, fileName: String, graphName: String = "dbgw") = {
  //  val g = new Digraph(graphName)
  //  dbgw foreach {
  //    case (sourceNode, setOfNodes) => setOfNodes foreach {
  //      case (destNode: String, weight: Int) => g.edge(sourceNode, destNode, attrs = scala.collection.mutable.Map(("label", weight.toString)))
  //    }
  //  }
  //  g.view(fileName = fileName, directory = "sandbox")
  //}
}