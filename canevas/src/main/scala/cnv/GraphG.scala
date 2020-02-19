package cnv

import scala.annotation.tailrec

object GraphG {

  case class Edge[Vertex](from: Vertex, to: Vertex)
  type OutEdgeSet[Vertex] = Set[Vertex]
  def OutEdgeSet[Vertex](xs: Vertex*) = Set[Vertex](xs: _*)

  trait Weighted extends Ordered[Weighted] {
    def weight: Int
    def incrementedWeight: Weighted

    def compare(that: Weighted): Int = (this.weight) compare (that.weight)
  }

  case class SimpleWeight(weight: Int) extends Weighted {
    def incrementedWeight: Weighted = SimpleWeight(this.weight + 1)
  }

  def updateWeight(ow: Option[SimpleWeight]) = {
    ow match {
      case None => Some(SimpleWeight(1))
      case Some(SimpleWeight(weight)) => Some(SimpleWeight(weight + 1))
    }
  }

  case class Graph[Vertex, VertexProperties, EdgeProperties, GraphProperties](adjacencyMap: Map[Vertex, OutEdgeSet[Vertex]], vertexProperties: Map[Vertex, VertexProperties], edgeProperties: Map[Edge[Vertex], EdgeProperties], graphProperties: GraphProperties)

  /** Creates an empty graph */
  def createEmptyGraph[Vertex, VertexProperties, EdgeProperties, GraphProperties](gp: GraphProperties) = {
    Graph[Vertex, VertexProperties, EdgeProperties, GraphProperties](Map[Vertex, OutEdgeSet[Vertex]](), Map[Vertex, VertexProperties](), Map[Edge[Vertex], EdgeProperties](), gp)
  }

  /** Creates an empty graph with a string graph property (e.g., name of the graph) */
  def createEmptyGraph(gp: String) = {
    createEmptyGraph[Int, String, Int, String](gp)
  }

  def createEmptyWeightedEdgeGraph(gp: String) = {
    Graph[Int, String, SimpleWeight, String](Map(), Map(), Map(), gp)
  }

  /** Adds a vertex to an adjacency map */
  def addVertexToAm[Vertex](vertex: Vertex, adjacencyMap: Map[Vertex, OutEdgeSet[Vertex]]) = {
    adjacencyMap.updated(vertex, adjacencyMap.getOrElse(vertex, OutEdgeSet[Vertex]()))
  }

  // TODO : Refactor all functions below until comment that explain where to stop

  /** Adds a vertex to a graph */
  def addVertex[Vertex](vertex: Vertex, graph: Graph[Vertex, _, _, _]) = {
    val newAdjacencyMap: Map[Vertex, OutEdgeSet[Vertex]] = addVertexToAm(vertex, graph.adjacencyMap)
    graph.copy(adjacencyMap = newAdjacencyMap)
  }

  /** Adds an edge to a simple graph */
  def addEdge[Vertex, T1, T2, T3](edge: Edge[Vertex], g: Graph[Vertex, T1, T2, T3]) = {
    val edges = g.adjacencyMap.getOrElse(edge.from, OutEdgeSet[Vertex]())
    val newEdges = edges + edge.to
    val newAdjacencyMap = addVertexToAm(edge.to, g.adjacencyMap.updated(edge.from, newEdges))
    g.copy(adjacencyMap = newAdjacencyMap)
  }

  /** Adds an edge to a weighted graph */
  def addWeightedEdge[Vertex, T1, T2](edge: Edge[Vertex], g: Graph[Vertex, T1, SimpleWeight, T2]) = {
    val edges = g.adjacencyMap.getOrElse(edge.from, OutEdgeSet[Vertex]())
    val newEdges = edges + edge.to
    val newAdjacencyMap = addVertexToAm(edge.to, g.adjacencyMap.updated(edge.from, newEdges))
    val newEdgeProperties = g.edgeProperties.updatedWith(edge)(updateWeight)
    g.copy(adjacencyMap = newAdjacencyMap, edgeProperties = newEdgeProperties)
  }

  /** Add an edge with a given weight */
  def addWeightedEdge[Vertex, T1, T2](edge: Edge[Vertex], weight: SimpleWeight, g: Graph[Vertex, T1, SimpleWeight, T2]) = {
    val edges = g.adjacencyMap.getOrElse(edge.from, OutEdgeSet[Vertex]())
    val newEdges = edges + edge.to
    val newAdjacencyMap = addVertexToAm(edge.to, g.adjacencyMap.updated(edge.from, newEdges))
    val newEdgeProperties = g.edgeProperties.updated(edge, weight)
    g.copy(adjacencyMap = newAdjacencyMap, edgeProperties = newEdgeProperties)
  }

  // ODOT : End refactoring

  /** Removes unconnected nodes */
  def removeUnconnectedVertices[Vertex, T1, T2, T3](g: Graph[Vertex, T1, T2, T3]) = {
    // Nodes that edges point to
    val destinationNodes = g.adjacencyMap.values.foldLeft(Set[Vertex]())((acc, e) => acc.union(e.toSet))
    // Nodes that are in the graph and have neither outgoing edges nor are destinations
    val lonelyNodes = (g.adjacencyMap filter {_._2.isEmpty}).keySet -- destinationNodes
    // Remove the unconnected nodes
    val newAdjacencyMap = g.adjacencyMap -- lonelyNodes
    g.copy(adjacencyMap = newAdjacencyMap)
  }

  /** Prunes edge if predicate on it's adge property is true */
  def pruneBasedOnPredicate[Vertex, T1, EdgeProperties, T3](g: Graph[Vertex, T1, EdgeProperties, T3], pred: (EdgeProperties) => Boolean) = {
    val newAdjacencyMap = g.adjacencyMap map {case (from, out) => (from, out filterNot {to => g.edgeProperties.get(Edge(from, to)) match {
      case None => false
      case Some(value) => pred(value)
    }})}
    removeUnconnectedVertices(g.copy(adjacencyMap = newAdjacencyMap))
  }

  /** Prunes the graph by removing vertices that satisfy a predicate, then remove any unconnected vertices */
  def pruneVerticesBasedOnPredicate[Vertex, VertexProperties, T2, T3](g: Graph[Vertex, VertexProperties, T2, T3], pred: (VertexProperties) => Boolean) = {
    val checkAdj = (adj: (Vertex, OutEdgeSet[Vertex])) => pred(g.vertexProperties.get(adj._1).get)
    val checkVertices = (vertex: Vertex) => pred(g.vertexProperties.get(vertex).get)

    val newAdjacencyMap = (g.adjacencyMap filterNot checkAdj).view.mapValues(outEdgeList => outEdgeList filterNot checkVertices).toMap

    removeUnconnectedVertices(g.copy(adjacencyMap = newAdjacencyMap))
  }

  /** Removes vertices from a graph, does nothing if a vertex is not in the graph */
  def pruneVertices[Vertex, VertexProperties, T2, T3](verticesToRemove: Iterable[Vertex], g: Graph[Vertex, VertexProperties, T2, T3]) = {
    val newAdjacencyMap = g.adjacencyMap.removedAll(verticesToRemove).view.mapValues(outEdgeList => outEdgeList.removedAll(verticesToRemove)).toMap
    val newVertexProperties = g.vertexProperties.removedAll(verticesToRemove)

    g.copy(adjacencyMap = newAdjacencyMap, vertexProperties = newVertexProperties)
  }

  /** Returns the transpose of the graph (all edges in opposite direction) */
  def transpose[Vertex, T1, T2, T3](g: Graph[Vertex, T1, T2, T3]): Graph[Vertex, T1, T2, T3] = {
    def reverseEdges(adj: (Vertex, OutEdgeSet[Vertex])) = {
      adj match {
        case (from, toList) => toList map {to => Edge(to, from)}
      }
    }

    // Create a base graph with the same properties as the original graph however fill it with an empty adjacency map
    val baseGraph = g.copy(adjacencyMap = Map())
    // Generate the reverse edges and create a new graph by adding them to the base graph
    val transposeGraph = (g.adjacencyMap map reverseEdges).flatten.foldLeft(baseGraph)((graph, edge) => addEdge(edge, graph))
    // Return the transposed graph
    transposeGraph
  }

  /** Returns a collection of node that have no incomming edges */
  def findStartingPoints[V, G](g: G)(implicit gt: G <:< Graph[V, _, _ , _]) = {
    val vertices = g.adjacencyMap.keySet
    val verticesWithIncomingEdge = g.adjacencyMap.values.foldLeft(Set[V]())((set, vec) => set.union(vec.toSet))

    vertices -- verticesWithIncomingEdge
  }

  def findEndingPoints[V, G](g: G)(implicit gt: G <:< Graph[V, _, _ , _]) = {
    (g.adjacencyMap filter {(kv) => kv._2.isEmpty}).keySet
  }

  /* Classes for traversal / topology functions */
  type PathMap[Vertex] = Map[Vertex, Int]
  def PathMap[Vertex](xs: (Vertex, Int)*) = Map[Vertex, Int](xs: _*)
  type Path[Vertex] = List[Vertex]
  def Path[Vertex](xs: Vertex*) = List(xs: _*)
  case class Branch[Vertex](nextVertex: Vertex, PathMap: PathMap[Vertex])
  type Branches[Vertex] = List[Branch[Vertex]]
  def Branches[Vertex](xs: Branch[Vertex]*) = List(xs: _*)
  type FinishedPathMaps[Vertex] = List[PathMap[Vertex]]
  def FinishedPathMaps[Vertex](xs: PathMap[Vertex]*) = List(xs: _*)

  def pathFromPathMap[Vertex](pm: PathMap[Vertex]): Path[Vertex] = {
    pm.toList.sortBy(_._2).map(_._1)
  }

  def listOfPathesFromFinishedPathMaps[Vertex](fpm: FinishedPathMaps[Vertex]) = {
    // For all the path maps transform them into a list of vertices sorted by position
    fpm map pathFromPathMap
  }

  def singleStrandPathes[Vertex, G](startingPoints: Set[Vertex], g: G, verbose: Boolean = false)(implicit gt: G <:< Graph[Vertex, _, _ , _]): List[Path[Vertex]] = {
    case class Acc(currentPathMap: PathMap[Vertex], position: Vertex, positionCounter: Int)

    @tailrec
    def followSingleStrand(acc: Acc, g: G): PathMap[Vertex] = {
      acc match {
        case Acc(currentPathMap, position, positionCounter) => {
          if (currentPathMap.get(position).isDefined) {
            if (verbose) {println("Loop detected")}
            // Circular edge (loop)
            currentPathMap
          } else {
            val updatedPathMap = currentPathMap.updated(position, positionCounter)
            g.adjacencyMap.get(position) match {
              // No outgoing edges list
              case None => {
                println("Warning : Edge " + position + " not in adjacency list")
                updatedPathMap
              }
              // Single outgoing edge, continue following the strand
              case Some(outEdges) if (outEdges.size == 1) => followSingleStrand(acc.copy(currentPathMap = updatedPathMap, position = outEdges.head, positionCounter = positionCounter + 1), g)
              // Either empty outgoing edges list or multiple edges
              case Some(_) => updatedPathMap
            }    
          }
        }
      }
    }

    // Generate pathes from starting points
    listOfPathesFromFinishedPathMaps(startingPoints.foldLeft(FinishedPathMaps[Vertex]())((pathMaps, start) => followSingleStrand(Acc(PathMap[Vertex](), start, 0), g) :: pathMaps).reverse)
  }

  //def singleStrandReversePathes[Vertex, G](startingPoints: Set[Vertex], g: G)(implicit gt: G <:< Graph[Vertex, _, _ , _]): List[Path[Vertex]] = {
//
  //}

  /** Will generate all non looping pathes from any starting point of a graph (nothing if circular) 
   * The pathes are maps that map vertices to positions, sorting the vertices (keys) by positions (values) will
   * give the PathMap as a list of vertices.
  */
  def nonLoopingPathes[Vertex, G](g: G)(implicit gt: G <:< Graph[Vertex, _, _ , _]): List[Path[Vertex]] = {
    nonLoopingPathesStartingAt(findStartingPoints(g), g)
  }

  /** Will generate all non looping pathes from any of the starting points given 
   * The pathes are maps that map vertices to positions, sorting the vertices (keys) by positions (values) will
   * give the PathMap as a list of vertices.
  */
  def nonLoopingPathesStartingAt[Vertex, G](startingPoints: Set[Vertex], g: G)(implicit gt: G <:< Graph[Vertex, _, _ , _]): List[Path[Vertex]] = {
    // Accumulator case class for recursion/folding
    case class Acc(startingPoints: Set[Vertex], currentPathMap: PathMap[Vertex], branches: Branches[Vertex], finishedPathMaps: FinishedPathMaps[Vertex], position: Option[Vertex], positionCounter: Int)

    // Empty accumulator
    val acc = Acc(startingPoints, Map(), List(), List(), None, 0)

    // Graph traversing recursive function
    @tailrec
    def tr(acc: Acc, g: G): FinishedPathMaps[Vertex] = {
      acc match {
        case Acc(startingPoints, currentPathMap, branches, finishedPathMaps, position, positionCounter) => {
          position match {
            case None => {
              // If the current position is undefined (meaning we either just started or finished a PathMap)
              if (!branches.isEmpty) {
                // If there is a branch to handle
                val nextBranch = branches.head
                val tailBranches = branches.tail
                // Go to this branch, i.e., update current PathMap, branches, position, positionCounter
                tr(acc.copy(currentPathMap = nextBranch.PathMap, branches = tailBranches, position = Some(nextBranch.nextVertex), positionCounter = nextBranch.PathMap.values.max + 1), g)
              } else {
                // Since there are no pending branches to explore, check if there are other starting points to follow
                if (!startingPoints.isEmpty) {
                  // Start new PathMap
                  val start = startingPoints.head
                  val nextStartingPoints = startingPoints - start
                  // Starting points now has one less elemnt, we start from the start position with a counter set at 0
                  tr(acc.copy(currentPathMap = Map(), startingPoints = nextStartingPoints, position = Some(start), positionCounter = 0), g)
                } else {
                  // If there are none we are finished, return the finished path maps
                  acc.finishedPathMaps
                }
              }
            }
            case Some(definedPosition) => {
              // Continue traversal, this "get" is the reason that the PathMap is a map and not a list, in order to have fast check (if vertex already in PathMap)
              currentPathMap.get(definedPosition) match {
                // If we already have seen this vertex we have a looping PathMap
                case Some(_) => {
                  // Append the PathMap to the finished path maps without the loop
                  val updatedFinishedPathMaps = currentPathMap :: finishedPathMaps
                  // Update the finished path maps and continue algorithm
                  tr(acc.copy(finishedPathMaps = updatedFinishedPathMaps, position = None), g)
                }
                case None => {
                  // Update current PathMap with position and counter value
                  val updatedPathMap = currentPathMap.updated(definedPosition, positionCounter)
                  g.adjacencyMap.get(definedPosition) match {
                    // If there are no outgoing edges we are in an ending point
                    case None => { // This could be refactored
                      println("Warning : Edge " + definedPosition + " not in adjacency list")
                      val updatedFinishedPathMaps = updatedPathMap :: finishedPathMaps
                      // Update the finished path maps and continue algorithm
                      tr(acc.copy(finishedPathMaps = updatedFinishedPathMaps, position = None), g)
                    }
                    case Some(outEdgeList) if (outEdgeList.isEmpty) => {
                      val updatedFinishedPathMaps = updatedPathMap :: finishedPathMaps
                      // Update the finished path maps and continue algorithm
                      tr(acc.copy(finishedPathMaps = updatedFinishedPathMaps, position = None), g)
                    }
                    case Some(outEdgeList) if (outEdgeList.size == 1) => {
                      // If there is a single outgoing edge continue the algorithm
                      tr(acc.copy(currentPathMap = updatedPathMap, position = Some(outEdgeList.head), positionCounter = positionCounter + 1), g)
                    }
                    case Some(outEdgeList) => {
                      // If there are multiple outgoing edges, update the branches and continue with the tail of the outgoing edge vector
                      val updatedBranches = outEdgeList.tail.foldLeft(branches)((branches, to) => Branch(to, updatedPathMap) :: branches)
                      // Continue the algorithm by following the head of the outgoing edge vector
                      tr(acc.copy(currentPathMap = updatedPathMap, branches = updatedBranches, position = Some(outEdgeList.head), positionCounter = positionCounter + 1) ,g)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    listOfPathesFromFinishedPathMaps(tr(acc, g))
  }

  /** This will find a loop from the starting or ending points if there is one
   * However if the loop has no starting or ending points (e.g., a circle) it will not be found because it has no starting or stopping points
   * We are not interested in loops with no start or ends here so they will not be found with this algorithm
   */
  def findLoopFromStartingAndEndingPoints[Vertex, T1, T2, T3](g: Graph[Vertex, T1, T2, T3]): Option[Path[Vertex]] = {
    // Accumulator case class for recursion/folding
    case class Acc(startingPoints: Set[Vertex], currentPathMap: PathMap[Vertex], branches: Branches[Vertex], finishedPathMaps: FinishedPathMaps[Vertex], position: Option[Vertex], positionCounter: Int)

    // Graph traversing recursive function
    @tailrec
    def tr(acc: Acc, g: Graph[Vertex, T1, T2, T3]): Option[Path[Vertex]] = {
      acc match {
        case Acc(startingPoints, currentPathMap, branches, finishedPathMaps, position, positionCounter) => {
          position match {
            case None => {
              // If the current position is undefined (meaning we either just started or finished a PathMap)
              if (!branches.isEmpty) {
                // If there is a branch to handle
                val nextBranch = branches.head
                val tailBranches = branches.tail
                // Go to this branch, i.e., update current PathMap, branches, position, positionCounter
                tr(acc.copy(currentPathMap = nextBranch.PathMap, branches = tailBranches, position = Some(nextBranch.nextVertex), positionCounter = nextBranch.PathMap.values.max + 1), g)
              } else {
                // Since there are no pending branches to explore, check if there are other starting points to follow
                if (!startingPoints.isEmpty) {
                  // Start new PathMap
                  val start = startingPoints.head
                  val nextStartingPoints = startingPoints - start
                  // Starting points now has one less elemnt, we start from the start position with a counter set at 0
                  tr(acc.copy(currentPathMap = Map(), startingPoints = nextStartingPoints, position = Some(start), positionCounter = 0), g)
                } else {
                  // If there are none we are finished, there are no loops
                  None
                }
              }
            }
            case Some(definedPosition) => {
              // Continue traversal, this "get" is the reason that the PathMap is a map and not a list, in order to have fast check (if vertex already in PathMap)
              currentPathMap.get(definedPosition) match {
                // If we already have seen this vertex we have a looping PathMap
                case Some(_) => {
                  Some(pathFromPathMap(currentPathMap) ++ List(definedPosition))
                }
                case None => {
                  // Update current PathMap with position and counter value
                  val updatedPathMap = currentPathMap.updated(definedPosition, positionCounter)
                  g.adjacencyMap.get(definedPosition) match {
                    // If there are no outgoing edges we are in an ending point
                    case None => { // This could be refactored
                      println("Warning : Edge " + definedPosition + " not in adjacency list")
                      val updatedFinishedPathMaps = updatedPathMap :: finishedPathMaps
                      // Update the finished path maps and continue algorithm
                      tr(acc.copy(finishedPathMaps = updatedFinishedPathMaps, position = None), g)
                    }
                    case Some(outEdgeList) if (outEdgeList.isEmpty) => {
                      val updatedFinishedPathMaps = updatedPathMap :: finishedPathMaps
                      // Update the finished path maps and continue algorithm
                      tr(acc.copy(finishedPathMaps = updatedFinishedPathMaps, position = None), g)
                    }
                    case Some(outEdgeList) if (outEdgeList.size == 1) => {
                      // If there is a single outgoing edge continue the algorithm
                      tr(acc.copy(currentPathMap = updatedPathMap, position = Some(outEdgeList.head), positionCounter = positionCounter + 1), g)
                    }
                    case Some(outEdgeList) => {
                      // If there are multiple outgoing edges, update the branches and continue with the tail of the outgoing edge vector
                      val updatedBranches = outEdgeList.tail.foldLeft(branches)((branches, to) => Branch(to, updatedPathMap) :: branches)
                      // Continue the algorithm by following the head of the outgoing edge vector
                      tr(acc.copy(currentPathMap = updatedPathMap, branches = updatedBranches, position = Some(outEdgeList.head), positionCounter = positionCounter + 1) ,g)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }


    // Empty accumulator
    val acc = Acc(findStartingPoints(g), Map(), List(), List(), None, 0)

    // Empty accumulator
    val transposedGraph = transpose(g)
    val transposedAcc = Acc(findStartingPoints(transposedGraph), Map(), List(), List(), None, 0)

    val possibleLoop = tr(acc, g)

    // If a loop has been found return it else check the transposed graph
    possibleLoop match {
      case Some(_) => possibleLoop
      case None => tr(transposedAcc, transposedGraph) match {
        case None => None
        case Some(value) => Some(value.reverse) // Reverse the path because on transposed graph
      }
    }
  }

  /** Returns true if the graph has a loop in the pathes starting at starting points or if there is a loop to as ending point */
  def hasLoopFromStartOrEnd[Vertex, G](g: G)(implicit gt: G <:< Graph[Vertex , _, _ , _]): Boolean = {
    findLoopFromStartingAndEndingPoints(g).isDefined
  }

  def am[G, V](g: G)(implicit gt: G <:< Graph[V, _, _ , _]): Map[V, OutEdgeSet[V]] = {
    g.adjacencyMap
  }
}
