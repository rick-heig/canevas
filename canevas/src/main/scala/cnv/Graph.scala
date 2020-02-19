package cnv
import scala.annotation.tailrec

object Graph {

  case class Edge[Vertex](from: Vertex, to: Vertex)
  type OutEdgeList[Vertex] = Vector[Vertex]
  def OutEdgeList[Vertex](xs: Vertex*) = Vector[Vertex](xs: _*)

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

  abstract sealed class Graph[Vertex, VertexProperties, EdgeProperties, GraphProperties] {
    def adjacencyMap: Map[Vertex, OutEdgeList[Vertex]]
    def vertexProperties: Map[Vertex, VertexProperties]
    def edgeProperties: Map[Edge[Vertex], EdgeProperties]
    def graphProperties: GraphProperties
  }

  case class SimpleGraph[Vertex, VertexProperties, EdgeProperties, GraphProperties](adjacencyMap: Map[Vertex, OutEdgeList[Vertex]], vertexProperties: Map[Vertex, VertexProperties], edgeProperties: Map[Edge[Vertex], EdgeProperties], graphProperties: GraphProperties) extends Graph[Vertex, VertexProperties, EdgeProperties, GraphProperties]
  case class WeightedGraph[Vertex, VertexProperties, EdgeProperties <: Weighted, GraphProperties](adjacencyMap: Map[Vertex, OutEdgeList[Vertex]], vertexProperties: Map[Vertex, VertexProperties], edgeProperties: Map[Edge[Vertex], EdgeProperties], graphProperties: GraphProperties) extends Graph[Vertex, VertexProperties, EdgeProperties, GraphProperties]
  //def WeightedGraph[Vertex, VertexProperties, EdgeProperties <: Weighted, GraphProperties](adjacencyMap: Map[Vertex, OutEdgeList[Vertex]], vertexProperties: Map[Vertex, VertexProperties], edgeProperties: Map[Edge[Vertex], EdgeProperties], graphProperties: GraphProperties) = Graph[Vertex, VertexProperties, EdgeProperties, GraphProperties](adjacencyMap, vertexProperties, edgeProperties, graphProperties)

  /** Creates an empty graph */
  def createEmptyGraph[Vertex, VertexProperties, EdgeProperties, GraphProperties](gp: GraphProperties) = {
    SimpleGraph[Vertex, VertexProperties, EdgeProperties, GraphProperties](Map[Vertex, OutEdgeList[Vertex]](), Map[Vertex, VertexProperties](), Map[Edge[Vertex], EdgeProperties](), gp)
  }

  /** Creates an empty graph with a string graph property (e.g., name of the graph) */
  def createEmptyGraph(gp: String) = {
    createEmptyGraph[Int, String, Int, String](gp)
  }

  def createEmptyWeightedEdgeGraph(gp: String) = {
    WeightedGraph[Int, String, SimpleWeight, String](Map(), Map(), Map(), gp)
  }

  /** Adds a vertex to an adjacency map */
  def addVertexToAm[Vertex](vertex: Vertex, adjacencyMap: Map[Vertex, OutEdgeList[Vertex]]) = {
    adjacencyMap.updated(vertex, adjacencyMap.getOrElse(vertex, OutEdgeList[Vertex]()))
  }

  // TODO : Refactor all functions below until comment that explain where to stop

  /** Adds a vertex to a graph */
  //def addVertex[Vertex, G](vertex: Vertex, graph: G)(implicit gt: G <:< Graph[Vertex, _, _ , _]): G = {
  //  val newAdjacencyMap: Map[Vertex, OutEdgeList[Vertex]] = addVertexToAm(vertex, graph.adjacencyMap)
  //  graph.copy(adjacencyMap = newAdjacencyMap)
  //}

  def addVertex[Vertex, G <: Graph[Vertex, _, _ , _]](vertex: Vertex, graph: G): G = {
    val newAdjMap: Map[Vertex, OutEdgeList[Vertex]] = addVertexToAm(vertex, graph.adjacencyMap)
    graph match {
      case g: SimpleGraph[Vertex, _, _, _] => g.copy(adjacencyMap = newAdjMap).asInstanceOf[G]
      case g: WeightedGraph[Vertex, _, _, _] => g.copy(adjacencyMap = newAdjMap).asInstanceOf[G]
    }
  }

  /** Adds an edge to a weighted graph */
  def addEdge[Vertex, T1, T2](edge: Edge[Vertex], g: WeightedGraph[Vertex, T1, SimpleWeight, T2]) = {
    val edges = g.adjacencyMap.getOrElse(edge.from, OutEdgeList[Vertex]())
    val newEdges = edges.appended(edge.to)
    val newAdjacencyMap = addVertexToAm(edge.to, g.adjacencyMap.updated(edge.from, newEdges))
    val newEdgeProperties = g.edgeProperties.updatedWith(edge)(updateWeight)
    g.copy(adjacencyMap = newAdjacencyMap, edgeProperties = newEdgeProperties)
  }

  /** Add an edge with a given weight */
  def addEdge[Vertex, T1, T2](edge: Edge[Vertex], weight: SimpleWeight, g: WeightedGraph[Vertex, T1, SimpleWeight, T2]) = {
    val edges = g.adjacencyMap.getOrElse(edge.from, OutEdgeList[Vertex]())
    val newEdges = edges.appended(edge.to)
    val newAdjacencyMap = addVertexToAm(edge.to, g.adjacencyMap.updated(edge.from, newEdges))
    val newEdgeProperties = g.edgeProperties.updated(edge, weight)
    g.copy(adjacencyMap = newAdjacencyMap, edgeProperties = newEdgeProperties)
  }

  /** Adds an edge to a simple graph */
  def addEdge[Vertex, T1, T2, T3](edge: Edge[Vertex], g: SimpleGraph[Vertex, T1, T2, T3]) = {
    val edges = g.adjacencyMap.getOrElse(edge.from, OutEdgeList[Vertex]())
    val newEdges = edges.appended(edge.to)
    val newAdjacencyMap = addVertexToAm(edge.to, g.adjacencyMap.updated(edge.from, newEdges))
    g.copy(adjacencyMap = newAdjacencyMap)
  }

  // ODOT : End refactoring

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

  def listOfPathesFromFinishedPathMaps[Vertex](fpm: FinishedPathMaps[Vertex]) = {
    // For all the path maps transform them into a list of vertices sorted by position
    fpm map {_.toList.sortBy(_._2).map(_._1)}
  }

  def singleStrandPathes[Vertex, G](startingPoints: Set[Vertex], g: G)(implicit gt: G <:< Graph[Vertex, _, _ , _]): List[Path[Vertex]] = {
    case class Acc(currentPathMap: PathMap[Vertex], position: Vertex, positionCounter: Int)

    @tailrec
    def followSingleStrand(acc: Acc, g: G): PathMap[Vertex] = {
      acc match {
        case Acc(currentPathMap, position, positionCounter) => {
          if (currentPathMap.get(position).isDefined) {
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

  def am[G, V](g: G)(implicit gt: G <:< Graph[V, _, _ , _]): Map[V, OutEdgeList[V]] = {
    g.adjacencyMap
  }
}
