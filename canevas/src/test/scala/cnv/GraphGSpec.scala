package cnv

import cnv.GraphG._

import org.scalatest._

class GraphGSpec extends FlatSpec with Matchers {

  /** Test if cnv.Basic.getLines is able to read files */
  "Loops" should "be found only from starting and ending points" in {

    /*
     *     tg : Test graph, has a loop but no starting/ending points (circular)
     * 
     *     A->B->C-+
     *     ^       |
     *     |       |
     *     +-------+
     * 
     *     tg2 : Test Graph 2, has a loop that goes into an ending point
     * 
     *        D
     *        ^
     *        |
     *     A->B->C-+
     *     ^       |
     *     |       |
     *     +-------+
     * 
     *     tg3 : Test Graph 3, Has a loop when traversed from a starting point
     * 
     *           D
     *           |
     *           v
     *     A->B->C-+
     *     ^       |
     *     |       |
     *     +-------+
     * 
     */

    val eg = createEmptyGraph[String, String, String, String]("test_graph")
    val triangleList = List(("A", "B"), ("B", "C"), ("C", "A"))
    val tg = triangleList.foldLeft(eg)((g, e) => addEdge(Edge(e._1, e._2), g))
    val tg2 = addEdge(Edge("B", "D"), tg)
    val tg3 = addEdge(Edge("D", "C") ,tg)

    findLoopFromStartingAndEndingPoints(tg) should be (None)
    findLoopFromStartingAndEndingPoints(tg2) should be (Some(List("B", "C", "A", "B", "D")))
    findLoopFromStartingAndEndingPoints(tg3) should be (Some(List("D", "C", "A", "B", "C")))
  }


}