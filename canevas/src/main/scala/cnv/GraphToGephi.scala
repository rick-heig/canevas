package cnv

import cnv.Things._
import cnv.Assembly._
import cnv.GraphG._
import scalaj.http._

object GraphToGephi {

  def sendGraphToGephiG[V,VP, EP, GP](g: Graph[V, VP, EP, GP], connection: GephiConnection, workspace: String = "workspace1", verbose: Boolean = false) = {
    val address = "http://localhost:" + connection.socket.getPort().toString() + "/" + workspace + "?operation=updateGraph"

    // Send the nodes
    g.adjacencyMap foreach {
      adjacency => {
        adjacency match {
          case (from, _) => {
            // Add each node
            val response = Http(address).postData("{\"an\":{\"" + from + "\":{\"label\":\"" + from + "\"}}}").asString
            if (verbose) {println(response)}
          }
        }
      }
    }
    g.adjacencyMap foreach {
      adjacency => {
        adjacency match {
          case (from, outEdges) => {
            // Add each node
            outEdges foreach {
              case to => {
                val weight = g.edgeProperties.get(Edge(from, to)) match {
                  case None => 0
                  case Some(SimpleWeight(weight)) => weight
                  case Some(_) => 0
                }
                val response = Http(address).postData("{\"ae\":{\"" + from.toString + to.toString + "\":{\"source\":\"" + from.toString + "\",\"target\":\"" + to.toString + "\",\"directed\":true,\"weight\":" + weight.toString + "}}}").asString
                if (verbose) {println(response)}
              }
            }
          }
        }
      }
    }
  }

  def sendGraphToGephi(g: GraphWeAsAdjacencyMap, connection: GephiConnection, workspace: String = "workspace1") = {
    val address = "http://localhost:" + connection.socket.getPort().toString() + "/" + workspace + "?operation=updateGraph"

    // Send the nodes
    g foreach {
      adjacencyList => {
        adjacencyList match {
          case (node, weightedEdges) => {
            // Add each node
            println(Http(address).postData("{\"an\":{\"" + node + "\":{\"label\":\"" + node + "\"}}}").asString)
            weightedEdges foreach {
              case (toNode, weight) => {
                //println(Http(address).postData("{\"ae\":{\"" + node + toNode + "\":{\"source\":\"" + node + "\",\"target\":\"" + toNode + "\",\"directed\":true,\"weight\":" + weight + "}}}").asString)
              }
            }
          }
        }
      }
    }
    g foreach {
      adjacencyList => {
        adjacencyList match {
          case (node, weightedEdges) => {
            // Add each node
            //println(Http(address).postData("{\"an\":{\"" + node + "\":{\"label\":\"" + node + "\"}}}").asString)
            weightedEdges foreach {
              case (toNode, weight) => {
                println(Http(address).postData("{\"ae\":{\"" + node + toNode + "\":{\"source\":\"" + node + "\",\"target\":\"" + toNode + "\",\"directed\":true,\"weight\":" + weight + "}}}").asString)
              }
            }
          }
        }
      }
    }
  }

}