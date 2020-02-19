package cnv
import java.net.Socket
import java.io._
import java.lang.StringBuilder
import scala.annotation.tailrec

object Things {

  case class IgvConnection(socket: Socket, out: PrintWriter, in: BufferedReader)

  def connectToIGV(port: Int = 60151) = {
    val socket = new Socket("127.0.0.1", port)
    val out = new PrintWriter(socket.getOutputStream(), true)
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))

    IgvConnection(socket, out, in)
  }

  def sendCommandToIGV(command: String, connection: IgvConnection) = {
    connection.out.println(command)
    val answer = connection.in.readLine()
  }

  // TODO : Refactor the code below with the code above, since both basically do the same thing
  case class GephiConnection(socket: Socket, out: PrintWriter, in: BufferedReader)

  def connectToGephi(port: Int = 60150) = {
    val socket = new Socket("127.0.0.1", port)
    val out = new PrintWriter(socket.getOutputStream(), true)
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))

    GephiConnection(socket, out, in)
  }

  def sendCommandToGephi(command: String, connection: GephiConnection) = {
    connection.out.println()

    @tailrec
    def readResponse(connection: GephiConnection, acc: StringBuilder): String = {
      connection.in.readLine match {
        case line => {
          acc.append(line + "\n")
          if (line.contains("</html>")) {
            acc.toString
          } else {
            readResponse(connection, acc)
          }
        }
      }
    }
    val answer = readResponse(connection, new StringBuilder)
  }

}