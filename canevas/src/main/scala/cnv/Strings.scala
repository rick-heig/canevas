package cnv

import cnv.InputArgs._
import cnv.Tasks._

/** Holds string constants for the Canevas App */
object Strings {

  val usage =
  """Usage: canevas [args]""" +
  "\n" +
  tasksToDocString(tasks) +
  "\n" +
  "\n" +
  "The software application is still in development, not all features are directly available from here, missing features can be accessed through an interactive Scala console"
}