package cnv

import cnv.InputArgs._

/** Holds string constants for the Canevas App */
object Strings {

  val usage =
  """Usage: canevas [args]""" +
  "\n" +
  "The first argument should be one of the following [" + (mainOptions map {_.option}).mkString("|") + "]\n" +
  "\n" +
  (mainOptions map {o => o.option + "\t\t" + o.description}).toList.sorted.mkString("\n") +
  "\n" +
  "\n" +
  "The software application is still in development, not all features are directly available from here, missing features can be accessed through an interactive Scala console"
}