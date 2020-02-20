package cnv

import cnv.CanevasApp.printUsageAndQuit
import cnv.Tasks._
import scala.collection.immutable.Nil

object InputArgs {

  case class MainOption(option: String, description: String)
  val EMPTY_MAIN_OPTION = MainOption("", "")

  val mainOptions = tasks map {t => MainOption(t.name, t.description)}

  case class OptionEntry(option: String, isPair: Boolean, docString: String, shortOption: String = "")
  object OptionEntry {
    def toString(oe: OptionEntry) = {
      oe match {
        case OptionEntry(option, isPair, docString, shortOption) => {
          val head = option +
          (if (shortOption != "") {", " + shortOption} else {""}) +
          (if (isPair) {" <value>"} else {""})

          val desc = " " * (40-head.size) + docString

          head + desc
        }
      }
    }
  }

  // These are the options accepted by the program
  val options = Set(
    OptionEntry("--help", false, "Prints help", "-h"),
    regionOption,
    bamFileOption,
    outputDirOption,
    signalDirOption
  )

  // Short to long option conversion map
  val shortOptionsToLong = Map((options filter {_.shortOption != ""} map {o => (o.shortOption, o.option)}).toSeq: _*)
  // The bimap of the above
  val longOptionsToShort = Map(shortOptionsToLong.toList map {case (e1, e2) => (e2, e1)}: _*)

  /** Changes the option to the original option version */
  def normalizeOption(option: String) = {
    shortOptionsToLong.getOrElse(option, option)
  }

  trait OptionT {
    val option: String
  }
  case class OptionPair(option: String, value: String) extends OptionT
  case class OptionSingle(option: String) extends OptionT
  trait Documented {
    def docString: String = ""
  }

  private val (sops, pops) = options partition {!_.isPair}

  /** Set of all acceptable first position options */
  val firstOptions = mainOptions map {_.option}
  /** Set with all acceptable single option strings */
  val singleOptions = (sops map {_.option}) ++ (sops map {opt => longOptionsToShort.getOrElse(opt.option, opt.option)})
  /** Set with all acceptable pair option strings */
  val pairOptions = (pops map {_.option}) ++ (pops map {opt => longOptionsToShort.getOrElse(opt.option, opt.option)})

  def getOptionsSplit(optionList: List[OptionT]) = {
    optionList.partition(_ match {
      case OptionPair(_, _) => true
      case OptionSingle(_) => false
    }).asInstanceOf[(List[OptionPair], List[OptionSingle])]
  }

  def getOptionsSplit(optionSet: Set[OptionEntry]) = {
    val (pairs, singles) = optionSet.toList.partition(_.isPair)
    val popts = (pairs map {_.option}) ++ (pairs map {_.shortOption} filter {_ != ""})
    val sopts = (singles map {_.option}) ++ (singles map {_.shortOption} filter {_ != ""})
    (popts, sopts)
  }

  /** Extracts options from the args array */
  def extractOptions(args: Array[String]) = {
    def nextOption(accOptionList: List[OptionT], list: List[String]): List[OptionT] = {
      list match {
        case Nil => {
          accOptionList.reverse
        }
        case option :: tail if (singleOptions.contains(option)) => {
          nextOption(OptionSingle(normalizeOption(option)) :: accOptionList, tail)
        }
        case option :: value :: tail if (pairOptions.contains(option)) => {
          nextOption(OptionPair(normalizeOption(option), value) :: accOptionList, tail)
        }
        case _ => {
          println("Unrecognized option !")
          List()
        }
      }
    }

    args.toList match {
      case head :: tail if (firstOptions.contains(head)) => (MainOption(head, ""), nextOption(List(), tail))
      case _ => (EMPTY_MAIN_OPTION, List())
    }
  }

  def extractArguments(args: List[String], requiredOptions: Set[OptionEntry], optionalOptions: Set[OptionEntry]): Option[List[OptionT]] = {
    val acceptedArguments = (requiredOptions ++ optionalOptions)
    //println(requiredOptions)
    val requiredOptionStrings = requiredOptions map {_.option}
    val (pairOptions, singleOptions) = getOptionsSplit(acceptedArguments)

    def nextOption(accOptionList: List[OptionT], list: List[String]): List[OptionT] = {
      list match {
        case Nil => {
          accOptionList.reverse
        }
        case option :: tail if (singleOptions.contains(option)) => {
          nextOption(OptionSingle(normalizeOption(option)) :: accOptionList, tail)
        }
        case option :: value :: tail if (pairOptions.contains(option)) => {
          nextOption(OptionPair(normalizeOption(option), value) :: accOptionList, tail)
        }
        case option :: tail => {
          println(s"Unrecognized option ! $option")
          List()
        }
      }
    }

    //println(requiredOptionStrings)
    val options = nextOption(List(), args)
    val missingOptions = requiredOptionStrings -- (options map {_.option}).toSet

    if (!missingOptions.isEmpty) {
      println("The following options are missing : " + missingOptions.mkString(" ") + "\n")
      None
    } else {
      Some(options)
    }
  }

  def runCanevasTasks(tasks: List[TaskT], args: List[String], parentTask: Option[TaskWithSubTasks] = None): Unit = {
    def printHelp = {
      parentTask match {
        case Some(value) => println(tasksToDocString(value.subTasks))
        case None => println(tasksToDocString(tasks))
      }
    }

    args match {
      case head :: next => {
        tasks.find(_.name == head) match {
          case Some(value) => value match {
            case task: Task => {
              task match {
                case Task(_, _, requiredOptions, optionalOptions, function) => {
                  extractArguments(next, requiredOptions, optionalOptions) match {
                    case Some(value) => function(value)
                    case None => println(taskToDocString(task))
                  }
                }
              }
            }
            case task: TaskWithSubTasks => runCanevasTasks(task.subTasks, next, Some(task))
          }
          case None => printHelp
        }
      }
      case Nil => printHelp
    }
  }
}