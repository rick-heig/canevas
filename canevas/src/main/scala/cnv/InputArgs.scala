package cnv

import cnv.CanevasApp.printUsageAndQuit

object InputArgs {

  case class MainOption(option: String, description: String)
  val EMPTY_MAIN_OPTION = MainOption("", "")

  val mainOptions = Set(
    MainOption("signal", "Signal extraction and generation"),
    MainOption("vcf", "VCF generation"),
    MainOption("regions", "Shows regions from BAM file")
  )

  case class OptionEntry(option: String, isPair: Boolean, docString: String, shortOption: String = "")

  // These are the options accepted by the program
  val options = Set(
    OptionEntry("--help", false, "Prints help", "-h"),
    OptionEntry("--input-bamfile", true, "Defines input bamfile", "-b"),
    OptionEntry("--output-dir", true, "Defines the output directory", "-o"),
    OptionEntry("--version", false, "Shows version", "-v"),
    OptionEntry("--signal-dir", true, "Signal directory")
  )

  // Short to long option conversion map
  val shortOptionsToLong = Map((options filter {_.shortOption != ""} map {o => (o.shortOption, o.option)}).toSeq: _*)
  // The bimap of the above
  val longOptionsToShort = Map(shortOptionsToLong.toList map {case (e1, e2) => (e2, e1)}: _*)

  /** Changes the option to the original option version */
  def normalizeOption(option: String) = {
    shortOptionsToLong.getOrElse(option, option)
  }

  trait Option
  case class OptionPair(option: String, value: String) extends Option
  case class OptionSingle(option: String) extends Option
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

  def getOptionsSplit(optionList: List[Option]) = {
    optionList.partition(_ match {
      case OptionPair(_, _) => true
      case OptionSingle(_) => false
    }).asInstanceOf[(List[OptionPair], List[OptionSingle])]
  }

  /** Extracts options from the args array */
  def extractOptions(args: Array[String]) = {
    def nextOption(accOptionList: List[Option], list: List[String]): List[Option] = {
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
}