package cnv

import cnv.InputArgs._

object Tasks {

  val bamFileOption = OptionEntry("--input-bamfile", true, "Defines the input bamfile", "-b")
  val outputDirOption = OptionEntry("--output-dir", true, "Defines the output directory", "-o")
  val regionOption = OptionEntry("--region", true, "Defines a region of the reference", "-r")
  val signalDirOption = OptionEntry("--signal-dir", true, "Path to the Signal directory")

  /** This defines the main software tasks */
  val tasks: List[TaskT] = List(
    Task(
      name = "version",
      description = "Shows version",
      requiredOptions = Set(),
      optionalOptions = Set(),
      function = _ => println("Canevas version : " + getClass.getPackage.getImplementationVersion())
    ),
    TaskWithSubTasks(
      name = "signal",
      description = "Signal extraction and generation",
      subTasks = List(
        Task(
          name = "extract",
          description = "Extracts signals from a bamfile",
          requiredOptions = Set(bamFileOption, outputDirOption),
          optionalOptions = Set(regionOption),
          function = cnv.CanevasApp.generateSignals
        )
      )
    ),
    TaskWithSubTasks(
      name = "vcf",
      description = "VCF generation",
      subTasks = List(
        Task(
          name = "generate",
          description = "Generates VCFs from signals",
          requiredOptions = Set(bamFileOption, outputDirOption, signalDirOption),
          optionalOptions = Set(regionOption),
          function = cnv.CanevasApp.generateVCFs
        )
      )
    ),
    Task(
      name = "regions",
      description = "Shows regions from BAM file",
      requiredOptions = Set(bamFileOption),
      optionalOptions = Set(),
      function = cnv.CanevasApp.taskRegions
    )
  )

  trait TaskT {
    val name: String
    val description: String
    val help: String = ""
  }
  case class Task(
    name: String,
    description: String,
    requiredOptions: Set[OptionEntry],
    optionalOptions: Set[OptionEntry],
    function: (List[OptionT]) => Any
  ) extends TaskT
  case class TaskWithSubTasks(
    name: String,
    description: String,
    subTasks: List[TaskT]
  ) extends TaskT

  def tasksToDocString(tasks: List[TaskT]) = {
    "The first argument should be one of the following [" + (tasks map {t => t.name}).mkString("|") + "]\n" +
    "\n" +
    (tasks map {t => t.name + "\t\t" + t.description}).toList.sorted.mkString("\n")
  }

  def taskToDocString(task: Task) = {
    task match {
      case Task(name, description, requiredOptions, optionalOptions, _) => {
        name + " " * (40-name.size) + description + "\n" +
        "Required arguments : \n" + 
        requiredOptions.map(OptionEntry.toString(_)).mkString("\n") +
        (if (!optionalOptions.isEmpty) {
          "\nOptional arguments : \n" +
          optionalOptions.map(OptionEntry.toString(_)).mkString("\n")
        } else {
          ""
        })
      }
    }
  }
}