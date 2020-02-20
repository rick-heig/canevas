package cnv

import cnv.InputArgs._

object Tasks {

  val bamFileOption = OptionEntry("--input-bamfile", true, "Defines the input bamfile", "-b")
  val outputDirOption = OptionEntry("--output-dir", true, "Defines the output directory", "-o")
  val regionOption = OptionEntry("--region", true, "Defines a region of the reference", "-r")
  val signalDirOption = OptionEntry("--signal-dir", true, "Path to the Signal directory")
  val truthVCFOption = OptionEntry("--truth-vcf", true, "Path to the truth set VCF", "-t")
  val candidateVCFOption = OptionEntry("--candidate-vcf", true, "Path to the candidate set VCF", "-c")
  val svTypeOption = OptionEntry("--sv-type", true, "SV type to be included", "-s")
  val generateDiffVCFs = OptionEntry("--generate-sub-vcfs", false, "Generates found, missed, false vcf sets", "")
  val vcfFileOption = OptionEntry("--vcf-file", true, "The VCF file in which to replace the IDs", "-fi")
  val vcfFileOutOption = OptionEntry("--vcf-file-out", true, "The output VCF file with new IDs", "-fo")
  val idPrefixOption = OptionEntry("--prefix", true, "Prefix to be appended to the ID (ID is a number)", "-p")

  // Tasks can be defined this way and added to the tasks list below
  val vcfReIdTask =
    Task (
      name = "re-identify",
      description = "Replaces the IDs in a VCF file",
      requiredOptions = Set(vcfFileOption, vcfFileOutOption),
      optionalOptions = Set(idPrefixOption),
      function = cnv.CanevasApp.reIdTask
    )

  // Not yet ready
  val vcfCompareTask = 
    Task(
      name = "compare",
      description = "Compares a candidate VCF to a truth set VCF",
      requiredOptions = Set(truthVCFOption, candidateVCFOption),
      optionalOptions = Set(svTypeOption, regionOption, generateDiffVCFs),
      function = (_) => println("In development")
    )

  // Not yet ready
  val predictLowCoverageTask =
    Task(
      name = "predict-low-coverage",
      description = "Predicts low coverage regions",
      requiredOptions = Set(signalDirOption, outputDirOption),
      optionalOptions = Set(regionOption),
      function = (_) => println("In development")
    )

  /////////////////////////////////////////////////////////////////////////
  // Tasks in the list below will appears as option in the software menu //
  /////////////////////////////////////////////////////////////////////////

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
        ),
        vcfReIdTask
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