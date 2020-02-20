package cnv

import cnv.InputArgs._
import cnv.Basic.pathExists

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object CanevasApp extends App {
  /////////////////////////
  // Input args handling //
  /////////////////////////
  
  runCanevasTasks(cnv.Tasks.tasks, args.toList)
  System.exit(0)

  ////////////////////
  // Main functions //
  ////////////////////

  /** Generate VCFs */
  def generateVCFs(options: List[OptionT]) = {
    val (pairOptions, singleOptions) = getOptionsSplit(options)

    val bamFile = pairOptions.find(_.option == "--input-bamfile") match {
      case Some(value) if (pathExists(value.value)) => value.value
      case Some(_) => println("Bam file not found !"); quit(); ""
      case None => println("Missing bamfile !"); printUsageAndQuit(); ""
    }

    val signalDir = pairOptions.find(_.option == "--signal-dir") match {
      case Some(value) if (pathExists(value.value)) => value.value
      case Some(_) => println("Signal directory not found !"); quit(); ""
      case None => println("Missing signal dir !"); printUsageAndQuit(); ""
    }

    val outputDir = pairOptions.find(_.option == "--output-dir") match {
      case Some(value) if (pathExists(value.value)) => value.value
      case Some(_) => println("Output directory not found !"); quit(); ""
      case None => println("Missing output dir !"); printUsageAndQuit(); ""
    }

    val regions = checkRegions(bamFile, pairOptions filter {_.option == "--region"} map {_.value})

    regions match {
        case List() => {
          println("Start generating VCFs for whole genome...")
          cnv.Canevas.allNiceVCFGeneration(bamFile, signalDir, outputDir)
        }
        case _ => {
          // Per region
          println("Start generating VCFs for select regions...")
          regions map {
            region => Future{
              cnv.Canevas.niceVcfGenerators(30) foreach {
                gen => gen.fun(bamFile, signalDir, region, outputDir)
              }
            }
          } foreach {Await.result(_, Duration.Inf)}
        }
      }
    println(s"Done generating VCFs, files are available in $outputDir")
  }

  /** Generate signals */
  def generateSignals(options: List[OptionT]) = {
    val (pairOptions, singleOptions) = getOptionsSplit(options)

    val bamFile = pairOptions.find(_.option == "--input-bamfile") match {
      case Some(value) if (pathExists(value.value)) => value.value
      case Some(_) => println("Bam file not found !"); quit(); ""
      case None => println("Missing bamfile !"); printUsageAndQuit(); ""
    }

    val outputDir = pairOptions.find(_.option == "--output-dir") match {
      case Some(value) if (pathExists(value.value)) => value.value
      case Some(_) => println("Output directory not found !"); quit(); ""
      case None => println("Missing output dir !"); printUsageAndQuit(); ""
    }

    val regions = checkRegions(bamFile, pairOptions filter {_.option == "--region"} map {_.value})

    regions match {
      case List() => {
        println("Start extracting all signals for whole genome...")
        cnv.Canevas.allSignalGeneration(bamFile, outputDir)
      }
      case _ => {
        // Per region
        regions foreach {
          println("Start extracting all signals for select regions...")
          region => cnv.Canevas.allSignalGenerationChr(bamFile, outputDir, region)
        }
      }
    }
    println(s"Done generating signals, files are available in $outputDir")
  }

  /** Prints the regions inside a bam file */
  def taskRegions(options: List[OptionT]) = {
    // Get the bamfile
    val bamOptions = options filter {
      _ match {
        case OptionPair(option, value) => option == "--input-bamfile"
        case _ => false
      }
    }

    if (bamOptions.isEmpty) {
      printUsageAndQuit()
    } else {
      bamOptions foreach {
        _ match {
          case OptionPair(_, value) => {
            println(s"File : $value contains regions :")
            try {
              println("(" + cnv.SamToolBox.getRegions(value).map(_._1).toList.sorted.mkString(", ") + ")")
            } catch {
              case e: htsjdk.samtools.util.RuntimeIOException => println(s"File $value not found")
            }
          }
          case _ => {}
        }
      }
    }
  }

  def reIdTask(options: List[OptionT]) = {
    val (pairOptions, singleOptions) = getOptionsSplit(options)

    val vcfFile = pairOptions.find(_.option == "--vcf-file") match {
      case Some(value) if (pathExists(value.value)) => value.value
      case Some(_) => println("VCF file not found !"); quit(); ""
      case None => println("Missing VCF file !"); printUsageAndQuit(); ""
    }

    val outVcfFile = pairOptions.find(_.option == "--vcf-file-out").get.value

    val idPrefix = pairOptions.find(_.option == "--prefix") match {
      case Some(value) => value.value
      case None => ""
    }

    println(s"File $vcfFile ID fields will be updated as $idPrefix N in file $outVcfFile")
    cnv.VcfToolBox.reIdVcfFile(vcfFile, outVcfFile, idPrefix)
  }

  //////////////////////
  // Helper functions //
  //////////////////////

  def checkRegions(bamFile: String, regions: List[String]) = {
    val availableRegions = (cnv.SamToolBox.getRegions(bamFile) map {_._1}).toSet
    val unknownRegions = regions.toSet -- availableRegions
    if (!unknownRegions.isEmpty) {
      println("Regions [" + unknownRegions.mkString(",") + "] are not available for analysis")
      println("Possible regions are [" + availableRegions.mkString(",") + "]")
    }
    regions.toSet -- unknownRegions
  }

  def quit() = {
    System.exit(1)
  }

  /** Prints the usage string and quits */
  def printUsageAndQuit() = {
    println(cnv.Strings.usage)
    quit()
  }
}