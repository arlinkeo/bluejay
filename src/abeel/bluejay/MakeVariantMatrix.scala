package abeel.bluejay

import atk.util.Tool
import java.io.File
import atk.io.PatternFileFilter
import atk.compbio.vcf.VCFFile
import java.io.PrintWriter
import atk.util.TimeInterval

object MakeVariantMatrix extends Tool {

 case class Config(vcf: File = null, outputPrefix: String = "")

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("java -jar bluejay.jar variant-matrix") {
      opt[File]('v', "vcf") action { (x, c) => c.copy(vcf = x) } text ("File containing a list of VCF files")
      opt[String]('o',"output") action{ (x, c) => c.copy(outputPrefix = x) } text ("Output prefix")
    }

    parser.parse(args, Config()).map { config =>
      matrixify(config);
    }
  }

  def matrixify(config: Config) {
    val files = tLines(config.vcf).map(f=>new File(f))

    val listOfVariants = files.foldLeft(Set[String]())((r, c) => r ++ {
      println("Pre-processing " + c)
      val lines = VCFFile(c)

      lines.filter(_.pass).map(vl => vl.pos + "_" + vl.ref + "_" + vl.alt)

    })
    println("Sorting")
    val sorted = listOfVariants.toList.sortBy(_.split("_")(0).toInt)
    val pwx = new PrintWriter(config.outputPrefix + "variantlist.txt")
    pwx.println(generatorInfo)
    pwx.println(sorted.mkString("\n"))
    pwx.println("# unique variants: " + sorted.size)
    pwx.close

    val variants = tLines(config.outputPrefix + "variantlist.txt")
  
    val pw = new PrintWriter(config.outputPrefix + "variantmatrix.txt")
    pw.println("$$\t" + variants.mkString("\t"))
    var idx=0
    val startTime=System.currentTimeMillis()
    files.map(f => {
      idx+=1
      val elapsed=System.currentTimeMillis()-startTime+1.0
      println("Processing: " + f+ "\n\t"+idx+"/"+files.size+"\t"+new TimeInterval(elapsed.toLong)+"\tETA: "+new TimeInterval((((elapsed/idx)*files.size)-elapsed).toLong))
      val lines = VCFFile(f)
      val g = f.toString()
      val arr = Array.ofDim[Int](variants.length)
      lines.toList.filter(_.pass).map(vl => {
        val id = vl.pos + "_" + vl.ref + "_" + vl.alt
        val idx = variants.indexOf(id)
        arr(idx) = 1
      })
      pw.println(g + "\t" + arr.mkString("\t"))

    })
    pw.close

  }

}