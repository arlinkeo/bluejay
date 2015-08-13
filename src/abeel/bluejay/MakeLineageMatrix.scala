package abeel.bluejay

import atk.util.Tool
import java.io.PrintWriter
import java.io.File

object MakeLineageMatrix extends Tool {

  case class Config(input: File = null, output: File = null, column:Int=3)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("java -jar bluejay.jar lineage-matrix") {
      opt[File]('i', "input") required () action { (x, c) => c.copy(input = x) } text ("Input lineage information. Which is the output of the spoligotype figure.")
      opt[File]('o', "output") action { (x, c) => c.copy(output = x) } text ("Output file. Default = input+.matrix")
      opt[Int]('c',"column") action { (x, c) => c.copy(column = x) } text ("Column to extract")

    }

    parser.parse(args, Config()).map { config =>
      matrixify(config);
    }
  }
  def matrixify(config: Config) {
    val lines = tLines(config.input)

    val lineages1 = (lines.map { l =>
      val arr = l.split("\t")

      val lin = arr(config.column)
      lin
    }).toSet.toList
    
    val lineages=lineages1.sortBy(_.toString())
    
    
    val output = if (config.output == null) new File(config.input + ".matrix") else config.output
    if (!output.getAbsoluteFile().getParentFile().exists())
      output.getAbsoluteFile().getParentFile().mkdirs()
    val pw = new PrintWriter(output)
    pw.println("$$\t" + lineages.mkString("\t"))
    val count = Array.ofDim[Int](lineages.size)
    lines.map { l =>
      val arr = l.split("\t")
      val g = arr(0)
      val lin = arr(config.column)
      val out = Array.ofDim[Int](lineages.size)
      val idx = lineages.indexOf(lin)
      if (idx >= 0) {
        out(idx) = 1
        count(idx) += 1
      }
      pw.println(g + "\t" + out.mkString("\t"))
    }
    pw.println("#Count\t" + count.mkString("\t"))
    pw.close

  }

}