package abeel.bluejay

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import atk.util.Tool

object TransposeMatrix extends Tool {

  case class Config(variantMatrix: File = null)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("java -jar bluejay.jar transpose-matrix") {
      opt[File]("variant") required () action { (x, c) => c.copy(variantMatrix = x) } text ("Input file with the variant matrix") validate { x => if (x.exists) success else failure("Variant matrix file not found: " + x) }
    }

    parser.parse(args, Config()).map { config =>

      /** Elapsed time function */
      def time[R](block: => R): R = {
        val t0 = System.currentTimeMillis()
        val result = block // call-by-name
        val t1 = System.currentTimeMillis()
        println("Elapsed time: " + (t1 - t0) + "ms")
        result
      }

      val uniqueVariants = tLines(config.variantMatrix)(0).split("\t").drop(1).length
      println(uniqueVariants + " Unique variants")

      val pw = new PrintWriter(new File(config.variantMatrix.getName.dropRight(4) + "_transposed.txt"))
      time {
        val rows = Matrix.rowsFromFile(config.variantMatrix).rowLabels
        pw.print("$$\t")
        pw.print(rows.mkString("\t"))

        for (variant <- 1 until uniqueVariants by 10000) {
          val range = if (variant + 10000 > uniqueVariants) (variant to (uniqueVariants)).toList else (variant until (variant + 10000)).toList
          println("Range " + range(0) + "-" + range.last)
          val submatrix = Matrix.fromFile(config.variantMatrix, range)
          for (i <- range) {
            val idx = if (i % 10000 == 0) 10000 else i % 10000
            pw.println
            pw.print(submatrix.tmpColMatrix(idx).mkString("\t"))
          }
        }
      }
      pw.close

      /** Print used memory */
      val mb = 1024 * 1024
      val runtime = Runtime.getRuntime
      println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb + " Mb")
      println("** Free Memory:  " + runtime.freeMemory / mb + " Mb")
      println("** Total Memory: " + runtime.totalMemory / mb + " Mb")
      println("** Max Memory:   " + runtime.maxMemory / mb + " Mb")

    }

  }
}