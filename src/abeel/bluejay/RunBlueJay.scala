package abeel.bluejay

import java.io.PrintWriter

import atk.util.Tool
import be.abeel.util.FrequencyMapUtils
import be.abeel.util.FrequencyMap
import java.text.NumberFormat
import java.util.Locale
import atk.util.BitSetTools
import scala.collection.BitSet
import java.io.File
import scala.collection.JavaConversions._

object RunBlueJay extends Tool with BitSetTools {

  case class Config(lineageMatrix: File = null, variantMatrix: File = null, val debug: Boolean = false, outputPrefix: String = "", val minTP: Int = 10, val minTNR: Double = .9, val minTPR: Double = .9, val minNPV: Double = .9, val minPPV: Double = .9, val absence: Boolean = false)

  override val version = """
    2014/11/17: Fixed issue where NPV and PPV were not properly set on the CLI
    2015/08/06: First public version
      
   """

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("java -jar bluejay.jar associate") {
      opt[File]("lineage") required () action { (x, c) => c.copy(lineageMatrix = x) } text ("Input file with the lineage matrix") validate { x => if (x.exists) success else failure("Lineage matrix file not found: " + x) }
      opt[File]("variant") required () action { (x, c) => c.copy(variantMatrix = x) } text ("Input file with the variant matrix") validate { x => if (x.exists) success else failure("Variant matrix file not found: " + x) }

      opt[String]('o', "output") action { (x, c) => c.copy(outputPrefix = x) } text ("Output prefix")
      opt[Int]("min-tp") action { (x, c) => c.copy(minTP = x) } text ("Minimum number of true positives for association (absolute count, default 10)")
      opt[Double]("min-tnr") action { (x, c) => c.copy(minTNR = x) } text ("Minimum true negative rate (specificity), percentage, default .9)")
      opt[Double]("min-tpr") action { (x, c) => c.copy(minTPR = x) } text ("Minimum true posititive rate (sensitivity), percentage, default .9)")
      opt[Double]("min-ppv") action { (x, c) => c.copy(minPPV = x) } text ("Minimum positive predictive value (ppv), percentage, default .9)")
      opt[Double]("min-npv") action { (x, c) => c.copy(minNPV = x) } text ("Minimum negative predictive value (npv), percentage, default .9)")
      opt[Unit]("absence") action { (x, c) => c.copy(absence = true) } text ("Investigate the absence of markers as association (default = false)")
      opt[Unit]("debug") action { (x, c) => c.copy(debug = true) } text ("Output information about variants that fail the criteria as commented output (default = false)")

    }

    parser.parse(args, Config()).map { config =>
      run(config.lineageMatrix, config.variantMatrix, config.outputPrefix, config);
    }

  }
  def run(lineageMatrix: File, variantMatrix: File, outputPrefix: String, config: Config) {
    val tmpLineages = Matrix.fromFile(lineageMatrix)
    val tmpSNP = Matrix.fromFile(variantMatrix)

    val nonZero = (tmpLineages.rowLabels.filter(f => {
      val row = tmpLineages.row(f)
      row.size > 0
    }).toSet.intersect(tmpSNP.rowLabels.toSet)).toList

    if (nonZero.size == 0) {
      println("None of your strains appear to have lineage information, please make sure you're using the correct matrix format.\nExiting...")
      System.exit(-1)
    }

    println(tmpLineages.rowLabels.size + "\t" + tmpSNP.rowLabels.size + "\t" + nonZero.size)

    val matLineages = rowMatch(nonZero, tmpLineages)
    val matSNPS = rowMatch(nonZero, tmpSNP)
    (matLineages.rowLabels.zip(matSNPS.rowLabels)).map(f => assume(f._1 == f._2))

    val cm = new FrequencyMap
    val x = matSNPS.colMatrix.mapValues(f => f.size)
    x.map(f => { cm.count(f._2) })

    val step1File = outputPrefix + "lineage_snp.tsv"
    val pw = new PrintWriter(step1File)
    pw.println(generatorInfo)
    pw.println("# variant = " + config.variantMatrix)
    pw.println("# lineage = " + config.lineageMatrix)
    pw.println("# output = " + config.outputPrefix)
    pw.println("# min TP = " + config.minTP)
    pw.println("# min TPR = " + config.minTPR)
    pw.println("# min TNR = " + config.minTNR)
    pw.println("# min PPV = " + config.minPPV)
    pw.println("# min NPV = " + config.minNPV)
    pw.println("# absence = " + config.absence)
    pw.println("##")
    pw.println("# Variant summary")
    pw.println("# total = " + matSNPS.columnLabels.size)
    pw.println("# occur in single sample = " + cm.getOrElse(1, null))
    pw.println("# occur in multiple samples = " + (cm.totalCount - cm.getOrElse(1, null)))
    pw.println("#AC2\ttpr(sensitivity)\ttnr(specificity)\tppv\tnpv\ttp\ttn\tfp\tfn\tpvalue\tlineage\tAP\tvariant")
    for (cn <- matLineages.columnLabels.filterNot(_.equals("$$"))) {
      val l = matLineages.column(cn)
      println("Processing: " + cn + "\t" + l.count(_ => true))
      for (rn <- matSNPS.columnLabels) {

        def eval(x: BitSet, postFix: String) {
          val bjn = new BJN(matSNPS.rowLabels.size, x, l)

          val outputString = List(bjn.ac2, bjn.tpr, bjn.tnr, bjn.ppv, bjn.npv, bjn.tp, bjn.tn, bjn.fp, bjn.fn, "" + bjn.pvalue, rn, postFix, cn).map(f => f match {
            case x: Int => "" + x
            case x: Double => nfP.format(x)
            case y: String => y

            case _ => f
          }).mkString("\t")
          if (bjn.tp >= config.minTP && bjn.tpr >= config.minTPR && bjn.tnr >= config.minTNR && bjn.ppv >= config.minPPV && bjn.npv >= config.minNPV)
            pw.println(outputString)
          else if (config.debug)
            pw.println("# " + outputString)
        }

        val s = matSNPS.column(rn)
        eval(s, "presence")
        if (config.absence) {
          val t = s ^ BitSet((0 to matSNPS.rowLabels.size): _*)
          eval(t, "absence")
        }

      }

    }
    pw.close

    val pw2 = new PrintWriter(outputPrefix + "lineageSNP_stats.txt")
    pw2.println(tLines(step1File).map(_.split("\t")(12)).groupBy(identity).mapValues(_.size).mkString("\n"))
    pw2.close

  }

  def rowMatch(rows: List[String], matrix1: Matrix): Matrix = {
    val columnLabels = matrix1.columnLabels
    val data = List(List("$$") ++ columnLabels) ++ rows.filterNot(p => p.equals("$$")).map(f => List(f) ++ bitset2String(matrix1.row(f), matrix1.columnLabels.length).getBytes().toList.map(c => c.toChar.toString))
    new Matrix(data);
  }

}