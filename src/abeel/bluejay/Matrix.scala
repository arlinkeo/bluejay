package abeel.bluejay



import scala.collection.BitSet
import atk.util.Tool
import java.io.File

object Matrix extends Tool {
  
  
  def fromFile(file: File): Matrix = {
    new Matrix(tLines(file).map(f => f.split("\t").toList))

  }
}

class Matrix(tmpRowMatrix: List[List[String]]) extends Tool {

 
  lazy val tmpColMatrix = tmpRowMatrix.transpose
  lazy val rowMatrix = tmpRowMatrix.map(row => {
    val bs = BitSet(row.drop(1).zipWithIndex.filter(p => p._1 == "1").map(_._2): _*)
    row.head -> bs
  }).toMap

  lazy val columnLabels = tmpRowMatrix(0).drop(1)
  lazy val rowLabels = tmpColMatrix(0).drop(1)

  lazy val colMatrix = tmpColMatrix.map(col => {
    val bs = BitSet(col.drop(1).zipWithIndex.filter(p => p._1 == "1").map(_._2): _*)
    col.head -> bs
  }).toMap

  def row(key: String): BitSet = {
    rowMatrix.getOrElse(key, null)
  }
  def column(key: String): BitSet = {
    colMatrix.getOrElse(key, null)
  }

}