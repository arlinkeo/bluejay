package abeel.bluejay
import scala.collection.BitSet
import edu.northwestern.at.utils.math.statistics.FishersExactTest


object BlueJayNumbers {

  def main(args: Array[String]): Unit = {

  }
 

}

class BJN(val len: Int, predictor: BitSet, target: BitSet) {


  override def toString():String={
    List(tp,fp,tn,fn).mkString("\t")
  }
  lazy val ac2= (len-(predictor ^ target).size)/len.toDouble
  
  lazy val pvalue=FishersExactTest.fishersExactTest(tp, fn, fp, tn)(2)
  
  lazy val tp = (predictor & target).size
  lazy val fp = (predictor &~ target).size
  lazy val tn = len - ((predictor | target).size)
  lazy val fn = (target &~ predictor).size
 
  lazy val ppv = tp.toDouble / (tp + fp)
  lazy val npv = tn.toDouble / (tn + fn)

  lazy val sensitivity = tp.toDouble / (tp + fn)
  def tpr = sensitivity
  def recall = sensitivity

  lazy val precision = tp.toDouble / (tp + fp)

  lazy val lr_pos= tpr/fpr
  lazy val lr_neg=fnr/tnr
  
  
  lazy val fpr=fp.toDouble/(fp+tn)
  lazy val fnr=fn.toDouble/(tp+fn)
  
  lazy val dor=lr_pos/lr_neg
  
  lazy val specificity = tn.toDouble / (fp + tn)
  def tnr = specificity

  lazy val accuracy = (tp.toDouble + tn) / (tp + tn + fp + fn)

  lazy val f1 = 2 * (precision * recall) / (precision + recall)

}