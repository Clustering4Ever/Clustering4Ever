package org.clustering4ever.scala.clustering
/**
 * @author Beck Gaël
 */
import scala.collection.{mutable, GenSeq}
import scala.collection.parallel.mutable.ParArray
import org.clustering4ever.clustering.{ClusteringAlgorithm, ClusteringModel}
/**
 *
 */
final case class JenksNaturalBreaks(desiredNumberCategories: Int) extends ClusteringAlgorithm {

  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.JenksNaturalBreaks
  /**
   * Look at https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization for more details 
   * Return breaks position in the sorted GenSeq
   * @param data a sorted GenSeq
   * @param desiredNumberCategories : number of breaks user desire
   * @return Indices of breaks in data sequence
   */
  final def run[@specialized(Int, Double, Long, Float) N](data: GenSeq[N])(implicit num: Numeric[N]): JenksNaturalBreaksModel[N] = {

    val nbCat = desiredNumberCategories - 1
    val nbValues = data.size
    var value = 0D
    var v = 0D
    var i3 = 0
    var i4 = 0

    val mat1 = Array.fill(nbValues)(Array.fill(nbCat)(1D))
    val mat2 = Array.fill(nbValues)(Array.fill(nbCat)(Double.MaxValue))
    
    (2 to nbValues).foreach{ l =>
      var s1 = 0D
      var s2 = 0D
      var w = 0D
      (1 to l).foreach{ m =>
        val i3 = l - m + 1
        value = num.toDouble(data(i3 - 1))
        s2 += value * value
        s1 += value
        w += 1
        v = s2 - (s1 * s1) / w
        i4 = i3 - 1
        if(i4 != 0) {
          (2 to nbCat).foreach{ j =>
            if(mat2(l - 1)(j - 1) >= (v + mat2(i4 - 1)(j - 2))) {
              mat1(l - 1)(j - 1) = i3
              mat2(l - 1)(j - 1) = v + mat2(i4 - 1)(j - 2)
            }
          }
        }
      }
     
      mat1(l - 1)(0) = 1
      mat2(l - 1)(0) = v
    }
          
    val kclass = (1 to nbCat).map(_.toDouble).toBuffer

    kclass(nbCat - 1) = nbValues

    def update(j: Int, k: Int, kclass: mutable.Buffer[Double]) = {
      val id = (mat1(k - 1)(j - 1)).toInt - 1
      kclass(j - 2) = id
      (id, kclass)      
    }

    @annotation.tailrec
    def go(n: Int, k: Int, kclass: mutable.Buffer[Double]): (Int, mutable.Buffer[Double]) = {
      val (kUpdt, kclassUpdt) = update(n, k, kclass)
      if( n > 2 ) go(n - 1, kUpdt, kclassUpdt)
      else (kUpdt, kclassUpdt)
    }

    val (_, kclass2) = go(nbCat, nbValues, kclass)

    val res = mutable.ArrayBuffer.empty[N]
    res += data.head

    (1 to nbCat).foreach( i => res += data(kclass2(i - 1).toInt - 1) )
    
    JenksNaturalBreaksModel(res)
  }
}
/**
 *
 */
object JenksNaturalBreaks {

  final def run[@specialized(Int, Double, Long, Float) N](data: GenSeq[N], desiredNumberCategories: Int)(implicit num: Numeric[N]): JenksNaturalBreaksModel[N] = JenksNaturalBreaks(desiredNumberCategories).run(data)

  final def apply[@specialized(Int, Double, Long, Float) N](data: GenSeq[N], desiredNumberCategories: Int)(implicit num: Numeric[N]): JenksNaturalBreaksModel[N] = run(data, desiredNumberCategories)

}
/**
 *
 */
final case class JenksNaturalBreaksModel[@specialized(Int, Double, Long, Float) N](breaks: mutable.ArrayBuffer[N])(implicit num: Numeric[N]) extends ClusteringModel {

  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.JenksNaturalBreaks
  /**
   *
   */
  private val breaksIndexed = breaks.zipWithIndex
  /**
   * Predict between which breaks fall a new point, 0 stands for -infinity/1st-Break and breaks.size for last-Break/+infinity
   */
  final def predict(v: N): Int = breaksIndexed.find{ case (break, _) => num.lteq(v, break) }.map(_._2).getOrElse(breaksIndexed.size)

}
