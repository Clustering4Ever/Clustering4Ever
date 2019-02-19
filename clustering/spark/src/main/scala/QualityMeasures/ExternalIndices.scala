package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.annotation.meta.param
import scala.collection.parallel.mutable.ParArray
import scala.collection.mutable
import scala.math.{max, log, sqrt}
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.enums.NmiNormalizationNature._
import org.clustering4ever.util.ClusteringIndicesCommons
/**
 *
 */
trait ExternalIndicesDistributedAncestor extends ExternalIndicesCommons {
  /**
   * StorageLevel for RDD
   */
  val persistanceLVL: StorageLevel
  /**
   * RDD of (Target, Prediction)
   */
  val targetAndPred: RDD[(Int, Int)]

  targetAndPred.persist(persistanceLVL)
  
  final lazy val (tp, tn, fp, fn) = {
    targetAndPred.map{ case (target, pred) => fillConfusionMatrix(target, pred) }.fold((0, 0, 0, 0))( (a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))
  }
  
  final lazy val purity: Double = {
    val cols = targetAndPred.map( line => (line, 1) ).reduceByKey(_ + _).map{ case ((_, pred), sum) => (pred, sum) }.cache
    val sum = cols.reduceByKeyLocally(_ + _)
    val maxByPred = cols.reduceByKeyLocally(max)
    cols.unpersist(false)
    sum.zip(maxByPred).map{ case ((_, s1), (_, m1)) => m1.toDouble / s1 }.sum / sum.size
  }

}
/**
 *
 */
final case class ExternalIndicesDistributed(final val targetAndPred: RDD[(Int, Int)], final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY) extends ExternalIndicesDistributedAncestor {

  /**
   * Compute the mutual information
   * @return (Mutual Information, entropy x, entropy y)
   */
  final lazy val (mutualInformation, hu, hv): (Double, Double, Double) = {
    val maxX = targetAndPred.max()(Ordering[Int].on(_._1))._1
    val maxY = targetAndPred.max()(Ordering[Int].on(_._2))._2

    val maxOneIndices = (0 to maxX).toArray
    val maxTwoIndices = (0 to maxY).toArray

    @transient val sc = targetAndPred.sparkContext

    val accNmi = new NmiAccumulator(mutable.ArrayBuffer.fill(maxX + 1)(mutable.ArrayBuffer.fill(maxY + 1)(0D)), maxX + 1, maxY + 1)
    sc.register(accNmi, "NmiAccumulator")
    targetAndPred.foreach{ case (x, y) => accNmi.addOne(x, y) }

    val count = accNmi.value

    val ai = ClusteringIndicesCommons.nmiObtainAi(new Array[Double](maxX + 1), maxOneIndices, maxTwoIndices, count)
    val bj = ClusteringIndicesCommons.nmiObtainBj(new Array[Double](maxY + 1), maxTwoIndices, maxOneIndices, count)

    val aiSum = ai.sum

    val hu = ClusteringIndicesCommons.nmiIn1(ai, aiSum)
    val hv = ClusteringIndicesCommons.nmiIn1(bj, aiSum)
    val huStrichV = ClusteringIndicesCommons.nmiIn2(maxOneIndices, maxTwoIndices, count, aiSum, bj)
    val mi = hu - huStrichV

    (mi, hu, hv)
  }
	/**
	 * Compute the normalize mutual entropy
	 * It is advise to cache targetAndPred before passing it to this method.
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 */
	final lazy val nmi: Normalization => Double = normalization => {
		val nmi = normalization match {
			case SQRT => mutualInformation / sqrt(hu * hv)
			case MAX => mutualInformation / max(hu, hv)
		}
		nmi
  }
  /**
   * mutualInformation / sqrt(hu * hv)
   */
  final lazy val nmiSQRT: Double = nmi(SQRT)
  /**
   * mutualInformation / max(hu, hv)
   */
  final lazy val nmiMAX: Double = nmi(MAX)
}

object ExternalIndicesDistributed {
	/**
	 * Prepare labels in order to get them in the range 0 -> n-1 rather than random labels values
	 */
	final def prepareLabels(x: RDD[Int]) = {
		val indexedValuesMap = x.distinct.zipWithIndex.collectAsMap
		(indexedValuesMap, x.map(indexedValuesMap))
	}
  /**
   * Compute the mutual information
   * It is advise to cache targetAndPred before passing it to this method.
   * @return (Mutual Information, entropy x, entropy y)
   */
  final def mutualInformation(targetAndPred: RDD[(Int, Int)], persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY): Double =
    (new ExternalIndicesDistributed(targetAndPred, persistanceLVL)).mutualInformation
  /**
   * Compute the normalize mutual entropy
   * It is advise to cache targetAndPred before passing it to this method.
   * @param normalization : nature of normalization, either sqrt or max
   * @return Normalize Mutual Information
   */
  final def nmi(targetAndPred: RDD[(Int, Int)], normalization: Normalization = SQRT, persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY): Double =
    (new ExternalIndicesDistributed(targetAndPred, persistanceLVL)).nmi(normalization)
}