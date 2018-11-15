package org.clustering4ever.spark.indexes
/**
 * @author Beck Gaël
 *
 * Refactor from an internship work of CAO Anh Quan https://github.com/Spark-clustering-notebook/ClusteringIndices
 */
import scala.annotation.meta.param
import scala.collection.parallel.mutable.ParArray
import scala.collection.mutable
import scala.math.{max, log, sqrt}
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.scala.indexes.NmiNormalizationNature._
import org.clustering4ever.util.ClusteringIndexesCommons
/**
 *
 */
class ExternalIndexes(@(transient @param) sc: SparkContext, groundTruthAndPred: RDD[(Int, Int)], persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY) extends Serializable  {

  groundTruthAndPred.persist(persistanceLVL)
  /**
   * Compute the mutual information
   * @return (Mutual Information, entropy x, entropy y)
   */
  private lazy val (mi, hu, hv): (Double, Double, Double) = {
    val maxX = groundTruthAndPred.max()(Ordering[Int].on(_._1))._1
    val maxY = groundTruthAndPred.max()(Ordering[Int].on(_._2))._2

    val maxOneIndices = (0 to maxX).toArray
    val maxTwoIndices = (0 to maxY).toArray

    val accNmi = new NmiAccumulator(mutable.ArrayBuffer.fill(maxX + 1)(mutable.ArrayBuffer.fill(maxY + 1)(0D)), maxX + 1, maxY + 1)
    sc.register(accNmi, "NmiAccumulator")
    groundTruthAndPred.foreach{ case (x, y) => accNmi.addOne(x, y) }

    val count = accNmi.value

    val ai = ClusteringIndexesCommons.nmiObtainAi(new Array[Double](maxX + 1), maxOneIndices, maxTwoIndices, count)
    val bj = ClusteringIndexesCommons.nmiObtainBj(new Array[Double](maxY + 1), maxTwoIndices, maxOneIndices, count)

    val aiSum = ai.sum

    val hu = ClusteringIndexesCommons.nmiIn1(ai, aiSum)
    val hv = ClusteringIndexesCommons.nmiIn1(bj, aiSum)
    val huStrichV = ClusteringIndexesCommons.nmiIn2(maxOneIndices, maxTwoIndices, count, aiSum, bj)
    val mi = hu - huStrichV

    (mi, hu, hv)
  }

  lazy val mutualInformation: Double = mi
	/**
	 * Compute the normalize mutual entropy
	 * It is advise to cache groundTruthAndPred before passing it to this method.
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 */
	lazy val nmi: Normalization => Double = normalization => {
		val nmi = normalization match {
			case SQRT => mi / sqrt(hu * hv)
			case MAX => mi / max(hu, hv)
		}
		nmi
  }
}

object ExternalIndexes {
	/**
	 * Prepare labels in order to get them in the range 0 -> n-1 rather than random labels values
	 */
	def prepareLabels(x: RDD[Int]) = {
		val indexedValuesMap = x.distinct.zipWithIndex.collectAsMap
		(indexedValuesMap, x.map(indexedValuesMap))
	}
  /**
   * Compute the mutual information
   * It is advise to cache groundTruthAndPred before passing it to this method.
   * @return (Mutual Information, entropy x, entropy y)
   */
  def mutualInformation(sc: SparkContext, groundTruthAndPred: RDD[(Int, Int)], persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY): Double =
    (new ExternalIndexes(sc, groundTruthAndPred, persistanceLVL)).mutualInformation
  /**
   * Compute the normalize mutual entropy
   * It is advise to cache groundTruthAndPred before passing it to this method.
   * @param normalization : nature of normalization, either sqrt or max
   * @return Normalize Mutual Information
   */
  def nmi(sc: SparkContext, groundTruthAndPred: RDD[(Int, Int)], normalization: Normalization = SQRT, persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY): Double =
    (new ExternalIndexes(sc, groundTruthAndPred, persistanceLVL)).nmi(normalization)
}