package clustering4ever.spark.indexes

import scala.annotation.meta.param
import scala.collection.immutable.{HashMap, Map}
import scala.collection.parallel.mutable.ParArray
import scala.math.{max, log, sqrt}
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import org.apache.spark.storage.StorageLevel
import clustering4ever.scala.indexes.NmiNormalizationNature._
import clustering4ever.util.ClusteringIndexesCommons

/**
 * @author Beck Gaël
 *
 */
class ExternalIndexes
{
	private def mutualInformationInternal(@(transient @param) sc: SparkContext, trueAndPredict: RDD[(Int, Int)], persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY) =
	{
		trueAndPredict.persist(persistanceLVL)

		val n = trueAndPredict.count
		val maxX = trueAndPredict.max()(Ordering[Int].on(_._1))._1
		val maxY = trueAndPredict.max()(Ordering[Int].on(_._2))._2

		val maxOneIndices = (0 to maxX).toVector
		val maxTwoIndices = (0 to maxY).toVector

		val accNmi = new NmiAccumulator(Array.fill(maxX + 1)(Array.fill(maxY + 1)(0D)), maxX + 1, maxY + 1)
		sc.register(accNmi, "NmiAccumulator")
		trueAndPredict.foreach{ case (x, y) => accNmi.addOne(x, y) }

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

}

object ExternalIndexes
{
	/**
	 * Compute the mutual information
	 * It is advise to cache trueAndPredict before passing it to this method.
	 * @return (Mutual Information, entropy x, entropy y)
	 **/
	def mutualInformation(sc: SparkContext, trueAndPredict: RDD[(Int, Int)]) =
	{
		(new ExternalIndexes).mutualInformationInternal(sc, trueAndPredict)._1
	}

	/**
	 * Compute the normalize mutual entropy
	 * It is advise to cache trueAndPredict before passing it to this method.
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 **/
	def nmi(sc: SparkContext, trueAndPredict: RDD[(Int, Int)], normalization: Normalization = SQRT, persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY) =
	{
		val (mi, hu, hv) = (new ExternalIndexes).mutualInformationInternal(sc, trueAndPredict, persistanceLVL)
		val nmi = normalization match
		{
			case SQRT => mi / sqrt(hu * hv)
			case MAX => mi / max(hu, hv)
		}
		nmi
	}

	/**
	 * Prepare labels in order to get them in the range 0 -> n-1 rather than random labels values
	 **/
	def prepareList(x: Array[Int]) =
	{
		val indexedValuesMap = x.distinct.zipWithIndex.toMap
		(indexedValuesMap, x.map(indexedValuesMap))
	}
}