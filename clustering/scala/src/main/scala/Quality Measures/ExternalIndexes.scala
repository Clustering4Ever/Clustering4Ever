package clustering4ever.scala.indexes

import scala.math.{max, log, sqrt}
import scala.collection.GenSeq
import clustering4ever.util.ClusteringIndexesCommons
import clustering4ever.scala.indexes.NmiNormalizationNature._

/**
 * @author Beck Gaël
 *
 */
class ExternalIndexes
{
	private def mutualInformationInternal(x: GenSeq[Int], y: GenSeq[Int]) =
	{
		require( x.size == y.size )
		val n = x.size
		val maxX = x.max
		val maxY = y.max

		val maxOneIndices = (0 to maxX).toVector
		val maxTwoIndices = (0 to maxY).toVector

		val count = Array.fill(maxX + 1)(Array.fill(maxY +1)(0D))
		x.seq.indices.foreach( i => count(x(i))(y(i)) += 1D )

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
	 * @return (Mutual Information, entropy x, entropy y)
	 **/
	def mutualInformation(x: GenSeq[Int], y: GenSeq[Int]) =
	{
		(new ExternalIndexes).mutualInformationInternal(x, y)._1
	}

	/**
	 * Compute the normalize mutual entropy
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 **/
	def nmi(x: GenSeq[Int], y: GenSeq[Int], normalization: Normalization = SQRT) =
	{
		val (mi, hu, hv) = (new ExternalIndexes).mutualInformationInternal(x, y)
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
	def prepareList(x: GenSeq[Int]) =
	{
		val indexedValuesMap = x.distinct.zipWithIndex.seq.toMap
		(indexedValuesMap, x.map(indexedValuesMap))
	}
}