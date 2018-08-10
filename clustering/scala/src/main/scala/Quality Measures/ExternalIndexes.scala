package clustering4ever.scala.indexes

import scala.math.{max, log, sqrt}
import scala.collection.parallel.mutable.ParArray
import scala.collection.immutable.{HashMap, Map}
import clustering4ever.scala.indexes.NmiNormalizationNature._
import clustering4ever.util.ClusteringIndexesCommons

/**
 * @author Beck Gaël
 *
 */
class ExternalIndexes
{
	private def mutualInformationInternal(x: Vector[Int], y: Vector[Int]) =
	{
		require( x.size == y.size )
		val n = x.size
		val maxX = x.max
		val maxY = y.max

		val maxOneIndices = (0 to maxX).toArray
		val maxTwoIndices = (0 to maxY).toArray

		val count = Array.fill(maxX + 1)(Array.fill(maxY +1)(0D))
		for( i <- x.indices ) count(x(i))(y(i)) += 1D

		val ai = new Array[Double](maxX + 1)
		val bj = new Array[Double](maxY + 1)

		for( m <- maxOneIndices ) for( l <- maxTwoIndices ) ai(m) += count(m)(l)
		for( m <- maxTwoIndices ) for( l <- maxOneIndices ) bj(m) += count(l)(m)

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
	def mutualInformation(x: Vector[Int], y:Vector[Int]) =
	{
		(new ExternalIndexes).mutualInformationInternal(x, y)._1
	}

	/**
	 * Compute the normalize mutual entropy
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 **/
	def nmi(x: Vector[Int], y:Vector[Int], normalization: Normalization = SQRT) =
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
	def prepareList(x: Vector[Int]) =
	{
		val indexedValuesMap = x.distinct.zipWithIndex.toMap
		(indexedValuesMap, x.map(indexedValuesMap))
	}
}