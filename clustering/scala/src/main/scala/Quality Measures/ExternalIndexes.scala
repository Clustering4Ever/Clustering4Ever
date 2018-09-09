package clustering4ever.scala.indexes

import scala.math.{max, log, sqrt}
import scala.collection.GenSeq
import clustering4ever.util.ClusteringIndexesCommons
import clustering4ever.scala.indexes.NmiNormalizationNature._

/**
 * @author Beck Gaël
 *
 */
class ExternalIndexes {

	def mutualInformationInternal[S <: GenSeq[Int]](x: S, y: S) = {
		require( x.size == y.size )
		val n = x.size
		val maxX = x.max
		val maxY = y.max

		val maxOneIndices = (0 to maxX).toArray
		val maxTwoIndices = (0 to maxY).toArray

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

object ExternalIndexes {
	/**
	 * Compute the mutual information
	 * @return (Mutual Information, entropy x, entropy y)
	 */
	def mutualInformation[S <: GenSeq[Int]](x: S, y: S): Double = (new ExternalIndexes).mutualInformationInternal(x, y)._1
	/**
	 * Compute the mutual information
	 * @return (Mutual Information, entropy x, entropy y)
	 */
	def mutualInformation[S <: GenSeq[(Int, Int)]](xy: S): Double =	{
		val (x, y) = xy.unzip
		mutualInformation(x, y)
	}
	/**
	 * Compute the normalize mutual entropy
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 */
	def nmi[S <: GenSeq[Int]](x: S, y: S, normalization: Normalization = SQRT) = {
		val (mi, hu, hv) = (new ExternalIndexes).mutualInformationInternal(x, y)
		val nmi = normalization match {
			case SQRT => mi / sqrt(hu * hv)
			case MAX => mi / max(hu, hv)
		}
		nmi
	}
	/**
	 * Compute the normalize mutual entropy
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 */
	def nmi[S <: GenSeq[Int]](xy: Seq[(Int, Int)], normalization: Normalization): Double = {
		val (x, y) = xy.unzip
		nmi(x, y, normalization)
	}
	/**
	 * Prepare labels in order to get them in the range 0 -> n-1 rather than random labels values
	 */
	def prepareLabels[S <: GenSeq[Int]](x: S) =	{
		val indexedValuesMap = x.distinct.zipWithIndex.toMap.seq
		(indexedValuesMap, x.map(indexedValuesMap))
	}
}