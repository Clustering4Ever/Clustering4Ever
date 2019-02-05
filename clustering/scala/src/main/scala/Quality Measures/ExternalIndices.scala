package org.clustering4ever.indices
/**
 * @author Beck Gaël
 */
import scala.math.{max, log, sqrt}
import scala.collection.GenSeq
import org.clustering4ever.util.ClusteringIndicesCommons
import org.clustering4ever.enums.NmiNormalizationNature._
import scala.collection.mutable
/**
 *
 */
trait ExternalIndicesLocal {

	def mutualInformationInternal(x: GenSeq[Int], y: GenSeq[Int]) = {
		require( x.size == y.size )
		val n = x.size
		val maxX = x.max
		val maxY = y.max

		val maxOneIndices = (0 to maxX).toArray
		val maxTwoIndices = (0 to maxY).toArray

		val count = mutable.ArrayBuffer.fill(maxX + 1)(mutable.ArrayBuffer.fill(maxY +1)(0D))
		x.seq.indices.foreach( i => count(x(i))(y(i)) += 1D )

		val ai = ClusteringIndicesCommons.nmiObtainAi(new Array[Double](maxX + 1), maxOneIndices, maxTwoIndices, count)
		val bj = ClusteringIndicesCommons.nmiObtainBj(new Array[Double](maxY + 1), maxTwoIndices, maxOneIndices, count)
		val aiSum = ai.sum
		val hu = ClusteringIndicesCommons.nmiIn1(ai, aiSum)
		val hv = ClusteringIndicesCommons.nmiIn1(bj, aiSum)
		val huStrichV = ClusteringIndicesCommons.nmiIn2(maxOneIndices, maxTwoIndices, count, aiSum, bj)
		val mi = hu - huStrichV

		(mi, hu, hv)
	}

}
/**
 *
 */
object ExternalIndicesLocal extends ExternalIndicesLocal {
	/**
	 * Compute the mutual information
	 * @return (Mutual Information, entropy x, entropy y)
	 */
	def mutualInformation(x: GenSeq[Int], y: GenSeq[Int]): Double = mutualInformationInternal(x, y)._1
	/**
	 * Compute the normalize mutual entropy
	 * @param normalization nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 */
	def nmi(x: GenSeq[Int], y: GenSeq[Int], normalization: Normalization = SQRT) = {
		val (mi, hu, hv) = mutualInformationInternal(x, y)
		val nmi = normalization match {
			case SQRT => mi / sqrt(hu * hv)
			case MAX => mi / max(hu, hv)
		}
		nmi
	}
	/**
	 * @return (MI, NMI_sqrt, NMI_max)
	 */
	def everyMI(x: GenSeq[Int], y: GenSeq[Int]) = {
		val (mi, hu, hv) =  mutualInformationInternal(x, y)
		(
			mi,
			mi / sqrt(hu * hv),
			mi / max(hu, hv)
		)
	}
	/**
	 * Compute the normalize mutual entropy
	 * @param normalization nature of normalization, either sqrt or max
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
		(indexedValuesMap, x.map(indexedValuesMap).asInstanceOf[S])
	}
}