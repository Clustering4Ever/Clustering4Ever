package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.math.{max, log, sqrt}
import scala.collection.GenSeq
import org.clustering4ever.util.ClusteringIndicesCommons
import org.clustering4ever.enums.NmiNormalizationNature._
import scala.collection.mutable
// import org.clustering4ever.util.SumVectors
/**
 *
 */
trait ExternalIndicesLocal {

	final type TP = Int
	final type FP = Int
	final type TN = Int
	final type FN = Int

	private lazy final val tp = (1, 0, 0, 0)
	private lazy final val tn = (0, 1, 0, 0)
	private lazy final val fp = (0, 0, 1, 0)
	private lazy final val fn = (0, 0, 0, 1)

	private final def fillConfusionMatrix(truth: Int, pred: Int): (TP, FP, TN, FN) = {
	  if(truth == 1) if(truth == pred) tp else fp
	  else if(truth == pred) tn else fn
	}

	final def obtainConfusionMatrix(truthAndPred: GenSeq[(Int, Int)]): (TP, FP, TN, FN) = {
		truthAndPred.map{ case (truth, pred) => fillConfusionMatrix(truth, pred) }.fold((0, 0, 0, 0))( (a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))
	}

	final def accuracy(truthAndPred: GenSeq[(Int, Int)]) = {
		val (tp, tn, fp, fn) = obtainConfusionMatrix(truthAndPred)
		(tp + tn).toDouble / (tp + tn + fp + fn)

	}

	final def mutualInformationInternal(x: GenSeq[Int], y: GenSeq[Int]) = {
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
	final def mutualInformation(x: GenSeq[Int], y: GenSeq[Int]): Double = mutualInformationInternal(x, y)._1
	/**
	 * Compute the normalize mutual entropy
	 * @param normalization nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 */
	final def nmi(x: GenSeq[Int], y: GenSeq[Int], normalization: Normalization = SQRT) = {
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
	final def everyMI(x: GenSeq[Int], y: GenSeq[Int]) = {
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
	final def nmi[S <: GenSeq[Int]](xy: Seq[(Int, Int)], normalization: Normalization): Double = {
		val (x, y) = xy.unzip
		nmi(x, y, normalization)
	}
	/**
	 * Prepare labels in order to get them in the range 0 -> n-1 rather than random labels values
	 */
	final def prepareLabels[S <: GenSeq[Int]](x: S) =	{
		val indexedValuesMap = x.distinct.zipWithIndex.toMap.seq
		(indexedValuesMap, x.map(indexedValuesMap).asInstanceOf[S])
	}
}