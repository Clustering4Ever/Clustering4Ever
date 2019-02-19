package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.math.{max, log, sqrt}
import scala.collection.GenSeq
import org.clustering4ever.util.ClusteringIndicesCommons
import org.clustering4ever.enums.NmiNormalizationNature
import scala.collection.mutable
/**
 *
 */
trait ExternalIndicesCommons extends Serializable {

	final type TP = Int
	final type FP = Int
	final type TN = Int
	final type FN = Int

	private[indices] lazy final val tpCount = (1, 0, 0, 0)
	private[indices] lazy final val tnCount = (0, 1, 0, 0)
	private[indices] lazy final val fpCount = (0, 0, 1, 0)
	private[indices] lazy final val fnCOunt = (0, 0, 0, 1)

	private[indices] final def fillConfusionMatrix(truth: Int, pred: Int): (TP, FP, TN, FN) = {
	  if(truth == 1) if(truth == pred) tpCount else fpCount
	  else if(truth == pred) tnCount else fnCOunt
	}
	/**
	 * Total True Positive
	 */
	val tp: TP
	/**
	 * Total True Negative
	 */
	val tn: TN
	/**
	 * Total False Positive
	 */
	val fp: FP 
	/**
	 * Total False Negative
	 */
	val fn: FN
	/**
	 *
	 */
	final lazy val confusionMatrixElementsSum: Double =  (tp + tn + fp + fn).toDouble
	/**
	 *
	 */
	final lazy val accuracy: Double = (tp + tn).toDouble / confusionMatrixElementsSum
	/**
	 *
	 */
	final lazy val precision: Double = tp.toDouble / (tp + fp)
	/**
	 *
	 */
	final lazy val recall: Double = tp.toDouble / (tp + fn)
	/**
	* F(β) score
	*/
	final lazy val fBeta: Double => Double = β => (1D + β * β) * (precision * recall) / (β * β * (precision + recall))
	/**
	* F1 score
	*/
	final lazy val f1: Double = fBeta(1D)
	/**
	* Matthews correlation coefficient
	*/
	final lazy val mcc: Double = (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)) 
	/**
	 *
	 */
	final lazy val czekanowskiDice: Double = (2D * tp) / (2D * tp + fn + fp )
	/**
	 *
	 */
	final lazy val rand: Double = (tp + tn) / confusionMatrixElementsSum
	/**
	 *
	 */
	final lazy val rogersTanimoto: Double = (tp + tn) / (tp + (2D * (fn + fp)) + tn)
	/**
	 *
	 */
	final lazy val folkesMallows: Double = (tp / sqrt((tp + fn) * (fp + tp)))
	/**
	 *
	 */
	final lazy val jaccard: Double = tp / (tp + fn + fp)
	/**
	 *
	 */
	final lazy val kulcztnski: Double = ((tp / (tp + fp)) + (tp / (tp + fn))) / 2D
	/**
	 *
	 */
	final lazy val mcNemar: Double = (tn - fp) / sqrt(tn + fp)
	/**
	 *
	 */
	final lazy val russelRao: Double = tp / confusionMatrixElementsSum
	/**
	 *
	 */
	final lazy val sokalSneath1: Double = tp / (tp + (2D * (fn + fp)))
	/**
	 *
	 */
	final lazy val sokalSneath2: Double = (tp + tn) / (tp + tn + ((fn + fp) / 2D))
}
/**
 *
 */
trait MIInfo extends Serializable {
	/**
	 * Mutual Information Internal
	 */
	val mutualInformation: Double
	/**
	 * Entropy for target data
	 */
	val hu: Double
	/**
	 * Entropy for predict data
	 */
	val hv: Double
}
/**
 *
 */
trait ExternalIndicesLocalAncestor extends ExternalIndicesCommons with MIInfo {
	/**
	 * Collection of (Target, Prediction)
	 */
	val targetAndPred: GenSeq[(Int, Int)]
	
	lazy final val (tp, tn, fp, fn) = {
		targetAndPred.map{ case (target, pred) => fillConfusionMatrix(target, pred) }.fold((0, 0, 0, 0))( (a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))
	}

	final lazy val purity: Double = {
		val cols = targetAndPred.groupBy(identity).map{ case ((_, pred), aggregate) => (pred, aggregate.size) }
		val groupByPred = cols.groupBy(_._1)
		val sum = groupByPred.map{ case (pred, aggregate) => (pred, aggregate.map(_._2).sum) }
		val maxByPred = groupByPred.map{ case (pred, aggregate) => (pred, aggregate.map(_._2).max) }
		sum.zip(maxByPred).map{ case ((_, s1), (_, m1)) => m1.toDouble / s1 }.sum / sum.size
	}

	final lazy val (mutualInformation, hu, hv) = {

		val n = targetAndPred.size
		val (maxX, maxY) = targetAndPred.reduce( (a, b) => (max(a._1, b._1), max(a._2, b._2)) )
		val maxOneIndices = (0 to maxX).toArray
		val maxTwoIndices = (0 to maxY).toArray

		val count = mutable.ArrayBuffer.fill(maxX + 1)(mutable.ArrayBuffer.fill(maxY + 1)(0D))
		targetAndPred.seq.indices.foreach( i => count(targetAndPred(i)._1)(targetAndPred(i)._2) += 1D )

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
	 *
	 */
	final lazy val nmi: NmiNormalizationNature.Normalization => Double = normalization => {
		normalization match {
			case NmiNormalizationNature.SQRT => mutualInformation / sqrt(hu * hv)
			case NmiNormalizationNature.MAX => mutualInformation / max(hu, hv)
		}
	}
	/**
	 * mutualInformation / sqrt(hu * hv)
	 */
	final lazy val nmiSQRT: Double = nmi(NmiNormalizationNature.SQRT)
	/**
	 * mutualInformation / max(hu, hv)
	 */
	final lazy val nmiMAX: Double = nmi(NmiNormalizationNature.MAX)
}
/**
 *
 */
final case class ExternalIndicesLocal(final val targetAndPred: GenSeq[(Int, Int)]) extends ExternalIndicesLocalAncestor