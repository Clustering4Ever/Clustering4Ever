package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.math.{max, log, sqrt}
import scala.collection.GenSeq
import org.clustering4ever.util.ClusteringIndicesCommons
import org.clustering4ever.enums.NmiNormalizationNature
import scala.collection.{mutable, immutable}
/**
 *
 */
trait ExternalIndicesLocalAncestor extends Serializable {
	/**
	 * Collection of (Target, Prediction)
	 */
	val targetAndPred: GenSeq[(Int, Int)]
}
/**
 *
 */
trait BinaryExternalIndicesAncestor extends Serializable {
	/**
	 * Total True Positive
	 */
	val tp: Int
	/**
	 * Total True Negative
	 */
	val tn: Int
	/**
	 * Total False Positive
	 */
	val fp: Int 
	/**
	 * Total False Negative
	 */
	val fn: Int

	private[indices] final type TP = Int

	private[indices] final type FP = Int

	private[indices] final type TN = Int

	private[indices] final type FN = Int

	private[indices] lazy final val tpCount = (1, 0, 0, 0)

	private[indices] lazy final val tnCount = (0, 1, 0, 0)

	private[indices] lazy final val fpCount = (0, 0, 1, 0)

	private[indices] lazy final val fnCOunt = (0, 0, 0, 1)

	private[indices] final def fillBinaryConfusionMatrix(truth: Int, pred: Int): (TP, FP, TN, FN) = {
	  if(truth == 1) if(truth == pred) tpCount else fpCount
	  else if(truth == pred) tnCount else fnCOunt
	}
}
/**
 *
 */
trait BinaryExternalIndicesCommons extends BinaryExternalIndicesAncestor {
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
	* F<sub>β</sub> score
	*/
	final lazy val fBeta: Double => Double = beta => (1D + beta * beta) * (precision * recall) / (beta * beta * (precision + recall))
	/**
	* F<sub>1</sub> score
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
trait IndicesDoc extends Serializable {
	/**
	 * Mutual Information Internal
	 */
	val mutualInformation: Double
	/**
	 * Entropy for target data
	 */
	val et: Double
	/**
	 * Entropy for predict data
	 */
	val ep: Double
	/**
	 * Compute the normalize mutual entropy
	 * It is advise to cache targetAndPred before passing it to this method.
	 * @param normalization : nature of normalization, either sqrt or max
	 * @return Normalize Mutual Information
	 */
	val nmi: NmiNormalizationNature.Normalization => Double
	/**
	 * mutualInformation / sqrt(et * ep)
	 */
	val nmiSQRT: Double
	/**
	 * mutualInformation / max(et, ep)
	 */
	val nmiMAX: Double
	/**
	 *
	 */
	val purity: Double
}
/**
 *
 */
trait MultiConfusionMatrixAncestor extends Serializable {
	/**
	 *
	 */
	val classesNumber: Int
	/**
	 * True Positives per label from 0 until classesNumber-1 
	 */
	val tps: Array[Int]
	/**
	 * False Positives per label from 0 until classesNumber-1 
	 */
	val fps: Array[Int]
	/**
	 * False Negatives per label from 0 until classesNumber-1 
	 */
	val fns: Array[Int]
	/**
	 *
	 */
	val confusionMatrixSum: Int
	/**
	 * True Negatives per label from 0 until classesNumber-1 
	 */
	val tns: Array[Int]
	/**
	 * The multi-class confusion matrix 
	 */
	val confusionMatrix: Array[Array[Int]]

}
/**
 *
 */
trait MultiConfusionMatrixLocalAncestor extends ExternalIndicesLocalAncestor with ConfusionMatrixLocalCommons {

	final lazy val classesNumber: Int = targetAndPred.map(_._1).distinct.size
	/**
	 *
	 */
	final private lazy val emptyConfusionMatrix = Array.fill(classesNumber)(Array.fill(classesNumber)(0))

	final lazy val confusionMatrix: Array[Array[Int]] = targetAndPred.aggregate(emptyConfusionMatrix)(putInConfusionMatrix, fusionTwoConfusionMatrix)

	private final lazy val rangeCM = (0 until confusionMatrix.size)

	final lazy val tps: Array[Int] = rangeCM.map( diag => confusionMatrix(diag)(diag) ).toArray

	final lazy val fps: Array[Int] = rangeCM.map( i => rangeCM.map( j => confusionMatrix(j)(i) ).sum - tps(i) ).toArray

	final lazy val fns: Array[Int] = rangeCM.map( i => confusionMatrix(i).sum - tps(i) ).toArray
	/**
	 * TO DO with tailrec
	 */
	final lazy val confusionMatrixSum: Int = confusionMatrix.aggregate(0)( (s, row) => s + row.sum, (s1, s2) => s1 + s2 )

	final lazy val tns: Array[Int] = rangeCM.map( i => confusionMatrixSum - tps(i) - fps(i) - fns(i) ).toArray
}
/**
 *
 */
final case class MultiConfusionMatrixLocal(val targetAndPred: GenSeq[(Int, Int)]) extends MultiConfusionMatrixLocalAncestor
/**
 *
 */
trait BinaryAndMultiExternalIndicesLocalAncestor extends ExternalIndicesLocalAncestor with IndicesDoc {

	final lazy val purity: Double = {
		val cols = targetAndPred.groupBy(identity).map{ case ((_, pred), aggregate) => (pred, aggregate.size) }
		val groupByPred = cols.groupBy(_._1)
		val sum = groupByPred.map{ case (pred, aggregate) => (pred, aggregate.map(_._2).sum) }
		val maxByPred = groupByPred.map{ case (pred, aggregate) => (pred, aggregate.map(_._2).max) }
		// sum.zip(maxByPred).map{ case ((_, s1), (_, m1)) => m1.toDouble / s1 }.sum / sum.size
		sum.zip(maxByPred).aggregate(0D)( (sum, x) => sum + x._2._2.toDouble / x._1._2, (sum1, sum2) => sum1 + sum2 ) / sum.size
	}

	final lazy val (mutualInformation, et, ep) = {

		val n = targetAndPred.size
		val (maxX, maxY) = targetAndPred.reduce( (a, b) => (max(a._1, b._1), max(a._2, b._2)) )
		val maxOneIndices = (0 to maxX).toArray
		val maxTwoIndices = (0 to maxY).toArray

		val count = mutable.ArrayBuffer.fill(maxX + 1)(mutable.ArrayBuffer.fill(maxY + 1)(0D))
		targetAndPred.seq.indices.foreach( i => count(targetAndPred(i)._1)(targetAndPred(i)._2) += 1D )

		val ai = ClusteringIndicesCommons.nmiObtainAi(new Array[Double](maxX + 1), maxOneIndices, maxTwoIndices, count)
		val bj = ClusteringIndicesCommons.nmiObtainBj(new Array[Double](maxY + 1), maxTwoIndices, maxOneIndices, count)
		val aiSum = ai.sum
		val et = ClusteringIndicesCommons.nmiIn1(ai, aiSum)
		val ep = ClusteringIndicesCommons.nmiIn1(bj, aiSum)
		val etStricep = ClusteringIndicesCommons.nmiIn2(maxOneIndices, maxTwoIndices, count, aiSum, bj)
		val mi = et - etStricep

		(mi, et, ep)
	}

	final lazy val nmi: NmiNormalizationNature.Normalization => Double = normalization => {
		normalization match {
			case NmiNormalizationNature.SQRT => mutualInformation / sqrt(et * ep)
			case NmiNormalizationNature.MAX => mutualInformation / max(et, ep)
		}
	}

	final lazy val nmiSQRT: Double = nmi(NmiNormalizationNature.SQRT)

	final lazy val nmiMAX: Double = nmi(NmiNormalizationNature.MAX)
}
/**
 *
 */
trait BinaryExternalIndicesLocalAncestor extends ExternalIndicesLocalAncestor with BinaryExternalIndicesCommons {

	final val (tp, tn, fp, fn) = {
		// val authorizedValues = immutable.Set(0, 1)
		// val isBinary = !targetAndPred.exists{ case (target, pred) => !authorizedValues.contains(target) || !authorizedValues.contains(pred) }
		// require(isBinary, println("You set a values other than {0, 1}"))
		targetAndPred.map{ case (target, pred) => fillBinaryConfusionMatrix(target, pred) }.fold((0, 0, 0, 0))( (a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))
	}
}
/**
 *
 */
final case class MultiExternalIndicesLocal(final val targetAndPred: GenSeq[(Int, Int)]) extends BinaryAndMultiExternalIndicesLocalAncestor
/**
 *
 */
final case class BinaryExternalIndicesLocal(final val targetAndPred: GenSeq[(Int, Int)]) extends BinaryExternalIndicesLocalAncestor with BinaryAndMultiExternalIndicesLocalAncestor
/**
 *
 */
object ExternalIndicesLocalUtils extends Serializable {
	/**
	 * Reformate labels from any Int combination to usable one from 0 until l-1 with l number of distincts labels
	 */
	final def prepareLabels(labelsOrPred: GenSeq[Int]): (immutable.Map[Int, Int], GenSeq[Int]) = {
		val diffLabelsMapping = labelsOrPred.distinct.zipWithIndex.toMap
		(diffLabelsMapping.toSeq.seq.toMap, labelsOrPred.map(diffLabelsMapping(_)))
	}
}