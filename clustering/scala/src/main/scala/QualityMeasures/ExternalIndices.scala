package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.math.{max, log, sqrt}
import scala.collection.GenSeq
import org.clustering4ever.util.ClusteringIndicesCommons
import org.clustering4ever.enums.NmiNormalizationNature
import scala.collection.{mutable, immutable}
import org.clustering4ever.math.MathC4E
import java.util.concurrent.Semaphore
/**
 *
 */
trait ExternalIndicesLocalDataAncestor extends Serializable {
	/**
	 * Collection of (Target, Prediction)
	 */
	val targetAndPred: GenSeq[(Int, Int)]
}
/**
 *
 */
trait ContingencyTableLocalCommons extends Serializable {
  /**
   *
   */
  private[indices] val targetClassesNumber: Int
  /**
   *
   */
  private[indices] val predClassesNumber: Int
  /**
   *
   */
  final private[indices] lazy val emptyContingencyTable: Array[Array[Long]] = Array.fill(targetClassesNumber)(Array.fill(predClassesNumber)(0L))
  /**
   *
   */
  val contingencyTable: Array[Array[Long]]
  /**
   *
   */
  final protected[indices] def putInContingencyTable(contingencyTable: Array[Array[Long]], targetAndPred: (Int, Int)): Array[Array[Long]] = {
    contingencyTable(targetAndPred._1)(targetAndPred._2) += 1
    contingencyTable
  }
  /**
   *
   */
  final protected[indices] def fusionContingencyTables(contingencyTable1: Array[Array[Long]], contingencyTable2: Array[Array[Long]]): Array[Array[Long]] = {
    contingencyTable1.zipWithIndex.foreach{ case (col, i) => col.zipWithIndex.foreach{ case (v, j) => contingencyTable2(i)(j) += v } }
    contingencyTable2
  }
}
/**
 *
 */
trait TPTNFPFNAncestor extends Serializable {
	/**
	 * Total True Positive
	 */
	val tp: Long
	/**
	 * Total True Negative
	 */
	val tn: Long
	/**
	 * Total False Positive
	 */
	val fp: Long 
	/**
	 * Total False Negative
	 */
	val fn: Long
}
/**
 *
 */
trait BinaryExternalIndicesAncestor extends TPTNFPFNAncestor {

	private[indices] final type TP = Long

	private[indices] final type FP = Long

	private[indices] final type TN = Long

	private[indices] final type FN = Long

	private[indices] lazy final val tpCount = (1L, 0L, 0L, 0L)

	private[indices] lazy final val tnCount = (0L, 1L, 0L, 0L)

	private[indices] lazy final val fpCount = (0L, 0L, 1L, 0L)

	private[indices] lazy final val fnCOunt = (0L, 0L, 0L, 1L)

	private[indices] final def fillBinaryConfusionMatrix(truth: Int, pred: Int): (TP, FP, TN, FN) = {
	  if(truth == 1) if(truth == pred) tpCount else fpCount
	  else if(truth == pred) tnCount else fnCOunt
	}
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
trait MultiClassesContingencyTable extends ContingencyTableLocalCommons with TPTNFPFNAncestor {
  /**
   *
   */
  private final lazy val rangeTargetCM = (0 until targetClassesNumber)
  /**
   *
   */
  private final lazy val rangePredCM = (0 until predClassesNumber)
  /**
   *
   */
  final lazy val sumTargetRowCM: immutable.IndexedSeq[Long] = rangeTargetCM.map(contingencyTable(_).sum)
  /**
   *
   */
  final lazy val sumPredColumnsCM: immutable.IndexedSeq[Long] = rangePredCM.map( i => rangeTargetCM.map( j => contingencyTable(j)(i) ).sum )
  /**
   *
   */
  private[indices] final lazy val tpfp: Long = sumPredColumnsCM.aggregate(0L)( (s, v) => s + MathC4E.binom(v, 2L), (sum1, sum2) => sum1 + sum2 )
  /**
   *
   */
  private[indices] final lazy val tpfn: Long = sumTargetRowCM.aggregate(0L)( (s, v) => s + MathC4E.binom(v, 2L), (sum1, sum2) => sum1 + sum2 )
  /**
   *
   */
  private[indices] final lazy val columnSize: Int = contingencyTable.head.size
  /**
   *
   */
  protected[indices] lazy val sumOfChoose2ContingencyTable: Long = {
    @annotation.tailrec
    def go(i: Int, j: Int, t: Long): Long = {
      val updt = t + MathC4E.binom(contingencyTable(i)(j), 2L)
      if(i >= 0 && j > 0) go(i, j - 1, updt)
      else if(i > 0 && j == 0) go(i - 1, columnSize - 1, updt)
      else updt
    }
    go(contingencyTable.size - 1, columnSize - 1, 0L)
  }

  final lazy val tp: Long = sumOfChoose2ContingencyTable

  final lazy val fp: Long = tpfp - tp

  final lazy val fn: Long = tpfn - tp

  final lazy val tn: Long = {
    /**
     *
     */
    val computeSumContingencyTable: Long = {
      @annotation.tailrec
      def go(i: Int, j: Int, t: Long): Long = {
        val updt = t + contingencyTable(i)(j)
        if(i >= 0 && j > 0) go(i, j - 1, updt)
        else if(i > 0 && j == 0) go(i - 1, columnSize - 1, updt)
        else updt
      }
      go(contingencyTable.size - 1, columnSize - 1, 0)
    }
    MathC4E.binom(computeSumContingencyTable, 2) - tp - fp - fn
  }
}
/**
 *
 */
trait ContingencyMatricesBasicsIndices extends TPTNFPFNAncestor {
  /**
   *
   */
  final lazy val tpfptnfnSum: Double =  (tp + tn + fp + fn).toDouble
  /**
   *
   */
  final lazy val accuracy: Double = (tp + tn).toDouble / tpfptnfnSum
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
}
/**
 *
 */
trait ContingencyMatricesIndices extends MultiClassesContingencyTable with ContingencyMatricesBasicsIndices {
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
  final lazy val rand: Double = (tp + tn) / tpfptnfnSum
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
  final lazy val russelRao: Double = tp / tpfptnfnSum
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
trait ARANDCommons extends ContingencyMatricesIndices {
  /**
   * TODO
   */
  // def computeRowSums(m: Array[Array[Int]]): Array[Int] = {
  //   @annotation.tailrec
  //   def go(i: Int, j: Int, t: Array[Int], columnSize: Int): Array[Int] = {
  //     t(i) += MathC4E.binom(m(i)(j), 2)
  //     if(i >= 0 && j > 0) go(i, j - 1, t, columnSize)
  //     else if(i > 0 && j == 0) go(i - 1, columnSize - 1, t, columnSize)
  //     else t
  //   }
  //   go(m.size - 1, m.head.size - 1, Array.fill(m.head.size)(0), m.head.size)
  // }
  /**
   * TODO
   */
  // def computeColumnSums(m: Array[Array[Int]]): Array[Int] = {
  //   @annotation.tailrec
  //   def go(i: Int, j: Int, t: Array[Int], columnSize: Int): Array[Int] = {
  //     t(j) += MathC4E.binom(m(i)(j), 2)
  //     if(i >= 0 && j > 0) go(i, j - 1, t, columnSize)
  //     else if(i > 0 && j == 0) go(i - 1, columnSize - 1, t, columnSize)
  //     else t
  //   }
  //   go(m.size - 1, m.head.size - 1, Array.fill(m.size)(0), m.size)
  // }
  
  protected final val index: Double = sumOfChoose2ContingencyTable

  protected final val rowSum: Double = sumTargetRowCM.aggregate(0D)( (sum, v) => sum + MathC4E.binom(v, 2L), (sum1, sum2) => sum1 + sum2 )
  
  protected final val columnSum: Double = sumPredColumnsCM.aggregate(0D)( (sum, v) => sum + MathC4E.binom(v, 2L), (sum1, sum2) => sum1 + sum2 )
  
  protected final val maxIndex = (rowSum + columnSum) / 2D

  protected val expectedIndex: Double
  /**
   *
   */
  final lazy val arand: Double = (index - expectedIndex) / (maxIndex - expectedIndex)

}
/**
 *
 */
trait ARANDLocal extends ARANDCommons with ExternalIndicesLocalDataAncestor {

  protected val expectedIndex = (rowSum * columnSum) / MathC4E.binom(targetAndPred.size, 2)

}
/**
 *
 */
trait MultiClassesContingencyTableLocal extends ARANDLocal {
  /**
   *
   */
  final lazy val (targetClassesNumber, predClassesNumber): (Int, Int) = {
    val (tMinus1, pMinus1) = targetAndPred.aggregate((0, 0))( (acc, r) => (max(acc._1, r._1), max(acc._2, r._2)), (acc1, acc2) => (max(acc1._1, acc2._1), max(acc1._2, acc2._2)) )
    (tMinus1 + 1, pMinus1 + 1)
  }

  final lazy val contingencyTable: Array[Array[Long]] = targetAndPred.aggregate(emptyContingencyTable)(putInContingencyTable, fusionContingencyTables)
}
/**
 *
 */
trait BinaryAndMultiExternalIndicesLocalAncestor extends ExternalIndicesLocalDataAncestor with IndicesDoc {

	final lazy val purity: Double = {
		val cols = targetAndPred.groupBy(identity).map{ case ((_, pred), aggregate) => (pred, aggregate.size) }
		val groupByPred = cols.groupBy(_._1)
		val sum = groupByPred.map{ case (pred, aggregate) => (pred, aggregate.map(_._2).sum) }
		val maxByPred = groupByPred.map{ case (pred, aggregate) => (pred, aggregate.map(_._2).max) }
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
trait BinaryExternalIndicesLocalAncestor extends ExternalIndicesLocalDataAncestor with BinaryExternalIndicesAncestor with ContingencyMatricesBasicsIndices {

	final val (tp, tn, fp, fn) = {
		// val authorizedValues = immutable.Set(0, 1)
		// val isBinary = !targetAndPred.exists{ case (target, pred) => !authorizedValues.contains(target) || !authorizedValues.contains(pred) }
		// require(isBinary, println("You set a values other than {0, 1}"))
		val (tp0, tn0, fp0, fn0) = targetAndPred.map{ case (target, pred) => fillBinaryConfusionMatrix(target, pred) }.fold((0L, 0L, 0L, 0L))( (a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))
    (tp0.toLong, tn0.toLong, fp0.toLong, fn0.toLong)
	}
}
/**
 *
 */
final case class MultiExternalIndicesLocal(final val targetAndPred: GenSeq[(Int, Int)]) extends MultiClassesContingencyTableLocal with BinaryAndMultiExternalIndicesLocalAncestor
/**
 *
 */
final case class BinaryExternalIndicesLocal(final val targetAndPred: GenSeq[(Int, Int)]) extends BinaryExternalIndicesLocalAncestor with BinaryAndMultiExternalIndicesLocalAncestor
/**
 *
 */
object ExternalIndicesLocalUtils extends Serializable {
	/**
	 * Reformulate labels from any Int combination to usable one from 0 until l-1 with l number of distincts labels
	 */
	final def prepareLabels(labelsOrPred: GenSeq[Int]): (immutable.Map[Int, Int], GenSeq[Int]) = {
		val diffLabelsMapping = labelsOrPred.distinct.zipWithIndex.toMap
		(diffLabelsMapping.toSeq.seq.toMap, labelsOrPred.map(diffLabelsMapping(_)))
	}
}