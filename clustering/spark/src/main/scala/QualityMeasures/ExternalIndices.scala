package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.annotation.meta.param
import scala.collection.parallel.mutable.ParArray
import scala.collection.mutable
import scala.math.{max, log, sqrt}
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.enums.NmiNormalizationNature._
import org.clustering4ever.util.ClusteringIndicesCommons
import org.clustering4ever.math.MathC4E
/**
 *
 */
trait ExternalIndicesDistributedDataAncestor extends TPTNFPFNAncestor {
  /**
   * StorageLevel for RDD
   */
  val persistanceLVL: StorageLevel
  /**
   * RDD of (Target, Prediction)
   */
  val targetAndPred: RDD[(Int, Int)]

  targetAndPred.persist(persistanceLVL)
}
/**
 *
 */
trait BinaryExternalIndicesDistributedAncestor extends ExternalIndicesDistributedDataAncestor with BinaryExternalIndicesAncestor with ContingencyMatricesBasicsIndices {

  final lazy val (tp, tn, fp, fn) = {
    targetAndPred.map{ case (target, pred) => fillBinaryConfusionMatrix(target, pred) }.fold((0L, 0L, 0L, 0L))( (a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4))
  }

}
/**
 *
 */
trait ARANDDistributed extends ARANDCommons with ExternalIndicesDistributedDataAncestor {

  protected final val expectedIndex = (rowSum * columnSum) / MathC4E.binom(targetAndPred.count, 2)

}

/**
 *
 */
trait MultiClassesContingencyTableDistributed extends ARANDDistributed {
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
trait BinaryAndMultiExternalIndicesDistributedAncestor extends ExternalIndicesDistributedDataAncestor with IndicesDoc {
  
  final lazy val purity: Double = {
    val cols = targetAndPred.map( line => (line, 1) ).reduceByKey(_ + _).map{ case ((_, pred), sum) => (pred, sum) }.cache
    val sum = cols.reduceByKeyLocally(_ + _)
    val maxByPred = cols.reduceByKeyLocally(max)
    cols.unpersist(false)
    sum.zip(maxByPred).map{ case ((_, s1), (_, m1)) => m1.toDouble / s1 }.sum / sum.size
  }

  final lazy val (mutualInformation, et, ep): (Double, Double, Double) = {
    val maxX = targetAndPred.max()(Ordering[Int].on(_._1))._1
    val maxY = targetAndPred.max()(Ordering[Int].on(_._2))._2

    val maxOneIndices = (0 to maxX).toArray
    val maxTwoIndices = (0 to maxY).toArray

    @transient val sc = targetAndPred.sparkContext

    val accNmi = new NmiAccumulator(mutable.ArrayBuffer.fill(maxX + 1)(mutable.ArrayBuffer.fill(maxY + 1)(0D)), maxX + 1, maxY + 1)
    sc.register(accNmi, "NmiAccumulator")
    targetAndPred.foreach{ case (x, y) => accNmi.addOne(x, y) }

    val count = accNmi.value

    val ai = ClusteringIndicesCommons.nmiObtainAi(new Array[Double](maxX + 1), maxOneIndices, maxTwoIndices, count)
    val bj = ClusteringIndicesCommons.nmiObtainBj(new Array[Double](maxY + 1), maxTwoIndices, maxOneIndices, count)

    val aiSum = ai.sum

    val et = ClusteringIndicesCommons.nmiIn1(ai, aiSum)
    val ep = ClusteringIndicesCommons.nmiIn1(bj, aiSum)
    val etStricep = ClusteringIndicesCommons.nmiIn2(maxOneIndices, maxTwoIndices, count, aiSum, bj)
    val mi = et - etStricep

    (mi, et, ep)
  }

	final lazy val nmi: Normalization => Double = normalization => {
		val nmi = normalization match {
			case SQRT => mutualInformation / sqrt(et * ep)
			case MAX => mutualInformation / max(et, ep)
		}
		nmi
  }

  final lazy val nmiSQRT: Double = nmi(SQRT)

  final lazy val nmiMAX: Double = nmi(MAX)
}
/**
 *
 */
final case class MultiExternalIndicesDistributed(final val targetAndPred: RDD[(Int, Int)], final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY) extends BinaryAndMultiExternalIndicesDistributedAncestor with MultiClassesContingencyTableDistributed
/**
 *
 */
final case class BinaryExternalIndicesDistributed(final val targetAndPred: RDD[(Int, Int)], final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY) extends BinaryExternalIndicesDistributedAncestor with BinaryAndMultiExternalIndicesDistributedAncestor
/**
 *
 */
object ExternalIndicesDistributedUtils {
	/**
	 * Prepare labels in order to get them in the range 0 -> n-1 rather than random labels values
	 */
	final def prepareLabels(x: RDD[Int]) = {
		val indexedValuesMap = x.distinct.zipWithIndex.collectAsMap
		(indexedValuesMap, x.map(indexedValuesMap))
	}
}