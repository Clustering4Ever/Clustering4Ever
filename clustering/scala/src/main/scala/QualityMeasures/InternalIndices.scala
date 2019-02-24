package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.{max, min}
import scala.collection.{GenSeq, GenMap, mutable}
import scala.language.higherKinds
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.clustering.ClusteringSharedTypes
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.math.MathC4E
/**
 *
 */
trait ConfusionMatrixLocalCommons extends Serializable {
  /**
   *
   */
  final private[indices] def putInConfusionMatrix(confusionMatrix: Array[Array[Int]], targetAndPred: (Int, Int)): Array[Array[Int]] = {
    confusionMatrix(targetAndPred._1)(targetAndPred._2) += 1
    confusionMatrix
  }
  /**
   *
   */
  final private[indices] def fusionTwoConfusionMatrix(confusionMatrix1: Array[Array[Int]], confusionMatrix2: Array[Array[Int]]): Array[Array[Int]] = {
    confusionMatrix1.zipWithIndex.foreach{ case (row, i) => row.zipWithIndex.foreach{ case (v, j) => confusionMatrix2(i)(j) += v } }
    confusionMatrix2
  }
}
/**
 *
 */
trait InternalConfusionMatrix extends ConfusionMatrixLocalCommons {
  /**
   *
   */
  val targetAndPred: GenSeq[(Int, Int)]
  /**
   *
   */
  final lazy val (targetClassesNumber, predClassesNumber): (Int, Int) = targetAndPred.reduce( (a, b) => (max(a._1, b._1), max(a._2, b._2)) )
  /**
   *
   */
  final private lazy val emptyConfusionMatrix: Array[Array[Int]] = Array.fill(targetClassesNumber)(Array.fill(predClassesNumber)(0))
  /**
   *
   */
  final lazy val confusionMatrix: Array[Array[Int]] = targetAndPred.aggregate(emptyConfusionMatrix)(putInConfusionMatrix, fusionTwoConfusionMatrix)
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
  final lazy val sumTargetRowCM: Array[Int] = rangeTargetCM.map(confusionMatrix(_).sum).toArray
  /**
   *
   */
  final lazy val sumPredColumnsCM: Array[Int] = rangePredCM.map( i => rangeTargetCM.map( j => confusionMatrix(j)(i) ).sum ).toArray
  /**
   *
   */
  final lazy val arand: Double = {
    /**
     *
     */
    def computeIndex(m: Array[Array[Int]]): Int = {
      @annotation.tailrec
      def go(i: Int, j: Int, t: Int, sizeColumn: Int): Int = {
        val updt = t + MathC4E.binom(m(i)(j), 2)
        if(i >= 0 && j > 0) go(i, j - 1, updt, sizeColumn)
        else if(i > 0 && j == 0) go(i - 1, sizeColumn - 1, updt, sizeColumn)
        else updt
      }
      go(m.size - 1, m.size - 1, 0, m.size)
    }
    /**
     * TODO
     */
    def computeRowSums(m: Array[Array[Int]]): Array[Int] = {
      @annotation.tailrec
      def go(i: Int, j: Int, t: Array[Int], sizeColumn: Int): Array[Int] = {
        t(i) += MathC4E.binom(m(i)(j), 2)
        if(i >= 0 && j > 0) go(i, j - 1, t, sizeColumn)
        else if(i > 0 && j == 0) go(i - 1, sizeColumn - 1, t, sizeColumn)
        else t
      }
      go(m.size - 1, m.head.size - 1, Array.fill(m.head.size)(0), m.head.size)
    }
    /**
     * TODO
     */
    def computeColumnSums(m: Array[Array[Int]]): Array[Int] = {
      @annotation.tailrec
      def go(i: Int, j: Int, t: Array[Int], sizeColumn: Int): Array[Int] = {
        t(j) += MathC4E.binom(m(i)(j), 2)
        if(i >= 0 && j > 0) go(i, j - 1, t, sizeColumn)
        else if(i > 0 && j == 0) go(i - 1, sizeColumn - 1, t, sizeColumn)
        else t
      }
      go(m.size - 1, m.head.size - 1, Array.fill(m.size)(0), m.size)
    }
    
    // val index: Double = confusionMatrix.aggregate(0D)( (sum, row) => (sum + row.aggregate(0D)( (s, v) => s + MathC4E.binom(v, 2), (s1, s2) => s1 + s2 )), (sum1, sum2) => sum1 + sum2 )
    val index: Double = computeIndex(confusionMatrix)

    val rowSum: Double = sumTargetRowCM.aggregate(0D)( (sum, v) => sum + MathC4E.binom(v, 2), (sum1, sum2) => sum1 + sum2 )
    // val rowSum: Double = computeRowSums(sumTargetRowCM)
    
    val columnSum: Double = sumPredColumnsCM.aggregate(0D)( (sum, v) => sum + MathC4E.binom(v, 2), (sum1, sum2) => sum1 + sum2 )
    // val columnSum: Double = computeColumnSums(sumTargetRowCM)
    
    val expectedIndex = (rowSum * columnSum) / MathC4E.binom(targetAndPred.size, 2)

    val maxIndex = (rowSum + columnSum) / 2D
    
    (index - expectedIndex) / (maxIndex - expectedIndex)
  }

}

/**
 * @param metric
 * @param clustersIDsOp
 */
trait InternalIndicesAncestorLocal[V <: GVector[V], D <: Distance[V]] extends InternalIndicesCommons[V, D] {
  /**
   * The employed metric
   */
  val metric: D
  /**
   *
   */
  // val vectorsByClusterID = mutable.ArrayBuffer.empty[GenMap[ClusterID, GenSeq[V]]]
  /**
   *
   */
  protected def obtainVectorsByClusterID[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: GenSeq[Cz[O, V]], clusteringNumber: Int): GenMap[ClusterID, GenSeq[V]] = {
    clusterized.groupBy(_.clusterIDs(clusteringNumber)).map{ case (clusterID, aggregate) => (clusterID, aggregate.map(_.v)) }
  }
  /**
   *
   */
  final def clustersIDsNumber[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: GenSeq[Cz[O, V]], clusteringNumber: Int) = {
    mutable.ArrayBuffer(clusterized.map(_.clusterIDs(clusteringNumber)).distinct.seq:_*).sorted
  }
  /**
   *
   */
  final def daviesBouldin[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: GenSeq[Cz[O, V]], clusteringNumber: Int): Double = {

    if(clustersIDsNumber(clusterized, clusteringNumber).size == 1) {
      println(" One Cluster found")
      0D
    }
    else {
      val clusters = obtainVectorsByClusterID(clusterized, clusteringNumber)
      val centers = clusters.map{ case (k, cluster) => (k, ClusterBasicOperations.obtainMinimizingPoint(cluster, metric)) }.toArray
      val scatters = clusters.zipWithIndex.map{ case ((k, cluster), idCLust) => (k, scatter(cluster, centers(idCLust)._2, metric)) }
      val clustersWithCenterandScatters = (centers.map{ case (id, ar) => (id, (Some(ar), None)) } ++ scatters.map{ case (id, v) => (id, (None, Some(v))) })
        .par
        .groupBy(_._1)
        .map{ case (id, aggregate) => 
          val agg = aggregate.map(_._2)
          val a = agg.head
          val b = agg.last
          if(a._1.isDefined) (id, (b._2.get, a._1.get)) else (id, (a._2.get, b._1.get))
        }
      val cart = for( i <- clustersWithCenterandScatters; j <- clustersWithCenterandScatters if i._1 != j._1 ) yield (i, j)
      val rijList = cart.map{ case ((idClust1, (centroid1, scatter1)), (idClust2, (centroid2, scatter2))) => (idClust1, good(centroid1, centroid2, scatter1, scatter2, metric)) }
      val di = rijList.groupBy(_._1).map{ case (_, goods) => goods.map(_._2).reduce(max(_,_)) }
      val numCluster = clustersIDsNumber(clusterized, clusteringNumber).size
      val daviesBouldinIndex = di.sum / numCluster
      daviesBouldinIndex
    }

  }
  /**
   *
   */
  final def ballHall[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: GenSeq[Cz[O, V]], clusteringNumber: Int): Double = {
    val clusters = obtainVectorsByClusterID(clusterized, clusteringNumber)
    val prototypes = clusters.map{ case (clusterID, cluster) => (clusterID, ClusterBasicOperations.obtainMinimizingPoint(cluster, metric)) }
    clusters.map{ case (clusterID, aggregate) => aggregate.map( v => metric.d(v, prototypes(clusterID)) ).sum / aggregate.size }.sum / clusters.size

  }
  // TO FIX
  /**
   * Silhouette Index
   * Complexity : O(n<sup>2</sup>)
   */
  private final def silhouette[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: GenSeq[Cz[O, V]], clusteringNumber: Int): Double = {  
    /*
     * Compute the  within-cluster mean distance a(i) for all the point in cluster
     * Param: cluster: RDD[Seq]
     * Return index of point and the corresponding a(i) Array[(Int, Double)]
     */
    def aiList(cluster: Seq[(Int, V)]): Map[Int, Double] = {
      val pointPairs = for(i <- cluster; j <- cluster if i._1 != j._1) yield (i, j)
      val allPointsDistances = pointPairs.map( pp => ((pp._1._1, pp._2._1), metric.d(pp._1._2, pp._2._2)) )
      val totalDistanceList = allPointsDistances.map(v => (v._1._1, v._2)).groupBy(_._1).map{ case (k, v) => (k, v.map(_._2).sum) }
      val count = totalDistanceList.size
      val aiList = totalDistanceList.map{ case (k, v) => (k, (v / (count - 1))) }
      aiList
    }
    /** 
     * The mean of the silhouette widths for a given cluster
     * testedLabel the cluster label that we want to compute
     *
     */
    def sk(testedLabel: Int): Double = {
      val uniqclusterized = clusterized.zipWithIndex
      val (target, others) = uniqclusterized.partition{ case (cz, _) => cz.clusterIDs(clusteringNumber) == testedLabel }
      val cart = for( i <- target; j <- others ) yield (i, j)
      val allDistances: GenMap[(Int, Int), Double] = cart.map{ case ((cz1, id1), (cz2, _)) => ((id1, cz2.clusterIDs(clusteringNumber)), metric.d(cz1.v, cz2.v)) }.groupBy(_._1).map{ case (k, v)=> (k, v.map(_._2).sum) }
      val numPoints = others.groupBy{ case (cz, _) => cz.clusterIDs(clusteringNumber) }.map{ case (clusterID, aggregate)=> (clusterID, aggregate.size) }
      val deltas = allDistances.map( v => (v._1._1, v._2 / numPoints.getOrElse(v._1._2, 1)) )
      val bi = deltas.groupBy(_._1).map{ case (k, v) => (k, v.map(_._2).reduce(min(_, _))) }
      val ai = aiList(target.map{ case (cz, id) => (id, cz.v) }.seq).par
      val si = (ai.map{ case (id, d) => (id, (Some(d), None)) } ++ bi.map{ case (id, d) => (id, (None, Some(d))) })
        .groupBy(_._1)
        .map{ case (id, aggregate) => 
          val agg = aggregate.map(_._2)
          val a = agg.head
          val b = agg.last
          // To fixxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
          if(a._1.isDefined) (id, (b._2.get, a._1.get)) else (id, (a._2.get, b._1.get))
        }
        .map( x => (x._2._1 - x._2._2) / max(x._2._2, x._2._1) )
      val sk = si.sum / si.size
      sk
    }
    val distinctClusterIDs = clustersIDsNumber(clusterized, clusteringNumber)
    distinctClusterIDs.map(sk).sum / distinctClusterIDs.size
  }

}
/**
 * @param metric
 * @tparam V
 * @tparam D
 */
final case class InternalIndicesLocal[V <: GVector[V], D[A <: GVector[A]] <: Distance[A]](metric: D[V]) extends InternalIndicesAncestorLocal[V, D[V]]
/**
 *
 * @tparam V
 * @tparam D
 */
final case class InternalIndicesScalarLocal[V <: Seq[Double], D[A <: Seq[Double]] <: ContinuousDistance[A]](metric: D[V]) extends InternalIndicesAncestorLocal[ScalarVector[V], D[V]]
/**
 *
 * @tparam V
 * @tparam D
 */
final case class InternalIndicesBinaryLocal[V <: Seq[Int], D[A <: Seq[Int]] <: BinaryDistance[A]](metric: D[V]) extends InternalIndicesAncestorLocal[BinaryVector[V], D[V]]
/**
 *
 */
object InternalIndicesLocal extends ClusteringSharedTypes {
  /**
   * Davies bouldin index
   * Complexity O(n.c<sup>2</sup>) with:
   *   * n number of clusterized points
   *   * c number of clusters
   */
  def daviesBouldin[O, V <: GVector[V], Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], D[A <: GVector[A]] <: Distance[A]](clusterized: GenSeq[Cz[O, V]], metric: D[V], clusteringNumber: Int, clusterLabels: Option[Seq[ClusterID]] = None): Double = {
    InternalIndicesLocal(metric).daviesBouldin(clusterized, clusteringNumber)
  }
  /**
   *
   */
  // def silhouette[O, V <: GVector[V], Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], D[A <: GVector[A]] <: Distance[A]](clusterized: GenSeq[Cz[O, V]], metric: D[V], clusteringNumber: Int, clusterLabels: Option[Seq[ClusterID]] = None): Double = {
  //   InternalIndicesLocal(metric).silhouette(clusterized, clusteringNumber)
  // }
  /**
   *
   */
  def ballHall[O, V <: GVector[V], Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], D[A <: GVector[A]] <: Distance[A]](clusterized: GenSeq[Cz[O, V]], metric: D[V], clusteringNumber: Int) = {
    InternalIndicesLocal(metric).ballHall(clusterized, clusteringNumber)
  }

}