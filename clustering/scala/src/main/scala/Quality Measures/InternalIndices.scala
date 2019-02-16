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
  val clustersIDsOp: Option[mutable.ArrayBuffer[Int]]
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
  def clustersIDsNumber[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: GenSeq[Cz[O, V]], clusteringNumber: Int) = if(clustersIDsOp.isDefined) clustersIDsOp.get else mutable.ArrayBuffer(clusterized.map(_.clusterIDs(clusteringNumber)).distinct.seq:_*).sorted
  /**
   *
   */
  def daviesBouldin[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: GenSeq[Cz[O, V]], clusteringNumber: Int): Double = {

    if(clustersIDsNumber(clusterized, clusteringNumber).size == 1) {
      println(" One Cluster found")
      0D
    }
    else {
      val clusters = obtainVectorsByClusterID(clusterized, clusteringNumber)
      val centers = clusters.map{ case (k, cluster) => (k, ClusterBasicOperations.obtainCenter(cluster, metric)) }.toArray
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
  def ballHall[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: GenSeq[Cz[O, V]], clusteringNumber: Int): Double = {
    val clusters = obtainVectorsByClusterID(clusterized, clusteringNumber)
    val prototypes = clusters.map{ case (clusterID, cluster) => (clusterID, ClusterBasicOperations.obtainCenter(cluster, metric)) }
    clusters.map{ case (clusterID, aggregate) => aggregate.map( v => metric.d(v, prototypes(clusterID)) ).sum / aggregate.size }.sum / clusters.size

  }
  /**
   * Silhouette Index
   * Complexity : O(n<sup>2</sup>)
   */
  def silhouette[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: GenSeq[Cz[O, V]], clusteringNumber: Int): Double = {  
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
 * @param clustersIDsOp
 * @tparam V
 * @tparam D
 */
final case class InternalIndicesLocal[V <: GVector[V], D[A <: GVector[A]] <: Distance[A]](metric: D[V], clustersIDsOp: Option[mutable.ArrayBuffer[Int]] = None) extends InternalIndicesAncestorLocal[V, D[V]]
/**
 *
 */
final case class InternalIndicesScalarLocal[V <: Seq[Double], D[A <: Seq[Double]] <: ContinuousDistance[A]](metric: D[V], clustersIDsOp: Option[mutable.ArrayBuffer[Int]] = None) extends InternalIndicesAncestorLocal[ScalarVector[V], D[V]]
/**
 *
 */
final case class InternalIndicesBinaryLocal[V <: Seq[Int], D[A <: Seq[Int]] <: BinaryDistance[A]](metric: D[V], clustersIDsOp: Option[mutable.ArrayBuffer[Int]] = None) extends InternalIndicesAncestorLocal[BinaryVector[V], D[V]]
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
  def silhouette[O, V <: GVector[V], Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], D[A <: GVector[A]] <: Distance[A]](clusterized: GenSeq[Cz[O, V]], metric: D[V], clusteringNumber: Int, clusterLabels: Option[Seq[ClusterID]] = None): Double = {
    InternalIndicesLocal(metric).silhouette(clusterized, clusteringNumber)
  }
  /**
   *
   */
  def ballHall[O, V <: GVector[V], Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], D[A <: GVector[A]] <: Distance[A]](clusterized: GenSeq[Cz[O, V]], metric: D[V], clusteringNumber: Int) = {
    InternalIndicesLocal(metric).ballHall(clusterized, clusteringNumber)
  }

}