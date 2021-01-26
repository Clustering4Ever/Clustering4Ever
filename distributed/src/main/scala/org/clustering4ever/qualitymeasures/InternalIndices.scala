package org.clustering4ever.qualitymeasures

/**
 * @author Beck Gaël
 */
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.clustering4ever.clusteringtraits.ClusteringSharedTypes
import org.clustering4ever.distances.Distance
import org.clustering4ever.qualitymeasures.InternalIndicesCommons
import org.clustering4ever.roottraits.{Clusterizable, GVector}
import org.clustering4ever.util.ClusterBasicOperations

import scala.collection.mutable
import scala.language.higherKinds
import scala.math.max
import scala.reflect.ClassTag

/**
 *
 */
trait InternalIndicesAncestorDistributed[V <: GVector[V], D <: Distance[V]] extends InternalIndicesCommons[V, D] {
  /**
   *
   */
  implicit val ct: ClassTag[V]  
  /**
   *
   */
  val metric: D
  /**
   *
   */
  private[this] def addToBuffer(buff: mutable.ArrayBuffer[V], elem: V) = buff += elem
  /**
   *
   */
  private[this] def aggregateBuff(buff1: mutable.ArrayBuffer[V], buff2: mutable.ArrayBuffer[V]) = buff1 ++= buff2
  /**
   *
   */
  protected val neutralElement = mutable.ArrayBuffer.empty[V]
  /**
   *
   */
  final def clustersIDs[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: RDD[Cz[O, V]], clusteringNumber: Int) = {
    mutable.ArraySeq(clusterized.map(_.clusterIDs(clusteringNumber)).distinct.collect:_*).sorted
  }
  /**
   *
   */
  final def obtainVectorsByClusterID[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: RDD[Cz[O, V]], clusteringNumber: Int): RDD[(ClusterID, mutable.ArrayBuffer[V])] = {
    clusterized.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) ).aggregateByKey(neutralElement)(addToBuffer, aggregateBuff)
  }
  /**
   *
   */
  final def daviesBouldin[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](sc: SparkContext, clusterized: RDD[Cz[O, V]], clusteringNumber: Int): Double = {
    if(clustersIDs(clusterized, clusteringNumber).size == 1) {
      println(" One Cluster found")
      0D
    }
    else {
      
      val neutralElement2 = mutable.ArrayBuffer.empty[Double]
      def addToBuffer2(buff: mutable.ArrayBuffer[Double], elem: Double) = buff += elem
      def aggregateBuff2(buff1: mutable.ArrayBuffer[Double], buff2: mutable.ArrayBuffer[Double]) = buff1 ++= buff2

      val clusters = obtainVectorsByClusterID(clusterized, clusteringNumber).collect
      val centers = clusters.map{ case (clusterID, cluster) => (clusterID, ClusterBasicOperations.obtainCenter(cluster, metric)) }
      val scatters = clusters.zipWithIndex.map{ case ((k, v), idCLust) => (k, scatter(v, centers(idCLust)._2, metric)) }
      val clustersWithCenterandScatters = (centers.map{ case (id, ar) => (id, (Some(ar), None)) } ++ scatters.map{ case (id, v) => (id, (None, Some(v))) })
        .par
        .groupBy(_._1)
        .map{ case (id, aggregate) =>
          val agg = aggregate.map(_._2)
          val a = agg.head
          val b = agg.last
          if(a._1.isDefined) (id, (b._2.get, a._1.get)) else (id, (a._2.get, b._1.get))
        }
      val cart = for(i <- clustersWithCenterandScatters; j <- clustersWithCenterandScatters if(i._1 != j._1)) yield (i, j)
      val rijList = sc.parallelize(cart.seq.toSeq).map{ case ((idClust1, (centroid1, scatter1)), (idClust2, (centroid2, scatter2))) => (idClust1, good(centroid1, centroid2, scatter1, scatter2, metric)) }
      val di = rijList.aggregateByKey(neutralElement2)(addToBuffer2, aggregateBuff2).map{ case (_, goods)=> goods.reduce(max(_, _)) }
      val numCluster = clustersIDs(clusterized, clusteringNumber).size
      val daviesBouldinIndex = di.sum / numCluster
      daviesBouldinIndex
    }
  }

  final def ballHall[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](clusterized: RDD[Cz[O, V]], clusteringNumber: Int): Double = {
    val neutralElement = mutable.ArrayBuffer.empty[V]
    val clusters = obtainVectorsByClusterID(clusterized, clusteringNumber).cache
    val prototypes = clusters.map{ case (clusterID, cluster) => (clusterID, ClusterBasicOperations.obtainCenter(cluster, metric)) }.collectAsMap
    clusters.map{ case (clusterID, aggregate) => aggregate.map( v => metric.d(v, prototypes(clusterID)) ).sum / aggregate.size }.sum / clusters.count
  }
}
/**
 * This object is used to compute internals clustering indices as Davies Bouldin or Silhouette
 */
final case class InternalIndicesDistributed[V <: GVector[V], D[A <: GVector[A]] <: Distance[A]](metric: D[V])(implicit val ct: ClassTag[V]) extends InternalIndicesAncestorDistributed[V, D[V]]
/**
 *
 */
object InternalIndicesDistributed extends ClusteringSharedTypes {
  /**
   * Monothreaded version of davies bouldin index
   * Complexity O(n.c<sup>2</sup>) with n number of individuals and c the number of clusters
   */
  final def daviesBouldin[O, V <: GVector[V] : ClassTag, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], D[A <: GVector[A]] <: Distance[A]](sc: SparkContext, clusterized: RDD[Cz[O, V]], metric: D[V], clusteringNumber: Int): Double = { 
    InternalIndicesDistributed(metric).daviesBouldin(sc, clusterized, clusteringNumber)
  }
  /**
   *
   */
  final def ballHall[O, V <: GVector[V] : ClassTag, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], D[A <: GVector[A]] <: Distance[A]](clusterized: RDD[Cz[O, V]], metric: D[V], clusteringNumber: Int): Double = {
    InternalIndicesDistributed(metric).ballHall(clusterized, clusteringNumber)
  }

}