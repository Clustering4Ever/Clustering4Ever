package clustering4ever.spark.indexes

import scala.math.max
import scala.collection.immutable.{HashMap, Map}
import scala.collection.{mutable, immutable}
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.util.SumArrays
import clustering4ever.scala.indexes.InternalIndexesDBCommons

/**
 * @author Beck Gaël
 * This object is used to compute internals clustering indexes as Davies Bouldin or Silhouette
 */
class InternalIndexes extends DataSetsTypes[Int, immutable.Seq[Double]]
{
  private def internalDaviesBouldinIndex(sc: SparkContext, data: RDD[(Int, Vector)], clusterLabels: immutable.Vector[Int], metric: ContinuousDistances = new Euclidean(true)) =
  {
    if( clusterLabels.size == 1 )
    {
      println(" One Cluster found")
      0D
    }
    else
    {
      val neutralElement = mutable.ArrayBuffer.empty[Vector]
      def addToBuffer(buff: mutable.ArrayBuffer[Vector], elem: Vector) = buff += elem
      def aggregateBuff(buff1: mutable.ArrayBuffer[Vector], buff2: mutable.ArrayBuffer[Vector]) = buff1 ++= buff2
      val neutralElement2 = mutable.ArrayBuffer.empty[Double]
      def addToBuffer2(buff: mutable.ArrayBuffer[Double], elem: Double) = buff += elem
      def aggregateBuff2(buff1: mutable.ArrayBuffer[Double], buff2: mutable.ArrayBuffer[Double]) = buff1 ++= buff2

      val clusters = data.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).collect
      val centers = clusters.map{ case (k, v) => (k, SumArrays.obtainMean(immutable.Seq(v:_*))) }
      val scatters = clusters.zipWithIndex.map{ case ((k, v), idCLust) => (k, InternalIndexesDBCommons.scatter(v, centers(idCLust)._2, metric)) }
      val clustersWithCenterandScatters = (
        centers.map{ case (id, ar) => (id, (Some(ar), None)) } ++ 
        scatters.map{ case (id, v) => (id, (None, Some(v))) }
        ).groupBy(_._1)
        .map{ case (id, rest) => (id, rest.map(_._2)) }
        .map{ case (id, rest) => (id, rest.head, rest.last) }
        .map{ case (id, a, b) => if( a._1.isDefined ) (id, (b._2.get, a._1.get)) else (id, (a._2.get, b._1.get)) }
      val cart = for( i <- clustersWithCenterandScatters; j <- clustersWithCenterandScatters if( i._1 != j._1 ) ) yield (i, j)
      val rijList = for( ((idClust1, (centroid1, scatter1)), (idClust2, (centroid2, scatter2))) <- cart ) yield (idClust1, InternalIndexesDBCommons.good(centroid1, centroid2, scatter1, scatter2, metric))
      val di = sc.parallelize(rijList.toSeq).aggregateByKey(neutralElement2)(addToBuffer2, aggregateBuff2).map{ case (_, goods)=> goods.reduce(max(_, _)) }
      val numCluster = clusterLabels.size
      val daviesBouldinIndex = di.fold(0)(_ + _) / numCluster
      daviesBouldinIndex
    }
  }

  private def internalBallHallIndex(clusterized: RDD[(ClusterID, immutable.Vector[Double])], metric: ContinuousDistances = new Euclidean(true)): Double =
  {
    val neutralElement = mutable.ArrayBuffer.empty[immutable.Seq[Double]]
    def addToBuffer(buff: mutable.ArrayBuffer[immutable.Seq[Double]], elem: immutable.Seq[Double]) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[immutable.Seq[Double]], buff2: mutable.ArrayBuffer[immutable.Seq[Double]]) = buff1 ++= buff2

    val clusters = clusterized.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).cache

    val prototypes = clusters.map{ case (clusterID, aggregate) =>
    (
      clusterID,
      SumArrays.obtainMean(immutable.Seq(aggregate:_*))
    )}.collectAsMap

    clusters.map{ case (clusterID, aggregate) => aggregate.map( v => metric.d(v, prototypes(clusterID)) ).sum / aggregate.size }.sum / clusters.count
  }
}

object InternalIndexes extends DataSetsTypes[Int, immutable.Seq[Double]]
{
  /**
   * Monothreaded version of davies bouldin index
   * Complexity O(n.c<sup>2</sup>) with n number of individuals and c the number of clusters
   **/
  def daviesBouldinIndexWithLabels(sc: SparkContext, clusterized: RDD[(ClusterID, Vector)], clusterLabels: immutable.Vector[Int], metric: ContinuousDistances = new Euclidean(true)): Double =
  {
    (new InternalIndexes).internalDaviesBouldinIndex(sc, clusterized, clusterLabels, metric)
  }

  /**
   * Monothreaded version of davies bouldin index
   * Complexity O(n.c<sup>2</sup>) with n number of individuals and c the number of clusters
   **/
  def daviesBouldinIndex(sc: SparkContext, clusterized: RDD[(ClusterID, Vector)], metric: ContinuousDistances = new Euclidean(true)): Double =
  {
    val clusterLabels = clusterized.map(_._1).distinct.collect.toVector
    daviesBouldinIndexWithLabels(sc, clusterized, clusterLabels, metric)
  }

  def ballHallIndex(clusterized: RDD[(ClusterID, immutable.Vector[Double])], metric: ContinuousDistances = new Euclidean(true)): Double =
  {
    (new InternalIndexes).internalBallHallIndex(clusterized, metric)
  }

}