package clustering4ever.spark.indexes

import _root_.scala.math.max
import _root_.scala.collection.immutable.{HashMap, Map}
import _root_.scala.collection.{mutable, immutable}
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.scala.indexes.InternalIndexesDBCommons
import _root_.org.apache.spark.rdd.RDD
import _root_.org.apache.spark.SparkContext
/**
 * @author Beck Gaël
 * This object is used to compute internals clustering indexes as Davies Bouldin or Silhouette
 */
class InternalIndexes extends DataSetsTypes[Int, immutable.Vector[Double]]
{
  private def daviesBouldinIndexInside(sc: SparkContext, data: RDD[(Int, Vector)], clusterLabels: immutable.Vector[Int], metric: ContinuousDistances) =
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
      val centers = clusters.map{ case (k, v) => (k, SumArrays.obtainMean(v)) }
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
      val di = sc.parallelize(rijList.toSeq).aggregateByKey(neutralElement2)(addToBuffer2, aggregateBuff2).map{ case (_, goods)=> goods.reduce(max(_,_)) }
      val numCluster = clusterLabels.size
      val daviesBouldinIndex = di.fold(0)(_ + _) / numCluster
      daviesBouldinIndex
    }
  }
}

object InternalIndexes extends DataSetsTypes[Int, immutable.Vector[Double]]
{
  /**
   * Monothreaded version of davies bouldin index
   * Complexity O(n)
   **/
  def daviesBouldinIndex(sc: SparkContext, data: RDD[(ClusterID, Vector)], clusterLabels: immutable.Vector[Int], metric: ContinuousDistances) =
  {
    (new InternalIndexes).daviesBouldinIndexInside(sc, data, clusterLabels, metric)
  }

  /**
   * Monothreaded version of davies bouldin index
   * Complexity O(n)
   **/
  def daviesBouldinIndex(sc: SparkContext, data: RDD[(ClusterID, Vector)], metric: ContinuousDistances) =
  {
    val clusterLabels = data.map(_._1).distinct.collect.toVector
    (new InternalIndexes).daviesBouldinIndexInside(sc, data, clusterLabels, metric) 
  }

}