package clustering4ever.scala.indexes

import _root_.scala.math.{pow, sqrt, max, min}
import _root_.scala.collection.parallel.mutable.ParArray
import _root_.scala.collection.immutable.{HashMap, Map}
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.util.SumArrays

/**
 * @author Beck Gaël
 * This object is used to compute internals clustering indexes as Davies Bouldin or Silhouette
 */
class InternalIndexes extends DataSetsTypes[Int, Double]
{
  /**
  * Measure of how good the clustering scheme is
  * Params:
  *  scatter1,scatter2: Double - the scatter value of cluster 1 and cluster 2
  *  center1,center2: Array[Double] - The centroid of cluster 1 and cluster 2
  **/
  private[this] def good(scatter1: Double, scatter2: Double, center1: Array[Double], center2: Array[Double], metric: ContinuousDistances) = (scatter1 + scatter2) / metric.d(center1, center2)

  /**
   * Scatter of point in cluster
   * Measure average distance to centroïd
   * @return Double - Scatter value
   **/
  private[this] def scatter(cluster: Array[Array[Double]], centroid: Array[Double], metric: ContinuousDistances) = ( for(p <- cluster) yield(metric.d(centroid, p)) ).sum / cluster.size



  private def daviesBouldinIndexInside(data: Array[(Int, Array[Double])], clusterLabels: Array[Int], metric: ContinuousDistances) =
  {
  
    if( clusterLabels.size == 1 )
    {
      println(" One Cluster found")
      0D
    }
    else
    {
      val clusters = data.groupBy(_._1).map{ case (k, v) => (k, v.map(_._2)) }.toParArray
      val centers = clusters.map{ case (k, v) => (k, SumArrays.obtainCentroid(v)) }
      val scatters = clusters.zipWithIndex.map{ case ((k, v), idCLust) => (k, scatter(v, centers(idCLust)._2, metric)) }
      val clustersWithCenterandScatters = (
        centers.map{ case (id, ar) => (id, (Some(ar), None)) } ++ 
        scatters.map{ case (id, v) => (id, (None, Some(v))) }
        ).groupBy(_._1)
        .map{ case (id, rest) => (id, rest.map(_._2)) }
        .map{ case (id, rest) => (id, rest.head, rest.last) }
        .map{ case (id, a, b) => if( a._1.isDefined ) (id, (b._2.get, a._1.get)) else (id, (a._2.get, b._1.get)) }
      val cart = ( for( i <- clustersWithCenterandScatters; j <- clustersWithCenterandScatters if( i._1 != j._1 ) ) yield( (i, j) ) )
      val rijList = for( ((idClust1, (centroid1, scatter1)), (idClust2, (centroid2, scatter2))) <- cart ) yield( ((idClust1, idClust2), good(centroid1, centroid2, scatter1, scatter2, metric)) )
      val di = (for( ((idClust1, _), good) <- rijList) yield((idClust1, good))).groupBy(_._1).map{ case (idClust, goods)=> (idClust, goods.map(_._2).reduce(max(_,_))) }
      val numCluster = clusterLabels.size
      val daviesBouldinIndex = di.map(_._2).sum / numCluster
      daviesBouldinIndex
    }
  }


  /**
   * Silhouette Index
   * Complexity : O(n<sup>2</sup>)
   **/
  def silhouette(clusterLabels: Array[Int], data: Array[(Int, Array[Double])], metric: ContinuousDistances) =
  {  
    /*
     * Compute the  within-cluster mean distance a(i) for all the point in cluster
     * Param: cluster: RDD[Array[Double]]
     * Return index of point and the corresponding a(i) Array[(Int, Double)]
     */
    def aiList(cluster:Array[(Int, Array[Double])]) =
    {
      val pointPairs = for( i <- cluster; j <- cluster if( i._1 != j._1 ) ) yield( (i,j) )
      val allPointsDistances = for( pp <- pointPairs ) yield( ((pp._1._1, pp._2._1), metric.d(pp._1._2, pp._2._2)) )
      val totalDistanceList = allPointsDistances.map(v => (v._1._1, v._2)).groupBy(_._1).map{ case (k, v) => (k, v.map(_._2).sum) }
      val count = totalDistanceList.size
      val aiList = totalDistanceList.map{ case (k, v) => (k, (v / (count - 1))) }
      aiList
    }
  
    /*
     * The mean of the silhouette widths for a given cluster
     * @param : label: Int - the cluster label that we want to compute
     * @return :  Double
     */
    def sk(testedLabel:Int) =
    {
      val uniqData = data.zipWithIndex
      val (target, others) = uniqData.partition{ case ((clusterID, _), _) => clusterID == testedLabel }
      //val target_other = for(v<-uniqData) yield(if(v._1._1==label) (1,v) else (0,v))
      val cart = for( i <- target; j <- others ) yield( (i, j) )

      //get the sum distance between each point and other clusters
      val allDistances = cart.map{ case (((_, vector1), id1), ((clusterID2, vector2), _)) => ((id1, clusterID2), metric.d(vector1, vector2)) }.groupBy(_._1).map{ case (k,v)=> (k, v.map(_._2).sum) }.toArray
      // numbers of point of others clusters
      val numPoints = others.map( v => (v._1._1, 1) ).groupBy(_._1).map{ case (k, v)=> (k, v.map(_._2).sum) }
      //mean distance of point to the points of the other clusters 
      val deltas = allDistances.map( v => (v._1._1, v._2 / numPoints.getOrElse(v._1._2, 1)) )
      // Compute b(i) the smallest of these mean distances
      val bi = deltas.groupBy(_._1).map{ case (k, v) => (k, v.map(_._2).reduce(min(_, _))) }
      val ai = aiList(target.map(v => (v._2, v._1._2)))
      val si = (
        ai.toSeq.map{ case (id, d) => (id, (Some(d), None)) } ++
        bi.toSeq.map{ case (id, d) => (id, (None, Some(d))) }
        )
        .groupBy(_._1)
        .map{ case (id, rest) => (id, rest.map(_._2)) }.map{ case (id, rest) => (id, rest.head, rest.last) }
        .map{ case (id, a, b) => if( a._1.isDefined ) (id, (b._2.get, a._1.get)) else (id, (a._2.get, b._1.get)) }
        .map( x => (x._2._1 - x._2._2) / max(x._2._2, x._2._1) )
      val sk = si.sum / si.size
      sk
    }

    clusterLabels.map(sk).sum / clusterLabels.size
  }

}

object InternalIndexes extends DataSetsTypes[Int, Double]
{
  /**
   * Monothreaded version of davies bouldin index
   * Complexity O(n)
   **/
  def daviesBouldinIndex(data: Array[(ClusterID, Array[Double])], clusterLabels: Array[Int], metric: ContinuousDistances) =
  {
    (new InternalIndexes).daviesBouldinIndexInside(data, clusterLabels, metric)
  }

  /**
   * Monothreaded version of davies bouldin index
   * Complexity O(n)
   **/
  def daviesBouldinIndex(data: Array[(ClusterID, Array[Double])], metric: ContinuousDistances) =
  {
    val clusterLabels = data.map(_._1).distinct.toArray
    (new InternalIndexes).daviesBouldinIndexInside(data, clusterLabels, metric) 
  }

  def silhouette(clusterLabels: Array[Int], data: Array[(Int, Array[Double])], metric: ContinuousDistances) =
  {
    (new InternalIndexes).silhouette(clusterLabels, data, metric)
  }

}