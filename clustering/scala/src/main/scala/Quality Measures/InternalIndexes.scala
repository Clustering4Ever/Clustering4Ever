package clustering4ever.scala.indexes

import scala.math.{pow, sqrt, max, min}
import scala.collection.{immutable, GenSeq, GenMap}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.clustering.ClusteringCommons
import clustering4ever.util.SumArrays

/**
 * @author Beck Gaël
 * This object is used to compute internals clustering indexes as Davies Bouldin or Silhouette
 */
class InternalIndexes extends ClusteringCommons
{
  def daviesBouldinIndex(data: GenSeq[(ClusterID, immutable.Seq[Double])], clusterLabels: immutable.Seq[Int], metric: ContinuousDistance[immutable.Seq[Double]]) =
  {
    if( clusterLabels.size == 1 )
    {
      println(" One Cluster found")
      0D
    }
    else
    {
      val clusters = data.par.groupBy(_._1).map{ case (k, v) => (k, v.map(_._2)) }
      val centers = clusters.map{ case (k, v) => (k, SumArrays.obtainMean(v)) }.toArray
      val scatters = clusters.zipWithIndex.map{ case ((k, v), idCLust) => (k, InternalIndexesDBCommons.scatter(v, centers(idCLust)._2, metric)) }
      val clustersWithCenterandScatters = (centers.map{ case (id, ar) => (id, (Some(ar), None)) } ++ scatters.map{ case (id, v) => (id, (None, Some(v))) })
        .par
        .groupBy(_._1)
        .map{ case (id, aggregate) => 
        {
          val agg = aggregate.map(_._2)
          val a = agg.head
          val b = agg.last
          if( a._1.isDefined ) (id, (b._2.get, a._1.get)) else (id, (a._2.get, b._1.get))
        }}
      val cart = for( i <- clustersWithCenterandScatters; j <- clustersWithCenterandScatters if( i._1 != j._1 ) ) yield (i, j)
      val rijList = for( ((idClust1, (centroid1, scatter1)), (idClust2, (centroid2, scatter2))) <- cart ) yield (idClust1, InternalIndexesDBCommons.good(centroid1, centroid2, scatter1, scatter2, metric))
      val di = rijList.groupBy(_._1).map{ case (_, goods) => goods.map(_._2).reduce(max(_,_)) }
      val numCluster = clusterLabels.size
      val daviesBouldinIndex = di.sum / numCluster
      daviesBouldinIndex
    }
  }

  def ballHallIndex(clusterized: GenSeq[(ClusterID, immutable.Seq[Double])], metric: ContinuousDistance[immutable.Seq[Double]] = new Euclidean(true)): Double =
  {
    val clusters = clusterized.par.groupBy(_._1).map{ case (clusterID, aggregate) => (clusterID, aggregate.map(_._2)) }

    val prototypes = clusters.map{ case (clusterID, aggregate) => (clusterID, SumArrays.obtainMean(aggregate)) }
    
    clusters.map{ case (clusterID, aggregate) => aggregate.map( v => metric.d(v, prototypes(clusterID)) ).sum / aggregate.size }.sum / clusters.size
  }

  /**
   * Silhouette Index
   * Complexity : O(n<sup>2</sup>)
   **/
  def silhouette(clusterLabels: immutable.Seq[Int], data: GenSeq[(Int, immutable.Seq[Double])], metric: ContinuousDistance[immutable.Seq[Double]]) =
  {  
    /*
     * Compute the  within-cluster mean distance a(i) for all the point in cluster
     * Param: cluster: RDD[Seq]
     * Return index of point and the corresponding a(i) Array[(Int, Double)]
     */
    def aiList(cluster: GenSeq[(Int, immutable.Seq[Double])]): GenMap[Int, Double] =
    {
      val pointPairs = for( i <- cluster; j <- cluster if( i._1 != j._1 ) ) yield (i,j)
      val allPointsDistances = pointPairs.map( pp => ((pp._1._1, pp._2._1), metric.d(pp._1._2, pp._2._2)) )
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
      val uniqData = data.par.zipWithIndex
      val (target, others) = uniqData.partition{ case ((clusterID, _), _) => clusterID == testedLabel }
      val cart = for( i <- target; j <- others ) yield (i, j)
      //get the sum distance between each point and other clusters
      val allDistances = cart.map{ case (((_, vector1), id1), ((clusterID2, vector2), _)) => ((id1, clusterID2), metric.d(vector1, vector2)) }.groupBy(_._1).map{ case (k,v)=> (k, v.map(_._2).sum) }
      // numbers of point of others clusters
      val numPoints = others.map( v => (v._1._1, 1) ).groupBy(_._1).map{ case (k, v)=> (k, v.map(_._2).sum) }
      //mean distance of point to the points of the other clusters 
      val deltas = allDistances.map( v => (v._1._1, v._2 / numPoints.getOrElse(v._1._2, 1)) )
      // Compute b(i) the smallest of these mean distances
      val bi = deltas.groupBy(_._1).map{ case (k, v) => (k, v.map(_._2).reduce(min(_, _))) }
      val ai = aiList(target.map(v => (v._2, v._1._2))).par.toSeq
      val si = (ai.map{ case (id, d) => (id, (Some(d), None)) } ++ bi.map{ case (id, d) => (id, (None, Some(d))) })
        .groupBy(_._1)
        .map{ case (id, aggregate) => 
        {
          val agg = aggregate.map(_._2)
          val a = agg.head
          val b = agg.last
          if( a._1.isDefined ) (id, (b._2.get, a._1.get)) else (id, (a._2.get, b._1.get))
        }}
        .map( x => (x._2._1 - x._2._2) / max(x._2._2, x._2._1) )
      val sk = si.sum / si.size
      sk
    }

    clusterLabels.map(sk).sum / clusterLabels.size
  }

}

object InternalIndexes extends ClusteringCommons
{
  /**
   * Davies bouldin index
   * Complexity O(n.c<sup>2</sup>) with:
   *   * n number of data points
   *   * c number of clusters
   */
  def daviesBouldinIndex(clusterized: GenSeq[(ClusterID, immutable.Seq[Double])], clusterLabels: immutable.Seq[Int], metric: ContinuousDistance[immutable.Seq[Double]]): Double =
    (new InternalIndexes).daviesBouldinIndex(clusterized, clusterLabels, metric)
  /**
   * Davies bouldin index
   * Complexity O(n.c<sup>2</sup>) with:
   *   * n number of data points
   *   * c number of clusters
   */
  def daviesBouldinIndex(clusterized: GenSeq[(ClusterID, immutable.Seq[Double])], metric: ContinuousDistance[immutable.Seq[Double]]): Double =
  {
    val clusterLabels = immutable.Seq(clusterized.map(_._1).distinct.seq:_*)
    (new InternalIndexes).daviesBouldinIndex(clusterized, clusterLabels, metric) 
  }

  def silhouetteIndex(clusterLabels: immutable.Seq[ClusterID], clusterized: Seq[(ClusterID, immutable.Seq[Double])], metric: ContinuousDistance[immutable.Seq[Double]]): Double =
    (new InternalIndexes).silhouette(clusterLabels, clusterized, metric)

  def ballHallIndex(clusterized: GenSeq[(ClusterID, immutable.Seq[Double])], metric: ContinuousDistance[immutable.Seq[Double]] = new Euclidean(true)) =
    (new InternalIndexes).ballHallIndex(clusterized, metric)

}