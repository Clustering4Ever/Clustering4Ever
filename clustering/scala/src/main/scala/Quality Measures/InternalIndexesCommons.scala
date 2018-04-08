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
object InternalIndexesDBCommons
{
  /**
  * Measure of how good the clustering scheme is
  * Params:
  *  scatter1,scatter2: Double - the scatter value of cluster 1 and cluster 2
  *  center1,center2: Array[Double] - The centroid of cluster 1 and cluster 2
  **/
  def good(scatter1: Double, scatter2: Double, center1: Array[Double], center2: Array[Double], metric: ContinuousDistances) = (scatter1 + scatter2) / metric.d(center1, center2)

  /**
   * Scatter of point in cluster
   * Measure average distance to centroïd
   * @return Double - Scatter value
   **/
  def scatter(cluster: Array[Array[Double]], centroid: Array[Double], metric: ContinuousDistances) = ( for(p <- cluster) yield(metric.d(centroid, p)) ).sum / cluster.size
}