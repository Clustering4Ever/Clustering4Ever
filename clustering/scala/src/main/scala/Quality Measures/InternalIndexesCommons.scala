package clustering4ever.scala.indexes

import scala.collection.{immutable, GenSeq}
import clustering4ever.math.distances.ContinuousDistance

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
  def good(scatter1: Double, scatter2: Double, center1: Seq[Double], center2: Seq[Double], metric: ContinuousDistance): Double = (scatter1 + scatter2) / metric.d(center1, center2)

  /**
   * Scatter of point in cluster
   * Measure average distance to centroïd
   * @return Double - Scatter value
   **/
  def scatter(cluster: GenSeq[Seq[Double]], centroid: Seq[Double], metric: ContinuousDistance): Double = cluster.map(metric.d(centroid, _)).sum / cluster.size
}