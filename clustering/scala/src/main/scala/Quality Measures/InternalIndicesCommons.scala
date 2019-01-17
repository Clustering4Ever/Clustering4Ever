package org.clustering4ever.indices
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import org.clustering4ever.math.distances.GenericDistance
import org.clustering4ever.clustering.ClusteringCommons
/**
 * This object is used to compute internals clustering indices as Davies Bouldin or Silhouette
 */
trait InternalIndicesCommons[O, D <: GenericDistance[O]] extends ClusteringCommons {
  /**
   * Measure of how good the clustering scheme is
   * Params:
   *  scatter1,scatter2: Double - the scatter value of cluster 1 and cluster 2
   *  center1,center2: Array[Double] - The centroid of cluster 1 and cluster 2
   */
  protected def good(scatter1: Double, scatter2: Double, center1: O, center2: O, metric: D): Double = (scatter1 + scatter2) / metric.d(center1, center2)
  /**
   * Scatter of point in cluster
   * Measure average distance to centroïd
   * @return Double - Scatter value
   */
  protected def scatter(cluster: GenSeq[O], centroid: O, metric: D): Double = cluster.map(metric.d(centroid, _)).sum / cluster.size

}