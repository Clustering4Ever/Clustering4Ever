package org.clustering4ever.clusteringtraits

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.Distance
import org.clustering4ever.roottraits.ClusteringNumberType._
import org.clustering4ever.roottraits.MetricIDType._
import org.clustering4ever.roottraits.{Clusterizable, GVector}

import _root_.scala.collection.{immutable, mutable}
import _root_.scala.language.higherKinds
/**
 *
 */
trait ClusteringBasicStats extends ClusteringSharedTypes {
    /**
     * Map of clusters cardinalities by clusteringNumber
     */
    val cardinalitiesByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[ClusterID, Long]]
    /**
     *
     */
    final def addCardinalities(clusteringNumber: ClusteringNumber, cardinalities: immutable.Map[ClusterID, Long]) = cardinalitiesByClusteringNumber += ((clusteringNumber, cardinalities))
    /**
     * Map of clusters size proportion by clusteringNumber
     */
    val clustersProportionsByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[ClusterID, Double]]
    /**
     *
     */
    final def addClustersProportions(clusteringNumber: ClusteringNumber, clustersProportions: immutable.Map[ClusterID, Double]) = {
        clustersProportionsByClusteringNumber += ((clusteringNumber, clustersProportions))
    }
}
/**
 *
 */
trait ClusteringBasicStatsExtended[V <: GVector[V]] extends ClusteringBasicStats {
    /**
     * Map of centroids by (clusteringNumber, metricID)
     */
    val centroidsByClusteringNumber: mutable.HashMap[(ClusteringNumber, MetricID), immutable.Map[ClusterID, V]]
    /**
     *
     */
    final def addCentroids(clusteringNumber: ClusteringNumber, metricID: MetricID, centroids: immutable.Map[ClusterID, V]) = centroidsByClusteringNumber += (((clusteringNumber, metricID), centroids))
}
/**
 *
 */
final case class ClusteringAnalysisBasicStatsKeeper(
    final val cardinalitiesByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[Int, Long]] = mutable.HashMap.empty[ClusteringNumber, immutable.Map[Int, Long]],
    final val clustersProportionsByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[Int, Double]] = mutable.HashMap.empty[ClusteringNumber, immutable.Map[Int, Double]]
) extends ClusteringBasicStats
/**
 *
 */
final case class ClusteringBasicStatsKeeper[V <: GVector[V]](
    final val cardinalitiesByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[Int, Long]] = mutable.HashMap.empty[ClusteringNumber, immutable.Map[Int, Long]],
    final val clustersProportionsByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[Int, Double]] = mutable.HashMap.empty[ClusteringNumber, immutable.Map[Int, Double]],
    final val centroidsByClusteringNumber: mutable.HashMap[(ClusteringNumber, MetricID), immutable.Map[Int, V]] = mutable.HashMap.empty[(ClusteringNumber, MetricID), immutable.Map[Int, V]]
) extends ClusteringBasicStatsExtended[V]
/**
 *
 */
trait ClustersAnalysis[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends DataExplorator[O, V, Cz, Collection] {
    /**
     *
     */
    val datasetSize: Long
    /**
     * Object which regroups clustering informations
     */
    val clustersBasicStatsKeeper: ClusteringBasicStatsKeeper[V]
    /**
     * @return cardianalities per cluster for a specific clusteringNumber
     */
    def cardinalities(clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, Long]
    /**
     * @return clusters size proportions for a specific clusteringNumber
     */
    def clustersProportions(clusteringNumber: ClusteringNumber): immutable.Map[Int, Double]
    /**
     * @return centroids for a specific metric and clusteringNumber
     */
    def centroids[D <: Distance[V]](metric: D, clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, V]

}
