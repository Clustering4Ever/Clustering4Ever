package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable, immutable, Map}
import shapeless.HMap
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.vectorizables.Vectorizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectorizations.{VectorizationAncestor, Vectorization, VectorizationLocal, EasyVectorizationLocal}
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.extensibleAlgorithmNature.ClusteringAlgorithmNature
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
    def addCardinalities(clusteringNumber: ClusteringNumber, cardinalities: immutable.Map[ClusterID, Long]) = cardinalitiesByClusteringNumber += ((clusteringNumber, cardinalities))
    /**
     * Map of clusters size proportion by clusteringNumber
     */
    val clustersProportionsByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[ClusterID, Double]]
    /**
     *
     */
    def addClustersProportions(clusteringNumber: ClusteringNumber, clustersProportions: immutable.Map[ClusterID, Double]) = clustersProportionsByClusteringNumber += ((clusteringNumber, clustersProportions))
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
    def addCentroids(clusteringNumber: ClusteringNumber, metricID: MetricID, centroids: immutable.Map[ClusterID, V]) = centroidsByClusteringNumber += (((clusteringNumber, metricID), centroids))
}
/**
 *
 */
case class ClusteringAnalysisBasicStatsKeeper(
    val cardinalitiesByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[Int, Long]] = mutable.HashMap.empty[ClusteringNumber, immutable.Map[Int, Long]],
    val clustersProportionsByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[Int, Double]] = mutable.HashMap.empty[ClusteringNumber, immutable.Map[Int, Double]]
) extends ClusteringBasicStats
/**
 *
 */
case class ClusteringBasicStatsKeeper[V <: GVector[V]](
    val cardinalitiesByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[Int, Long]] = mutable.HashMap.empty[ClusteringNumber, immutable.Map[Int, Long]],
    val clustersProportionsByClusteringNumber: mutable.HashMap[ClusteringNumber, immutable.Map[Int, Double]] = mutable.HashMap.empty[ClusteringNumber, immutable.Map[Int, Double]],
    val centroidsByClusteringNumber: mutable.HashMap[(ClusteringNumber, MetricID), immutable.Map[Int, V]] = mutable.HashMap.empty[(ClusteringNumber, MetricID), immutable.Map[Int, V]]
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
