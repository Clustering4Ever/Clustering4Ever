package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
import org.clustering4ever.clusteringtraits.ClusteringSharedTypes
import org.clustering4ever.distances.Distance
import org.clustering4ever.roottraits.ClusteringNumberType._
import org.clustering4ever.roottraits.ExternalsIndices._
import org.clustering4ever.roottraits.InternalsIndices._
import org.clustering4ever.roottraits.MetricIDType._

import scala.collection.{Map, immutable, mutable}
import scala.language.higherKinds
/**
 *
 */
trait ClustersIndicesAnalysisAncestor[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends ClusteringSharedTypes {
    /**
     *
     */
    val clusterized: Collection[Cz[O, V]]
    // val clusteringInformations: ClusteringInformations[ID, O, V, Cz, Collection]
}
/**
 *
 */
trait ClustersInternalIndicesAnalysis[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends ClustersIndicesAnalysisAncestor[O, V, Cz, Collection] {
	/**
	 *
	 */
	final val internalsIndicesByMetricClusteringNumberIndex = mutable.HashMap.empty[(MetricID, ClusterID, InternalsIndicesType), Double]
    /**
     * @param metric metric employed
     * @param indices indices explored
     */
    def computeInternalsIndicesOfEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Seq[immutable.Map[InternalsIndicesType, Double]]
    /**
     * @param metric metric employed
     * @param indices indices explored
     * @param clusteringNumber cluster explored
	 * @return interal indices for a given clusteringNumber
	 */
	def obtainInternalsIndices[D <: Distance[V]](metric: D, clusteringNumber: ClusteringNumber, indices: InternalsIndicesType*): immutable.Map[InternalsIndicesType, Double]

}
/**
 *
 */
trait ClustersExternalIndicesAnalysis[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends ClustersIndicesAnalysisAncestor[O, V, Cz, Collection] {
	/**
	 *
	 */
	val externalsIndicesByClusteringNumber = mutable.HashMap.empty[Int, immutable.Map[ExternalsIndicesType, Double]]
    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    def computeExternalsIndices(groundTruth: Collection[ClusterID], clusteringNumber: ClusteringNumber, indices: ExternalsIndicesType*): immutable.Map[ExternalsIndicesType, Double]
    /**
     *
     */
    def computeExternalsIndicesOfEveryClusteringNumber(groundTruth: Collection[ClusterID], indices: ExternalsIndicesType*): Seq[Map[ExternalsIndicesType, Double]]

}
/**
 *
 */
trait ClustersIndicesAnalysis[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends ClustersInternalIndicesAnalysis[O, V, Cz, Collection] with ClustersExternalIndicesAnalysis[O, V, Cz, Collection]