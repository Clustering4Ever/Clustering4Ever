package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable, immutable, Map}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.GVector
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.clustering.{DataExplorator, ClusteringSharedTypes}
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
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