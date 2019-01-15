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
import org.clustering4ever.clustering.{DataExplorator, ClusteringCommons}
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.clustering.ClusteringInformations
/**
 *
 */
trait ClustersIndicesAnalysisAncestor[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Collection[_]
] {
    val clusteringInformations: ClusteringInformations[ID, O, Cz, Collection]
}
/**
 *
 */
trait ClustersInternalIndicesAnalysis[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] with ClusteringCommons {
	/**
	 *
	 */
	type Self <: ClustersInternalIndicesAnalysis[ID, O, V, Cz, Collection]
	/**
	 *
	 */
	val internalsIndicesByMetricClusteringNumberIndex: immutable.Map[(MetricID, ClusterID, InternalsIndicesType), Double]
    /**
     * Compute given internals indices and add result to internalsIndicesByClusteringNumber
     * @return A Map which link internal indices to its associate value 
     */
    def saveInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): Self
    /**
     *
     */
    def computeInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Seq[immutable.Map[InternalsIndicesType, Double]]
	/**
	 *
	 */
	def obtainInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): immutable.Map[InternalsIndicesType, Double]

}
/**
 *
 */
trait ClustersExternalIndicesAnalysis[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] with ClusteringCommons {
	/**
	 *
	 */
	type Self <: ClustersExternalIndicesAnalysis[ID, O, V, Cz, Collection]
	/**
	 *
	 */
	val externalsIndicesByClusteringNumber = mutable.Map.empty[Int, immutable.Map[ExternalsIndicesType, Double]]
    /**
     *
     */
    def saveInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Self
    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    def computeExternalsIndices(groundTruth: Collection[ClusterID], indices: ExternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): immutable.Map[ExternalsIndicesType, Double]
    /**
     *
     */
    def computeExternalsIndicesForEveryClusteringNumber(groundTruth: Collection[ClusterID], indices: ExternalsIndicesType*): Seq[Map[ExternalsIndicesType, Double]]

}
/**
 *
 */
trait ClustersIndicesAnalysis[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] with ClustersInternalIndicesAnalysis[ID, O, V, Cz, Collection] with ClustersExternalIndicesAnalysis[ID, O, V, Cz, Collection] {
	/**
	 *
	 */
	type Self <: ClustersIndicesAnalysis[ID, O, V, Cz, Collection]

}