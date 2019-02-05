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
	type Self <: ClustersInternalIndicesAnalysis[O, V, Cz, Collection]
	/**
	 *
	 */
	val internalsIndicesByMetricClusteringNumberIndex: immutable.Map[(MetricID, ClusterID, InternalsIndicesType), Double]
    /**
     * Compute given internals indices and add result to internalsIndicesByClusteringNumber
     * @return A Map which link internal indices to its associate value 
     */
    def saveInternalsIndices[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): Self
    /**
     *
     */
    def computeInternalsIndicesForEveryClusteringNumber[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*): Seq[immutable.Map[InternalsIndicesType, Double]]
	/**
	 *
	 */
	def obtainInternalsIndices[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): immutable.Map[InternalsIndicesType, Double]

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
	type Self <: ClustersExternalIndicesAnalysis[O, V, Cz, Collection]
	/**
	 *
	 */
	val externalsIndicesByClusteringNumber = mutable.Map.empty[Int, immutable.Map[ExternalsIndicesType, Double]]
    /**
     *
     */
    def saveInternalsIndicesForEveryClusteringNumber[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*): Self
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
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends ClustersInternalIndicesAnalysis[O, V, Cz, Collection] with ClustersExternalIndicesAnalysis[O, V, Cz, Collection] {
	/**
	 *
	 */
	type Self <: ClustersIndicesAnalysis[O, V, Cz, Collection]

}