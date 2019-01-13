package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import org.apache.spark.rdd.RDD
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectors.GVector
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.vectorizations.EmployedVectorization
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait DistributedClusteringAlgorithm[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], +CA <: ClusteringArgs[V], +CM <: ClusteringModelCz[ID, O, V, Cz, RDD]] extends ClusteringAlgorithmCz[ID, O, V, Cz, RDD, CA, CM] {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run(data: RDD[Cz[ID, O, V]]): CM

}
/**
 *
 */
trait ClusteringModelDistributed[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]] extends ClusteringModelCz[ID, O, V, Cz, RDD] {
	/**
	 *
	 */
	def obtainClusteringIDs(data: RDD[Cz[ID, O, V]]): RDD[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last)
	}
}
/**
 *
 */
case class ClusteringInformationsDistributed[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](
	val clusteringInformations: immutable.Vector[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs[_], ClusteringModelCz[ID, O, _, Cz, RDD])] = immutable.Vector.empty[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs[_], ClusteringModelCz[ID, O, _, Cz, RDD])]
	// val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double] = immutable.Map.empty[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double],
	// val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double] = immutable.Map.empty[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]
) extends ClusteringInformations[ID, O, Cz, RDD]

/**
 *
 */
trait ClusteringArgsDistributed[V <: GVector[V]] extends ClusteringArgs[V] {
	/**
	 *
	 */
	type CM[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]] <: ClusteringModelDistributed[ID, O, V, Cz]
	/**
	 *
	 */
	// def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): DistributedClusteringAlgorithm[ID, O, V, Cz, CA, CM[ID, O, Cz]]
}