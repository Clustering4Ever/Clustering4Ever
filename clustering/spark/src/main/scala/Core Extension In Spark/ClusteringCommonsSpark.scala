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
trait DistributedClusteringAlgorithm[V <: GVector[V], +CA <: ClusteringArgs, +CM <: ClusteringModelCz[V, RDD]] extends ClusteringAlgorithmCz[V, RDD, CA, CM] {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): CM

}
/**
 *
 */
trait ClusteringModelDistributed[V <: GVector[V]] extends ClusteringModelCz[V, RDD] {
	/**
	 *
	 */
	def obtainClusteringIDs[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): RDD[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last).asInstanceOf[RDD[ClusterID]]
	}
}
/**
 *
 */
class ClusteringInformationsDistributed(
	val clusteringInfo: immutable.Vector[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, RDD])] = immutable.Vector.empty[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, RDD])],
	val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double] = immutable.Map.empty[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double],
	val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double] = immutable.Map.empty[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]
) extends CollectionNature[RDD]