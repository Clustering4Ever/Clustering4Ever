package org.clustering4ever.clustering.dataset
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Row
import org.apache.spark.sql.Encoders
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.shapeless.{VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.vectors.GVector
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.vectorizations.Vectorization
import org.apache.spark.sql.Dataset
import org.clustering4ever.clustering.{ClusteringAlgorithm, ClusteringModel}
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait ClusteringAlgorithmDistributedDS[V <: GVector[V], CA <: ClusteringModelDistributedDS[V]] extends ClusteringAlgorithm {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return GenericClusteringModel
	 */
	def run[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): CA

}
/**
 *
 */
trait ClusteringModelDistributedDS[V <: GVector[V]] extends ClusteringModel {
	/**
	 * kryo Serialization if true, java one else
	 */
	val kryoSerialization: Boolean
	/**
	 * kryo Serialization if true, java one else
	 */
	val encoderClusterIDs = if(kryoSerialization) Encoders.kryo[Int] else Encoders.javaSerialization[Int]
	/**
	 * General methods to obtain a clustering from the model in order to measure performances scores
	 */
	protected[clustering] def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): Dataset[Cz[O, V]]
	/**
	 *
	 */
	protected[clustering] def obtainClusteringIDs[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): Dataset[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last)(encoderClusterIDs)
	}
}