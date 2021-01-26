package org.clustering4ever.sparkcoreextension

/**
 * @author Beck Gaël
 */
import org.apache.spark.sql.{Dataset, Encoders}
import org.clustering4ever.clusteringtraits.{ClusteringAlgorithm, ClusteringModel}
import org.clustering4ever.roottraits.{Clusterizable, GVector}

import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait ClusteringAlgorithmDistributedDS[V <: GVector[V], CA <: ClusteringModelDistributedDS[V]] extends ClusteringAlgorithm {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return GenericClusteringModel
	 */
	def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): CA

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
	final private val encoderClusterIDs = if(kryoSerialization) Encoders.kryo[Int] else Encoders.javaSerialization[Int]
	/**
	 * General methods to obtain a clustering from the model in order to measure performances scores
	 */
	protected[clustering4ever] def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): Dataset[Cz[O, V]]
	/**
	 *
	 */
	protected[clustering4ever] final def obtainClusteringIDs[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): Dataset[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last)(encoderClusterIDs)
	}
}