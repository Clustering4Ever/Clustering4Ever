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
import org.clustering4ever.clustering.{ClusteringAlgorithm, ClusteringModel, ClusteringArgs}
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait ClusteringAlgorithmDistributedDS[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], +CA <: ClusteringArgsDistributedDS[V], +CM <: ClusteringModelDistributedDS[ID, O, V, Cz, CA]] extends ClusteringAlgorithm[ID, O, V, Cz, Dataset, CA, CM] {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return GenericClusteringModel
	 */
	def run(data: Dataset[Cz[ID, O, V]]): CM

}
/*
 *
 */
trait ClusteringModelDistributedDS[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], +CA <: ClusteringArgsDistributedDS[V]] extends ClusteringModel[ID, O, V, Cz, Dataset, CA] {
	/**
	 * kryo Serialization if true, java one else
	 */
	val encoderClusterIDs = if(args.kryoSerialization) Encoders.kryo[Int] else Encoders.javaSerialization[Int]
	/**
	 *
	 */
	def obtainClusteringIDs(data: Dataset[Cz[ID, O, V]]): Dataset[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last)(encoderClusterIDs)
	}
}
/**
 *
 */
trait ClusteringArgsDistributedDS[V <: GVector[V]] extends ClusteringArgs[V] {
	/**
	 * kryo Serialization if true, java one else
	 */
	val kryoSerialization: Boolean
	/**
	 * @return the corresponding algorithm with given arguments to run on data
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: Dataset[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringAlgorithmDistributedDS[ID, O, V, Cz, ClusteringArgsDistributedDS[V], ClusteringModelDistributedDS[ID, O, V, Cz, ClusteringArgsDistributedDS[V]]]
}