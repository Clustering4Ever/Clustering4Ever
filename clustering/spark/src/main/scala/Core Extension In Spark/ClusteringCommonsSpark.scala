package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectors.GVector
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait DistributedClusteringAlgorithm[V <: GVector[V], +CA <: ClusteringArgs, +CM <: ClusteringModelCz[V, RDD]] extends ClusteringAlgorithmCz[V, RDD, CA, CM] {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run[
		ID,
		O,
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]
	](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): CM
}