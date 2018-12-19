package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.clustering4ever.scala.clusterizables.Clusterizable
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait DistributedClusteringAlgorithm[V] { // extends ClusteringAlgorithmExt[V] {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run[
		ID: Numeric,
		O,
		Cz[X, Y, Z] <: Clusterizable[X, Y, Z, Cz[X, Y, Z]]
	](data: RDD[Cz[ID, O, V]])(workingVector: Int = 0)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringModel
}