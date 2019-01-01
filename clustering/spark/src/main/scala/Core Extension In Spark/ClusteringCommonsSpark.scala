package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.scala.vectors.GVector
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait DistributedClusteringAlgorithm[V <: GVector] extends ClusteringAlgorithmGen[V, RDD] {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run[
		ID,
		O,
		D <: Distance[V],
		Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz]
	](data: RDD[Cz[ID, O, V]], metric: Option[D] = None, args: Option[ClusteringArgs] = None)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringModel
}