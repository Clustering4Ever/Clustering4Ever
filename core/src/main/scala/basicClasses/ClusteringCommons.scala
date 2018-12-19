package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.GenSeq
import org.clustering4ever.scala.clusterizables.Clusterizable
/**
 * Commons properties of all clustering linked class
 */
trait ClusteringCommons extends Serializable {
	type ClusterID = Int
}
/**
 * The basic trait shared by all clustering models
 */
trait ClusteringModel extends ClusteringCommons
/**
 * The basic trait shared by all clustering algorithms
 */
trait ClusteringAlgorithm extends ClusteringCommons
/**
 * The basic trait shared by all clustering algorithms
 */
trait ClusteringAlgorithmExt[V] extends ClusteringAlgorithm {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run[
		ID: Numeric,
		O,
		Cz[X, Y, Z] <: Clusterizable[X, Y, Z, Cz[X, Y, Z]],
		Container[_]
	](data: Container[Cz[ID, O, V]])(workingVector: Int = 0): ClusteringModel
}
/**
 * The basic trait shared by all local clustering algorithms
 */
trait LocalClusteringAlgorithm[V] {//extends ClusteringAlgorithmExt {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run[
		ID: Numeric,
		O,
		Cz[X, Y, Z] <: Clusterizable[X, Y, Z, Cz[X, Y, Z]],
		GS[U] <: GenSeq[U]
	](data: GS[Cz[ID, O, V]])(workingVector: Int = 0): ClusteringModel
}
/**
 *
 */
trait ClusteringArgs extends Serializable