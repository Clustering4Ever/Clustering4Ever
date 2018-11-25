package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.collection.GenSeq
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
trait ClusteringAlgorithmExt[DataType] extends ClusteringAlgorithm {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run(data: DataType)(workingVector: Int = 0): ClusteringModel
}
/**
 * The basic trait shared by all local clustering algorithms
 */
trait LocalClusteringAlgorithm[DataType <: GenSeq[_]] extends ClusteringAlgorithmExt[DataType]
/**
 *
 */
trait ClusteringArgs extends Serializable