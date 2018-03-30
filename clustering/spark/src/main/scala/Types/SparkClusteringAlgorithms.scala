package clustering4ever.clustering

import clustering4ever.clustering.datasetstype.RDDDataSetsTypes

/**
 * @author Beck Gaël
 **/
trait SparkClusteringAlgorithm extends RDDDataSetsTypes
{
	/**
	 * Execute the corresponding clustering algorithm
	 * @return Array[(ClusterID, (ID, Vector))]
	 **/
	def run(): ClusterizedRDD
}

trait SparkRealClusteringAlgorithm extends SparkClusteringAlgorithm
{
	type NaturesValue = Double
}