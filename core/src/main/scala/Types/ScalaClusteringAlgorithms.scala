package clustering4ever.clustering

import clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
trait ScalaClusteringAlgorithm extends DataSetsTypes
{
	/**
	 * Execute the corresponding clustering algorithm
	 * @return Array[(ClusterID, (ID, Vector))]
	 **/
	def run(): ClusterizedData
}