package clustering4ever.clustering

import clustering4ever.clustering.datasetstype.DataSetsTypes

trait ClusteringCommons
{
	type ClusterID = Int
}

trait ClusteringModel extends ClusteringCommons with Serializable

trait ClusteringAlgorithms[IDNature] extends DataSetsTypes[IDNature] with Serializable
{
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 **/
	def run(): ClusteringModel
}