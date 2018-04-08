package clustering4ever.clustering

import clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
trait ClusteringAlgorithms[IDNature, InputVector] extends DataSetsTypes[IDNature, InputVector] with Serializable
{
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 **/
	def run(): ClusteringModel
}