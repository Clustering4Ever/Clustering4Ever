package clustering4ever.clustering

import _root_.clustering4ever.clustering.datasetstype.ClusteringTypes

/**
 * @author Beck Gaël
 **/
trait ClusteringAlgorithms[ID, NaturesValue, ClusterizedNature] extends ClusteringTypes[ID, NaturesValue, ClusterizedNature]
{
	/**
	 * Execute the corresponding clustering algorithm
	 * @return Array[(ClusterID, (ID, Vector))]
	 **/
	def run(): ClusterizedData
}