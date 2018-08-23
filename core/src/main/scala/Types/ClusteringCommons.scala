package clustering4ever.clustering

trait ClusteringCommons
{
	type ClusterID = Int
}

trait DataSetsTypes[IDNature] extends ClusteringCommons
{
	type ID = IDNature
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
