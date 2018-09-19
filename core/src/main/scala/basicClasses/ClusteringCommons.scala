package clustering4ever.clustering
/**
 * @author Beck Gaël
 */
 
trait ClusteringCommons {
	type ClusterID = Int
}

trait DataSetsTypes[IDNature] extends ClusteringCommons {
	type ID = IDNature
}

trait ClusteringModel extends ClusteringCommons with Serializable

trait ClusteringAlgorithms[ID] extends DataSetsTypes[ID] with Serializable {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run(): ClusteringModel
}
