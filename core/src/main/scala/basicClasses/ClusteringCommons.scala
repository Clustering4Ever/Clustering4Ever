package clustering4ever.clustering
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
trait ClusteringAlgorithm[DataType] extends ClusteringCommons {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run(data: DataType): ClusteringModel
}
/**
 * The basic trait shared by all local clustering algorithms
 */
trait LocalClusteringAlgorithm[DataType <: GenSeq[_]] extends ClusteringAlgorithm[DataType]
/**
 * Trait for clustering algorithms which can go through Mean Shift algorithm
 */
trait MeanShiftable[DataType <: GenSeq[_]] extends LocalClusteringAlgorithm[DataType] {
	
	type ClusteringModelType <: ClusteringModel
	/**
	 * That's all for the moment, it cast the model into its right type, it has to be applied once the clustering algorithm is applied
	 */
	def castingModel(clusteringModel: ClusteringModel): ClusteringModelType = clusteringModel.asInstanceOf[ClusteringModelType]
}