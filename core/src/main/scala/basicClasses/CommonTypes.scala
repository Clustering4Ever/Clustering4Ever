package org.clustering4ever.types
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
object MetricIDType {
	/**
	 * The ID os the metric
	 */
	type MetricID = Int
}
/**
 *
 */
object ClusteringNumberType {
	/**
	 * The number in which this clustering was executed, from 0 to n - 1
	 */
	type ClusteringNumber = Int
}
/**
 *
 */
object VectorizationIDTypes {
	/**
	 * The ID of the vectorization
	 */
	type VectorizationID = Int
}
/**
 *
 */
object ClusteringInformationTypes {
	/**
	 * The number of clustering that have been made by the clustering chaining class
	 */
	type ClusteringRunNumber = Int
}
/**
 *
 */
object ClusterIDType {
	/**
	 * The ID of a particuler cluster
	 */
	type ClusterID = Int	
}