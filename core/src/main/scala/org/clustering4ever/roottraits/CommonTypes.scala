package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
/**
 *
 */
object MetricIDType {
	/**
	 * The ID os the metric
	 */
	final type MetricID = Int
}
/**
 *
 */
object ClusteringNumberType {
	/**
	 * The number in which this clustering was executed, from 0 to n - 1
	 */
	final type ClusteringNumber = Int
}
/**
 *
 */
object VectorizationIDTypes {
	/**
	 * The ID of the vectorization
	 */
	final type VectorizationID = Int
}
/**
 *
 */
object ClusteringInformationTypes {
	/**
	 * The number of clustering that have been made by the clustering chaining class
	 */
	final type ClusteringRunNumber = Int
}
/**
 *
 */
object ClusterIDType {
	/**
	 * The ID of a particuler cluster
	 */
	final type ClusterID = Int	
}