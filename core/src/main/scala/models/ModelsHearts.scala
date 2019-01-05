package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.clustering.ClusteringModel
/**
 *
 */
trait MetricModel[O, D <: Distance[O]] extends ClusteringModel {
	/**
	 *
	 */
	val metric: D
}