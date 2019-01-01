package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import org.clustering4ever.math.distances.Distance
/**
 *
 */
trait MetricModel[O, D <: Distance[O]] extends ClusteringModel {
	/**
	 *
	 */
	val metric: D
}
