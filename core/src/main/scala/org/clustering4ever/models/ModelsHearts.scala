package org.clustering4ever.models

/**
 * @author Beck Gaël
 */
import org.clustering4ever.clusteringtraits.ClusteringModel
import org.clustering4ever.distances.Distance
import org.clustering4ever.roottraits.GVector
/**
 * @tparam V
 * @tparam D
 */
trait MetricModel[V <: GVector[V], D <: Distance[V]] extends ClusteringModel {
	/**
	 * A metric defined on any object which inherit GVector
	 */
	val metric: D
}