package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import org.clustering4ever.math.distances.{GenericDistance, Distance}
import org.clustering4ever.clustering.GenericClusteringModel
import org.clustering4ever.vectors.GVector
/**
 *
 */
trait GenericMetricModel[O, D <: GenericDistance[O]] extends GenericClusteringModel {
	/**
	 *
	 */
	val metric: D
}
/**
 *
 */
trait MetricModel[V <: GVector[V], D <: Distance[V]] extends GenericMetricModel[V, D]