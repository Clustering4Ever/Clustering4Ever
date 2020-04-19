package org.clustering4ever.spark.clustering.mtm

import org.clustering4ever.math.distances.RawContinuousDistance

/**
 * @author Sarazin Tugdual & Beck Gaël
 **/
abstract class AbstractPrototype(val id: Int, var point: Array[Double], metric: RawContinuousDistance) extends Serializable
{
	def update(newPoint: Array[Double]): Double =
	{
		val dist = metric.d(point, newPoint)
		point = newPoint
		dist
	}

	// a modifier: - ajouter une pondération fixe; - ajouter une pondération adaptative
	def dist(data: Array[Double]) = metric.d(point, data)
	
	def dist(prototype: AbstractPrototype) = metric.d(point, prototype.point)
}
