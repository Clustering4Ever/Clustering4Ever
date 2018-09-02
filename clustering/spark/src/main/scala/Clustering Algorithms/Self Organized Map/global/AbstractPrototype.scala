package clustering4ever.spark.clustering.mtm.global

import org.apache.spark.mllib.linalg.{Vectors, Vector, DenseVector}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.ContinuousDistance

/**
 * @author Sarazin Tugdual & Beck Gaël
 **/
abstract class AbstractPrototype(val id: Int, var point: Seq[Double], metric: ContinuousDistance[Seq[Double]]) extends Serializable
{
	def update(newPoint: Seq[Double]): Double =
	{
		val dist = metric.d(point, newPoint)
		point = newPoint
		dist
	}

	// a modifier: - ajouter une pondération fixe; - ajouter une pondération adaptative
	def dist(data: Seq[Double]) = metric.d(point, data)
	
	def dist(prototype: AbstractPrototype) = metric.d(point, prototype.point)
}
