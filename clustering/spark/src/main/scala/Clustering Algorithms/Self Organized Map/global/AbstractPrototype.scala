package clustering4ever.spark.clustering.mtm.global

import org.apache.spark.mllib.linalg.{Vectors, Vector, DenseVector}

/**
 * @author Sarazin Tugdual & Beck Gaël
 **/
abstract class AbstractPrototype(val id: Int, var _point: DenseVector) extends Serializable
{
	def update(newPoint: DenseVector): Double =
	{
		val dist = Vectors.sqdist(_point, newPoint)
		_point = newPoint
		dist
	}

	def dist(data: DenseVector) =
	{
		Vectors.sqdist(_point, data) // a modifier: - ajouter une pondération fixe; - ajouter une pondération adaptative
	}
	
	def dist(prototype: AbstractPrototype) =
	{
		Vectors.sqdist(_point,prototype._point)
	}
}
