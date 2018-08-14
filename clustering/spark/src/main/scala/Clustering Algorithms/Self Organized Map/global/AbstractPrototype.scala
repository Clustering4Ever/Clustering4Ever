package clustering4ever.spark.clustering.mtm.global

import org.apache.spark.mllib.linalg.{Vectors, Vector, DenseVector}

/**
 * @author Sarazin Tugdual & Beck Gaël
 **/
abstract class AbstractPrototype(val id: Int, var point: DenseVector) extends Serializable
{
	def update(newPoint: DenseVector): Double =
	{
		val dist = Vectors.sqdist(point, newPoint)
		point = newPoint
		dist
	}

	def dist(data: DenseVector) =
	{
		Vectors.sqdist(point, data) // a modifier: - ajouter une pondération fixe; - ajouter une pondération adaptative
	}
	
	def dist(prototype: AbstractPrototype) =
	{
		Vectors.sqdist(point, prototype.point)
	}
}
