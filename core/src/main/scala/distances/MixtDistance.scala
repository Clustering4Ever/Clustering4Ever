package clustering4ever.math.distances

import clustering4ever.scala.measurableclass.BinaryScalarVector

/**
 * @author Beck Gaël
 **/
trait MixtDistance extends Distance[BinaryScalarVector]
{
	def d(vector1: BinaryScalarVector, vector2: BinaryScalarVector): Double
}