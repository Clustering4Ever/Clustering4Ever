package clustering4ever.math.distances

import _root_.scala.math.{pow, sqrt}

/**
 * @author Beck Gaël
 **/
trait ContinuousDistances extends Distance[Vector[Double]]
{
	def d(vector1: Vector[Double], vector2: Vector[Double]): Double
}



