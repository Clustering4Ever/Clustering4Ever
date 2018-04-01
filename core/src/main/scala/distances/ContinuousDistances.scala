package clustering4ever.math.distances

import _root_.scala.math.{pow, sqrt}

/**
 * @author Beck Gaël
 **/
trait ContinuousDistances extends Distance[Array[Double]]
{
	def d(vector1: Array[Double], vector2: Array[Double]): Double
}



