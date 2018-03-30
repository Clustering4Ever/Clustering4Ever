package clustering4ever.math.distances

import _root_.scala.math.{pow, sqrt}

/**
 * @author Beck Gaël
 **/
trait ContinuousDistances extends Distance
{
	type T = Double
	def distance(vector1: Seq[T], vector2: Seq[T]): Double
}



