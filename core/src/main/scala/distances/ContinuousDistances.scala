package clustering4ever.math.distances

import scala.math.{pow, sqrt}
import scala.collection.immutable

/**
 * @author Beck Gaël
 **/
trait ContinuousDistances extends Distance[immutable.Seq[Double]]
{
	def d(vector1: immutable.Seq[Double], vector2: immutable.Seq[Double]): Double
}