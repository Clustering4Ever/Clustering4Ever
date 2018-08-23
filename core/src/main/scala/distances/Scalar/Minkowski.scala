package clustering4ever.math.distances.scalar

import clustering4ever.math.distances.ContinuousDistance
import scala.math.pow
import scala.util.Try
import scala.collection.immutable

/**
 * @author Beck Gaël
 **/
class Minkowski(p: Int) extends ContinuousDistance
{
	/**
	  * The famous Minkowski distance implemented
	  * @return The Minkowski distance between dot1 and dot2
	  * @param p : Minkowsiki parameter
	  **/
	override def d(dot1: Seq[Double], dot2: Seq[Double]): Double =
	{
		val preDistance = dot1.zip(dot2).map{ case (a, b) => pow(a - b, p) }.sum
		pow(preDistance, 1D / p )
	}
}



