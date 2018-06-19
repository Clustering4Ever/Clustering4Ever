package clustering4ever.math.distances.scalar

import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.scala.math.pow
import _root_.scala.util.Try

/**
 * @author Beck Gaël
 **/
class Minkowski(p: Int) extends ContinuousDistances
{
	/**
	  * The famous Minkowski distance implemented
	  * @return The Minkowski distance between dot1 and dot2
	  * @param p : Minkowsiki parameter
	  **/
	override def d(dot1: Vector[Double], dot2: Vector[Double]): Double =
	{
		var predDistance = 0D
		for( i <- dot1.indices ) predDistance += pow(dot1(i) - dot2(i), p)
		pow(predDistance, 1D / p )
	}
}



