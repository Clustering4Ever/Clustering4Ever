package clustering4ever.math.distances.scalar

import clustering4ever.math.distances.ContinuousDistances
import scala.math.pow
import scala.util.Try
import scala.collection.immutable

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
	override def d(dot1: immutable.Seq[Double], dot2: immutable.Seq[Double]): Double =
	{
		var predDistance = 0D
		for( i <- dot1.indices ) predDistance += pow(dot1(i) - dot2(i), p)
		pow(predDistance, 1D / p )
	}
}



