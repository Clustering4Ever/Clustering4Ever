package clustering4ever.math.distances.scalar

import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.scala.math.{pow, sqrt}

/**
 * @author Beck Gaël
 **/
class Euclidean(root: Boolean) extends ContinuousDistances
{

	private def euclideanIntern(dot1: Vector[Double], dot2: Vector[Double]) =
	{
		var d = 0D
		for( i <- dot1.indices ) d += pow(dot1(i) - dot2(i), 2)
		d
	}

	/**
	  * The famous euclidean distance implemented in its fast mono thread scala version without SQRT part
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	override def d(dot1: Vector[Double], dot2: Vector[Double]): Double =
	{
		if( root ) sqrt(euclideanIntern(dot1, dot2))
		else euclideanIntern(dot1, dot2)
	}

	lazy val toStringRoot = if( root ) "with " else "without "

	override def toString = "Euclidean " + toStringRoot + "root applied"

}



