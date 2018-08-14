package clustering4ever.math.distances.scalar

import clustering4ever.math.distances.ContinuousDistances
import scala.math.{pow, sqrt}
import scala.collection.immutable

/**
 * @author Beck Gaël
 **/
class Euclidean(root: Boolean) extends ContinuousDistances
{
	private def euclideanIntern(dot1: immutable.Seq[Double], dot2: immutable.Seq[Double]) =
	{
		dot1.zip(dot2).map{ case (a, b) => pow(a - b, 2) }.sum
	}

	/**
	  * The famous euclidean distance implemented in its fast mono thread scala version without SQRT part
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	override def d(dot1: immutable.Seq[Double], dot2: immutable.Seq[Double]): Double =
	{
		if( root ) sqrt(euclideanIntern(dot1, dot2))
		else euclideanIntern(dot1, dot2)
	}

	lazy val toStringRoot = if( root ) "with " else "without "

	override def toString = "Euclidean " + toStringRoot + "root applied"

}



