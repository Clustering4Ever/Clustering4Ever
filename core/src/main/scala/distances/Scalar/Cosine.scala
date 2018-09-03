package clustering4ever.math.distances.scalar

import clustering4ever.math.distances.ContinuousDistance
import scala.math.{pow, sqrt}
import scala.collection.immutable

/**
 * @author Beck Gaël
 **/
class Cosine extends ContinuousDistance[Seq[Double]]
{

	private def norm(dot1: Seq[Double]): Double =
	{
		var d = 0D
		dot1.foreach( v =>  d += v * v )
		sqrt(d)
	}

	private def dotProd(dot1: Seq[Double], dot2: Seq[Double]): Double =
	{
		var dp = 0D
		dot1.zip(dot2).foreach{ case (a, b) => dp += a * b }
		dp
	}

	/**
	  * The cosine distance implemented
	  * @return The cosine distance between dot1 and dot2
	  **/
	override def d(dot1: Seq[Double], dot2: Seq[Double]): Double =
	{
		val anorm = norm(dot1)
		val bnorm = norm(dot2)
		dotProd(dot1, dot2) / (anorm * bnorm)
	}
}





