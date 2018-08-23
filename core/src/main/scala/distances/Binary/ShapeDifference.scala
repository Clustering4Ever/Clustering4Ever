package clustering4ever.math.distances.binary

import clustering4ever.math.distances.{BinaryDistance, BinaryDistanceUtil}
import scala.math.pow
import scala.collection.immutable

/**
 * @author Beck Gaël
 **/
class ShapeDifference extends BinaryDistance[immutable.Vector[Int]]
{
	def d(vector1: immutable.Vector[Int], vector2: immutable.Vector[Int]): Double =
	{
		val (a, b, c, d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		((a + b + c + d) * (b + c) - pow(b - c, 2)) / pow(a + b + c + d,2)
	}	
}