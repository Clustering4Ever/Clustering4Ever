package clustering4ever.math.distances.binary

import clustering4ever.math.distances.{BinaryDistance, BinaryDistanceUtil}
import scala.math.pow
import scala.collection.mutable

/**
 * @author Beck Gaël
 **/
class ShapeDifference extends BinaryDistance[mutable.ArrayBuffer[Int]]
{
	def d(vector1: mutable.ArrayBuffer[Int], vector2: mutable.ArrayBuffer[Int]): Double = {
		val (a, b, c, d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		val s = a + b + c + d
		val s2 = b -c
		(s * (b + c) - (s2 * s2)) / (s * s)
	}	
}