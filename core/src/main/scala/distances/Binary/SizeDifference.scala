package clustering4ever.math.distances.binary

import _root_.clustering4ever.math.distances.{BinaryDistance, BinaryDistanceUtil}
import _root_.scala.math.pow

/**
 * @author Beck Gaël
 **/
class SizeDifference extends BinaryDistance
{

	override def d(vector1: Array[Int], vector2: Array[Int]): Double =
	{
		val (a,b,c,d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		pow(b + c, 2) / pow(a + b + c + d, 2)
	}
	
}