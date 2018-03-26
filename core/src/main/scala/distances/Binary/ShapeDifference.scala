package lipn.spartakus.core.math.distances

import _root_.scala.math.pow

class ShapeDifference extends BinaryDistance
{
	override def distance(vector1: Seq[Int], vector2: Seq[Int]): Double =
	{
		val (a, b, c, d) = contingencyTable(vector1, vector2)
		((a + b + c + d) * (b + c) - pow(b - c, 2)) / pow(a + b + c + d,2)
	}	
}