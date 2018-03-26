package lipn.spartakus.core.math.distances

import _root_.scala.math.pow

class PatternDifference extends BinaryDistance
{

	override def distance(vector1: Seq[Int], vector2: Seq[Int]): Double =
	{
		val (a,b,c,d) = contingencyTable(vector1, vector2)
		(4D * b * c) / pow(a + b + c + d, 2)
	}
	
}