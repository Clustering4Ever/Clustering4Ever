package lipn.spartakus.core.math.distances

import _root_.scala.math.pow

class MeanMahanttan extends BinaryDistance
{

	override def distance(vector1: Seq[Int], vector2: Seq[Int]): Double =
	{
		val (a,b,c,d) = contingencyTable(vector1, vector2)
		(b + c).toDouble / (a + b + c + d)
	}
	
}