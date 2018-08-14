package clustering4ever.math.distances.binary

import clustering4ever.math.distances.{BinaryDistanceVector, BinaryDistanceUtil}
import scala.math.pow
import scala.collection.immutable

/**
 * @author Beck Gaël
 **/
class MeanMahanttan extends BinaryDistanceVector
{

	override def d(vector1: immutable.Vector[Int], vector2: immutable.Vector[Int]): Double =
	{
		val (a,b,c,d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		(b + c).toDouble / (a + b + c + d)
	}
	
}