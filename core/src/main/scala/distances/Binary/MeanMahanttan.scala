package clustering4ever.math.distances.binary

import clustering4ever.math.distances.{BinaryDistance, BinaryDistanceUtil}
import scala.collection.mutable

/**
 * @author Beck Gaël
 */
class MeanMahanttan extends BinaryDistance[mutable.ArrayBuffer[Int]] {

	def d(vector1: mutable.ArrayBuffer[Int], vector2: mutable.ArrayBuffer[Int]): Double = {
		val (a,b,c,d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		(b + c).toDouble / (a + b + c + d)
	}
	
}