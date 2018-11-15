package org.clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import org.clustering4ever.math.distances.{BinaryDistance, BinaryDistanceUtil}
import scala.collection.mutable

class PatternDifference extends BinaryDistance[mutable.ArrayBuffer[Int]] {

	def d(vector1: mutable.ArrayBuffer[Int], vector2: mutable.ArrayBuffer[Int]): Double = {
		val (a,b,c,d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		val s = a + b + c + d
		(4D * b * c) / (s * s)
	}
	
}