package org.clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import scala.collection.mutable
import org.clustering4ever.math.distances.{BinaryDistance, BinaryDistanceUtil}
import org.clustering4ever.scala.vectors.BinaryVector
/**
 *
 */
class SizeDifference[V <: Seq[Int]] extends BinaryDistance[V] {

	def d(vector1: BinaryVector[V], vector2: BinaryVector[V]): Double = {
		val (a,b,c,d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		val s = a + b + c + d
		val s2 = b + c
		(s2 * s2) / (s * s)
	}
	
}