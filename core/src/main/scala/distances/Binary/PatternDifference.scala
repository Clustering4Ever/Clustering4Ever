package org.clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import scala.collection.mutable
import org.clustering4ever.math.distances.{BinaryDistance, BinaryDistanceUtil}
import org.clustering4ever.vectors.BinaryVector
import org.clustering4ever.types.MetricIDType._
/**
 *
 */
final case class PatternDifference(val id: MetricID = 6) extends BinaryDistance {

	final def dRaw(vector1: Array[Int], vector2: Array[Int]): Double = {
		val (a, b, c, d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		val s = a + b + c + d
		(4D * b * c) / (s * s)
	}

	final def d(vector1: BinaryVector, vector2: BinaryVector): Double = dRaw(vector1.vector, vector2.vector)

	
}