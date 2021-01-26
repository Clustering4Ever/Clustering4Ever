package org.clustering4ever.distances.binary

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.{BinaryDistance, BinaryDistanceUtil}
import org.clustering4ever.roottraits.BinaryVector
import org.clustering4ever.roottraits.MetricIDType._
/**
 *
 **/
final case class Vari(final val id: MetricID = 9) extends BinaryDistance {

	final def dRaw(vector1: Array[Int], vector2: Array[Int]): Double = {
		val (a, b, c, d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		(b + c).toDouble / (4 * (a + b + c + d))
	}

	final def d(vector1: BinaryVector, vector2: BinaryVector): Double = dRaw(vector1.vector, vector2.vector)

}