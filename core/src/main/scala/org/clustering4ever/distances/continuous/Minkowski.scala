package org.clustering4ever.distances.continuous

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.ContinuousDistance
import org.clustering4ever.roottraits.MetricIDType._
import org.clustering4ever.roottraits.ScalarVector

import scala.language.higherKinds
import scala.math.pow
/**
 *
 */
trait MinkowskiMeta extends Serializable {
	/**
	 * Minkowski parameter
	 */
	val p: Int
	/**
	 *
	 */
	protected final def minkowski(dot1: Array[Double], dot2: Array[Double]): Double = {
		@annotation.tailrec
		def go(d: Double, i: Int): Double = {
			if(i < dot1.length) go(d + pow(dot1(i) - dot2(i), p), i + 1)
			else d
		}
		pow(go(0D, 0), 1D / p)
	}
}
/**
 *
 */
final case class Minkowski(final val p: Int = 2, final val id: MetricID = 3) extends MinkowskiMeta with ContinuousDistance {
	/**
	  * The Minkowski distance
	  * @return The Minkowski distance between dot1 and dot2
	  */
	final def dRaw(dot1: Array[Double], dot2: Array[Double]): Double = minkowski(dot1, dot2)
	/**
	  * The Minkowski distance
	  * @return The Minkowski distance between dot1 and dot2
	  */
	final def d(dot1: ScalarVector, dot2: ScalarVector): Double = minkowski(dot1.vector, dot2.vector)
}