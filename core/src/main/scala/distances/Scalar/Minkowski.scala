package org.clustering4ever.math.distances.scalar
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import scala.math.pow
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.ContinuousDistance
import org.clustering4ever.vectors.ScalarVector
import org.clustering4ever.types.MetricIDType._
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
			if(i < dot1.size) go(d + pow(dot1(i) - dot2(i), p), i + 1)
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