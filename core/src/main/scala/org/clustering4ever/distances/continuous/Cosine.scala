package org.clustering4ever.distances.continuous

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.ContinuousDistance
import org.clustering4ever.roottraits.MetricIDType._
import org.clustering4ever.roottraits.ScalarVector
import org.clustering4ever.util.SumVectors

import scala.language.higherKinds
/**
 *
 */
final case class Cosine(final val id: MetricID = 2) extends ContinuousDistance {
	/**
	  * The cosine distance
	  * @return The cosine distance between dot1 and dot2
	  */
	final def dRaw(dot1: Array[Double], dot2: Array[Double]): Double = {
		val anorm = SumVectors.euclideanNorm(dot1)
		val bnorm = SumVectors.euclideanNorm(dot2)
		SumVectors.dotProduct(dot1, dot2) / (anorm * bnorm)
	}
	/**
	  * The cosine distance
	  * @return The cosine distance between dot1 and dot2
	  */
	final def d(dot1: ScalarVector, dot2: ScalarVector): Double = dRaw(dot1.vector, dot2.vector)
}