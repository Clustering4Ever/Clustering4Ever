package org.clustering4ever.math.distances.scalar
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import org.clustering4ever.math.distances.ContinuousDistance
import org.clustering4ever.util.SumVectors
import org.clustering4ever.vectors.ScalarVector
import org.clustering4ever.types.MetricIDType._
/**
 *
 */
case class Cosine[V <: Seq[Double]](final val id: MetricID = 2) extends ContinuousDistance[V] {
	/**
	  * The cosine distance
	  * @return The cosine distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = {
		val anorm = SumVectors.euclideanNorm(dot1)
		val bnorm = SumVectors.euclideanNorm(dot2)
		SumVectors.dotProduct(dot1, dot2) / (anorm * bnorm)
	}
	/**
	  * The cosine distance
	  * @return The cosine distance between dot1 and dot2
	  */
	def d(dot1: ScalarVector[V], dot2: ScalarVector[V]): Double = d(dot1.vector, dot2.vector)
}