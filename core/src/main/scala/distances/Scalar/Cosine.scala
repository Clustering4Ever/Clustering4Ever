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
class Cosine[V <: Seq[Double]](val id: MetricID = 2) extends ContinuousDistance[V] {
	/**
	  * The cosine distance
	  * @return The cosine distance between dot1 and dot2
	  */
	override def d(dot1: ScalarVector[V], dot2: ScalarVector[V]): Double = {
		val anorm = SumVectors.euclideanNorm(dot1.vector)
		val bnorm = SumVectors.euclideanNorm(dot2.vector)
		SumVectors.dotProduct(dot1.vector, dot2.vector) / (anorm * bnorm)
	}
}