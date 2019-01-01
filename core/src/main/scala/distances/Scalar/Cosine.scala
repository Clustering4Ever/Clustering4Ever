package org.clustering4ever.math.distances.scalar
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import org.clustering4ever.math.distances.ContinuousDistance
import org.clustering4ever.util.SumVectors
import org.clustering4ever.scala.vectors.ScalarVector
/**
 *
 */
class Cosine[V <: Seq[Double]] extends ContinuousDistance[V] {
	/**
	  * The cosine distance
	  * @return The cosine distance between dot1 and dot2
	  */
	override def d(dot1: ScalarVector[V], dot2: ScalarVector[V]): Double = {
		val anorm = SumVectors.norm(dot1.vector)
		val bnorm = SumVectors.norm(dot2.vector)
		SumVectors.dotProd(dot1.vector, dot2.vector) / (anorm * bnorm)
	}
}
/**
 *
 */
class EasyCosine extends Cosine[mutable.ArrayBuffer[Double]]