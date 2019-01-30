package org.clustering4ever.math.distances.scalar
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.sqrt
import scala.collection.mutable
import org.clustering4ever.util.SumVectors
import org.clustering4ever.math.distances.{ContinuousDistance, RawContinuousDistance, Distance}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector}
import org.clustering4ever.types.MetricIDType._
/**
 *
 */
trait EuclideanMeta[V <: Seq[Double]] extends Serializable {
	/**
	 *
	 */
	protected val squareRoot: Boolean
	/**
	 * Euclidean distance in recursion style, faster than while
	 */
	protected def euclidean(dot1: V, dot2: V): Double = {
		@annotation.tailrec
		def go(d: Double, i: Int): Double = {
			if(i < dot1.size) {
			  val toPow2 = dot1(i) - dot2(i)
			  go(d + toPow2 * toPow2, i + 1)
			}
			else d
		}
		val d = go(0D, 0)
		if(squareRoot) sqrt(d) else d
	}
	/**
	 *
	 */
	private val toStringRoot = if(squareRoot) "with " else "without "
	/**
	 *
	 */
	override def toString = "Euclidean distance " + toStringRoot + "squared root applied"
}
/**
 * The Euclidean distance with or without squareRoot
 */
case class RawEuclidean[V <: Seq[Double]](final val squareRoot: Boolean = true, final val id: MetricID = 0) extends EuclideanMeta[V] with RawContinuousDistance[V] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = euclidean(dot1, dot2)
	/**
	 *
	 */
	def norm(dot: V): Double = SumVectors.euclideanNorm(dot)
}
/**
 * The Euclidean distance with or without squareRoot
 */
case class Euclidean[V <: Seq[Double]](final val squareRoot: Boolean = true, final val id: MetricID = 0) extends EuclideanMeta[V] with ContinuousDistance[V] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = euclidean(dot1, dot2)
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  */
	def d(dot1: ScalarVector[V], dot2: ScalarVector[V]): Double = euclidean(dot1.vector, dot2.vector)
	/**
	 *
	 */
	def norm(dot: V): Double = SumVectors.euclideanNorm(dot)
	/**
	 *
	 */
	def norm(dot: ScalarVector[V]): Double = SumVectors.euclideanNorm(dot.vector)

}