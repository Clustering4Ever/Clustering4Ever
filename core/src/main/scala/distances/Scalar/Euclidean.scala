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
trait EuclideanAncestor[V <: Seq[Double]] extends Serializable {
	/**
	 * Applied square root to euclidean distance if true
	 */
	protected val squareRoot: Boolean
	/**
	 * Euclidean distance in recursion style, faster than while
	 */
	protected final def euclidean(v1: V, v2: V): Double = {
		@annotation.tailrec
		def go(d: Double, i: Int): Double = {
			if(i < v1.size) {
			  val toPow2 = v1(i) - v2(i)
			  go(d + toPow2 * toPow2, i + 1)
			}
			else d
		}
		val d = go(0D, 0)
		if(squareRoot) sqrt(d) else d
	}
}
/**
 * @tparam V
 */
trait EuclideanMeta[V <: Seq[Double]] extends EuclideanAncestor[V] {
	/**
	 *
	 */
	private final val toStringRoot = if(squareRoot) "with " else "without "
	/**
	 *
	 */
	override def toString = "Euclidean distance " + toStringRoot + "squared root applied"
}
/**
 * @tparam V
 * The Euclidean distance with or without squareRoot
 */
final case class RawEuclidean[V <: Seq[Double]](final val squareRoot: Boolean = true, final val id: MetricID = 0) extends EuclideanMeta[V] with RawContinuousDistance[V] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between v1 and v2
	  */
	final def d(v1: V, v2: V): Double = euclidean(v1, v2)
	/**
	 *
	 */
	final def norm(dot: V): Double = SumVectors.euclideanNorm(dot)
}
/**
 * @tparam V
 * The Euclidean distance with or without squareRoot
 */
final case class Euclidean[V <: Seq[Double]](final val squareRoot: Boolean = true, final val id: MetricID = 0) extends EuclideanMeta[V] with ContinuousDistance[V] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between v1 and v2
	  */
	final def d(v1: V, v2: V): Double = euclidean(v1, v2)
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between v1 and v2
	  */
	final def d(v1: ScalarVector[V], v2: ScalarVector[V]): Double = euclidean(v1.vector, v2.vector)
	/**
	 *
	 */
	final def norm(dot: V): Double = SumVectors.euclideanNorm(dot)
	/**
	 *
	 */
	final def norm(dot: ScalarVector[V]): Double = SumVectors.euclideanNorm(dot.vector)

}