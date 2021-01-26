package org.clustering4ever.distances.continuous

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.{ContinuousDistance, RawContinuousDistance}
import org.clustering4ever.roottraits.MetricIDType._
import org.clustering4ever.roottraits.ScalarVector
import org.clustering4ever.util.SumVectors

import scala.language.higherKinds
import scala.math.sqrt
/**
 *
 */
trait EuclideanAncestor extends Serializable {
	/**
	 * Applied square root to euclidean distance if true
	 */
	protected val squareRoot: Boolean
	/**
	 * Euclidean distance in recursion style, faster than while
	 */
	protected final def euclidean(v1: Array[Double], v2: Array[Double]): Double = {
		
		@annotation.tailrec
		def go(d: Double, i: Int): Double = {
			if(i < v1.length) {
			  val toPow2 = v1(i) - v2(i)
			  go(d + toPow2 * toPow2, i + 1)
			}
			else d
		}
		
		val d = go(0D, 0)
		
		if (squareRoot) sqrt(d)
		else d

	}
}
/**
 * @tparam Array[Double]
 */
trait EuclideanSubAncestor extends EuclideanAncestor {
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
 * @tparam Array[Double]
 * The Euclidean distance with or without squareRoot
 */
final case class RawEuclidean(final val squareRoot: Boolean = true, final val id: MetricID = 0) extends EuclideanSubAncestor with RawContinuousDistance {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between v1 and v2
	  */
	final def d(v1: Array[Double], v2: Array[Double]): Double = euclidean(v1, v2)
	/**
	 *
	 */
	final def norm(dot: Array[Double]): Double = SumVectors.euclideanNorm(dot)
}
/**
 * @param squareRoot apply or not square root to euclidean distance 
 * @param useNorm fast computation of Euclidean distance with vectors norm precomputed
 * @param id
 * The Euclidean distance 
 */
final case class Euclidean(final val squareRoot: Boolean = true, final val useNorm: Boolean = true, final val id: MetricID = 0) extends EuclideanSubAncestor with ContinuousDistance {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between v1 and v2
	  */
	final def dRaw(v1: Array[Double], v2: Array[Double]): Double = euclidean(v1, v2)
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between v1 and v2
	  */
	final def d(v1: ScalarVector, v2: ScalarVector): Double = {
		if(useNorm) {
			val euclideanNoSquareRoot = v1.norm2 + v2.norm2 - 2 * v1.dot(v2)
			if(squareRoot) sqrt(euclideanNoSquareRoot)
			else euclideanNoSquareRoot
		}
		else {
			euclidean(v1.vector, v2.vector)
		}
	}
	/**
	 *
	 */
	final def norm(dot: Array[Double]): Double = SumVectors.euclideanNorm(dot)
	/**
	 *
	 */
	final def norm(dot: ScalarVector): Double = SumVectors.euclideanNorm(dot.vector)

}