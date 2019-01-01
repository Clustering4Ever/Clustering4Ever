package org.clustering4ever.math.distances.scalar
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.sqrt
import scala.collection.mutable
import org.clustering4ever.util.SumVectors
import org.clustering4ever.math.distances.{RealClusterizableDistance, ContinuousDistance, RawContinuousDistance, Distance, ClusterizableDistanceBuilder}
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.scala.vectors.{GVector, ScalarVector}
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
	override def toString() = "Euclidean distance " + toStringRoot + "squared root applied"
}
/**
 * The Euclidean distance with or without squareRoot
 */
 class RawEuclidean[V <: Seq[Double]](final val squareRoot: Boolean = true) extends EuclideanMeta[V] with RawContinuousDistance[V] {
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
 class Euclidean[V <: Seq[Double]](final val squareRoot: Boolean = true) extends EuclideanMeta[V] with ContinuousDistance[V] with ClusterizableDistanceBuilder[ScalarVector[V]] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  */
	def d(dot1: ScalarVector[V], dot2: ScalarVector[V]): Double = euclidean(dot1.vector, dot2.vector)
	/**
	 *
	 */
	def norm(dot: ScalarVector[V]): Double = SumVectors.euclideanNorm(dot.vector)
	/**
	 *
	 */
	def obtainClusterizableMetric[ID, O, Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz]](clusterizable: Cz[ID, O, ScalarVector[V]]) = {
		new EuclideanClusterizable[ID, O, V, Cz](squareRoot)
	}
}
/**
 * The easy to use Euclidean distance with or without squareRoot for vectors =:= mutable.ArrayBuffer[Double]
 */
class EasyEuclidean(squareRoot: Boolean = false) extends Euclidean[mutable.ArrayBuffer[Double]](squareRoot)
/**
 *
 */
class EuclideanClusterizable[ID, O, V <: Seq[Double], Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz]](final val squareRoot: Boolean = true) extends EuclideanMeta[V] with RealClusterizableDistance[ID, O, V, Cz] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  */
	def d(dot1: Cz[ID, O, ScalarVector[V]], dot2: Cz[ID, O, ScalarVector[V]]): Double = euclidean(dot1.workingVector.vector, dot2.workingVector.vector)
	/**
	 *
	 */
	def norm(dot: Cz[ID, O, ScalarVector[V]]): Double = SumVectors.euclideanNorm(dot.workingVector.vector)
}