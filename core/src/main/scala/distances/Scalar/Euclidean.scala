package org.clustering4ever.math.distances.scalar
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.math.sqrt
import scala.collection.mutable
import org.clustering4ever.util.SumVectors
import org.clustering4ever.math.distances.{RealClusterizableDistance, ContinuousDistance}
import org.clustering4ever.scala.clusterizables.Clusterizable
/**
 *
 */
trait EuclideanMeta extends Serializable {
	/**
	 *
	 */
	protected val squareRoot: Boolean
	/**
	 * Euclidean distance in recursion style, faster than while
	 */
	protected def euclidean[V <: Seq[Double]](dot1: V, dot2: V): Double = {

		@annotation.tailrec
		def go(d: Double, i: Int): Double = {
			if( i < dot1.size ) {
			  val toPow2 = dot1(i) - dot2(i)
			  go(d + toPow2 * toPow2, i + 1)
			}
			else d
		}

		val d = go(0D, 0)

		if( squareRoot ) sqrt(d) else d
	}

	private val toStringRoot = if( squareRoot ) "with " else "without "

	override def toString() = "Euclidean distance " + toStringRoot + "root applied"
}
/**
 * The Euclidean distance with or without squareRoot
 */
 class Euclidean[V <: Seq[Double]](final val squareRoot: Boolean = true) extends EuclideanMeta with ContinuousDistance[V] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = euclidean(dot1, dot2)
	/**
	 * The euclidean norm
	 */
	def norm(dot: V): Double = SumVectors.norm(dot)
}
/**
 * The easy to use Euclidean distance with or without squareRoot for vectors =:= mutable.ArrayBuffer[Double]
 */
class EasyEuclidean(squareRoot: Boolean = true) extends Euclidean[mutable.ArrayBuffer[Double]](squareRoot)
/**
 *
 */
class EuclideanClusterizable[@specialized(Int, Long) ID: Numeric, O, V <: Seq[Double], D <: Euclidean[V], Cz <: Clusterizable[ID, O, V, Cz]](final val squareRoot: Boolean = true, val classicalMetric: D, workingVector: Int = 0) extends EuclideanMeta with RealClusterizableDistance[Cz, V, D] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  */
	def d(dot1: Cz, dot2: Cz): Double = euclidean(dot1.vector(workingVector), dot2.vector(workingVector))
}