package clustering4ever.math.distances.scalar

import scala.reflect.runtime.universe.TypeTag
import scala.math.sqrt
import clustering4ever.math.distances.{RealClusterizableDistance, ContinuousDistance}
import clustering4ever.scala.clusterizables.RealClusterizable

/**
 * @author Beck Gaël
 **/
trait EuclideanMeta extends Serializable {

	protected val squareRoot: Boolean
	/**
	 * Less elegant than recursion or zip style but much more efficient
	 */
	protected def euclidean[V <: Seq[Double]](dot1: V, dot2: V): Double = {
		var d = 0D
		var i = 0
		while( i < dot1.size ) {
			val toPow2 = dot1(i) - dot2(i)
			d += toPow2 * toPow2
			i += 1
		}
		if( squareRoot ) sqrt(d) else d
	}

	private val toStringRoot = if( squareRoot ) "with " else "without "

	override def toString() = "Euclidean distance " + toStringRoot + "root applied"
}

class Euclidean[V <: Seq[Double]](final val squareRoot: Boolean = true) extends EuclideanMeta with ContinuousDistance[V] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = euclidean[V](dot1, dot2)
}

class EuclideanClusterizable[ID: Numeric, Obj, V <: Seq[Double]](final val squareRoot: Boolean = true, val classicalMetric: Euclidean[V]) extends EuclideanMeta with RealClusterizableDistance[RealClusterizable[ID, Obj, V], V] {
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  */
	def d(dot1: RealClusterizable[ID, Obj, V], dot2: RealClusterizable[ID, Obj, V]): Double = euclidean[V](dot1.vector, dot2.vector)
}