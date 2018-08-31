package clustering4ever.math.distances.scalar

import scala.math.{pow, sqrt}
import scala.collection.{GenSeq, parallel, mutable}
import clustering4ever.math.distances.{RealClusterizableDistance, ContinuousDistance}
import clustering4ever.scala.clusterizables.RealClusterizable

/**
 * @author Beck Gaël
 **/
trait EuclideanMeta extends Serializable
{
	protected val squareRoot: Boolean

	/**
	 * Less elegant than recursion style but more efficient
	 */
	protected def euclidean[V <: GenSeq[Double]](dot1: V, dot2: V): Double =
	{
		var d = 0D
		var i = 0
		while( i < dot1.size )
		{
			d += pow(dot1(i) - dot2(i), 2)
			i += 1
		}
		d
	}

	lazy val toStringRoot = if( squareRoot ) "with " else "without "

	override def toString = "Euclidean " + toStringRoot + "root applied"
}

class Euclidean[V <: GenSeq[Double]](final val squareRoot: Boolean = true) extends EuclideanMeta with ContinuousDistance[V]
{
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	def d(dot1: V, dot2: V): Double = euclidean[V](dot1, dot2)
}

class FastEuclideanLowD(squareRoot: Boolean = true) extends Euclidean[mutable.Buffer[Double]](squareRoot)

/**
 * For very very high dimension > 1000000
 */
class FastEuclideanHighD(squareRoot: Boolean = true) extends Euclidean[parallel.mutable.ParArray[Double]](squareRoot)

class EuclideanClusterizable[ID: Numeric, Obj, V <: Seq[Double]](final val squareRoot: Boolean = true) extends EuclideanMeta with RealClusterizableDistance[RealClusterizable[ID, Obj, V], V]
{
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	def d(dot1: RealClusterizable[ID, Obj, V], dot2: RealClusterizable[ID, Obj, V]): Double = euclidean[V](dot1.vector, dot2.vector)

	def obtainClassicalDistance(): Euclidean[V] = new Euclidean[V](squareRoot)
}