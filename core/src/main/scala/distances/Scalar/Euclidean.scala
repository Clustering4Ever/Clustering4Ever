package clustering4ever.math.distances.scalar

import scala.math.{pow, sqrt}
import scala.collection.{immutable, mutable}
import clustering4ever.math.distances.{RealClusterizableDistance, ContinuousDistance}
import clustering4ever.scala.clusterizables.RealClusterizable

/**
 * @author Beck Gaël
 **/
trait EuclideanMeta extends Serializable
{
	protected val squareRoot: Boolean

	protected def euclidean[V <: Seq[Double]](dot1: V, dot2: V): Double =
	{
		var sum = 0D
		dot1.zip(dot2).foreach{ case (a, b) => sum += pow(a - b, 2) }
		if( squareRoot ) sqrt(sum) else sum
	}

	lazy val toStringRoot = if( squareRoot ) "with " else "without "

	override def toString = "Euclidean " + toStringRoot + "root applied"
}

class Euclidean[V <: Seq[Double]](final val squareRoot: Boolean = true) extends EuclideanMeta with ContinuousDistance[V]
{
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	def d(dot1: V, dot2: V): Double = euclidean[V](dot1, dot2)
}

class FastEuclidean(squareRoot: Boolean = true) extends Euclidean[mutable.Buffer[Double]](squareRoot)
{
	override def d(dot1: mutable.Buffer[Double], dot2: mutable.Buffer[Double]): Double =
	{
		var sum = 0D
		dot1.indices.foreach( i => sum += pow(dot1(i) - dot2(i), 2) )
		if( squareRoot ) sqrt(sum) else sum
	}
}

class EuclideanClusterizable[ID: Numeric, Obj, V <: Seq[Double]](final val squareRoot: Boolean = true) extends EuclideanMeta with RealClusterizableDistance[RealClusterizable[ID, Obj, V], V]
{
	/**
	  * The Euclidean distance with or without squareRoot
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	def d(dot1: RealClusterizable[ID, Obj, V], dot2: RealClusterizable[ID, Obj, V]): Double = euclidean[V](dot1.vector, dot2.vector)

	def obtainClassicalDistance(): Euclidean[V] = new Euclidean[V](squareRoot)
}