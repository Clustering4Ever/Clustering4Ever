package clustering4ever.math.distances.scalar

import scala.math.{pow, sqrt}
import scala.collection.immutable
import clustering4ever.math.distances.{RealClusterizableDistance, ContinuousDistance}
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable

/**
 * @author Beck Gaël
 **/
trait EuclideanMeta extends Serializable
{
	protected val squareRoot: Boolean

	protected def euclidean(dot1: Seq[Double], dot2: Seq[Double]) =
	{
		val rawEuclidean = dot1.zip(dot2).map{ case (a, b) => pow(a - b, 2) }.sum
		if( squareRoot ) sqrt(rawEuclidean) else rawEuclidean
	}

	lazy val toStringRoot = if( squareRoot ) "with " else "without "

	override def toString = "Euclidean " + toStringRoot + "root applied"
}

class Euclidean(final val squareRoot: Boolean = true) extends EuclideanMeta with ContinuousDistance
{
	/**
	  * The famous euclidean distance implemented in its fast mono thread scala version without SQRT part
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	def d(dot1: Seq[Double], dot2: Seq[Double]): Double = euclidean(dot1, dot2)
}

class EuclideanClusterizable[ID: Numeric, Obj, S <: Seq[Double]](final val squareRoot: Boolean = true) extends EuclideanMeta with RealClusterizableDistance[RealClusterizable[ID, Obj, S]]
{
	/**
	  * The famous euclidean distance implemented in its fast mono thread scala version without SQRT part
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	def d(dot1: RealClusterizable[ID, Obj, S], dot2: RealClusterizable[ID, Obj, S]): Double = euclidean(dot1.vector, dot2.vector)

	def obtainClassicalDistance(): Euclidean = new Euclidean(squareRoot)
}
