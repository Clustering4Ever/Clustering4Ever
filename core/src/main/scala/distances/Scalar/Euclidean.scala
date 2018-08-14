package clustering4ever.math.distances.scalar

import scala.math.{pow, sqrt}
import scala.collection.immutable
import clustering4ever.math.distances.{Distance, ContinuousDistances}
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
/**
 * @author Beck Gaël
 **/
class Euclidean(root: Boolean) extends ContinuousDistances
{
	private def euclideanIntern(dot1: immutable.Seq[Double], dot2: immutable.Seq[Double]) = dot1.zip(dot2).map{ case (a, b) => pow(a - b, 2) }.sum

	/**
	  * The famous euclidean distance implemented in its fast mono thread scala version without SQRT part
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	override def d(dot1: immutable.Seq[Double], dot2: immutable.Seq[Double]): Double =
	{
		if( root ) sqrt(euclideanIntern(dot1, dot2))
		else euclideanIntern(dot1, dot2)
	}

	lazy val toStringRoot = if( root ) "with " else "without "

	override def toString = "Euclidean " + toStringRoot + "root applied"

}

class EuclideanClusterizable[ID: Numeric, Obj](squareRoot: Boolean = true) extends Distance[RealClusterizable[ID, Obj]]
{

	private def euclideanIntern(dot1: RealClusterizable[ID, Obj], dot2: RealClusterizable[ID, Obj]) = dot1.vector.zip(dot2.vector).map{ case (a, b) => pow(a - b, 2) }.sum

	/**
	  * The famous euclidean distance implemented in its fast mono thread scala version without SQRT part
	  * @return The Euclidean distance between dot1 and dot2
	  **/
	override def d(dot1: RealClusterizable[ID, Obj], dot2: RealClusterizable[ID, Obj]): Double =
	{
		if( squareRoot ) sqrt(euclideanIntern(dot1, dot2))
		else euclideanIntern(dot1, dot2)
	}

	lazy val toStringRoot = if( squareRoot ) "with " else "without "

	override def toString() = "Euclidean " + toStringRoot + "square root applied"

}
