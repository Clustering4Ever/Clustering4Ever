package clustering4ever.math.distances.scalar

import _root_.scala.math.{pow, sqrt}
import _root_.clustering4ever.math.distances.Distance
import _root_.clustering4ever.scala.clusterizables.RealClusterizable
import _root_.clustering4ever.scala.vectorizables.RealVectorizable

/**
 * @author Beck Gaël
 **/
class EuclideanClusterizable[ID: Numeric, Obj](squareRoot: Boolean = true) extends Distance[RealClusterizable[ID, Obj]]
{

	private def euclideanIntern(dot1: RealClusterizable[ID, Obj], dot2: RealClusterizable[ID, Obj]) =
	{
		var d = 0D
		for( i <- dot1.vector.indices ) d += pow(dot1.vector(i) - dot2.vector(i), 2)
		d
	}

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