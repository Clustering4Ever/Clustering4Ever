/**
 * @author Beck Gaël
 */
package clustering4ever.math.distances.scalar
import scala.math.pow
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.math.distances.{RealClusterizableDistance, ContinuousDistance}

trait MinkowshiMeta extends Serializable {

	val p: Int

	protected def minkowski[V <: Seq[Double]](dot1: V, dot2: V): Double = {
		var d = 0D
		var i = 0
		while( i < dot1.size ) {
			d += pow(dot1(i) - dot2(i), p)
			i += 1			
		}
		pow(d, 1D / p )
	}
}

class Minkowski[V <: Seq[Double]](final val p: Int = 2) extends MinkowshiMeta with ContinuousDistance[V] {
	/**
	  * The Minkowski distance
	  * @return The Minkowski distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = minkowski[V](dot1, dot2)
}

class MinkowskiClusterizable[ID: Numeric, Obj, V <: Seq[Double]](final val p: Int = 2, val classicalMetric: Minkowski[V]) extends MinkowshiMeta with RealClusterizableDistance[RealClusterizable[ID, Obj, V], V] {
	/**
	  * The Minkowski distance
	  * @return The Minkowski distance between dot1 and dot2
	  */
	def d(dot1: RealClusterizable[ID, Obj, V], dot2: RealClusterizable[ID, Obj, V]): Double = minkowski[V](dot1.vector, dot2.vector)

	def obtainClassicalDistance(): Minkowski[V] = new Minkowski[V]
}