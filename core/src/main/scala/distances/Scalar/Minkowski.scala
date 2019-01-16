package org.clustering4ever.math.distances.scalar
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import scala.math.pow
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.ContinuousDistance
import org.clustering4ever.vectors.ScalarVector
import org.clustering4ever.types.MetricIDType._
/**
 *
 */
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
/**
 *
 */
case class Minkowski[V <: Seq[Double]](final val p: Int = 2, final val id: MetricID = 3) extends MinkowshiMeta with ContinuousDistance[V] {
	/**
	  * The Minkowski distance
	  * @return The Minkowski distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = minkowski(dot1, dot2)
	/**
	  * The Minkowski distance
	  * @return The Minkowski distance between dot1 and dot2
	  */
	def d(dot1: ScalarVector[V], dot2: ScalarVector[V]): Double = minkowski(dot1.vector, dot2.vector)
}