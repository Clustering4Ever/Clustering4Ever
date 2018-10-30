package clustering4ever.math.distances.scalar
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import scala.math.pow
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.math.distances.{RealClusterizableDistance, ContinuousDistance}
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
class Minkowski[V <: Seq[Double]](final val p: Int = 2) extends MinkowshiMeta with ContinuousDistance[V] {
	/**
	  * The Minkowski distance
	  * @return The Minkowski distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = minkowski(dot1, dot2)
}
/**
 *
 */
class EasyMinkowski(p: Int) extends Minkowski[mutable.ArrayBuffer[Double]](p)
/**
 *
 */
class MinkowskiClusterizable[@specialized(Int, Long) ID: Numeric, O, V <: Seq[Double], D <: Minkowski[V], Cz <: RealClusterizable[ID, O, V, Cz]](final val p: Int = 2, val classicalMetric: D) extends MinkowshiMeta with RealClusterizableDistance[Cz, V, D] {
	/**
	  * The Minkowski distance
	  * @return The Minkowski distance between dot1 and dot2
	  */
	def d(dot1: Cz, dot2: Cz): Double = minkowski(dot1.vector, dot2.vector)
}