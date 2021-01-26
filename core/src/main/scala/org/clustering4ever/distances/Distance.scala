package org.clustering4ever.distances

/**
 * @author Beck Gaël
 */
import org.clustering4ever.roottraits.MetricIDType._
import org.clustering4ever.roottraits._

import scala.language.higherKinds
/**
 *
 */
trait MetricArgs extends Serializable
/**
 * Most general notion of Distance, taking two object of type O and returning a Double
 * @tparam O the object on which is apply the distance method
 */
trait GenericDistance[O] extends Serializable {
// trait Distance[O, MA <: MetricArgs] extends Serializable {
	// val metricArgs: MA
	/**
	 * @return the distance between two objects
	 */
	def d(o1: O, o2: O): Double
	/**
	 * The unique ID of the metric, it is used for internal clustering indices
	 */
	val id: MetricID
}
/**
 * The EmptyDistance for algorithm which doesn't require any distances
 */
object EmptyDistance extends GenericDistance[Nothing] {

	def d(o1: Nothing, o2: Nothing): Double = 0D

	val id = 0
}
/**
 * Distance trait for applicable on GVector descendant as objects
 * @tparam V the GVector on which is apply the distance method
 */
trait Distance[V <: GVector[V]] extends GenericDistance[V]
/**
 * @tparam SV the GVector on which is apply the distance method
 * @tparam T the nature of object inside vector field of GVector
 * @tparam V the nature of collection for vector field of GVector
 */
trait GSimpleVectorDistance[@specialized(Int, Double) N, SV <: GSimpleVector[N, SV]] extends Distance[SV] {
	/**
	 * @return the distance between two objects of nature Seq[T] where T can be any Numeric type ie (Double, float, Int...) usually
	 */
	def dRaw(v1: Array[N], v2: Array[N]): Double
}
/**
 *
 */
trait ContinuousDistance extends GSimpleVectorDistance[Double, ScalarVector]
/**
 *
 */
trait BinaryDistance extends GSimpleVectorDistance[Int, BinaryVector]
/**
 *
 */
trait MixedDistance extends Distance[MixedVector] {
	/**
	 * Distance on raw vector nature
	 */
	def dRaw(v1: (Array[Int], Array[Double]), v2: (Array[Int], Array[Double])): Double
}
/**
 *
 */
trait RawContinuousDistance extends GenericDistance[Array[Double]]
/**
 *
 */
trait RawBinaryDistance extends GenericDistance[Array[Int]]