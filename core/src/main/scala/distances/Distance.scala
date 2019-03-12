package org.clustering4ever.math.distances
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, GSimpleVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.types.MetricIDType._
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
trait GSimpleVectorDistance[@specialized(Int, Double) N, V <: Seq[N], SV <: GSimpleVector[N, V, SV]] extends Distance[SV] {
	/**
	 * @return the distance between two objects of nature Seq[T] where T can be any Numeric type ie (Double, float, Int...) usually
	 */
	def d(v1: V, v2: V): Double
}
/**
 *
 */
trait ContinuousDistance[V <: Seq[Double]] extends GSimpleVectorDistance[Double, V, ScalarVector[V]]
/**
 *
 */
trait BinaryDistance[V <: Seq[Int]] extends GSimpleVectorDistance[Int, V, BinaryVector[V]]
/**
 *
 */
trait MixedDistance[Vb <: Seq[Int], Vs <: Seq[Double]] extends Distance[MixedVector[Vb, Vs]] {
	/**
	 * Distance on raw vector nature
	 */
	def d(v1: (Vb, Vs), v2: (Vb, Vs)): Double
}
/**
 *
 */
trait RawContinuousDistance[V <: Seq[Double]] extends GenericDistance[V]
/**
 *
 */
trait RawBinaryDistance[V <: Seq[Int]] extends GenericDistance[V]