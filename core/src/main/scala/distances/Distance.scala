package org.clustering4ever.math.distances
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.types.MetricIDType._
/**
 *
 */
trait MetricArgs extends Serializable
/**
 * Most general notion of Distance, taking two object of type O and returning a Double
 */
trait Distance[O] extends Serializable {
// trait Distance[O, MA <: MetricArgs] extends Serializable {
	// val metricArgs: MA
	/**
	 *
	 */
	def d(o1: O, o2: O): Double
	/**
	 *
	 */
	val id: MetricID
}
/**
 * The EmptyDistance for algorithm which doesn't require any distances
 */
object EmptyDistance extends Distance[Nothing] {
	/**
	 *
	 */
	def d(o1: Nothing, o2: Nothing): Double = 0D
	/**
	 *
	 */
	val id = 0
}
/**
 * Clusterizable Distance Builder void trait
 * It is used to optionalize obtention of a metric on Clusterizable for clustering algorithms
 */
trait ClusterizableDistance[ID, O, V <: GVector[V], Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]] extends Distance[Cz[ID, O, V]] {
	/**
	 *
	 */
	val gvMetricIntern: Distance[V]
}
/**
 *
 */
trait ClusterizableDistanceBuilder[V <: GVector[V]] extends Serializable {
	/**
	 *
	 */
	def obtainClusterizableMetric[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]](clusterizable: Cz[ID, O, V]): ClusterizableDistance[ID, O, V, Cz]

}
/**
 *
 */
trait ContinuousDistance[V <: Seq[Double]] extends Distance[ScalarVector[V]]
/**
 *
 */
trait BinaryDistance[V <: Seq[Int]] extends Distance[BinaryVector[V]]
/**
 *
 */
trait MixtDistance[Vb <: Seq[Int], Vs <: Seq[Double]] extends Distance[MixtVector[Vb, Vs]]
/**
 *
 */
trait RawContinuousDistance[V <: Seq[Double]] extends Distance[V]
/**
 *
 */
trait RawBinaryDistance[V <: Seq[Int]] extends Distance[V]
/**
 *
 */
trait RealClusterizableDistance[ID, O, V <: Seq[Double], Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]] extends ClusterizableDistance[ID, O, ScalarVector[V], Cz]
/**
 *
 */
trait BinaryClusterizableDistance[ID, O, V <: Seq[Int], Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]] extends ClusterizableDistance[ID, O, BinaryVector[V], Cz]
/**
 *
 */
trait MixtClusterizableDistance[ID, O, Vb <: Seq[Int], Vs <: Seq[Double], Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]] extends ClusterizableDistance[ID, O, MixtVector[Vb, Vs], Cz]