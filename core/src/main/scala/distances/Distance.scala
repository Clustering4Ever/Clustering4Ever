package org.clustering4ever.math.distances
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.scala.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
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

	def d(o1: O, o2: O): Double
}
/**
 *
 */
trait DistanceV[V <: GVector] extends Serializable {
	
	def dV(o1: V, o2: V): Double

}
/**
 *
 */
trait DoubleDistance[O, V <: GVector] extends Distance[O] with DistanceV[V]
/**
 *
 */
trait ClusterizableDistance[ID, O, V <: GVector, Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz]] extends Distance[Cz[ID, O, V]]
/**
 *
 */
trait ClusterizableDistanceBuilder[V <: GVector] extends Serializable {

	def obtainClusterizableMetric[ID: Numeric, O, Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz]](clusterizable: Cz[ID, O, V]): ClusterizableDistance[ID, O, V, Cz]

}
/**
 *
 */
trait DoubleDistanceClusterizable[ID, O, V <: GVector, Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz]] extends DoubleDistance[Cz[ID, O, V], V]
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
trait RealClusterizableDistance[ID, O, V <: Seq[Double], Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz]] extends ClusterizableDistance[ID, O, ScalarVector[V], Cz]
/**
 *
 */
trait BinaryClusterizableDistance[ID, O, V <: Seq[Int], Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz]] extends ClusterizableDistance[ID, O, BinaryVector[V], Cz]
/**
 *
 */
trait MixtClusterizableDistance[ID, O, Vb <: Seq[Int], Vs <: Seq[Double], Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz]] extends ClusterizableDistance[ID, O, MixtVector[Vb, Vs], Cz]