package clustering4ever.math.distances
/**
 * @author Beck Gaël
 */
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.clusterizables.SimpleMixtClusterizable
/**
 * Most general notion of Distance, taking two object of type T and returning a Double
 */
trait Distance[T] extends Serializable {
	def d(obj1: T, obj2: T): Double
}
/**
 *
 */
trait ClusterizableDistance[T, V, D <: Distance[V]] extends Distance[T] {
	val classicalMetric: D
}
/**
 *
 */
trait RealClusterizableDistance[T, V <: Seq[Double], D <: ContinuousDistance[V]] extends ClusterizableDistance[T, V, D]
/**
 *
 */
trait BinaryClusterizableDistance[T, V <: Seq[Int], D <: BinaryDistance[V]] extends ClusterizableDistance[T, V, D]
/**
 *
 */
trait MixtClusterizableDistance[T, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs], D <: MixtDistance[Vb, Vs, V]] extends ClusterizableDistance[T, V, D]
/**
 *
 */
trait ContinuousDistance[V <: Seq[Double]] extends Distance[V]
/**
 *
 */
trait BinaryDistance[V <: Seq[Int]] extends Distance[V]
/**
 *
 */
trait MixtDistance[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]] extends Distance[V]