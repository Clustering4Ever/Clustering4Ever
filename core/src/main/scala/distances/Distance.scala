package clustering4ever.math.distances

import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.clusterizables.MixtClusterizable

/**
 * @author Beck Gaël
 * Most general notion of Distance, taking two object of type T and returning a Double
 **/
trait Distance[T] extends Serializable
{
	def d(obj1: T, obj2: T): Double
}

trait ClusterizableDistance[T, S, D <: Distance[S]] extends Distance[T]
{
	def obtainClassicalDistance(): D
}

trait RealClusterizableDistance[T, S <: Seq[Double]] extends ClusterizableDistance[T, S, ContinuousDistance[S]]

trait BinaryClusterizableDistance[T, S <: Seq[Int]] extends ClusterizableDistance[T, S, BinaryDistance[S]]

trait MixtClusterizableDistance[T, Vb <: Seq[Int], Vs <: Seq[Double], S <: BinaryScalarVector[Vb, Vs]] extends ClusterizableDistance[T, S, MixtDistance[Vb, Vs, S]]

trait ContinuousDistance[S <: Seq[Double]] extends Distance[S]

trait BinaryDistance[S <: Seq[Int]] extends Distance[S]

trait MixtDistance[Vb <: Seq[Int], Vs <: Seq[Double], S <: BinaryScalarVector[Vb, Vs]] extends Distance[S]

trait MixtDistanceClusterizable[ID, Obj, Vb <: Seq[Int], Vs <: Seq[Double], S <: BinaryScalarVector[Vb, Vs]] extends Distance[MixtClusterizable[ID, Obj, Vb, Vs, S]]