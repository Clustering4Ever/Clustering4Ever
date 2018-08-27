package clustering4ever.math.distances

import scala.collection.immutable
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

trait ClusterizableDistance[T, V, D <: Distance[V]] extends Distance[T]
{
	def obtainClassicalDistance(): D
}

trait RealClusterizableDistance[T, V <: Seq[Double]] extends ClusterizableDistance[T, V, ContinuousDistance[V]]

trait BinaryClusterizableDistance[T, V <: Seq[Int]] extends ClusterizableDistance[T, V, BinaryDistance[V]]

trait MixtClusterizableDistance[T, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]] extends ClusterizableDistance[T, V, MixtDistance[Vb, Vs, V]]

trait ContinuousDistance[V <: Seq[Double]] extends Distance[V]
{
	def d(vector1: V, vector2: V): Double
}

trait BinaryDistance[V <: Seq[Int]] extends Distance[V]
{
	def d(vector1: V, vector2: V): Double
}

trait MixtDistance[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]] extends Distance[V]
{
	def d(vector1: V, vector2: V): Double
}

trait MixtDistanceClusterizable[ID, Obj, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]] extends Distance[MixtClusterizable[ID, Obj, Vb, Vs, V]]
{
	def d(vector1: MixtClusterizable[ID, Obj, Vb, Vs, V], vector2: MixtClusterizable[ID, Obj, Vb, Vs, V]): Double
}