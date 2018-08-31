package clustering4ever.math.distances

import scala.collection.GenSeq
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

trait RealClusterizableDistance[T, V <: GenSeq[Double]] extends ClusterizableDistance[T, V, ContinuousDistance[V]]

trait BinaryClusterizableDistance[T, V <: GenSeq[Int]] extends ClusterizableDistance[T, V, BinaryDistance[V]]

trait MixtClusterizableDistance[T, Vb <: GenSeq[Int], Vs <: GenSeq[Double], V <: BinaryScalarVector[Vb, Vs]] extends ClusterizableDistance[T, V, MixtDistance[Vb, Vs, V]]

trait ContinuousDistance[V <: GenSeq[Double]] extends Distance[V]

trait BinaryDistance[V <: GenSeq[Int]] extends Distance[V]

trait MixtDistance[Vb <: GenSeq[Int], Vs <: GenSeq[Double], V <: BinaryScalarVector[Vb, Vs]] extends Distance[V]

trait MixtDistanceClusterizable[ID, Obj, Vb <: GenSeq[Int], Vs <: GenSeq[Double], V <: BinaryScalarVector[Vb, Vs]] extends Distance[MixtClusterizable[ID, Obj, Vb, Vs, V]]