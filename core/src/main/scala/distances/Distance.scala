package clustering4ever.math.distances

import scala.collection.immutable
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.clusterizables.ClusterizableM

/**
 * @author Beck Gaël
 * Most general notion of Distance, taking two object of type T and returning a Double
 **/
trait Distance[T] extends Serializable
{
	def d(obj1: T, obj2: T): Double
}

trait ClusterizableDistance[T, V] extends Distance[T]
{
	def obtainClassicalDistance(): Distance[V]
}

trait RealClusterizableDistance[T, V <: Seq[Double]] extends ClusterizableDistance[T, V]
{
	def obtainClassicalDistance(): ContinuousDistance[V]
}

trait ContinuousDistance[V <: Seq[Double]] extends Distance[V]
{
	def d(vector1: V, vector2: V): Double
}

trait BinaryDistance[V <: Seq[Int]] extends Distance[V]
{
	def d(vector1: V, vector2: V): Double
}

trait MixtDistance[Vb <: Seq[Int], Vs <: Seq[Double]] extends Distance[BinaryScalarVector[Vb, Vs]]
{
	def d(vector1: BinaryScalarVector[Vb, Vs], vector2: BinaryScalarVector[Vb, Vs]): Double
}

trait MixtDistanceClusterizable[ID, Obj, Vb <: Seq[Int], Vs <: Seq[Double]] extends Distance[ClusterizableM[ID, Obj, Vb, Vs]]
{
	def d(vector1: ClusterizableM[ID, Obj, Vb, Vs], vector2: ClusterizableM[ID, Obj, Vb, Vs]): Double
}