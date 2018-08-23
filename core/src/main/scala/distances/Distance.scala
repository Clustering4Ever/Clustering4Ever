package clustering4ever.math.distances

import scala.collection.immutable
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.clusterizables.ClusterizableM

/**
 * @author Beck Gaël
 * Most general notion of Distance, taking two object of type T and returning a Double
 **/
trait Distance[-T] extends Serializable
{
	def d(obj1: T, obj2: T): Double
}

trait ClusterizableDistance[T, U] extends Distance[T]
{
	def obtainClassicalDistance(): Distance[Seq[U]]
}

trait RealClusterizableDistance[T] extends ClusterizableDistance[T, Double]
{
	def obtainClassicalDistance(): ContinuousDistance
}

trait ContinuousDistance extends Distance[Seq[Double]]
{
	def d(vector1: Seq[Double], vector2: Seq[Double]): Double
}

trait BinaryDistance[T <: Seq[Int]] extends Distance[T]
{
	def d(vector1: T, vector2: T): Double
}

trait MixtDistance extends Distance[BinaryScalarVector]
{
	def d(vector1: BinaryScalarVector, vector2: BinaryScalarVector): Double
}

trait MixtDistanceClusterizable[ID, Obj] extends Distance[ClusterizableM[ID, Obj]]
{
	def d(vector1: ClusterizableM[ID, Obj], vector2: ClusterizableM[ID, Obj]): Double
}