package clustering4ever.scala.vectorizables

import clustering4ever.scala.measurableclass.BinaryScalarVector
import scala.collection.immutable

trait Vectorizable[Vector] extends Serializable
{
	def toVector(): Vector
}

abstract class VectorizableObj[Obj, Vector](val obj: Obj, vectorizableFct: Option[Obj => Vector] = None) extends Vectorizable[Vector]
{
	def toVector(): Vector =
	{
		if( ! vectorizableFct.isDefined ) obj.asInstanceOf[Vector]
		else vectorizableFct.get(obj)
	}
}

class MixtVectorizable[Obj, Vb <: immutable.Seq[Int], Vs <: immutable.Seq[Double]](obj: Obj, vectorizableFct: Option[Obj => BinaryScalarVector[Vb, Vs]] = None) extends VectorizableObj[Obj, BinaryScalarVector[Vb, Vs]](obj, vectorizableFct)

class MixtVector[Vb <: immutable.Seq[Int], Vs <: immutable.Seq[Double]](obj: BinaryScalarVector[Vb, Vs]) extends MixtVectorizable[BinaryScalarVector[Vb, Vs], Vb, Vs](obj)

class RealVectorizable[Obj, V <: immutable.Seq[Double]](obj: Obj, vectorizableFct: Option[Obj => V] = None) extends VectorizableObj[Obj, V](obj, vectorizableFct)

class RealVector[V <: immutable.Seq[Double]](vector: V) extends RealVectorizable[V, V](vector)

class BinaryVectorizable[Obj, V <: immutable.Seq[Int]](obj: Obj, vectorizableFct: Option[Obj => V] = None) extends VectorizableObj[Obj, V](obj, vectorizableFct)

class BinaryVector[V <: immutable.Seq[Int]](vector: V) extends BinaryVectorizable[V, V](vector)