package clustering4ever.scala.vectorizables

import clustering4ever.scala.measurableclass.BinaryScalarVector
import scala.collection.GenSeq

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

class MixtVectorizable[Obj, Vb <: GenSeq[Int], Vs <: GenSeq[Double], V <: BinaryScalarVector[Vb, Vs]](obj: Obj, vectorizableFct: Option[Obj => V] = None) extends VectorizableObj[Obj, V](obj, vectorizableFct)

class MixtVector[Vb <: GenSeq[Int], Vs <: GenSeq[Double], V <: BinaryScalarVector[Vb, Vs]](obj: V) extends MixtVectorizable[V, Vb, Vs, V](obj)

class RealVectorizable[Obj, V <: GenSeq[Double]](obj: Obj, vectorizableFct: Option[Obj => V] = None) extends VectorizableObj[Obj, V](obj, vectorizableFct)

class RealVector[V <: GenSeq[Double]](vector: V) extends RealVectorizable[V, V](vector)

class BinaryVectorizable[Obj, V <: GenSeq[Int]](obj: Obj, vectorizableFct: Option[Obj => V] = None) extends VectorizableObj[Obj, V](obj, vectorizableFct)

class BinaryVector[V <: GenSeq[Int]](vector: V) extends BinaryVectorizable[V, V](vector)