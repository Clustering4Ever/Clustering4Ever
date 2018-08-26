package clustering4ever.scala.vectorizables

import clustering4ever.scala.measurableclass.BinaryScalarVector

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

class VectorizableM[Obj, Vb <: Seq[Int], Vs <: Seq[Double]](obj: Obj, vectorizableFct: Option[Obj => BinaryScalarVector[Vb, Vs]] = None) extends VectorizableObj[Obj, BinaryScalarVector[Vb, Vs]](obj, vectorizableFct)

class MixtVector[Vb <: Seq[Int], Vs <: Seq[Double]](obj: BinaryScalarVector[Vb, Vs]) extends VectorizableM[BinaryScalarVector[Vb, Vs], Vb, Vs](obj)

class RealVectorizable[Obj, V <: Seq[Double]](obj: Obj, vectorizableFct: Option[Obj => V] = None) extends VectorizableObj[Obj, V](obj, vectorizableFct)

class RealVector[V <: Seq[Double]](vector: V) extends RealVectorizable[V, V](vector)

class BinaryVectorizable[Obj, V <: Seq[Int]](obj: Obj, vectorizableFct: Option[Obj => V] = None) extends VectorizableObj[Obj, V](obj, vectorizableFct)

class BinaryVector[V <: Seq[Int]](vector: V) extends BinaryVectorizable[V, V](vector)