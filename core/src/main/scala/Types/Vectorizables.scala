package clustering4ever.scala.vectorizables

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
class VectorizableG[@specialized(Int, Double) T, Obj](obj: Obj, vectorizableFct: Option[Obj => immutable.Vector[T]] = None) extends VectorizableObj[Obj, immutable.Vector[T]](obj, vectorizableFct)

class VectorizableM[Obj](obj: Obj, vectorizableFct: Option[Obj => (Seq[Int], Seq[Double])] = None) extends VectorizableObj[Obj, (Seq[Int], Seq[Double])](obj, vectorizableFct)

class MixtVector(obj: (immutable.Vector[Int], immutable.Vector[Double])) extends VectorizableM[(immutable.Vector[Int], immutable.Vector[Double])](obj)

class RealVectorizable[Obj](obj: Obj, vectorizableFct: Option[Obj => Seq[Double]] = None) extends VectorizableObj[Obj, Seq[Double]](obj, vectorizableFct)

class RealVector(vector: Seq[Double]) extends RealVectorizable[Seq[Double]](vector)

class BinaryVectorizable[Obj](obj: Obj, vectorizableFct: Option[Obj => Seq[Int]] = None) extends VectorizableObj[Obj, Seq[Int]](obj)

class BinaryVector(vector: Seq[Int]) extends BinaryVectorizable[Seq[Int]](vector)