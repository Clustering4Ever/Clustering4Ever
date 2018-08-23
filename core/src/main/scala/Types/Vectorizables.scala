package clustering4ever.scala.vectorizables

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
class VectorizableG[@specialized(Int, Double) T, Obj](obj: Obj, vectorizableFct: Option[Obj => Seq[T]] = None) extends VectorizableObj[Obj, Seq[T]](obj, vectorizableFct)

class VectorizableM[Obj](obj: Obj, vectorizableFct: Option[Obj => (Seq[Int], Seq[Double])] = None) extends VectorizableObj[Obj, (Seq[Int], Seq[Double])](obj, vectorizableFct)

class MixtVector(obj: (Seq[Int], Seq[Double])) extends VectorizableM[(Seq[Int], Seq[Double])](obj)

class RealVectorizable[Obj, S <: Seq[Double]](obj: Obj, vectorizableFct: Option[Obj => S] = None) extends VectorizableObj[Obj, S](obj, vectorizableFct)

class RealVector[S <: Seq[Double]](vector: S) extends RealVectorizable[S, S](vector)

class BinaryVectorizable[Obj](obj: Obj, vectorizableFct: Option[Obj => Seq[Int]] = None) extends VectorizableObj[Obj, Seq[Int]](obj)

class BinaryVector(vector: Seq[Int]) extends BinaryVectorizable[Seq[Int]](vector)