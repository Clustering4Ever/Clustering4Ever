package clustering4ever.scala.vectorizables
/**
 * @author Beck Gaël
 */
import clustering4ever.scala.measurableclass.BinaryScalarVector

trait Vectorizable[V] extends Serializable {
	def toVector(): V
}

abstract class VectorizableObj[O, V](val obj: O, vectorizableFct: Option[O => V] = None) extends Vectorizable[V] {
	def toVector(): V = {
		if( ! vectorizableFct.isDefined ) obj.asInstanceOf[V]
		else vectorizableFct.get(obj)
	}
}

class RealVectorizable[O, V <: Seq[Double]](obj: O, vectorizableFct: Option[O => V] = None) extends VectorizableObj[O, V](obj, vectorizableFct)

class RealVector[V <: Seq[Double]](vector: V) extends RealVectorizable[V, V](vector, None)

class BinaryVectorizable[O, V <: Seq[Int]](obj: O, vectorizableFct: Option[O => V] = None) extends VectorizableObj[O, V](obj, vectorizableFct)

class BinaryVector[V <: Seq[Int]](vector: V) extends BinaryVectorizable[V, V](vector, None)

class MixtVectorizable[O, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](obj: O, vectorizableFct: Option[O => V] = None) extends VectorizableObj[O, V](obj, vectorizableFct)

class MixtVector[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](obj: V) extends MixtVectorizable[V, Vb, Vs, V](obj, None)