package clustering4ever.scala.vectorizables
/**
 * @author Beck Gaël
 */
import clustering4ever.scala.measurableclass.BinaryScalarVector
/**
 *
 */
abstract class Vectorizable[O, V](val o: O, toVector: O => V) extends Serializable {
	def toVector(): V = toVector(o)
}
/**
 *
 */
class Vector[V](o: V) extends Vectorizable[V, V](o, identity)
/**
 *
 */
class RealVectorizable[O, V <: Seq[Double]](o: O, toVector: O => V = identity _) extends Vectorizable[O, V](o, toVector)
/**
 *
 */
class RealVector[V <: Seq[Double]](vector: V) extends RealVectorizable[V, V](vector, identity)
/**
 *
 */
class BinaryVectorizable[O, V <: Seq[Int]](o: O, toVector: O => V = identity _) extends Vectorizable[O, V](o, toVector)
/**
 *
 */
class BinaryVector[V <: Seq[Int]](vector: V) extends BinaryVectorizable[V, V](vector, identity)
/**
 *
 */
class MixtVectorizable[O, Vb <: Seq[Int], Vs <: Seq[Double]](o: O, toVector: O => BinaryScalarVector[Vb, Vs] = identity _) extends Vectorizable[O, BinaryScalarVector[Vb, Vs]](o, toVector)
/**
 *
 */
class MixtVector[Vb <: Seq[Int], Vs <: Seq[Double]](o: BinaryScalarVector[Vb, Vs]) extends MixtVectorizable[BinaryScalarVector[Vb, Vs], Vb, Vs](o, identity)