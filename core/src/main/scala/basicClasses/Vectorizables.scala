package org.clustering4ever.scala.vectorizables
/**
 * @author Beck Gaël
 */
import org.clustering4ever.scala.vectors.GMixtVector
/**
 *
 */
class Vectorizable[O](val o: O) extends Serializable {
	def toVector[V](towardVector: O => V = identity _): V = towardVector(o)
}
/**
 *
 */
// case class RealVector[V <: Seq[Double]](vector: V) extends Vectorizable[V](vector)
/**
 *
 */
// case class BinaryVector[V <: Seq[Int]](vector: V) extends Vectorizable[V](vector)
/**
 *
 */
// case class MixtVector[Vb <: Seq[Int], Vs <: Seq[Double]](vector: GMixtVector[Vb, Vs]) extends Vectorizable[GMixtVector[Vb, Vs]](vector)