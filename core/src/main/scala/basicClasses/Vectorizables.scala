package org.clustering4ever.scala.vectorizables
/**
 * @author Beck Gaël
 */
import scala.language.implicitConversions
/**
 *
 */
object ToVectorizable {
	implicit def oToVectorizableO[O](o: O): Vectorizable[O] = new Vectorizable(o)
}
/**
 *
 */
case class Vectorizable[O](val o: O) extends Serializable {
	def toVector[V](towardVector: O => V): V = towardVector(o)
}
