package org.clustering4ever.vectorizables
/**
 * @author Beck Gaël
 */
import scala.language.implicitConversions
/**
 *
 */
sealed trait VectorizableOrNot extends Serializable
/**
 *
 */
case class Vectorizable[O](val o: O) extends VectorizableOrNot {
	def toVector[V](towardVector: O => V): V = towardVector(o)
}
/**
 *
 */
object NotVectorizable extends VectorizableOrNot