package org.clustering4ever.vectorizables
/**
 * @author Beck Gaël
 */
import scala.language.implicitConversions
/**
 *
 */
case class Vectorizable[O](val o: O) extends Serializable {
	def toVector[V](towardVector: O => V): V = towardVector(o)
}