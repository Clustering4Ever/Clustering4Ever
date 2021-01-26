package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */

import scala.language.higherKinds
/**
 *
 */
sealed trait VectorizableOrNot extends Serializable
/**
 * The container of the raw object
 * @tparam O the raw object type
 */
final case class Vectorizable[O](val o: O) extends VectorizableOrNot {
	/**
	 * Methods which transform the raw object toward a GVector descendant given the towardVector function
	 */
	final def toVector[V <: GVector[V]](towardVector: O => V): V = towardVector(o)
}
/**
 *
 */
object NotVectorizable extends VectorizableOrNot