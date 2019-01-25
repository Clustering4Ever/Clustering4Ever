package org.clustering4ever.vectorizables
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.vectors.GVector
/**
 *
 */
sealed trait VectorizableOrNot extends Serializable
/**
 * The container of the raw object
 */
case class Vectorizable[O](val o: O) extends VectorizableOrNot {
	/**
	 * Methods which transform the raw object toward a GVector descendant given the towardVector function
	 */
	def toVector[V <: GVector[V]](towardVector: O => V): V = towardVector(o)
}
/**
 *
 */
object NotVectorizable extends VectorizableOrNot