package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
import shapeless.HMap

import scala.language.higherKinds
/**
 * HashCode of IdentifiedRawObject descendant is fixed to ID hashcode
 */
trait IdentifiedRawObject[O] {
	/**
	 * The ID of the container class
	 */
	val id: Long
	/**
	 * The raw object of the container class
	 */
	val o: O
	/**
	 * HashCode of the container is defined by hashcode of its id field
	 */
	final override def hashCode(): Int = id.hashCode
}
/**
 *
 */
final case class EasyIdentifiedRawObject[O](final val id: Long, final val o: O) extends IdentifiedRawObject[O]
/**
 * Identified Vectorizable Object
 */
trait IdentifiedVectorizableObject[O] extends IdentifiedRawObject[Vectorizable[O]]
/**
 *
 */
trait IdentifiedVector[O, V] extends IdentifiedVectorizableObject[O] {
	/**
	 * The working vector of the container
	 */
	val v: V
}
/**
 *
 */
trait HashCodeExtensionForVeryLargeDataSets extends Serializable {
	/**
	 * Second hashCode just in case if IDs are more than 2^32
	 */
	def hashCodeExt: Int

}
/**
 * Restrict V to be a GVector descendant
 */
trait IdentifiedGVector[O, V <: GVector[V]] extends IdentifiedVector[O, V]
/**
 *
 */
trait IdentifiedWithVectorizations[O, V <: GVector[V]] extends IdentifiedGVector[O, V] {
	/**
	 * HMap containing various vectorization of the raw object as descendant of GVector
	 */
	val vectorized: HMap[VMapping]
}
/**
 *
 */
final case class EasyIdentifiedVector[O, V <: GVector[V]](
	final val id: Long,
	final val v: V,
	final val o: Vectorizable[O],
	final val vectorized: HMap[VMapping] = HMap.empty[VMapping]
) extends IdentifiedWithVectorizations[O, V]